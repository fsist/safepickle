package io.github.danarmak.safepickle

import scala.collection.mutable

/** A way to create new TreeNodes in some concrete implementation.
  *
  * Implementing this trait provides a `Writer` implementation via [[TreeWriter]].
  * However, tree-based representations are both less efficient and less secure than stream-based ones.
  *
  * @tparam Base the base concrete type of the nodes created by this factory.
  */
trait TreeBuilder[Base, Backend <: PicklingBackend] {
  def int(int: Int): Base
  def long(long: Long): Base
  def float(float: Float): Base
  def double(double: Double): Base
  def string(string: String): Base
  def boolean(boolean: Boolean): Base
  def nul: Base
  def array(array: Iterable[Base]): Base
  def obj(obj: Map[String, Base]): Base
}

/** Implements a Writer if given a way to build nodes in a tree-based representation. */
class TreeWriter[Base, Backend <: PicklingBackend](builder: TreeBuilder[Base, Backend]) extends Writer[Base, Backend] {

  import TreeWriter._

  private val stack = mutable.Stack[StackableWriter[Base]]()

  private val scalarWriter = new ScalarWriter[Base]()
  stack.push(scalarWriter)

  override def result(): Base = stack.top.result
  
  /** If currently expecting a scalar (top level or as an attribute value), writes this value.
    * If currently in an array context, appends this value. 
    * Otherwise fails with an IllegalStateException.
    */
  def write(value: Base): Unit = stack.top.append(value)

  override def writeString(string: String): Unit = stack.top.append(builder.string(string))
  override def writeFloat(float: Float): Unit = stack.top.append(builder.float(float))
  override def writeDouble(double: Double): Unit = stack.top.append(builder.double(double))
  override def writeNull(): Unit = stack.top.append(builder.nul)
  override def writeInt(int: Int): Unit = stack.top.append(builder.int(int))
  override def writeBoolean(boolean: Boolean): Unit = stack.top.append(builder.boolean(boolean))
  override def writeLong(long: Long): Unit = stack.top.append(builder.long(long))

  override def writeArrayStart(): Unit = stack.push(new ArrayWriter(builder))
  override def writeObjectStart(): Unit = stack.push(new ObjectWriter(builder))
  
  override def writeArrayEnd(): Unit = stack.top match {
    case array: ArrayWriter[Base, Backend] =>
      stack.pop()
      stack.top.append(array.result)
    case other => throw new IllegalStateException("Not in an array")
  }
  
  override def writeObjectEnd(): Unit = stack.top match {
    case obj: ObjectWriter[Base, Backend] =>
      stack.pop()
      stack.top.append(obj.result)
    case other => throw new IllegalStateException("Not in an object")
  }
  
  override def writeAttributeName(name: String): Unit = stack.top match {
    case obj: ObjectWriter[Base, Backend] =>
      obj.writeAttributeName(name)
    case other => throw new IllegalStateException("Not in an object")
  }
}

private object TreeWriter {

  trait StackableWriter[Base] {
    def append(value: Base): Unit
    def result: Base
  }
  
  class ScalarWriter[Base]() extends StackableWriter[Base] {
    private var value: Option[Base] = None
    
    override def append(v: Base): Unit = value match {
      case None => value = Some(v)
      case Some(other) => throw new IllegalStateException("Cannot write two values in a row")
    }
    
    override def result: Base = value.getOrElse(throw new IllegalStateException("No value written"))
  }

  class ArrayWriter[Base, Backend <: PicklingBackend](builder: TreeBuilder[Base, Backend]) extends StackableWriter[Base] {
    private val vector = Vector.newBuilder[Base]
    
    override def append(value: Base): Unit = vector += value
    
    override def result: Base = builder.array(vector.result())
  }

  class ObjectWriter[Base, Backend <: PicklingBackend](builder: TreeBuilder[Base, Backend]) extends StackableWriter[Base] {
    private val map = Map.newBuilder[String, Base]
    private var nextAttributeName: String = ""
    
    def writeAttributeName(name: String): Unit = {
      if (nextAttributeName == "") nextAttributeName = name
      else throw new IllegalStateException("Attribute name already written, expecting attribute value")
    }
    
    override def append(value: Base): Unit = {
      if (nextAttributeName != "") {
        map += ((nextAttributeName, value))
        nextAttributeName = ""
      }
      else throw new IllegalStateException("No attribute name written")
    }
    
    override def result: Base = builder.obj(map.result())
  }
}
