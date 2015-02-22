package io.github.danarmak.safepickle

import org.scalatest.FunSuiteLike

/** Serializes to ordinary Scala objects. Used for testing. */
object WrapperBackend extends PicklingBackend {
  override type Repr = Wrapper
  override type PickleWriter = TreeWriter[Wrapper, this.type]
  override type PickleReader = TreeReader[Wrapper, this.type]
  
  override def writer(): PickleWriter = new TreeWriter(WrapperBuilder)
  override def reader(repr: Repr): PickleReader = new TreeReader(WrapperParser, repr)
}

sealed trait Wrapper

case class StringWrapper(string: String) extends Wrapper
case class BooleanWrapper(boolean: Boolean) extends Wrapper
case class IntWrapper(int: Int) extends Wrapper
case class LongWrapper(long: Long) extends Wrapper
case class FloatWrapper(float: Float) extends Wrapper
case class DoubleWrapper(double: Double) extends Wrapper
case object NullWrapper extends Wrapper
case class ObjectWrapper(attributes: Map[String, Wrapper]) extends Wrapper
case class ArrayWrapper(values: Iterable[Wrapper]) extends Wrapper

object WrapperBuilder extends TreeBuilder[Wrapper, WrapperBackend.type] {
  override def int(int: Int): Wrapper = IntWrapper(int)
  override def boolean(boolean: Boolean): Wrapper = BooleanWrapper(boolean)
  override def float(float: Float): Wrapper = FloatWrapper(float)
  override def string(string: String): Wrapper = StringWrapper(string)
  override def double(double: Double): Wrapper = DoubleWrapper(double)
  override def nul: Wrapper = NullWrapper
  override def long(long: Long): Wrapper = LongWrapper(long)
  override def array(array: Iterable[Wrapper]): Wrapper = ArrayWrapper(array)
  override def obj(obj: Map[String, Wrapper]): Wrapper = ObjectWrapper(obj)
}

object WrapperParser extends TreeParser[Wrapper, WrapperBackend.type] {
  override def nodeType(node: Wrapper): TreeNodeType = node match {
    case StringWrapper(_) => TreeNodeType.String
    case BooleanWrapper(_) => TreeNodeType.Boolean
    case IntWrapper(_) => TreeNodeType.Int
    case LongWrapper(_) => TreeNodeType.Long
    case FloatWrapper(_) => TreeNodeType.Float
    case DoubleWrapper(_) => TreeNodeType.Double
    case NullWrapper => TreeNodeType.Null
    case ObjectWrapper(_) => TreeNodeType.Object
    case ArrayWrapper(_) => TreeNodeType.Array
  }
  
  override def boolean(node: Wrapper): Boolean = node.asInstanceOf[BooleanWrapper].boolean
  override def float(node: Wrapper): Float = node.asInstanceOf[FloatWrapper].float
  override def int(node: Wrapper): Int = node.asInstanceOf[IntWrapper].int
  override def string(node: Wrapper): String = node.asInstanceOf[StringWrapper].string
  override def double(node: Wrapper): Double = node.asInstanceOf[DoubleWrapper].double
  override def long(node: Wrapper): Long = node.asInstanceOf[LongWrapper].long
  override def array(node: Wrapper): Iterator[Wrapper] = node.asInstanceOf[ArrayWrapper].values.iterator
  override def obj(node: Wrapper): Iterator[(String, Wrapper)] = node.asInstanceOf[ObjectWrapper].attributes.iterator
}

trait WrapperTester { self: FunSuiteLike =>
  def roundtrip[T](value: T, expectedWrapper: Wrapper)(implicit pickler: Pickler[T, PicklingBackend]): Unit = {
    import WrapperBackend.picklers._

    val writer = WrapperBackend.writer()
    pickler.pickle(value, writer)
    val wrapper = writer.result()
    assert(wrapper == expectedWrapper)
    val reader = WrapperBackend.reader(wrapper)
    val read = pickler.unpickle(reader)
    assert(read == value)
  }
}

