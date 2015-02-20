package io.github.danarmak.safepickle.reactivemongo

import io.github.danarmak.safepickle._
import _root_.reactivemongo.bson._

object ReactiveMongoPicklingBackend extends PicklingBackend {
  type Repr = BSONValue
  type PickleReader = BSONTreeReader
  type PickleWriter = BSONTreeWriter
  
  override def reader(repr: BSONValue): PickleReader = new BSONTreeReader(repr)
  
  override def writer(): PickleWriter = new BSONTreeWriter
}

object ReactiveMongoTreeParser extends TreeParser[BSONValue, ReactiveMongoPicklingBackend.type] {

  override def nodeType(node: BSONValue): TreeNodeType = node.code match {
    case 1 => TreeNodeType.Double
    case 2 => TreeNodeType.String
    case 3 => TreeNodeType.Object
    case 4 => TreeNodeType.Array
    case 5 => TreeNodeType.Other // Binary
    case 7 => TreeNodeType.Other // ObjectID
    case 8 => TreeNodeType.Boolean
    case 9 => TreeNodeType.Other // Date
    case 10 => TreeNodeType.Null
    case 11 => TreeNodeType.String // Regexp
    case 13 => TreeNodeType.String // Javascript
    case 14 => TreeNodeType.String // Symbol
    case 15 => TreeNodeType.String // Javascript with scope
    case 16 => TreeNodeType.Int
    case 17 => TreeNodeType.Long // Timestamp - this is an internal Mongo type only seen in the replication log
    case 18 => TreeNodeType.Long
  }

  override def boolean(node: BSONValue): Boolean = node.asInstanceOf[BSONBoolean].value
  override def int(node: BSONValue): Int = node.asInstanceOf[BSONInteger].value
  override def string(node: BSONValue): String = node match {
    case str: BSONString => str.value
    case reg: BSONRegex => reg.value
    case js: BSONJavaScript => js.value
    case jsw: BSONJavaScriptWS => jsw.value
    case sym: BSONSymbol => sym.value
    case other => throw new IllegalStateException(s"Unsupported BSON node type for reading as a string: $other")
  }
  override def long(node: BSONValue): Long = node match {
    case long: BSONLong => long.value
    case ts: BSONTimestamp => ts.value
    case other => throw new IllegalStateException(s"Unsupported BSON node type for reading as a Long: $other")
  }
  override def float(node: BSONValue): Float = node.asInstanceOf[BSONDouble].value.toFloat
  override def double(node: BSONValue): Double = node.asInstanceOf[BSONDouble].value.toFloat
  override def array(node: BSONValue): Iterator[BSONValue] = node.asInstanceOf[BSONArray].values.iterator
  override def obj(node: BSONValue): Iterator[(String, BSONValue)] = node.asInstanceOf[BSONDocument].elements.iterator
}

/** A TreeReader for the ReactiveMongo backend with support for the extra BSOn native types. */
class BSONTreeReader(root: BSONValue) extends TreeReader[BSONValue, ReactiveMongoPicklingBackend.type](ReactiveMongoTreeParser, root) {
  def binary: Array[Byte] = {
    val bin = currentNode.asInstanceOf[BSONBinary].value
    bin.readArray(bin.size)
  }
  
  def objectId: BSONObjectID = currentNode.asInstanceOf[BSONObjectID]
  
  def dateTime: Long = currentNode.asInstanceOf[BSONDateTime].value
}

object ReactiveMongoTreeBuilder extends TreeBuilder[BSONValue, ReactiveMongoPicklingBackend.type] {
  override def int(int: Int): BSONValue = BSONInteger(int)
  override def boolean(boolean: Boolean): BSONValue = BSONBoolean(boolean)
  override def float(float: Float): BSONValue = BSONDouble(float)
  override def string(string: String): BSONValue = BSONString(string)
  override def double(double: Double): BSONValue = BSONDouble(double)
  override def nul: BSONValue = BSONNull
  override def long(long: Long): BSONValue = BSONLong(long)
  override def array(array: Iterable[BSONValue]): BSONValue = BSONArray(array)
  override def obj(obj: Map[String, BSONValue]): BSONValue = BSONDocument(obj)
}

/** A TreeWriter for the ReactiveMongo backend with support for the extra BSON native types. */
class BSONTreeWriter extends TreeWriter[BSONValue, ReactiveMongoPicklingBackend.type](ReactiveMongoTreeBuilder) {
  def writeBinary(bytes: Array[Byte]): this.type = {
    write(BSONBinary(bytes, Subtype.GenericBinarySubtype))
    this
  }
  
  def writeObjectId(id: BSONObjectID): this.type = {
    write(id)
    this
  }
  
  def writeDateTime(timestamp: Long): this.type = {
    write(BSONDateTime(timestamp))
    this
  }
}

