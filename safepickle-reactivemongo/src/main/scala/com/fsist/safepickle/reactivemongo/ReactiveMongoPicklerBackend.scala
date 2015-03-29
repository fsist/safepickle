package com.fsist.safepickle.reactivemongo

import com.fsist.safepickle.Schema.{SString, Desc}

import scala.reflect.runtime.universe._
import java.util.Date

import akka.util.ByteString
import com.fsist.safepickle
import org.joda.time.{DateTime, Instant}
import _root_.reactivemongo.bson._
import com.fsist.safepickle._

object ReactiveMongoPicklerBackend extends safepickle.PicklerBackend {
  override type Repr = BSONValue

  override def reader(repr: BSONValue): PickleReader = new BSONTreePickleReader(repr)
  
  override def writer(): PickleWriter[Repr] = new BSONTreePickleWriter
}

object ReactiveMongoPicklers {
  /** Default way of writing a BSONObjectID as a string to backends other than ReactiveMongo. */
  implicit object stringifiedObjectId extends Pickler[BSONObjectID] {
    override def ttag = typeTag[BSONObjectID]
    override def pickle(t: BSONObjectID, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit =
      writer.writeString(t.stringify)
    override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): BSONObjectID =
      BSONObjectID(reader.string)
    override def schema: Schema = SString(Desc(typeHint = Some("ObjectId")))
  }
}

/** Maps object attribute names to valid Mongo object field names by escaping the $ and . characters. */
object ReactiveMongoEscapes {
  private val escape: String = "\\"
  private val dot: Char = '.'
  private val escapedDot: Char = '．'
  private val dollar: Char = '$'
  private val escapedDollar: Char = '＄'

  def escapeChar(from: Char, to: Char, in: String): String =
    in.replace(to.toString, escape + to).replace(from, to)

  def escapeAttributeName(in: String): String = {
    val withoutEscape = in.replace(escape, escape + escape)
    val withoutDot = escapeChar(dot, escapedDot, withoutEscape)
    val withoutDollar = escapeChar(dollar, escapedDollar, withoutDot)
    withoutDollar
  }

  def unescapeChar(from: Char, to: Char, in: String): String =
    in.replace(from, to).replace(escape + from, from.toString)

  def unescapeAttributeName(in: String): String = {
    val withDollar = unescapeChar(escapedDollar, dollar, in)
    val withDot = unescapeChar(escapedDot, dot, withDollar)
    withDot.replace(escape + escape, escape)
  }
}

object ReactiveMongoTreeParser extends TreeParser[BSONValue] {

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

  override def obj(node: BSONValue): Iterator[(String, BSONValue)] =
    node.asInstanceOf[BSONDocument].elements.iterator.map {
      case (name, value) => (ReactiveMongoEscapes.unescapeAttributeName(name), value)
    }
}

/** A TreeReader for the ReactiveMongo backend with support for the extra BSON native types. */
class BSONTreePickleReader(root: BSONValue) extends TreePickleReader[BSONValue](ReactiveMongoTreeParser, root) {
  // Override for specific values with primitive representation in BSON
  override def read[T](expectObjectStart: Boolean = true)(implicit pickler: Pickler[T]): T = {
    val ret = pickler.typeName match {
      case "reactivemongo.bson.BSONObjectID" => currentNode.asInstanceOf[BSONObjectID]
      case "scala.Array[Byte]" =>
        val bin = currentNode.asInstanceOf[BSONBinary].value
        bin.readArray(bin.size)
      case "akka.util.ByteString" =>
        val bin = currentNode.asInstanceOf[BSONBinary].value
        val arr = bin.readArray(bin.size)
        ByteString(arr)
      case "org.joda.time.Instant" => new Instant(currentNode.asInstanceOf[BSONDateTime].value)
      case "org.joda.time.DateTime" => new DateTime(currentNode.asInstanceOf[BSONDateTime].value)
      case "java.util.Date" => new Date(currentNode.asInstanceOf[BSONDateTime].value)
      case _ => pickler.unpickle(this, expectObjectStart)
    }

    ret.asInstanceOf[T]
  }
}

object ReactiveMongoTreeBuilder extends TreeBuilder[BSONValue] {
  override def int(int: Int): BSONValue = BSONInteger(int)
  override def boolean(boolean: Boolean): BSONValue = BSONBoolean(boolean)
  override def float(float: Float): BSONValue = BSONDouble(float)
  override def string(string: String): BSONValue = BSONString(string)
  override def double(double: Double): BSONValue = BSONDouble(double)
  override def nul: BSONValue = BSONNull
  override def long(long: Long): BSONValue = BSONLong(long)
  override def array(array: Iterable[BSONValue]): BSONValue = BSONArray(array)

  override def obj(obj: Map[String, BSONValue]): BSONValue = BSONDocument(
    obj.map{
      case (key, value) => (ReactiveMongoEscapes.escapeAttributeName(key), value)
    }
  )
}

/** A TreeWriter for the ReactiveMongo backend with support for the extra BSON native types. */
class BSONTreePickleWriter extends TreePickleWriter[BSONValue](ReactiveMongoTreeBuilder) {
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

  // Override for specific values with primitive representation in BSON
  override def write[T](t: T, emitObjectStart: Boolean = true)(implicit pickler: Pickler[T]): this.type = {
    t match {
      case id: BSONObjectID => write(id)
      case array: Array[Byte] => write(BSONBinary(array, Subtype.GenericBinarySubtype))
      case bytes: ByteString => write(BSONBinary(bytes.toArray, Subtype.GenericBinarySubtype))
      case instant: Instant => write(BSONDateTime(instant.getMillis))
      case dateTime: DateTime => write(BSONDateTime(dateTime.getMillis))
      case date: Date => write(BSONDateTime(date.getTime))
      case _ => pickler.pickle(t, this, emitObjectStart)
    }

    this
  }
}

