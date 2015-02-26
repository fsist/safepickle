package com.fsist.safepickle

import org.apache.commons.codec.binary.Base64

/** A way to pickle or unpickle a type. */
trait Pickler[T] {
  /** Should write the value `t` to the `writer`. If the type `T` contains sub-types for which we have sub-picklers,
    * they should be used by calling `writer.write[T](pickler)` for those types.
    *
    * WARNING: do not call this method directly; use `PickleWriter.write` with this pickler instead.
    *
    * @param emitObjectStart If writing an object, and this argument is true, the pickler will write an ObjectStart token.
    *                        If false, it will assume an object scope has already been opened, and start emitting attributes.
    */
  def pickle(t: T, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit

  /** Should read and parse the reader's current token without advancing it. If the type `T` contains sub-types for
    * which we have sub-picklers, they should be used by calling `reader.read[T](pickler)` for those types.
    * 
    * If reading a complex value (object, array) or a series of values, consisting of multiple Reader tokens, 
    * should assume the current token when called is the first token to be read, and should leave the last token
    * that was read and processed as the current token.
    *
    * WARNING: do not call this method directly; use `PickleReader.read` with this pickler instead.
    *
    * @param expectObjectStart if reading an object, and this argument is true, expect the current reader token to be
    *                          the object start. If it is false, expect the current token to be the first attribute name
    *                          inside the object. If not reading an object, ignore this argument.
    * @throws UnpicklingException if the reader provides unexpected input
    */
  def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): T
}

object Pickler extends PrimitivePicklersMixin with CollectionPicklersMixin

class UnpicklingException(msg: String, cause: Throwable = null) extends Exception(msg, cause)
object UnpicklingException {
  def apply(msg: String, cause: Throwable = null) = new UnpicklingException(msg, cause)
}

case class UnexpectedEofException(expected: String) extends UnpicklingException(s"Unexpected EOF (expected: $expected)")

/** Implicit definitions of picklers for standard types.
  *
  * This trait exists as a mixin so that types extending it can re-export its implicit definitions.
  * If you don't need that, use the PrimitivePicklers object.
  *
  * NOTE: in non-generic situations, when you know the type T you're dealing with statically, you don't have to use these
  * as sub-picklers; you can call the methods PickleWriter.writeXxx and PickleReader.readXxx directly instead, which is
  * more efficient and clearer to read.
  */
trait PrimitivePicklersMixin {

  implicit object ShortPickler extends Pickler[Short] {
    final override def pickle(short: Short, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit =
      writer.writeInt(short)

    final override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): Short =
      reader.int.toShort
  }

  implicit object IntPickler extends Pickler[Int] {
    final override def pickle(int: Int, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit =
      writer.writeInt(int)

    final override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): Int =
      reader.int
  }

  implicit object LongPickler extends Pickler[Long] {
    final override def pickle(long: Long, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit =
      writer.writeLong(long)

    final override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): Long =
      reader.long
  }

  implicit object FloatPickler extends Pickler[Float] {
    final override def pickle(float: Float, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit =
      writer.writeFloat(float)

    final override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): Float =
      reader.float
  }

  implicit object DoublePickler extends Pickler[Double] {
    final override def pickle(double: Double, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit =
      writer.writeDouble(double)

    final override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): Double =
      reader.double
  }

  implicit object BooleanPickler extends Pickler[Boolean] {
    final override def pickle(boolean: Boolean, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit =
      writer.writeBoolean(boolean)

    final override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): Boolean =
      reader.boolean
  }

  implicit object StringPickler extends Pickler[String] {
    final override def pickle(string: String, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit =
      writer.writeString(string)

    final override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): String =
      reader.string
  }

  implicit object NullPickler extends Pickler[Null] {
    final override def pickle(Null: Null, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit =
      writer.writeNull()

    final override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): Null = {
      reader.assertTokenType(TokenType.Null)
      null
    }
  }

  /** Byte array pickler that writes the base64 value of the array as a string. */
  implicit object ByteArrayPickler extends Pickler[Array[Byte]] {
    final override def pickle(t: Array[Byte], writer: PickleWriter[_], emitObjectStart: Boolean): Unit = {
      writer.writeString(Base64.encodeBase64String(t))
    }

    final override def unpickle(reader: PickleReader, expectObjectStart: Boolean): Array[Byte] = {
      Base64.decodeBase64(reader.string)
    }
  }
}

/** Implicit definitions of picklers for standard types. */
object PrimitivePicklers extends PrimitivePicklersMixin

/** Pickles values of type `T` by converting them to values of type `Other`, which has an `otherPickler` provided. */
abstract class ConvertPickler[T, Other](implicit val otherPickler: Pickler[Other]) extends Pickler[T]{
  def convertTo(t: T): Other
  def convertFrom(other: Other): T

  def pickle(t: T, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit =
    otherPickler.pickle(convertTo(t), writer, emitObjectStart)

  def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): T =
    convertFrom(otherPickler.unpickle(reader, expectObjectStart))
}

/** A refining of [[ConvertPickler]] for converting types to Strings using their `toString` method. */
abstract class ConvertToStringPickler[T] extends ConvertPickler[T, String]()(PrimitivePicklers.StringPickler) {
  def convertTo(t: T): String = t.toString
}
