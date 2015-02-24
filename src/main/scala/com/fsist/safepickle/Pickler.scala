package com.fsist.safepickle

import org.apache.commons.codec.binary.Base64

/** A way to pickle or unpickle a type.
  *
  * @tparam Backend allows the pickler to be specific to a backend, allowing different picklers to produce different
  *                 representations of the same value for different backends.
  */
trait Pickler[T, -Backend <: PicklingBackend] {
  /** Should call whatever Writer methods are necessary to write the value `t`.
    *
    * @param emitObjectStart If writing an object, and this argument is true, the pickler will write an ObjectStart token.
    *                        If false, it will assume an object scope has already been opened, and start emitting attributes.
    */
  def pickle(t: T, writer: Backend#PickleWriter, emitObjectStart: Boolean = true): Unit

  /** Should read and parse the reader's current token without advancing it.
    * 
    * If reading a complex value (object, array) or a series of values, consisting of multiple Reader tokens, 
    * should assume the current token when called is the first token to be read, and should leave the last token
    * that was read and processed as the current token.
    * 
    * @param expectObjectStart if reading an object, and this argument is true, expect the current reader token to be
    *                          the object start. If it is false, expect the current token to be the first attribute name
    *                          inside the object. If not reading an object, ignore this argument.
    * @throws UnpicklingException if the reader provides unexpected input
    */
  def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean = true): T
}

object Pickler {
  type Generic[T] = Pickler[T, PicklingBackend]
}

class UnpicklingException(msg: String, cause: Throwable = null) extends Exception(msg, cause)
object UnpicklingException {
  def apply(msg: String, cause: Throwable = null) = new UnpicklingException(msg, cause)
}

case class UnexpectedEofException(expected: String) extends UnpicklingException(s"Unexpected EOF (expected: $expected)")

/** Implicit definitions of picklers for standard types.
  * 
  * NOTE: this trait exists only so that mixing it into a Backend.picklers cake can export its implicits.
  * You should not extend it yourself, since that will create new classes & instances for all the picklers it defines,
  * for every instance of your class that extends the trait. Instead, use [[PrimitivePicklers]] directly.
  */
trait PrimitivePicklersMixin {

  implicit object IntPickler extends Pickler.Generic[Int] {
    override def pickle(int: Int, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit = 
      writer.writeInt(int)

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): Int = 
      reader.int
  }

  implicit object LongPickler extends Pickler.Generic[Long] {
    override def pickle(long: Long, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit = 
      writer.writeLong(long)

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): Long = 
      reader.long
  }

  implicit object FloatPickler extends Pickler.Generic[Float] {
    override def pickle(float: Float, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit = 
      writer.writeFloat(float)

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): Float = 
      reader.float
  }

  implicit object DoublePickler extends Pickler.Generic[Double] {
    override def pickle(double: Double, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit = 
      writer.writeDouble(double)

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): Double = 
      reader.double
  }

  implicit object BooleanPickler extends Pickler.Generic[Boolean] {
    override def pickle(boolean: Boolean, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit = 
      writer.writeBoolean(boolean)

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): Boolean = 
      reader.boolean
  }

  implicit object StringPickler extends Pickler.Generic[String] {
    override def pickle(string: String, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit = 
      writer.writeString(string)

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): String = 
      reader.string
  }

  implicit object NullPickler extends Pickler.Generic[Null] {
    override def pickle(Null: Null, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit =
      writer.writeNull()

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): Null = {
      reader.assertTokenType(TokenType.Null)
      null
    }
  }

  /** Byte array pickler that writes the base64 value of the array as a string. */
  implicit object ByteArrayPickler extends Pickler[Array[Byte], PicklingBackend] {
    override def pickle(t: Array[Byte], writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean): Unit = {
      writer.writeString(Base64.encodeBase64String(t))
    }

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean): Array[Byte] = {
      Base64.decodeBase64(reader.string)
    }
  }
}

/** Implicit definitions of picklers for standard types. */
object PrimitivePicklers extends PrimitivePicklersMixin

/** Pickles values of type `T` by converting them to values of type `Other`, which has an `otherPickler` provided. */
trait ConvertPickler[T, Other, Backend <: PicklingBackend] extends Pickler[T, Backend]{
  implicit def otherPickler: Pickler[Other, Backend]

  def convertTo(t: T): Other
  def convertFrom(other: Other): T

  def pickle(t: T, writer: Backend#PickleWriter, emitObjectStart: Boolean = true): Unit =
    otherPickler.pickle(convertTo(t), writer, emitObjectStart)

  def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean = true): T =
    convertFrom(otherPickler.unpickle(reader, expectObjectStart))
}

/** A refining of [[ConvertPickler]] for converting types to Strings using their `toString` method. */
trait ConvertToStringPickler[T] extends ConvertPickler[T, String, PicklingBackend] {
  implicit def otherPickler: Pickler[String, PicklingBackend] = PrimitivePicklers.StringPickler
  def convertTo(t: T): String = t.toString
}
