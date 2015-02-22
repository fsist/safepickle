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
    */
  def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean = true): T
}

object Pickler {
  type Generic[T] = Pickler[T, PicklingBackend]
}

/** Implicit definitions of picklers for standard types. */
trait PrimitivePicklers {

  implicit object Int extends Pickler.Generic[Int] {
    override def pickle(int: Int, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit = 
      writer.writeInt(int)

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): Int = 
      reader.int
  }

  implicit object Long extends Pickler.Generic[Long] {
    override def pickle(long: Long, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit = 
      writer.writeLong(long)

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): Long = 
      reader.long
  }

  implicit object Float extends Pickler.Generic[Float] {
    override def pickle(float: Float, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit = 
      writer.writeFloat(float)

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): Float = 
      reader.float
  }

  implicit object Double extends Pickler.Generic[Double] {
    override def pickle(double: Double, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit = 
      writer.writeDouble(double)

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): Double = 
      reader.double
  }

  implicit object Boolean extends Pickler.Generic[Boolean] {
    override def pickle(boolean: Boolean, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit = 
      writer.writeBoolean(boolean)

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): Boolean = 
      reader.boolean
  }

  implicit object String extends Pickler.Generic[String] {
    override def pickle(string: String, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit = 
      writer.writeString(string)

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): String = 
      reader.string
  }

  implicit object Null extends Pickler.Generic[Null] {
    override def pickle(Null: Null, writer: PicklingBackend#PickleWriter, emitObjectStart: Boolean = true): Unit =
      writer.writeNull()

    override def unpickle(reader: PicklingBackend#PickleReader, expectObjectStart: Boolean = true): Null = 
      if (reader.tokenType == TokenType.Null) null else throw new IllegalStateException("Expected: null")
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
