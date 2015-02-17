package io.github.danarmak.safepickle

/** A way to pickle or unpickle a type.
  * 
  * @tparam Backend allows the pickler to be specific to a backend, allowing different picklers to produce different
  *                 representations of the same value for different backends.
  */
trait Pickler[T, Backend <: PicklingBackend] {
  /** Should call whatever Writer methods are necessary to write the value `t`. */
  def pickle(t: T, writer: Writer[_, Backend]): Unit
  
  /** Should read and parse the reader's current token without advancing it. */
  def unpickle(reader: Reader[Backend]): T
}

object Pickler {
  type Generic[T] = Pickler[T, PicklingBackend]
}

/** Implicit definitions of picklers for standard types. */
object PrimitivePicklers {
  
  implicit object Int extends Pickler.Generic[Int] {
    override def pickle(int: Int, writer: Writer[_, PicklingBackend]): Unit = writer.writeInt(int)

    override def unpickle(reader: Reader[PicklingBackend]): Int = reader.int
  }

  implicit object Long extends Pickler.Generic[Long] {
    override def pickle(long: Long, writer: Writer[_, PicklingBackend]): Unit = writer.writeLong(long)

    override def unpickle(reader: Reader[PicklingBackend]): Long = reader.long
  }

  implicit object Float extends Pickler.Generic[Float] {
    override def pickle(float: Float, writer: Writer[_, PicklingBackend]): Unit = writer.writeFloat(float)

    override def unpickle(reader: Reader[PicklingBackend]): Float = reader.float
  }

  implicit object Double extends Pickler.Generic[Double] {
    override def pickle(double: Double, writer: Writer[_, PicklingBackend]): Unit = writer.writeDouble(double)

    override def unpickle(reader: Reader[PicklingBackend]): Double = reader.double
  }

  implicit object Boolean extends Pickler.Generic[Boolean] {
    override def pickle(boolean: Boolean, writer: Writer[_, PicklingBackend]): Unit = writer.writeBoolean(boolean)

    override def unpickle(reader: Reader[PicklingBackend]): Boolean = reader.boolean
  }

  implicit object String extends Pickler.Generic[String] {
    override def pickle(string: String, writer: Writer[_, PicklingBackend]): Unit = writer.writeString(string)

    override def unpickle(reader: Reader[PicklingBackend]): String = reader.string
  }

  implicit object Null extends Pickler.Generic[Null] {
    override def pickle(Null: Null, writer: Writer[_, PicklingBackend]): Unit = writer.writeNull()

    override def unpickle(reader: Reader[PicklingBackend]): Null = if (reader.tokenType == TokenType.Null) null else throw new IllegalStateException("Expected: null")
  }
}
