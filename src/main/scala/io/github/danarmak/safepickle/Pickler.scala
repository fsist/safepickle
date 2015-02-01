package io.github.danarmak.safepickle

/** A way to pickle or unpickle a type. */
trait Pickler[T] {
  def pickle(t: T, writer: Writer[_]): Unit
  
  /** Should read and parse the reader's current token without advancing it. */
  def unpickle(reader: Reader): T
}

/** Implicit definitions of picklers for standard types. */
object DefaultPicklers {

  implicit object Int extends Pickler[Int] {
    override def pickle(int: Int, writer: Writer[_]): Unit = writer.writeInt(int)

    override def unpickle(reader: Reader): Int = reader.int
  }

  implicit object Long extends Pickler[Long] {
    override def pickle(long: Long, writer: Writer[_]): Unit = writer.writeLong(long)

    override def unpickle(reader: Reader): Long = reader.long
  }

  implicit object Boolean extends Pickler[Boolean] {
    override def pickle(boolean: Boolean, writer: Writer[_]): Unit = writer.writeBoolean(boolean)

    override def unpickle(reader: Reader): Boolean = reader.boolean
  }

  implicit object String extends Pickler[String] {
    override def pickle(string: String, writer: Writer[_]): Unit = writer.writeString(string)

    override def unpickle(reader: Reader): String = reader.string
  }

  implicit object Null extends Pickler[Null] {
    override def pickle(Null: Null, writer: Writer[_]): Unit = writer.writeNull()

    override def unpickle(reader: Reader): Null = if (reader.isNull) null else throw new IllegalStateException("Expected: null")
  }
}
