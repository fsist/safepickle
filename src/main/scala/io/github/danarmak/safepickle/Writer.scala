package io.github.danarmak.safepickle

/** Writes a stream of tokens to an internal mutable buffer.
  * 
  * Instances are assumed to be mutable and not safe for concurrent write access, with some sort of mutable buffer or
  * builder collecting the Result.
  * 
  * Methods may, but are not required to, fail with an IllegalStateException if called in an illegal sequence
  * (e.g. writeAttributeName twice in a row).
  */
trait Writer[Repr] {
  /** Returns everything written so far.
    * 
    * May optionally fail with an IllegalStateException if the data written is not valid, e.g. an open Object or Array
    * has not been closed.
    */
  def result(): Repr
  
  def writeInt(int: Int): Unit
  def writeLong(long: Long): Unit
  def writeString(string: String): Unit
  def writeBoolean(boolean: Boolean): Unit
  def writeNull(): Unit

  /** After writing an attribute name, write its value using one of the other writeXxx methods */
  def writeAttributeName(name: String): Unit
  
  def writeArrayStart(): Unit
  def writeArrayEnd(): Unit
  
  def writeObjectStart(): Unit
  def writeObjectEnd(): Unit
}

/** A way of writing the type T which has a direct representation in some, but not all, Writer implementations.
  *
  * A Writer could support the type directly (i.e. it would be a `Writer with OptionalWriter[T]`; otherwise, a fallback
  * implementation would be required that could use any standard Writer.
  */
trait OptionalWriter[T] {
  def writeT(t: T): Unit
}
