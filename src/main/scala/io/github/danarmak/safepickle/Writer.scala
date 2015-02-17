package io.github.danarmak.safepickle

/** Writes a stream of tokens to an internal mutable buffer.
  * 
  * Instances are assumed to be mutable and not safe for concurrent write access, with some sort of mutable buffer or
  * builder collecting the Result.
  * 
  * Methods may, but are not required to, fail with an IllegalStateException if called in an illegal sequence
  * (e.g. writeAttributeName twice in a row).
  */
trait Writer[Repr, Backend <: PicklingBackend] {

  /** Returns everything written so far.
    * 
    * May optionally fail with an IllegalStateException if the data written is not valid, e.g. an open Object or Array
    * has not been closed. However, implementations are not required to validate the produced result.
    */
  def result(): Repr
  
  def writeInt(int: Int): Unit
  def writeLong(long: Long): Unit
  def writeFloat(float: Float): Unit
  def writeDouble(double: Double): Unit
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

object Writer {
  type Generic[Repr] = Writer[Repr, PicklingBackend]
}
