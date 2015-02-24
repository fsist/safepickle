package com.fsist.safepickle

/** Writes a stream of tokens to an internal mutable buffer.
  * 
  * Instances are assumed to be mutable and not safe for concurrent write access, with some sort of mutable buffer or
  * builder collecting the Result.
  * 
  * Methods may, but are not required to, fail with an IllegalStateException if called in an illegal sequence
  * (e.g. writeAttributeName twice in a row).
  */
trait PickleWriter[+Repr] {

  /** Returns everything written so far.
    * 
    * May optionally fail with an IllegalStateException if the data written is not valid, e.g. an open Object or Array
    * has not been closed. However, implementations are not required to validate the produced result.
    */
  def result(): Repr
  
  // All writeXxx methods return the Writer
  
  def writeInt(int: Int): this.type
  def writeLong(long: Long): this.type
  def writeFloat(float: Float): this.type
  def writeDouble(double: Double): this.type
  def writeString(string: String): this.type
  def writeBoolean(boolean: Boolean): this.type
  def writeNull(): this.type

  /** Can be overridden by a particular implementation to intercept certain types, based on runtime type checking of `t`,
    * and write them in some backend-specific way without using the provided `pickler`.
    *
    * Otherwise, if the type is not being overridden, delegates to the `pickler` provided. */
  def write[T](t: T, emitObjectStart: Boolean = true)(implicit pickler: Pickler[T]): this.type = {
    pickler.pickle(t, this, emitObjectStart)
    this
  }

  /** After writing an attribute name, write its value using one of the other writeXxx methods */
  def writeAttributeName(name: String): this.type
  
  def writeArrayStart(): this.type
  def writeArrayEnd(): this.type
  
  def writeObjectStart(): this.type
  def writeObjectEnd(): this.type
}

