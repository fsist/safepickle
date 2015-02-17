package io.github.danarmak.safepickle

/** A marker trait for pickling backend implementations, allowing them to be distinguished at the type level.
  * Also serves as a factory for Reader and Writer instances.
  * 
  * Note: this requires implementations to be singletons, because `this.type` is used to parameterize the Reader and Writer
  * instances returned.
  */
trait PicklingBackend {
  /** The type of the serialized form used by this backend. */
  type Repr
  
  type PickleReader <: Reader[this.type]
  type PickleWriter <: Writer[Repr, this.type]
  
  /** Creates a new Reader that will read produce the values encoded in `repr` */
  def reader(repr: Repr): PickleReader
  
  def writer(): PickleWriter
}

