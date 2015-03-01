package com.fsist.safepickle

/** A marker trait for pickling backend implementations, allowing them to be distinguished at the type level.
  * Also serves as a factory for Reader and Writer instances.
  *
  * Note: this requires implementations to be singletons, because `this.type` is used to parameterize the Reader and Writer
  * instances returned.
  */
trait PicklerBackend {
  /** The type of the pickled form used by this backend. */
  type Repr

  /** Creates a new Reader that will read produce the values encoded in `repr` */
  def reader(repr: Repr): PickleReader

  def writer(): PickleWriter[Repr]

  // Convenience methods

  def write[T](t: T)(implicit pickler: Pickler[T]): Repr = writer().write(t).result()

  def read[T](repr: Repr)(implicit pickler: Pickler[T]): T = reader(repr).read[T]()
}

/** The default picklers whose definitions should always be available. */
object DefaultPicklers extends PrimitivePicklersMixin with CollectionPicklersMixin
