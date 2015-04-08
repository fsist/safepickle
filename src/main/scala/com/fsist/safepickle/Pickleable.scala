package com.fsist.safepickle

import scala.reflect.runtime.universe._

case class Pickleable[T](t: T)(implicit val pickler: Pickler[T]) {
  def write(writer: PickleWriter[_]): Unit = writer.write(t)(pickler)
}
object Pickleable {
  implicit def pickler[T](implicit tag: TypeTag[Pickleable[T]]): Pickler[Pickleable[T]] = new Pickler[Pickleable[T]] {
    override val ttag: TypeTag[Pickleable[T]] = tag
    override def pickle(t: Pickleable[T], writer: PickleWriter[_], emitObjectStart: Boolean): Unit =
      writer.write(t.t, emitObjectStart)(t.pickler)
    override def unpickle(reader: PickleReader, expectObjectStart: Boolean): Pickleable[T] =
      throw new NotImplementedError(s"This type cannot support unpickling")
    override val schema: Schema = throw new NotImplementedError(s"This type cannot support unpickling")
  }
}

