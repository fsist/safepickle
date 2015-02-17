package io.github.danarmak.safepickle.reactivemongo

import io.github.danarmak.safepickle.{Writer, Reader, Pickler}

/** Picklers for types that have a native representation in ReactiveMongo BSON. */
object ReactiveMongoExtraTypes {
  implicit object Binary extends Pickler[Iterable[Byte], ReactiveMongoPicklingBackend.type] {
    override def pickle(t: Iterable[Byte], writer: ReactiveMongoPicklingBackend.PickleWriter): Unit =
      writer.
    
    override def unpickle(reader: ReactiveMongoPicklingBackend.PickleReader): Iterable[Byte] =
      reader.
  }
}
