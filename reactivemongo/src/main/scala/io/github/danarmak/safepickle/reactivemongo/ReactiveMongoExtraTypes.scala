package io.github.danarmak.safepickle.reactivemongo

import io.github.danarmak.safepickle.reactivemongo.ReactiveMongoPicklingBackend.{PickleWriter, PickleReader}
import io.github.danarmak.safepickle.{PicklingBackend, Writer, Reader, Pickler}
import reactivemongo.bson.BSONObjectID

/** Picklers for types that have a native representation in ReactiveMongo BSON. */
object ReactiveMongoExtraTypes {
  
  // TODO add a pickler for DateTime - can't just keep using Long, need to use Joda Instant 
}
