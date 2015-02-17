package io.github.danarmak.safepickle.reactivemongo

import io.github.danarmak.safepickle.reactivemongo.ReactiveMongoPicklingBackend.{PickleWriter, PickleReader}
import io.github.danarmak.safepickle.{PicklingBackend, Writer, Reader, Pickler}
import reactivemongo.bson.BSONObjectID

/** Picklers for types that have a native representation in ReactiveMongo BSON. */
object ReactiveMongoExtraTypes {
  implicit object Binary extends Pickler[Array[Byte], ReactiveMongoPicklingBackend.type] {
    override def pickle(t: Array[Byte], writer: ReactiveMongoPicklingBackend.PickleWriter): Unit = writer.writeBinary(t)
    override def unpickle(reader: ReactiveMongoPicklingBackend.PickleReader): Array[Byte] = reader.binary
  }
  
  implicit object ObjectId extends Pickler[BSONObjectID, ReactiveMongoPicklingBackend.type] {
    override def pickle(t: BSONObjectID, writer: PickleWriter): Unit = writer.writeObjectId(t)
    override def unpickle(reader: PickleReader): BSONObjectID = reader.objectId
  }
  
  /** Default way of writing a BSONObjectID as a string to backends other than ReactiveMongo. */
  implicit def stringifiedObjectId[Backend <: PicklingBackend]: Pickler[BSONObjectID, Backend] = new Pickler[BSONObjectID, Backend] {
    override def pickle(t: BSONObjectID, writer: Backend#PickleWriter): Unit = writer.writeString(t.stringify)
    override def unpickle(reader: Backend#PickleReader): BSONObjectID = BSONObjectID(reader.string)
  }
  
  // TODO add a pickler for DateTime - can't just keep using Long, need to use Joda Instant 
}
