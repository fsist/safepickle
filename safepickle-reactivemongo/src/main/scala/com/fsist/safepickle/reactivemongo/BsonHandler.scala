package com.fsist.safepickle.reactivemongo

import scala.reflect.runtime.universe._

import com.fsist.safepickle.Pickler
import reactivemongo.bson._

object BsonHandler {
  /** Creates a ReactiveMongo BSONHandler based on the given Pickler. */
  def of[T](implicit pickler: Pickler[T], tag: TypeTag[T]): BSONHandler[BSONValue, T] = new BSONHandler[BSONValue, T] {
    override def read(bson: BSONValue): T = ReactiveMongoPicklerBackend.reader(bson).readTagged[T]()
    override def write(t: T): BSONValue = ReactiveMongoPicklerBackend.writer().write[T](t).result()
  }

  /** Creates a ReactiveMongo document handler based on the given Pickler.
    *
    * If the pickler produces an Object, it is used directly. Otherwise, if the pickler produces a primitive value,
    * it is wrapped in a BSONDocument that looks like { "_id" -> pickled value, "$$wrapped" -> true }.
    */
  def document[T](implicit pickler: Pickler[T], tag: TypeTag[T]): BSONDocumentReader[T] with BSONDocumentWriter[T] =
    new BSONDocumentReader[T] with BSONDocumentWriter[T] {
      override def read(bson: BSONDocument): T = {
        bson.get("$wrapped") match {
          case Some(BSONBoolean(true)) => ReactiveMongoPicklerBackend.reader(bson.get("_id").get).readTagged[T]()
          case _ => ReactiveMongoPicklerBackend.reader(bson).readTagged[T]()
        }
      }

      override def write(t: T): BSONDocument = {
        val bson = ReactiveMongoPicklerBackend.writer().write[T](t).result()
        bson match {
          case doc: BSONDocument => doc
          case value => BSONDocument(
            "_id" -> value,
            "$wrapped" -> true
          )
        }
      }
    }
}
