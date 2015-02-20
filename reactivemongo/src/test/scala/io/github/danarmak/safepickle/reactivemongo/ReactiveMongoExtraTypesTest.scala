package io.github.danarmak.safepickle.reactivemongo

import io.github.danarmak.safepickle.{StringWrapper, WrapperBackend}
import org.scalatest.FunSuite
import reactivemongo.bson.BSONObjectID

class ReactiveMongoExtraTypesTest extends FunSuite {
  import ReactiveMongoExtraTypes._
  
  test("Write ObjectId to different backends") {
    val oid = BSONObjectID.generate 
    
    {
      val writer = ReactiveMongoPicklingBackend.writer()
      writer.pickle(oid)
      assert(writer.result() == oid)
    }
    
    {
      val writer = WrapperBackend.writer()
      writer.pickle(oid)
      assert(writer.result() == StringWrapper(oid.stringify))
    }
  }
}
