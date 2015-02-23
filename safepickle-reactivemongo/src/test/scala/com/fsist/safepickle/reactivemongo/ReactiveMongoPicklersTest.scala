package com.fsist.safepickle.reactivemongo

import com.fsist.safepickle.joda.JodaTimePicklers
import com.fsist.safepickle.{LongWrapper, StringWrapper, WrapperBackend}
import org.apache.commons.codec.binary.Base64
import org.joda.time.Instant
import org.scalatest.FunSuite
import reactivemongo.bson.{BSONDateTime, BSONBinary, BSONObjectID}

class ReactiveMongoPicklersTest extends FunSuite {
  import ReactiveMongoPicklingBackend.picklers._

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
  
  test("Write byte array to different backends") {
    val arr = Array[Byte](1,2,3,4,5)

    {
      val writer = ReactiveMongoPicklingBackend.writer()
      writer.pickle(arr)
      // BSONBinary.equals doesn't compare contents of arrays and so always returns false
      val result = writer.result().asInstanceOf[BSONBinary]
      assert(result.value.readArray(result.value.size).toIndexedSeq == arr.toIndexedSeq)
    }

    {
      val writer = WrapperBackend.writer()
      writer.pickle(arr)
      assert(writer.result() == StringWrapper(Base64.encodeBase64String(arr)))
    }
  }

  test("Write Instant to different backends") {
    val ts = new Instant(12345)

    {
      val writer = ReactiveMongoPicklingBackend.writer()
      writer.pickle(ts)
      val result = writer.result().asInstanceOf[BSONDateTime]
      assert(result.value == ts.getMillis)
    }

    {
      import JodaTimePicklers._
      val writer = WrapperBackend.writer()
      writer.pickle(ts)
      assert(writer.result() == LongWrapper(12345))
    }
  }
}
