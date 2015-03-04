package com.fsist.safepickle.reactivemongo

import _root_.akka.util.ByteString
import com.fsist.safepickle.akka.AkkaPicklers
import com.fsist.safepickle.joda.JodaTimePicklers
import com.fsist.safepickle._
import org.apache.commons.codec.binary.Base64
import org.joda.time.Instant
import org.scalatest.FunSuite
import _root_.reactivemongo.bson.{BSONDateTime, BSONBinary, BSONObjectID}

class ReactiveMongoPicklersTest extends FunSuite {
  import JodaTimePicklers._
  import ReactiveMongoPicklers._
  import AkkaPicklers._

  test("typeName works") {
    assert(ReactiveMongoPicklers.stringifiedObjectId.typeName == "reactivemongo.bson.BSONObjectID")
  }

  test("Write ObjectId to different backends") {
    val oid = BSONObjectID.generate
    
    {
      val writer = ReactiveMongoPicklerBackend.writer()
      writer.write(oid)
      assert(writer.result() == oid)
    }
    
    {
      val writer = WrapperBackend.writer()
      writer.write(oid)
      assert(writer.result() == StringWrapper(oid.stringify))
    }
  }
  
  test("Write byte array to different backends") {
    val arr = Array[Byte](1,2,3,4,5)

    {
      val writer = ReactiveMongoPicklerBackend.writer()
      writer.write(arr)
      // BSONBinary.equals doesn't compare contents of arrays and so always returns false
      val result = writer.result().asInstanceOf[BSONBinary]
      assert(result.value.readArray(result.value.size).toIndexedSeq == arr.toIndexedSeq)
    }

    {
      val writer = WrapperBackend.writer()
      writer.write(arr)
      assert(writer.result() == StringWrapper(Base64.encodeBase64String(arr)))
    }
  }

  test("Write Instant to different backends") {
    val ts = new Instant(12345)

    {
      val writer = ReactiveMongoPicklerBackend.writer()
      writer.write(ts)
      val result = writer.result().asInstanceOf[BSONDateTime]
      assert(result.value == ts.getMillis)
    }

    {
      import JodaTimePicklers._
      val writer = WrapperBackend.writer()
      writer.write(ts)
      assert(writer.result() == LongWrapper(12345))
    }
  }

  test("Write and read ByteString") {
    val bytes = ByteString(Array[Byte](1,2,3,4,5))
    val bson = ReactiveMongoPicklerBackend.write(bytes)
    assert(bson.isInstanceOf[BSONBinary])
    val bytes2 = ReactiveMongoPicklerBackend.read[ByteString](bson)
    assert(bytes2.toIndexedSeq == bytes.toIndexedSeq)
  }
}
