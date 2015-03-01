package com.fsist.safepickle.akka

import akka.util.ByteString
import com.fsist.safepickle.ConvertPickler

/** Picklers for Akka types. Placed in the safepickle-reactivemongo module since it has an Akka dep. */
object AkkaPicklers {

  /** Default way of writing a ByteString to non-ReactiveMongo backends is to convert it to an Array[Byte] and do
    * whatever that does. */
  implicit object ByteStringPickler extends ConvertPickler[ByteString, Array[Byte]] {
    override def convertTo(bytes: ByteString): Array[Byte] = bytes.toArray
    override def convertFrom(array: Array[Byte]): ByteString = ByteString(array)
  }
}
