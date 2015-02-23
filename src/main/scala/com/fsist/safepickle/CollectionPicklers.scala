package com.fsist.safepickle

import scala.language.higherKinds

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait CollectionPicklers extends TuplePicklers {
  implicit def iterablePickler[T, Coll[T] <: Iterable[T], Backend <: PicklingBackend](implicit tpickler: Pickler[T, Backend],
                                                                                      cbf: CanBuildFrom[Nothing, T, Coll[T]]): Pickler[Coll[T], Backend] = new Pickler[Coll[T], Backend] {
    override def pickle(coll: Coll[T], writer: Backend#PickleWriter, emitObjectStart: Boolean = true): Unit = {
      writer.writeArrayStart()
      val iter = coll.iterator
      while (iter.hasNext) tpickler.pickle(iter.next, writer)
      writer.writeArrayEnd()
    }

    override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean = true): Coll[T] = {
      reader.assertTokenType(TokenType.ArrayStart)
      reader.nextInArray()

      val builder = cbf()
      while (reader.tokenType != TokenType.ArrayEnd) {
        builder += tpickler.unpickle(reader)
        reader.nextInArray()
      }

      // Leave the array end token as the current token
      builder.result()
    }
  }

  /** An array is not natively an Iterable, and isn't picked up by `iterablePickler` above */
  implicit def arrayPickler[T, Backend <: PicklingBackend](implicit tpickler: Pickler[T, Backend], tag: ClassTag[T]): Pickler[Array[T], Backend] = new Pickler[Array[T], Backend] {
    val cbf = Array.canBuildFrom[T]

    override def pickle(Array: Array[T], writer: Backend#PickleWriter, emitObjectStart: Boolean = true): Unit = {
      writer.writeArrayStart()
      val iter = Array.iterator
      while (iter.hasNext) tpickler.pickle(iter.next, writer)
      writer.writeArrayEnd()
    }

    override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean = true): Array[T] = {
      reader.assertTokenType(TokenType.ArrayStart)
      reader.nextInArray()

      val builder = cbf()
      while (reader.tokenType != TokenType.ArrayEnd) {
        builder += tpickler.unpickle(reader)
        reader.nextInArray()
      }

      // Leave the array end token as the current token
      builder.result()
    }
  }

  implicit def stringMapPickler[T, Coll[String, T] <: Map[String, T], Backend <: PicklingBackend](implicit tpickler: Pickler[T, Backend],
                                                                                                  cbf: CanBuildFrom[Nothing, (String, T), Coll[String, T]]): Pickler[Coll[String, T], Backend] =
    new Pickler[Coll[String, T], Backend] {
      override def pickle(coll: Coll[String, T], writer: Backend#PickleWriter, emitObjectStart: Boolean = true): Unit = {
        if (emitObjectStart) writer.writeObjectStart()

        val iter = coll.iterator
        while (iter.hasNext) {
          val (k, v) = iter.next
          writer.writeAttributeName(k)
          tpickler.pickle(v, writer)
        }
        writer.writeObjectEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean = true): Coll[String, T] = {
        if (expectObjectStart) {
          reader.assertTokenType(TokenType.ObjectStart)
          reader.nextInObject()
        }

        val builder = cbf()
        while (reader.tokenType != TokenType.ObjectEnd) {
          val name = reader.attributeName
          reader.nextInObject()
          val value = tpickler.unpickle(reader)
          builder += ((name, value))
          reader.nextInObject()
        }

        // Leave the object end token as the current token
        builder.result()
      }
    }

  implicit def anyMapPickler[K, V, Coll[K, V] <: Map[K, V], Backend <: PicklingBackend](implicit kpickler: Pickler[K, Backend],
                                                                                        vpickler: Pickler[V, Backend],
                                                                                        cbf: CanBuildFrom[Nothing, (K, V), Coll[K, V]]): Pickler[Coll[K, V], Backend] = {
    val tpickler = tuple2[K, V, Backend]

    // Can't figure out how to call iterablePickler here
    new Pickler[Coll[K, V], Backend] {
      override def pickle(coll: Coll[K, V], writer: Backend#PickleWriter, emitObjectStart: Boolean = true): Unit = {
        writer.writeArrayStart()
        val iter = coll.iterator
        while (iter.hasNext) tpickler.pickle(iter.next, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean = true): Coll[K, V] = {
        reader.assertTokenType(TokenType.ArrayStart)
        reader.nextInArray()

        val builder = cbf()
        while (reader.tokenType != TokenType.ArrayEnd) {
          builder += tpickler.unpickle(reader)
          reader.nextInArray()
        }

        // Leave the array end token as the current token
        builder.result()
      }
    }
  }
}
