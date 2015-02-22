package com.fsist.safepickle

import scala.language.higherKinds

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

trait CollectionPicklers {
  implicit def iterablePickler[T, Coll[T] <: Iterable[T], Backend <: PicklingBackend](implicit tpickler: Pickler[T, Backend],
                                                                                      cbf: CanBuildFrom[Nothing, T, Coll[T]]): Pickler[Coll[T], Backend] = new Pickler[Coll[T], Backend] {
    override def pickle(coll: Coll[T], writer: Backend#PickleWriter, emitObjectStart: Boolean = true): Unit = {
      writer.writeArrayStart()
      val iter = coll.iterator
      while (iter.hasNext) tpickler.pickle(iter.next, writer)
      writer.writeArrayEnd()
    }

    override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean = true): Coll[T] = {
      if (reader.tokenType != TokenType.ArrayStart) throw new IllegalStateException("Expected: array start")
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
      if (reader.tokenType != TokenType.ArrayStart) throw new IllegalStateException("Expected: array start")
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
          if (reader.tokenType != TokenType.ObjectStart) throw new IllegalStateException("Expected: object start")
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
        if (reader.tokenType != TokenType.ArrayStart) throw new IllegalStateException("Expected: array start")
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

  // TODO in future, will need (via whitebox macro? Shapeless Generic?) to support tuples of all sizes

  implicit def tuple2[T1, T2, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend],
                                                          tpickler2: Pickler[T2, Backend]): Pickler[Tuple2[T1, T2], Backend] =
    new Pickler[Tuple2[T1, T2], Backend] {
      override def pickle(t: Tuple2[T1, T2], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple2[T1, T2] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new IllegalArgumentException("Expected array start token")
        reader.nextInObject()

        val t1 = tpickler1.unpickle(reader)
        reader.nextInObject()
        val t2 = tpickler2.unpickle(reader)
        reader.nextInObject()

        if (reader.tokenType != TokenType.ArrayEnd) throw new IllegalArgumentException("Expected array end token")

        (t1, t2)
      }
    }

  implicit def tuple3[T1, T2, T3, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend],
                                                              tpickler2: Pickler[T2, Backend],
                                                              tpickler3: Pickler[T3, Backend]): Pickler[Tuple3[T1, T2, T3], Backend] =
    new Pickler[Tuple3[T1, T2, T3], Backend] {
      override def pickle(t: Tuple3[T1, T2, T3], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple3[T1, T2, T3] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new IllegalArgumentException("Expected array start token")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader)
        reader.nextInArray()
        val t2 = tpickler2.unpickle(reader)
        reader.nextInArray()
        val t3 = tpickler3.unpickle(reader)
        reader.nextInArray()

        if (reader.tokenType != TokenType.ArrayEnd) throw new IllegalArgumentException("Expected array end token")

        (t1, t2, t3)
      }
    }
}
