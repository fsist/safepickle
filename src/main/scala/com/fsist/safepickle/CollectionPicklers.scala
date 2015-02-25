package com.fsist.safepickle

import scala.language.higherKinds

import scala.language.experimental.macros
import scala.reflect.runtime.universe._

import scala.collection.generic.CanBuildFrom
import scala.reflect.ClassTag

/** Implicit picklers for collection types.
  *
  * This trait exists as a mixin so that types extending it can re-export its implicit definitions.
  * If you don't need that, use the CollectionPicklers object.
  */
trait CollectionPicklersMixin extends TuplePicklers {
  implicit def iterablePickler[T, Coll[T] <: Iterable[T]](implicit tpickler: Pickler[T],
                                                          cbf: CanBuildFrom[Nothing, T, Coll[T]],
                                                          tag: TypeTag[T]): Pickler[Coll[T]] = new Pickler[Coll[T]] {
    override def pickle(coll: Coll[T], writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit = {
      writer.writeArrayStart()
      val iter = coll.iterator
      while (iter.hasNext) writer.write(iter.next)(tpickler)
      writer.writeArrayEnd()
    }

    override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): Coll[T] = {
      reader.assertTokenType(TokenType.ArrayStart)
      reader.nextInArray()

      val builder = cbf()
      while (reader.tokenType != TokenType.ArrayEnd) {
        builder += reader.readTagged[T]()
        reader.nextInArray()
      }

      // Leave the array end token as the current token
      builder.result()
    }
  }

  /** An array is not natively an Iterable, and isn't picked up by `iterablePickler` above */
  implicit def arrayPickler[T](implicit tpickler: Pickler[T], tag: TypeTag[T], classTag: ClassTag[T]): Pickler[Array[T]] = new Pickler[Array[T]] {
    val cbf = Array.canBuildFrom[T]

    override def pickle(Array: Array[T], writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit = {
      writer.writeArrayStart()
      val iter = Array.iterator
      while (iter.hasNext) writer.write(iter.next)(tpickler)
      writer.writeArrayEnd()
    }

    override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): Array[T] = {
      reader.assertTokenType(TokenType.ArrayStart)
      reader.nextInArray()

      val builder = cbf()
      while (reader.tokenType != TokenType.ArrayEnd) {
        builder += reader.readTagged[T]()
        reader.nextInArray()
      }

      // Leave the array end token as the current token
      builder.result()
    }
  }

  implicit def stringMapPickler[T, Coll[String, T] <: Map[String, T]](implicit tpickler: Pickler[T],
                                                                      cbf: CanBuildFrom[Nothing, (String, T), Coll[String, T]],
                                                                      tag: TypeTag[T]): Pickler[Coll[String, T]] =
    new Pickler[Coll[String, T]] {
      override def pickle(coll: Coll[String, T], writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit = {
        if (emitObjectStart) writer.writeObjectStart()

        val iter = coll.iterator
        while (iter.hasNext) {
          val (k, v) = iter.next
          writer.writeAttributeName(k)
          writer.write(v)(tpickler)
        }
        writer.writeObjectEnd()
      }

      override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): Coll[String, T] = {
        if (expectObjectStart) {
          reader.assertTokenType(TokenType.ObjectStart)
          reader.nextInObject()
        }

        val builder = cbf()
        while (reader.tokenType != TokenType.ObjectEnd) {
          val name = reader.attributeName
          reader.nextInObject()
          val value = reader.readTagged[T]()
          builder += ((name, value))
          reader.nextInObject()
        }

        // Leave the object end token as the current token
        builder.result()
      }
    }

  implicit def anyMapPickler[K, V, Coll[K, V] <: Map[K, V]](implicit kpickler: Pickler[K],
                                                            vpickler: Pickler[V],
                                                            cbf: CanBuildFrom[Nothing, (K, V), Coll[K, V]],
                                                            ktag: TypeTag[K], vtag: TypeTag[V]): Pickler[Coll[K, V]] = {
    implicit val tpickler = tuple2[K, V]

    // Can't figure out how to call iterablePickler here
    new Pickler[Coll[K, V]] {
      override def pickle(coll: Coll[K, V], writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit = {
        writer.writeArrayStart()
        val iter = coll.iterator
        while (iter.hasNext) writer.write(iter.next)(tpickler)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): Coll[K, V] = {
        reader.assertTokenType(TokenType.ArrayStart)
        reader.nextInArray()

        val builder = cbf()
        while (reader.tokenType != TokenType.ArrayEnd) {
          builder += reader.readTagged[(K, V)]()
          reader.nextInArray()
        }

        // Leave the array end token as the current token
        builder.result()
      }
    }
  }
}

/** Implicit picklers for collection types. */
object CollectionPicklers extends CollectionPicklersMixin
