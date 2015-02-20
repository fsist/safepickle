package io.github.danarmak.safepickle

import scala.collection.generic.CanBuildFrom

object CollectionPicklers {
  implicit def iterablePickler[T, Coll <: Iterable[T], Backend <: PicklingBackend](implicit tpickler: Pickler[T, Backend],
                                                                                   cbf: CanBuildFrom[Nothing, T, Coll]): Pickler[Coll, Backend] = new Pickler[Coll, Backend] {
    override def pickle(coll: Coll, writer: Backend#PickleWriter, emitObjectStart: Boolean = true): Unit = {
      writer.writeArrayStart()
      val iter = coll.iterator
      while (iter.hasNext) tpickler.pickle(iter.next, writer)
      writer.writeArrayEnd()
    }

    override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean = true): Coll = {
      if (reader.tokenType != TokenType.ArrayStart) throw new IllegalStateException("Expected: array start")
      if (!reader.next()) throw new IllegalStateException("Unexpected EOF after array start")

      val builder = cbf()
      while (reader.tokenType != TokenType.ArrayEnd) {
        builder += tpickler.unpickle(reader)
        if (!reader.next()) throw new IllegalStateException("Unexpected EOF inside array")
      }

      // Leave the array end token as the current token
      builder.result()
    }
  }

  implicit def stringMapPickler[T, Coll <: Map[String, T], Backend <: PicklingBackend](implicit tpickler: Pickler[T, Backend],
                                                           cbf: CanBuildFrom[Nothing, (String, T), Coll]): Pickler[Coll, Backend] = new Pickler[Coll, Backend] {
    override def pickle(coll: Coll, writer: Backend#PickleWriter, emitObjectStart: Boolean = true): Unit = {
      if (emitObjectStart) writer.writeObjectStart()

      val iter = coll.iterator
      while (iter.hasNext) {
        val (k, v) = iter.next
        writer.writeAttributeName(k)
        tpickler.pickle(v, writer)
      }
      writer.writeObjectEnd()
    }

    override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean = true): Coll = {
      if (expectObjectStart) {
        if (reader.tokenType != TokenType.ObjectStart) throw new IllegalStateException("Expected: object start")
        if (!reader.next()) throw new IllegalStateException("Unexpected EOF after object start")
      }

      val builder = cbf()
      while (reader.tokenType != TokenType.ObjectEnd) {
        val name = reader.attributeName
        reader.next()
        val value = tpickler.unpickle(reader)
        builder += ((name, value))
        if (!reader.next()) throw new IllegalStateException("Unexpected EOF inside object")
      }

      // Leave the object end token as the current token
      builder.result()
    }
  }
}
