package com.fsist.safepickle

trait TuplePicklers {
  implicit def tuple1[T1, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend]): Pickler[Tuple1[T1], Backend] =
    new Pickler[Tuple1[T1], Backend] {
      override def pickle(t: Tuple1[T1], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple1[T1] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        Tuple1(t1)
      }
    }


  implicit def tuple2[T1, T2, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend]): Pickler[Tuple2[T1, T2], Backend] =
    new Pickler[Tuple2[T1, T2], Backend] {
      override def pickle(t: Tuple2[T1, T2], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple2[T1, T2] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2)
      }
    }


  implicit def tuple3[T1, T2, T3, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend]): Pickler[Tuple3[T1, T2, T3], Backend] =
    new Pickler[Tuple3[T1, T2, T3], Backend] {
      override def pickle(t: Tuple3[T1, T2, T3], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple3[T1, T2, T3] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3)
      }
    }


  implicit def tuple4[T1, T2, T3, T4, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend]): Pickler[Tuple4[T1, T2, T3, T4], Backend] =
    new Pickler[Tuple4[T1, T2, T3, T4], Backend] {
      override def pickle(t: Tuple4[T1, T2, T3, T4], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple4[T1, T2, T3, T4] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4)
      }
    }


  implicit def tuple5[T1, T2, T3, T4, T5, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend]): Pickler[Tuple5[T1, T2, T3, T4, T5], Backend] =
    new Pickler[Tuple5[T1, T2, T3, T4, T5], Backend] {
      override def pickle(t: Tuple5[T1, T2, T3, T4, T5], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple5[T1, T2, T3, T4, T5] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5)
      }
    }


  implicit def tuple6[T1, T2, T3, T4, T5, T6, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend]): Pickler[Tuple6[T1, T2, T3, T4, T5, T6], Backend] =
    new Pickler[Tuple6[T1, T2, T3, T4, T5, T6], Backend] {
      override def pickle(t: Tuple6[T1, T2, T3, T4, T5, T6], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple6[T1, T2, T3, T4, T5, T6] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6)
      }
    }


  implicit def tuple7[T1, T2, T3, T4, T5, T6, T7, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend]): Pickler[Tuple7[T1, T2, T3, T4, T5, T6, T7], Backend] =
    new Pickler[Tuple7[T1, T2, T3, T4, T5, T6, T7], Backend] {
      override def pickle(t: Tuple7[T1, T2, T3, T4, T5, T6, T7], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple7[T1, T2, T3, T4, T5, T6, T7] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7)
      }
    }


  implicit def tuple8[T1, T2, T3, T4, T5, T6, T7, T8, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend]): Pickler[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8], Backend] =
    new Pickler[Tuple8[T1, T2, T3, T4, T5, T6, T7, T8], Backend] {
      override def pickle(t: Tuple8[T1, T2, T3, T4, T5, T6, T7, T8], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple8[T1, T2, T3, T4, T5, T6, T7, T8] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8)
      }
    }


  implicit def tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend]): Pickler[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9], Backend] =
    new Pickler[Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9], Backend] {
      override def pickle(t: Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9)
      }
    }


  implicit def tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend], tpickler10: Pickler[T10, Backend]): Pickler[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], Backend] =
    new Pickler[Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], Backend] {
      override def pickle(t: Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        tpickler10.pickle(t._10, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray
        val t10 = tpickler10.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
      }
    }


  implicit def tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend], tpickler10: Pickler[T10, Backend], tpickler11: Pickler[T11, Backend]): Pickler[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], Backend] =
    new Pickler[Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], Backend] {
      override def pickle(t: Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        tpickler10.pickle(t._10, writer)
        tpickler11.pickle(t._11, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray
        val t10 = tpickler10.unpickle(reader);
        reader.nextInArray
        val t11 = tpickler11.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
      }
    }


  implicit def tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend], tpickler10: Pickler[T10, Backend], tpickler11: Pickler[T11, Backend], tpickler12: Pickler[T12, Backend]): Pickler[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], Backend] =
    new Pickler[Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], Backend] {
      override def pickle(t: Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        tpickler10.pickle(t._10, writer)
        tpickler11.pickle(t._11, writer)
        tpickler12.pickle(t._12, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray
        val t10 = tpickler10.unpickle(reader);
        reader.nextInArray
        val t11 = tpickler11.unpickle(reader);
        reader.nextInArray
        val t12 = tpickler12.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
      }
    }


  implicit def tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend], tpickler10: Pickler[T10, Backend], tpickler11: Pickler[T11, Backend], tpickler12: Pickler[T12, Backend], tpickler13: Pickler[T13, Backend]): Pickler[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], Backend] =
    new Pickler[Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], Backend] {
      override def pickle(t: Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        tpickler10.pickle(t._10, writer)
        tpickler11.pickle(t._11, writer)
        tpickler12.pickle(t._12, writer)
        tpickler13.pickle(t._13, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray
        val t10 = tpickler10.unpickle(reader);
        reader.nextInArray
        val t11 = tpickler11.unpickle(reader);
        reader.nextInArray
        val t12 = tpickler12.unpickle(reader);
        reader.nextInArray
        val t13 = tpickler13.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)
      }
    }


  implicit def tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend], tpickler10: Pickler[T10, Backend], tpickler11: Pickler[T11, Backend], tpickler12: Pickler[T12, Backend], tpickler13: Pickler[T13, Backend], tpickler14: Pickler[T14, Backend]): Pickler[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], Backend] =
    new Pickler[Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], Backend] {
      override def pickle(t: Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        tpickler10.pickle(t._10, writer)
        tpickler11.pickle(t._11, writer)
        tpickler12.pickle(t._12, writer)
        tpickler13.pickle(t._13, writer)
        tpickler14.pickle(t._14, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray
        val t10 = tpickler10.unpickle(reader);
        reader.nextInArray
        val t11 = tpickler11.unpickle(reader);
        reader.nextInArray
        val t12 = tpickler12.unpickle(reader);
        reader.nextInArray
        val t13 = tpickler13.unpickle(reader);
        reader.nextInArray
        val t14 = tpickler14.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)
      }
    }


  implicit def tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend], tpickler10: Pickler[T10, Backend], tpickler11: Pickler[T11, Backend], tpickler12: Pickler[T12, Backend], tpickler13: Pickler[T13, Backend], tpickler14: Pickler[T14, Backend], tpickler15: Pickler[T15, Backend]): Pickler[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], Backend] =
    new Pickler[Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], Backend] {
      override def pickle(t: Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        tpickler10.pickle(t._10, writer)
        tpickler11.pickle(t._11, writer)
        tpickler12.pickle(t._12, writer)
        tpickler13.pickle(t._13, writer)
        tpickler14.pickle(t._14, writer)
        tpickler15.pickle(t._15, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray
        val t10 = tpickler10.unpickle(reader);
        reader.nextInArray
        val t11 = tpickler11.unpickle(reader);
        reader.nextInArray
        val t12 = tpickler12.unpickle(reader);
        reader.nextInArray
        val t13 = tpickler13.unpickle(reader);
        reader.nextInArray
        val t14 = tpickler14.unpickle(reader);
        reader.nextInArray
        val t15 = tpickler15.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)
      }
    }


  implicit def tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend], tpickler10: Pickler[T10, Backend], tpickler11: Pickler[T11, Backend], tpickler12: Pickler[T12, Backend], tpickler13: Pickler[T13, Backend], tpickler14: Pickler[T14, Backend], tpickler15: Pickler[T15, Backend], tpickler16: Pickler[T16, Backend]): Pickler[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], Backend] =
    new Pickler[Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], Backend] {
      override def pickle(t: Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        tpickler10.pickle(t._10, writer)
        tpickler11.pickle(t._11, writer)
        tpickler12.pickle(t._12, writer)
        tpickler13.pickle(t._13, writer)
        tpickler14.pickle(t._14, writer)
        tpickler15.pickle(t._15, writer)
        tpickler16.pickle(t._16, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray
        val t10 = tpickler10.unpickle(reader);
        reader.nextInArray
        val t11 = tpickler11.unpickle(reader);
        reader.nextInArray
        val t12 = tpickler12.unpickle(reader);
        reader.nextInArray
        val t13 = tpickler13.unpickle(reader);
        reader.nextInArray
        val t14 = tpickler14.unpickle(reader);
        reader.nextInArray
        val t15 = tpickler15.unpickle(reader);
        reader.nextInArray
        val t16 = tpickler16.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16)
      }
    }


  implicit def tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend], tpickler10: Pickler[T10, Backend], tpickler11: Pickler[T11, Backend], tpickler12: Pickler[T12, Backend], tpickler13: Pickler[T13, Backend], tpickler14: Pickler[T14, Backend], tpickler15: Pickler[T15, Backend], tpickler16: Pickler[T16, Backend], tpickler17: Pickler[T17, Backend]): Pickler[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], Backend] =
    new Pickler[Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], Backend] {
      override def pickle(t: Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        tpickler10.pickle(t._10, writer)
        tpickler11.pickle(t._11, writer)
        tpickler12.pickle(t._12, writer)
        tpickler13.pickle(t._13, writer)
        tpickler14.pickle(t._14, writer)
        tpickler15.pickle(t._15, writer)
        tpickler16.pickle(t._16, writer)
        tpickler17.pickle(t._17, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray
        val t10 = tpickler10.unpickle(reader);
        reader.nextInArray
        val t11 = tpickler11.unpickle(reader);
        reader.nextInArray
        val t12 = tpickler12.unpickle(reader);
        reader.nextInArray
        val t13 = tpickler13.unpickle(reader);
        reader.nextInArray
        val t14 = tpickler14.unpickle(reader);
        reader.nextInArray
        val t15 = tpickler15.unpickle(reader);
        reader.nextInArray
        val t16 = tpickler16.unpickle(reader);
        reader.nextInArray
        val t17 = tpickler17.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)
      }
    }


  implicit def tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend], tpickler10: Pickler[T10, Backend], tpickler11: Pickler[T11, Backend], tpickler12: Pickler[T12, Backend], tpickler13: Pickler[T13, Backend], tpickler14: Pickler[T14, Backend], tpickler15: Pickler[T15, Backend], tpickler16: Pickler[T16, Backend], tpickler17: Pickler[T17, Backend], tpickler18: Pickler[T18, Backend]): Pickler[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], Backend] =
    new Pickler[Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], Backend] {
      override def pickle(t: Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        tpickler10.pickle(t._10, writer)
        tpickler11.pickle(t._11, writer)
        tpickler12.pickle(t._12, writer)
        tpickler13.pickle(t._13, writer)
        tpickler14.pickle(t._14, writer)
        tpickler15.pickle(t._15, writer)
        tpickler16.pickle(t._16, writer)
        tpickler17.pickle(t._17, writer)
        tpickler18.pickle(t._18, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray
        val t10 = tpickler10.unpickle(reader);
        reader.nextInArray
        val t11 = tpickler11.unpickle(reader);
        reader.nextInArray
        val t12 = tpickler12.unpickle(reader);
        reader.nextInArray
        val t13 = tpickler13.unpickle(reader);
        reader.nextInArray
        val t14 = tpickler14.unpickle(reader);
        reader.nextInArray
        val t15 = tpickler15.unpickle(reader);
        reader.nextInArray
        val t16 = tpickler16.unpickle(reader);
        reader.nextInArray
        val t17 = tpickler17.unpickle(reader);
        reader.nextInArray
        val t18 = tpickler18.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)
      }
    }


  implicit def tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend], tpickler10: Pickler[T10, Backend], tpickler11: Pickler[T11, Backend], tpickler12: Pickler[T12, Backend], tpickler13: Pickler[T13, Backend], tpickler14: Pickler[T14, Backend], tpickler15: Pickler[T15, Backend], tpickler16: Pickler[T16, Backend], tpickler17: Pickler[T17, Backend], tpickler18: Pickler[T18, Backend], tpickler19: Pickler[T19, Backend]): Pickler[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], Backend] =
    new Pickler[Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], Backend] {
      override def pickle(t: Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        tpickler10.pickle(t._10, writer)
        tpickler11.pickle(t._11, writer)
        tpickler12.pickle(t._12, writer)
        tpickler13.pickle(t._13, writer)
        tpickler14.pickle(t._14, writer)
        tpickler15.pickle(t._15, writer)
        tpickler16.pickle(t._16, writer)
        tpickler17.pickle(t._17, writer)
        tpickler18.pickle(t._18, writer)
        tpickler19.pickle(t._19, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray
        val t10 = tpickler10.unpickle(reader);
        reader.nextInArray
        val t11 = tpickler11.unpickle(reader);
        reader.nextInArray
        val t12 = tpickler12.unpickle(reader);
        reader.nextInArray
        val t13 = tpickler13.unpickle(reader);
        reader.nextInArray
        val t14 = tpickler14.unpickle(reader);
        reader.nextInArray
        val t15 = tpickler15.unpickle(reader);
        reader.nextInArray
        val t16 = tpickler16.unpickle(reader);
        reader.nextInArray
        val t17 = tpickler17.unpickle(reader);
        reader.nextInArray
        val t18 = tpickler18.unpickle(reader);
        reader.nextInArray
        val t19 = tpickler19.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)
      }
    }


  implicit def tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend], tpickler10: Pickler[T10, Backend], tpickler11: Pickler[T11, Backend], tpickler12: Pickler[T12, Backend], tpickler13: Pickler[T13, Backend], tpickler14: Pickler[T14, Backend], tpickler15: Pickler[T15, Backend], tpickler16: Pickler[T16, Backend], tpickler17: Pickler[T17, Backend], tpickler18: Pickler[T18, Backend], tpickler19: Pickler[T19, Backend], tpickler20: Pickler[T20, Backend]): Pickler[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], Backend] =
    new Pickler[Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], Backend] {
      override def pickle(t: Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        tpickler10.pickle(t._10, writer)
        tpickler11.pickle(t._11, writer)
        tpickler12.pickle(t._12, writer)
        tpickler13.pickle(t._13, writer)
        tpickler14.pickle(t._14, writer)
        tpickler15.pickle(t._15, writer)
        tpickler16.pickle(t._16, writer)
        tpickler17.pickle(t._17, writer)
        tpickler18.pickle(t._18, writer)
        tpickler19.pickle(t._19, writer)
        tpickler20.pickle(t._20, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray
        val t10 = tpickler10.unpickle(reader);
        reader.nextInArray
        val t11 = tpickler11.unpickle(reader);
        reader.nextInArray
        val t12 = tpickler12.unpickle(reader);
        reader.nextInArray
        val t13 = tpickler13.unpickle(reader);
        reader.nextInArray
        val t14 = tpickler14.unpickle(reader);
        reader.nextInArray
        val t15 = tpickler15.unpickle(reader);
        reader.nextInArray
        val t16 = tpickler16.unpickle(reader);
        reader.nextInArray
        val t17 = tpickler17.unpickle(reader);
        reader.nextInArray
        val t18 = tpickler18.unpickle(reader);
        reader.nextInArray
        val t19 = tpickler19.unpickle(reader);
        reader.nextInArray
        val t20 = tpickler20.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)
      }
    }


  implicit def tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend], tpickler10: Pickler[T10, Backend], tpickler11: Pickler[T11, Backend], tpickler12: Pickler[T12, Backend], tpickler13: Pickler[T13, Backend], tpickler14: Pickler[T14, Backend], tpickler15: Pickler[T15, Backend], tpickler16: Pickler[T16, Backend], tpickler17: Pickler[T17, Backend], tpickler18: Pickler[T18, Backend], tpickler19: Pickler[T19, Backend], tpickler20: Pickler[T20, Backend], tpickler21: Pickler[T21, Backend]): Pickler[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], Backend] =
    new Pickler[Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], Backend] {
      override def pickle(t: Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        tpickler10.pickle(t._10, writer)
        tpickler11.pickle(t._11, writer)
        tpickler12.pickle(t._12, writer)
        tpickler13.pickle(t._13, writer)
        tpickler14.pickle(t._14, writer)
        tpickler15.pickle(t._15, writer)
        tpickler16.pickle(t._16, writer)
        tpickler17.pickle(t._17, writer)
        tpickler18.pickle(t._18, writer)
        tpickler19.pickle(t._19, writer)
        tpickler20.pickle(t._20, writer)
        tpickler21.pickle(t._21, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray
        val t10 = tpickler10.unpickle(reader);
        reader.nextInArray
        val t11 = tpickler11.unpickle(reader);
        reader.nextInArray
        val t12 = tpickler12.unpickle(reader);
        reader.nextInArray
        val t13 = tpickler13.unpickle(reader);
        reader.nextInArray
        val t14 = tpickler14.unpickle(reader);
        reader.nextInArray
        val t15 = tpickler15.unpickle(reader);
        reader.nextInArray
        val t16 = tpickler16.unpickle(reader);
        reader.nextInArray
        val t17 = tpickler17.unpickle(reader);
        reader.nextInArray
        val t18 = tpickler18.unpickle(reader);
        reader.nextInArray
        val t19 = tpickler19.unpickle(reader);
        reader.nextInArray
        val t20 = tpickler20.unpickle(reader);
        reader.nextInArray
        val t21 = tpickler21.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21)
      }
    }


  implicit def tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, Backend <: PicklingBackend](implicit tpickler1: Pickler[T1, Backend], tpickler2: Pickler[T2, Backend], tpickler3: Pickler[T3, Backend], tpickler4: Pickler[T4, Backend], tpickler5: Pickler[T5, Backend], tpickler6: Pickler[T6, Backend], tpickler7: Pickler[T7, Backend], tpickler8: Pickler[T8, Backend], tpickler9: Pickler[T9, Backend], tpickler10: Pickler[T10, Backend], tpickler11: Pickler[T11, Backend], tpickler12: Pickler[T12, Backend], tpickler13: Pickler[T13, Backend], tpickler14: Pickler[T14, Backend], tpickler15: Pickler[T15, Backend], tpickler16: Pickler[T16, Backend], tpickler17: Pickler[T17, Backend], tpickler18: Pickler[T18, Backend], tpickler19: Pickler[T19, Backend], tpickler20: Pickler[T20, Backend], tpickler21: Pickler[T21, Backend], tpickler22: Pickler[T22, Backend]): Pickler[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22], Backend] =
    new Pickler[Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22], Backend] {
      override def pickle(t: Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22], writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        tpickler1.pickle(t._1, writer)
        tpickler2.pickle(t._2, writer)
        tpickler3.pickle(t._3, writer)
        tpickler4.pickle(t._4, writer)
        tpickler5.pickle(t._5, writer)
        tpickler6.pickle(t._6, writer)
        tpickler7.pickle(t._7, writer)
        tpickler8.pickle(t._8, writer)
        tpickler9.pickle(t._9, writer)
        tpickler10.pickle(t._10, writer)
        tpickler11.pickle(t._11, writer)
        tpickler12.pickle(t._12, writer)
        tpickler13.pickle(t._13, writer)
        tpickler14.pickle(t._14, writer)
        tpickler15.pickle(t._15, writer)
        tpickler16.pickle(t._16, writer)
        tpickler17.pickle(t._17, writer)
        tpickler18.pickle(t._18, writer)
        tpickler19.pickle(t._19, writer)
        tpickler20.pickle(t._20, writer)
        tpickler21.pickle(t._21, writer)
        tpickler22.pickle(t._22, writer)
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): Tuple22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22] = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        val t1 = tpickler1.unpickle(reader);
        reader.nextInArray
        val t2 = tpickler2.unpickle(reader);
        reader.nextInArray
        val t3 = tpickler3.unpickle(reader);
        reader.nextInArray
        val t4 = tpickler4.unpickle(reader);
        reader.nextInArray
        val t5 = tpickler5.unpickle(reader);
        reader.nextInArray
        val t6 = tpickler6.unpickle(reader);
        reader.nextInArray
        val t7 = tpickler7.unpickle(reader);
        reader.nextInArray
        val t8 = tpickler8.unpickle(reader);
        reader.nextInArray
        val t9 = tpickler9.unpickle(reader);
        reader.nextInArray
        val t10 = tpickler10.unpickle(reader);
        reader.nextInArray
        val t11 = tpickler11.unpickle(reader);
        reader.nextInArray
        val t12 = tpickler12.unpickle(reader);
        reader.nextInArray
        val t13 = tpickler13.unpickle(reader);
        reader.nextInArray
        val t14 = tpickler14.unpickle(reader);
        reader.nextInArray
        val t15 = tpickler15.unpickle(reader);
        reader.nextInArray
        val t16 = tpickler16.unpickle(reader);
        reader.nextInArray
        val t17 = tpickler17.unpickle(reader);
        reader.nextInArray
        val t18 = tpickler18.unpickle(reader);
        reader.nextInArray
        val t19 = tpickler19.unpickle(reader);
        reader.nextInArray
        val t20 = tpickler20.unpickle(reader);
        reader.nextInArray
        val t21 = tpickler21.unpickle(reader);
        reader.nextInArray
        val t22 = tpickler22.unpickle(reader);
        reader.nextInArray

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22)
      }
    }
}

object TuplePicklersGen {
  /** Generates the Scala code for all 22 tuple widths */
  def generatePickler(size: Int): String = {
    val ts = (1 to size) map (i => s"T$i") mkString (", ")
    val picklerParams = (1 to size) map (i => s"tpickler$i: Pickler[T$i, Backend]") mkString (", ")
    val tuple = s"Tuple$size[$ts]"

    s"""implicit def tuple$size[$ts, Backend <: PicklingBackend](implicit $picklerParams): Pickler[$tuple, Backend] =
    new Pickler[$tuple, Backend] {
      override def pickle(t: $tuple, writer: Backend#PickleWriter, emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        ${(1 to size) map (i => s"tpickler$i.pickle(t._$i, writer)") mkString ("\n        ")}
        writer.writeArrayEnd()
      }

      override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean): $tuple = {
        if (reader.tokenType != TokenType.ArrayStart) throw new UnexpectedEofException("array start")
        reader.nextInArray()

        ${(1 to size) map (i => s"val t$i = tpickler$i.unpickle(reader); reader.nextInArray") mkString ("\n        ")}

        if (reader.tokenType != TokenType.ArrayEnd) throw new UnexpectedEofException("array end")

        Tuple$size(${(1 to size) map (i => s"t$i") mkString (", ")})
      }
    }
    """
  }

  def main(args: Array[String]): Unit = {
    for (i <- 1 to 22) {
      val pickler = generatePickler(i)
      println(pickler)
      println()
    }
  }
}
