package com.fsist.safepickle

import org.scalatest.FunSuite

// NOTE: must come before the class, so that macros can see the knownDirectSubclasses of traits
object AutogenTest {
  case class C1(s: String, i: Int)

  class C2(val s: String, val i: Int) {
    override def equals(other: Any): Boolean = other.isInstanceOf[C2] && {
      val otherc = other.asInstanceOf[C2]
      otherc.s == s && otherc.i == i
    }
    override def hashCode(): Int = s.hashCode * i
  }

  case object O1

  object O2

  case class C3()

  class C4 {
    override def equals(other: Any): Boolean = other.isInstanceOf[C4]
    override def hashCode(): Int = 0
  }

  sealed trait T1
  object T1 {
    case class C(s: String) extends T1
    case class D(i: Int) extends T1
    case object O extends T1
  }

  case class C5(opt: Option[Int])

  case class C6(i: Int = 5)

  sealed trait T2
  sealed trait T3 extends T2
  case class C7(i: Int) extends T3

  sealed abstract class C8(i: Int)
  case class C9(i: Int) extends C8(i)

  case class C10(i: Int)
  object C10 {
    implicit object pickler extends Pickler[C10] {
      override def pickle(t: C10, writer: PickleWriter[_], emitObjectStart: Boolean): Unit = {
        writer.writeInt(t.i)
      }
      override def unpickle(reader: PickleReader, expectObjectStart: Boolean): C10 =
        C10(reader.int)
    }
  }

  case class C11(c10: C10)

  case class C12(@Name("bar") foo: String)

  case class C13(s: String)
  case class C14(c: C13)
}

class AutogenTest extends FunSuite with WrapperTester {

  import AutogenTest._
  import DefaultPicklers._
  import Autogen.Implicits._

  test("Case class") {
    roundtrip(
      C1("foo", 123),
      ObjectWrapper(Map(
        "s" -> StringWrapper("foo"),
        "i" -> IntWrapper(123)
      ))
    )
  }

  test("Non-case class") {
    roundtrip(
      new C2("foo", 123),
      ObjectWrapper(Map(
        "s" -> StringWrapper("foo"),
        "i" -> IntWrapper(123)
      ))
    )
  }

  test("Case object") {
    roundtrip(
      O1,
      StringWrapper("O1")
    )
  }

  test("Non-case object") {
    roundtrip(
      O2,
      StringWrapper("O2")
    )
  }

  test("Case class with zero parameters") {
    roundtrip(
      C3(),
      StringWrapper("C3")
    )
  }

  test("Class with zero argument lists") {
    roundtrip(
      new C4,
      StringWrapper("C4")
    )
  }

  test("Sealed trait") {
    roundtrip[T1](
      T1.C("foo"),
      ObjectWrapper(Map(
        "$type" -> StringWrapper("C"),
        "s" -> StringWrapper("foo")
      ))
    )

    roundtrip[T1](
      T1.O,
      StringWrapper("O")
    )
  }

  test("Class parameter of type Option[T]") {
    roundtrip(
      C5(Some(1)),
      ObjectWrapper(Map("opt" -> IntWrapper(1)))
    )

    roundtrip(
      C5(None),
      ObjectWrapper(Map())
    )
  }

  test("Param with default value") {
    roundtrip(
      C6(6),
      ObjectWrapper(Map("i" -> IntWrapper(6)))
    )

    roundtrip(
      C6(),
      ObjectWrapper(Map())
    )
  }

  test("Sealed trait extending sealed trait") {
    roundtrip[T2](
      C7(123),
      ObjectWrapper(Map(
        "$type" -> StringWrapper("C7"),
        "i" -> IntWrapper(123)
      ))
    )
  }

  test("Abstract sealed class") {
    roundtrip[C8](
      C9(123),
      ObjectWrapper(Map(
        "$type" -> StringWrapper("C9"),
        "i" -> IntWrapper(123)
      ))
    )

    roundtrip(
      C9(123),
      ObjectWrapper(Map(
        "i" -> IntWrapper(123)
      ))
    )
  }

  test("Explicit custom pickler in companion object overrides autogen") {
    roundtrip(
      C10(123),
      IntWrapper(123)
    )
  }

  test("Autogenerated pickler uses explicit pickler from companion object") {
    roundtrip(
      C11(C10(123)),
      ObjectWrapper(Map(
        "c10" -> IntWrapper(123)
      ))
    )
  }

  test("Autogenerated pickler uses explicit pickler defined locally") {
    implicit object pickler extends Pickler[C13] {
      override def pickle(t: C13, writer: PickleWriter[_], emitObjectStart: Boolean): Unit = {
        writer.writeString(t.s)
      }
      override def unpickle(reader: PickleReader, expectObjectStart: Boolean): C13 =
        C13(reader.string)
    }

    roundtrip(
      C14(C13("foo")),
      ObjectWrapper(Map(
        "c14" -> StringWrapper("foo")
      ))
    )
  }

  test("Name annotation") {
    roundtrip(
      C12("x"),
      ObjectWrapper(Map(
        "bar" -> StringWrapper("x")
      ))
    )
  }
}

