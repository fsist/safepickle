package com.fsist.safepickle

import com.fsist.safepickle.Autogen.|
import org.scalatest.FunSuite

// NOTE: must come before the class, so that macros can see the knownDirectSubclasses of traits
object AutogenTest {
  case class C1(s: String, is: List[Int])
  object C1 {
    implicit val pickler = Autogen[C1]
  }

  class C2(val s: String, val i: Int) {
    override def equals(other: Any): Boolean = other.isInstanceOf[C2] && {
      val otherc = other.asInstanceOf[C2]
      otherc.s == s && otherc.i == i
    }
    override def hashCode(): Int = s.hashCode * i
  }
  object C2 {
    implicit val pickler = Autogen[C2]
  }

  case object O1 {
    implicit val pickler = Autogen[O1.type]
  }

  object O2 {
    implicit val pickler = Autogen[O2.type]
  }

  case class C3()
  object C3 {
    implicit val pickler = Autogen[C3]
  }

  class C4 {
    override def equals(other: Any): Boolean = other.isInstanceOf[C4]
    override def hashCode(): Int = 0
  }
  object C4 {
    implicit val pickler = Autogen[C4]
  }

  sealed trait T1
  object T1 {
    case class C(s: String) extends T1
    case class D(i: Int) extends T1
    case object O extends T1

    implicit val pickler = Autogen.children[T1, C | D | O.type]
  }

  case class C5(opt: Option[Int])
  object C5 {
    implicit val pickler = Autogen[C5]
  }

  case class C6(i: Int = 5)
  object C6 {
    implicit val pickler = Autogen[C6]
  }

  sealed trait T2
  object T2 {
    private implicit val t3picker : Pickler[T3] = T3.pickler
    implicit val pickler = Autogen.children[T2, T3]
  }

  sealed trait T3 extends T2
  object T3 {
    implicit val pickler = Autogen.children[T3, C7]
  }

  case class C7(i: Int) extends T3
  object C7 {
    implicit val pickler = Autogen[C7]
  }

  sealed abstract class C8(i: Int)
  object C8 {
    implicit val pickler = Autogen.children[C8, C9]
  }
  case class C9(i: Int) extends C8(i)
  object C9 {
    implicit val pickler = Autogen[C9]
  }

  case class C10(i: Int)
  object C10 {
    implicit val pickler = new Pickler[C10] {
      override def pickle(t: C10, writer: PickleWriter[_], emitObjectStart: Boolean): Unit = {
        writer.writeInt(t.i)
      }
      override def unpickle(reader: PickleReader, expectObjectStart: Boolean): C10 = {
        C10(reader.int)
      }
      override def toString(): String = "C10 custom pickler"
    }
  }

  case class C11(c10: C10)
  object C11 {
    implicit val pickler = Autogen[C11]
  }

  case class C12(@Name("bar") foo: String)
  object C12 {
    implicit val pickler = Autogen[C12]
  }

  case class C13(s: String)
  object C13 {
    implicit val pickler = Autogen[C13]
  }

  case class C14(c: C13)
  object C14 {
    implicit val pickler = Autogen[C14]
  }
}

class AutogenTest extends FunSuite with WrapperTester {

  import AutogenTest._

  test("Case class") {
    roundtrip(
      C1("foo", List(123, 456)),
      ObjectWrapper(Map(
        "s" -> StringWrapper("foo"),
        "i" -> ArrayWrapper(List(
          IntWrapper(123), IntWrapper(456)
        ))
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
        "$type" -> StringWrapper("T3"),
        "$value" -> ObjectWrapper(Map(
          "$type" -> StringWrapper("C7"),
          "i" -> IntWrapper(123)
        ))
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

