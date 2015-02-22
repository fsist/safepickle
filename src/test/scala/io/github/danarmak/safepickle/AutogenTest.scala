package io.github.danarmak.safepickle

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
}

class AutogenTest extends FunSuite with WrapperTester {
  import AutogenTest._

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

  test("Class parameter of type Option[T]"){
    roundtrip(
      C5(Some(1)),
      ObjectWrapper(Map("opt" -> IntWrapper(1)))
    )

    roundtrip(
      C5(None),
      ObjectWrapper(Map())
    )
  }
}
