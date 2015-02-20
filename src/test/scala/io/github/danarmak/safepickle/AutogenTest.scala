package io.github.danarmak.safepickle

import org.scalatest.FunSuite

class AutogenTest extends FunSuite {
  def roundtrip[T](value: T, expectedWrapper: Wrapper)(implicit pickler: Pickler[T, PicklingBackend]): Unit = {
    val writer = WrapperBackend.writer()
    pickler.pickle(value, writer)
    val wrapper = writer.result()
    assert(wrapper == expectedWrapper)
    val reader = WrapperBackend.reader(wrapper)
    val read = pickler.unpickle(reader)
    assert(read == value)
  }
  
  test("Case class") {
    case class C(s: String, i: Int)
    
    roundtrip(
      C("foo", 123),
      ObjectWrapper(Map(
        "s" -> StringWrapper("foo"),
        "i" -> IntWrapper(123)
      ))
    )
  }

  test("Non-case class") {
    class C(val s: String, val i: Int) {
      override def equals(other: Any): Boolean = other.isInstanceOf[C] && {
        val otherc = other.asInstanceOf[C]        
        otherc.s == s && otherc.i == i
      }
      override def hashCode(): Int = s.hashCode * i
    }

    roundtrip(
      new C("foo", 123),
      ObjectWrapper(Map(
        "s" -> StringWrapper("foo"),
        "i" -> IntWrapper(123)
      ))
    )
  }
  
  test("Case object") {
    case object O

    roundtrip(
      O,
      StringWrapper("O")
    )
  }

  test("Non-case object") {
    object O

    roundtrip(
      O,
      StringWrapper("O")
    )
  }
  
  test("Case class with zero parameters") {
    case class C() 

    roundtrip(
      C(),
      StringWrapper("C")
    )
  }

  test("Class with zero argument lists") {
    class C {
      override def equals(other: Any): Boolean = other.isInstanceOf[C]
      override def hashCode(): Int = 0
    }
    
    roundtrip(
      new C,
      StringWrapper("C")
    )
  }
  
  test("Sealed trait") {
    sealed trait T
    case class C(s: String) extends T
    case class D(i: Int) extends T
    case object O extends T
    
    roundtrip[T](
      C("foo"),
      ObjectWrapper(Map(
        "$type" -> StringWrapper("C"),
        "s" -> StringWrapper("foo")
      ))
    )

    roundtrip[T](
      O,
      StringWrapper("O")
    )
  }
}
