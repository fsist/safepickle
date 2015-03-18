package com.fsist.safepickle

import org.scalatest.FunSuite

object AutogenVersionedTest {
  object Scope1 {
    case class Old(s: String) extends OldVersion[New] {
      override def toNewVersion: New = New(s.toInt)
    }

    case class New(i: Int)
    object New {
      implicit val pickler : Pickler[New] = Autogen.versioned[New, Old]
    }
  }

  object Scope2 {
    case class V1(s: String) extends OldVersion[V2] {
      override def toNewVersion: V2 = V2(true)
    }

    case class V2(b: Boolean) extends OldVersion[V3] {
      override def toNewVersion: V3 = V3(123L)
    }

    case class V3(l: Long) extends OldVersion[New] {
      override def toNewVersion: New = New(l.toInt)
    }

    case class New(i: Int)
    object New {
      implicit val pickler : Pickler[New] = Autogen.versionedDebug[New, V1]
    }
  }
}

class AutogenVersionedTest extends FunSuite with WrapperTester {
  import AutogenVersionedTest._

  def testReading[T](pickled: Wrapper, expected: T)(implicit pickler: Pickler[T]) =  {
    val reader = WrapperBackend.reader(pickled)
    val value = reader.read[T]()
    assert(value == expected)
  }

  test("Versioned") {
    import Scope1._

    roundtrip(
      New(123),
      ObjectWrapper(Map(
        "$version" -> IntWrapper(2),
        "i" -> IntWrapper(123)
      ))
    )

    testReading[New](
      ObjectWrapper(Map(
        "$version" -> IntWrapper(1),
        "s" -> StringWrapper("4")
      )),
      New(4)
    )
  }

  test("Multiple versions") {
    import Scope2._

    roundtrip(
      New(123),
      ObjectWrapper(Map(
        "$version" -> IntWrapper(4),
        "i" -> IntWrapper(123)
      ))
    )

    testReading[New](
      ObjectWrapper(Map(
        "$version" -> IntWrapper(1),
        "s" -> StringWrapper("4")
      )),
      New(123)
    )

    testReading[New](
      ObjectWrapper(Map(
        "$version" -> IntWrapper(2),
        "b" -> BooleanWrapper(false)
      )),
      New(123)
    )

    testReading[New](
      ObjectWrapper(Map(
        "$version" -> IntWrapper(3),
        "l" -> LongWrapper(123L)
      )),
      New(123)
    )
  }
}
