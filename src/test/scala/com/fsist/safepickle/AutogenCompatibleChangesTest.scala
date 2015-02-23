package com.fsist.safepickle

import org.scalatest.FunSuite

class AutogenCompatibleChangesTest extends FunSuite with WrapperTester {
  import AutogenCompatibleChangesTest._
  import WrapperBackend.picklers._

  def assertEqualPickle[A, B](a: A, b: B)(implicit apickler: Pickler[A, PicklingBackend], bpickler: Pickler[B, PicklingBackend]): Unit = {
    val awriter = WrapperBackend.writer()
    apickler.pickle(a, awriter)
    val awrapper = awriter.result()

    val bwriter = WrapperBackend.writer()
    bpickler.pickle(b, bwriter)
    val bwrapper = bwriter.result()

    assert(awrapper == bwrapper)
  }

  /** Pickles `orig` using `apickler`, unpickles it using `bpickler`, and checks that the result equals `expected`. */
  def roundtrip2[A, B](orig: A, expected: B)(implicit apickler: Pickler[A, PicklingBackend], bpickler: Pickler[B, PicklingBackend]): Unit = {
    val writer = WrapperBackend.writer()
    apickler.pickle(orig, writer)
    val areader = WrapperBackend.reader(writer.result())
    val b = bpickler.unpickle(areader)

    assert(b == expected)
  }

  test("The class name doesn't matter when not pickling a sealed trait") {
    assertEqualPickle(Scope7.C1(123), Scope7.C2(123))
  }

  test("Classes without parameters are compatible with objects") {
    assertEqualPickle(new Scope1.C, new Scope2.C)
    assertEqualPickle(new Scope1.C, Scope3.C)
  }

  test("Case and non-case objects and classes are compatible") {
    assertEqualPickle(Scope4.C1(123), new Scope4.C2(123))
    assertEqualPickle(Scope4.C1(123), new Scope4.C3(123))
    assertEqualPickle(Scope4.O, Scope5.O)
  }

  test("Add param with default value, at any position") {
    roundtrip2(Scope6.C1(123), Scope6.C2(123, "foo"))
    roundtrip2(Scope6.C2(123, "bar"), Scope6.C1(123))

    roundtrip2(Scope6.C1(123), Scope6.C3("foo", 123))
    roundtrip2(Scope6.C3("foo", 123), Scope6.C1(123))

    roundtrip2(Scope6.C2(123, "foo"), Scope6.C3("foo", 123))
    roundtrip2(Scope6.C3("foo", 123), Scope6.C2(123, "foo"))
  }

  test("Change parameter order") {
    roundtrip2(Scope9.C1(123, "foo"), Scope9.C2("foo", 123))
  }

  test("Change iterable type") {
    val xs = Seq(1,2,3)

    roundtrip2(Scope10.C1(xs.toList), Scope10.C2(xs.toVector))
    roundtrip2(Scope10.C1(xs.toList), Scope10.C3(xs.toSet))
    roundtrip2(Scope10.C1(xs.toList), Scope10.C4(xs.toIterable))
  }

  test("Make param with default value optional") {
    roundtrip2(Scope11.C1(5), Scope11.C2(Some(5)))
    roundtrip2(Scope11.C2(None), Scope11.C1(8))
  }
}

object AutogenCompatibleChangesTest {

  // Note: classes within objects have to live here in the companion object, and cannot be defined inside the test
  // methods, because that creates types the Autogen macro legitimately can't handle.
  // This includes classes with default arguments, because the default argument definitions are generated
  // on the companion object.

  object Scope1 {
    class C
  }
  object Scope2 {
    class C()
  }
  object Scope3 {
    object C
  }

  object Scope4 {
    case class C1(i: Int)
    class C2(val i: Int)
    class C3(var i: Int)

    object O
  }
  object Scope5 {
    case object O
  }

  object Scope6 {
    case class C1(i: Int)
    case class C2(i: Int, s: String = "foo")
    case class C3(s: String = "foo", i: Int)
  }

  object Scope7 {
    case class C1(i: Int)
    case class C2(i: Int)
  }

  object Scope8 {
    case class C1(i: Int = 1)
    case class C2(i: Int = 2)
  }

  object Scope9 {
    case class C1(i: Int, s: String)
    case class C2(s: String, i: Int)
  }

  object Scope10 {
    case class C1(ints: List[Int])
    case class C2(ints: Vector[Int])
    case class C3(ints: Set[Int])
    case class C4(ints: Iterable[Int])
  }

  object Scope11 {
    case class C1(i: Int = 8)
    case class C2(i: Option[Int])
  }
}
