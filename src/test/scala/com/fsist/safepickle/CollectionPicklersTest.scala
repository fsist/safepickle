package com.fsist.safepickle

import org.scalatest.FunSuite

class CollectionPicklersTest extends FunSuite with WrapperTester {
  test("Iterables") {
    val xs = Seq(1,2,3)
    val wrapper = ArrayWrapper(xs.map(IntWrapper(_)))

    roundtrip(xs.toIterable, wrapper)
    roundtrip(xs.toSeq, wrapper)
    roundtrip(xs.toList, wrapper)
    roundtrip(xs.toVector, wrapper)
    roundtrip(xs.toSet, wrapper)
    roundtrip(xs.toArray, wrapper, Some((arr1 : Array[Int], arr2: Array[Int]) => arr1.toSeq == arr2.toSeq))
  }

  test("string map") {
    roundtrip(
      Map("a" -> 1, "b" -> 2),
      ObjectWrapper(Map(
        "a" -> IntWrapper(1),
        "b" -> IntWrapper(2)
      ))
    )
  }

  test("tuple") {
    roundtrip(
      (1, "a", false),
      ArrayWrapper(Seq(
        IntWrapper(1),
        StringWrapper("a"),
        BooleanWrapper(false)
      ))
    )
  }

  test("any map") {
    roundtrip(
      Map(1 -> 2, 3 -> 4),
      ArrayWrapper(Seq(
        ArrayWrapper(Seq(IntWrapper(1), IntWrapper(2))),
        ArrayWrapper(Seq(IntWrapper(3), IntWrapper(4)))
      ))
    )
  }
}
