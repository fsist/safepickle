package io.github.danarmak.safepickle

import org.scalatest.FunSuite

import scala.collection.generic.CanBuildFrom

class CollectionPicklersTest extends FunSuite with WrapperTester {
  import WrapperBackend.picklers._

  test("Iterable types") {
    val xs = Seq(1,2,3)
    val wrapper = ArrayWrapper(xs.map(IntWrapper(_)))

    roundtrip(xs.toIterable, wrapper)
    roundtrip(xs.toSeq, wrapper)
    roundtrip(xs.toList, wrapper)
    roundtrip(xs.toVector, wrapper)
    roundtrip(xs.toSet, wrapper)
    roundtrip(xs.toArray, wrapper, Some((arr1 : Array[Int], arr2: Array[Int]) => arr1.toSeq == arr2.toSeq))
  }
}
