package io.github.danarmak.safepickle

import org.scalatest.FunSuite

import scala.collection.generic.CanBuildFrom

class CollectionPicklersTest extends FunSuite with WrapperTester {
  import WrapperBackend.picklers._

  test("Iterable types") {
    val xs = Seq(1,2,3)
    val wrapper = ArrayWrapper(xs.map(IntWrapper(_)))

    roundtrip(xs, wrapper)
  }
}
