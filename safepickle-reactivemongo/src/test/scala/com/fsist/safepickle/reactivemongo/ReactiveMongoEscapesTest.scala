package com.fsist.safepickle.reactivemongo

import org.scalatest.FunSuite

class ReactiveMongoEscapesTest extends FunSuite {
  import ReactiveMongoEscapes._

  private def test(in: String, expected: String): Unit = {
    val escaped = escapeAttributeName(in)
    assert(escaped == expected, "Escape correctly")
    val unescaped = unescapeAttributeName(escaped)
    assert(unescaped == in, "Unescaped correctly")
  }

  test("Escaping") {
    test("foobar123", "foobar123")
    test("", "")
    test("$type", "＄type")
    test("a.b", "a．b")
  }
}
