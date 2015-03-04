package com.fsist.safepickle.jackson

import com.fsist.safepickle.{jackson, Autogen}
import com.fsist.safepickle.jackson.JacksonPicklerBackendTest.Foo
import org.scalatest.FunSuite

object JacksonPicklerBackendTest {
  case class Foo(str: String)
  object Foo {
    implicit val pickler = Autogen[Foo]
  }
}

class JacksonPicklerBackendTest extends FunSuite {
  test("JSON reading and writing") {
    val foo = Foo("bar")
    val json = JacksonPicklerBackend.String.write(foo)
//    val expected = "{\n  \"str\" : \"bar\"\n})"
//    assert(json == expected)
    val foo2 = JacksonPicklerBackend.String.read[Foo](json)
    assert(foo == foo2)
  }
}

