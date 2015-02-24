package com.fsist.safepickle

import org.scalatest.FunSuite

class TreePickleReaderTest extends FunSuite {
  def reader(value: Wrapper) = WrapperBackend.reader(value)

  test("Read string") {
    val str = StringWrapper("foo")
    val r = reader(str)
    assert(r.tokenType == TokenType.String)
    assert(r.string == "foo")
    assert(! r.next())
  }

  test("Read int") {
    val int = IntWrapper(1234)
    val r = reader(int)
    assert(r.tokenType == TokenType.Int)
    assert(r.int == 1234)
    assert(! r.next())
  }

  test("Read float") {
    val float = FloatWrapper(123.123f)
    val r = reader(float)
    assert(r.tokenType == TokenType.Float)
    assert(r.float == 123.123f)
    assert(! r.next())
  }

  test("Read long") {
    val long = LongWrapper(12321L)
    val r = reader(long)
    assert(r.tokenType == TokenType.Long)
    assert(r.long == 12321L)
    assert(! r.next())
  }

  test("Read double") {
    val double = DoubleWrapper(123.123d)
    val r = reader(double)
    assert(r.tokenType == TokenType.Double)
    assert(r.double == 123.123d)
    assert(! r.next())
  }

  test("Read null") {
    val r = reader(NullWrapper)
    assert(r.tokenType == TokenType.Null)
    assert(! r.next())
  }

  test("Read array") {
    val array = ArrayWrapper(Seq(
      StringWrapper("foo"),
      IntWrapper(123)
    ))
    val r = reader(array)
    
    assert(r.tokenType == TokenType.ArrayStart)
    assert(r.next())
    assert(r.tokenType == TokenType.String)
    assert(r.string == "foo")
    assert(r.next())
    assert(r.tokenType == TokenType.Int)
    assert(r.int == 123)
    assert(r.next())
    assert(r.tokenType == TokenType.ArrayEnd)
    assert(! r.next())
  }
  
  test("Read object") {
    val obj = ObjectWrapper(Map(
      "a" -> StringWrapper("foo"),
      "b" -> IntWrapper(123)
    ))
    val r = reader(obj)
    
    assert(r.tokenType == TokenType.ObjectStart)
    assert(r.next())
    assert(r.tokenType == TokenType.AttributeName)
    assert(r.attributeName == "a")
    assert(r.next())
    assert(r.tokenType == TokenType.String)
    assert(r.string == "foo")
    assert(r.next())
    assert(r.tokenType == TokenType.AttributeName)
    assert(r.attributeName == "b")
    assert(r.next())
    assert(r.tokenType == TokenType.Int)
    assert(r.int == 123)
    assert(r.next())
    assert(r.tokenType == TokenType.ObjectEnd)
    assert(! r.next())
  }
}
