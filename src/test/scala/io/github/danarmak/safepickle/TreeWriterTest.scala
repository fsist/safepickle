package io.github.danarmak.safepickle

import org.scalatest.FunSuite

class TreeWriterTest extends FunSuite {
  def writer = WrapperBackend.writer()

  def testWriting(value: Wrapper): Unit = {
    val written = writer.write(value).result()
    assert(value == written, "Survived roundtrip")
  }

  test("Write primitive values") {
    testWriting(StringWrapper("foobar"))
    testWriting(IntWrapper(123))
    testWriting(FloatWrapper(1.2f))
    testWriting(DoubleWrapper(1.2d))
    testWriting(LongWrapper(-123L))
    testWriting(NullWrapper)
  }

  test("Write compound values") {
    testWriting(
      ArrayWrapper(Seq(
        StringWrapper("foo"),
        ArrayWrapper(Seq(
          IntWrapper(1), IntWrapper(2), StringWrapper("bar")
        )),
        ObjectWrapper(Map(
          "a" -> StringWrapper("a"),
          "b" -> ObjectWrapper(Map(
            "c" -> ArrayWrapper(Seq(
              FloatWrapper(1.23f)
            ))
          ))
        ))
      ))
    )

    testWriting(
      ObjectWrapper(Map(
        "arr" -> ArrayWrapper(Seq(
          StringWrapper("foo"),
          ArrayWrapper(Seq(
            IntWrapper(1), IntWrapper(2), StringWrapper("bar")
          )),
          ObjectWrapper(Map(
            "a" -> StringWrapper("a"),
            "b" -> ObjectWrapper(Map(
              "c" -> ArrayWrapper(Seq(
                FloatWrapper(1.23f)
              ))
            ))
          ))
        ))
      ))
    )
  }
}
