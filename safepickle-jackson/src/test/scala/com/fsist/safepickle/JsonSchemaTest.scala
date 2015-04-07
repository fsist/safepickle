package com.fsist.safepickle

import com.fsist.safepickle.Autogen.|
import com.fsist.safepickle.JsonSchema._
import com.fsist.safepickle._
import com.fsist.safepickle.jackson.JacksonPicklerBackend
import org.scalatest.FunSuite

object JsonSchemaTest {
  case class C1(i: Int, s: String, b: Boolean = false)
  object C1 {
    implicit val pickler = Autogen[C1]
  }

  case class C2(c1: C1)
  object C2 {
    implicit val pickler = Autogen[C2]
  }

  case class C3(opt: Option[C3])
  object C3 {
    implicit def pickler : Pickler[C3] = thePickler
    private lazy val thePickler : Pickler[C3] = Autogen[C3]
  }

  sealed trait T
  object T {
    implicit def pickler: Pickler[T] = thePickler
    implicit val listPickler: Pickler[List[T]] = Pickler.iterablePickler[T, List]
    private lazy val thePickler = Autogen.children[T, O.type | C1 | C2]

    case object O extends T
    case class C1(i: String, o: Option[O.type] = None) extends T

    case class C2(c1s: List[T]) extends T
    object C2 {
      implicit val pickler = Autogen[C2]
    }
  }

  sealed trait T2
  object T2 {
    implicit val pickler = Autogen.children[T2, A.type | B.type]

    case object A extends T2
    case object B extends T2
  }

  case class C4(s: String)
  object C4 {
    implicit val pickler = Autogen.versioned[C4, C4Old]
  }
  case class C4Old(s: String) extends OldVersion[C4] {
    override def toNewVersion: C4 = C4(s)
  }
}

class JsonSchemaTest extends FunSuite {

  import JsonSchemaTest._

  def jss[T](implicit pickler: Pickler[T]) = JsonSchema(pickler.schema)

  test("Primitive types") {
    assert(jss[Short] == JSInteger.short())
    assert(jss[Int] == JSInteger.int())
    assert(jss[Long] == JSInteger.long())
    assert(jss[Float] == JSNumber.float())
    assert(jss[Double] == JSNumber.double())
    assert(jss[String] == JSString())
    assert(jss[Boolean] == JSBoolean(format = Some("checkbox")))
    assert(jss[Null] == JSNull())
  }

  test("Sequence") {
    assert(jss[List[Int]] == JSArray(items = Some(jss[Int])))
    assert(jss[Array[String]] == JSArray(items = Some(jss[String])))
  }

  test("Tuple") {
    assert(jss[(Int, String)] == JSTuple(items = List(jss[Int], jss[String])))
  }

  test("Map") {
    assert(jss[Map[String, Int]] == JSObject(additionalProperties = AdditionalProperties.WithSchema(jss[Int])))
  }

  test("Class") {
    assert(jss[C1] ==
      JSObject(
        "C1",
        properties = Map(
          "i" -> JSInteger.int("i").withPropertyOrder(Some(0)),
          "s" -> JSString("s", propertyOrder = Some(1)),
          "b" -> JSBoolean("b", format = Some("checkbox"), propertyOrder = Some(2))
        ),
        required = List("i", "s"),
        defaultProperties = List("i", "s")
      )
    )
  }

  test("Class with reference") {
    assert(jss[C2] ==
      JSObject(
        "C2",
        properties = Map(
          "c1" -> JSObject(
            "c1",
            properties = Map(
              "i" -> JSInteger.int("i").withPropertyOrder(Some(0)),
              "s" -> JSString("s", propertyOrder = Some(1)),
              "b" -> JSBoolean("b", format = Some("checkbox"), propertyOrder = Some(2))
            ),
            required = List("i", "s"),
            defaultProperties = List("i", "s"),
            propertyOrder = Some(0)
          )
        ),
        required = List("c1"),
        defaultProperties = List("c1")
      )
    )
  }

  test("Class with circular reference") {
    assert(jss[C3] ==
      JSRef(
        title = "C3",
        ref = "#/definitions/com.fsist.safepickle.JsonSchemaTest.C3",
        definitions = Map(
          "com.fsist.safepickle.JsonSchemaTest.C3" -> JSObject(
            "C3",
            properties = Map(
              "opt" -> JSRef(
                "opt",
                ref = "#/definitions/com.fsist.safepickle.JsonSchemaTest.C3",
                propertyOrder = Some(0)
              )
            ),
            required = List("opt"),
            defaultProperties = List("opt")
          )
        )
      )
    )
  }

  test("Trait with descendants and a circular reference") {
    assert(jss[T] ==
      JSRef(
        title = "T",
        ref = "#/definitions/com.fsist.safepickle.JsonSchemaTest.T",
        definitions = Map(
          "com.fsist.safepickle.JsonSchemaTest.T.C1" -> JSObject(
            "C1",
            properties = Map(
              "$type" -> JSString("$type", default = Some("C1"), readOnly = true, enum = List("C1"), options = JSEditorOptions(hidden = true), propertyOrder = Some(0)),
              "i" -> JSString("i", propertyOrder = Some(1)),
              "o" -> JSString(
                title = "o",
                default = Some("O"),
                enum = List("O"),
                readOnly = true,
                propertyOrder = Some(2)
              )
            ),
            required = List("$type", "i"),
            defaultProperties = List("$type", "i")
          ),
          "com.fsist.safepickle.JsonSchemaTest.T.C2" -> JSObject(
            title = "C2",
            properties = Map(
              "$type" -> JSString("$type", default = Some("C2"), readOnly = true, enum = List("C2"), options = JSEditorOptions(hidden = true), propertyOrder = Some(0)),
              "c1s" -> JSArray(
                "c1s",
                items = Some(
                  JSRef("T", "#/definitions/com.fsist.safepickle.JsonSchemaTest.T")
                ),
                propertyOrder = Some(1)
              )
            ),
            required = List("$type", "c1s"),
            defaultProperties = List("$type", "c1s")
          ),
          "com.fsist.safepickle.JsonSchemaTest.T" -> JSObject(
            "T",
            oneOf = List(
              JSString(
                title = "O",
                default = Some("O"),
                enum = List("O"),
                readOnly = true
              ),
              JSRef("C1", "#/definitions/com.fsist.safepickle.JsonSchemaTest.T.C1"),
              JSRef("C2", "#/definitions/com.fsist.safepickle.JsonSchemaTest.T.C2")
            )
          )
        )
      )
    )
  }

  test("Trait with singleton descendants only") {
    assert(jss[T2] ==
      JSString(
        title = "T2",
        enum = List("A", "B"),
        options = JSEditorOptions(
          enumTitles = List("A", "B")
        )
      )
    )
  }

  test("Versioned") {
    assert(jss[C4] ==
      JSObject(
        title = "C4",
        properties = Map(
          "$version" -> JSInteger("$version", readOnly = true, options = JSEditorOptions(hidden = true), default = Some(2), enum = List(2), propertyOrder = Some(0)),
          "s" -> JSString("s", propertyOrder = Some(1))
        ),
        required = List("$version", "s"),
        defaultProperties = List("$version", "s")
      )
    )
  }
}
