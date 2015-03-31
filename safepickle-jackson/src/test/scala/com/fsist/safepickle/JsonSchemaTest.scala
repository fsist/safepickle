package com.fsist.safepickle

import com.fsist.safepickle.Autogen.|
import com.fsist.safepickle.JsonSchema._
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

  sealed trait T
  object T {
    implicit def pickler: Pickler[T] = thePickler
    implicit val listPickler: Pickler[List[T]] = Pickler.iterablePickler[T, List]
    private lazy val thePickler = Autogen.children[T, O.type | C1 | C2]

    case object O extends T
    case class C1(i: String, o: Option[O.type] = None) extends T

    case class C2(c1s: List[T]) extends T
    object C2 {
      implicit val pickler = Autogen.debug[C2]
    }
  }
}

class JsonSchemaTest extends FunSuite {

  import JsonSchemaTest._

  def jss[T](implicit pickler: Pickler[T]) = JsonSchema(pickler.schema)

  test("Primitive types") {
    assert(jss[Int] == JsonSchema.JSInteger())
    assert(jss[Long] == JsonSchema.JSInteger())
    assert(jss[Float] == JsonSchema.JSNumber())
    assert(jss[Double] == JsonSchema.JSNumber())
    assert(jss[String] == JsonSchema.JSString())
    assert(jss[Boolean] == JsonSchema.JSBoolean())
    assert(jss[Null] == JsonSchema.JSNull())
  }

  test("Sequence") {
    assert(jss[List[Int]] == JsonSchema.JSArray(items = Items.WithSchema(jss[Int])))
    assert(jss[Array[String]] == JsonSchema.JSArray(items = Items.WithSchema(jss[String])))
  }

  test("Tuple") {
    assert(jss[(Int, String)] == JsonSchema.JSArray(items = Items.WithSchemas(List(jss[Int], jss[String]))))
  }

  test("Map") {
    assert(jss[Map[String, Int]] == JsonSchema.JSObject(additionalProperties = AdditionalProperties.WithSchema(jss[Int])))
  }

  test("Class") {
    assert(jss[C1] ==
      JsonSchema.JSRef(
        title = "C1",
        ref = "#/definitions/C1",
        definitions = Map(
          "C1" -> JsonSchema.JSObject(
            title = "C1",
            properties = Map(
              "i" -> jss[Int],
              "s" -> jss[String],
              "b" -> jss[Boolean]
            ),
            required = Set("i", "s")
          )
        )
      )
    )
  }

  test("Class with reference") {
    assert(jss[C2] == JSRef(
      title = "C2",
      ref = "#/definitions/C2",
      definitions = Map(
        "C2" -> JsonSchema.JSObject(
          title = "C2",
          properties = Map(
            "c1" -> JsonSchema.JSRef("#/definitions/C1", "C1")
          ),
          required = Set("c1")),
        "C1" -> JsonSchema.JSObject(
          title = "C1",
          properties = Map(
            "i" -> jss[Int],
            "s" -> jss[String],
            "b" -> jss[Boolean]
          ),
          required = Set("i", "s")
        )
      )
    ))
  }

  test("Trait with descendants") {
    assert(jss[T] == JsonSchema.JSRef(
      title = "T",
      ref = "#/definitions/T",
      definitions = Map(
        "O" -> JsonSchema.JSString(
          title = "O",
          enum = JSEnum(List(Pickleable("O"))),
          readOnly = true
        ),
        "C1" -> JsonSchema.JSObject(
          title = "C1",
          properties = Map(
            "i" -> jss[String],
            "o" -> JSRef("#/definitions/O", "O")
          ),
          required = Set("i")
        ),
        "C2" -> JsonSchema.JSObject(
          title = "C2",
          properties = Map(
            "c1s" -> JsonSchema.JSArray(
              items = JsonSchema.Items.WithSchema(
                JSRef("#/definitions/T", "T")
              )
            )
          ),
          required = Set("c1s")
        ),
        "T" -> JsonSchema.JSOneOf(title = "T", options = Set(
          JSRef("#/definitions/O", "O"),
          JSRef("#/definitions/C1", "C1"),
          JSRef("#/definitions/C2", "C2")
        ))
      )
    ))
  }
}