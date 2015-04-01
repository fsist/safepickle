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
      implicit val pickler = Autogen[C2]
    }
  }

  case class C3(s: String)
  object C3 {
    implicit val pickler = Autogen.versioned[C3, C3Old]
  }
  case class C3Old(s: String) extends OldVersion[C3] {
    override def toNewVersion: C3 = C3(s)
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
        ref = "#/definitions/com.fsist.safepickle.JsonSchemaTest.C1",
        definitions = Map(
          "com.fsist.safepickle.JsonSchemaTest.C1" -> JsonSchema.JSObject(
            title = "C1",
            properties = Map(
              "i" -> jss[Int],
              "s" -> jss[String],
              "b" -> jss[Boolean]
            ),
            required = Set("i", "s"),
            defaultProperties = Set("i", "s")
          )
        )
      )
    )
  }

  test("Class with reference") {
    assert(jss[C2] == JSRef(
      title = "C2",
      ref = "#/definitions/com.fsist.safepickle.JsonSchemaTest.C2",
      definitions = Map(
        "com.fsist.safepickle.JsonSchemaTest.C2" -> JsonSchema.JSObject(
          title = "C2",
          properties = Map(
            "c1" -> JsonSchema.JSRef("#/definitions/com.fsist.safepickle.JsonSchemaTest.C1", "C1")
          ),
          required = Set("c1")),
        "com.fsist.safepickle.JsonSchemaTest.C1" -> JsonSchema.JSObject(
          title = "C1",
          properties = Map(
            "i" -> jss[Int],
            "s" -> jss[String],
            "b" -> jss[Boolean]
          ),
          required = Set("i", "s"),
          defaultProperties = Set("i", "s")
        )
      )
    ))
  }

  test("Trait with descendants") {
    def withTypeName(typeName: String, ref: JSRef): JSAllOf =
      JSAllOf(
        common = Some(JSObject(
          properties = Map(
            "$type" -> JSString(readOnly = true, default = Some(typeName), hidden = true)
          ),
          required = Set("$type")
        )),
        options = Set(ref)
      )

    assert(jss[T] == JsonSchema.JSRef(
      title = "T",
      ref = "#/definitions/com.fsist.safepickle.JsonSchemaTest.T",
      definitions = Map(
        "com.fsist.safepickle.JsonSchemaTest.T.O.type" -> JsonSchema.JSString(
          title = "O",
          default = Some("O"),
          readOnly = true
        ),
        "com.fsist.safepickle.JsonSchemaTest.T.C1" -> JsonSchema.JSObject(
          title = "C1",
          properties = Map(
            "i" -> jss[String],
            "o" -> JSRef("#/definitions/com.fsist.safepickle.JsonSchemaTest.T.O.type", "O")
          ),
          required = Set("i"),
          defaultProperties = Set("i")
        ),
        "com.fsist.safepickle.JsonSchemaTest.T.C2" -> JsonSchema.JSObject(
          title = "C2",
          properties = Map(
            "c1s" -> JsonSchema.JSArray(
              items = JsonSchema.Items.WithSchema(
                JSRef("#/definitions/com.fsist.safepickle.JsonSchemaTest.T", "T")
              )
            )
          ),
          required = Set("c1s")
        ),
        "com.fsist.safepickle.JsonSchemaTest.T" -> JsonSchema.JSOneOf(title = "T", options = Set(
          JSRef("#/definitions/com.fsist.safepickle.JsonSchemaTest.T.O.type"),
          withTypeName("C1", JSRef("#/definitions/com.fsist.safepickle.JsonSchemaTest.T.C1")),
          withTypeName("C2", JSRef("#/definitions/com.fsist.safepickle.JsonSchemaTest.T.C2"))
        ))
      )
    ))
  }

  test("Versioned") {
    assert(jss[C3] == JSRef(
      title = "C3", ref = "#/definitions/com.fsist.safepickle.JsonSchemaTest.C3",
      definitions = Map(
        "com.fsist.safepickle.JsonSchemaTest.C3" -> JSObject(
          "C3",
          properties = Map(
            "$version" -> JSInteger(readOnly = true, hidden = true, default = Some(2)),
            "s" -> JSString()
          ),
          required = Set("$version", "s")
        )
      )
    ))
  }
}
