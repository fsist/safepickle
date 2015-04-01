package com.fsist.safepickle

import com.fsist.safepickle.Autogen.|
import com.fsist.safepickle.JsonSchema.JSEnum
import com.fsist.safepickle.Schema.SOneOf.SchemaOption
import com.fsist.safepickle.Schema._
import com.typesafe.scalalogging.LazyLogging

import scala.reflect.runtime.universe._

sealed trait JsonSchema {
  def title: String
  def description: String
  def definitions: Map[String, JsonSchema]

  def withDefinitions(definitions: Map[String, JsonSchema]): JsonSchema
}

object JsonSchema {
  type PropertyName = String

  private implicit def additionalPropertiesPickler = AdditionalProperties.pickler
  private implicit def dependencyPickler = Dependency.pickler
  private implicit def itemsPicklers = Items.pickler
  private implicit def enumPickler = JSEnum.pickler

  implicit def pickler: Pickler[JsonSchema] = thePickler
  private val thePickler: Pickler[JsonSchema] = Autogen.children[JsonSchema,
    JSString | JSInteger | JSNumber | JSBoolean | JSNull | JSRef | JSObject | JSArray | JSAllOf | JSAnyOf | JSOneOf | JSNot]

  case class JSString(title: String = "", description: String = "",
                      minLength: Option[Int] = None, maxLength: Option[Int] = None,
                      pattern: Option[String] = None, format: Option[String] = None,
                      definitions: Map[String, JsonSchema] = Map.empty,
                      enum: JSEnum = JSEnum.nil,
                      readOnly: Boolean = false, default: Option[String] = None, hidden: Boolean = false,
                      @WriteDefault @Name("type") schemaType: String = "string") extends JsonSchema {
    require(schemaType == "string", "Do not change the schemaType")
    override def withDefinitions(definitions: Map[String, JsonSchema]): JsonSchema = copy(definitions = definitions)
  }

  sealed trait JSNumeric[T] extends JsonSchema {
    def multipleOf: Option[T]
    def minimum: Option[T]
    def exclusiveMinimum: Boolean
    def maximum: Option[T]
    def exclusiveMaximum: Boolean
    def readOnly: Boolean
    def default: Option[T]
    def hidden: Boolean
  }

  case class JSInteger(title: String = "", description: String = "",
                       multipleOf: Option[Long] = None,
                       minimum: Option[Long] = None, exclusiveMinimum: Boolean = false,
                       maximum: Option[Long] = None, exclusiveMaximum: Boolean = false,
                       definitions: Map[String, JsonSchema] = Map.empty,
                       enum: JSEnum = JSEnum.nil,
                       readOnly: Boolean = false, default: Option[Long] = None, hidden: Boolean = false,
                       @WriteDefault @Name("type") schemaType: String = "integer") extends JSNumeric[Long] {
    require(schemaType == "integer", "Do not change the schemaType")
    override def withDefinitions(definitions: Map[String, JsonSchema]): JsonSchema = copy(definitions = definitions)
  }

  case class JSNumber(title: String = "", description: String = "",
                      multipleOf: Option[Double] = None,
                      minimum: Option[Double] = None, exclusiveMinimum: Boolean = false,
                      maximum: Option[Double] = None, exclusiveMaximum: Boolean = false,
                      definitions: Map[String, JsonSchema] = Map.empty,
                      enum: JSEnum = JSEnum.nil,
                      readOnly: Boolean = false, default: Option[Double] = None, hidden: Boolean = false,
                      @WriteDefault @Name("type") schemaType: String = "number") extends JSNumeric[Double] {
    require(schemaType == "number", "Do not change the schemaType")
    override def withDefinitions(definitions: Map[String, JsonSchema]): JsonSchema = copy(definitions = definitions)
  }


  case class JSBoolean(title: String = "", description: String = "",
                       definitions: Map[String, JsonSchema] = Map.empty,
                       readOnly: Boolean = false, default: Option[Boolean] = None, hidden: Boolean = false,
                       @WriteDefault @Name("type") schemaType: String = "boolean") extends JsonSchema {
    require(schemaType == "boolean", "Do not change the schemaType")
    override def withDefinitions(definitions: Map[String, JsonSchema]): JsonSchema = copy(definitions = definitions)
  }

  case class JSNull(title: String = "", description: String = "",
                    definitions: Map[String, JsonSchema] = Map.empty,
                    @WriteDefault @Name("type") schemaType: String = "null") extends JsonSchema {
    require(schemaType == "null", "Do not change the schemaType")
    override def withDefinitions(definitions: Map[String, JsonSchema]): JsonSchema = copy(definitions = definitions)
  }

  case class JSRef(@Name("$ref") ref: String,
                   title: String = "", description: String = "",
                   definitions: Map[String, JsonSchema] = Map.empty) extends JsonSchema {
    override def withDefinitions(definitions: Map[String, JsonSchema]): JsonSchema = copy(definitions = definitions)
  }

  case class JSObject(title: String = "", description: String = "",
                      properties: Map[PropertyName, JsonSchema] = Map.empty,
                      additionalProperties: AdditionalProperties = AdditionalProperties.disallowed,
                      required: Set[PropertyName] = Set.empty,
                      dependencies: Map[PropertyName, Dependency] = Map.empty,
                      minProperties: Option[Int] = None, maxProperties: Option[Int] = None,
                      patternProperties: Map[String, JsonSchema] = Map.empty,
                      definitions: Map[String, JsonSchema] = Map.empty,
                      enum: JSEnum = JSEnum.nil,
                      @WriteDefault @Name("type") schemaType: String = "object") extends JsonSchema {
    require(schemaType == "object", "Do not change the schemaType")
    override def withDefinitions(definitions: Map[String, JsonSchema]): JsonSchema = copy(definitions = definitions)
  }

  sealed trait AdditionalProperties

  object AdditionalProperties {

    case class Any(allowed: Boolean) extends AdditionalProperties

    case class WithSchema(schema: JsonSchema) extends AdditionalProperties

    val allowed = Any(true)
    val disallowed = Any(false)

    // Custom pickling: must write 'true', 'false', or the schema
    implicit val pickler = new Pickler[AdditionalProperties] {
      override val ttag: TypeTag[AdditionalProperties] = typeTag[AdditionalProperties]

      override def pickle(ap: AdditionalProperties, writer: PickleWriter[_], emitObjectStart: Boolean): Unit = {
        ap match {
          case Any(_) if !emitObjectStart => throw new IllegalArgumentException("Cannot pickle this as an object")
          case Any(allowed) => writer.writeBoolean(allowed)
          case WithSchema(schema) => writer.write(schema, emitObjectStart)
        }
      }

      override def unpickle(reader: PickleReader, expectObjectStart: Boolean): AdditionalProperties = {
        reader.tokenType match {
          // If it's an object with expectObjectStart = false, the first attribute will be the JsonSchema's $type tag, not a Boolean
          case TokenType.Boolean => Any(reader.boolean)
          case _ =>
            val schema = reader.read[JsonSchema](expectObjectStart)
            WithSchema(schema)
        }
      }

      override def schema: Schema = ???
    }
  }

  sealed trait Dependency

  object Dependency {

    case class Properties(properties: Set[PropertyName]) extends Dependency

    case class WithSchema(schema: JsonSchema) extends Dependency

    // Custom pickling: must write the `properties` or `schema` directly
    implicit val pickler = new Pickler[Dependency] {
      override val ttag: TypeTag[Dependency] = typeTag[Dependency]

      override def pickle(dep: Dependency, writer: PickleWriter[_], emitObjectStart: Boolean): Unit = {
        dep match {
          case Properties(properties) => writer.write(properties)
          case WithSchema(schema) => writer.write(schema)
        }
      }

      override def unpickle(reader: PickleReader, expectObjectStart: Boolean): Dependency = {
        reader.tokenType match {
          case TokenType.ArrayStart =>
            val properties = reader.read[Set[PropertyName]](expectObjectStart)
            Properties(properties)
          case _ =>
            val schema = reader.read[JsonSchema](expectObjectStart)
            WithSchema(schema)
        }
      }

      override def schema: Schema = ???
    }
  }

  case class JSArray(title: String = "", description: String = "",
                     items: Items,
                     minItems: Option[Int] = None, maxItems: Option[Int] = None,
                     uniqueItems: Boolean = false,
                     definitions: Map[String, JsonSchema] = Map.empty,
                     enum: JSEnum = JSEnum.nil,
                     @WriteDefault @Name("type") schemaType: String = "array") extends JsonSchema {
    require(schemaType == "array", "Do not change the schemaType")
    override def withDefinitions(definitions: Map[String, JsonSchema]): JsonSchema = copy(definitions = definitions)
  }

  case class Pickleable[T](t: T)(implicit val pickler: Pickler[T]) {
    def write(writer: PickleWriter[_]): Unit = writer.write(t)(pickler)
  }

  case class JSEnum(values: List[Pickleable[_]])
  object JSEnum {
    val nil = JSEnum(Nil)

    implicit val pickler = new Pickler[JSEnum] {
      override val ttag: TypeTag[JSEnum] = typeTag[JSEnum]

      override def pickle(enum: JSEnum, writer: PickleWriter[_], emitObjectStart: Boolean): Unit = {
        writer.writeArrayStart()
        for (value <- enum.values) {
          value.write(writer)
        }
        writer.writeArrayEnd()
      }

      override def unpickle(reader: PickleReader, expectObjectStart: Boolean): JSEnum = ???

      override def schema: Schema = ???
    }
  }

  sealed trait Items

  object Items {
    case class WithSchema(schema: JsonSchema) extends Items
    case class WithSchemas(schemas: List[JsonSchema]) extends Items

    // Custom pickling: must write the `schema` or `schemas` directly
    implicit val pickler = new Pickler[Items] {
      override val ttag: TypeTag[Items] = typeTag[Items]

      override def pickle(it: Items, writer: PickleWriter[_], emitObjectStart: Boolean): Unit = {
        it match {
          case WithSchema(schema) => writer.write(schema)
          case WithSchemas(schemas) => writer.write(schemas)
        }
      }

      override def unpickle(reader: PickleReader, expectObjectStart: Boolean): Items = {
        reader.tokenType match {
          case TokenType.ArrayStart =>
            val schemas = reader.read[List[JsonSchema]](expectObjectStart)
            WithSchemas(schemas)
          case _ =>
            val schema = reader.read[JsonSchema](expectObjectStart)
            WithSchema(schema)
        }
      }

      override def schema: Schema = ???
    }
  }

  sealed trait Combinator extends JsonSchema {
    def title: String
    def description: String
    def common: Option[JsonSchema]
    def options: Set[JsonSchema]
    def keyword: String
  }

  object Combinator {
    /** A pickler that writes the common part inline, then writes the `keyword` mapped to an array of `options` */
    sealed trait CombinatorPickler[T <: Combinator] extends Pickler[T] {
      override def pickle(t: T, writer: PickleWriter[_], emitObjectStart: Boolean): Unit = {
        if (emitObjectStart) writer.writeObjectStart()

        writer.writeAttributeName(t.keyword)
        writer.writeArrayStart()
        for (schema <- t.options) writer.write(schema)
        writer.writeArrayEnd()

        if (t.common.isDefined) {
          writer.write(t.common.get, false)
        }
        else {
          writer.writeObjectEnd()
        }
      }

      override def unpickle(reader: PickleReader, expectObjectStart: Boolean): T = ???
      override def schema: Schema = ???
    }
  }

  case class JSAllOf(title: String = "", description: String = "",
                     common: Option[JsonSchema] = None, options: Set[JsonSchema],
                     definitions: Map[String, JsonSchema] = Map.empty) extends Combinator {
    override def withDefinitions(definitions: Map[String, JsonSchema]): JsonSchema = copy(definitions = definitions)
    override def keyword: String = "allOf"
  }
  object JSAllOf {
    implicit val pickler: Pickler[JSAllOf] = new Combinator.CombinatorPickler[JSAllOf] {
      override def ttag: TypeTag[JSAllOf] = typeTag[JSAllOf]
    }
  }

  case class JSAnyOf(title: String = "", description: String = "",
                     common: Option[JsonSchema] = None, options: Set[JsonSchema],
                     definitions: Map[String, JsonSchema] = Map.empty) extends Combinator {
    override def withDefinitions(definitions: Map[String, JsonSchema]): JsonSchema = copy(definitions = definitions)
    override def keyword: String = "anyOf"
  }
  object JSAnyOf {
    implicit val pickler: Pickler[JSAnyOf] = new Combinator.CombinatorPickler[JSAnyOf] {
      override def ttag: TypeTag[JSAnyOf] = typeTag[JSAnyOf]
    }
  }

  case class JSOneOf(title: String = "", description: String = "",
                     common: Option[JsonSchema] = None, options: Set[JsonSchema],
                     definitions: Map[String, JsonSchema] = Map.empty) extends Combinator {
    override def withDefinitions(definitions: Map[String, JsonSchema]): JsonSchema = copy(definitions = definitions)
    override def keyword: String = "oneOf"
  }
  object JSOneOf {
    implicit val pickler: Pickler[JSOneOf] = new Combinator.CombinatorPickler[JSOneOf] {
      override def ttag: TypeTag[JSOneOf] = typeTag[JSOneOf]
    }
  }

  case class JSNot(title: String = "", description: String = "", not: JsonSchema,
                   definitions: Map[String, JsonSchema] = Map.empty) extends JsonSchema {
    override def withDefinitions(definitions: Map[String, JsonSchema]): JsonSchema = copy(definitions = definitions)
  }

  // =============================================

  def apply(root: Schema): JsonSchema = {
    val schemas = collectSchemas(root)
    val jsonSchemas = (for (schema <- schemas) yield schema -> convert(schema)).toMap
    val topLevel = jsonSchemas(root)

    var definitions = Map[String, JsonSchema]()
    jsonSchemas.map {
      case (schema, jsonSchema) if schema.desc.typeHint.isDefined =>
        val key = schema.desc.typeHint.get
        if (definitions.contains(key)) {
          if (definitions(key) == jsonSchema) {
            // Let's hope this is OK....
          }
          else {
            throw new IllegalArgumentException(s"Two different types have the typeHint $key: $jsonSchema and ${definitions(key)}")
          }
        }
        else {
          definitions = definitions.updated(schema.desc.typeHint.get, jsonSchema)
        }
      case _ =>
    }

    referenceKey(root) match {
      case Some(key) => reference(root).withDefinitions(definitions)
      case None => topLevel.withDefinitions(definitions)
    }
  }

  private def referenceKey(schema: Schema): Option[String] = schema.desc.typeHint

  /** Collect all Schema instances referenced from this root  */
  private def collectSchemas(schema: Schema, seen: Set[Schema] = Set.empty): Set[Schema] = {
    if (seen.contains(schema)) seen
    else {
      val set = seen + schema
      schema match {
        case array: SArray => collectSchemas(array.member, set)
        case tuple: STuple => tuple.members.foldLeft(set) {
          case (set, schema) => collectSchemas(schema, set)
        }
        case obj: SObject => obj.members.foldLeft(set) {
          case (set, member) => collectSchemas(member.schema, set)
        }
        case dict: SDict => collectSchemas(dict.members, set)
        case oneOf: SOneOf => oneOf.options.foldLeft(set) {
          case (set, option) => collectSchemas(option.schema, set)
        }
        case ref: Reference => collectSchemas(ref.target(), set)
        case _ => set
      }
    }
  }

  private def reference(schema: Schema): JsonSchema = {
    schema match {
      case Reference(target, _) => reference(target())
      case _ =>
        referenceKey(schema) match {
          case Some(hint) => JSRef(s"#/definitions/$hint", title = schema.desc.name, description = schema.desc.description)
          case None => convert(schema)
        }
    }
  }

  private def convert(schema: Schema): JsonSchema = {
    schema match {
      case SShort(desc, min, max, readOnly, default, hidden) =>
        JSInteger(
          desc.name, desc.description, minimum = min.map(_.toLong), maximum = max.map(_.toLong),
          readOnly = readOnly, default = default.map(_.toLong), hidden = hidden)
      case SInt(desc, min, max, readOnly, default, hidden) =>
        JSInteger(
          desc.name, desc.description, minimum = min.map(_.toLong), maximum = max.map(_.toLong),
          readOnly = readOnly, default = default.map(_.toLong), hidden = hidden)
      case SLong(desc, min, max, readOnly, default, hidden) =>
        JSInteger(
          desc.name, desc.description, minimum = min, maximum = max,
          readOnly = readOnly, default = default, hidden = hidden)
      case SFloat(desc, min, max, readOnly, default, hidden) =>
        JSNumber(desc.name, desc.description, minimum = min.map(_.toDouble), maximum = max.map(_.toDouble),
          readOnly = readOnly, default = default.map(_.toFloat), hidden = hidden)
      case SDouble(desc, min, max, readOnly, default, hidden) =>
        JSNumber(desc.name, desc.description, minimum = min, maximum = max,
          readOnly = readOnly, default = default, hidden = hidden)
      case SBoolean(desc, readOnly, default, hidden) =>
        JSBoolean(
          desc.name, desc.description,
          readOnly = readOnly, default = default, hidden = hidden)
      case SNull(desc) =>
        JSNull(desc.name, desc.description)

      case SString(desc, minLength, maxLength, pattern, readOnly, default, hidden) =>
        JSString(desc.name, desc.description, minLength, maxLength, pattern,
          readOnly = readOnly, default = default, hidden = hidden)

      case SArray(member, desc, minLength, maxLength) =>
        JSArray(desc.name, desc.description, Items.WithSchema(reference(member)), minLength, maxLength)

      case STuple(members, desc) =>
        JSArray(desc.name, desc.description, Items.WithSchemas(members map reference))

      case SObject(members, desc) =>
        JSObject(
          desc.name, desc.description,
          (members map (member => member.name -> reference(member.schema))).toMap,
          AdditionalProperties.disallowed,
          (members.filter(_.required).map(_.name)).toSet
        )

      case SDict(members, desc) =>
        JSObject(
          desc.name, desc.description,
          additionalProperties = AdditionalProperties.WithSchema(reference(members))
        )

      case SOneOf(options, desc) =>
        val jsSchemas = options.map { _ match {
          case SchemaOption(schema, Some(typeHint)) =>
            JSAllOf(
              common = Some(JSObject(
                properties = Map(
                  "$type" -> JSString(readOnly = true, default = Some(typeHint), hidden = true)
                ),
                required = Set("$type")
              )),
              options = Set(convert(schema))
            )

          case SchemaOption(schema, None) => convert(schema)
        }}

        JSOneOf(desc.name, desc.description, options = jsSchemas)

      case Reference(target, name) =>
        val resolved = target()
        resolved.desc.typeHint match {
          case Some(hint) => JSRef(s"#/definitions/$hint")
          case None => convert(resolved)
        }

    }
  }
}
