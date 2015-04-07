package com.fsist.safepickle

import com.fsist.safepickle.Autogen.|
import com.fsist.safepickle.JsonSchema.JSEditorOptions
import com.fsist.safepickle.Schema._
import com.typesafe.scalalogging.LazyLogging

import scala.reflect.runtime.universe._

case class Pickleable[T](t: T)(implicit val pickler: Pickler[T]) {
  def write(writer: PickleWriter[_]): Unit = writer.write(t)(pickler)
}
object Pickleable {
  implicit def pickler[T](implicit tag: TypeTag[Pickleable[T]]): Pickler[Pickleable[T]] = new Pickler[Pickleable[T]] {
    override val ttag: TypeTag[Pickleable[T]] = tag
    override def pickle(t: Pickleable[T], writer: PickleWriter[_], emitObjectStart: Boolean): Unit =
      writer.write(t.t, emitObjectStart)(t.pickler)
    override def unpickle(reader: PickleReader, expectObjectStart: Boolean): Pickleable[T] =
      throw new NotImplementedError(s"This type cannot support unpickling")
    override val schema: Schema = throw new NotImplementedError(s"This type cannot support unpickling")
  }
}

sealed trait JsonSchema {
  def title: String
  def withTitle(title: String): JsonSchema

  def schemaType: String

  def description: String
  def withDescription(description: String): JsonSchema

  def propertyOrder: Option[Int]
  def withPropertyOrder(order: Option[Int]): JsonSchema

  def options: JSEditorOptions
  def mapOptions(func: JSEditorOptions => JSEditorOptions): JsonSchema
}

object JsonSchema {
  type PropertyName = String

  private implicit def additionalPropertiesPickler = AdditionalProperties.pickler
  private implicit def somePickleablePickler: Pickler[Pickleable[_]] = Pickleable.pickler[Any].asInstanceOf[Pickler[Pickleable[_]]]

  implicit def pickler: Pickler[JsonSchema] = thePickler
  private lazy val thePickler: Pickler[JsonSchema] = Autogen.children[JsonSchema,
    JSString | JSInteger | JSNumber | JSBoolean | JSNull | JSRef | JSObject | JSArray | JSTuple]

  /** Non-standard options implemented by the jdorn/json-editor project.
    * See: https://github.com/jdorn/json-editor
    */
  case class JSEditorOptions(@Name("disable_collapse") disableCollapse: Boolean = false,
                             collapsed: Boolean = false,
                             @Name("disable_array_add") disableArrayAdd: Boolean = false,
                             @Name("disable_array_delete") disableArrayDelete: Boolean = false,
                             @Name("disable_array_reorder") disableArrayReorder: Boolean = false,
                             @Name("enum_titles") enumTitles: List[String] = Nil,
                             @Name("expand_height") expandHeight: Boolean = false,
                             @Name("grid_columns") gridColumns: Option[Int] = None,
                             hidden: Boolean = false,
                             @Name("input_height") inputHeight: Option[String] = None,
                             @Name("input_width") inputWidth: Option[String] = None,
                             @Name("remove_empty_properties") removeEmptyProperties: Boolean = false)

  case class JSString(title: String = "", description: String = "",
                      minLength: Option[Int] = None, maxLength: Option[Int] = None,
                      pattern: Option[String] = None, format: Option[String] = None,
                      enum: List[String] = Nil,
                      readOnly: Boolean = false, default: Option[String] = None,
                      options: JSEditorOptions = JSEditorOptions(),
                      propertyOrder: Option[Int] = None,
                      @WriteDefault @Name("type") schemaType: String = "string") extends JsonSchema {
    override def withPropertyOrder(order: Option[Int]): JsonSchema = copy(propertyOrder = order)
    override def withTitle(title: String): JsonSchema = copy(title = title)
    override def withDescription(description: String): JsonSchema = copy(description = description)
    override def mapOptions(func: (JSEditorOptions) => JSEditorOptions): JsonSchema = copy(options = func(options))
  }

  sealed trait JSNumeric[T] extends JsonSchema {
    def format: Option[String]
    def multipleOf: Option[T]
    def minimum: Option[T]
    def exclusiveMinimum: Boolean
    def maximum: Option[T]
    def exclusiveMaximum: Boolean
    def readOnly: Boolean
    def default: Option[T]
  }

  case class JSInteger(title: String = "", description: String = "",
                       format: Option[String] = None,
                       multipleOf: Option[Long] = None,
                       minimum: Option[Long] = None, exclusiveMinimum: Boolean = false,
                       maximum: Option[Long] = None, exclusiveMaximum: Boolean = false,
                       enum: List[Long] = Nil,
                       readOnly: Boolean = false, default: Option[Long] = None,
                       options: JSEditorOptions = JSEditorOptions(),
                       propertyOrder: Option[Int] = None,
                       @WriteDefault @Name("type") schemaType: String = "integer") extends JSNumeric[Long] {
    require(schemaType == "integer", "Do not change the schemaType")
    override def withPropertyOrder(order: Option[Int]): JsonSchema = copy(propertyOrder = order)
    override def withTitle(title: String): JsonSchema = copy(title = title)
    override def withDescription(description: String): JsonSchema = copy(description = description)
    override def mapOptions(func: (JSEditorOptions) => JSEditorOptions): JsonSchema = copy(options = func(options))
  }

  object JSInteger {
    def short(title: String = "") = JSInteger(title, minimum = Some(Short.MinValue), maximum = Some(Short.MaxValue))
    def int(title: String = "") = JSInteger(title, minimum = Some(Int.MinValue), maximum = Some(Int.MaxValue))
    def long(title: String = "") = JSInteger(title, minimum = Some(Long.MinValue), maximum = Some(Long.MaxValue))
  }

  case class JSNumber(title: String = "", description: String = "",
                      format: Option[String] = None,
                      multipleOf: Option[Double] = None,
                      minimum: Option[Double] = None, exclusiveMinimum: Boolean = false,
                      maximum: Option[Double] = None, exclusiveMaximum: Boolean = false,
                      enum: List[Double] = Nil,
                      readOnly: Boolean = false, default: Option[Double] = None,
                      options: JSEditorOptions = JSEditorOptions(),
                      propertyOrder: Option[Int] = None,
                      @WriteDefault @Name("type") schemaType: String = "number") extends JSNumeric[Double] {
    require(schemaType == "number", "Do not change the schemaType")
    override def withPropertyOrder(order: Option[Int]): JsonSchema = copy(propertyOrder = order)
    override def withTitle(title: String): JsonSchema = copy(title = title)
    override def withDescription(description: String): JsonSchema = copy(description = description)
    override def mapOptions(func: (JSEditorOptions) => JSEditorOptions): JsonSchema = copy(options = func(options))
  }

  object JSNumber {
    def float(title: String = "") = JSNumber(title, minimum = Some(Float.MinValue), maximum = Some(Float.MaxValue))
    def double(title: String = "") = JSNumber(title, minimum = Some(Double.MinValue), maximum = Some(Double.MaxValue))
  }

  case class JSBoolean(title: String = "", description: String = "",
                       format: Option[String] = None,
                       enum: List[Boolean] = Nil, // Used for constants
                       readOnly: Boolean = false, default: Option[Boolean] = None,
                       options: JSEditorOptions = JSEditorOptions(),
                       propertyOrder: Option[Int] = None,
                       @WriteDefault @Name("type") schemaType: String = "boolean") extends JsonSchema {
    require(schemaType == "boolean", "Do not change the schemaType")
    override def withPropertyOrder(order: Option[Int]): JsonSchema = copy(propertyOrder = order)
    override def withTitle(title: String): JsonSchema = copy(title = title)
    override def withDescription(description: String): JsonSchema = copy(description = description)
    override def mapOptions(func: (JSEditorOptions) => JSEditorOptions): JsonSchema = copy(options = func(options))
  }

  case class JSNull(title: String = "", description: String = "",
                    propertyOrder: Option[Int] = None,
                    options: JSEditorOptions = JSEditorOptions(),
                    @WriteDefault @Name("type") schemaType: String = "null") extends JsonSchema {
    require(schemaType == "null", "Do not change the schemaType")
    override def withPropertyOrder(order: Option[Int]): JsonSchema = copy(propertyOrder = order)
    override def withTitle(title: String): JsonSchema = copy(title = title)
    override def withDescription(description: String): JsonSchema = copy(description = description)
    override def mapOptions(func: (JSEditorOptions) => JSEditorOptions): JsonSchema = copy(options = func(options))
  }

  case class JSRef(title: String = "",
                   @Name("$ref") ref: String, description: String = "",
                   definitions: Map[String, JsonSchema] = Map.empty,
                   options: JSEditorOptions = JSEditorOptions(),
                   propertyOrder: Option[Int] = None,
                   @Name("type") schemaType: String = "object") extends JsonSchema {
    override def withPropertyOrder(order: Option[Int]): JsonSchema = copy(propertyOrder = order)
    override def withTitle(title: String): JsonSchema = copy(title = title)
    override def withDescription(description: String): JsonSchema = copy(description = description)
    override def mapOptions(func: (JSEditorOptions) => JSEditorOptions): JsonSchema = copy(options = func(options))
  }

  case class JSObject(title: String = "", description: String = "",
                      definitions: Map[String, JsonSchema] = Map.empty,
                      properties: Map[PropertyName, JsonSchema] = Map.empty,
                      additionalProperties: AdditionalProperties = AdditionalProperties.disallowed,
                      required: List[PropertyName] = Nil,
                      @WriteDefault defaultProperties: List[PropertyName] = Nil,
                      minProperties: Option[Int] = None, maxProperties: Option[Int] = None,
                      patternProperties: Map[String, JsonSchema] = Map.empty,
                      enum: List[Pickleable[_]] = Nil,
                      propertyOrder: Option[Int] = None,
                      allOf: List[JsonSchema] = Nil, anyOf: List[JsonSchema] = Nil, oneOf: List[JsonSchema] = Nil,
                      not: Option[JsonSchema] = None,
                      options: JSEditorOptions = JSEditorOptions(),
                      @WriteDefault @Name("type") schemaType: String = "object") extends JsonSchema {
    require(schemaType == "object", "Do not change the schemaType")

    override def withPropertyOrder(order: Option[Int]): JsonSchema = copy(propertyOrder = order)
    override def withTitle(title: String): JsonSchema = copy(title = title)
    override def withDescription(description: String): JsonSchema = copy(description = description)
    override def mapOptions(func: (JSEditorOptions) => JSEditorOptions): JsonSchema = copy(options = func(options))
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

      override val schema: Schema = SOneOf(ttag.tpe, List(SBoolean(typeOf[Any]), JsonSchema.pickler.schema))
    }
  }

  case class JSArray(title: String = "", description: String = "",
                     items: Option[JsonSchema] = None,
                     minItems: Option[Int] = None, maxItems: Option[Int] = None,
                     uniqueItems: Boolean = false,
                     propertyOrder: Option[Int] = None,
                     options: JSEditorOptions = JSEditorOptions(),
                     @WriteDefault @Name("type") schemaType: String = "array") extends JsonSchema {
    require(schemaType == "array", "Do not change the schemaType")
    override def withPropertyOrder(order: Option[Int]): JsonSchema = copy(propertyOrder = order)
    override def withTitle(title: String): JsonSchema = copy(title = title)
    override def withDescription(description: String): JsonSchema = copy(description = description)
    override def mapOptions(func: (JSEditorOptions) => JSEditorOptions): JsonSchema = copy(options = func(options))
  }

  case class JSTuple(title: String = "", description: String = "",
                     items: List[JsonSchema] = Nil,
                     minItems: Option[Int] = None, maxItems: Option[Int] = None,
                     uniqueItems: Boolean = false,
                     propertyOrder: Option[Int] = None,
                     options: JSEditorOptions = JSEditorOptions(),
                     @WriteDefault @Name("type") schemaType: String = "array") extends JsonSchema {
    require(schemaType == "array", "Do not change the schemaType")
    override def withPropertyOrder(order: Option[Int]): JsonSchema = copy(propertyOrder = order)
    override def withTitle(title: String): JsonSchema = copy(title = title)
    override def withDescription(description: String): JsonSchema = copy(description = description)
    override def mapOptions(func: (JSEditorOptions) => JSEditorOptions): JsonSchema = copy(options = func(options))
  }

  // =============================================

  /** Wraps a Type and overrides equality to be based on =:= */
  private[safepickle] implicit class EqType(val tpe: Type) {
    override def equals(other: Any): Boolean = other.isInstanceOf[EqType] && tpe =:= other.asInstanceOf[EqType].tpe
    override def hashCode: Int = tpe.toString.hashCode
    override def toString: String = tpe.toString
  }

  def apply(root: Schema): JsonSchema = {
    val schemas = collectSchemas(root)
    val canUseDefinitions = root.isInstanceOf[SObject] || root.isInstanceOf[SOneOf]
    val needsDefinitions = hasCycles(root)

    if (!needsDefinitions) {
      convert(root, false, Map.empty)
    }
    else {
      if (!canUseDefinitions) {
        throw new IllegalArgumentException(
          "Schema with circular references must have a top-level SOjbect where we can define the common types. " +
            s"This is an implementation restriction. Instead found: $root")
      }

      var usedKeys = Set.empty[String]
      var keyIndex = 0

      val referenceKeys: Map[Schema, String] =
        (for (schema <- schemas if useReference(schema)) yield {
          val keyBase = schema.tpe.toString
          val key =
            if (!usedKeys.contains(keyBase)) keyBase
            else {
              keyIndex += 1
              keyBase + keyIndex
            }
          usedKeys += key
          schema -> key
        }).toMap

      val referencePaths = referenceKeys.mapValues("#/definitions/" + _)

      val definitions =
        (for (schema <- schemas if useReference(schema))
          yield referenceKeys(schema) -> convert(schema, true, referencePaths, false)).toMap

      convert(root, true, referencePaths) match {
        case obj: JSObject => obj.copy(definitions = definitions)
        case ref: JSRef => ref.copy(definitions = definitions)
        case other => throw new IllegalStateException("Bug")
      }
    }
  }

  /** Whether this schema should be stored in the top-level definitions and referred to using JSRef. */
  private def useReference(schema: Schema): Boolean = schema match {
    case obj: SObject => true
    case oneOf: SOneOf => true
    case _ => false
  }

  /** Whether this schema has circular references that require the use of JSRef and shared definitions. */
  private def hasCycles(schema: Schema, seen: Set[Schema] = Set.empty): Boolean = {
    if (seen.contains(schema)) true
    else schema match {
      case obj: SObject => obj.members.exists(member => hasCycles(member.schema, seen + schema))
      case arr: SArray => hasCycles(arr.member, seen + schema)
      case tup: STuple => tup.members.exists(member => hasCycles(member, seen + schema))
      case ref: SRef => hasCycles(ref.resolve, seen + schema)
      case oneOf: SOneOf => oneOf.options.exists(option => hasCycles(option, seen + schema))
      case _ => false
    }
  }

  /** Collect all Schema instances referenced from this root, dereferencing all references.  */
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
          case (set, option) => collectSchemas(option, set)
        }
        case ref: SRef => collectSchemas(ref.resolve, set)
        case _ => set
      }
    }
  }

  private def title(schema: Schema): String = schema match {
    case _: SObject | _: SOneOf => schema.tpe.typeSymbol.name.decodedName.toString
    case SShortConst(tpe, constant) => constant.toString
    case SIntConst(tpe, constant) => constant.toString
    case SLongConst(tpe, constant) => constant.toString
    case SFloatConst(tpe, constant) => constant.toString
    case SDoubleConst(tpe, constant) => constant.toString
    case SBooleanConst(tpe, constant) => constant.toString
    case SStringConst(tpe, constant) => constant
    case _ => ""
  }

  /** Convert a Schema to a JsonSchema, recursively.
    *
    * @param useReferences If true, JSRefs to #/definition/xxx are generated for all schemas where `useReference` is true.
    *                      If false, all schemas are inlined.
    * @param referencePaths Maps schemas (for which `useReference` is true) to the keys that can be used with JSRef.
    * @param allowTopLevelReference If true, the original `schema` itself can be converted to a JSRef (`useReferences` and
    *                               `useReference` permitting). If false, the top-level schema does not become a reference.
    */
  private def convert(schema: Schema, useReferences: Boolean, referencePaths: Map[Schema, String],
                      allowTopLevelReference: Boolean = true): JsonSchema = {


    def convertOrReference(schema: Schema): JsonSchema =
      if (!useReferences || !useReference(schema)) convert(schema, useReferences, referencePaths)
      else JSRef(title(schema), ref = referencePaths(schema))

    schema match {
      case SShort(tpe) => JSInteger.short(title(schema))
      case SInt(tpe) => JSInteger.int(title(schema))
      case SLong(tpe) => JSInteger.long(title(schema))
      case SFloat(tpe) => JSNumber.float(title(schema))
      case SDouble(tpe) => JSNumber.double(title(schema))

      case SShortConst(tpe, constant) => JSInteger(title(schema), readOnly = true, default = Some(constant), enum = List(constant))
      case SIntConst(tpe, constant) => JSInteger(title(schema), readOnly = true, default = Some(constant), enum = List(constant))
      case SLongConst(tpe, constant) => JSInteger(title(schema), readOnly = true, default = Some(constant), enum = List(constant))
      case SFloatConst(tpe, constant) => JSNumber(title(schema), readOnly = true, default = Some(constant), enum = List(constant))
      case SDoubleConst(tpe, constant) => JSNumber(title(schema), readOnly = true, default = Some(constant), enum = List(constant))

      case SBoolean(tpe) => JSBoolean(title(schema), format = Some("checkbox"))
      case SBooleanConst(tpe, constant) => JSBoolean(title(schema), readOnly = true, default = Some(constant), enum = List(constant))

      case SString(tpe) => JSString(title(schema))
      case SStringConst(tpe, constant) => JSString(title(schema), readOnly = true, default = Some(constant), enum = List(constant))

      case SNull(tpe) => JSNull(title(schema))

      case SArray(tpe, member) => JSArray(title(schema), items = Some(convertOrReference(member)))

      case STuple(tpe, members) => JSTuple(title(schema), items = members.map(convertOrReference(_)))

      case SObject(tpe, members) =>
        if (allowTopLevelReference && useReferences && useReference(schema)) {
          JSRef(title(schema), ref = referencePaths(schema))
        }
        else {
          JSObject(
            title(schema),
            properties = members.zipWithIndex.map { case (SObjectMember(name, schema, required), index) =>
              name -> {
                val ret = convertOrReference(schema).withTitle(name).withPropertyOrder(Some(index))
                val shouldHide = schema.isInstanceOf[AtomicConst[_]] && name.startsWith("$")
                ret.mapOptions(_.copy(hidden = shouldHide))
              }
            }.toMap,
            required = members.filter(_.required).map(_.name),
            defaultProperties = members.filter(_.required).map(_.name)
          )
        }

      case SDict(tpe, members) =>
        JSObject(
          title(schema),
          additionalProperties = AdditionalProperties.WithSchema(convertOrReference(members))
        )

      case SOneOf(tpe, options) =>
        val alts = options.map(convertOrReference)

        if (options.forall(_.isInstanceOf[SStringConst])) {
          JSString(
            title(schema),
            enum = options.map(_.asInstanceOf[SStringConst].constant),
            options = JSEditorOptions(
              enumTitles = alts.map(_.title)
            )
          )
        }
        else if (options.forall(_.isInstanceOf[SShortConst])) {
          JSInteger(
            title(schema),
            enum = options.map(_.asInstanceOf[SShortConst].constant.toLong),
            options = JSEditorOptions(
              enumTitles = alts.map(_.title)
            )
          )
        }
        else if (options.forall(_.isInstanceOf[SIntConst])) {
          JSInteger(
            title(schema),
            enum = options.map(_.asInstanceOf[SIntConst].constant.toLong),
            options = JSEditorOptions(
              enumTitles = alts.map(_.title)
            )
          )
        }
        else if (options.forall(_.isInstanceOf[SLongConst])) {
          JSInteger(
            title(schema),
            enum = options.map(_.asInstanceOf[SLongConst].constant),
            options = JSEditorOptions(
              enumTitles = alts.map(_.title)
            )
          )
        }
        else if (options.forall(_.isInstanceOf[SFloatConst])) {
          JSNumber(
            title(schema),
            enum = options.map(_.asInstanceOf[SShortConst].constant.toDouble),
            options = JSEditorOptions(
              enumTitles = alts.map(_.title)
            )
          )
        }
        else if (options.forall(_.isInstanceOf[SDoubleConst])) {
          JSNumber(
            title(schema),
            enum = options.map(_.asInstanceOf[SDoubleConst].constant),
            options = JSEditorOptions(
              enumTitles = alts.map(_.title)
            )
          )
        }

        else if (allowTopLevelReference && useReferences && useReference(schema)) {
          JSRef(title(schema), ref = referencePaths(schema))
        }

        else {
          JSObject(
            title(schema),
            oneOf = alts
          )
        }

      case ref: SRef =>
        val resolved = ref.resolve
        if (useReferences && useReference(resolved)) JSRef(title(resolved), ref = referencePaths.get(resolved).getOrElse(
          throw new IllegalStateException(s"Reference path for schema not found: $resolved")
        ))
        else convertOrReference(resolved)
    }
  }
}
