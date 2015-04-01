package com.fsist.safepickle

import com.fsist.safepickle.Schema.SOneOf.SchemaOption
import com.fsist.safepickle.Schema._
import com.typesafe.scalalogging.LazyLogging
import scala.collection.mutable

sealed trait Schema extends LazyLogging {
  def desc: Desc

  override def toString: String = s"${desc.name} (${getClass.getSimpleName}) #${System.identityHashCode(this)}"
}

object Schema {
  case class Desc(name: String = "", description: String = "", typeHint: Option[String] = None)

  object Desc {
    val none = Desc()
  }

  case class SShort(desc: Desc = Desc.none, min: Option[Short] = None, max: Option[Short] = None,
                    readOnly: Boolean = false, default: Option[Short] = None, hidden: Boolean = false) extends Schema
  val short = SShort()

  case class SInt(desc: Desc = Desc.none, min: Option[Int] = None, max: Option[Int] = None,
                  readOnly: Boolean = false, default: Option[Int] = None, hidden: Boolean = false) extends Schema
  val int = SInt()

  case class SLong(desc: Desc = Desc.none, min: Option[Long] = None, max: Option[Long] = None,
                   readOnly: Boolean = false, default: Option[Long] = None, hidden: Boolean = false) extends Schema
  val long = SLong()

  case class SFloat(desc: Desc = Desc.none, min: Option[Float] = None, max: Option[Float] = None,
                    readOnly: Boolean = false, default: Option[Float] = None, hidden: Boolean = false) extends Schema
  val float = SFloat()

  case class SDouble(desc: Desc = Desc.none, min: Option[Double] = None, max: Option[Double] = None,
                     readOnly: Boolean = false, default: Option[Double] = None, hidden: Boolean = false) extends Schema
  val double = SDouble()

  case class SBoolean(desc: Desc = Desc.none,
                      readOnly: Boolean = false, default: Option[Boolean] = None, hidden: Boolean = false) extends Schema
  val boolean = SBoolean()

  case class SNull(desc: Desc = Desc.none) extends Schema
  val nul = SNull()

  case class SString(desc: Desc = Desc.none,
                     minLength: Option[Int] = None, maxLength: Option[Int] = None,
                     pattern: Option[String] = None, enum: List[String] = Nil,
                     readOnly: Boolean = false, default: Option[String] = None, hidden: Boolean = false) extends Schema
  val string = SString()

  case class SArray(member: Schema, desc: Desc = Desc.none,
                    minLength: Option[Int] = None, maxLength: Option[Int] = None) extends Schema

  case class STuple(members: List[Schema], desc: Desc = Desc.none) extends Schema

  case class SObject(members: List[Member], desc: Desc = Desc.none) extends Schema

  case class Member(name: String, schema: Schema, required: Boolean = true)

  case class SDict(members: Schema, desc: Desc = Desc.none) extends Schema

  case class SOneOf(options: Set[SchemaOption], desc: Desc = Desc.none) extends Schema

  object SOneOf {
    case class SchemaOption(schema: Schema, typeField: Option[String])
  }

  case class Reference(target: () => Schema, name: String = "") extends Schema {
    override def desc: Desc = Desc(s"Reference to $name")
    override def toString: String = s"Reference to $name"
  }
}
