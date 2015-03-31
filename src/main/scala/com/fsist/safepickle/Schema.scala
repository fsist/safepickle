package com.fsist.safepickle

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

  case class SShort(desc: Desc = Desc.none, min: Option[Short] = None, max: Option[Short] = None) extends Schema
  val short = SShort()

  case class SInt(desc: Desc = Desc.none, min: Option[Int] = None, max: Option[Int] = None) extends Schema
  val int = SInt()

  case class SLong(desc: Desc = Desc.none, min: Option[Long] = None, max: Option[Long] = None) extends Schema
  val long = SLong()

  case class SFloat(desc: Desc = Desc.none, min: Option[Float] = None, max: Option[Float] = None) extends Schema
  val float = SFloat()

  case class SDouble(desc: Desc = Desc.none, min: Option[Double] = None, max: Option[Double] = None) extends Schema
  val double = SDouble()

  case class SBoolean(desc: Desc = Desc.none) extends Schema
  val boolean = SBoolean()

  case class SNull(desc: Desc = Desc.none) extends Schema
  val nul = SNull()

  case class SString(desc: Desc = Desc.none,
                     minLength: Option[Int] = None, maxLength: Option[Int] = None,
                     pattern: Option[String] = None) extends Schema
  val string = SString()

  case class SConst(value: String, desc: Desc = Desc.none) extends Schema

  case class SArray(member: Schema, desc: Desc = Desc.none,
                    minLength: Option[Int] = None, maxLength: Option[Int] = None) extends Schema

  case class STuple(members: List[Schema], desc: Desc = Desc.none) extends Schema

  case class SObject(members: List[Member], desc: Desc = Desc.none) extends Schema

  case class Member(name: String, schema: Schema, required: Boolean = true)

  case class SDict(members: Schema, desc: Desc = Desc.none) extends Schema

  case class SOneOf(schemas: Set[Schema], desc: Desc = Desc.none) extends Schema

  case class Reference(target: () => Schema, name: String = "") extends Schema {
    override def desc: Desc = Desc(s"Reference to $name")
    override def toString: String = s"Reference to $name"
  }
}