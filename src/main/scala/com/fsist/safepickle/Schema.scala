package com.fsist.safepickle

import scala.reflect.runtime.universe._

/** A schema describes the values that the associated pickler can write and read. */
sealed trait Schema {
  /** The Scala type corresponding to this schema; normally this is the associated `pickler.ttag.tpe`. */
  def tpe: Type

  /** Copy this Schema, replacing the tpe value. */
  def withTpe(tpe: Type): Schema
}

object Schema {
  sealed trait AtomicConst[T] {
    def constant: T
  }

  case class SShort(tpe: Type) extends Schema {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SShortConst(tpe: Type, constant: Short) extends Schema with AtomicConst[Short] {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SInt(tpe: Type) extends Schema {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SIntConst(tpe: Type, constant: Int) extends Schema with AtomicConst[Int] {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SLong(tpe: Type) extends Schema {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SLongConst(tpe: Type, constant: Long) extends Schema with AtomicConst[Long] {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SFloat(tpe: Type) extends Schema {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SFloatConst(tpe: Type, constant: Float) extends Schema with AtomicConst[Float] {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SDouble(tpe: Type) extends Schema {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SDoubleConst(tpe: Type, constant: Double) extends Schema with AtomicConst[Double] {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SBoolean(tpe: Type) extends Schema {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SBooleanConst(tpe: Type, constant: Boolean) extends Schema with AtomicConst[Boolean] {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SNull(tpe: Type) extends Schema {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SString(tpe: Type) extends Schema {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SStringConst(tpe: Type, constant: String) extends Schema with AtomicConst[String] {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  /** Repeated items with the same schema. */
  case class SArray(tpe: Type, member: Schema) extends Schema {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  /** A fixed number of items with different schemas. */
  case class STuple(tpe: Type, members: List[Schema]) extends Schema {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  /** An object with a fixed number of attributes, each having a different schema. */
  case class SObject(tpe: Type, members: List[SObjectMember]) extends Schema {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SObjectMember(name: String, schema: Schema, required: Boolean = true)

  /** An object with any number of attributes with different names, all sharing the same value schema. */
  case class SDict(tpe: Type, members: Schema) extends Schema {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  case class SOneOf(tpe: Type, options: List[Schema]) extends Schema {
    override def withTpe(tpe: Type): Schema = copy(tpe = tpe)
  }

  /** A reference is equivalent to the schema returned by `target`, but prevents loops in recursive or mutually recursive
    * schema definitions. */
  case class SRef(toTpe: () => Type, target: () => Schema) extends Schema {
    override def tpe: Type = toTpe()
    override def withTpe(tpe: Type): Schema = copy(toTpe = () => tpe)

    def resolve(): Schema = target() match {
      case ref: SRef => ref.resolve()
      case other => other
    }
  }
}
