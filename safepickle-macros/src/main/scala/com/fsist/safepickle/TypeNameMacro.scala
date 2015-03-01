package com.fsist.safepickle

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

/** A macro that returns the name of the type parameter `T` as a string. */
class TypeNameMacro(val c: Context) {
  import c.universe._

  def make[T: c.WeakTypeTag]: Expr[String] = {
    val tag = implicitly[WeakTypeTag[T]]
    val name = tag.tpe.typeSymbol.fullName
    c.Expr[String](q"$name")
  }
}

object TypeNameMacro {
  def apply[T]: String = macro TypeNameMacro.make[T]
}