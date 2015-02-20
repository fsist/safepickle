package io.github.danarmak.safepickle

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

/** Entrypoint for the Pickler autogeneration macro. See the documentation in the project's README.md. */
class Autogen(val c: Context) {

  import c.universe._

  def generate[T: c.WeakTypeTag, Backend <: PicklingBackend : c.WeakTypeTag]: Expr[Pickler[T, Backend]] = {
    val tag = implicitly[WeakTypeTag[T]]
    val symbol = tag.tpe.typeSymbol.asType

    if (symbol.isClass) {
      val cls = symbol.asClass
      if (cls.isTrait) {
        if (cls.isSealed) {
          generateTraitPickler(cls)
        }
        else c.abort(c.enclosingPosition, s"Cannot generate pickler for non-sealed trait $cls")
      }
      else if (cls.isModuleClass) {
        val module = cls.module.asModule
        generateModulePickler(module)
      }
      else if (cls.isAbstract) {
        c.abort(c.enclosingPosition, s"Cannot generate pickler for abstract class $cls")
      }
      else if (cls.isPackageClass) {
        c.abort(c.enclosingPosition, s"Cannot generate pickler for package class $cls")
      }
      else {
        generateClassPickler(cls)
      }
    }
    else if (symbol.isModule) {
      generateModulePickler(symbol.asModule)
    }
    else {
      c.abort(c.enclosingPosition, s"Cannot generate pickler for type $symbol")
    }
  }

  private def generateModulePickler[T: c.WeakTypeTag, Backend <: PicklingBackend : c.WeakTypeTag](symbol: ModuleSymbol): Expr[Pickler[T, Backend]] = {
    // A module is serialized by writing its non-qualified name as a string.

    val name = symbol.name.decodedName.toString
    val ttype = tq"${implicitly[c.WeakTypeTag[T]].tpe}"
    val btype = tq"${implicitly[c.WeakTypeTag[Backend]].tpe}"
    c.Expr(q"new SingletonPickler[$ttype, $btype]($name, $symbol)")
  }

  private def generateClassPickler[T: c.WeakTypeTag, Backend <: PicklingBackend : c.WeakTypeTag](clazz: ClassSymbol): Expr[Pickler[T, Backend]] = {
    val ctor = clazz.primaryConstructor.asMethod
    if (ctor.typeParams.nonEmpty) c.abort(c.enclosingPosition, "Generic types are not supported")

    if (ctor.paramLists.size > 1) c.abort(c.enclosingPosition, "Classes with multiple parameter lists are not supported")

    val ttype = tq"${implicitly[c.WeakTypeTag[T]].tpe}"
    val btype = tq"${implicitly[c.WeakTypeTag[Backend]].tpe}"

    val clazzName = clazz.name.decodedName.toString

    // Serialize 0-parameter classes the same way as modules
    if (ctor.paramLists.isEmpty) {
      c.Expr(q"new SingletonPickler[$ttype, $btype]($clazzName, new $clazz)")
    }
    else {
      val params = ctor.paramLists.head
      if (params.isEmpty) {
        c.Expr(q"new SingletonPickler[$ttype, $btype]($clazzName, new $clazz())")
      }
      else {
        case class ParamInfo(name: TermName, tpe: Type, picklerName: TermName, picklerDecl: Tree, writeParam: Tree,
                             argDecl: Tree, argInit: TermName, argInitDecl: Tree, argNameMatchClause: Tree, getArgValue: Tree)

        val defaultValues = paramDefaultValues(clazz, ctor)
        val paramInfos: List[ParamInfo] = for ((param, defaultValue) <- params.zip(defaultValues)) yield {
          val name = param.name.decodedName.toTermName
          val tpe = param.typeSignature

          val paramPicklerName = TermName(c.freshName(s"paramPickler_$name"))
          val paramPicklerDecl = q"val $paramPicklerName = implicitly[Pickler[$tpe, $btype]]"

          val writeParam =
            q"""writer.writeAttributeName(${name.toString})
               $paramPicklerName.pickle(tvalue.$name, writer)"""

          val argDecl = defaultValue match {
            case Some(value) => q"var $name: $tpe = $value"
            case None => q"var $name: $tpe = null.asInstanceOf[$tpe]"
          }

          val argInit = TermName(name.toString + "$initialized")
          val argInitDecl = q"var $argInit: Boolean = false"

          val argNameMatchClause = cq"${name.toString} => $name = $paramPicklerName.unpickle(reader) ; $argInit = true"

          val paramFullName = s"$clazzName.$name"
          val getArgValue = q"""if ($argInit) $name else throw new IllegalArgumentException("No value found for " + $paramFullName)"""

          ParamInfo(name, tpe, paramPicklerName, paramPicklerDecl, writeParam, argDecl, argInit, argInitDecl, argNameMatchClause, getArgValue)
        }

        val implicitSubPicklers = q"..${paramInfos.map(_.picklerDecl)}"
        val writeParams = q"..${paramInfos.map(_.writeParam)}"
        val argDecls = q"..${paramInfos.map(_.argDecl)}"
        val argInitDecls = q"..${paramInfos.map(_.argInitDecl)}"
        val argNameMatchClauses = q"..${paramInfos.map(_.argNameMatchClause)}"
        val getArgValues = q"..${paramInfos.map(_.getArgValue)}"

        val readAttributes =
          q"""while (reader.tokenType != TokenType.ObjectEnd) {
                val argName = reader.attributeName
                reader.next()

                argName match {
                  case ..$argNameMatchClauses

                  case other => // Discard argument with unexpected name (or type tag)
                }
                reader.next()
              }"""

        val ret = q"""
          new Pickler[$ttype, $btype] {
            import io.github.danarmak.safepickle._
            import PrimitivePicklers._
            import CollectionPicklers._

            ..$implicitSubPicklers

            override def pickle(tvalue: $ttype, writer: $btype#PickleWriter, emitObjectStart: Boolean = true): Unit = {
              if (emitObjectStart) writer.writeObjectStart()
              ..$writeParams
              writer.writeObjectEnd()
            }

            override def unpickle(reader: $btype#PickleReader, expectObjectStart: Boolean = true): $ttype = {
              ..$argInitDecls
              ..$argDecls

              reader.tokenType match {
                case TokenType.String => 
                  // Can be a String if all params have default values, and was written by an oder version that
                  // didn't have any params

                case TokenType.ObjectStart if expectObjectStart =>
                  reader.next()
                  $readAttributes

                case TokenType.AttributeName if ! expectObjectStart =>
                  $readAttributes

                case TokenType.ObjectEnd if ! expectObjectStart => // Empty object

                case other => throw new IllegalStateException("Unexpected next token type $$other")
              }

              new $clazz(..$getArgValues)
            }
          }
         """

        //                c.info(c.enclosingPosition, s"Generated: $ret", false)

        c.Expr(ret)
      }
    }
  }

  /** Returns the declared default values of the parameters in the first parameter list of this method.
    *
    * The actual Trees returned are calls to the compiler-generated methods on the class's companion object that return
    * the default values.
    *
    * See: http://stackoverflow.com/a/21970758/1314705 
    */
  private def paramDefaultValues(classSym: ClassSymbol, method: MethodSymbol): List[Option[Tree]] = {
    val moduleSym = classSym.companion

    method.paramLists.head.map(_.asTerm).zipWithIndex.map { case (p, i) =>
      if (!p.isParamWithDefault) None
      else {
        val getterName = TermName("apply$default$" + (i + 1))
        Some(q"$moduleSym.$getterName")
      }
    }
  }

  private def generateTraitPickler[T: c.WeakTypeTag, Backend <: PicklingBackend : c.WeakTypeTag](traitSym: ClassSymbol): Expr[Pickler[T, Backend]] = {
    val ttype = tq"${implicitly[c.WeakTypeTag[T]].tpe}"
    val btype = tq"${implicitly[c.WeakTypeTag[Backend]].tpe}"

    val traitName = traitSym.name.decodedName.toString

    case class Subtype(name: TermName, tpe: Type, picklerName: TermName, picklerDecl: Tree, picklerMatchClause: Tree,
                       unpicklerMatchClause: Tree)

    val subtypes = for (subtype <- traitSym.knownDirectSubclasses) yield {
      val subclass = subtype.asClass
      val name = subclass.name.decodedName.toTermName
      val tpe = subclass.toType

      val paramPicklerName = TermName(c.freshName(s"paramPickler_$name"))
      val paramPicklerDecl = q"val $paramPicklerName = implicitly[Pickler[$tpe, $btype]]"

      // Analyze the subtype's primary ctor to determine if it has any parameters; if not, it will be written as a
      // single string

      val ctor = subclass.primaryConstructor.asMethod
      if (ctor.typeParams.nonEmpty) c.abort(c.enclosingPosition, s"Generic types are not supported (in subtype $name of $traitName)")
      if (ctor.paramLists.size > 1) c.abort(c.enclosingPosition, "Classes with multiple parameter lists are not supported (in subtype $name of $traitName)")

      val hasParams = ctor.paramLists.nonEmpty && ctor.paramLists.head.nonEmpty

      val picklerMatchClause = if (hasParams) {
        cq"""value: $tpe => 
           writer.writeObjectStart()
           writer.writeAttributeName("$$type")
           writer.writeString(${name.toString})
           
           $paramPicklerName.pickle(value, writer, false)"""
      }
      else {
        cq"""value: $tpe =>
          writer.writeString(${name.toString})"""
      }

      val unpicklerMatchClause = cq"${name.toString} => $paramPicklerName.unpickle(reader, false)"

      Subtype(name, tpe, paramPicklerName, paramPicklerDecl, picklerMatchClause, unpicklerMatchClause)
    }

    for (subtype <- subtypes;
         otherSubtype <- subtypes if subtype.tpe.erasure =:= otherSubtype.tpe.erasure && subtype.name != otherSubtype.name) {
      throw new IllegalArgumentException(
        s"Two concrete subtypes of sealed trait $traitName have the same erasure, so we can't generate a pickler that would " +
          s"produce the correct serialized type tag at runtime. The subtypes ${subtype.name} and ${otherSubtype.name} " +
          s"both have erased type ${subtype.tpe.erasure}.")
    }

    val implicitSubPicklers = q"..${subtypes.map(_.picklerDecl)}"
    val picklerMatchClauses = q"..${subtypes.map(_.picklerMatchClause)}"
    val unpicklerMatchClauses = q"..${subtypes.map(_.unpicklerMatchClause)}"

    val tokenType = "$type"

    val ret = q"""
          new Pickler[$ttype, $btype] {
            import io.github.danarmak.safepickle._
            import PrimitivePicklers._
            import CollectionPicklers._

            ..$implicitSubPicklers

            override def pickle(tvalue: $ttype, writer: $btype#PickleWriter, emitObjectStart: Boolean = true): Unit = {
              tvalue match {
                case ..$picklerMatchClauses
                case null => throw new IllegalArgumentException("Refusing to serialize null value of type " + $traitName)
              }
            }

            override def unpickle(reader: $btype#PickleReader, expectObjectStart: Boolean = true): $ttype = {
              reader.tokenType match {
                case TokenType.String => 
                  // String value indicates the type of a no-arg subtype, possibly using default arguments
                  reader.string match {
                    case ..$unpicklerMatchClauses
                    case other => throw new IllegalArgumentException(s"Unexpected (primitive) type tag $$other as descendant of sealed trait " + $traitName)
                  }

                case TokenType.ObjectStart =>
                  // First attribute should be the type tag
                  reader.next()

                  if (reader.tokenType != TokenType.AttributeName) {
                    throw new IllegalArgumentException(s"Expected an attribute name (" + $tokenType+ s"), found token type $${reader.tokenType}")
                  }
                  if (reader.attributeName != $tokenType) {
                    throw new IllegalArgumentException(s"Expected an attribute name (" + $tokenType + s"), found $${reader.attributeName}")
                  }

                  reader.next()
                  if (reader.tokenType != TokenType.String) {
                    throw new IllegalArgumentException(s"Type tag attribute should have a string value, but found $${reader.tokenType}")
                  }

                  val typeTag = reader.string
                  reader.next()

                  typeTag match {
                    case ..$unpicklerMatchClauses
                    case other => throw new IllegalArgumentException(s"Unexpected (explicit) type tag $$other as descendant of sealed trait " + $traitName)
                  }

                case other => throw new IllegalStateException("Unexpected next token type $$other")
              }
            }
          }
         """

//    c.info(c.enclosingPosition, s"Generated: $ret", false)

    c.Expr(ret)
  }
}

/** A pickler for a value T that serializes it to the fixed string `name`. */
class SingletonPickler[T, Backend <: PicklingBackend](name: String, value: T) extends Pickler[T, Backend] {
  override def pickle(t: T, writer: Backend#PickleWriter, emitObjectStart: Boolean = true): Unit = {
    writer.writeString(name)
  }
  override def unpickle(reader: Backend#PickleReader, expectObjectStart: Boolean = true): T = {
    val read = reader.string
    if (read == name) value else throw new IllegalStateException(s"Expected to read $name but found $read")
  }
}
