package com.fsist.safepickle

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

/** Entrypoint for the Pickler autogeneration macro. See the documentation in the project's README.md. */
class Autogen(val c: Context) {

  import c.universe._

  private def info(msg: String): Unit = c.info(c.enclosingPosition, msg, false)

  def generate[T: c.WeakTypeTag]: Expr[Pickler[T]] = {
    val tag = implicitly[WeakTypeTag[T]]
    val symbol = tag.tpe.typeSymbol.asType

    {
      var owner = symbol.owner
      while (owner != c.universe.NoSymbol) {
        if (owner.isClass && !owner.isModuleClass)
          c.abort(c.enclosingPosition, s"Cannot generate pickler for $symbol because it is owned by class $owner")
        owner = owner.owner
      }
    }

    if (symbol.isClass) {
      val cls = symbol.asClass
      if (cls.isTrait || cls.isAbstract) {
        if (cls.isSealed) {
          generateSealedPickler(cls)
        }
        else c.abort(c.enclosingPosition, s"Cannot generate pickler for non-sealed $cls")
      }
      else if (cls.isModuleClass) {
        val module = cls.module.asModule
        generateModulePickler(module)
      }
      else if (cls.isAbstract) {
        c.abort(c.enclosingPosition, s"Cannot generate pickler for abstract $cls")
      }
      else if (cls.isPackageClass) {
        c.abort(c.enclosingPosition, s"Cannot generate pickler for package $cls")
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

  private def generateModulePickler[T: c.WeakTypeTag](symbol: ModuleSymbol): Expr[Pickler[T]] = {
    // A module is pickled by writing its non-qualified name as a string.

    val name = symbol.name.decodedName.toString
    val ttype = tq"${implicitly[c.WeakTypeTag[T]].tpe}"
    val ret = q"new SingletonPickler[$ttype]($name, $symbol)"
    //    info(s"Generated for module: $ret")
    c.Expr[Pickler[T]](ret)
  }

  private def generateClassPickler[T: c.WeakTypeTag](clazz: ClassSymbol): Expr[Pickler[T]] = {
    val ctor = clazz.primaryConstructor.asMethod
    if (ctor.typeParams.nonEmpty) c.abort(c.enclosingPosition, s"Cannot generate pickler for generic type $clazz")

    if (ctor.paramLists.size > 1) c.abort(c.enclosingPosition, s"Cannot generate pickler for class with multiple parameter lists $clazz")

    val ttype = tq"${implicitly[c.WeakTypeTag[T]].tpe}"

    val clazzName = clazz.name.decodedName.toString

    // Pickle 0-parameter classes the same way as modules
    if (ctor.paramLists.isEmpty) {
      val ret = q"new SingletonPickler[$ttype]($clazzName, new $clazz)"
      //      info(s"Generated for class without param lists: $ret")
      c.Expr[Pickler[T]](ret)
    }
    else {
      val params = ctor.paramLists.head
      if (params.isEmpty) {
        val ret = q"new SingletonPickler[$ttype]($clazzName, new $clazz())"
        //        info(s"Generated for class with empty param list: $ret")
        c.Expr[Pickler[T]](ret)
      }
      else {
        case class ParamInfo(name: TermName, tpe: Type, picklerName: TermName, picklerDecl: Tree, writeParam: Tree,
                             argDecl: Tree, argInit: TermName, argInitDecl: Tree, argNameMatchClause: Tree,
                             getArgValue: Tree, defaultArgValueName: TermName, defaultArgValueDecl: Option[Tree])

        val defaultValues = paramDefaultValues(clazz, ctor)
        val paramInfos: List[ParamInfo] = for ((param, defaultValue) <- params.zip(defaultValues)) yield {
          val name = param.name.decodedName.toTermName

          val isOption = param.typeSignature <:< c.typeOf[Option[Any]]

          val tpe = if (!isOption) param.typeSignature
          else {
            param.typeSignature.typeArgs.head
          }

          val paramPicklerName = TermName(name + "$paramPickler")
          val paramPicklerDecl = q"val $paramPicklerName = implicitly[Pickler[$tpe]]"

          val defaultArgValueName = TermName(name + "$default")
          val defaultArgValueDecl = defaultValue map (tree => q"val $defaultArgValueName = $tree")

          val pickledArgName = param.annotations.find { ann =>
            ann.tree.tpe =:= typeOf[com.fsist.safepickle.Name]
          }.map { ann =>
            val nameTree = ann.tree.children.tail.head
            if (!nameTree.isInstanceOf[LiteralApi])
              c.abort(c.enclosingPosition, s"Argument to Name annotation must be a String literal (in $clazzName.$name)")
            nameTree.asInstanceOf[LiteralApi].value.value.asInstanceOf[String]
          }.getOrElse(name.toString)

          val doWriteParam = if (!isOption) {
            q"""writer.writeAttributeName($pickledArgName)
                writer.write[$tpe](paramValue, true)($paramPicklerName)
              """
          }
          else {
            q"""paramValue match {
                  case Some(inner) =>
                    writer.writeAttributeName($pickledArgName)
                    writer.write[$tpe](inner, true)($paramPicklerName)
                  case None =>
                }
               """
          }

          val writeParam = if (defaultValue.isDefined) {
            q"""{
                val paramValue = tvalue.$name
                if (paramValue != $defaultArgValueName) $doWriteParam
               }
             """
          }
          else {
            q"""{
                val paramValue = tvalue.$name
                $doWriteParam
               }
             """
          }

          val argDecl = if (defaultValue.isDefined) {
            if (isOption) q"var $name: Option[$tpe] = $defaultArgValueName"
            else q"var $name: $tpe = $defaultArgValueName"
          }
          else {
            if (isOption) q"var $name: Option[$tpe] = None"
            else q"var $name: $tpe = null.asInstanceOf[$tpe]"
          }

          val argInit = TermName(name + "$initialized")
          val argInitDecl = q"var $argInit: Boolean = ${defaultValue.isDefined || isOption}"

          val argNameMatchClause = if (isOption) {
            cq"$pickledArgName => $name = Some(reader.read[$tpe](${tpe.toString}, true)($paramPicklerName)) ; $argInit = true"
          } else {
            cq"$pickledArgName => $name = reader.read[$tpe](${tpe.toString}, true)($paramPicklerName) ; $argInit = true"
          }

          val paramFullName = s"$clazzName.$name"
          val getArgValue = if (isOption) {
            q"$name"
          }
          else {
            q"""if ($argInit) $name else throw new UnpicklingException("No value found for " + $paramFullName)"""
          }

          ParamInfo(name, tpe, paramPicklerName, paramPicklerDecl, writeParam, argDecl, argInit, argInitDecl,
            argNameMatchClause, getArgValue, defaultArgValueName, defaultArgValueDecl)
        }

        val implicitSubPicklers = q"..${paramInfos.map(_.picklerDecl)}"
        val writeParams = q"..${paramInfos.map(_.writeParam)}"
        val argDecls = q"..${paramInfos.map(_.argDecl)}"
        val argInitDecls = q"..${paramInfos.map(_.argInitDecl)}"
        val argNameMatchClauses = q"..${paramInfos.map(_.argNameMatchClause)}"
        val getArgValues = q"..${paramInfos.map(_.getArgValue)}"
        val defaultArgValues = q"..${paramInfos.map(_.defaultArgValueDecl).filter(_.isDefined).map(_.get)}"

        val readAttributes =
          q"""while (reader.tokenType != TokenType.ObjectEnd) {
                val argName = reader.attributeName
                reader.nextInObject()

                argName match {
                  case ..$argNameMatchClauses

                  case other => // Discard argument with unexpected name (or type tag)
                }
                reader.nextInObject()
              }"""

        val ret = q"""
          new Pickler[$ttype] {
            import com.fsist.safepickle._
            import PrimitivePicklers._

            ..$implicitSubPicklers

            ..$defaultArgValues

            override def pickle(tvalue: $ttype, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit = {
              if (emitObjectStart) writer.writeObjectStart()
              ..$writeParams
              writer.writeObjectEnd()
            }

            override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): $ttype = {
              ..$argInitDecls
              ..$argDecls

              reader.tokenType match {
                case TokenType.String => 
                  // Can be a String if all params have default values, and was written by an oder version that
                  // didn't have any params

                case TokenType.ObjectStart if expectObjectStart =>
                  reader.nextInObject()
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

//       info(s"Generated for class: $ret")

        c.Expr[Pickler[T]](ret)
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

  private def generateSealedPickler[T: c.WeakTypeTag](sealedSym: ClassSymbol): Expr[Pickler[T]] = {
    val ttype = tq"${implicitly[c.WeakTypeTag[T]].tpe}"

    val traitName = sealedSym.name.decodedName.toString

    case class Subtype(name: TermName, tpe: Type, picklerName: TermName, picklerDecl: Tree, picklerMatchClause: Tree,
                       unpicklerMatchClause: Tree)

    def collectDescendantClasses(parent: ClassSymbol): Set[ClassSymbol] = {
      if (parent.isTrait || parent.isAbstract) {
        if (parent.isSealed) {
          val direct = parent.knownDirectSubclasses
          if (direct.isEmpty) {
            c.abort(c.enclosingPosition, s"Cannot generate pickler for sealed trait or abstract class '$parent', no subtypes found. " +
              s"If it has subtypes, this can happen if you invoke the macro in the same file where the trait or abstract class is defined. " +
              s"In this case, the macro invocation must come after all of the subtype definitions.")
          }
          else direct.flatMap(sym => collectDescendantClasses(sym.asClass))
        }
        else {
          c.abort(c.enclosingPosition, s"Cannot generate pickler for non-sealed $parent")
        }
      }
      else Set(parent)
    }

    val subtypes = for (subtype <- collectDescendantClasses(sealedSym)) yield {
      val subclass = subtype.asClass
      val name = subclass.name.decodedName.toTermName
      val tpe = subclass.toType

      val paramPicklerName = TermName(c.freshName(s"paramPickler_$name"))
      val paramPicklerDecl = q"val $paramPicklerName = implicitly[Pickler[$tpe]]"

      // Analyze the subtype's primary ctor to determine if it has any parameters; if not, it will be written as a
      // single string

      val writtenAsObject = {
        subclass.isTrait || {
          // Another sealed trait
          val ctor = subclass.primaryConstructor.asMethod
          if (ctor.typeParams.nonEmpty) c.abort(c.enclosingPosition, s"Generic types are not supported (in subtype $name of $traitName)")
          if (ctor.paramLists.size > 1) c.abort(c.enclosingPosition, "Classes with multiple parameter lists are not supported (in subtype $name of $traitName)")

          ctor.paramLists.nonEmpty && ctor.paramLists.head.nonEmpty
        }
      }

      val picklerMatchClause = if (writtenAsObject) {
        cq"""value: ${tpe} =>
           writer.writeObjectStart()
           writer.writeAttributeName("$$type")
           writer.writeString(${name.toString})

           writer.write[$tpe](value, false)($paramPicklerName)"""
      }
      else {
        cq"""value: $tpe =>
          writer.writeString(${name.toString})"""
      }

      val unpicklerMatchClause = cq"${name.toString} => reader.read[$tpe](${tpe.toString}, false)($paramPicklerName)"

      Subtype(name, tpe, paramPicklerName, paramPicklerDecl, picklerMatchClause, unpicklerMatchClause)
    }

    for (subtype <- subtypes;
         otherSubtype <- subtypes if subtype.tpe.erasure =:= otherSubtype.tpe.erasure && subtype.name != otherSubtype.name) {
      c.abort(c.enclosingPosition,
        s"Two concrete subtypes of sealed trait $traitName have the same erasure, so we can't generate a pickler that would " +
          s"produce the correct pickled type tag at runtime. The subtypes ${subtype.name} and ${otherSubtype.name} " +
          s"both have erased type ${subtype.tpe.erasure}.")
    }

    for (subtype <- subtypes;
         otherSubtype <- subtypes if otherSubtype != subtype && subtype.name.decodedName.toString == otherSubtype.name.decodedName.toString) {
      c.abort(c.enclosingPosition,
        s"Two concrete subtypes of sealed trait $traitName have the same local name ${subtype.name.decodedName}, " +
          s"so we can't distinguish between them with a type tag. Such a naming convention is usually bad practice.")
    }

    val implicitSubPicklers = q"..${subtypes.map(_.picklerDecl)}"
    val picklerMatchClauses = q"..${subtypes.map(_.picklerMatchClause)}"
    val unpicklerMatchClauses = q"..${subtypes.map(_.unpicklerMatchClause)}"

    val tokenType = "$type"

    val ret = q"""
          new Pickler[$ttype] {
            import com.fsist.safepickle._
            import PrimitivePicklers._

            ..$implicitSubPicklers

            override def pickle(tvalue: $ttype, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit = {
              tvalue match {
                case ..$picklerMatchClauses
                case null => throw new IllegalArgumentException("Refusing to pickle null value of type " + $traitName)
              }
            }

            override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): $ttype = {
              reader.tokenType match {
                case TokenType.String => 
                  // String value indicates the type of a no-arg subtype, possibly using default arguments
                  reader.string match {
                    case ..$unpicklerMatchClauses
                    case other => throw new UnpicklingException(s"Unexpected (primitive) type tag $$other as descendant of sealed trait " + $traitName)
                  }

                case TokenType.ObjectStart =>
                  // First attribute should be the type tag
                  reader.nextInObject()

                  reader.assertTokenType(TokenType.AttributeName)
                  if (reader.attributeName != $tokenType) {
                    throw new UnpicklingException(s"Expected an attribute name (" + $tokenType + s"), found $${reader.attributeName}")
                  }

                  reader.nextInObject()
                  reader.assertTokenType(TokenType.String)
                  val typeTag = reader.string
                  reader.nextInObject()

                  typeTag match {
                    case ..$unpicklerMatchClauses
                    case other => throw new UnpicklingException(s"Unexpected (explicit) type tag $$other as descendant of sealed trait " + $traitName)
                  }

                case other => throw new IllegalStateException("Unexpected next token type $$other")
              }
            }
          }
         """

    //            info(s"Generated for trait: $ret")

    c.Expr[Pickler[T]](ret)
  }
}

/** A pickler for a value T that pickles it to the fixed string `name`. */
class SingletonPickler[T](name: String, value: T) extends Pickler[T] {
  override def pickle(t: T, writer: PickleWriter[_], emitObjectStart: Boolean = true): Unit = {
    writer.writeString(name)
  }
  override def unpickle(reader: PickleReader, expectObjectStart: Boolean = true): T = {
    val read = reader.string
    if (read == name) value else throw new UnpicklingException(s"Expected to read $name but found $read")
  }
}

object Autogen {
  /** Implicit definitions of the autogen macro for all types T */
  object Implicits {
    /** Autogenerate a Pickler. See the documentation in the project's README.md. */
    implicit def generate[T]: Pickler[T] = macro Autogen.generate[T]
  }
}