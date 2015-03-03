package com.fsist.safepickle

import com.fsist.safepickle.Autogen.|

import scala.collection.mutable
import scala.language.experimental.macros
import scala.language.{higherKinds, implicitConversions}
import scala.reflect.macros.blackbox.Context

/** Entrypoint for the Pickler autogeneration macro. See the documentation in the project's README.md. */
class Autogen(val c: Context) {

  import c.universe._

  private case class Debug(enabled: Boolean)

  // Entrypoints:

  def generate[T: c.WeakTypeTag]: Expr[Pickler[T]] = {
    implicit val debug = Debug(false)
    doGenerate[T]
  }

  def generateDebug[T: c.WeakTypeTag]: Expr[Pickler[T]] = {
    implicit val debug = Debug(true)
    doGenerate[T]
  }

  def generateChildren[T: c.WeakTypeTag, Children: c.WeakTypeTag]: Expr[Pickler[T]] = {
    implicit val debug = Debug(false)
    doGenerateChildren[T, Children]
  }

  def generateChildrenDebug[T: c.WeakTypeTag, Children: c.WeakTypeTag]: Expr[Pickler[T]] = {
    implicit val debug = Debug(true)
    doGenerateChildren[T, Children]
  }

  // End of entrypoints

  private def info(msg: String)(implicit debug: Debug): Unit = if (debug.enabled) c.info(c.enclosingPosition, msg, false)

  private def doGenerate[T: c.WeakTypeTag](implicit debug: Debug): Expr[Pickler[T]] = {
    val tag = implicitly[WeakTypeTag[T]]
    val symbol = tag.tpe.typeSymbol.asType

    checkInitialSymbol(symbol)

    if (symbol.isExistential) {
      c.abort(c.enclosingPosition, s"Cannot generate pickler for existential type $symbol")
    }
    else if (symbol.isImplementationArtifact) {
      c.abort(c.enclosingPosition, s"Not generating pickler for implementation artifact $symbol, this sounds dangerous")
    }
    else if (symbol.isSynthetic) {
      c.abort(c.enclosingPosition, s"Not generating pickler for synthetic type $symbol, this sounds dangerous")
    }
    else if (symbol.typeParams.nonEmpty) {
      c.abort(c.enclosingPosition, s"Cannot generate pickler for generic type $symbol")
    }
    else if (symbol.isClass) {
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

  private def checkInitialSymbol(symbol: Symbol): Unit = {
    var owner = symbol.owner
    while (owner != c.universe.NoSymbol) {
      if (owner.isClass && !owner.isModuleClass)
        c.abort(c.enclosingPosition, s"Cannot generate pickler for $symbol because it is owned by class $owner")
      owner = owner.owner
    }
  }

  def doGenerateChildren[T: c.WeakTypeTag, Children: c.WeakTypeTag](implicit debug: Debug): Expr[Pickler[T]] = {
    val tag = implicitly[WeakTypeTag[T]]
    val symbol = tag.tpe.typeSymbol.asType
    checkInitialSymbol(symbol)

    def collectChildTypes(tpe: Type): List[Type] = {
      if (tpe <:< typeOf[|[_, _]]) {
        val rest = tpe.typeArgs(0)
        val next = tpe.typeArgs(1)
        next :: collectChildTypes(rest)
      }
      else List(tpe)
    }

    val childTypes = collectChildTypes(implicitly[WeakTypeTag[Children]].tpe)
    for (tpe <- childTypes) {
      if (!(tpe <:< tag.tpe)) c.abort(c.enclosingPosition, s"$tpe (passed to Autogen.children) is not a subtype of ${tag.tpe}")
    }
    val childSymbols = childTypes.map(_.typeSymbol.asClass)

    if (!symbol.isClass) {
      c.abort(c.enclosingPosition, s"Autogen.children can only be called on a sealed trait or abstract class, is not applicable to $symbol")
    }
    else {
      val cls = symbol.asClass
      if (!(cls.isAbstract || cls.isTrait)) {
        c.abort(c.enclosingPosition, s"Autogen.children can only be called on a sealed trait or abstract class, is not applicable to $symbol")
      }
      else {
        generateSealedPickler(cls, Some(childSymbols))
      }
    }
  }

  private def generateModulePickler[T](symbol: ModuleSymbol)(implicit ttag: WeakTypeTag[T], debug: Debug): Expr[Pickler[T]] = {
    // A module is pickled by writing its non-qualified name as a string.

    val name = symbol.name.decodedName.toString
    val ttype = ttag.tpe
    val ret = q"new SingletonPickler[$ttype]($name, $symbol)"
    info(s"Generated for module: $ret")
    c.Expr[Pickler[T]](ret)
  }

  /** Wraps a Type and overrides equality to be based on =:= */
  private implicit class EqType(val tpe: Type) {
    override def equals(other: Any): Boolean = other.isInstanceOf[EqType] && tpe =:= other.asInstanceOf[EqType].tpe
    override def hashCode: Int = tpe.toString.hashCode
    override def toString: String = tpe.toString
  }

  /** Wraps `picklerForType` and returns either an implicit val declaration for the pickler, with an autogenerated
    * unique name, or None if no pickler was generated.
    *
    * If an implicit already exists in scope for this type, returns None.
    */
  private def generateImplicitPicklerDeclOf(tpe: Type, existingPicklers: Set[EqType])(implicit debug: Debug): Option[Tree] =
    if (implicitPicklerOf(tpe).isDefined) None
    else generatePicklerOf(tpe, existingPicklers) map {
      case tree =>
        val name = TermName(c.freshName("subpickler$" + tpe.typeSymbol.name.toString))
        q"implicit val $name = $tree"
    }

  /** Returns zero or more implicit val declarations for the picklers in this list of types.
    *
    * Types that don't need declarations because they're already in `existingPicklers`, or are identical to other types
    * in `types`, are omitted.
    */
  private def generateImplicitPicklerDeclsOf(types: List[Type], existingPicklers: Set[EqType])(implicit debug: Debug): List[Tree] = types match {
    case tpe :: rest =>
      val restPicklers = generateImplicitPicklerDeclsOf(rest, existingPicklers + tpe)
      generateImplicitPicklerDeclOf(tpe, existingPicklers) match {
        case Some(pickler) => pickler :: restPicklers
        case None => restPicklers
      }
    case Nil => List.empty
  }

  /** If an implicit Pickler[tpe] is present, returns Some reference to it, otherwise None. */
  private def implicitPicklerOf(tpe: Type)(implicit debug: Debug): Option[Tree] = {
    val picklerTpe = c.universe.appliedType(typeOf[Pickler[_]], List(tpe))
    c.inferImplicitValue(picklerTpe) match {
      case tree if tree.nonEmpty => Some(tree)
      case _ => None
    }
  }

  /** If an implicit pickler for tpe exists, returns a reference to it. Otherwise, returns an expression that creates
    * a new pickler, as per `generatePicklerForType`. */
  private def picklerOf(tpe: Type)(implicit debug: Debug): Tree = {
    implicitPicklerOf(tpe).getOrElse(
      generatePicklerOf(tpe, Set.empty).get
    )
  }

  /** Returns a Pickler[tpe] value. Uses the implicit Pickler for this type if one is in scope,
    * or a call to Autogen combined with CollectionPicklers as needed.
    *
    * @param existingPicklers types for which implicit picklers are already in scope. For these types we return None.
    */
  private def generatePicklerOf(tpe: Type, existingPicklers: Set[EqType])(implicit debug: Debug): Option[Tree] = {
    if (existingPicklers.contains(tpe)) {
      None
    }
    else Some {
      // When combining the methods from CollectionPicklers or TuplePicklers, which are always in scope,
      // we should generate Seq[Autogen[T]] and not Autogen[Seq[T]] so to speak.
      //
      // The easy way to do this is just to check for all the type signatures supported by CollectionPicklers.
      // But then, if the user defines his own pickler-combinators, they wouldn't be used. So we could also
      // pick apart the type T and try to provide picklers for the innermost type params first, until implicitly[]
      // succeeds. The only problem with this approach is that it requires using eval() several times, and that
      // makes the macro very very slow.
      //
      // For now we limit ourselves to the first method.

      def withTypeArgs(typeArgs: List[Type]): Tree = {
        q"""{
              ..${generateImplicitPicklerDeclsOf(typeArgs, existingPicklers + tpe)}
              implicitly[Pickler[$tpe]]
           }"""
      }

      if (tpe <:< typeOf[collection.Map[_, _]]) {
        // Need to specify the dealiased type for Map
        val typeArgs = tpe.baseType(symbolOf[collection.Map[_, _]]).typeArgs
        withTypeArgs(typeArgs)
      }
      else if (tpe <:< typeOf[Iterable[_]]) {
        // Need to specify the dealiased type for Iterable
        val typeArgs = tpe.baseType(symbolOf[collection.Iterable[_]]).typeArgs
        withTypeArgs(typeArgs)
      }
      else if (tpe <:< typeOf[Array[_]]) {
        val typeArgs = tpe.baseType(symbolOf[Array[_]]).typeArgs
        withTypeArgs(typeArgs)
      }
      else if (tpe.dealias.typeSymbol.fullName.startsWith("scala.Tuple")) {
        val typeArgs = tpe.typeArgs
        withTypeArgs(typeArgs)
      }
      else {
        q"Autogen[$tpe]"
      }
    }
  }

  private def generateClassPickler[T](clazz: ClassSymbol)(implicit ttag: WeakTypeTag[T], debug: Debug): Expr[Pickler[T]] = {
    val ctor = clazz.primaryConstructor.asMethod
    if (ctor.typeParams.nonEmpty) c.abort(c.enclosingPosition, s"Cannot generate pickler for generic type $clazz")

    if (ctor.paramLists.size > 1) c.abort(c.enclosingPosition, s"Cannot generate pickler for class with multiple parameter lists $clazz")

    val ttype = ttag.tpe
    val clazzName = clazz.name.decodedName.toString

    // Pickle 0-parameter classes the same way as modules
    if (ctor.paramLists.isEmpty) {
      val ret = q"new SingletonPickler[$ttype]($clazzName, new $clazz)"
      info(s"Generated for class without param lists: $ret")
      c.Expr[Pickler[T]](ret)
    }
    else {
      val params = ctor.paramLists.head
      if (params.isEmpty) {
        val ret = q"new SingletonPickler[$ttype]($clazzName, new $clazz())"
        info(s"Generated for class with empty param list: $ret")
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
          val paramPicklerDecl = q"val $paramPicklerName = ${picklerOf(tpe)}"

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
            cq"$pickledArgName => $name = Some(reader.read[$tpe](true)($paramPicklerName)) ; $argInit = true"
          } else {
            cq"$pickledArgName => $name = reader.read[$tpe](true)($paramPicklerName) ; $argInit = true"
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

        val ret = q""" {
          import com.fsist.safepickle._

          ..$implicitSubPicklers // Outside the class to get the implicits from where the macro was invoked

          new Pickler[$ttype] {
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

            override def toString(): String = "Autogenerated pickler for " + $clazzName
          }
         }"""

        info(s"Generated for class: $ret")

        c.Expr[Pickler[T]](ret)
      }
    }
  }

  /** Returns the declared default values of the parameters in the first parameter list of this method.
    *
    * The actual Trees returned are calls to the compiler-generated methods on the class's companion object that return
    * the default values.
    *
    * Normally we can just generate a call to the compiler-generated method of the companion object which returns the
    * default value, and is named apply$default$index. See: http://stackoverflow.com/a/21970758/1314705
    *
    * However, if the call to the macro itself is inside the companion object, then calling apply$default$index won't
    * compile; scalac will insist this method doesn't exist. In this case we take advantage of the fact we're inside
    * the companion object, and inspect the original constructor declaration to get a copy of the default parameter
    * value Tree directly.
    */
  private def paramDefaultValues(classSym: ClassSymbol, method: MethodSymbol): List[Option[Tree]] = {
    val moduleSym = classSym.companion

    val isInsideCompanionModule = {
      val enclosing = mutable.Stack[Symbol](c.internal.enclosingOwner)
      while (enclosing.top != NoSymbol) {
        enclosing.push(enclosing.top.owner)
      }

      enclosing.exists { sym =>
        if (sym.isModuleClass) {
          val module = sym.asClass.companion.companion
          if (module.isModule && module == moduleSym) true
          else false
        }
        else false
      }
    }

    method.paramLists.head.map(_.asTerm).zipWithIndex.map { case (p, i) =>
      if (!p.isParamWithDefault) None
      else {
        val defaultMethodName = "apply$default$" + (i + 1)

        if (isInsideCompanionModule) {
          Some(
            q"""{
                import scala.tools.reflect._
                import scala.reflect.runtime.{universe => ru}

                val mirr = ru.runtimeMirror(getClass.getClassLoader)

                val tpe = ru.typeOf[$moduleSym]
                val method = tpe.decl(ru.TermName($defaultMethodName)).asMethod
                val classSymbol = mirr.classSymbol(mirr.runtimeClass(tpe)) // This is the module class
                val moduleSymbol = classSymbol.companion.companion.asModule
                val module = mirr.reflectModule(moduleSymbol).instance
                val moduleMirr = mirr.reflect(module)
                val methodMirr = moduleMirr.reflectMethod(method)
                methodMirr.apply().asInstanceOf[${p.typeSignature}]
             }""")
        }
        else {
          val getterName = TermName(defaultMethodName)
          Some(q"$moduleSym.$getterName")
        }
      }
    }
  }

  private def collectDescendantClasses(parent: ClassSymbol): Set[ClassSymbol] = {
    if (parent.isTrait || parent.isAbstract) {
      if (parent.isSealed) {
        parent.typeSignature // Initializes parent.knownDirectSubclasses, works around SI-7588 in some but not all cases
        val direct = parent.knownDirectSubclasses
        if (direct.isEmpty) {
          c.abort(c.enclosingPosition, s"Cannot generate pickler for sealed trait or abstract class '$parent', no subtypes found. " +
            s"If the macro was invoked in the same compilation unit as the definition of $parent, this is due to SI-7588. " +
            s"In that case, use Autogen.children.")
        }
        else direct.map(_.asClass)
      }
      else {
        c.abort(c.enclosingPosition, s"Cannot generate pickler for non-sealed $parent")
      }
    }
    else Set(parent)
  }

  private def generateSealedPickler[T](sealedSym: ClassSymbol, children: Option[List[ClassSymbol]] = None)
                                      (implicit ttag: WeakTypeTag[T], debug: Debug): Expr[Pickler[T]] = {
    val ttype = ttag.tpe

    val traitName = sealedSym.name.decodedName.toString

    case class Subtype(name: TermName, tpe: Type, picklerName: TermName, picklerDecl: Tree, picklerMatchClause: Tree,
                       unpicklerMatchClause: Tree)

    val subtypes = for (subtype <- children.getOrElse(collectDescendantClasses(sealedSym))) yield {
      val subclass = subtype.asClass
      val name = subclass.name.decodedName.toTermName
      val tpe = subclass.toType

      val paramPicklerName = TermName(c.freshName(s"$name$$pickler"))
      val paramPicklerDecl = q"val $paramPicklerName = ${picklerOf(tpe)}"

      val writtenAsDollarValue = subclass.isSealed && (subclass.isTrait || subclass.isAbstract)

      // Analyze the subtype's primary ctor to determine if it has any parameters; if not, it will be written as a
      // single string

      val writtenAsObject = !writtenAsDollarValue && {
        val ctor = subclass.primaryConstructor.asMethod
        if (ctor.typeParams.nonEmpty) c.abort(c.enclosingPosition, s"Generic types are not supported (in subtype $name of $traitName)")
        if (ctor.paramLists.size > 1) c.abort(c.enclosingPosition, "Classes with multiple parameter lists are not supported (in subtype $name of $traitName)")

        ctor.paramLists.nonEmpty && ctor.paramLists.head.nonEmpty
      }

      val dollarValue = "$value"

      val picklerMatchClause = if (writtenAsDollarValue) {
        cq"""value: $tpe =>
                 writer.writeObjectStart()
                 writer.writeAttributeName("$$type")
                 writer.writeString(${name.toString})
                 writer.writeAttributeName($dollarValue)
                 writer.write[$tpe](value, false)($paramPicklerName)
          """
      }
      else if (writtenAsObject) {
        cq"""value: $tpe =>
                 writer.writeObjectStart()
                 writer.writeAttributeName("$$type")
                 writer.writeString(${name.toString})
                 writer.write[$tpe](value, false)($paramPicklerName)
          """
      }
      else {
        cq"""value: $tpe =>
                 writer.writeString(${name.toString})"""
      }

      val unpicklerMatchClause = if (writtenAsDollarValue) {
        cq"""
           ${name.toString} =>
                  reader.assertTokenType(TokenType.AttributeName)
                  if (reader.attributeName != $dollarValue) {
                    throw new UnpicklingException(s"Expected attribute name $$$$value, found $${reader.attributeName}")
                  }

                  reader.nextInObject()
                  reader.read[$tpe](false)($paramPicklerName)
          """
      } else {
        cq"${name.toString} => reader.read[$tpe](false)($paramPicklerName)"
      }

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

    val ret = q"""{
          import com.fsist.safepickle._

          ..$implicitSubPicklers // Outside the class to get the implicits from where the macro was invoked

          new Pickler[$ttype] {
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

            override def toString(): String = "Autogenerated pickler for " + $traitName
          }
         }"""

    info(s"Generated for trait: $ret")

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
  /** Autogenerate a Pickler. See the documentation in the project's README.md. */
  def apply[T]: Pickler[T] = macro Autogen.generate[T]

  /** As [[apply]], but prints the generated code as a compiler informational message.
    * Useful for debugging if the macro-generated code does not compile.
    */
  def debug[T]: Pickler[T] = macro Autogen.generateDebug[T]

  /** A way to list two or more types, e.g. `String | Int | Foo`. */
  type |[A, B]

  /** Works like `Autogen.apply[T]`. T must be a sealed trait or sealed abstract class.
    *
    * The type argument `Children` explicitly specifies the concrete sub-types of T that should be supported.
    * It is necessary to call this instead of Autogen.apply[T] when the macro call is made in the same compilation unit
    * where T is defined, due to SI-7588.
    *
    * @tparam Children should be either a concrete subtype of T, or several types chained with the `|` type, e.g. `A | B | C`.
    */
  def children[T, Children]: Pickler[T] = macro Autogen.generateChildren[T, Children]

  /** As [[children]], but prints the generated code as a compiler informational message.
    * Useful for debugging if the macro-generated code does not compile. */
  def childrenDebug[T, Children]: Pickler[T] = macro Autogen.generateChildrenDebug[T, Children]
}
