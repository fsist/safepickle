package com.fsist.safepickle

import scala.reflect.runtime.universe._

/** Produces a stream of tokens. Counterpart of `Writer`.
  * 
  * This is patterned after the Jackson stream interface. At any given time, the Reader has a current token,
  * unless it hasn't started reading yet or is already done. The current token can be inspected using the methods
  * on the trait. Typical usage looks like this:
  * 
  * while (reader.next) {
  *   reader.tokenType match ...
  * }
  * 
  * The methods `int`, `string` etc. throw an UnpicklingException if the current token is not of the right type,
  * or if there is no current token.
  */
trait PickleReader {
  /** Advances to the next token. Returns false on EOF. */
  def next(): Boolean

  /** Like next(), but throws an UnexpectedEofException if EOF is encountered. */
  def nextInArray(): Unit = if (! next()) throw new UnexpectedEofException("next array element")

  /** Like next(), but throws an UnexpectedEofException if EOF is encountered. */
  def nextInObject(): Unit = if (! next()) throw new UnexpectedEofException("next object member")
  
  /** Begins returning `true` after the first time `next` returns false. */
  def atEof: Boolean
  
  def tokenType: TokenType

  /** Throws an UnpicklingException if the current tokenType isn't `tt`. */
  def assertTokenType(tt: TokenType): Unit = if (tokenType != tt) throw new UnpicklingException(s"Expected $tt but found $tokenType")
  
  def int: Int
  def long: Long
  def float: Float
  def double: Double
  def string: String
  def boolean: Boolean
  def attributeName: String

  /** Can be overridden by a particular implementation to intercept certain types, based on runtime type checking of
    * the `typeName`, and read them in some backend-specific way without using the provided `pickler`.
    *
    * Otherwise, if the type is not being overridden, delegates to the `pickler` provided.
    *
    * @param typeName the full name of the concrete type. This is returned by Type.toString.
    *                 This is used in place of a TypeTag to improve performance.
    */
  def read[T](typeName: String, expectObjectStart: Boolean = true)(implicit pickler: Pickler[T]): T =
    pickler.unpickle(this, expectObjectStart)

  /** As `read`, but uses a TypeTag, which is slightly more expensive. */
  def readTagged[T](expectObjectStart: Boolean = true)(implicit pickler: Pickler[T], tag: TypeTag[T]): T =
    read(tag.tpe.toString, expectObjectStart)(pickler)
}

