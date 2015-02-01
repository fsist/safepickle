package io.github.danarmak.safepickle

/** Produces a stream of tokens. Counterpart of `Writer`.
  * 
  * This is patterned after the Jackson stream interface. At any given time, the Reader has a current token,
  * unless it hasn't started reading yet or is already done. The current token can be inspected using the methods
  * on the trait. Typical usage looks like this:
  * 
  * while (reader.next) {
  *   if (reader.isInt) ... else ...
  * }
  * 
  * The methods `int`, `string` etc. throw an IllegalStateException if there current token is not of the right type,
  * or if there is no current token.
  */
trait Reader {
  /** Advances to the next token. Returns false on EOF. */
  def next(): Boolean
  
  def hasToken(): Boolean
  
  def isInt: Boolean
  def isLong: Boolean
  def isString: Boolean
  def isBoolean: Boolean
  def isNull: Boolean
  def isBinary: Boolean
  
  def isAttributeName: Boolean
  
  def isArrayStart: Boolean
  def isArrayEnd: Boolean
  
  def isObjectStart: Boolean
  def isObjectEnd: Boolean
  
  def int: Int
  def long: Long
  def string: String
  def boolean: Boolean
  def binary: Array[Byte]
  def attributeName: String
}

/** A way of reading the type T which has a direct representation in some, but not all, Reader implementations.
  * 
  * A Reader could support the type directly (i.e. it would be a `Reader with OptionalReader[T]`; otherwise, a fallback
  * implementation would be required that could use any standard Reader.
  */
trait OptionalReader[T] {
  def isT: Boolean
  def t: T
}
