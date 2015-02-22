package com.fsist.safepickle

/** Produces a stream of tokens. Counterpart of `Writer`.
  * 
  * This is patterned after the Jackson stream interface. At any given time, the Reader has a current token,
  * unless it hasn't started reading yet or is already done. The current token can be inspected using the methods
  * on the trait. Typical usage looks like this:
  * 
  * while (reader.next) {
  *   reader.tokenTYpe match ...
  * }
  * 
  * The methods `int`, `string` etc. throw an IllegalStateException if the current token is not of the right type,
  * or if there is no current token.
  */
trait Reader[Backend <: PicklingBackend] {
  /** Advances to the next token. Returns false on EOF. */
  def next(): Boolean
  
  /** Begins returning `true` after the first time `next` returns false. */
  def atEof: Boolean
  
  def tokenType: TokenType
  
  def int: Int
  def long: Long
  def float: Float
  def double: Double
  def string: String
  def boolean: Boolean
  def attributeName: String
  
  def unpickle[T](implicit unpickler: Pickler[T, Backend]): T = unpickler.unpickle(this.asInstanceOf[Backend#PickleReader])
}

object Reader {
  type Generic = Reader[PicklingBackend]
}
