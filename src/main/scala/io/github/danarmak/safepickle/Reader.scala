package io.github.danarmak.safepickle

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
  * The methods `int`, `string` etc. throw an IllegalStateException if there current token is not of the right type,
  * or if there is no current token.
  */
trait Reader {
  /** Advances to the next token. Returns false on EOF. */
  def next(): Boolean
  
  /** Begins returning `true` after the first time `next` returns false. */
  def atEof: Boolean
  
  def tokenType: TokenType
  
  def int: Int
  def long: Long
  def string: String
  def boolean: Boolean
  def attributeName: String
}
