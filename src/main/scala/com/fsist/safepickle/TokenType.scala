package com.fsist.safepickle

/** The type of a lexer-level token that can be read and written directly to a Reader/Writer.
  * 
  * Token types might more naturally be represented as a series of traits implemented by the actual tokens,
  * but that would require instantiating wrapper objects for the tokens, which would hurt performance.
  */
sealed trait TokenType

object TokenType {
  case object Int extends TokenType
  case object Long extends TokenType
  case object Float extends TokenType
  case object Double extends TokenType
  case object Boolean extends TokenType
  case object String extends TokenType
  case object Null extends TokenType
  case object AttributeName extends TokenType
  case object ArrayStart extends TokenType
  case object ArrayEnd extends TokenType
  case object ObjectStart extends TokenType
  case object ObjectEnd extends TokenType
  
  /** Any other type, supported by this pickling format but not required to be supported by other formats. */
  case object Other extends TokenType
}
