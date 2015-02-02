package io.github.danarmak.safepickle

/** The type of a value represented directly in the serialized form. */
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
}
