package com.fsist.safepickle

/** Like [[TokenType]], but for reified trees, with Object and Array nodes instead of Object/Array Start/End. */
sealed trait TreeNodeType

object TreeNodeType {

  case object Int extends TreeNodeType

  case object Long extends TreeNodeType

  case object Float extends TreeNodeType

  case object Double extends TreeNodeType

  case object Boolean extends TreeNodeType

  case object String extends TreeNodeType

  case object Null extends TreeNodeType

  case object Array extends TreeNodeType

  case object Object extends TreeNodeType

  /** Any other type directly represented in the tree structure */
  case object Other extends TreeNodeType
}

