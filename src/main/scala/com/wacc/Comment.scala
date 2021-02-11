package com.wacc

import parsley.Parsley

case class Comment(comment: String) extends ASTNodeVoid {
  override def toString: String = comment
}

object Comment {
  def apply(comment: Parsley[List[Char]]): Parsley[Comment] =
    comment.map(cmnt => Comment(cmnt.mkString))
}
