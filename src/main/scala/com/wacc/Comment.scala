package com.wacc

import parsley.Parsley

case class Comment(comment: String) extends ASTNodeVoid {
  override def toString: String = comment
}

object Comment {
  val build: (List[Char] => Comment) = comment => Comment(comment.mkString)
  def apply(comment: Parsley[List[Char]]): Parsley[Comment] =
    comment.map(cmnt => Comment(cmnt.mkString))
}
