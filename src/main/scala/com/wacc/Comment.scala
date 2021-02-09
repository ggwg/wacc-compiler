package com.wacc

case class Comment(comment: String) extends ASTNodeVoid {
  override def toString: String = comment
}

object Comment {
  val build: (List[Char] => Comment) = comment => Comment(comment.mkString)
}
