package com.wacc

case class Comment(comment: String) {
  override def toString: String = comment
}

object Comment {
  val build: (List[Char] => Comment) = comment => Comment(comment.mkString)
}
