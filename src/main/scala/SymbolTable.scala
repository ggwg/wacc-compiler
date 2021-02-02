import scala.collection.mutable

import WACCParser._
import com.wacc._

class SymbolTable(enclosingSymbolTable: SymbolTable) {

  var encSymbolTable: SymbolTable = enclosingSymbolTable
  var dict = new mutable.HashMap[String, ASTNode]()

  def add(name: String, obj: ASTNode) : Unit = {
    dict(name) = obj
  }

  // Returns Type object else null if name not in dict
  // NOTE: can also use dict.get(), which returns Option[Type]
  def lookup(name: String) : ASTNode = {
    return dict.getOrElse(name, null)
  }

  override def toString: String = dict.toString()

}
