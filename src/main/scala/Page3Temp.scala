import com.wacc.assignment.ArrayLiter
import com.wacc.expressions.{Expression, StringLiter}
import com.wacc.functions.Program
import com.wacc.miscellaneous.Comment
import com.wacc.primitives.{DefaultCharacter, EscapedCharacter}
import parsley.Parsley
import parsley.expr.precedence
import parsley.implicits._

import scala.io.Source
import javax.swing.text.html.HTMLEditorKit.Parser
import java.beans.Expression

object Main2 {
  def main(args: Array[String]): Unit = {
    val filename = "/home/codrin/wacc_examples/valid/basic/skip/comment.wacc"
    for (line <- Source.fromFile(filename).getLines()) {
      println(line)
    }

    val toComment = (comment: String) 
      => new Comment("")
    val toArgumentList = (expression: Expression, expressions: List[Expression]) 
      => new ArgumentList(expression :: expressions)
    
    //dp1119
    lazy val programParser: Parsley[Program] = toProgram.lift("")
    lazy val functionParser: Parsley[Function] = toFunction.lift("")
    lazy val parameterListParser: Parsley[ParameterList] = toParameterList.lift("")
    lazy val parameterParser: Parsley[Parameter] = toParameter.lift("")
  
    //ss8119 - from stat in page one up until array type in page two
    lazy val statementParser = precedence[Statement](
      skipParser 
      <\> identifierDeclarationParser 
      <\> assignmentParser 
      <\> "read" *> expressionParser 
      <\> "free" *> expressionParser
      <\> "return" *> expressionParser
      <\> "exit" *> expressionParser
      <\> "print" *> expressionParser
      <\> "println" *> expressionParser
    )
    lazy val assignmentLeftParser = precedence[AssignmentLeft](
      identifierParser, 
      arrayElementParser,
      pairElementparser
    )
    lazy val assignmentRightParser = precedence[AssignmentRight]()

    // <\> *> 
    /* Things that don't have precedence */
    lazy val argumentListParser: Parsley[ArgumentList] 
      = toArgumentList.lift(expression, manyOf(',' *> expressions))
    
    //gjw19 - remaining types on page 2

    // Functions
    val toArrayElement = (identifier: Identifier, expressions: List[Expression]) => new ArrayElement(identifier, expressions)
    val toIdentifier = (char: Char, chars: List[Char]) => new Identifier((char :: chars).mkString(""))
    val arrayType = (arrayType: Type) => new ArrayType(arrayType)
    val toBinaryOperator = (operator: String) => operator match {
      case "*" =>
      case "" =>
      case "" =>
      case "" =>
      case "" =>
      case "" =>
      case "" =>
      case "" =>
      case "" =>
      case "" =>
    }





    //
    lazy val pairElementParser = precedence[PairElement]()
    lazy val typeParser = precedence[Type]()
    lazy val baseTypeParser = precedence[BaseType]()    
    lazy val pairElementTypeParser = precedence[PairElementType]()
    lazy val expressionParser = precedence[Expression]()
    /* Things that don't have precedence */

    lazy val unaryOperatorParser = toUnaryOperator.lift("!" <\> "-" <\> "len" <\> "ord" <\> "chr")
    lazy val binaryOperatorParser
      = toBinaryOperator.lift("*" <\> "/" <\> "%" <\> "+" <\> "-" <\> ">" <\> ">=" <\> "<" <\> "<=" <\> "==" <\> "!=" <\> "&&" <\> "||") 
      
    lazy val arrayTypeParser: Parsley[ArrayType]
      = toArrayType.lift(typeParser <* '[' <* ']')
    lazy val identifierParser: Parsley[Identifier]
       = toIdentifier.lift('_' <\> letter(), many('_' <\> alphaNum()))
    lazy val arrayElementParser: Parsley[ArrayElement]
      = toArrayElement.lift(identifierParser, manyN(1, "[" *> expressionParser <* "]"))
    
    /* Functions */
    val toComment = (chars: List[Char]) 
      => new Comment(chars.mkString)
    val toStringLiter = (chars: List[DefaultCharacter]) 
      => new StringLiter(chars.map(chr -> chr.char).mkString(""))
    val toPairLiter = (nullstr: String) 
      => new PairLiter()
    val toArrayLiter = (opt: Option[(Expression, List[Expression])]) 
      => opt match {
        case Some((e, es)) => new ArrayLiter(e :: es)
        case None          => new ArrayLiter(List())
      }
    val toEscapedCharacter = (char: Char) 
      => new EscapedCharacter(char)
    val toCharacterLiter = (char: DefaultCharacter) 
      => new CharacterLiter(char.char)
    val toBooleanLiter = (boolean: String) 
      => new BooleanLiter(Boolean.parseBoolean(boolean))
    val integerSignParser = (sign: Char) 
      => new IntegerSign(sign)
    val toDigit = (digit: Char) 
      => new Digit(digit)
    val toIntegerLiter = (sign: Option[IntegerSign], digits: List[Digit]) 
      => new IntegerLiter(sign, digits)
    // val toDefaultCharacter = (char: Char) => new DefaultCharacter(char, )

    // parsers
    lazy val integerLiterParser: Parsley[IntegerLiter] =
      toIntegerLiter.lift(option(integerSignParser), manyN(1, digitParser))
    lazy val digitParser: Parsley[Digit] =
      toDigit.lift('0' <\> '1' <\> '2' <\> '3' <\> '4' <\> '5' <\> '6' <\> '7' <\> '8' <\> '9')
    lazy val integerSignParser: Parsley[IntegerSign] =
      toIntegerSign.lift('+' <\> '-')
    lazy val booleanLiterParser: Parsley[BooleanLiter] =
      toBooleanLiter.lift("true" <\> "false") 
    lazy val characterLiterParser: Parsley[CharacterLiter] =
      toCharacterLiter.lift("\'" *> defaultCharacterParser <* "\'")
    lazy val defaultCharacterParser: Parsley[DefaultCharacter] =   // TODO
      toDefaultCharacter.lift(noneOf('\\', '\'', '\"') <\> '\\' *> escapedCharParser)
    lazy val escapedCharParser: Parsley[EscapedCharacter] =
      toEscapedCharacter.lift('0' <\> 'b' <\> 't' <\> 'n' <\> 'f' <\> 'r' <\> '\"' <\> '\'' <\> '\\')

    lazy val arrayLiterParser: Parsley[ArrayLiter] 
      = toArrayLiter.lift("[" *> option(expressionParser, many("," *> expressionParser)) <* "]")
    lazy val pairLiterParser: Parsley[PairLiter] 
      = toPairLiter.lift("null")
    lazy val stringLiterParser: Parsley[StringLiter] 
      = toStringLiter.lift("\"" *> many(defaultCharacterParser) <* "\"")
    
    lazy val commentParser: Parsley[Comment] = toComment.lift('#' *> many(noneOf('\n')))

    commentParser.runParser("")
  }

  def square(x: Int): Int = x * x
}


/*
a <~> b -> pair(a, b)
a, b    -> a concat b
a *> b  -> b
a #> b  -> pattern match on a; if successful, replace with b 
a <\> b -> a OR b
a <|> b -> try a until you fail; continue with b
*/