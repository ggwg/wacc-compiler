import com.wacc.assignment._
import com.wacc.binaryoperators._
import com.wacc.expressions._
import com.wacc.functions._
import com.wacc.miscellaneous._
import com.wacc.primitives._
import com.wacc.statements._
import com.wacc.types._
import com.wacc.unaryoperators._
import parsley.Parsley
import parsley.character.{alphaNum, letter, noneOf}
import parsley.combinator.{many, manyN, option}
import parsley.expr.{InfixL, Ops, precedence}
import parsley.implicits._

import scala.io.Source

object Main2 {
  def main(args: Array[String]): Unit = {
    val filename = "/home/codrin/wacc_examples/valid/basic/skip/comment.wacc"
    for (line <- Source.fromFile(filename).getLines()) {
      println(line)
    }

    //functions

    val toArgumentList = (expression: Expression, expressions: List[Expression]) 
      => new ArgumentList(expression :: expressions)
    val toNewpair = (expr1: Expression, expr2: Expression) 
      => new NewPair(expr1, expr2)
    val toFunctionCall = (identifier: Identifier, arguments: Option[ArgumentList]) 
      => new FunctionCall(identifier, arguments)
    val toProgram = (functions: List[Function], body: Statement) 
      => new Program(functions, body)
    val toFunctionCall = (functionType: Type, functionName: Identifier, parameters: Option[ParameterList], body: Statement) 
      => new FunctionCall(functionType, functionName, parameters, body)
    val toParameterList = (param: Parameter, params: List[Parameter])
      => new ParameterList(param :: params)
    val toParameter = (parameterType: Type, parameterName: Identifier) 
      => new Parameter(parameterType, parameterName)
    
    ///////
    lazy val programParser: Parsley[Program]       
      = toProgram.lift("begin" *> many(functionParser), statementParser <* "end")
    lazy val functionParser: Parsley[Function]
      = toFunctionCall(typeParser, identifierParser, '(' *> option(parameterListParser) <* ')', 
                         "is" *> statementParser <* "end")
    lazy val parameterListParser: Parsley[ParameterList] 
      = toParameterList.lift(parameterParser, many(',' *> parameterParser))
    lazy val parameterParser: Parsley[Parameter] 
      = toParameter.lift(typeParser, identifierParser)
  
    lazy val statementParser = precedence[Statement](   
      "skip".map(_ => new Skip()) 
      <\> (typeParser <~> identifierParser <~> ("=" *> assignmentRightParser)).map(
        (identifierType, identifierName, assignmentRight) => new IdentifierDeclaration(identifierType, identifierName, assignmentRight) 
      )
      <\> (assignmentLeftParser <~> ('=' *> assignmentRightParser)).map(
        (assignmentLeft, assignmentRight) => new Assignment(assignmentLeft, assignmentRight)
      )
      <\> ("read"    *> expressionParser).map(expression => new Read(expression))
      <\> ("free"    *> expressionParser).map(expression => new Free(expression))
      <\> ("return"  *> expressionParser).map(expression => new Return(expression))
      <\> ("exit"    *> expressionParser).map(expression => new Exit(expression))
      <\> ("print"   *> expressionParser).map(expression => new Print(expression))
      <\> ("println" *> expressionParser).map(expression => new Println(expression))
      <\> (("if" *> expressionParser) <~> ("then" *> statementParser) <~> ("else" *> statementParser <* "fi")).map(
        (condition, statement1, statement2) => new If(condition, statement1, statement2)
      )
      <\> (("while" *> expressionParser) <~> ("do" *> statementParser <* "done")).map(
        (condition, statement) => new While(condition, statement)
      )
      <\> ("begin" *> expressionParser <* "end").map(expression => new BeginEnd(expression)),
      Ops(InfixL)(";" #> ((_, _) => new StatementSequence(_, _)))
      ) 
    lazy val assignmentLeftParser: Parsley[AssignmentLeft] 
      = identifierParser 
      <\> arrayElementParser 
      <\> pairElementparser
    lazy val assignmentRightParser: Parsley[AssignmentRight] 
      = expressionParser
      <\> arrayLiterParser
      <\> newpairParser
      <\> pairElementParser
      <\> functionCallParser 

    lazy val newpairParser: Parsley[NewPair] 
      = toNewpair.lift("newpair(" *> expressionParser, ',' *> expressionParser <* ')')
    lazy val functionCallParser: Parsely[FunctionCall] 
      = toFunctionCall.lift("call" *> identifierParser, '(' *> option(argumentListParser) <* ')')
    // <\> *> 
    /* Things that don't have precedence */
    lazy val argumentListParser: Parsley[ArgumentList] 
      = toArgumentList.lift(expressionParser, many(',' *> expressionParser))
    

    // Functions
    /* TODO: Add comments for each */
    val toArrayElement = (identifier: Identifier, expressions: List[Expression]) 
      => new ArrayElement(identifier, expressions)
    val toIdentifier = (char: Char, chars: List[Char]) 
      => new Identifier((char :: chars).mkString(""))
    val arrayType = (arrayType: Type) 
      => new ArrayType(arrayType)
    val toBinaryOperator = (operator: String) 
      => operator match {
        case "*"  => new Multiply()
        case "/"  => new Divide()
        case "%"  => new Modulo()
        case "+"  => new Add()
        case "-"  => new Subtract()
        case ">"  => new GreaterThan()
        case ">=" => new GreaterEqualThan()
        case "<"  => new SmallerThan()
        case "<=" => new SmallerEqualThan()
        case "==" => new Equals()
        case "!=" => new NotEquals()
        case "&&" => new And()
        case "||" => new Or()
      }
    val toUnaryOperator = (operator: String) 
      => operator match {
        case "!"   => new Not()
        case "-"   => new Negate()
        case "len" => new Length()
        case "ord" => new Ord()
        case "chr" => new Chr()
      }

    val toPair = (pair: String) 
      => new PairDefault()
    val toPairType = (type1: PairElementType, type2: PairElementType) 
      => new PairType(type1, type2) 
    val toBaseType = (baseType: String)
      => baseType match {
        case "int"    => new IntType() 
        case "bool"   => new BooleanType()
        case "char"   => new CharacterType()
        case "string" => new StringType()
      }
    val toUnaryOperatorApplication = (opType: UnaryOperator, expression: Expression) 
      => new UnaryOperatorApplication(opType, expression)
    
    val binaryFunctionGenerator = (operator: String)
      => operator match {
        case "*" => ((expr1: Expression, expr2: Expression) 
                      => new BinaryOperatorApplication(expr1, new Multiply(), expr2))
        case "/" => ((expr1: Expression, expr2: Expression) 
                      => new BinaryOperatorApplication(expr1, new Divide(), expr2))
        case "%" => ((expr1: Expression, expr2: Expression) 
                      => new BinaryOperatorApplication(expr1, new Modulo(), expr2))
        case "+" => ((expr1: Expression, expr2: Expression) 
                      => new BinaryOperatorApplication(expr1, new Add(), expr2))
        case "-" => ((expr1: Expression, expr2: Expression) 
                      => new BinaryOperatorApplication(expr1, new Subtract(), expr2))
        case ">" => ((expr1: Expression, expr2: Expression) 
                      => new BinaryOperatorApplication(expr1, new GreaterThan(), expr2))
        case ">=" => ((expr1: Expression, expr2: Expression) 
                      => new BinaryOperatorApplication(expr1, new GreaterEqualThan(), expr2))
        case "<" => ((expr1: Expression, expr2: Expression) 
                      => new BinaryOperatorApplication(expr1, new SmallerThan(), expr2))
        case "<=" => ((expr1: Expression, expr2: Expression) 
                      => new BinaryOperatorApplication(expr1, new SmallerEqualThan(), expr2))
        case "==" => ((expr1: Expression, expr2: Expression) 
                      => new BinaryOperatorApplication(expr1, new Equals(), expr2))
        case "!=" => ((expr1: Expression, expr2: Expression) 
                      => new BinaryOperatorApplication(expr1, new NotEquals(), expr2))
        case "&&" => ((expr1: Expression, expr2: Expression) 
                      => new BinaryOperatorApplication(expr1, new And(), expr2))
        case "||" => ((expr1: Expression, expr2: Expression) 
                      => new BinaryOperatorApplication(expr1, new Or(), expr2))
      }

    //
    
    lazy val expressionParser  = precedence[Expression](
      integerLiterParser 
      <\> booleanLiterParser
      <\> characterLiterParser
      <\> stringLiterParser
      <\> pairLiterParser
      <\> identifierParser
      <\> arrayElementParser
      <\> unaryOperatorApplicationParser
      <\> ('(' *> expressionParser <* ')'),
      Ops(InfixL)("*" #> binaryFunctionGenerator("*"),
        "/" #> binaryFunctionGenerator("/"),
        "%" #> binaryFunctionGenerator("%")),
      Ops(InfixL)("+" #> binaryFunctionGenerator("+"),
        "-" #> binaryFunctionGenerator("-"))
      Ops(InfixL)(">" #> binaryFunctionGenerator(">"),
        ">=" #> binaryFunctionGenerator(">="),
        "<" #> binaryFunctionGenerator("<"),
        "<=" #> binaryFunctionGenerator("<="),
        "==" #> binaryFunctionGenerator("=="),
        "!=" #> binaryFunctionGenerator("!="))
      Ops(InfixL)("&" #> binaryFunctionGenerator("+"),
        "-" #> binaryFunctionGenerator("-"))  
    )

    /* Things that don't have precedence */
    lazy val unaryOperatorApplicationParser: Parsley[UnaryOperatorApplication]
      = toUnaryOperatorApplication.lift(unaryOperatorParser, expressionParser)
    
    lazy val pairElementParser: Parsley[PairElement]
      = toPairElement.lift("fst", expressionParser <\> "snd", expressionParser)
    lazy val typeParser: Parsley[Type] 
      = baseTypeParser <\> arrayTypeParser <\> pairTypeParser
    lazy val baseTypeParser: Parsley[BaseType]
      = toBaseType.lift("int" <\> "bool" <\> "char" <\> "string")  
    lazy val pair: Parsley[PairElementType] 
      = toPair.lift("pair")
    lazy val pairElementTypeParser: Parsley[PairElementType] 
      = baseTypeParser <\> arrayTypeParser <\> pair
    lazy val pairTypeParser: Parsley[PairType] 
      = toPairType.lift("pair", "(" *> pairElementTypeParser <*, 
          "," *> pairElementTypeParser <* ")")
    lazy val unaryOperatorParser: Parsley[UnaryOperator] 
      = toUnaryOperator.lift("!" <\> "-" <\> "len" <\> "ord" <\> "chr")
    lazy val binaryOperatorParser: Parsley[BinaryOperator]
      = toBinaryOperator.lift("*" <\> "/" <\> "%" <\> "+" <\> "-" <\> ">" 
          <\> ">=" <\> "<" <\> "<=" <\> "==" <\> "!=" <\> "&&" <\> "||") 
      
    lazy val arrayTypeParser: Parsley[ArrayType]
      = toArrayType.lift(typeParser <* '[' <* ']')
    lazy val identifierParser: Parsley[Identifier]
      = toIdentifier.lift('_' <\> letter, many('_' <\> alphaNum)
    lazy val arrayElementParser: Parsley[ArrayElement]
      = toArrayElement.lift(identifierParser, manyN(1, "[" *> expressionParser <* "]"))
    
    /* Functions */
    val toComment = (chars: List[Char]) 
      => new Comment(chars.mkString)
    val toStringLiter = (chars: List[DefaultCharacter]) 
      => new StringLiter(chars.map(chr => chr.char).mkString(""))
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
      => new BooleanLiter(boolean.equals("true"))
    val integerSignParser = (sign: Char) 
      => new IntegerSign(sign)
    val toDigit = (digit: Char) 
      => new Digit(digit)
    val toIntegerLiter = (sign: Option[IntegerSign], digits: List[Digit]) 
      => new IntegerLiter(sign, digits)

    /* Parsers */
    lazy val integerLiterParser: Parsley[IntegerLiter]
      = toIntegerLiter.lift(option(integerSignParser), manyN(1, digitParser))
    lazy val digitParser: Parsley[Digit]
      = toDigit.lift('0' <\> '1' <\> '2' <\> '3' <\> '4' <\> '5' <\> '6' <\> '7'
          <\> '8' <\> '9')
    lazy val integerSignParser: Parsley[IntegerSign]
      = toIntegerSign.lift('+' <\> '-')
    lazy val booleanLiterParser: Parsley[BooleanLiter]
      = toBooleanLiter.lift("true" <\> "false")
    lazy val characterLiterParser: Parsley[CharacterLiter]
      = toCharacterLiter.lift("\'" *> defaultCharacterParser <* "\'")
    lazy val defaultCharacterParser: Parsley[DefaultCharacter]
      = noneOf('\\', '\'', '\"').map(chr => new DefaultCharacter(chr, false))
        <\> ('\\' *> escapedCharParser).map(chr => new DefaultCharacter(chr, true))
    lazy val escapedCharParser: Parsley[EscapedCharacter]
      = toEscapedCharacter.lift('0' <\> 'b' <\> 't' <\> 'n' <\> 'f' <\> 'r' 
          <\> '\"' <\> '\'' <\> '\\')
    lazy val arrayLiterParser: Parsley[ArrayLiter] 
      = toArrayLiter.lift("[" *> option(expressionParser, 
          many("," *> expressionParser)) <* "]")
    lazy val pairLiterParser: Parsley[PairLiter] 
      = toPairLiter.lift("null")
    lazy val stringLiterParser: Parsley[StringLiter] 
      = toStringLiter.lift("\"" *> many(defaultCharacterParser) <* "\"")
    lazy val commentParser: Parsley[Comment] 
      = toComment.lift('#' *> many(noneOf('\n')))

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