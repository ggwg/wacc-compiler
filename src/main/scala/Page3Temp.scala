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
import Parsley._
import parsley.character.{alphaNum, letter, noneOf}
import parsley.combinator.{manyN, option}
import parsley.expr.{InfixL, Ops, precedence}
import parsley.implicits._

import scala.io.Source

object Main2 {
  def main(args: Array[String]): Unit = {

    //functions
    lazy val toArgumentList =
      (expression: Expression, expressions: List[Expression]) =>
        new ArgumentList(expression :: expressions)
    lazy val toNewpair = (expr1: Expression, expr2: Expression) =>
      new NewPair(expr1, expr2)
    lazy val toFunctionCall =
      (identifier: Identifier, arguments: Option[ArgumentList]) =>
        new FunctionCall(identifier, arguments)
    lazy val toProgram = (functions: List[Function], body: Statement) =>
      new Program(functions, body)
    lazy val toFunction = (
        functionType: Type,
        functionName: Identifier,
        parameters: Option[ParameterList],
        body: Statement
    ) => new Function(functionType, functionName, parameters, body)
    lazy val toParameterList = (param: Parameter, params: List[Parameter]) =>
      new ParameterList(param :: params)
    lazy val toParameter = (parameterType: Type, parameterName: Identifier) =>
      new Parameter(parameterType, parameterName)
    lazy val toComment = (chars: List[Char]) => new Comment(chars.mkString)
    lazy val toStringLiter = (chars: List[DefaultCharacter]) =>
      new StringLiter(chars.map(chr => chr.char).mkString(""))
    lazy val toPairLiter = (nullstr: String) => new PairLiter()
    lazy val toArrayLiter = (opt: Option[(Expression, List[Expression])]) =>
      opt match {
        case Some((e, es)) => new ArrayLiter(e :: es)
        case None          => new ArrayLiter(List())
      }
    lazy val toEscapedCharacter = (char: Char) => new EscapedCharacter(char)
    lazy val toCharacterLiter = (char: DefaultCharacter) =>
      new CharacterLiter(char.char)
    lazy val toBooleanLiter = (boolean: String) =>
      new BooleanLiter(boolean.equals("true"))
    lazy val toDigit = (digit: Char) => new Digit(digit)
    lazy val toIntegerLiter =
      (sign: Option[IntegerSign], digits: List[Digit]) =>
        new IntegerLiter(sign, digits)

    lazy val toArrayElement =
      (identifier: Identifier, expressions: List[Expression]) =>
        new ArrayElement(identifier, expressions)
    lazy val toIdentifier = (char: Char, chars: List[Char]) =>
      new Identifier((char :: chars).mkString(""))
    lazy val arrayType = (arrayType: Type) => new ArrayType(arrayType)
    lazy val toBinaryOperator = (operator: String) =>
      operator match {
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
    lazy val toUnaryOperator = (operator: String) =>
      operator match {
        case "!"   => new Not()
        case "-"   => new Negate()
        case "len" => new Length()
        case "ord" => new Ord()
        case "chr" => new Chr()
      }

    lazy val toPair = (pair: String) => new PairDefault()
    lazy val toPairType = (type1: PairElementType, type2: PairElementType) =>
      new PairType(type1, type2)
    lazy val toBaseType = (baseType: String) =>
      baseType match {
        case "int"    => new IntType()
        case "bool"   => new BooleanType()
        case "char"   => new CharacterType()
        case "string" => new StringType()
      }
    lazy val toUnaryOperatorApplication =
      (opType: UnaryOperator, expression: Expression) =>
        new UnaryOperatorApplication(opType, expression)

    lazy val binaryFunctionGenerator = (operator: String) =>
      operator match {
        case "*" => (
          (expr1: Expression, expr2: Expression) =>
            new BinaryOperatorApplication(expr1, new Multiply(), expr2)
        )
        case "/" => (
          (expr1: Expression, expr2: Expression) =>
            new BinaryOperatorApplication(expr1, new Divide(), expr2)
        )
        case "%" => (
          (expr1: Expression, expr2: Expression) =>
            new BinaryOperatorApplication(expr1, new Modulo(), expr2)
        )
        case "+" => (
          (expr1: Expression, expr2: Expression) =>
            new BinaryOperatorApplication(expr1, new Add(), expr2)
        )
        case "-" => (
          (expr1: Expression, expr2: Expression) =>
            new BinaryOperatorApplication(expr1, new Subtract(), expr2)
        )
        case ">" => (
          (expr1: Expression, expr2: Expression) =>
            new BinaryOperatorApplication(expr1, new GreaterThan(), expr2)
        )
        case ">=" => (
          (expr1: Expression, expr2: Expression) =>
            new BinaryOperatorApplication(expr1, new GreaterEqualThan(), expr2)
        )
        case "<" => (
          (expr1: Expression, expr2: Expression) =>
            new BinaryOperatorApplication(expr1, new SmallerThan(), expr2)
        )
        case "<=" => (
          (expr1: Expression, expr2: Expression) =>
            new BinaryOperatorApplication(expr1, new SmallerEqualThan(), expr2)
        )
        case "==" => (
          (expr1: Expression, expr2: Expression) =>
            new BinaryOperatorApplication(expr1, new Equals(), expr2)
        )
        case "!=" => (
          (expr1: Expression, expr2: Expression) =>
            new BinaryOperatorApplication(expr1, new NotEquals(), expr2)
        )
        case "&&" => (
          (expr1: Expression, expr2: Expression) =>
            new BinaryOperatorApplication(expr1, new And(), expr2)
        )
        case "||" => (
          (expr1: Expression, expr2: Expression) =>
            new BinaryOperatorApplication(expr1, new Or(), expr2)
        )
      }

    lazy val integerLiterParser: Parsley[IntegerLiter] =
      toIntegerLiter.lift(option(integerSignParser), manyN(1, digitParser))
    lazy val digitParser: Parsley[Digit] = toDigit.lift(
      '0' <\> '1' <\> '2' <\> '3' <\> '4' <\> '5' <\> '6' <\> '7'
        <\> '8' <\> '9'
    )
    lazy val integerSignParser: Parsley[IntegerSign] =
      ('+' <\> '-').map(sign => new IntegerSign(sign))
    lazy val booleanLiterParser: Parsley[BooleanLiter] =
      toBooleanLiter.lift("true" <\> "false")
    lazy val characterLiterParser: Parsley[CharacterLiter] =
      toCharacterLiter.lift("\'" *> defaultCharacterParser <* "\'")
    lazy val defaultCharacterParser: Parsley[DefaultCharacter] =
      (noneOf('\\', '\'', '\"')).map(chr =>
        new DefaultCharacter(chr, false)
      ) <\> ('\\' *> escapedCharParser).map(escapedCharacter =>
        new DefaultCharacter(escapedCharacter.char, true)
      )
    lazy val escapedCharParser: Parsley[EscapedCharacter] =
      toEscapedCharacter.lift(
        '0' <\> 'b' <\> 't' <\> 'n' <\> 'f' <\> 'r'
          <\> '\"' <\> '\'' <\> '\\'
      )
    lazy val arrayLiterParser: Parsley[ArrayLiter] = toArrayLiter.lift(
      "[" *> option(expressionParser <~> many("," *> expressionParser)) <* "]"
    )
    lazy val pairLiterParser: Parsley[PairLiter] = toPairLiter.lift("null")
    lazy val stringLiterParser: Parsley[StringLiter] =
      toStringLiter.lift("\"" *> many(defaultCharacterParser) <* "\"")
    lazy val commentParser: Parsley[Comment] =
      toComment.lift('#' *> many(noneOf('\n')))
    lazy val programParser: Parsley[Program] =
      toProgram.lift("begin" *> many(functionParser), statementParser <* "end")
    lazy val functionParser: Parsley[Function] = toFunction.lift(
      typeParser,
      identifierParser,
      '(' *> option(parameterListParser) <* ')',
      "is" *> statementParser <* "end"
    )
    lazy val parameterListParser: Parsley[ParameterList] =
      toParameterList.lift(parameterParser, many(',' *> parameterParser))
    lazy val parameterParser: Parsley[Parameter] =
      toParameter.lift(typeParser, identifierParser)

    lazy val toSkip = (str: String) => new SkipStatement()
    lazy val skipStatementParser: Parsley[SkipStatement] = toSkip.lift("skip")
    lazy val statementParser: Parsley[Statement] = precedence[Statement](
      skipStatementParser
        <\> (typeParser <~> identifierParser <~> ("=" *> assignmentRightParser))
          .map { case ((identifierType, identifierName), assignmentRight) =>
            new IdentifierDeclaration(
              identifierType,
              identifierName,
              assignmentRight
            )
          }
        <\> (assignmentLeftParser <~> ('=' *> assignmentRightParser)).map {
          case (assignmentLeft, assignmentRight) =>
            new Assignment(assignmentLeft, assignmentRight)
        }
        <\> ("read" *> assignmentLeftParser).map(assignmentLeft =>
          new Read(assignmentLeft)
        )
        <\> ("free" *> expressionParser).map(expression => new Free(expression))
        <\> ("return" *> expressionParser).map(expression =>
          new Return(expression)
        )
        <\> ("exit" *> expressionParser).map(expression => new Exit(expression))
        <\> ("print" *> expressionParser).map(expression =>
          new Print(expression)
        )
        <\> ("println" *> expressionParser).map(expression =>
          new Println(expression)
        )
        <\> (("if" *> expressionParser) <~> ("then" *> statementParser) <~> ("else" *> statementParser <* "fi"))
          .map { case ((condition, statement1), statement2) =>
            new If(condition, statement1, statement2)
          }
        <\> (("while" *> expressionParser) <~> ("do" *> statementParser <* "done"))
          .map { case (condition, statement) =>
            new While(condition, statement)
          }
        <\> ("begin" *> statementParser <* "end").map(statement =>
          new BeginEnd(statement)
        ),
      Ops(InfixL)(";" #> (new StatementSequence(_, _)))
    )
    lazy val assignmentLeftParser: Parsley[AssignmentLeft] =
      identifierParser <\> arrayElementParser <\> pairElementParser
    lazy val assignmentRightParser: Parsley[AssignmentRight] =
      expressionParser <\> arrayLiterParser <\> newpairParser <\> pairElementParser <\> functionCallParser

    lazy val newpairParser: Parsley[NewPair] = toNewpair.lift(
      "newpair(" *> expressionParser,
      ',' *> expressionParser <* ')'
    )
    lazy val functionCallParser: Parsley[FunctionCall] = toFunctionCall.lift(
      "call" *> identifierParser,
      '(' *> option(argumentListParser) <* ')'
    )
    // <\> *>
    /* Things that don't have precedence */
    lazy val argumentListParser: Parsley[ArgumentList] =
      toArgumentList.lift(expressionParser, many(',' *> expressionParser))

    // Functions
    /* TODO: Add comments for each */

    //

    lazy val expressionParser: Parsley[Expression] = precedence[Expression](
      integerLiterParser
        <\> booleanLiterParser
        <\> characterLiterParser
        <\> stringLiterParser
        <\> pairLiterParser
        <\> identifierParser
        <\> arrayElementParser
        <\> unaryOperatorApplicationParser
        <\> ("(" *> expressionParser <* ")"),
      Ops(InfixL)(
        "*" #> binaryFunctionGenerator("*"),
        "/" #> binaryFunctionGenerator("/"),
        "%" #> binaryFunctionGenerator("%")
      ),
      Ops(InfixL)(
        "+" #> binaryFunctionGenerator("+"),
        "-" #> binaryFunctionGenerator("-")
      ),
      Ops(InfixL)(
        ">" #> binaryFunctionGenerator(">"),
        ">=" #> binaryFunctionGenerator(">="),
        "<" #> binaryFunctionGenerator("<"),
        "<=" #> binaryFunctionGenerator("<="),
        "==" #> binaryFunctionGenerator("=="),
        "!=" #> binaryFunctionGenerator("!=")
      ),
      Ops(InfixL)(
        "&" #> binaryFunctionGenerator("+"),
        "-" #> binaryFunctionGenerator("-")
      )
    )

    /* Things that don't have precedence */
    lazy val unaryOperatorApplicationParser: Parsley[UnaryOperatorApplication] =
      toUnaryOperatorApplication.lift(unaryOperatorParser, expressionParser)

    lazy val pairElementParser: Parsley[PairElement] =
      (("fst" *> expressionParser) <~> ("snd" *> expressionParser)).map {
        case (expr1, expr2) => new PairElement(expr1, expr2)
      }
    lazy val typeParser: Parsley[Type] =
      baseTypeParser <\> arrayTypeParser <\> pairTypeParser
    lazy val baseTypeParser: Parsley[BaseType] =
      toBaseType.lift("int" <\> "bool" <\> "char" <\> "string")
    lazy val pair: Parsley[PairElementType] = toPair.lift("pair")
    lazy val pairElementTypeParser: Parsley[PairElementType] =
      baseTypeParser <\> arrayTypeParser <\> pair
    lazy val pairTypeParser: Parsley[PairType] = toPairType.lift(
      "pair(" *> pairElementTypeParser,
      "," *> pairElementTypeParser <* ")"
    )
    lazy val unaryOperatorParser: Parsley[UnaryOperator] =
      toUnaryOperator.lift("!" <\> "-" <\> "len" <\> "ord" <\> "chr")
    lazy val binaryOperatorParser: Parsley[BinaryOperator] =
      toBinaryOperator.lift(
        "*" <\> "/" <\> "%" <\> "+" <\> "-" <\> ">"
          <\> ">=" <\> "<" <\> "<=" <\> "==" <\> "!=" <\> "&&" <\> "||"
      )

    lazy val arrayTypeParser: Parsley[ArrayType] =
      (typeParser <* '[' <* ']').map(arrayType => new ArrayType(arrayType))
    lazy val identifierParser: Parsley[Identifier] =
      toIdentifier.lift('_' <\> letter, many('_' <\> alphaNum))
    lazy val arrayElementParser: Parsley[ArrayElement] = toArrayElement.lift(
      identifierParser,
      manyN(1, "[" *> expressionParser <* "]")
    )

    /* Functions */

    /* Parsers */

    commentParser.runParser("")

    // Testing the parser on some example inputs
    println("Hello World!")
    println(expressionParser.runParser("5+5"))             // Should succeed
    println(expressionParser.runParser("152*55/22!=0"))    // Should succeed
    println(integerLiterParser.runParser("5"))             // Should succeed
    println(integerLiterParser.runParser("a"))             // Should fail
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
