import com.wacc.assignment._
import com.wacc.binaryoperators._
import com.wacc.expressions._
import com.wacc.functions._
import com.wacc.miscellaneous._
import com.wacc.primitives._
import com.wacc.statements._
import com.wacc.types._
import com.wacc.unaryoperators._
import parsley.Parsley._
import parsley.character.{alphaNum, letter, noneOf}
import parsley.combinator.{manyN, option}
import parsley.expr.{InfixL, Ops, precedence}
import parsley.implicits._
import parsley.{Parsley, combinator}

object WACCParser {

  lazy val binaryFunctionGenerator
      : String => (Expression, Expression) => BinaryOperatorApplication =
    (operator: String) =>
      (expr1: Expression, expr2: Expression) =>
        new BinaryOperatorApplication(expr1, BinaryOperator(operator), expr2)

  lazy val integerLiterParser: Parsley[IntegerLiter] =
    (option(integerSignParser) <~> manyN(1, digitParser)).map {
      case (sign, digits) => new IntegerLiter(sign, digits)
    }

  lazy val digitParser: Parsley[Digit] =
    ('0' <\> '1' <\> '2' <\> '3' <\> '4' <\> '5' <\> '6' <\> '7' <\> '8' <\> '9')
      .map(new Digit(_))
  lazy val integerSignParser: Parsley[IntegerSign] =
    ('+' <\> '-').map(sign => new IntegerSign(sign))

  lazy val booleanLiterParser: Parsley[BooleanLiter] =
    ("true" <\> "false").map {
      case "true"  => new BooleanLiter(true)
      case "false" => new BooleanLiter(false)
    }

  lazy val characterLiterParser: Parsley[CharacterLiter] =
    ("\'" *> defaultCharacterParser <* "\'").map(character =>
      new CharacterLiter(character.char)
    )

  lazy val defaultCharacterParser: Parsley[DefaultCharacter] =
    (noneOf('\\', '\'', '\"')).map(
      new DefaultCharacter(_, false)
    ) <\> ('\\' *> escapedCharParser).map(escapedCharacter =>
      new DefaultCharacter(escapedCharacter.char, true)
    )

  lazy val escapedCharParser: Parsley[EscapedCharacter] =
    ('0' <\> 'b' <\> 't' <\> 'n' <\> 'f' <\> 'r' <\> '\"' <\> '\'' <\> '\\')
      .map(new EscapedCharacter(_))

  lazy val arrayLiterParser: Parsley[ArrayLiter] = ("[" *> option(
    expressionParser <~> combinator.many("," *> expressionParser)
  ) <* "]").map {
    case None => new ArrayLiter(List())
    case Some((expression: Expression, expressions: List[Expression])) =>
      new ArrayLiter(expression :: expressions)
  }

  lazy val pairLiterParser: Parsley[PairLiter] =
    precedence.apply("null").map(_ => new PairLiter())

  lazy val stringLiterParser: Parsley[StringLiter] =
    ("\"" *> combinator.many(defaultCharacterParser) <* "\"").map(characters =>
      new StringLiter(characters.mkString(""))
    )

  lazy val commentParser: Parsley[Comment] =
    ('#' *> combinator.many(noneOf('\n')))
      .map(comment => new Comment(comment.mkString("")))

  lazy val programParser: Parsley[Program] =
    (("begin" *> combinator.many(
      functionParser
    )) <~> (statementParser <* "end")).map {
      case (functions: List[Function], body: Statement) =>
        new Program(functions, body)
    }

  lazy val functionParser: Parsley[Function] = (
    typeParser <~>
      identifierParser <~>
      ('(' *> option(parameterListParser) <* ')') <~>
      ("is" *> statementParser <* "end")
  ).map { case (((functionType, functionName), optionParameters), body) =>
    new Function(functionType, functionName, optionParameters, body)
  }

  lazy val parameterListParser: Parsley[ParameterList] =
    (parameterParser <~> combinator.many(',' *> parameterParser)).map {
      case (parameter: Parameter, parameters: List[Parameter]) =>
        new ParameterList(parameter :: parameters)
    }

  lazy val parameterParser: Parsley[Parameter] =
    (typeParser <~> identifierParser).map {
      case (parameterType, parameterName) =>
        new Parameter(parameterType, parameterName)
    }

  lazy val skipStatementParser: Parsley[SkipStatement] =
    precedence.apply("skip").map(_ => new SkipStatement())

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
      <\> ("read" *> assignmentLeftParser).map(new Read(_))
      <\> ("free" *> expressionParser).map(new Free(_))
      <\> ("return" *> expressionParser).map(new Return(_))
      <\> ("exit" *> expressionParser).map(new Exit(_))
      <\> ("print" *> expressionParser).map(new Print(_))
      <\> ("println" *> expressionParser).map(new Println(_))
      <\> (("if" *> expressionParser) <~> ("then" *> statementParser) <~> ("else" *> statementParser <* "fi"))
        .map { case ((condition, statement1), statement2) =>
          new If(condition, statement1, statement2)
        }
      <\> (("while" *> expressionParser) <~> ("do" *> statementParser <* "done"))
        .map { case (condition, statement) =>
          new While(condition, statement)
        }
      <\> ("begin" *> statementParser <* "end").map(new BeginEnd(_)),
    Ops(InfixL)(";" #> (new StatementSequence(_, _)))
  )

  lazy val assignmentLeftParser: Parsley[AssignmentLeft] =
    identifierParser <\> arrayElementParser <\> pairElementParser

  lazy val assignmentRightParser: Parsley[AssignmentRight] =
    expressionParser <\> arrayLiterParser <\> newpairParser <\> pairElementParser <\> functionCallParser

  lazy val newpairParser: Parsley[NewPair] =
    (("newpair(" *> expressionParser) <~> (',' *> expressionParser <* ')'))
      .map { case (expr1: Expression, expr2: Expression) =>
        new NewPair(expr1, expr2)
      }

  lazy val functionCallParser: Parsley[FunctionCall] =
    (("call" *> identifierParser) <~> ('(' *> option(
      argumentListParser
    ) <* ')')).map {
      case (identifier: Identifier, optionArguments: Option[ArgumentList]) =>
        new FunctionCall(identifier, optionArguments)
    }

  lazy val argumentListParser: Parsley[ArgumentList] =
    (expressionParser <~> combinator.many(',' *> expressionParser)).map {
      case (expression: Expression, expressions: List[Expression]) =>
        new ArgumentList(expression :: expressions)
    }

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
    (unaryOperatorParser <~> expressionParser).map {
      case (operator, expression) =>
        new UnaryOperatorApplication(operator, expression)
    }

  lazy val pairElementParser: Parsley[PairElement] =
    (("fst" *> expressionParser) <~> ("snd" *> expressionParser)).map {
      case (expr1, expr2) => new PairElement(expr1, expr2)
    }
  lazy val typeParser: Parsley[Type] =
    baseTypeParser <\> arrayTypeParser <\> pairTypeParser

  lazy val baseTypeParser: Parsley[BaseType] =
    ("int" <\> "bool" <\> "char" <\> "string").map(BaseType(_))

  lazy val pair: Parsley[PairElementType] =
    precedence.apply("pair").map(_ => new Pair())

  lazy val pairElementTypeParser: Parsley[PairElementType] =
    baseTypeParser <\> arrayTypeParser <\> pair

  lazy val pairTypeParser: Parsley[PairType] =
    (("pair(" *> pairElementTypeParser) <~> ("," *> pairElementTypeParser <* ")"))
      .map { case (type1, type2) =>
        new PairType(type1, type2)
      }

  lazy val unaryOperatorParser: Parsley[UnaryOperator] =
    ("!" <\> "-" <\> "len" <\> "ord" <\> "chr").map(UnaryOperator(_))

  lazy val binaryOperatorParser: Parsley[BinaryOperator] =
    ("*" <\> "/" <\> "%" <\> "+" <\> "-" <\> ">"
      <\> ">=" <\> "<" <\> "<=" <\> "==" <\> "!=" <\> "&&" <\> "||")
      .map(BinaryOperator(_))

  lazy val arrayTypeParser: Parsley[ArrayType] =
    (typeParser <* '[' <* ']').map(new ArrayType(_))

  lazy val identifierParser: Parsley[Identifier] =
    (('_' <\> letter) <~> combinator.many('_' <\> alphaNum)).map {
      case (letter, letters) =>
        new Identifier((letter :: letters).mkString(""))
    }

  lazy val arrayElementParser: Parsley[ArrayElement] =
    (identifierParser <~> manyN(1, "[" *> expressionParser <* "]")).map {
      case (identifier, expressions) =>
        new ArrayElement(identifier, expressions)
    }

  def main(args: Array[String]): Unit = {
    // Testing the parser on some example inputs
    println("Hello World!")
    println(expressionParser.runParser("5+5")) // Should succeed
    println(expressionParser.runParser("152*55/22!=0")) // Should succeed
    println(integerLiterParser.runParser("5")) // Should succeed
    println(integerLiterParser.runParser("a")) // Should fail
  }
}
