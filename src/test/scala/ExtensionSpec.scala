import Parser._
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.sys.process._

class ExtensionSpec extends AnyFlatSpec {
  def compileAndMatch(path: String, expected: String, libraries: List[String] = List.empty): Boolean = {

    /* Assemble the file */
    Compiler.main((path :: libraries).toArray)

    /* Get the assembled file name */
    val fileName = path.split('.')(0).split('/').last + ".s"

    val execName = "exec"

    /* Create and run the linking process */
    val linkIO = new ProcessIO(_ => {}, _ => {}, _ => {})
    val linkProcess = s"arm-linux-gnueabi-gcc -o $execName -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $fileName".run(linkIO)
    while (linkProcess.isAlive()) {
      Thread.sleep(100)
    }

    /* Create and run the running process */
    var processOutput = ""
    val runIO = new ProcessIO(
      stdin => {},
      stdout => {
        processOutput = scala.io.Source.fromInputStream(stdout).mkString
        stdout.close()
      },
      stderr => { stderr.close() }
    )
    val runProcess = "qemu-arm-static -L /usr/arm-linux-gnueabi exec".run(runIO)
    while (runProcess.isAlive()) {
      Thread.sleep(100)
    }

    /* Check the outputs match */
    assertResult(expected)(processOutput)

    /* Remove the generated file */
    new File(fileName).delete()
    new File(execName).delete()

    true
  }

  "The extended compiler" should "compile integer bitwise operations" in {
    compileAndMatch("src/main/resources/extension_examples/bitwiseOperators/and_or.wacc", "8\n14\n")
    compileAndMatch("src/main/resources/extension_examples/bitwiseOperators/shifts.wacc", "36\n2\n")
  }
  it should "compile for loops" in {
    compileAndMatch("src/main/resources/extension_examples/for/nestedForLoops.wacc", "362880")
    compileAndMatch("src/main/resources/extension_examples/for/printAllDigits.wacc", "0123456789")
    compileAndMatch("src/main/resources/extension_examples/for/sumOfArray.wacc", "45")
  }
  it should "compile break and continueloop statements" in {
    compileAndMatch("src/main/resources/extension_examples/loopControl/breakFor.wacc", "012345")
    compileAndMatch("src/main/resources/extension_examples/loopControl/breakWhile.wacc", "012345")
    compileAndMatch("src/main/resources/extension_examples/loopControl/continueFor.wacc", "012345")
    compileAndMatch("src/main/resources/extension_examples/loopControl/continueWhile.wacc", "12345")
    compileAndMatch("src/main/resources/extension_examples/loopControl/continueAndBreak.wacc", "4567")
  }
  it should "compile numbers in different bases" in {
    compileAndMatch("src/main/resources/extension_examples/numberBases/binaryNumbers.wacc", "12")
    compileAndMatch("src/main/resources/extension_examples/numberBases/octalNumbers.wacc", "21")
    compileAndMatch("src/main/resources/extension_examples/numberBases/hexNumbers.wacc", "255")
    compileAndMatch("src/main/resources/extension_examples/numberBases/mixedBaseOperations.wacc", "true")
  }
  it should "compile void functions and standalone function calls" in {
    compileAndMatch("src/main/resources/extension_examples/void/functionCallStatement.wacc", "true\ntrue\n")
    compileAndMatch("src/main/resources/extension_examples/void/voidFunction.wacc", "Hello, World!\n")
  }
  it should "compile overloaded functions" in {
    compileAndMatch("src/main/resources/extension_examples/functionOverloading/differentArgumentNumber.wacc", "true")
    compileAndMatch("src/main/resources/extension_examples/functionOverloading/differentArgumentType.wacc", "true")
    compileAndMatch(
      "src/main/resources/extension_examples/functionOverloading/differentArgumentTypeAndNumber.wacc",
      "true"
    )
    compileAndMatch(
      "src/main/resources/extension_examples/functionOverloading/manyOverloaded.wacc",
      "10\n30\ntrue\na\n"
    )
  }
  it should "compile higher order functions" in {
    compileAndMatch("src/main/resources/extension_examples/higherOrderFunctions/simpleMap.wacc", "2\n3\n4\n5\n")
    compileAndMatch("src/main/resources/extension_examples/higherOrderFunctions/functionList.wacc", "11\n10\n9\n")
  }
  it should "compile try catch blocks" in {
    compileAndMatch("src/main/resources/extension_examples/exception/simpleException.wacc", "Exception caught\n")
    compileAndMatch(
      "src/main/resources/extension_examples/exception/nestedException1.wacc",
      "First catch block\nSecond catch block\n"
    )
    compileAndMatch(
      "src/main/resources/extension_examples/exception/nestedException2.wacc",
      "Inner catch block\nOuter catch block\n"
    )
  }
  it should "compile linked external libraries" in {
    compileAndMatch(
      "src/main/resources/extension_examples/linking/linking.wacc",
      "[1, 2, 3, 4]\n[2, 4, 6, 8]\n20\n",
      libraries = List(
        "src/main/resources/extension_examples/linking/hof_lib.wacc",
        "src/main/resources/extension_examples/linking/io_lib.wacc"
      )
    )
  }
  it should "compile an advanced program using all extensions" in {
    compileAndMatch(
      "src/main/resources/extension_examples/advanced/zipWith.wacc",
      "i exceeded array bounds\n[A, B, C, d]\n",
      libraries = List("src/main/resources/extension_examples/linking/io_lib.wacc")
    )
  }
  it should "optimise unreachable statements" in {
    assertResult(statementParser.runParser("skip").get)(
      statementParser
        .runParser(
          "if true then" +
            "       while false do" +
            "             println 100" +
            "       done" +
            "     else" +
            "       print 1000 fi"
        )
        .get
        .removeUnreachableStatements()
    )
  }
}
