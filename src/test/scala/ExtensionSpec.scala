import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future, blocking}
import scala.sys.process._

class ExtensionSpec extends AnyFlatSpec {
  def compileAndMatch(path: String, expected: String): Boolean = {

    /* Assemble the file */
    Compiler.main(Array(path))

    /* Get the assembled file name */
    val fileName = path.split('.')(0).split('/').last + ".s"

    /* Create and run the linking process */
    val linkIO = new ProcessIO(_ => {}, _ => {}, _ => {})
    val linkProcess = s"arm-linux-gnueabi-gcc -o exec -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $fileName".run(linkIO)
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
    val runProcess = "qemu-arm -L /usr/arm-linux-gnueabi exec".run(runIO)
    while (runProcess.isAlive()) {
      Thread.sleep(100)
    }

    /* Check the outputs match */
    assert(processOutput.equals(expected))

    /* Remove the generated file */
    new File(fileName).delete()
    true
  }

  "The extended compiler" should "compile integer bitwise operations" in {
    compileAndMatch("src/main/resources/extension_examples/bitwiseOperators/and_or.wacc", "8\n14\n")
  }
}
