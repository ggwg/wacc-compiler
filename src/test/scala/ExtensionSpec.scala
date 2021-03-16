import Parser.statementParser
import com.wacc.For
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.{a, convertToAnyShouldWrapper}

class ExtensionSpec extends AnyFlatSpec {
  "The compiler" should "successfully compile a program using a for statement" in {
    statementParser
      .runParser("for (int i = 0, j = 0; i < n && j < n; i = i + 1, j = j + 1) do skip done")
      .get shouldBe a[For]
    statementParser
      .runParser("for (;;) do skip done")
      .get shouldBe a[For]
  }

}
