import org.scalatest.flatspec.AnyFlatSpec

class MainSpec extends AnyFlatSpec {
  "Square" should "square a given number" in {
    assert(Main.square(2) === 4)
    assert(Main.square(4) === 16)
    assert(Main.square(8) === 64)
  }
}

