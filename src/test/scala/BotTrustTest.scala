import org.scalatest._

import BotTrust._

class BotTrustTest extends FunSuite {

  test("base #1") {
    assert(minTime(List()) === 0)
  }

  test("base #2") {
    assert(minTime(List(("O", 1))) === 1)
  }

  test("base #3") {
    assert(minTime(List(("O", 2))) === 2)
  }

  test("base #4") {
    assert(minTime(List(("O", 2), ("B", 1))) === 3)
  }

  test("base #5") {
    assert(minTime(List(("O", 2), ("B", 2))) === 3)
  }

  test("sample #1") {
    assert(minTime(List(("O", 2), ("B", 1), ("B", 2), ("O", 4))) === 6)
  }

  test("sample #2") {
    assert(minTime(List(("O", 5), ("O", 8), ("B", 100))) === 100)
  }

  test("sample #3") {
    assert(minTime(List(("B", 2), ("B", 1))) === 4)
  }
  
  test("sample case") {
    val input = "3\n4 O 2 B 1 B 2 O 4\n3 O 5 O 8 B 100\n2 B 2 B 1".lines

    val expected = "Case #1: 6\nCase #2: 100\nCase #3: 4".lines

    lineComparison(input, expected)
  }

  def lineComparison(input: Iterator[String], expected: Iterator[String]) {
    doIt(input) { s =>
      for (line <- s.lines) assert(line.trim === expected.next().trim)
    }
    assert(expected.hasNext === false, "Finished too fast.")
  }

}