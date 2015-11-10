import org.scalatest._

import TheNextNumber._

class TheNextNumberTest extends FunSuite {

  test("sample #1") {
    assert(nextNumber("115")  === "151" )
  }
  
  test("sample #2") {
    assert(nextNumber("1051")  === "1105" )
  }
  
  test("sample #3") {
    assert(nextNumber("6233")  === "6323" )
  }
  
  test("sample case") {
    val input = "3\n115\n1051\n6233".lines

    val expected = "Case #1: 151\nCase #2: 1105\nCase #3: 6323".lines

    compareLines(input, expected)
  }

  def compareLines(input: Iterator[String], expected: Iterator[String]) {
    doIt(input) { s =>
      for (line <- s.lines) assert(line.trim === expected.next().trim)
    }
    assert(expected.hasNext === false, "Finished too fast.")
  }

}
