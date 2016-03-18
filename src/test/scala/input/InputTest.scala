package iguana.util.input

import iguana.utils.input.Input
import org.scalatest.FunSuite

class InputTest extends FunSuite {

  test("lineColumnNumber1") {
    val input = Input("big\n brother")
    assertResult(1)(input.lineNumber(0))
    assertResult(1)(input.columnNumber(0))

    assertResult(1)(input.lineNumber(1))
    assertResult(2)(input.columnNumber(1))

    assertResult(1)(input.lineNumber(3))
    assertResult(4)(input.columnNumber(3))

    assertResult(2)(input.lineNumber(5))
    assertResult(2)(input.columnNumber(5))
  }

  test("lineColumnNumber2") {
    val input = Input("big\r\n brother")

    assertResult(1)(input.lineNumber(0))
    assertResult(1)(input.columnNumber(0))

    assertResult(1)(input.lineNumber(1))
    assertResult(2)(input.columnNumber(1))

    assertResult(1)(input.lineNumber(3))
    assertResult(4)(input.columnNumber(3))

    assertResult(2)(input.lineNumber(6))
    assertResult(2)(input.columnNumber(6))
  }

  test("insert1") {
    val input1 = Input("We are just in the wall")
    val input2 = Input(" another brick")
    val result = input1.insert(11, input2)
    assertResult(Input("We are just another brick in the wall"))(result)
  }

  test("insert2") {
    val input1 = Input(" another brick in the wall")
    val input2 = Input("We are just")
    val result = input1.insert(0, input2)
    assertResult(Input("We are just another brick in the wall"))(result)
  }

  test("insert3") {
    val input1 = Input("We are just another")
    val input2 = Input(" brick in the wall")
    val result = input1.insert(input1.length - 1, input2)
    assertResult(Input("We are just another brick in the wall"))(result)
  }

}
