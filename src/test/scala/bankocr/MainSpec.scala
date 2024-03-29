package bankocr

import org.scalatest._

class MainSpec extends FreeSpec with Matchers {

  val parser = new Parser

  val numbersList = List(
    (Numbers.zero, "0"),
    (Numbers.one, "1"),
    (Numbers.two, "2"),
    (Numbers.three, "3"),
    (Numbers.four, "4"),
    (Numbers.five, "5"),
    (Numbers.six, "6"),
    (Numbers.seven, "7"),
    (Numbers.eight, "8"),
    (Numbers.nine, "9")
  )

  numbersList.foreach {
    case (input, result) =>
      s"parse $result" in {

        parser.parse(parser.number, input).get shouldBe result
      }
  }

  "parse a line of numbers" in {
    val lineOfNumbers = " _  _  _  _  _  _  _  _  _ \n" +
                        "| || || || || || || || || |\n" +
                        "|_||_||_||_||_||_||_||_||_|"

    parser.parse(parser.accountNumber, lineOfNumbers) match {
      case parser.Success(result, next) => result shouldBe "000000000"
      case parser.NoSuccess(r, e) => fail(s"$r : $e")
    }
  }

  "chunk lists into smaller lists" in {
    val list = List(1,2,3,4,5,6)
    ListHelper.chunkList(list, 3) shouldBe List(List(1,2,3), List(4,5,6))
  }
}
