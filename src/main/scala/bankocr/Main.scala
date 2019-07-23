package bankocr

import scala.util.parsing.combinator.RegexParsers

object Main extends App {

}

class Parser extends RegexParsers {

  override def skipWhitespace = false

  val numbers: Parser[String] = "[\\s\\|\\_]{9}".r ^^ {
    case Numbers.zero => "0"
    case Numbers.one => "1"
    case Numbers.two => "2"
    case Numbers.three => "3"
    case Numbers.four => "4"
    case Numbers.five => "5"
    case Numbers.six => "6"
    case Numbers.seven => "7"
    case Numbers.eight => "8"
    case Numbers.nine => "9"
  }

  def multipleNumbers: Parser[String] = ???

}

object ListHelper {

  def chunkList[A](list: List[A], sizeOfChunks:Int):List[List[A]] = {

    list.zipWithIndex.foldLeft(List.empty[List[A]]){
      case (b, (a, num)) => if(num % sizeOfChunks == 0){
        List(a):: b
      } else {
        (a :: b.head) :: b.tail
      }
    } map {
      _.reverse
    } reverse
  }

}

object Numbers {

  val zero =
    " _ " +
      "| |" +
      "|_|"

  val one =
    "   " +
      "  |" +
      "  |"

  val two =
    " _ " +
      " _|" +
      "|_ "

  val three =
    " _ " +
      " _|" +
      " _|"

  val four =
    "   " +
      "|_|" +
      "  |"

  val five =
    " _ " +
      "|_ " +
      " _|"

  val six =
    " _ " +
      "|_ " +
      "|_|"

  val seven =
    " _ " +
      "  |" +
      "  |"

  val eight =
    " _ " +
      "|_|" +
      "|_|"

  val nine =
    " _ " +
      "|_|" +
      " _|"


}
