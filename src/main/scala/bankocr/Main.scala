package bankocr

import scala.util.parsing.combinator.RegexParsers

object Main extends App {

}

sealed trait Tokens
case object Newline extends Tokens

class Parser extends RegexParsers {

  import ListHelper._

  override def skipWhitespace = false

  val number: Parser[String] = "[\\s\\|\\_]{9}".r ^^ {
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

  val numbers: Parser[String] = repN(9, number) ^^ { _.mkString }

  def accountNumber: Parser[String] = digitCharacters

  val newLine: Parser[Tokens] = "\n".r ^^^ { Newline }

  val line: Parser[String] = "[\\s\\|\\_]{27}".r <~ (newLine ?) ^^ identity

  val chunkedLine: Parser[List[String]] = line ^^ { l =>
    chunkList(l.toList, 3).map(_.mkString)
  }

  val digitCharacters: Parser[String] = chunkedLine ~ chunkedLine ~ chunkedLine ^^ {
    case line1 ~ line2 ~ line3 =>
      val c1 = chunkList(line1, 3)
      val c2 = chunkList(line2, 3)
      val c3 = chunkList(line3, 3)

      c1.zip(c2.zip(c3)).map { case (l1, (l2, l3)) =>
        (l1 ++ l2 ++ l3).mkString
      }.mkString
  }
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
