import java.io.File
import scala.io.Source

//given a dictionary of words as a list, design a method translate such that
//translate(number) produces all phrases of a word that can serve as mnemonics for the phone number

object Mnemonics{
  //val in = Source.fromFile(new File("./data/text.txt"))
  val in = Source.fromURL("https://raw.githubusercontent.com/frankh/coursera-scala/master/forcomp/src/main/resources/forcomp/linuxwords.txt")
  //must be sure to split up sentences into single words
  val words = in.getLines().flatMap(_.split("\\W+")).toList filter (word => word forall (chr => chr.isLetter))

  val mnemonics = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  //invert the map structure to give a map from char to number
  val charCode: Map[Char, Char] = for{ (num, str) <- mnemonics; curr <- str } yield curr-> num

  //maps a word to the digit string it can represent -> Java = 5282
  def wordCode(word: String): String = {
    //(for{curr <- word.toUpperCase} yield charCode(curr)) mkString
    word.toUpperCase map charCode
  }

  //map from digit strings to words
  val wordsForNums: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue(Seq())

  //return all ways to encode a number as a list of words
  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(List())
    else{
      for{
        split <- 1 to number.length
        word <- wordsForNums(number take split)
        rest <- encode(number drop split)
      }yield word :: rest
    }.toSet
  }

  def main(args: Array[String]): Unit = {
    //println(System.getProperty("user.dir"))
    //words foreach(println)
    //println(charCode)
    //println("Java to wordCode is " + wordCode("Java"))
    //println(wordsForNums)
    println(encode("5282"))
    println(encode("7225247386"))
  }
}