import scala.io.Source

object Mnemonics{
  val in = Source.fromFile(System.getProperty("user.dir") + "Maps.sc")
  val words = in.getLines().toList

  val mnemonics = Map(2 -> "ABC", 3 -> "DEF", 4 -> "GHI", 5 -> "JKL", 6 -> "MNO", 7 -> "PQRS", 8 -> "TUV", 9 -> "WXYZ")
}
println(Mnemonics.in)

println(Mnemonics.words)