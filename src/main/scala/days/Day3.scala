case object Day3 extends Day{
  def charToScore(a: Char): Int = (a.toInt - 38) % 58 
  def parseLine(line: String): Int = 
    val half = line.size/2
    charToScore((line.take(half).toSet intersect line.drop(half).toSet).head)
  def parseLines(lines: List[String]): Int = charToScore(lines.map(_.toSet).reduce(_ intersect _).head)
  val output1 = input.map(parseLine(_)).sum.toString
  val output2 = input.grouped(3).map(parseLines).sum.toString
}