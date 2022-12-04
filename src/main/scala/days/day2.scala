case object day2 extends Day{
  def parse(in: Char): Int = in match {
    case 'A' | 'X' => 1
    case 'B' | 'Y' => 2
    case 'C' | 'Z' => 3
  }
  def score(my: Int, other: Int): Int = my match {
    case _ if my == other => my + 3
    case _ if my + other == 4 => if(my>other) my else my+6
    case _ => if(other > my) my else my + 6 
  }
  def score1(line: String): Int = score(parse(line(2)), parse(line(0)))
  def score2(line: String): Int = {
    val other = parse(line(0))
    line(2) match {
      case 'X' => (other + 1) % 3 + 1
      case 'Y' => (other + 3)
      case 'Z' => (other % 3) + 7
    }
  }

  val output1 = input.map(score1).sum
  val output2 = input.map(score2).sum
}
