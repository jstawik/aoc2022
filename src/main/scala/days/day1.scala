case object day1 extends Day{
  def parse(remn: List[String], accI: Int = 0, accLI: List[Int] = List.empty[Int]): List[Int] = remn match {
    case Nil => accLI :+ accI 
    case head :: next => head.toIntOption match {
      case Some(n) => parse(next, accI+n.toInt, accLI)
      case None => parse(next, 0, accLI:+accI)
    }
  }
  val output1 = parse(input).max
  val output2 = parse(input).sortWith(_>_).take(3).sum
}