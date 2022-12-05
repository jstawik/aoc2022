case object day4 extends Day{
  def line2ranges(line: String): Array[Int] = {
    line.split(",").flatMap(_.split("-")).map(_.toInt)
  }
  def fits1(rs: Array[Int]): Boolean = (rs(1)-rs(0)).max(rs(3)-rs(2)) == rs.max - rs.min
  def fits2(rs: Array[Int]): Boolean = rs(1)-rs(0)+rs(3)-rs(2) >= rs.max - rs.min
  val output1 = input.map(line2ranges).count(fits1).toString
  val output2 = input.map(line2ranges).count(fits2).toString
}