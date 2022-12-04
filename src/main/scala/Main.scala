@main def run: Unit = {
  print("Solve for day: ")
  val day = scala.io.StdIn.readInt()
  //println(Day.lookup(day).output)
  println(Day.fromInt(day).output)

}

object Day{
  def fromInt(value: Int): Day = 
    Vector(
        day1
      , day2
      , day3
      , day4
      , day5
    ).find(_.toString == "day"+value.toString).get //TODO: Circe?
}
trait Day{
  def filename = "src/resources/"+this.getClass.toString.drop(9).init
  lazy val input = io.Source.fromFile(filename).getLines().toList
  val output1: Int
  val output2: Int
  def output = output1.toString + " " + output2.toString
}
