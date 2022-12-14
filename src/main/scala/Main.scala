@main def run: Unit = {
  print("Solve for day: ")
  val day = scala.io.StdIn.readInt()
  println(Day.fromInt(day).output)

}

object Day{
  def fromInt(value: Int): Day = 
    Vector(
        Day1
      , Day2
      , Day3
      , Day4
      , Day5
      , Day6
      , Day7
      , Day8
      , Day9
    ).find(_.toString == "Day"+value.toString).get //TODO: Circe?
}
trait Day{
  def filename = "src/resources/"+this.getClass.toString.drop(9).init
  lazy val input = io.Source.fromFile(filename).getLines().toList
  val output1: String
  val output2: String
  def output = output1 + " " + output2
}
