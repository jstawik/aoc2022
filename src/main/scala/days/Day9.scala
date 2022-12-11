object Coords {
    def fromChar(c: Char): Coords = c match {
        case 'R' => Coords(1, 0)
        case 'L' => Coords(-1, 0)
        case 'U' => Coords(0, 1)
        case 'D' => Coords(0, -1)
    }
    def apply(): Coords = Coords(0, 0)
}
case class Coords(x: Int, y: Int){
    def +(that: Coords): Coords = Coords(this.x + that.x, this.y + that.y)
    def -(that: Coords): Coords = Coords(this.x - that.x, this.y - that.y)
}
case class Tail(position: Coords, offset: Coords){
    def ->(pull: Coords): Tail = { 
        val sum = offset + pull
        if (sum.x.abs > 1 || sum.y.abs > 1) Tail (position + offset, pull)
        else this.copy(offset = sum)
    }
}
case object Day9 extends Day {
    def parseLine(line: String): List[Coords] = List().padTo(line.split(" ")(1).toInt, Coords.fromChar(line(0)))
    val moves: List[Coords] = input.flatMap(parseLine(_))
    val output1 = {
        def traceTail(input: List[Coords], history: List[Tail]): List[Tail] = input match {
            case Nil => history
            case head :: next => traceTail(next, history :+ history.last -> head)
        }
        traceTail(moves, List(Tail(Coords(), Coords()))).map(_.position).toSet.size.toString
    }
    val output2 = {
        ""
    }
}
