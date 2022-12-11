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
    def dist(that: Coords): Int = { val diff = this - that; diff.x.abs.max(diff.y.abs) }
    def pull(that: Coords): Coords = {
        if(this.dist(that) < 2) that
        else (this-that) match { case c => Coords(c.x.sign, c.y.sign) + that}
    }
}

case object Day9 extends Day {
    def parseLine(line: String): List[Coords] = List().padTo(line.split(" ")(1).toInt, Coords.fromChar(line(0)))
    val moves: List[Coords] = input.flatMap(parseLine(_))
    def traceTail(input: List[Coords], history: List[List[Coords]]): List[List[Coords]] = input match {
        case Nil => history
        case in :: ins => 
            val jerkedkHead = history.last.head + in
            val newState: List[Coords] = history.last.tail.foldLeft(List(jerkedkHead))((prev, curr) => prev :+ prev.last.pull(curr)) 
            traceTail(ins, history :+ newState)
    }
    def simulateRope(segments: Int): List[List[Coords]] = traceTail(moves, List(List().padTo(segments, Coords())))
    val output1 = { // 6284 correct
        simulateRope(2).map(_.last).toSet.size.toString
    }
    val output2 = { //2953 too high
        simulateRope(10).map(_.last).toSet.size.toString
    }
}
