case object Day8 extends Day {
    def mergeLine[A](left: List[A], right: List[A], merge: Tuple2[A, A] => A): List[A] = left.zip(right).map(merge)
    def makeGrid[A](input: List[String], mergeFunc: Tuple2[A, A] => A, walkFunc: String => List[A]): List[List[A]] = {
        def parseLine(line: String) = mergeLine(walkFunc(line), walkFunc(line.reverse).reverse, mergeFunc)
        val hors = input.map(parseLine(_))
        val verts = input.transpose.map(_.mkString).map(parseLine(_)).transpose
        hors.zip(verts).map(p => mergeLine(p._1, p._2, mergeFunc))
    }

    val output1 = {
        def mergeFunc(p: Tuple2[Boolean, Boolean]): Boolean = p._1 || p._2
        def walkFunc(line: String): List[Boolean] = {
            def walk(ahead: List[Int], behind: List[Boolean] = List.empty, highest: Int = 0): List[Boolean] = ahead.match {
                case Nil => behind
                case head :: next => if (head > highest) walk(next, behind :+ true, head) else walk(next, behind :+ false, highest)
            }
            walk(line.toList.map(_.asDigit))
        }
        makeGrid(input, mergeFunc, walkFunc).map(_.count(identity)).sum.toString 
    }
    val output2 = {
        def mergeFunc(p: Tuple2[Int, Int]): Int = p._1 * p._2
        def walkFunc(line: String): List[Int] = {
            def walk(ahead: List[Int], behind: List[Int] = List.empty): List[Int] = ahead.match {
                case Nil => behind
                case head :: next => walk(next, behind :+ (next.takeWhile(_ < head).size + next.dropWhile(_ < head).take(1).size))
            }
            walk(line.toList.map(_.asDigit))
        }
        makeGrid(input, mergeFunc, walkFunc).flatten.max.toString
    }
}
