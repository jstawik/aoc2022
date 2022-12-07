case object Day7 extends Day{
    class Dir(val parent: Option[Dir]){
        var children: Map[String, Dir] = Map.empty
        var files: Map[String, Int] = Map.empty
        def readLs(ls: List[String]): Unit = ls.map(_.split(" ") match {
            case line if line(0) == "dir" => children = children + (line(1) -> new Dir(Some(this)))
            case line => files = files + (line(1) -> line(0).toInt)
        })
        def size: Int = files.values.sum + children.values.map(_.size).sum
    }
       
    def parseLines(lines: List[String], currDir: Dir, rootDir: Dir): Dir = lines match {
        case head :: next => head match {
            case comm if comm.take(4) == "$ cd" => comm.drop(5) match {
                case l if l == ".." => parseLines(next, currDir.parent.getOrElse(rootDir), rootDir) 
                case l if l == "/" => parseLines(next, rootDir, rootDir)
                case name => parseLines(next, currDir.children(name), rootDir)
            }
            case ls =>
                currDir.readLs(next.takeWhile(_(0) != '$'))
                parseLines(next.dropWhile(_(0) != '$'), currDir, rootDir)   
        }
        case Nil => rootDir
    }

    val root = new Dir(None)
    parseLines(input, root, root)

    def sizes(root: Dir): List[Int] = root.size +: root.children.values.flatMap(sizes(_)).toList
    val output1 = sizes(root).filter(_ < 100000).sum.toString

    val output2 = {
        val total = 70000000
        val required = 30000000
        def taken(root: Dir): Int = root.files.values.sum + root.children.values.map(taken(_)).sum
        val needed = required + taken(root) - total
        sizes(root).filter(_ > needed).sortWith(_ < _)(0).toString
    }
}
