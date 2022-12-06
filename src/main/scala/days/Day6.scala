case object Day6 extends Day{
    def seek(in: String, acc: String, markerSize: Int): Int = {
        if(acc.takeRight(markerSize).toSet.size == markerSize) acc.size
        else seek(in.tail, acc :+ in.head, markerSize)
    }
    val output1 = seek(input(0), "", 4).toString
    val output2 = seek(input(0), "", 14).toString
}