import collection.mutable.Stack

extension (state: List[Stack[Char]]){
  def commitInstruction9000(i: Instruction): Unit = 
    i.repeat {state(i.to).push(state(i.from).pop)}
  def commitInstruction9001(i: Instruction): Unit = {
    val craneState = Stack.empty[Char]
    i.repeat {craneState.push(state(i.from).pop)}
    i.repeat {state(i.to).push(craneState.pop)}
  }
}

object Instruction{
  def fromString(s: String): Instruction = 
    s match {case s"move ${c} from ${f} to ${t}" => Instruction(c.toInt,f.toInt-1,t.toInt-1)} 
}
case class Instruction(count: Int, from: Int, to: Int){
  def repeat(action: => Unit): Unit = for _ <- 0 until count do action
}

case object day5 extends Day{
  val initState = day5.input
    .takeWhile(_ != "")
    .map(_.toCharArray)
    .transpose
    .filter(_.exists(_.isDigit))
    .map(_.filter(_.isLetter))
    .map(Stack(_*))

  val instructionSet = day5.input.dropWhile(_ != "").drop(1).map(Instruction.fromString(_))

  val state1 = initState.map(_.clone)
  val state2 = initState.map(_.clone)
  instructionSet.map(state1.commitInstruction9000(_))
  instructionSet.map(state2.commitInstruction9001(_))
  val output1 = state1.map(_.top).mkString
  val output2 = state2.map(_.top).mkString
}

