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

case object Day5 extends Day{
  val stateTemplate = Day5.input
    .takeWhile(_ != "")
    .map(_.toCharArray)
    .transpose
    .filter(_.exists(_.isDigit))
    .map(_.filter(_.isLetter))

  val instructionSet = Day5.input.dropWhile(_ != "").drop(1).map(Instruction.fromString(_))

  val output1 = {
    val state = stateTemplate.map(Stack(_*))
    instructionSet.map(state.commitInstruction9000(_))
    state.map(_.top).mkString
  }
  val output2 = {
    val state = stateTemplate.map(Stack(_*))
    instructionSet.map(state.commitInstruction9001(_))
    state.map(_.top).mkString
  }
}

