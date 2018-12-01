package aoc2017

object day08 extends App {

  def common(input: String) = {
      val InstrReg = "([a-z]+) ([a-z]+) (-?[0-9]+) if ([a-z]+) (==|!=|<|<=|>|>=) (-?[0-9]+)".r

  val comp = Map[String, (Int, Int) => Boolean](
    "==" -> ((a, b) => a == b),
    "!=" -> ((a, b) => a != b),
    "<" -> ((a, b) => a < b),
    "<=" -> ((a, b) => a <= b),
    ">" -> ((a, b) => a > b),
    ">=" -> ((a, b) => a >= b))

  def loop(remain: List[String], state: Map[String, Int], maxKnown: Int): (Int, Int) = remain match {
    case Nil => (state.values.max, maxKnown)
    case InstrReg(reg, action, value, condReg, condComp, condValue) :: t =>
      if (comp(condComp)(state.getOrElse(condReg, 0), condValue.toInt)) {
        val newValue = action match {
          case "inc" => state.getOrElse(reg, 0) + value.toInt
          case "dec" => state.getOrElse(reg, 0) - value.toInt
        }
        loop(t, state + (reg -> newValue), math.max(maxKnown, newValue))
      } else {
        loop(t, state, maxKnown)
      }
  }

loop(input.split("\n").toList, Map.empty, Int.MinValue)

  }

  def part1(input: String) = common(input)._1

  def part2(input: String) = common(input)._2

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
