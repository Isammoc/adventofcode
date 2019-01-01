package aoc2015
import common.util.Memoize

object day07 extends App {
  class Wire(val affectations: Map[String, String]) {
    private val NumberReg = "^([0-9]+)$".r
    private val AndReg = "(.*) AND (.*)".r
    private val OrReg = "(.*) OR (.*)".r
    private val LShiftReg = "(.*) LSHIFT (.*)".r
    private val RShiftReg = "(.*) RSHIFT (.*)".r
    private val NotReg = "NOT (.*)".r

    lazy val value: String => Int = Memoize.memoize(v => (v, affectations.get(v)) match {
      case (NumberReg(n), _) => n.toInt
      case (_, Some(NumberReg(a))) => a.toInt
      case (_, Some(AndReg(a, b))) => value(a) & value(b)
      case (_, Some(OrReg(a, b))) => value(a) | value(b)
      case (_, Some(LShiftReg(a, b))) => value(a) << value(b)
      case (_, Some(RShiftReg(a, b))) => value(a) >> value(b)
      case (_, Some(NotReg(a))) => 65535 - value(a)
      case (_, Some(b)) if affectations.contains(b) => value(b)
    })

  }

  def parseInput(input: String): Wire = {
    val AffectationReg = "^(.*) -> (.*)$".r
    val affectations = input
      .split("\n")
      .map {
        case AffectationReg(left, right) => right -> left
      }
      .toMap

    new Wire(affectations)
  }

  def part1(input: String) = {
    val wire = parseInput(input)
    wire.value("a")
  }

  def part2(input: String) = {
    val wire = parseInput(input)
    val firstA = wire.value("a")
    val wire2 = new Wire(wire.affectations + ("b" -> firstA.toString))
    wire2.value("a")
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
