package aoc2015

object day02 extends App {

  def parseInput(input: String) = {
    val LineReg = "([0-9]+)x([0-9]+)x([0-9]+)".r
    input
      .split("\n")
      .map {
        case LineReg(a, b, c) =>
          List(a.toInt, b.toInt, c.toInt).sorted
      }
  }

  def part1(input: String) =
    parseInput(input).map {
      case List(l, w, h) =>
        3 * l * w + 2 * w * h + 2 * h * l
    }.sum

  def part2(input: String) =
    parseInput(input).map {
      case List(l, w, h) =>
        2 * l + 2 * w + l * w * h
    }.sum

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
