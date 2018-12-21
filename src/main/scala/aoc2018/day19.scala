package aoc2018
import aoc2018.opcode._

object day19 extends App {


  def part1(input: String): Int = {
    val lines = input.split("\n").toList
    val proc = Proc(lines.head, lines.tail)

    proc.execute((0 to 5).map(_ -> 0).toMap)(0)
  }

  // def part2(input: String) = ???
  // part2 is the sum of divisors of your value

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  //println("part2 = " + part2(input))
}
