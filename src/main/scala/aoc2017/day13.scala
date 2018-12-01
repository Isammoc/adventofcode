package aoc2017

object day13 extends App {

   case class Scanner(depth: Int, range: Int) {
    def catchMe(delay: Int): Boolean = (depth + delay) % ((range - 1) * 2) == 0
    def severity(delay: Int): Int =
      if (catchMe(delay))
        depth * range
      else
        0
  }

  def part1(input: String): Int = {
    val LineReg = "([0-9]+): ([0-9]+)".r
    val scanners = input.split("\n").toList.map {
      case LineReg(d, r) => Scanner(d.toInt, r.toInt)
    }

    scanners.map(_.severity(0)).sum
  }

  def part2(input: String): Int = {
    val LineReg = "([0-9]+): ([0-9]+)".r
    val scanners = input.split("\n").toList.map {
      case LineReg(d, r) => Scanner(d.toInt, r.toInt)
    }

    Stream.from(0).filter(delay => scanners.forall(!_.catchMe(delay))).head
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
