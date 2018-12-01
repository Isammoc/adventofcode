package aoc2017

object day04 extends App {

  def part1(input: String) =
    input
      .split("\n")
      .count {
        _.split(" ").toList
          .groupBy(i => i)
          .values
          .forall(_.size <= 1)
      }

  def part2(input: String) =
    input
      .split("\n")
      .count {
        _.split(" ").toList
          .groupBy(i => i.sorted)
          .values
          .forall(_.size <= 1)
      }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
