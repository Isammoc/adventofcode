package aoc2017

object day02 extends App {

  def part1(input: String): Int = {
    val differences = for {
      line <- input.split("\n")
      ints = line.split("\\s+").map(_.toInt)
      max = ints.max
      min = ints.min
    } yield max - min
    differences.sum
  }

  def part2(input: String): Int = {
    val divisions = input
      .split("\n")
      .map(_.split("\\s+").map(_.toInt))
      .toList
      .map { numbers =>
        val division = for {
          a <- numbers
          b <- numbers if a < b && b % a == 0
        } yield b / a
        division.sum
      }

    divisions.sum

  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
