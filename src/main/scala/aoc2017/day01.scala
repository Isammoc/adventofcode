package aoc2017

object day01 extends App {
  def part1(str: String): Int = {
    val inputInts = str.map(_.toString.toInt).toList
    inputInts
      .zip(inputInts.last :: inputInts)
      .collect {
        case (a, b) if a == b => a
      }
      .sum
  }

  def part2(input: String): Int = {
    val inputInts = input.map(_.toString.toInt).toList
    inputInts
      .zip(
        inputInts.drop(inputInts.size / 2) ++ inputInts.take(inputInts.size / 2)
      )
      .collect { case (a, b) if a == b => a }
      .sum
  }

  val input = io.Source.stdin.getLines.mkString
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))

}
