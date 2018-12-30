package aoc2015

object day11 extends App {

  def includeStraight(xs: List[Int]) =
    xs.zip(xs.tail).zip(xs.tail.tail).exists {
      case ((a, b), c) => b - c == 1 && a - b == 1
    }

  def freeOfForbidden(xs: List[Int]): Boolean = {
    val forbidden = Set('i' - 'a', 'o' - 'a', 'l' - 'a')
    !xs.exists(forbidden.contains)
  }

  def twoPairs(xs: List[Int]): Boolean =
    xs.zip(xs.tail)
      .filter {
        case (a, b) => a == b
      }
      .toSet
      .size >= 2

  def addOne(i: List[Int]): List[Int] =
    if (i.head == 25) 0 :: addOne(i.tail) else (i.head + 1) :: i.tail

  def parseInput(input: String): List[Int] = input.toList.map(_ - 'a').reverse

  def displayResult(xs: List[Int]) =
    xs.reverse.map(_ + 'a').map(_.toChar).mkString

  def part1(input: String) =
    displayResult(
      Stream
        .iterate(parseInput(input))(addOne)
        .filter(twoPairs)
        .filter(includeStraight)
        .filter(freeOfForbidden)
        .head
    )

  def part2(input: String) =
    displayResult(
      Stream
        .iterate(parseInput(input))(addOne)
        .filter(twoPairs)
        .filter(includeStraight)
        .filter(freeOfForbidden)
        .tail
        .head
    )

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
