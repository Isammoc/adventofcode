package aoc2015
import common.util.Repeat

object day10 extends App {

  def next(str: List[Int]): List[Int] = {
    def loop(remain: List[Int], res: List[Int]): List[Int] = remain match {
      case Nil                                  => res.reverse
      case a :: b :: c :: t if a == b && b == c => loop(t, a :: 3 :: res)
      case a :: b :: t if a == b                => loop(t, a :: 2 :: res)
      case a :: t                               => loop(t, a :: 1 :: res)
    }
    loop(str, Nil)
  }

  def parseInput(input: String): List[Int] = input.toList.map(_ - '0')

  def part1(input: String) =
    Repeat(40)(parseInput(input))(next).size

  def part2(input: String): Int =
    Repeat(50)(parseInput(input))(next).size

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
