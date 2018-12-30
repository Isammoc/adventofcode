package aoc2015

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
    (1 to 40)
      .foldLeft(parseInput(input))((previous, _) => next(previous))
      .size

  def part2(input: String): Int =
    (1 to 50)
      .foldLeft(parseInput(input))((previous, _) => next(previous))
      .size

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
