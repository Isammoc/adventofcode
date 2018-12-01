package aoc2018

object day01 extends App {
  def part1(input: List[String]): Int = input.map(_.toInt).sum

  def part2(input: List[String]): Int = {
    val initial = input.map(_.toInt)
    def loop(remain: List[Int], current: Int, visited: Set[Int]): Int =
      remain match {
        case Nil                                      => loop(initial, current, visited)
        case h :: _ if visited contains (current + h) => current + h
        case h :: t                                   => loop(t, current + h, visited + (current + h))
      }
    loop(initial, 0, Set(0))
  }

  val input = io.Source.stdin.getLines.toList
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
