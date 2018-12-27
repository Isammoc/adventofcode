package aoc2015

object day01 extends App {

  def part1(input: String) = {
    input.count(_ == '(') - input.count(_ == ')')
  }

  def part2(input: String) = {
    def loop(remain: List[Char], current: Int = 0): Int = remain match {
      case '(' :: t                 => loop(t, current + 1)
      case ')' :: _ if current == 0 => input.length - remain.size + 1
      case ')' :: t                 => loop(t, current - 1)
    }
    loop(input.toList)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
