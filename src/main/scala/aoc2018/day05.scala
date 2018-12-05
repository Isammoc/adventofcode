package aoc2018

object day05 extends App {

  def part1(input: String) = {
    def loop(remain: List[Char], visited: List[Char]): Int = (remain, visited) match {
      case (Nil, _) => visited.size
      case (a::t1, b::t2) if (a-b).abs == ('A'-'a').abs => loop(t1, t2)
      case (a::t, _) => loop(t, a::visited)
    }
    loop(input.toList, Nil)
  }

  def part2(input: String) = {
    val all = for{
      i <- 'a' to 'z'
      current = input.toList.filterNot(c => c == i || ((c - i).abs == ('A' - 'a').abs))
    } yield part1(current.mkString)
    all.min
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
