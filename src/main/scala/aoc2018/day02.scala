package aoc2018

object day02 extends App {

  def containsLettersExactlyNTimes(n: Int)(str: String) =
    str.groupBy(identity).exists { case (_, l) => l.length == n }

  def countSame(t: List[String], count: Int) =
    t.count(containsLettersExactlyNTimes(count))

  def part1(input: List[String]) =
    countSame(input, 2) * countSame(input, 3)

  def part2(input: List[String]) = {
    val possibles = for {
      a <- input
      b <- input if a < b
    } yield a.zip(b).collect { case (ca, cb) if ca == cb => ca }.mkString

    possibles.maxBy(_.length)
  }

  val input = io.Source.stdin.getLines.toList
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
