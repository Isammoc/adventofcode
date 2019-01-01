package aoc2015

object day17 extends App {

  def ways(containers: List[Int], total: Int): List[List[Int]] = {
    def loop(remainingContainers: List[Int],
             possible: List[List[Int]],
             currents: List[List[Int]]): List[List[Int]] =
      remainingContainers match {
        case Nil => possible
        case container :: t =>
          val next = for {
            current <- currents
            a <- List(current, container :: current)
          } yield a
          val newPossible = next.filter(_.sum == total)
          loop(t, newPossible ::: possible, next.filter(_.sum < total))
      }
    loop(containers, Nil, List(Nil))
  }

  def parseInput(input: String): List[Int] =
    input.split("\n").toList.map(_.toInt)

  def part1(input: String) = ways(parseInput(input), 150).size

  def part2(input: String) = {
    val w = ways(parseInput(input), 150)
    val minCount = w.map(_.size).min
    w.count(_.size == minCount)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
