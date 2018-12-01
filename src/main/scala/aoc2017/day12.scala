package aoc2017

object day12 extends App {
  def part1(input: String): Int = {
    val LineReg = "(\\d+) <-> (.*)".r

    val neigh = input
      .split("\n")
      .map {
        case LineReg(from, to) =>
          (from.toInt -> to.split(", ").map(_.toInt).toSet)
        case _ =>
          println("Nooooo")
          (0 -> Set(0))
      }
      .toMap
    def loop(toVisit: List[Int], visited: Set[Int]): Int = toVisit match {
      case Nil                          => visited.size
      case h :: t if visited contains h => loop(t, visited)
      case h :: t                       => loop(t ++ neigh(h), visited + h)
    }
    loop(List(0), Set.empty)
  }
  def part2(input: String): Int = {
    val LineReg = "(\\d+) <-> (.*)".r

    val neigh = input
      .split("\n")
      .map {
        case LineReg(from, to) =>
          (from.toInt -> to.split(", ").map(_.toInt).toSet)
        case _ =>
          println("Nooooo")
          (0 -> Set(0))
      }
      .toMap

    def loop(remain: List[Int],
             toVisit: List[Int],
             visited: Set[Int],
             groups: Int): Int = (remain, toVisit) match {
      case (Nil, Nil)                          => groups
      case (h :: t, Nil) if visited contains h => loop(t, Nil, visited, groups)
      case (h :: t, Nil)                       => loop(t, List(h), visited, groups + 1)
      case (_, h :: t) if visited contains h   => loop(remain, t, visited, groups)
      case (_, h :: t)                         => loop(remain, t ++ neigh(h), visited + h, groups)
    }
    loop(neigh.keys.toList, Nil, Set.empty, 0)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
