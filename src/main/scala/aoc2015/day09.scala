package aoc2015

object day09 extends App {

  def parseInput(input: String): Map[String, Map[String, Int]] = {
    val LineReg = "(.*) to (.*) = ([0-9]+)".r
    def loop(remain: List[String], res: Map[String, Map[String, Int]]): Map[String, Map[String, Int]] = remain match {
      case Nil => res
      case LineReg(a,b,w)::t =>
    loop(t, res + (a -> (res.getOrElse(a, Map.empty) + (b -> w.toInt))) + (b -> (res.getOrElse(b, Map.empty) + (a -> w.toInt))))
    }
    loop(input.split("\n").toList, Map.empty)
  }

  def part1(input: String) = {
    val graph = parseInput(input)
    graph.keySet.toList.permutations.map {order =>
      order.zip(order.tail).map {case (a,b) => graph(a)(b)}.sum
    }.min
  }

  def part2(input: String) = {
    val graph = parseInput(input)
    graph.keySet.toList.permutations.map {order =>
      order.zip(order.tail).map {case (a,b) => graph(a)(b)}.sum
    }.max
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
