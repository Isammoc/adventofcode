package aoc2018
import common.algo.graph.Dijkstra
import common.grid.Direction.{East, North, South, West}
import common.grid.{Direction, Point}

object day20 extends App {
  def parseInput(
    remain: List[Char],
    current: Point = Point(0, 0),
    stack: List[Point] = List(Point(0, 0)),
    edges: Map[Point, Set[Point]] = Map.empty
  ): Map[Point, Set[Point]] = remain match {
    case Nil      => edges
    case '^' :: t => parseInput(t, current, stack, edges)
    case '$' :: t => parseInput(t, current, stack, edges)
    case '(' :: t => parseInput(t, current, current :: stack, edges)
    case ')' :: t => parseInput(t, stack.head, stack.tail, edges)
    case '|' :: t => parseInput(t, stack.head, stack, edges)
    case w :: t =>
      val next = current.move(charToDir(w))
      parseInput(
        t,
        next,
        stack,
        edges + (current -> (edges
          .getOrElse(current, Set.empty) + next)) + (next -> (edges
          .getOrElse(next, Set.empty) + current))
      )
  }

  private def charToDir(w: Char): Direction = w match {
    case 'N' => North
    case 'W' => West
    case 'E' => East
    case 'S' => South
  }

  def part1(input: String) = {
    val neighbors = parseInput(input.toList)
    val dijkstra = Dijkstra.simpleDistance(Point(0, 0))(neighbors.apply)

    dijkstra.values.max
  }

  def part2(input: String) = {
    val neighbors = parseInput(input.toList)
    val dijkstra = Dijkstra.simpleDistance(Point(0, 0))(neighbors.apply)

    dijkstra.values.count(_ >= 1000)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
