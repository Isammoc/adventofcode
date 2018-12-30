package aoc2015
import common.grid.Point

object day06 extends App {

  case class Line(verb: String, x1: Int, y1: Int, x2: Int, y2: Int)

  def parseInput(input: String): List[Line] = {
    val LineReg = "(.*) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)".r
    input.split("\n").toList.map {
      case LineReg(verb, x1, y1, x2, y2) =>
        Line(verb, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
    }
  }

  def part1(input: String): Int = {
    val lines = parseInput(input)
    def loop(remain: List[Line], lamps: Map[Point, Int]): Int = remain match {
      case Nil => lamps.values.sum
      case Line(verb, x1, y1, x2, y2) :: t =>
        loop(
          t,
          lamps ++ (for {
            x <- x1 to x2
            y <- y1 to y2
          } yield {
            verb match {
              case "turn on"  => Point(x, y) -> 1
              case "turn off" => Point(x, y) -> 0
              case "toggle" =>
                Point(x, y) -> (1 - lamps.getOrElse(Point(x, y), 0))
            }
          })
        )
    }
    loop(lines, Map.empty)
  }

  def part2(input: String) = {
    val lines = parseInput(input)
    def loop(remain: List[Line], lamps: Map[Point, Int]): Int = remain match {
      case Nil => lamps.values.sum
      case Line(verb, x1, y1, x2, y2) :: t =>
        loop(
          t,
          lamps ++ (for {
            x <- x1 to x2
            y <- y1 to y2
          } yield {
            verb match {
              case "turn on" =>
                Point(x, y) -> (lamps.getOrElse(Point(x, y), 0) + 1)
              case "turn off" =>
                Point(x, y) -> math.max(0, lamps.getOrElse(Point(x, y), 0) - 1)
              case "toggle" =>
                Point(x, y) -> (lamps.getOrElse(Point(x, y), 0) + 2)
            }
          })
        )
    }
    loop(lines, Map.empty)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
