package aoc2017
import common.grid.Direction._
import common.grid._

object day19 extends App {

  implicit class MyDirection(direction: Direction) {
    def turn(p: Point): List[(Direction, Point)] = direction match {
      case North => List(West, East).map(d => (d, p.move(d)))
      case South => List(West, East).map(d => (d, p.move(d)))
      case East  => List(North, South).map(d => (d, p.move(d)))
      case West  => List(North, South).map(d => (d, p.move(d)))
    }
  }

  def part1(input: String): String = {
    def map = input.split("\n")

    def loop(p: Point, dir: Direction, found: List[Char]): String = {
      map(p.y)(p.x) match {
        case ' ' =>
          found.reverse.mkString
        case '+' => // turn
          val (d, p2) = dir
            .turn(p)
            .filter {
              case (_, p2) =>
                0 <= p2.y && p2.y < map.size && 0 <= p2.x && p2.x < map(0).size && map(
                  p2.y
                )(p2.x) != ' '
            }
            .head
          loop(p2, d, found)
        case c if c == '|' || c == '-' => // continue
          loop(p.move(dir), dir, found)
        case c => // letter
          loop(p.move(dir), dir, c :: found)
      }
    }
    loop(Point(map(0).indexOf('|'), 0), South, Nil)
  }

  def part2(input: String): Long = {
    def map = input.split("\n")

    def loop(p: Point, dir: Direction, count: Long): Long = {
      map(p.y)(p.x) match {
        case ' ' =>
          count
        case '+' => // turn
          val (d, p2) = dir
            .turn(p)
            .filter {
              case (_, p2) =>
                0 <= p2.y && p2.y < map.size && 0 <= p2.x && p2.x < map(0).size && map(
                  p2.y
                )(p2.x) != ' '
            }
            .head
          loop(p2, d, count + 1)
        case _ => // continue
          loop(p.move(dir), dir, count + 1)
      }
    }
    loop(Point(map(0).indexOf('|'), 0), South, 0)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
