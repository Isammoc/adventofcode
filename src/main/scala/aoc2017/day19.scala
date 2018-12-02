package aoc2017

object day19 extends App {

  case class Point(x: Int, y: Int) {
    def go(dir: Direction): Point = dir match {
      case Up    => Point(x, y - 1)
      case Down  => Point(x, y + 1)
      case Left  => Point(x - 1, y)
      case Right => Point(x + 1, y)
    }
  }
  abstract class Direction {
    def turn(p: Point): List[(Direction, Point)]
  }
  case object Up extends Direction {
    override def turn(p: Point): List[(Direction, Point)] =
      List(Left, Right).map(d => (d, p.go(d)))
  }
  case object Down extends Direction {
    override def turn(p: Point): List[(Direction, Point)] =
      List(Left, Right).map(d => (d, p.go(d)))
  }
  case object Right extends Direction {
    override def turn(p: Point): List[(Direction, Point)] =
      List(Up, Down).map(d => (d, p.go(d)))
  }
  case object Left extends Direction {
    override def turn(p: Point): List[(Direction, Point)] =
      List(Up, Down).map(d => (d, p.go(d)))
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
          loop(p.go(dir), dir, found)
        case c => // letter
          loop(p.go(dir), dir, c :: found)
      }
    }
    loop(Point(map(0).indexOf('|'), 0), Down, Nil)
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
          loop(p.go(dir), dir, count + 1)
      }
    }
    loop(Point(map(0).indexOf('|'), 0), Down, 0)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
