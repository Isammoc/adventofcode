package aoc2017

object day11 extends App {
  case class Position(sw: Int, s: Int, se: Int) {
    def to(dir: String): Position = dir match {
      case "s" => Position(sw, s + 1, se)
      case "se" => Position(sw, s, se + 1)
      case "sw" => Position(sw + 1, s, se)
      case "n" => Position(sw, s - 1, se)
      case "ne" => Position(sw - 1, s, se)
      case "nw" => Position(sw, s, se - 1)
    }

    def reduce: Position =
      if (sw * se > 0)
        Position(
          math.max(0, sw.abs - se.abs) * sw.signum,
          s + math.min(sw.abs, se.abs) * sw.signum,
          math.max(0, se.abs - sw.abs) * se.signum).reduce
      else if (sw * s < 0)
        Position(
          math.max(0, sw.abs - s.abs) * sw.signum,
          math.max(0, s.abs - sw.abs) * s.signum,
          se + math.min(s.abs, sw.abs) * s.signum).reduce
      else if (se * s < 0)
        Position(
          sw + math.min(s.abs, se.abs) * s.signum,
          math.max(0, s.abs - se.abs) * s.signum,
          math.max(0, se.abs - s.abs) * se.signum).reduce
      else this

    def length = sw.abs + s.abs + se.abs
  }

  def part1(input: String): Int = {
    def loop(remain: List[String], p: Position): Int = {
      remain match {
        case Nil => p.reduce.length
        case dir :: t => loop(t, p.to(dir).reduce)
      }
    }
    loop(input.split(",").toList, Position(0, 0, 0))
  }


  def part2(input: String): Int = {
    def loop(remain: List[String], p: Position, furthest: Int): Int = {
      remain match {
        case Nil => furthest
        case dir :: t => loop(t, p.to(dir).reduce, math.max(furthest, p.to(dir).reduce.length))
      }
    }
    loop(input.split(",").toList, Position(0, 0, 0), 0)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
