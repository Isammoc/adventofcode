package aoc2018
import common.grid.Direction.{East, North, South, West}
import common.grid.Point

object day17 extends App {

  def parseInput(input: String): Set[Point] = {
    val XYReg = "x=([0-9]+), y=([0-9]+)..([0-9]+)".r
    val YXReg = "y=([0-9]+), x=([0-9]+)..([0-9]+)".r
    input
      .split("\n")
      .flatMap {
        case XYReg(x, y1, y2) =>
          for (y <- y1.toInt to y2.toInt) yield Point(x.toInt, y)
        case YXReg(y, x1, x2) =>
          for (x <- x1.toInt to x2.toInt) yield Point(x, y.toInt)
      }
      .toSet
  }

  case class State(toConsider: Set[Point] = Set.empty,
                   movable: Set[Point] = Set.empty,
                   still: Set[Point] = Set.empty)

  def forAPoint(maxy: Int, movable: Set[Point], blocked: Set[Point])(
    w: Point
  ): State = {
    if (w.y >= maxy) {
      State()
    } else if (!blocked.contains(w.move(South))) {
      val lineDown = Stream
        .from(1)
        .map(y => w.copy(y = w.y + y))
        .takeWhile(_.y <= maxy)
        .takeWhile(!blocked.contains(_))
        .toSet
      val bottom = lineDown.maxBy(_.y)
      State(toConsider = Set(bottom), movable = lineDown)
    } else {
      val toLeft = Stream
        .from(0)
        .map(x => w.copy(x = w.x - x))
        .takeWhile(!blocked.contains(_))
        .takeWhile(p => blocked.contains(p.move(South)))
        .toSet
      val toRight = Stream
        .from(0)
        .map(x => w.copy(x = w.x + x))
        .takeWhile(!blocked.contains(_))
        .takeWhile(p => blocked.contains(p.move(South)))
        .toSet

      val line = toLeft ++ toRight
      if (line.isEmpty) {
        State()
      } else {
        val left = line.minBy(_.x)
        val right = line.maxBy(_.x)

        val toConsider = Set(left.move(West), right.move(East)) -- blocked
        if (toConsider.isEmpty) {
          val accessPoints = line.map(_.move(North)).intersect(movable)
          State(
            toConsider = accessPoints,
            still = line
          )
        } else {
          State(toConsider = toConsider, movable = line ++ toConsider)
        }
      }
    }
  }

  def simulate(clays: Set[Point]): State = {
    val maxy = clays.map(_.y).max
    val miny = clays.map(_.y).min
    def loop(toConsider: Set[Point] = Set(Point(500, 0)),
             movable: Set[Point] = Set(Point(500, 0)),
             still: Set[Point] = Set.empty,
             blocked: Set[Point] = clays): State = {
      if (toConsider.isEmpty) {
        State(
          movable = movable.filterNot(_.y < miny).filterNot(_.y > maxy),
          still = still
        )
      } else {
        val toVisit = for (w <- toConsider)
          yield forAPoint(maxy, movable, blocked)(w)
        val newToConsider = toVisit.flatMap(_.toConsider)
        val newStill = toVisit.flatMap(_.still)
        val newMovable = toVisit.flatMap(_.movable)
        loop(
          newToConsider,
          movable ++ newMovable -- newStill,
          still ++ newStill,
          blocked ++ newStill
        )
      }
    }
    loop()
  }

  def part1(input: String): Int = {
    val clays = parseInput(input)
    val result = simulate(clays)
    (result.movable ++ result.still).size
  }

  def part2(input: String) = {
    val clays = parseInput(input)
    val result = simulate(clays)
    result.still.size
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
