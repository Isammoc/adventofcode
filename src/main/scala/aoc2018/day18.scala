package aoc2018
import common.grid.Point

object day18 extends App {

  sealed abstract class Kind
  case object Open extends Kind
  case object Tree extends Kind
  case object LumberYard extends Kind

  def allNeighbors(p: Point): Set[Point] =
    (for {
      y <- -1 to 1
      x <- -1 to 1
    } yield Point(p.x + x, p.y + y)).toSet - p

  def next(origin: Kind, trees: Int, lumberYards: Int): Kind = origin match {
    case Open if trees >= 3                           => Tree
    case Open                                         => Open
    case Tree if lumberYards >= 3                     => LumberYard
    case Tree                                         => Tree
    case LumberYard if lumberYards >= 1 && trees >= 1 => LumberYard
    case LumberYard                                   => Open
  }

  def minute(area: Map[Point, Kind]): Map[Point, Kind] =
    (for {
      (p, k) <- area.toList
      ns = allNeighbors(p).toList.flatMap(area.get)
      trees = ns.count(_ == Tree)
      lumberyards = ns.count(_ == LumberYard)
    } yield (p, next(k, trees, lumberyards))).toMap

  def part1(input: String): Int = {
    val startArea = parseInput(input)
    val endArea = (1 to 10).foldLeft(startArea) {
      case (before, _) => minute(before)
    }

    endArea.values.count(_ == Tree) * endArea.values.count(_ == LumberYard)
  }

  def part2(input: String) = {
    val end = 1000000000
    val startArea = parseInput(input)
    def loop(count: Int = 0, current: Map[Point, Kind] = startArea, visited: Map[Map[Point, Kind], Int] = Map.empty): Map[Point, Kind] = if (visited.contains(current)) {
      val before = visited(current)
      (1 to ((end - before) % (count - before))).foldLeft(current) {
        case (todo, _) => minute(todo)
      }
    } else {
      loop(count + 1, minute(current), visited + (current -> count))
    }
    val endArea = loop()
    endArea.values.count(_ == Tree) * endArea.values.count(_ == LumberYard)
  }

  private def parseInput(input: String): Map[Point, Kind] = {
    (for {
      (line, y) <- input.split("\n").zipWithIndex
      (c, x) <- line.toList.zipWithIndex
    } yield
      (Point(x, y), c match {
        case '.' => Open
        case '|' => Tree
        case '#' => LumberYard
      })).toMap
  }
  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
