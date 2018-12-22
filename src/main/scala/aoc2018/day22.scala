package aoc2018
import common.algo.graph.Dijkstra
import common.grid.Direction.{North, West}
import common.grid.{Direction, Point}

object day22 extends App {

  case class Cave(depth: Int, target: Point) {
    sealed abstract class Equiped {
      val impossible: Int
    }
    case object Torch extends Equiped {
      val impossible: Int = WET
    }
    case object ClimbingGear extends Equiped {
      val impossible: Int = NARROW
    }
    case object Neither extends Equiped {
      val impossible: Int = ROCKY
    }

    val ROCKY = 0
    val WET = 1
    val NARROW = 2

    val start = Point(0, 0)
    private var erosionLevels = Map.empty[Point, Int]
    def geologicIndex(p: Point): Int = {
      val start = Point(0, 0)
      if (p == start || p == target) {
        0
      } else if (p.y == 0) {
        p.x * 16807
      } else if (p.x == 0) {
        p.y * 48271
      } else {
        erosionLevel(p.move(West)) * erosionLevel(p.move(North))
      }
    }

    def erosionLevel(p: Point): Int = erosionLevels.get(p) match {
      case Some(res) => res
      case None =>
        val res = (depth + geologicIndex(p)) % 20183
        erosionLevels += p -> res
        res
    }

    def risk(p: Point): Int = erosionLevel(p) % 3

    def riskArea: Int =
      (for {
        y <- 0 to target.y
        x <- 0 to target.x
      } yield risk(Point(x, y))).sum

    def fastest: Long = {
      case class State(tool: Equiped, location: Point) {
        def next: Iterable[(State, Long)] = {
          val allTools: Set[Equiped] = Set(Torch, ClimbingGear, Neither)
          val nextTool =
            (allTools - tool).filter(_.impossible != risk(location)).head

          val dests = (for {
            dir <- Direction.all
            dest = location.move(dir) if dest.x >= 0 && dest.y >= 0 && tool.impossible != risk(dest)
          } yield State(tool, dest) -> 1L).toMap
          dests + (State(nextTool, location) -> 7L)
        }
      }

      Dijkstra.reach(State(Torch, start))(State(Torch, target))(_.next)
    }
  }

  private def parseInput(input: String) = {
    val InputReg = "depth: ([0-9]+)\ntarget: ([0-9]+),([0-9]+)".r
    val cave = input match {
      case InputReg(depth, x, y) => Cave(depth.toInt, Point(x.toInt, y.toInt))
    }
    cave
  }

  def part1(input: String) = {
    val cave: Cave = parseInput(input)
    cave.riskArea
  }

  def part2(input: String) = {
    val cave: Cave = parseInput(input)
    cave.fastest
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
