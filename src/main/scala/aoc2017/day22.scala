package aoc2017
import common.grid.Direction._
import common.grid._

object day22 extends App {
  case class State(infected: Set[Point],
                   pos: Point = Point(0, 0),
                   dir: Direction = North) {
    def next: (Boolean, State) = {
      if (infected contains pos)
        (false, State(infected - pos, pos.move(dir.right), dir.right))
      else
        (true, State(infected + pos, pos.move(dir.left), dir.left))
    }
  }
  def part1(input: String, count: Int = 10000): Int = {
    val lines = input.split("\n")

    val initial = (for {
      y <- -(lines.size / 2) to (lines.size / 2)
      x <- -(lines(0).length / 2) to (lines(0).length / 2)
      if lines(y + (lines.size / 2))(x + (lines(0).length / 2)) == '#'
    } yield Point(x, y)).toSet

    def loop(state: State, remain: Int = count, infected: Int = 0): Int =
      if (remain <= 0) infected
      else
        state.next match {
          case (true, n)  => loop(n, remain - 1, infected + 1)
          case (false, n) => loop(n, remain - 1, infected)
        }

    loop(State(initial))
  }

  case class StateEvolved(infected: Set[Point],
                          weaken: Set[Point] = Set.empty,
                          flagged: Set[Point] = Set.empty,
                          pos: Point = Point(0, 0),
                          dir: Direction = North) {
    def next: (Boolean, StateEvolved) = {
      if (weaken contains pos)
        (
          true,
          StateEvolved(
            infected + pos,
            weaken - pos,
            flagged,
            pos.move(dir),
            dir
          )
        )
      else if (infected contains pos)
        (
          false,
          StateEvolved(
            infected - pos,
            weaken,
            flagged + pos,
            pos.move(dir.right),
            dir.right
          )
        )
      else if (flagged contains pos)
        (
          false,
          StateEvolved(
            infected,
            weaken,
            flagged - pos,
            pos.move(dir.reverse),
            dir.reverse
          )
        )
      else
        (
          false,
          StateEvolved(
            infected,
            weaken + pos,
            flagged,
            pos.move(dir.left),
            dir.left
          )
        )
    }
  }

  def part2(input: String, count: Int = 10000000): Int = {
    val lines = input.split("\n")

    val initial = (for {
      y <- -(lines.size / 2) to (lines.size / 2)
      x <- -(lines(0).length / 2) to (lines(0).length / 2)
      if lines(y + (lines.size / 2))(x + (lines(0).length / 2)) == '#'
    } yield Point(x, y)).toSet

    def loop(state: StateEvolved, remain: Int = count, infected: Int = 0): Int =
      if (remain <= 0) infected
      else
        state.next match {
          case (true, n)  => loop(n, remain - 1, infected + 1)
          case (false, n) => loop(n, remain - 1, infected)
        }

    loop(StateEvolved(initial))
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
