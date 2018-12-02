package aoc2017

object day22 extends App {

  sealed abstract class Direction {
    def left: Direction
    def right: Direction
    def reverse: Direction
  }
  case object Up extends Direction {
    override def left: Direction = Left
    override def right: Direction = Right
    override def reverse: Direction = Down
  }
  case object Down extends Direction {
    override def left: Direction = Right
    override def right: Direction = Left
    override def reverse: Direction = Up
  }
  case object Right extends Direction {
    override def left: Direction = Up
    override def right: Direction = Down
    override def reverse: Direction = Left
  }
  case object Left extends Direction {
    override def left: Direction = Down
    override def right: Direction = Up
    override def reverse: Direction = Right
  }
  case class Point(x: Long, y: Long) {
    def go(dir: Direction): Point = dir match {
      case Up    => Point(x, y - 1)
      case Down  => Point(x, y + 1)
      case Left  => Point(x - 1, y)
      case Right => Point(x + 1, y)
    }
  }

  case class State(infected: Set[Point],
                   pos: Point = Point(0, 0),
                   dir: Direction = Up) {
    def next: (Boolean, State) = {
      if (infected contains pos)
        (false, State(infected - pos, pos.go(dir.right), dir.right))
      else
        (true, State(infected + pos, pos.go(dir.left), dir.left))
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
                          dir: Direction = Up) {
    def next: (Boolean, StateEvolved) = {
      if (weaken contains pos)
        (
          true,
          StateEvolved(infected + pos, weaken - pos, flagged, pos.go(dir), dir)
        )
      else if (infected contains pos)
        (
          false,
          StateEvolved(
            infected - pos,
            weaken,
            flagged + pos,
            pos.go(dir.right),
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
            pos.go(dir.reverse),
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
            pos.go(dir.left),
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
