package aoc2017
import common.util.Repeat

object day16 extends App {

  case class State(current: String) {

    def spin(i: Int) =
      State(current.drop(current.size - i) + current.take(current.size - i))

    def exchange(i: Int, j: Int): State =
      if (i == j) this
      else if (i > j) this.exchange(j, i)
      else
        State(
          current.take(i) + current(j) + current.take(j).drop(i + 1) + current(
            i
          ) + current.drop(j + 1)
        )

    def partner(i: Char, j: Char) =
      State(current.map {
        case a if a == i => j
        case a if a == j => i
        case a           => a
      })
  }

  def part1(input: String, initial: String = "abcdefghijklmnop"): String = {
    val Spin = "s([0-9]+)".r
    val Exchange = "x([0-9]+)/([0-9]+)".r
    val Partner = "p([a-z]+)/([a-z]+)".r

    def loop(remain: List[String], current: State): String = remain match {
      case Nil                 => current.current
      case Spin(spin) :: t     => loop(t, current.spin(spin.toInt))
      case Exchange(i, j) :: t => loop(t, current.exchange(i.toInt, j.toInt))
      case Partner(i, j) :: t  => loop(t, current.partner(i(0), j(0)))
    }
    loop(input.split(",").toList, State(initial))
  }

  def part2(input: String,
            times: Int = 1000000000,
            initial: String = "abcdefghijklmnop"): String = {

    def loop(count: Int, current: String, seen: Map[String, Int]): String =
      (count, seen.get(current)) match {
        case (c, _) if c == times => current
        case (_, None) =>
          loop(count + 1, part1(input, current), seen + (current -> count))
        case (_, Some(s)) => {
          val remain = (times - s) % (s - count)
          Repeat(remain)(current)(part1(input, _))
        }
      }

    loop(0, initial, Map.empty)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
