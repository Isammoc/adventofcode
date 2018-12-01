package aoc2017

object day17 extends App {
  def part1(input: Int): Int = {
    def loop(current: Int, state: List[Int]): Int =
      if (current > 2017) {
        state(1)
      } else {
        val where = (input + 1) % state.size
        loop(current + 1, current :: state.drop(where) ++ state.take(where))
      }

    loop(1, List(0))
  }

  def part2(input: Int): Int = {
    def loop(current: Int, position: Int, possible: Int): Int =
      if (current > 50000000)
        possible
      else {
        val newPosition = (position + input) % current + 1
        if (newPosition == 1) {
          loop(current + 1, newPosition, current)
        } else {
          loop(current + 1, newPosition, possible)
        }
      }
    loop(1, 0, 1)
  }

  def part1(input: String) = ???

  // def part2(input: String) = ???

  val input = io.Source.stdin.getLines.next.toInt
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
