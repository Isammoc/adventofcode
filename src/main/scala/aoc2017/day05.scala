package aoc2017

object day05 extends App {

  def part1(input: String) = {
    val lines = input.split("\n").toList.map(_.toInt)
    val instr = lines.zipWithIndex.map {
      case (a, i) => (i -> a)
    }.toMap

    def loop(count: Int, current: Int, instr: Map[Int, Int]): Int =
      if (current >= lines.size) count
      else {
        loop(
          count + 1,
          current + instr(current),
          instr + (current -> (instr(current) + 1))
        )
      }
    loop(0, 0, instr)
  }

  def part2(input: String) = {
    val lines = input.split("\n").toList.map(_.toInt)
    val instr = lines.zipWithIndex.map {
      case (a, i) => (i -> a)
    }.toMap
    def loop(count: Int, current: Int, instr: Map[Int, Int]): Int =
      if (current >= lines.size || current < 0)
        count
      else if (instr(current) >= 3)
        loop(
          count + 1,
          current + instr(current),
          instr + (current -> (instr(current) - 1))
        )
      else
        loop(
          count + 1,
          current + instr(current),
          instr + (current -> (instr(current) + 1))
        )
    loop(0, 0, instr)

  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
