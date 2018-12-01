package aoc2017

object day15 extends App {
  def part1(inputA: Long, inputB: Long): Int = {
    def loop(remain: Long, previousA: Long, previousB: Long, count: Int): Int =
      if (remain <= 0)
        count
      else {
        val a = (previousA * 16807) % 2147483647
        val b = (previousB * 48271) % 2147483647
        if ((a % (2 << 15)) == (b % (2 << 15)))
          loop(remain - 1, a, b, count + 1)
        else
          loop(remain - 1, a, b, count)
      }

    loop(40000000, inputA, inputB, 0)
  }
 def part2(inputA: Long, inputB: Long): Int = {
    def nextA(previousA: Long): Long = {
      val a = (previousA * 16807) % 2147483647
      if ((a % 4) == 0) a
      else
        nextA(a)
    }
    def nextB(previousB: Long): Long = {
      val b = (previousB * 48271) % 2147483647
      if ((b % 8) == 0) b
      else nextB(b)
    }

    def loop(remain: Long, previousA: Long, previousB: Long, count: Int): Int =
      if (remain <= 0)
        count
      else (nextA(previousA), nextB(previousB)) match {
        case (a, b) if (a % (2 << 15)) == (b % (2 << 15)) => loop(remain - 1, a, b, count + 1)
        case (a, b) => loop(remain - 1, a, b, count)
      }
    loop(5000000, inputA, inputB, 0)
  }

  val List(inputA, inputB) = io.Source.stdin.getLines.map(_.drop(24).toInt).toList
  println("part1 = " + part1(inputA, inputB))
  println("part2 = " + part2(inputA, inputB))
}

