package aoc2018

object day11 extends App {

  def generateSummedAreaTable(f: (Int, Int) => Int): Array[Array[Int]] = {
    val result = Array.ofDim[Int](301, 301)
    for {
      x <- 1 to 300
      y <- 1 to 300
    } {
      result(x)(y) =
        result(x - 1)(y) +
          result(x)(y - 1) -
          result(x - 1)(y - 1) +
          f(x, y)
    }
    result
  }

  def power(serial: Int)(x: Int, y: Int) =
    if (x < 1 || x > 300 || y < 1 || y > 300)
      0
    else
      ((((x + 10) * y + serial) * (x + 10) / 100) % 10) - 5

  def part1(input: Int) = {
    val summedAreaTable = generateSummedAreaTable(power(input))
    var maxValue = Int.MinValue
    var result = "not found"

    for {
      x <- 1 to (300 - 2)
      y <- 1 to (300 - 2)
    } {
      val current =
        summedAreaTable(x + 2)(y + 2) +
          summedAreaTable(x - 1)(y - 1) -
          summedAreaTable(x - 1)(y + 2) -
          summedAreaTable(x + 2)(y - 1)

      if (current > maxValue) {
        maxValue = current
        result = s"$x,$y"
      }
    }

    result
  }

  def part2(input: Int) = {
    val summedAreaTable = generateSummedAreaTable(power(input))
    var maxValue = Int.MinValue
    var result = "not found"

    for {
      x <- 1 to (300 - 2)
      y <- 1 to (300 - 2)
      size <- 3 to (300 - math.max(x, y))
    } {
      val current =
        summedAreaTable(x + size - 1)(y + size - 1) +
          summedAreaTable(x - 1)(y - 1) -
          summedAreaTable(x - 1)(y + size - 1) -
          summedAreaTable(x + size - 1)(y - 1)

      if (current > maxValue) {
        maxValue = current
        result = s"$x,$y,$size"
      }
    }

    result
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input.toInt))
  println("part2 = " + part2(input.toInt))
}
