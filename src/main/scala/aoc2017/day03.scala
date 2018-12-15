package aoc2017
import common.grid.Point

object day03 extends App {

  def toMove(i: Int) =
    if (i == 1)
      0
    else {
      val ring = ((math.sqrt(i - 1) + 1) / 2).floor.toInt
      val previousRingSize = (2 * ring - 1) * (2 * ring - 1)
      ring + ((i - previousRingSize) % (ring * 2) - ring).abs
    }

  def part1(input: String) = toMove(input.toInt)

  implicit class ValuePoint(point: Point) {
    def ring = math.max(point.x.abs, point.y.abs)
    def n = point match {
      case Point(0, 0) => 1
      case Point(x, y) if x > 0 && x != y && x >= y.abs => // EAST
        (2 * this.ring - 1) * (2 * this.ring - 1) + (this.ring - y)
      case Point(x, y) if y < 0 && y != -x && -y >= x.abs => // NORTH
        (2 * this.ring - 1) * (2 * this.ring - 1) + (3 * this.ring - x)
      case Point(x, y) if x < 0 && x != y && -x >= y.abs => // WEST
        (2 * this.ring - 1) * (2 * this.ring - 1) + (5 * this.ring + y)
      case Point(x, y) if y > 0 && y != -x && y >= x.abs => // NORTH
        (2 * this.ring - 1) * (2 * this.ring - 1) + (7 * this.ring + x)
    }

  }

  def part2(input: String) = {
    val value = input.toInt
    def loop(point: Point = Point(0, 0),
             known: Map[Int, Int] = Map(1 -> 1)): Int = {
      val current = (for {
        x <- -1 to 1
        y <- -1 to 1 if (x, y) != (0, 0)
      } yield Point(point.x + x, point.y + y)).find(_.n == point.n + 1).get

      val currentValue = (for {
        x <- current.x - 1 to current.x + 1
        y <- current.y - 1 to current.y + 1
        n = Point(x, y).n if n < current.n
      } yield known(n)).sum

      if (currentValue > value) currentValue
      else loop(current, known + (current.n -> currentValue))
    }
    loop()
  }

  val input = io.Source.stdin.getLines.mkString
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
