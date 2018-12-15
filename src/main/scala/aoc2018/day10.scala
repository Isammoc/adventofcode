package aoc2018

object day10 extends App {

  case class Particle(x: Int, y: Int, vx: Int, vy: Int) {
    def current(s: Int): (Int, Int) = (x + s * vx, y + s * vy)
  }

  def message(input: String): (String, Int) = {
    val LineReg =
      "position=<\\s*(-?[0-9]+),\\s*(-?[0-9]+)> velocity=<\\s*(-?[0-9]+),\\s*(-?[0-9]+)>".r
    val points = input.split("\n").toList.map {
      case LineReg(x, y, vx, vy) => Particle(x.toInt, y.toInt, vx.toInt, vy.toInt)
    }

    def loop(current: Int, lastRange: Int): (String, Int) = {
      val currents = points.map(_.current(current)).toSet

      val minx = currents.map(_._1).min
      val maxx = currents.map(_._1).max
      val miny = currents.map(_._2).min
      val maxy = currents.map(_._2).max

      val currentValue = maxx + maxy - minx - miny

      if (currentValue > lastRange) {
        val result = points.map(_.current(current - 1)).toSet
        val msg =
          (for (y <- (result.map(_._2).min - 1) to (result.map(_._2).max + 1))
            yield
              (for (x <- (result
                      .map(_._1)
                      .min - 1) to (result.map(_._1).max + 1))
                yield if (result.contains((x, y))) '#' else '.').mkString)
            .mkString("\n")
        (msg, current - 1)
      } else {
        loop(current + 1, currentValue)
      }
    }

    loop(0, Int.MaxValue)
  }

  def part1(input: String): String = message(input)._1

  def part2(input: String): Int = message(input)._2

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
