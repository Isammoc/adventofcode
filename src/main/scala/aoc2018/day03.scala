package aoc2018
import common.grid.Point

object day03 extends App {

  case class Rect(id: String, x: Int, y: Int, w: Int, h: Int)

  def part1(input: String): Int = {
    val LineReg = "#(.+) @ (.+),(.+): (.+)x(.+)".r
    val rects = input.split("\n").toList.map {
      case LineReg(id, x, y, w, h) =>
        Rect(id, x.toInt, y.toInt, w.toInt, h.toInt)
    }
    val points = for {
      rect <- rects
      x <- rect.x until (rect.x + rect.w)
      y <- rect.y until (rect.y + rect.h)
    } yield Point(x, y)
    points.groupBy(identity).count(_._2.size >= 2)
  }

  def part2(input: String) = {
    val LineReg = "#(.+) @ (.+),(.+): (.+)x(.+)".r
    val rects = input.split("\n").toList.map {
      case LineReg(id, x, y, w, h) =>
        Rect(id, x.toInt, y.toInt, w.toInt, h.toInt)
    }
    val points = for {
      rect <- rects
      x <- rect.x until (rect.x + rect.w)
      y <- rect.y until (rect.y + rect.h)
    } yield Point(x, y)

    val used = points.groupBy(identity)
    rects
      .find { rect =>
        val points = for {
          x <- rect.x until (rect.x + rect.w)
          y <- rect.y until (rect.y + rect.h)
        } yield Point(x, y)
        points.forall { used(_).size == 1 }
      }
      .get
      .id
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
