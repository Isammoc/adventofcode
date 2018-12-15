package aoc2018
import common.grid.Point

object day06 extends App {

  def parsePoint(str: String): Point = {
      val LineReg = "\\s*(\\d+),\\s*(\\d+)\\s*".r
      str match {
        case LineReg(x, y) => Point(x.toInt, y.toInt)
      }
    }

  def part1(input: String) = {
    val points = input.split("\n").toList.map(parsePoint)
    val minx = points.map(_.x).min - 3
    val maxx = points.map(_.x).max + 3
    val miny = points.map(_.y).min - 3
    val maxy = points.map(_.y).max + 3
    val map = (for {
      x <- minx to maxx
      y <- miny to maxy
    } yield {
      points.sortBy(_.manhattan(x, y)) match {
        case p1 :: p2 :: _ if p1.manhattan(x, y) == p2.manhattan(x, y) =>
          (x, y) -> None
        case p :: _ => (x, y) -> Some(p)
      }
    }).toMap

    val up = for {
      x <- minx to maxx
      y = miny
      optPoint = map((x, y)) if optPoint.isDefined
    } yield optPoint.get
    val down = for {
      x <- minx to maxx
      y = maxy
      optPoint = map((x, y)) if optPoint.isDefined
    } yield optPoint.get
    val left = for {
      y <- miny to maxy
      x = minx
      optPoint = map((x, y)) if optPoint.isDefined
    } yield optPoint.get
    val right = for {
      y <- miny to maxy
      x = maxx
      optPoint = map((x, y)) if optPoint.isDefined
    } yield optPoint.get

    val infinites = List(up, down, left, right).flatten.toSet

    map.toList
      .filter(_._2.isDefined)
      .map {
        case (_, Some(p)) => p
      }
      .filterNot(infinites.contains)
      .groupBy(identity)
      .map(_._2.size)
      .max
  }

  def part2(input: String, threshold: Int = 10000) = {
    val points = input.split("\n").toList.map(parsePoint)
    val minx = points.map(_.x).min - 3
    val maxx = points.map(_.x).max + 3
    val miny = points.map(_.y).min - 3
    val maxy = points.map(_.y).max + 3
    (for {
      x <- minx to maxx
      y <- miny to maxy
    } yield {
      points.map(_.manhattan(x, y)).sum
    }).count(_ < threshold)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
