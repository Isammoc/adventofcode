package aoc2018

object day25 extends App {

  case class Point4(x: Int, y: Int, z: Int, t: Int) {
    def manhattan(that: Point4): Int =
      (this.x - that.x).abs + (this.y - that.y).abs + (this.z - that.z).abs + (this.t - that.t).abs
  }
  def parseInput(input: String): List[Point4] = {
    val LineReg =
      "\\s*(-?[0-9]+)\\s*,\\s*(-?[0-9]+)\\s*,\\s*(-?[0-9]+)\\s*,\\s*(-?[0-9]+)\\s*".r
    input.split("\n").toList.map {
      case LineReg(x, y, z, t) => Point4(x.toInt, y.toInt, z.toInt, t.toInt)
    }
  }

  class DisjointSet(n: Int) {
    private val array: Array[Int] = (0 until n).toArray
    def connect(a: Int, b: Int): Unit = {
      array(root(a)) = array(root(b))
    }
    def root(a: Int): Int =
      if (array(a) == a) {
        a
      } else {
        array(a) = root(array(a))
        array(a)
      }
    def size: Int = array.map(root).toSet.size
  }

  def part1(input: String): Int = {
    val points = parseInput(input).toArray

    val disjointSet = new DisjointSet(points.length)
    for {
      a <- points.indices
      b <- (a + 1) until points.length
      if points(a).manhattan(points(b)) <= 3
    } {
      disjointSet.connect(a, b)
    }
    disjointSet.size
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
}
