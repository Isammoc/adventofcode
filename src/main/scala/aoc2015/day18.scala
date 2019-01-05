package aoc2015
import common.grid.Point
import common.util.Repeat

object day18 extends App {

  case class Grid(width: Int, height: Int, on: Set[Point]) {
    def countOn: Int = on.size

    def withCornersOn: Grid =
      Grid(
        width,
        height,
        on + Point(0, 0) + Point(0, height - 1) + Point(width - 1, height - 1) + Point(
          width - 1,
          0
        )
      )

    def neighborCount(p: Point): Int =
      (for {
        x <- -1 to 1
        y <- -1 to 1
        if x != 0 || y != 0
        n = Point(p.x + x, p.y + y)
        if on.contains(n)
      } yield n).size

    def next: Grid = {
      val nextOn = (for {
        x <- 0 until width
        y <- 0 until height
      } yield Point(x, y)).filter { p =>
        (on.contains(p), neighborCount(p)) match {
          case (true, 2)  => true
          case (true, 3)  => true
          case (false, 3) => true
          case _          => false
        }
      }.toSet
      Grid(this.width, this.height, nextOn)
    }
  }

  def parseInput(input: String): Grid = {
    def lines = input.split("\n")
    val on = for {
      (line, y) <- lines.zipWithIndex
      (c, x) <- line.zipWithIndex if c == '#'
    } yield Point(x, y)

    Grid(lines(0).length, lines.length, on.toSet)
  }

  def part1(input: String) = Repeat(100)(parseInput(input))(_.next).countOn

  def part2(input: String) =
    Repeat(100)(parseInput(input).withCornersOn)(_.next.withCornersOn).countOn

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
