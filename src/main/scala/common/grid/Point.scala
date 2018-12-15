package common.grid

import Direction._

case class Point(x: Int, y: Int) extends Ordered[Point] {
  def manhattan(xx: Int, yy: Int): Int = (xx - x).abs + (yy - y).abs
  def manhattan(that: Point): Int = this.manhattan(that.x, that.y)
  import scala.math.Ordered.orderingToOrdered

  override def compare(that: Point): Int =
    (this.y, this.x) compare (that.y, that.x)

  def move(direction: Direction): Point = direction match {
    case North => Point(x, y - 1)
    case South => Point(x, y + 1)
    case East  => Point(x + 1, y)
    case West  => Point(x - 1, y)
  }
}
