package aoc2017
import common.util.Repeat

object day14 extends App {
  def round64(l: List[Int]): List[Int] = Repeat(6)(l)(l => l ++ l)

  def round(currentRing: List[Int],
            currentPlace: Int,
            remain: List[Int],
            skip: Int): List[Int] = remain match {
    case Nil =>
      val to = currentRing.size - (currentPlace % currentRing.size)
      currentRing.drop(to) ++ currentRing.take(to)
    case l :: t =>
      val afterKnot = currentRing.take(l).reverse ++ currentRing.drop(l)
      val to = (l + skip) % currentRing.size
      val replace = afterKnot.drop(to) ++ afterKnot.take(to)
      round(
        replace,
        currentPlace = (currentPlace + to) % currentRing.size,
        t,
        skip + 1
      )
  }

  def knotHash(input: String): String = {
    val inputLenghts = round64(
      input.toList.map(_.toInt) ++ List(17, 31, 73, 47, 23)
    )
    val output = round((0 until 256).toList, 0, inputLenghts, 0)
    output.grouped(16).map(_.reduce((a, b) => a ^ b).formatted("%02x")).mkString
  }

  def part1(input: String): Int = {
    (0 to 127)
      .map(input + '-' + _)
      .map(knotHash)
      .map(BigInt(_, 16))
      .map(_.toString(2).count(_ == '1'))
      .sum
  }

  def part2(input: String): Int = {
    class Connection(size: Int) {
      private var map: Map[Int, Int] = (0 until size).map(i => i -> i).toMap
      def getRoot(p: Int): Int =
        if (map(p) == p) p
        else {
          val r = getRoot(map(p))
          map = map + (p -> r)
          r
        }
      def connect(p1: Int, p2: Int) {
        val r1 = getRoot(p1)
        val r2 = getRoot(p2)
        map = map + (r1 -> r2)
      }
    }

    case class MyPoint(x: Int, y: Int) {
      def toInt: Int = x * 128 + y
      def neigh: List[MyPoint] =
        for {
          (dx, dy) <- List((0, 1), (0, -1), (1, 0), (-1, 0))
          p = MyPoint(x + dx, y + dy)
          if 0 <= p.x && p.x < 128 && 0 <= p.y && p.y < 128
        } yield p
    }

    val map = (0 to 127)
      .map(input + '-' + _)
      .map(knotHash)
      .map(BigInt(_, 16))
      .map(_.toString(2))
      .map(s => "0" * (128 - s.length) + s)
    val conn = new Connection(128 * 128)
    for {
      x <- 0 to 127
      y <- 0 to 127
      p1 = MyPoint(x, y) if map(p1.x)(p1.y) == '1'
      p2 <- p1.neigh if map(p2.x)(p2.y) == '1'
    } {
      conn.connect(p1.toInt, p2.toInt)
    }
    (for {
      x <- 0 to 127
      y <- 0 to 127
      p = MyPoint(x, y) if map(p.x)(p.y) == '1'
    } yield conn.getRoot(p.toInt)).toSet.size
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
