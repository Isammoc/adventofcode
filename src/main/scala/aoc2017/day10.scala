package aoc2017
import common.util.Repeat

object day10 extends App {

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

  def part1(input: String, ringSize: Int): Int =
    round(
      (0 until ringSize).toList,
      0,
      input.split(",").map(_.trim.toInt).toList,
      0
    ).take(2).product

  def convertInput(input: String): List[Int] = {
    input.toList.map(_.toInt) ++ List(17, 31, 73, 47, 23)
  }

  def round64(l: List[Int]): List[Int] = Repeat(6)(l)(l => l ++ l)

  def part2(input: String): String = {
    val inputLenghts = round64(convertInput(input))
    val output = round((0 until 256).toList, 0, inputLenghts, 0)
    output.grouped(16).map(_.reduce((a, b) => a ^ b).formatted("%02x")).mkString
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input, 256))
  println("part2 = " + part2(input))
}
