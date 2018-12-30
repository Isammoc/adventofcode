package aoc2015

object day08 extends App {

  def decodeSize(str: String): Int = {
    def loop(remain: List[Char], count: Int): Int = remain match {
      case Nil                        => count
      case '"' :: Nil                 => count
      case '\\' :: '\\' :: t          => loop(t, count + 1)
      case '\\' :: '"' :: t           => loop(t, count + 1)
      case '\\' :: 'x' :: _ :: _ :: t => loop(t, count + 1)
      case _ :: t                     => loop(t, count + 1)
    }
    loop(str.toList.tail, 0)
  }

  def encodeSize(str: String): Int = {
    def loop(remain: List[Char], count: Int): Int = remain match {
      case Nil => count
      case '"'::t => loop(t, count + 2)
      case '\\'::t => loop(t, count + 2)
      case _::t => loop(t, count + 1)
    }
    loop(str.toList, 2)
  }

  def parseInput(input: String): List[String] = input.split("\n").toList
  def part1(input: String) =
    parseInput(input).map { line =>
      line.length - decodeSize(line)
    }.sum

  def part2(input: String) =     parseInput(input).map { line =>
    encodeSize(line) - line.length
  }.sum


  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
