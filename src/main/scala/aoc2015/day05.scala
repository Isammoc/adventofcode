package aoc2015

object day05 extends App {

  def threeVowels(s: String): Boolean = {
    val vowels = Set('a', 'e', 'i', 'o', 'u')
    s.count(vowels.contains) >= 3
  }

  def twiceInARow(s: String): Boolean =
    s.zip(s.tail).exists {
      case (a, b) => a == b
    }

  def forbiddenStringsFree(s: String): Boolean = {
    val forbidden = Set(('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y'))
    !s.zip(s.tail).exists(forbidden.contains)
  }

  def isNice1(s: String): Boolean =
    threeVowels(s) && twiceInARow(s) && forbiddenStringsFree(s)

  def part1(input: String) = input.split("\n").count(isNice1)

  def containsPair(s: String): Boolean = {
    val pairs = s.zip(s.tail).toSet
    pairs.map(p => s.zip(s.tail).count(_ == p)).exists(_ >= 2)
  }

  def oneLetterAround(s: String): Boolean =
    s.zip(s.tail).zip(s.tail.tail).exists {
      case ((a, b), c) => a == c
    }

  def isNice2(s: String): Boolean = containsPair(s) && oneLetterAround(s)

  def part2(input: String) = input.split("\n").count(isNice2)

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
