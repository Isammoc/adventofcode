package aoc2015
import common.grid.{Direction, Point}
import common.grid.Direction._

object day03 extends App {

  def parseInput(input: String): List[Direction] =
    input.toList.map {
      case '^' => North
      case 'v' => South
      case '<' => West
      case '>' => East
    }

  def part1(input: String): Int = {
    val start = Point(0,0)
    parseInput(input).foldLeft((start, Set(start))){
      case ((current, visited), dir) => (current.move(dir), visited + current.move(dir))
    }._2.size
  }

  def part2(input: String): Int = {
    val start = Point(0,0)
    parseInput(input).zipWithIndex.foldLeft((start, start, Set(start))){
      case ((santa, robo, visited), (dir, i)) if i % 2 == 0 => (santa.move(dir), robo, visited + santa.move(dir))
      case ((santa, robo, visited), (dir, i)) => (santa, robo.move(dir), visited + robo.move(dir))
    }._3.size
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
