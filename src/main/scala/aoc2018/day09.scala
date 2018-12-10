package aoc2018
import java.util
import java.util.stream.Collectors
import scala.collection.JavaConverters._

object day09 extends App {

  implicit class CircleDeque[T](list: util.ArrayDeque[T]) {
    def rotate(num: Int): util.ArrayDeque[T] = {
      if (num == 0) {
        list
      } else if (num > 0) {
        for { _ <- 0 until num } {
          val t: T = list.removeLast()
          list.addFirst(t)
        }
        list
      } else {
        for { _ <- 0 until -num } {
          val t: T = list.removeFirst()
          list.addLast(t)
        }
        list
      }
    }
  }

  def part1(players: Int, last: Int): Long = {
    val circle = new util.ArrayDeque[Int]()
    circle.addFirst(0)
    var scores = Map.empty[Int, Long]

    for (i <- 1 to last) {
      if (i % 23 == 0) {
        circle.rotate(-6)
        val newScore: Long = scores
          .getOrElse(i % players, 0L)
          .toLong + i.toLong + circle
          .pop()
          .toLong
        scores = scores + ((i % players) -> newScore)
      } else {
        circle.rotate(2)
        circle.addLast(i)
      }
    }
    scores.values.max
  }

  def part2(players: Int, last: Int): Long = part1(players, last * 100)

  val input = io.Source.stdin.getLines.mkString("\n")
  val LineReg = "(.+) players; last marble is worth (.+) points".r
  input match {
    case LineReg(players, last) =>
      println("part1 = " + part1(players.toInt, last.toInt))
      println("part2 = " + part2(players.toInt, last.toInt))
  }
}
