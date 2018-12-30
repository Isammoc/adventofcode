package aoc2015
import scala.util.parsing.json._


object day12 extends App {

  def part1(input: String) = {
    def loop(toVisit: List[Any], res: Int): Int = toVisit match {
      case Nil => res
      case (_:Boolean)::t => loop(t, res)
      case (_:String)::t => loop(t, res)
      case (d:Double)::t => loop(t, res + d.toInt)
      case (xs:List[Any])::t => loop(xs ::: t, res)
      case (xs:Map[Any, Any])::t => loop(xs.values.toList ::: t, res)
    }
    loop(JSON.parseFull(input).get::Nil, 0)
  }
1
  def part2(input: String) = {
    def loop(toVisit: List[Any], res: Int): Int = toVisit match {
      case Nil => res
      case (_:Boolean)::t => loop(t, res)
      case (_:String)::t => loop(t, res)
      case (d:Double)::t => loop(t, res + d.toInt)
      case (xs:List[Any])::t => loop(xs ::: t, res)
      case (xs:Map[Any, Any])::t if xs.values.exists(_ == "red") => loop(t, res)
      case (xs:Map[Any, Any])::t => loop(xs.values.toList ::: t, res)
    }
    loop(JSON.parseFull(input).get::Nil, 0)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
