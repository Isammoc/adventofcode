package aoc2017

object day07 extends App {

  def part1(input: String) = {
    val LineReg = """([a-z]+) \(([0-9]+)\)(?: -> (.*))?""".r

    val toBottom = input
      .split("\n")
      .toList
      .flatMap {
        case LineReg(name, weight, top) if top != null =>
          top.split(", ").toList.map { s => (s -> name)
          }
        case _ => Nil
      }
      .toMap

    def loop(current: String): String =
      if (toBottom.isDefinedAt(current)) loop(toBottom(current)) else current

    loop(toBottom.keys.head)
  }

  def part2(input: String) = {
    val LineReg = """([a-z]+) \(([0-9]+)\)(?: -> (.*))?""".r
    val toBottom = input
      .split("\n")
      .toList
      .flatMap {
        case LineReg(name, weight, top) if top != null =>
          top.split(", ").toList.map { s => (s -> name)
          }
        case _ => Nil
      }
      .toMap

    val fromBottom = input
      .split("\n")
      .toList
      .map {
        case LineReg(name, weight, top) if top != null =>
          (name -> top.split(", ").toList)
        case LineReg(name, _*) => (name -> Nil)
      }
      .toMap

    val weights = input
      .split("\n")
      .toList
      .map {
        case LineReg(name, w, _*) => (name -> w.toInt)
      }
      .toMap

    def weight(name: String): Int =
      weights(name) + fromBottom(name).map(weight).sum

    def isBalanced(name: String): Boolean = {
      val upon = fromBottom(name).map(weight)
      if (upon.isEmpty) true
      else
        upon.forall(_ == upon(0))
    }

    def unbalanced(current: String): (String, String) =
      if (isBalanced(current))
        (toBottom(current), current)
      else {
        unbalanced(
          fromBottom(current)
            .groupBy(weight)
            .filter(_._2.size == 1)
            .head
            ._2
            .head
        )
      }

    val (base, toChange) = unbalanced(part1(input))

    val wanted = fromBottom(base).groupBy(weight).filter(_._2.size != 1).head._1

    wanted - fromBottom(toChange).map(weight).sum

  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
