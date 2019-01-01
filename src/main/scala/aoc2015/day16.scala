package aoc2015

object day16 extends App {

  case class Sue(id: Int, infos: Map[String, Int])
  def parseInput(input: String): List[Sue] = {
    val LineReg = "Sue ([0-9]+): (.*)".r
    input.split("\n").toList.map {
      case LineReg(id, infos) =>
        Sue(
          id.toInt,
          infos
            .split(", ")
            .map(_.split(": "))
            .map {
              case Array(info, count) => info -> count.toInt
            }
            .toMap
        )
    }
  }

  def part1(input: String) = {
    val clues = Map(
      "children" -> 3,
      "cats" -> 7,
      "samoyeds" -> 2,
      "pomeranians" -> 3,
      "akitas" -> 0,
      "vizslas" -> 0,
      "goldfish" -> 5,
      "trees" -> 3,
      "cars" -> 2,
      "perfumes" -> 1
    )
    val sues = parseInput(input)
    sues
      .filter {
        _.infos.forall {
          case (name, value) => clues.get(name).contains(value)
        }
      }
      .head
      .id
  }

  def part2(input: String) = {
    val clues = Map.apply[String, Int => Boolean](
      "children" -> (_ == 3),
      "cats" -> (_ > 7),
      "samoyeds" -> (_ == 2),
      "pomeranians" -> (_ < 3),
      "akitas" -> (_ == 0),
      "vizslas" -> (_ == 0),
      "goldfish" -> (_ < 5),
      "trees" -> (_ > 3),
      "cars" -> (_ == 2),
      "perfumes" -> (_ == 1)
    )
    val sues = parseInput(input)
    sues
      .filter {
        _.infos.forall {
          case (name, value) => clues.get(name).exists(_.apply(value))
        }
      }
      .head
      .id
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
