package aoc2015

object day13 extends App {

  def parseInput(input: String): Map[String, Map[String, Int]] = {
    val LineReg =
      "(.*) would (.*) ([0-9]+) happiness units by sitting next to (.*).".r
    input.split("\n").toList

    def loop(
      remain: List[String] = input.split("\n").toList,
      res: Map[String, Map[String, Int]] = Map.empty
    ): Map[String, Map[String, Int]] = remain match {
      case Nil => res
      case LineReg(a, verb, amount, b) :: t =>
        loop(
          t,
          res + (a -> (res
            .getOrElse(a, Map.empty) + (b -> (amount.toInt * (if (verb == "lose")
                                                                -1
                                                              else 1)))))
        )
    }

    loop()
  }
  def part1(input: String) = {
    val amounts = parseInput(input)
    amounts.keySet.toList.permutations.map { list =>
      list.zip(list.last :: list).map{
        case (a,b) => amounts(a)(b) + amounts(b)(a)
      }.sum
    }.max
  }

  def part2(input: String) = {
    val amounts = parseInput(input)
    amounts.keySet.toList.permutations.map { list =>
      list.zip(list.tail).map{
        case (a,b) => amounts(a)(b) + amounts(b)(a)
      }.sum
    }.max
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
