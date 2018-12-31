package aoc2015

object day14 extends App {

  case class Reindeer(speed: Int, endurance: Int, rest: Int) {
    def distance(time: Int): Int = {
      val wholeCycles = time / (endurance + rest)
      val remaining = time % (endurance + rest)
      val remainingDistance = if (remaining >= endurance) {
        speed * endurance
      } else {
        speed * remaining
      }
      wholeCycles * speed * endurance + remainingDistance
    }
  }

  def scoring(reinders: List[Reindeer], duration: Int): Map[Reindeer, Int] =
    (for {
      time <- 1 to duration
      max = reinders.map(_.distance(time)).max
      reinder <- reinders.filter(_.distance(time) == max)
    } yield reinder).groupBy(identity).map {
      case (a, b) => a -> b.size
    }

  def parseInput(input: String): List[Reindeer] = {
    val LineReg =
      ".* can fly ([0-9]+) km/s for ([0-9]+) seconds, but then must rest for ([0-9]+) seconds.".r
    input.split("\n").toList.map {
      case LineReg(speed, endurance, rest) =>
        Reindeer(speed.toInt, endurance.toInt, rest.toInt)
    }
  }

  def part1(input: String) = {
    val reindeers = parseInput(input)
    reindeers.map(_.distance(2503)).max
  }

  def part2(input: String) = {
    val reindeers = parseInput(input)
    scoring(reindeers, 2503).values.max
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
