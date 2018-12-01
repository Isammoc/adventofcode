package aoc2017

object day09 extends App {
  def read(input: String): (Int, Int) = {
    def garbage(input: Stream[Char],
                currentScore: Int,
                currentDeep: Int,
                garbageCount: Int): (Int, Int) = input match {
      case '!' #:: _ #:: t =>
        garbage(t, currentScore, currentDeep, garbageCount)
      case '>' #:: t => group(t, currentScore, currentDeep, garbageCount)
      case _ #:: t   => garbage(t, currentScore, currentDeep, garbageCount + 1)
    }

    def group(input: Stream[Char],
              currentScore: Int,
              currentDeep: Int,
              garbageCount: Int): (Int, Int) = input match {
      case Stream.Empty    => (currentScore, garbageCount)
      case '!' #:: _ #:: t => group(t, currentScore, currentDeep, garbageCount)
      case '{' #:: t       => group(t, currentScore, currentDeep + 1, garbageCount)
      case '}' #:: t =>
        group(t, currentScore + currentDeep, currentDeep - 1, garbageCount)
      case '<' #:: t => garbage(t, currentScore, currentDeep, garbageCount)
      case _ #:: t   => group(t, currentScore, currentDeep, garbageCount)
    }

    group(input.toStream, 0, 0, 0)
  }

  def part1(input: String) = read(input)._1

  def part2(input: String) = read(input)._2

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
