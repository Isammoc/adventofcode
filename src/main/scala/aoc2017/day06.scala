package aoc2017

object day06 extends App {

  def part1(input: String) = {
    def mainLoop(current: Array[Int],
                 count: Int = 0,
                 known: Set[List[Int]] = Set.empty): Int =
      if (known.contains(current.toList)) {
        count
      } else {
        val nowKnown = known + current.toList
        val index = current.zipWithIndex.maxBy(_._1)._2
        val remain = current(index)
        current(index) = 0
        def loop(index: Int = index, remain: Int = remain) {
          val currentIndex = (index + 1) % current.length
          current(currentIndex) = current(currentIndex) + 1
          if (remain > 1) {
            loop(currentIndex, remain - 1)
          }
        }
        loop()
        mainLoop(current, count + 1, nowKnown)
      }
    mainLoop(input.split("\\s+").map(_.toInt))
  }

  def part2(input: String) = {
    def mainLoop(current: Array[Int],
                 count: Int = 0,
                 known: Set[List[Int]] = Set.empty): (Int, List[Int]) =
      if (known.contains(current.toList)) {
        (count, current.toList)
      } else {
        val nowKnown = known + current.toList
        val index = current.zipWithIndex.maxBy(_._1)._2
        val remain = current(index)
        current(index) = 0
        def loop(index: Int = index, remain: Int = remain) {
          val currentIndex = (index + 1) % current.length
          current(currentIndex) = current(currentIndex) + 1
          if (remain > 1) {
            loop(currentIndex, remain - 1)
          }
        }
        loop()
        mainLoop(current, count + 1, nowKnown)
      }
    val (_, loopStart) = mainLoop(input.split("\\s+").map(_.toInt))

    mainLoop(loopStart.toArray)._1
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
