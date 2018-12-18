package aoc2018
import common.AocSpec

class day18Spec extends AocSpec {

  "day18 2018" can {
    val sample = """.#.#...|#.
                   |.....#|##|
                   |.|..|...#.
                   |..|#.....#
                   |#.#|||#|#|
                   |...#.||...
                   |.|....|...
                   |||...#|.#|
                   ||.||||..|.
                   |...#.|..|.""".stripMargin

    "part1" should {
      "example" in {
        day18.part1(sample) shouldEqual 1147
      }
    }
  }
}

