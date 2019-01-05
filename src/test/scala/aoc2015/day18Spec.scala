package aoc2015
import common.AocSpec
import common.util.Repeat

class day18Spec extends AocSpec {
  import day18._

  "day18 2015" can {
    val sample = """.#.#.#
                   |...##.
                   |#....#
                   |..#...
                   |#.#..#
                   |####..""".stripMargin

    "sample1" in {
      Repeat(4)(parseInput(sample))(_.next).countOn shouldEqual 4
    }
    "sample2" in {
      Repeat(5)(parseInput(sample).withCornersOn)(_.next.withCornersOn).countOn shouldEqual 17
    }
  }
}

