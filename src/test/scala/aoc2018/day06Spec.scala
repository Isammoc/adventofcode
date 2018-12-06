package aoc2018
import common.AocSpec

class day06Spec extends AocSpec {

  "day06 2018" can {
    val sample = """1, 1
               |1, 6
               |8, 3
               |3, 4
               |5, 5
               |8, 9""".stripMargin
    "part1" should {
      "example" in {
        day06.part1(sample) shouldEqual 17
      }
    }

    "part2" should {
      "example" in {
        day06.part2(sample, 32) shouldEqual 16
      }
    }
  }
}
