package aoc2018
import common.AocSpec

class day08Spec extends AocSpec {

  "day08 2018" can {
    val sample = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
    "part1" should {
      "example" in {
        day08.part1(sample) shouldEqual 138
      }
    }

    "part2" should {
      "example" in {
        day08.part2(sample) shouldEqual 66
      }
    }
  }
}
