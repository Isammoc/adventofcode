package aoc2018
import common.AocSpec

class day05Spec extends AocSpec {

  "day05 2018" can {
    val sample = "dabAcCaCBAcCcaDA"

    "part1" should {
      "example" in {
        day05.part1(sample) shouldEqual 10
      }
    }

    "part2" should {
      "example" in {
        day05.part2(sample) shouldEqual 4
      }
    }
  }
}
