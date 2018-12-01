package aoc2017
import common.AocSpec

class day06Spec extends AocSpec {

  "day06 2017" can {

    "part1" should {
      "example" in {
        day06.part1("0 2 7 0") shouldEqual 5
      }
    }

    "part2" should {
      "example" in {
        day06.part2("0 2 7 0") shouldEqual 4
      }
    }
  }
}

