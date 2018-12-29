package aoc2015
import common.AocSpec

class day04Spec extends AocSpec {

  "day04 2015" can {
    "part1" should {
      "example" in {
        day04.part1("abcdef") shouldEqual 609043
        day04.part1("pqrstuv") shouldEqual 1048970
      }
    }
  }
}

