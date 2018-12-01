package aoc2017
import common.AocSpec

class day03Spec extends AocSpec {

  "day03 2017" can {

    "part1" should {
      "example" in {
        day03.part1("1") shouldEqual 0
        day03.part1("12") shouldEqual 3
        day03.part1("23") shouldEqual 2
        day03.part1("1024") shouldEqual 31
      }
    }
  }
}

