package aoc2015
import common.AocSpec

class day02Spec extends AocSpec {

  "day02 2015" can {
    "part1" should {
      "example" in {
        day02.part1("2x3x4") shouldEqual 58
        day02.part1("1x1x10") shouldEqual 43
      }
    }

    "part2" should {
      "example" in {
        day02.part2("2x3x4") shouldEqual 34
        day02.part2("1x1x10") shouldEqual 14
      }
    }
  }
}

