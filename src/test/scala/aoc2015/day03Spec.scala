package aoc2015
import common.AocSpec

class day03Spec extends AocSpec {

  "day03 2015" can {
    "part1" should {
      "example" in {
        day03.part1(">") shouldEqual 2
        day03.part1("^>v<") shouldEqual 4
        day03.part1("^v^v^v^v^v") shouldEqual 2

      }
    }

    "part2" should {
      "example" in {
        day03.part2("^v") shouldEqual 3
        day03.part2("^>v<") shouldEqual 3
        day03.part2("^v^v^v^v^v") shouldEqual 11
      }
    }
  }
}

