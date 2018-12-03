package aoc2018
import common.AocSpec

class day03Spec extends AocSpec {

  "day03 2018" can {

    val sample = """#1 @ 1,3: 4x4
                   |#2 @ 3,1: 4x4
                   |#3 @ 5,5: 2x2""".stripMargin
    "part1" should {
      "example" in {
        day03.part1(sample) shouldEqual 4
      }
    }

    "part2" should {
      "example" in {
        day03.part2(sample) shouldEqual "3"
      }
    }
  }
}

