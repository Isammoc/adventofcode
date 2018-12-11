package aoc2018
import common.AocSpec

class day11Spec extends AocSpec {

  "day11 2018" can {

    "power" should {
      "example" in {
        day11.power(8)(3, 5) shouldEqual 4
        day11.power(57)(122, 79) shouldEqual -5
        day11.power(39)(217, 196) shouldEqual 0
        day11.power(71)(101, 153) shouldEqual 4
      }
    }

    "part1" should {
      "example" in {
        day11.part1(18) shouldEqual "33,45"
        day11.part1(42) shouldEqual "21,61"
      }
    }

    "part2" should {
      "example" in {
        day11.part2(18) shouldEqual "90,269,16"
        day11.part2(42) shouldEqual "232,251,12"
      }
    }
  }
}
