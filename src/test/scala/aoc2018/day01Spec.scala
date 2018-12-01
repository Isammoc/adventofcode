package aoc2018
import common.AocSpec

class day01Spec extends AocSpec {

  "day01 2018" can {

    def sample(str: String) = str.split(", ").toList

    "part1" should {
      "example" in {
        day01.part1(sample("+1, -2, +3, +1")) shouldEqual 3
        day01.part1(sample("+1, +1, +1")) shouldEqual 3
        day01.part1(sample("+1, +1, -2")) shouldEqual 0
        day01.part1(sample("-1, -2, -3")) shouldEqual -6
      }
    }

    "part2" should {
      "example" in {
        day01.part2(sample("+1, -2, +3, +1")) shouldEqual 2
        day01.part2(sample("+1, -1")) shouldEqual 0
        day01.part2(sample("+3, +3, +4, -2, -4")) shouldEqual 10
        day01.part2(sample("-6, +3, +8, +5, -6")) shouldEqual 5
        day01.part2(sample("+7, +7, -2, -7, -4")) shouldEqual 14
      }
    }
  }
}
