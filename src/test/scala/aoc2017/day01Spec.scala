package aoc2017
import common.AocSpec

class day01Spec extends AocSpec {

  "day01 2017" can {

    "part1" should {
      "example" in {
        day01.part1("1122") shouldEqual 3
        day01.part1("1111") shouldEqual 4
        day01.part1("1234") shouldEqual 0
        day01.part1("91212129") shouldEqual 9
      }
    }

    "part2" should {
      "example" in {
        day01.part2("1212") shouldEqual 6
        day01.part2("1221") shouldEqual 0
        day01.part2("123425") shouldEqual 4
        day01.part2("123123") shouldEqual 12
        day01.part2("12131415") shouldEqual 4
      }
    }
  }
}
