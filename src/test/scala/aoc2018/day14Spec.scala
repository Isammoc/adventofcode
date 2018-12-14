package aoc2018
import common.AocSpec

class day14Spec extends AocSpec {

  "day14 2018" can {

    "part1" should {
      "example" in {
        day14.part1(9) shouldEqual "5158916779"
        day14.part1(5) shouldEqual "0124515891"
        day14.part1(18) shouldEqual "9251071085"
        day14.part1(2018) shouldEqual "5941429882"
      }
    }

    "part2" should {
      "example" in {
        day14.part2("51589") shouldEqual 9
        day14.part2("01245") shouldEqual 5
        day14.part2("92510") shouldEqual 18
        day14.part2("59414") shouldEqual 2018
      }
    }
  }
}

