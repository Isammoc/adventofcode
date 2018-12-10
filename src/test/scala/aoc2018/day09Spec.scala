package aoc2018
import common.AocSpec

class day09Spec extends AocSpec {

  "day09 2018" can {

    "part1" should {
      "example" in {
        day09.part1(9, 25) shouldEqual 32
        day09.part1(10, 1618) shouldEqual 8317
        day09.part1(13, 7999) shouldEqual 146373
        day09.part1(17, 1104) shouldEqual 2764
        day09.part1(21, 6111) shouldEqual 54718
        day09.part1(30, 5807) shouldEqual 37305
      }
    }
  }
}
