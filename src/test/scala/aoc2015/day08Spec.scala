package aoc2015
import common.AocSpec

class day08Spec extends AocSpec {

  "day08 2015" can {
    val sample =
      """""
        |"abc"
        |"aaa\"aaa"
        |"\x27"""".stripMargin

    "part1" should {
      "example" in {
        day08.part1(sample) shouldEqual 12
      }
    }

    "part2" should {
      "example" in {
        day08.part2(sample) shouldEqual 19
      }
    }
  }
}

