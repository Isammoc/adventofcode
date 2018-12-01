package aoc2017
import common.AocSpec

class day15Spec extends AocSpec {

  "day15 2017" can {
    val sampleA = 65
    val sampleB = 8921

    "part1" should {
      "example" in {
        day15.part1(sampleA, sampleB) shouldBe 588
      }
    }

    "part2" should {
      "example" in {
        day15.part2(sampleA, sampleB) shouldBe 309
      }
    }
  }
}
