package aoc2017
import common.AocSpec

class day14Spec extends AocSpec {

  "day14 2017" can {
  val sample = "flqrgnkx"

    "part1" should {
      "example" in {
            day14.part1(sample) shouldBe 8108
      }
    }

    "part2" should {
      "example" in {
            day14.part2(sample) shouldBe 1242
      }
    }
  }
}

