package aoc2017
import common.AocSpec

class day22Spec extends AocSpec {

  "day22 2017" can {
    val sample = """..#
                |#..
                |...""".stripMargin

    "part1" should {
      "example" in {
        day22.part1(sample, 7) shouldBe 5
      }
    }

    "part2" should {
      "example" in {
        day22.part2(sample, 100) shouldBe 26
      }
    }
  }
}
