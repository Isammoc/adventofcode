package aoc2017
import common.AocSpec

class day19Spec extends AocSpec {

  "day19 2017" can {
    val sample = List(
      "     |          ",
      "     |  +--+    ",
      "     A  |  C    ",
      " F---|----E|--+ ",
      "     |  |  |  D ",
      "     +B-+  +--+ "
    ).mkString("\n")

    "part1" should {
      "example" in {
        day19.part1(sample) shouldBe "ABCDEF"
      }
    }

    "part2" should {
      "example" in {
        day19.part2(sample) shouldBe 38
      }
    }
  }
}
