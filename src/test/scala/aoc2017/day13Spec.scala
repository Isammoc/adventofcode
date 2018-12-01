package aoc2017
import common.AocSpec

class day13Spec extends AocSpec {

  "day13 2017" can {
    val sample = """0: 3
                |1: 2
                |4: 4
                |6: 4""".stripMargin

    "part1" should {
      "example" in {
        day13.part1(sample) shouldBe 24
      }
    }

    "part2" should {
      "example" in {
        day13.part2(sample) shouldBe 10
      }
    }
  }
}
