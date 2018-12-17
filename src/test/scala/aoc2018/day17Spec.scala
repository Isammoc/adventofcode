package aoc2018
import common.AocSpec

class day17Spec extends AocSpec {

  "day17 2018" can {
    val sample = """x=495, y=2..7
                   |y=7, x=495..501
                   |x=501, y=3..7
                   |x=498, y=2..4
                   |x=506, y=1..2
                   |x=498, y=10..13
                   |x=504, y=10..13
                   |y=13, x=498..504""".stripMargin

    "part1" should {
      "example" in {
        day17.part1(sample) shouldEqual 57
      }
    }

    "part2" should {
      "example" in {
        day17.part2(sample) shouldEqual 29
      }
    }
  }
}

