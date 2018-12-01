package aoc2017
import common.AocSpec

class day12Spec extends AocSpec {

  "day12 2017" can {
    val input = """0 <-> 2
                  |1 <-> 1
                  |2 <-> 0, 3, 4
                  |3 <-> 2, 4
                  |4 <-> 2, 3, 6
                  |5 <-> 6
                  |6 <-> 4, 5""".stripMargin

    "part1" should {
      "example" in {
        day12.part1(input) shouldBe 6
      }
    }

    "part2" should {
      "example" in {
        day12.part2(input) shouldBe 2
      }
    }
  }
}
