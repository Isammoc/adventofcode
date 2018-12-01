package aoc2017
import common.AocSpec

class day05Spec extends AocSpec {

  "day05 2017" can {

    "part1" should {
      "example" in {
        day05.part1("""0
                      |3
                      |0
                      |1
                      |-3""".stripMargin) shouldEqual 5
      }
    }

    "part2" should {
      "example" in {
        day05.part2("""0
                      |3
                      |0
                      |1
                      |-3""".stripMargin) shouldEqual 10
      }
    }
  }
}

