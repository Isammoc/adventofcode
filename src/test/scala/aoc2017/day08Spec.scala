package aoc2017
import common.AocSpec

class day08Spec extends AocSpec {

  "day08 2017" can {

    "part1" should {
      "example" in {
        day08.part1("""b inc 5 if a > 1
                      |a inc 1 if b < 5
                      |c dec -10 if a >= 1
                      |c inc -20 if c == 10""".stripMargin) shouldEqual 1
      }
    }

    "part2" should {
      "example" in {
        day08.part2("""b inc 5 if a > 1
                      |a inc 1 if b < 5
                      |c dec -10 if a >= 1
                      |c inc -20 if c == 10""".stripMargin) shouldEqual 10
      }
    }
  }
}

