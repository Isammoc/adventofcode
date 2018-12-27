package aoc2015
import common.AocSpec

class day01Spec extends AocSpec {

  "day01 2015" can {
    val sample = """""".stripMargin

    "part1" should {
      "example" in {
        day01.part1("(())") shouldEqual 0
        day01.part1("()()") shouldEqual 0
        day01.part1("(((") shouldEqual 3
        day01.part1("(()(()(") shouldEqual 3
        day01.part1("))(((((") shouldEqual 3
        day01.part1("())") shouldEqual -1
        day01.part1("))(") shouldEqual -1
        day01.part1(")))") shouldEqual -3
        day01.part1(")())())") shouldEqual -3
      }
    }

    "part2" should {
      "example" in {
        day01.part2(")") shouldEqual 1
        day01.part2("()())") shouldEqual 5
      }
    }
  }
}

