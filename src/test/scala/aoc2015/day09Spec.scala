package aoc2015
import common.AocSpec

class day09Spec extends AocSpec {

  "day09 2015" can {
    val sample = """London to Dublin = 464
                   |London to Belfast = 518
                   |Dublin to Belfast = 141""".stripMargin

    "part1" should {
      "example" in {
        day09.part1(sample) shouldEqual 605
      }
    }

    "part2" should {
      "example" in {
        day09.part2(sample) shouldEqual 982
      }
    }
  }
}

