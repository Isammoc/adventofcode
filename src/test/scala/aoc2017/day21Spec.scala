package aoc2017
import common.AocSpec

class day21Spec extends AocSpec {

  "day21 2017" can {
    val initial = """.#.
                    |..#
                    |###""".stripMargin
    val sample = """../.# => ##./#../...
                   |.#./..#/### => #..#/..../..../#..#""".stripMargin

    "third rule" in {
      day21.applyRule3(initial.split("\n").toList, day21.inputToRules(sample)) shouldBe """#..#
                                                                                           |....
                                                                                           |....
                                                                                           |#..#""".stripMargin
    }

    "part1" should {
      "example" in {
        day21.part1(sample, 2) shouldBe 12
      }
    }
  }
}
