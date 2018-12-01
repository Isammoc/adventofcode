package aoc2017
import common.AocSpec

class day11Spec extends AocSpec {

  "day11 2017" can {

    "part1" should {
      "example" in {
        day11.part1("ne,ne,ne") shouldBe 3
        day11.part1("ne,ne,sw,sw") shouldBe 0
        day11.part1("ne,ne,s,s") shouldBe 2
        day11.part1("se,sw,se,sw,sw") shouldBe 3
      }
    }
  }
}
