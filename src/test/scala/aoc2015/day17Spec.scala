package aoc2015
import common.AocSpec

class day17Spec extends AocSpec {

  "day17 2015" can {
    "ways" in {
      day17.ways(List(20, 15, 10, 5, 5), 25).size shouldEqual 4
    }
  }
}

