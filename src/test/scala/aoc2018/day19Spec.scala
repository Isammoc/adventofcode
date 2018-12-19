package aoc2018

import common.AocSpec

class day19Spec extends AocSpec {

  "day19 2018" can {
    val sample = """#ip 0
                   |seti 5 0 1
                   |seti 6 0 2
                   |addi 0 1 0
                   |addr 1 2 3
                   |setr 1 0 0
                   |seti 8 0 4
                   |seti 9 0 5""".stripMargin

    "part1" should {
      "example" in {
        day19.part1(sample) shouldEqual 7
      }
    }
  }
}

