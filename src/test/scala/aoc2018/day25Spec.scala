package aoc2018
import common.AocSpec

class day25Spec extends AocSpec {

  "day25 2018" can {
    val sample1 = """ 0,0,0,0
                    | 3,0,0,0
                    | 0,3,0,0
                    | 0,0,3,0
                    | 0,0,0,3
                    | 0,0,0,6
                    | 9,0,0,0
                    |12,0,0,0""".stripMargin
    val sample2 = """-1,2,2,0
                    |0,0,2,-2
                    |0,0,0,-2
                    |-1,2,0,0
                    |-2,-2,-2,2
                    |3,0,2,-1
                    |-1,3,2,2
                    |-1,0,-1,0
                    |0,2,1,-2
                    |3,0,0,0""".stripMargin
    val sample3 = """1,-1,0,1
                    |2,0,-1,0
                    |3,2,-1,0
                    |0,0,3,1
                    |0,0,-1,-1
                    |2,3,-2,0
                    |-2,2,0,0
                    |2,-2,0,-1
                    |1,-1,0,-1
                    |3,2,0,2""".stripMargin
    val sample4 = """1,-1,-1,-2
                    |-2,-2,0,1
                    |0,2,1,3
                    |-2,3,-2,1
                    |0,2,3,-2
                    |-1,-1,1,-2
                    |0,-2,-1,0
                    |-2,2,3,-1
                    |1,2,2,0
                    |-1,-2,0,-2""".stripMargin

    "part1" should {
      "example" in {
        day25.part1(sample1) shouldEqual 2
        day25.part1(sample2) shouldEqual 4
        day25.part1(sample3) shouldEqual 3
        day25.part1(sample4) shouldEqual 8
      }
    }
  }
}
