package aoc2018
import common.AocSpec

class day23Spec extends AocSpec {

  "day23 2018" can {

    "part1" should {
      val sample1 = """pos=<0,0,0>, r=4
                     |pos=<1,0,0>, r=1
                     |pos=<4,0,0>, r=3
                     |pos=<0,2,0>, r=1
                     |pos=<0,5,0>, r=3
                     |pos=<0,0,3>, r=1
                     |pos=<1,1,1>, r=1
                     |pos=<1,1,2>, r=1
                     |pos=<1,3,1>, r=1""".stripMargin
      "example" in {
        day23.part1(sample1) shouldEqual 7
      }
    }

    "part2" should {
      val sample2 = """pos=<10,12,12>, r=2
                      |pos=<12,14,12>, r=2
                      |pos=<16,12,12>, r=4
                      |pos=<14,14,14>, r=6
                      |pos=<50,50,50>, r=200
                      |pos=<10,10,10>, r=5""".stripMargin
      "example" in {
        day23.part2(sample2) shouldEqual 36
      }
    }
  }
}

