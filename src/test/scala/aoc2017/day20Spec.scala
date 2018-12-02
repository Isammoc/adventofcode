package aoc2017
import common.AocSpec

class day20Spec extends AocSpec {

  "day20 2017" can {

    "part1" should {
      "example" in {
        val sample = """p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"""
        day20.part1(sample) shouldBe 0

      }
    }
  }
}
