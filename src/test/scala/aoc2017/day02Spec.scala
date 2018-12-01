package aoc2017
import common.AocSpec

class day02Spec extends AocSpec {
    
  "day02 2017" can {

    "part1" should {
      "example" in {
        day02.part1("""5 1 9 5
                      |7 5 3
                      |2 4 6 8""".stripMargin) shouldEqual 18
      }
    }

    "part2" should {
      "example" in {
        day02.part2("""5 9 2 8
                      |9 4 7 3
                      |3 8 6 5""".stripMargin) shouldEqual 9
      }
    }
  }
}
