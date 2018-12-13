package aoc2018
import common.AocSpec

class day13Spec extends AocSpec {

  "day13 2018" can {
    "part1" should {
      val sample = """/->-\        
                     ||   |  /----\
                     || /-+--+-\  |
                     || | |  | v  |
                     |\-+-/  \-+--/
                     |  \------/   """.stripMargin
      "example" in {
        day13.part1(sample) shouldEqual "7,3"
      }
    }

    "part2" should {
      val sample = """/>-<\
                     ||   |
                     || /<+-\
                     || | | v
                     |\>+</ |
                     |  |   ^
                     |  \<->/""".stripMargin
      "example" in {
        day13.part2(sample) shouldEqual "6,4"
      }
    }
  }
}
