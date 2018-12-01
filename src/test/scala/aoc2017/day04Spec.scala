package aoc2017
import common.AocSpec

class day04Spec extends AocSpec {

  "day04 2017" can {

    "part1" should {
      "example" in {
        day04.part1("""aa bb cc dd ee
            |aa bb cc dd aa
            |aa bb cc dd aaa
          |""".stripMargin) shouldEqual 2
      }
    }

    "part2" should {
      "example" in {
        day04.part2(
          """abcde fghij
            |abcde xyz ecdab
            |a ab abc abd abf abj
            |iiii oiii ooii oooi oooo
            |oiii ioii iioi iiio
            |""".stripMargin) shouldEqual 3
      }
    }
  }
}
