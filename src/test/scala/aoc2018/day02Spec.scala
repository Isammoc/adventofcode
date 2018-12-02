package aoc2018
import common.AocSpec

class day02Spec extends AocSpec {

  "day02 2018" can {
    "part1" should {
      "example" in {
        day02.part1(
          List(
            "abcdef",
            "bababc",
            "abbcde",
            "abcccd",
            "aabcdd",
            "abcdee",
            "ababab"
          )
        ) shouldEqual 12
      }
    }

    "part2" should {
      "example" in {
        day02.part2(
          List("abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz")
        ) shouldEqual "fgij"
      }
    }
  }
}
