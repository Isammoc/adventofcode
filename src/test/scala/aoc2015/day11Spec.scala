package aoc2015
import common.AocSpec

class day11Spec extends AocSpec {

  "day11 2015" can {

    "part1" should {
      "example" in {
        day11.part1("abcdefgh") shouldEqual "abcdffaa"
        day11.part1("ghijklmn") shouldEqual "ghjaabcc"
      }
    }
  }
}

