package aoc2015
import common.AocSpec

class day10Spec extends AocSpec {

  "day10 2015" can {
    def next(str: String): String = day10.next(day10.parseInput(str)).mkString
    "next" in {
      next("1") shouldEqual "11"
      next("11") shouldEqual "21"
      next("21") shouldEqual "1211"
      next("1211") shouldEqual "111221"
      next("111221") shouldEqual "312211"
    }
  }
}
