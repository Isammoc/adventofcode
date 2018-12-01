package aoc2017
import common.AocSpec

class day09Spec extends AocSpec {

  "day09 2017" can {

    "part1" should {
      "example" in {
        day09.part1("""{}""".stripMargin) shouldEqual 1
        day09.part1("""{{{}}}""".stripMargin) shouldEqual 6
        day09.part1("""{{},{}}""".stripMargin) shouldEqual 5
        day09.part1("""{{{},{},{{}}}}""".stripMargin) shouldEqual 16
        day09.part1("""{<a>,<a>,<a>,<a>}""".stripMargin) shouldEqual 1
        day09.part1("""{{<ab>},{<ab>},{<ab>},{<ab>}}""".stripMargin) shouldEqual 9
        day09.part1("""{{<!!>},{<!!>},{<!!>},{<!!>}}""".stripMargin) shouldEqual 9
        day09.part1("""{{<a!>},{<a!>},{<a!>},{<ab>}}""".stripMargin) shouldEqual 3
      }
    }

    "part2" should {
      "example" in {
        day09.part2("""<>""".stripMargin) shouldEqual 0
        day09.part2("""<random characters>""".stripMargin) shouldEqual 17
        day09.part2("""<<<<>""".stripMargin) shouldEqual 3
        day09.part2("""<{!>}>""".stripMargin) shouldEqual 2
        day09.part2("""<!!>""".stripMargin) shouldEqual 0
        day09.part2("""<!!!>>""".stripMargin) shouldEqual 0
        day09.part2("""<{o"i!a,<{i<a>""".stripMargin) shouldEqual 10
      }
    }
  }
}

