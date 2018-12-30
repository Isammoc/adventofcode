package aoc2015
import common.AocSpec

class day12Spec extends AocSpec {

  "day12 2015" can {
    "part1" should {
      "example" in {
        day12.part1("""[1,2,3]""") shouldEqual 6
        day12.part1("""{"a":2,"b":4}""") shouldEqual 6
        day12.part1("""[[[3]]]""") shouldEqual 3
        day12.part1("""{"a":{"b":4},"c":-1}""") shouldEqual 3
        day12.part1("""{"a":[-1,1]}""") shouldEqual 0
        day12.part1("""[-1,{"a":1}]""") shouldEqual 0
        day12.part1("""[]""") shouldEqual 0
        day12.part1("""{}""") shouldEqual 0
      }
    }

    "part2" should {
      "example" in {
        day12.part2("""[1,2,3]""") shouldEqual 6
        day12.part2("""[1,{"c":"red","b":2},3]""") shouldEqual 4
        day12.part2("""{"d":"red","e":[1,2,3,4],"f":5}""") shouldEqual 0
        day12.part2("""[1,"red",5]""") shouldEqual 6
      }
    }
  }
}

