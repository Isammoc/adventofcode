package aoc2015
import common.AocSpec

class day07Spec extends AocSpec {

  "day07 2015" can {
    val sample = """123 -> x
                   |456 -> y
                   |x AND y -> d
                   |x OR y -> e
                   |x LSHIFT 2 -> f
                   |y RSHIFT 2 -> g
                   |NOT x -> h
                   |NOT y -> i""".stripMargin

    "wire" in {
      val wire = day07.parseInput(sample)
      wire.value("d") shouldEqual 72
      wire.value("e") shouldEqual 507
      wire.value("f") shouldEqual 492
      wire.value("g") shouldEqual 114
      wire.value("h") shouldEqual 65412
      wire.value("i") shouldEqual 65079
      wire.value("x") shouldEqual 123
      wire.value("y") shouldEqual 456
    }

  }
}
