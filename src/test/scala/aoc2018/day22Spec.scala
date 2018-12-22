package aoc2018
import aoc2018.day22.Cave
import common.AocSpec
import common.grid.Point

class day22Spec extends AocSpec {

  "day22 2018" can {
    val cave = Cave(510, Point(10, 10))
    "geologicIndex" in {
      cave.geologicIndex(cave.start) shouldEqual 0
      cave.geologicIndex(cave.target) shouldEqual 0
    }

    "erosionLevel" in {
      cave.erosionLevel(cave.start) shouldEqual 510
      cave.erosionLevel(Point(1, 0)) shouldEqual 17317
      cave.erosionLevel(Point(0, 1)) shouldEqual 8415
      cave.erosionLevel(Point(1, 1)) shouldEqual 1805
      cave.erosionLevel(cave.target) shouldEqual 510
    }

    "risk" in {
      val rocky = 0
      val wet = 1
      val narrow = 2
      cave.risk(cave.start) shouldBe rocky
      cave.risk(Point(1, 0)) shouldBe wet
      cave.risk(Point(0, 1)) shouldBe rocky
      cave.risk(Point(1, 1)) shouldBe narrow
      cave.risk(cave.target) shouldBe rocky
    }

    val sample = """depth: 510
                   |target: 10,10""".stripMargin

    "part1" should {
      "example" in {
        day22.part1(sample) shouldEqual 114
      }
    }

    "part2" should {
      "example" in {
        day22.part2(sample) shouldEqual 45
      }
    }
  }
}
