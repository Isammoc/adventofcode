package aoc2018
import aoc2018.day16.{AddI, MulR, SetI}
import common.AocSpec

class day16Spec extends AocSpec {

  "day16 2018" can {
    "MulR" in {
      MulR(2, 1, 2)(Map(0 -> 3, 1 -> 2, 2 -> 1, 3 -> 1)) shouldEqual Map(
        0 -> 3,
        1 -> 2,
        2 -> 2,
        3 -> 1
      )
    }

    "AddI" in {
      AddI(2, 1, 2)(Map(0 -> 3, 1 -> 2, 2 -> 1, 3 -> 1)) shouldEqual Map(
        0 -> 3,
        1 -> 2,
        2 -> 2,
        3 -> 1
      )
    }

    "SetI" in {
      SetI(2, 1, 2)(Map(0 -> 3, 1 -> 2, 2 -> 1, 3 -> 1)) shouldEqual Map(
        0 -> 3,
        1 -> 2,
        2 -> 2,
        3 -> 1
      )
    }

    val sample = """Before: [3, 2, 1, 1]
                   |9 2 1 2
                   |After:  [3, 2, 2, 1]
                   |
                   |
                   |
                   |0 0 0 0""".stripMargin
    "part1" should {
      "example" in {
        day16.part1(sample) shouldEqual 1
      }
    }
  }
}
