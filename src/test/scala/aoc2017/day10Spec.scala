package aoc2017
import common.AocSpec

class day10Spec extends AocSpec {

  "day10 2017" can {

    "part1" should {
      "example" in {
        day10.part1("3,4,1,5", 5) shouldBe 12
      }
    }

    "convertInput" should {
      "ici" in {
        day10.convertInput("1,2,3") shouldBe List(49, 44, 50, 44, 51, 17, 31,
          73, 47, 23)
      }
    }

    "round64" should {
      "64 times" in {
        day10.round64(List(1)).size shouldBe 64
      }
      "64 times for 2" in {
        day10.round64(List(1, 2)).size shouldBe 128
      }
    }

    "part2" should {
      "for empty string" in {
        day10.part2("") shouldBe "a2582a3a0e66e6e86e3812dcb672a272"
      }

      "for AoC 2017" in {
        day10.part2("AoC 2017") shouldBe "33efeb34ea91902bb2f59c9920caa6cd"
      }

      "for 1,2,3" in {
        day10.part2("1,2,3") shouldBe "3efbe78a8d82f29979031a4aa0b16a9d"
      }

      "for 1,2,4" in {
        day10.part2("1,2,4") shouldBe "63960835bcdc130f0b66d7ff4f6a5a8e"
      }

    }
  }
}
