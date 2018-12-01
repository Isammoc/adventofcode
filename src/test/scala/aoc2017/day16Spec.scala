package aoc2017
import common.AocSpec

class day16Spec extends AocSpec {

  "day16 2017" can {
    "State" should {
      "spin" in {
        day16.State("abcde").spin(3).current shouldBe "cdeab"
        day16.State("abcde").spin(1).current shouldBe "eabcd"
      }

      "exchange" in {
        day16.State("eabcd").exchange(3, 4).current shouldBe "eabdc"
      }

      "partner" in {
        day16.State("eabdc").partner('e', 'b').current shouldBe "baedc"
      }
    }
    val sample = "s1,x3/4,pe/b"

    "part1" should {
      "example" in {
        day16.part1(sample, "abcde") shouldBe "baedc"
      }
    }

    "part2" should {
      "example" in {
        day16.part2(sample, 2, "abcde") shouldBe "ceadb"
      }
    }
  }
}
