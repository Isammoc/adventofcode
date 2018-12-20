package aoc2018
import common.AocSpec

class day20Spec extends AocSpec {

  "day20 2018" can {
    val sample1 = "^WNE$"
    val sample2 = "^ENWWW(NEEE|SSE(EE|N))$"
    val sample3 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
    val sample4 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
    val sample5 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"

    "part1" should {
      "example" in {
        day20.part1(sample1) shouldEqual 3
        day20.part1(sample2) shouldEqual 10
        day20.part1(sample3) shouldEqual 18
        day20.part1(sample4) shouldEqual 23
        day20.part1(sample5) shouldEqual 31
      }
    }
  }
}

