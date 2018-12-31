package aoc2015
import common.AocSpec

class day14Spec extends AocSpec {

  "day14 2015" can {
    val comet = day14.Reindeer(14, 10, 127)
    val dancer = day14.Reindeer(16, 11, 162)

    "reindeers" should {
      "example" in {
        comet.distance(1000) shouldEqual 1120
        dancer.distance(1000) shouldEqual 1056
      }
    }

    "scoring" should {
      val scores = day14.scoring(List(comet, dancer), 1000)
      "example" in {
        scores(dancer) shouldEqual 689
        scores(comet) shouldEqual 312
      }
    }
  }
}
