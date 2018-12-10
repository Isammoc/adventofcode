package aoc2018
import common.AocSpec

class day07Spec extends AocSpec {

  "day07 2018" can {
    val sample = """Step C must be finished before step A can begin.
               |Step C must be finished before step F can begin.
               |Step A must be finished before step B can begin.
               |Step A must be finished before step D can begin.
               |Step B must be finished before step E can begin.
               |Step D must be finished before step E can begin.
               |Step F must be finished before step E can begin.""".stripMargin
    "part1" should {
      "example" in {
        day07.part1(sample) shouldEqual "CABDFE"
      }
    }

    "part2" should {
      "example" in {
        day07.part2(sample, 0, 2) shouldEqual 15
      }
    }
  }
}
