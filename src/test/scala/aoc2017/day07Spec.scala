package aoc2017
import common.AocSpec

class day07Spec extends AocSpec {

  "day07 2017" can {

    "part1" should {
      "example" in {
        day07.part1("""pbga (66)
                      |xhth (57)
                      |ebii (61)
                      |havc (66)
                      |ktlj (57)
                      |fwft (72) -> ktlj, cntj, xhth
                      |qoyq (66)
                      |padx (45) -> pbga, havc, qoyq
                      |tknk (41) -> ugml, padx, fwft
                      |jptl (61)
                      |ugml (68) -> gyxo, ebii, jptl
                      |gyxo (61)
                      |cntj (57)""".stripMargin) shouldEqual "tknk"
      }
    }

    "part2" should {
      "example" in {
        day07.part2("""pbga (66)
                      |xhth (57)
                      |ebii (61)
                      |havc (66)
                      |ktlj (57)
                      |fwft (72) -> ktlj, cntj, xhth
                      |qoyq (66)
                      |padx (45) -> pbga, havc, qoyq
                      |tknk (41) -> ugml, padx, fwft
                      |jptl (61)
                      |ugml (68) -> gyxo, ebii, jptl
                      |gyxo (61)
                      |cntj (57)""".stripMargin) shouldEqual 60
      }
    }
  }
}

