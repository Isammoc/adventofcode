package aoc2017
import common.AocSpec

class day18Spec extends AocSpec {

  "day18 2017" can {

    "part1" should {
      "example" in {
          val sample = """set a 1
                |add a 2
                |mul a a
                |mod a 5
                |snd a
                |set a 0
                |rcv a
                |jgz a -1
                |set a 1
                |jgz a -2""".stripMargin

        day18.part1(sample) shouldBe 4
      }
    }

    "part2" should {
      "example" in {
          val sample2 = """snd 1
                 |snd 2
                 |snd p
                 |rcv a
                 |rcv b
                 |rcv c
                 |rcv d""".stripMargin
      }
    }
  }
}

