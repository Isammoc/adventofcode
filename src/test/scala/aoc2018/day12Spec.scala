package aoc2018
import common.AocSpec

class day12Spec extends AocSpec {

  "day12 2018" can {
    val sample = """initial state: #..#.#..##......###...###
                   |
                   |...## => #
                   |..#.. => #
                   |.#... => #
                   |.#.#. => #
                   |.#.## => #
                   |.##.. => #
                   |.#### => #
                   |#.#.# => #
                   |#.### => #
                   |##.#. => #
                   |##.## => #
                   |###.. => #
                   |###.# => #
                   |####. => #""".stripMargin
    "part1" should {
      "example" in {
        day12.part1(sample) shouldEqual 325
      }
    }
  }
}
