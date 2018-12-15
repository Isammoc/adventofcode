package aoc2018
import common.AocSpec

class day15Spec extends AocSpec {

  "day15 2018" can {

    val sample1 = """#######
                    |#.G...#
                    |#...EG#
                    |#.#.#G#
                    |#..G#E#
                    |#.....#
                    |#######""".stripMargin
    val sample2 = """#######
                    |#G..#E#
                    |#E#E.E#
                    |#G.##.#
                    |#...#E#
                    |#...E.#
                    |#######""".stripMargin
    val sample3 = """#######
                    |#E..EG#
                    |#.#G.E#
                    |#E.##E#
                    |#G..#.#
                    |#..E#.#
                    |#######""".stripMargin
    val sample4 = """#######
                    |#E.G#.#
                    |#.#G..#
                    |#G.#.G#
                    |#G..#.#
                    |#...E.#
                    |#######""".stripMargin
    val sample5 = """#######
                    |#.E...#
                    |#.#..G#
                    |#.###.#
                    |#E#G#G#
                    |#...#G#
                    |#######""".stripMargin
    val sample6 = """#########
                    |#G......#
                    |#.E.#...#
                    |#..##..G#
                    |#...##..#
                    |#...#...#
                    |#.G...G.#
                    |#.....G.#
                    |#########""".stripMargin
    "part1" should {
      "example" in {
        day15.part1(sample1) shouldEqual 27730
        day15.part1(sample2) shouldEqual 36334
        day15.part1(sample3) shouldEqual 39514
        day15.part1(sample4) shouldEqual 27755
        day15.part1(sample5) shouldEqual 28944
        day15.part1(sample6) shouldEqual 18740
      }
    }

    "part2" should {
      "example" in {
        day15.part2(sample1) shouldEqual 4988
        day15.part2(sample3) shouldEqual 31284
        day15.part2(sample4) shouldEqual 3478
        day15.part2(sample5) shouldEqual 6474
        day15.part2(sample6) shouldEqual 1140
      }
    }
  }
}
