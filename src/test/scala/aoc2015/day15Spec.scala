package aoc2015
import common.AocSpec

class day15Spec extends AocSpec {

  "day15 2015" can {
    val sample =
      """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
                   |Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3""".stripMargin

    "recipe" should {
      val sampleRecipe = day15.Recipe(
        List(
          day15.Quantity(day15.Ingredient(-1, -2, 6, 3, 8), 44),
          day15.Quantity(day15.Ingredient(2, 3, -2, -1, 3), 56),
        )
      )
      "example" in {
        sampleRecipe.score shouldEqual 62842880
      }
    }
    "part1" should {
      "example" in {
        day15.part1(sample) shouldEqual 62842880
      }
    }

    "part2" should {
      "example" in {
        day15.part2(sample) shouldEqual 57600000
      }
    }
  }
}
