package aoc2015

object day15 extends App {

  case class Ingredient(capacity: Int,
                        durability: Int,
                        flavor: Int,
                        texture: Int,
                        calories: Int)

  case class Quantity(ingredient: Ingredient, count: Int) {
    def capacity: Int = ingredient.capacity * count
    def durability: Int = ingredient.durability * count
    def flavor: Int = ingredient.flavor * count
    def texture: Int = ingredient.texture * count
    def calories: Int = ingredient.calories * count
  }

  case class Recipe(quantities: List[Quantity]) {
    def score: Int = {
      val capacity = math.max(0, quantities.map(_.capacity).sum)
      val durability = math.max(0, quantities.map(_.durability).sum)
      val flavor = math.max(0, quantities.map(_.flavor).sum)
      val texture = math.max(0, quantities.map(_.texture).sum)
      capacity * durability * flavor * texture
    }

    def calories: Int = {
      math.max(0, quantities.map(_.calories).sum)
    }
  }

  def parseInput(input: String): List[Ingredient] = {
    val LineReg =
      ".*: capacity (-?[0-9]+), durability (-?[0-9]+), flavor (-?[0-9]+), texture (-?[0-9]+), calories (-?[0-9]+)".r
    input.split("\n").toList.map {
      case LineReg(capacity, durability, flavor, texture, calories) =>
        Ingredient(
          capacity.toInt,
          durability.toInt,
          flavor.toInt,
          texture.toInt,
          calories.toInt
        )
    }
  }

  def foundMaxScore(ingredients: List[Ingredient]): Int = {
    def loop(remain: List[Ingredient], current: List[Quantity]): Int =
      remain match {
        case Nil => Recipe(current).score
        case i :: t =>
          (for (q <- 0 to (100 - current.map(_.count).sum))
            yield loop(t, Quantity(i, q) :: current)).max
      }
    loop(ingredients, Nil)
  }

  def foundMaxScoreCaloriesFixed(ingredients: List[Ingredient]): Int = {
    def loop(remain: List[Ingredient], current: List[Quantity]): Int =
      remain match {
        case Nil =>
          if (Recipe(current).calories == 500)
            Recipe(current).score
          else 0
        case i :: t =>
          (for (q <- 0 to (100 - current.map(_.count).sum))
            yield loop(t, Quantity(i, q) :: current)).max
      }
    loop(ingredients, Nil)
  }

  def part1(input: String): Int = {
    val ingredients = parseInput(input)
    foundMaxScore(ingredients)
  }

  def part2(input: String): Int = {
    val ingredients = parseInput(input)
    foundMaxScoreCaloriesFixed(ingredients)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
