package aoc2018

object day14 extends App {

  val input = io.Source.stdin.getLines.mkString("\n")

  def part1(input: Int): String = {
    val recipes = Array.ofDim[Int](input + 20)
    recipes(0) = 3
    recipes(1) = 7
    def loop(count: Int = 2, first: Int = 0, second: Int = 1): String =
      if (count >= input + 10) {
        recipes.slice(input, input + 10).mkString
      } else {
        val toAdd = toList(recipes(first) + recipes(second))
        toAdd.zipWithIndex.foreach {
          case (recipe, z) => recipes(z + count) = recipe
        }
        val newCount = count + toAdd.size
        loop(
          newCount,
          (first + recipes(first) + 1) % newCount,
          (second + recipes(second) + 1) % newCount
        )
      }
    loop()
  }

  def part2(input: String): Int = {
    val recipes = Array.ofDim[Int](200000000)
    recipes(0) = 3
    recipes(1) = 7
    def loop(count: Int = 2, first: Int = 0, second: Int = 1): Int =
      if (recipes
            .slice(count - input.length, count)
            .mkString == input) {
        count - input.length
      } else if (recipes
                   .slice(count - input.length - 1, count - 1)
                   .mkString == input) {
        count - input.length - 1
      } else {
        val toAdd = toList(recipes(first) + recipes(second))
        toAdd.zipWithIndex.foreach {
          case (recipe, z) => recipes(z + count) = recipe
        }
        val newCount = count + toAdd.size
        loop(
          newCount,
          (first + recipes(first) + 1) % newCount,
          (second + recipes(second) + 1) % newCount
        )
      }
    loop()
  }

  def toList(n: Int): List[Int] = {
    def loop(current: Int = n, res: List[Int] = Nil): List[Int] =
      if (current == 0) {
        res
      } else {
        loop(current / 10, (current % 10) :: res)
      }
    if (n == 0) {
      List(0)
    } else {
      loop()
    }
  }
  println("part1 = " + part1(input.toInt))
  println("part2 = " + part2(input))
}
