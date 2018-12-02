package aoc2017

object day21 extends App {

  def symmetric(s: List[List[Char]]): List[List[Char]] = s.transpose

  def possible(s: String): Set[String] = {
    val initial = s.split("/").map(_.toList).toList

    Set(
      initial,
      initial.transpose,
      initial.transpose.reverse,
      initial.transpose.reverse.transpose,
      initial.transpose.reverse.transpose.reverse,
      initial.transpose.reverse.transpose.reverse.transpose,
      initial.transpose.reverse.transpose.reverse.transpose.reverse,
      initial.transpose.reverse.transpose.reverse.transpose.reverse.transpose
    ).map(_.map(_.mkString).mkString)
  }
  def inputToRules(input: String) = {
    val LineReg = "(.*) => (.*)".r
    input
      .split("\n")
      .flatMap {
        case LineReg(before, after) =>
          possible(before).map((_ -> after))
      }
      .toMap
  }

  def applyRule2(current: List[String], rules: Map[String, String]): String = {
    val res =
      Array.fill(current.size / 2 * 3)(Array.fill(current.size / 2 * 3)(' '))
    for {
      y <- 0 until current.size / 2
      x <- 0 until current.size / 2
    } {
      val after = rules(
        "" + current(2 * y)(2 * x) + current(2 * y)(2 * x + 1)
          + current(2 * y + 1)(2 * x) + current(2 * y + 1)(2 * x + 1)
      )

      val List(a, b, c, _, d, e, f, _, g, h, i) = after.toList

      res(3 * y)(3 * x) = a
      res(3 * y)(3 * x + 1) = b
      res(3 * y)(3 * x + 2) = c

      res(3 * y + 1)(3 * x) = d
      res(3 * y + 1)(3 * x + 1) = e
      res(3 * y + 1)(3 * x + 2) = f

      res(3 * y + 2)(3 * x) = g
      res(3 * y + 2)(3 * x + 1) = h
      res(3 * y + 2)(3 * x + 2) = i
    }

    res.map(_.mkString).mkString("\n")
  }
  def applyRule3(current: List[String], rules: Map[String, String]): String = {
    val res =
      Array.fill(current.size / 3 * 4)(Array.fill(current.size / 3 * 4)(' '))

    for {
      y <- 0 until current.size / 3
      x <- 0 until current.size / 3
    } {
      val after = rules(
        "" + current(3 * y)(3 * x) + current(3 * y)(3 * x + 1) + current(3 * y)(
          3 * x + 2
        )
          + current(3 * y + 1)(3 * x) + current(3 * y + 1)(3 * x + 1) + current(
          3 * y + 1
        )(3 * x + 2)
          + current(3 * y + 2)(3 * x) + current(3 * y + 2)(3 * x + 1) + current(
          3 * y + 2
        )(3 * x + 2)
      )

      val List(a, b, c, d, _, e, f, g, h, _, i, j, k, l, _, m, n, o, p) =
        after.toList

      res(4 * y)(4 * x) = a
      res(4 * y)(4 * x + 1) = b
      res(4 * y)(4 * x + 2) = c
      res(4 * y)(4 * x + 3) = d

      res(4 * y + 1)(4 * x) = e
      res(4 * y + 1)(4 * x + 1) = f
      res(4 * y + 1)(4 * x + 2) = g
      res(4 * y + 1)(4 * x + 3) = h

      res(4 * y + 2)(4 * x) = i
      res(4 * y + 2)(4 * x + 1) = j
      res(4 * y + 2)(4 * x + 2) = k
      res(4 * y + 2)(4 * x + 3) = l

      res(4 * y + 3)(4 * x) = m
      res(4 * y + 3)(4 * x + 1) = n
      res(4 * y + 3)(4 * x + 2) = o
      res(4 * y + 3)(4 * x + 3) = p
    }

    res.map(_.mkString).mkString("\n")
  }

  def part1(input: String, iter: Int = 5): Int = {
    val initial = """.#.
                 |..#
                 |###""".stripMargin
    val rules = inputToRules(input)

    def loop(current: String, count: Int = iter): String =
      if (count <= 0) current
      else {
        val currentList = current.split("\n").toList
        if (currentList.size % 2 == 0) {
          loop(applyRule2(currentList, rules), count - 1)
        } else {
          loop(applyRule3(currentList, rules), count - 1)
        }
      }

    loop(initial, iter).count('#' ==)
  }

  def part2(input: String) = part1(input, 18)

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
