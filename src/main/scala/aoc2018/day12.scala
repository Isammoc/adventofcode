package aoc2018

object day12 extends App {

  sealed abstract class Pot
  case object Plant extends Pot
  case object Empty extends Pot

  object Pot {
    def apply(c: Char): Pot = if (c == '#') Plant else Empty
    def apply(str: String): Pot = Pot(str(0))
  }

  case class State(left: Int, pots: List[Pot]) {
    def next(rules: Rules): State = {
      def loop(remain: List[Pot], result: List[Pot] = Nil): State =
        remain match {
          case a :: b :: c :: d :: e :: t if rules.contains((a, b, c, d, e)) =>
            loop(b :: c :: d :: e :: t, Plant :: result)
          case a :: b :: c :: d :: e :: t =>
            loop(b :: c :: d :: e :: t, Empty :: result)
          case _ => State(left - 2, result.reverse)
        }
      loop(
        List(Empty, Empty, Empty, Empty) ++
          pots ++
          List(Empty, Empty, Empty, Empty)
      )
    }

    def compute: Int =
      pots.zipWithIndex.filter(_._1 == Plant).map(_._2 + left).sum
  }

  object State {
    def apply(str: String): State = State(0, str.toList.map(Pot.apply))
  }
  type Rule = (Pot, Pot, Pot, Pot, Pot)
  type Rules = Set[Rule]

  def parse(input: String): (State, Rules) = {
    val InitialReg = "initial state: ([#.]+)".r
    val RuleReg = "([#.])([#.])([#.])([#.])([#.]) => #".r

    def loop(remain: List[String],
             rules: Rules = Set.empty,
             initial: Option[State] = None): (State, Rules) = remain match {
      case Nil                   => (initial.get, rules)
      case InitialReg(pots) :: t => loop(t, rules, Some(State(pots)))
      case RuleReg(a, b, c, d, e) :: t =>
        loop(t, rules + ((Pot(a), Pot(b), Pot(c), Pot(d), Pot(e))), initial)
      case _ :: t => loop(t, rules, initial)
    }
    loop(input.split("\n").toList)
  }

  def part1(input: String): Int = {
    val (initial, rules) = parse(input)

    def loop(remain: Int, current: State = initial): Int =
      if (remain > 0) {
        loop(remain - 1, current.next(rules))
      } else {
        current.compute
      }
    loop(20)
  }

  def part2(input: String) = {
    val (initial, rules) = parse(input)
    val fiftyBillion = BigInt("50000000000")
    def loop(step: Int = 0,
             current: State = initial,
             last: (Int, Int, Int, Int) = (0, 1, 2, 3)): BigInt = last match {
      case (a, b, c, d) if a == b && b == c && c == d =>
        (fiftyBillion - step) * a + current.compute
      case (_, b, c, d) =>
        loop(
          step + 1,
          current.next(rules),
          (b, c, d, current.next(rules).compute - current.compute)
        )
    }
    loop()
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
