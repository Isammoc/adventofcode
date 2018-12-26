package aoc2018
import scala.annotation.tailrec

object day24 extends App {

  sealed abstract class Team
  case object ImmuneSystem extends Team
  case object Infection extends Team

  case class Group(id: Int,
                   team: Team,
                   count: Int,
                   hitPoints: Int,
                   weak: Set[String],
                   immune: Set[String],
                   attackPoints: Int,
                   attackType: String,
                   initiative: Int) {
    val effectivePower: Int = this.count * this.attackPoints
    def damageTo(that: Group): Int =
      if (that.immune.contains(this.attackType)) {
        0
      } else if (that.weak.contains(this.attackType)) {
        this.effectivePower * 2
      } else {
        this.effectivePower
      }
  }

  case class Fight(groups: List[Group]) {
    def withBoost(boost: Int): Fight = {
      Fight(groups.map {
        case g if g.team == ImmuneSystem =>
          g.copy(attackPoints = g.attackPoints + boost)
        case e => e
      })
    }

    def isFinal: Boolean = groups.map(_.team).toSet.size == 1
    def whoWin: Team = groups.head.team
    def targetPhase: Map[Int, Int] = {
      def loop(remain: List[Group],
               chosen: Set[Int] = Set.empty,
               res: Map[Int, Int] = Map.empty): Map[Int, Int] = remain match {
        case Nil => res
        case g :: t =>
          val optEnemy =
            groups
              .filterNot(_.team == g.team)
              .filterNot(chosen contains _.id)
              .sortBy(
                enemy =>
                  (-g.damageTo(enemy), -enemy.effectivePower, -enemy.initiative)
              )
              .headOption.filterNot(g.damageTo(_) == 0)
          if (optEnemy.isDefined) {
            loop(t, chosen + optEnemy.get.id, res + (g.id -> optEnemy.get.id))
          } else {
            loop(t, chosen, res)
          }
      }
      loop(groups.sortBy(g => (-g.effectivePower, -g.initiative)))
    }

    def attackPhase(target: Map[Int, Int]): Fight = {
      def loop(
        remain: List[Int],
        result: Map[Int, Group] = groups.map(g => g.id -> g).toMap
      ): Fight = remain match {
        case Nil                             => Fight(result.values.toList)
        case g :: t if result.get(g).isEmpty => loop(t, result)
        case g :: t =>
          val attackers = result(g)
          target.get(g).map(result.apply) match {
            case None => loop(t, result)
            case Some(defenders) =>
              val damage = attackers.damageTo(defenders)
              val lost = damage / defenders.hitPoints
              if (lost < defenders.count) {
                loop(
                  t,
                  result - defenders.id + (defenders.id -> defenders
                    .copy(count = defenders.count - lost))
                )
              } else {
                loop(t, result - defenders.id)
              }
          }
      }
      loop(groups.sortBy(-_.initiative).map(_.id))
    }
  }

  def parseInput(input: String): Fight = {
    val GroupReg =
      "([0-9]+) units each with ([0-9]+) hit points (?:\\((.*)\\) )?with an attack that does ([0-9]+) (.*) damage at initiative ([0-9]+)".r
    val Array(immune, infection) = input.split("\n\n")
    val idGenerator = Stream.from(1).iterator
    def parseSpecial(input: String): (Set[String], Set[String]) = {
      def loop(remain: List[String],
               weak: Set[String] = Set.empty,
               immune: Set[String] = Set.empty): (Set[String], Set[String]) =
        remain match {
          case Nil => (weak, immune)
          case w :: t if w.startsWith("weak to ") =>
            loop(t, weak ++ w.substring("weak to ".length).split(", "), immune)
          case i :: t if i.startsWith("immune to ") =>
            loop(
              t,
              weak,
              immune ++ i.substring("immune to ".length).split(", ")
            )
        }
      loop(if (input == null) Nil else input.split("; ").toList)
    }
    def parseTeam(input: String, team: Team): List[Group] = {
      input.split("\n").toList.tail.map {
        case GroupReg(
            count,
            hitPoints,
            specials,
            attackPoints,
            attackType,
            initiative
            ) =>
          val (weak, immune) = parseSpecial(specials)
          Group(
            idGenerator.next,
            team,
            count.toInt,
            hitPoints.toInt,
            weak,
            immune,
            attackPoints.toInt,
            attackType,
            initiative.toInt
          )
      }
    }
    Fight(parseTeam(immune, ImmuneSystem) ++ parseTeam(infection, Infection))
  }

  @tailrec
  def combat(current: Fight, before: Fight = Fight(Nil)): Fight =
    if (current.isFinal) {
      current
    } else if (current == before) {
      Fight(
        List(
          Group(-1, Infection, -1, -1, Set.empty, Set.empty, -1, "nothing", -1)
        )
      )
    } else {
      combat(current.attackPhase(current.targetPhase), current)
    }

  def part1(input: String): Int = {
    val initial = parseInput(input)
    combat(initial).groups.map(_.count).sum
  }

  def part2(input: String): Int = {
    val initial = parseInput(input)
    val first = Stream
      .from(0)
      .map(initial.withBoost)
      .map(combat(_))
      .dropWhile(_.whoWin == Infection)
      .head
    first.groups.map(_.count).sum
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
