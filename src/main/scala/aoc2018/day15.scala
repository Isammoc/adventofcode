package aoc2018
import common.algo.graph.Dijkstra
import common.grid.{Direction, Point}

object day15 extends App {

  sealed abstract class Kind {
    val enemy: Kind
  }
  case object Elf extends Kind {
    override val enemy: Kind = Goblin
  }
  case object Goblin extends Kind {
    override val enemy: Kind = Elf
  }

  case class Unit(kind: Kind, location: Point, hitPoint: Int = 200)

  def parse(input: String): (List[Unit], Set[Point]) = {
    val grid = for {
      (line, y) <- input.split("\n").toList.zipWithIndex
      (c, x) <- line.toList.zipWithIndex
    } yield (Point(x, y), c)

    def loop(remain: List[(Point, Char)],
             units: List[Unit] = Nil,
             walls: Set[Point] = Set.empty): (List[Unit], Set[Point]) =
      remain match {
        case Nil           => (units, walls)
        case (p, '#') :: t => loop(t, units, walls + p)
        case (p, 'E') :: t => loop(t, Unit(Elf, p) :: units, walls)
        case (p, 'G') :: t => loop(t, Unit(Goblin, p) :: units, walls)
        case _ :: t        => loop(t, units, walls)
      }

    loop(grid)
  }

  def move(current: Unit, enemies: Set[Unit], blocked: Set[Point]): Unit = {
    Direction.all.map(current.location.move).toSet
    val reachableEnemies =
      enemies.filter(_.location.manhattan(current.location) == 1)
    if (reachableEnemies.isEmpty) {
      val dijkstra = Dijkstra.simpleDistance(current.location)(
        p => Direction.all.map(p.move).filterNot(blocked.contains)
      )
      val inRanges = for {
        enemy <- enemies
        pos = enemy.location
        range <- Direction.all.map(pos.move) if !blocked.contains(range)
      } yield range

      if (inRanges.flatMap(dijkstra.get).isEmpty) {
        current
      } else {
        val minDistance = inRanges.flatMap(dijkstra.get).min
        val chosen =
          inRanges.filter(dijkstra(_) == minDistance).minBy(p => (p.y, p.x))
        val distanceFromChosen = Dijkstra.simpleDistance(chosen)(
          p => Direction.all.map(p.move).filterNot(blocked.contains)
        )
        val possibleMoves =
          Direction.all.map(current.location.move).filterNot(blocked.contains)
        val minDistanceToChosen =
          possibleMoves.map(distanceFromChosen.apply).min
        val moves =
          possibleMoves.filter(distanceFromChosen(_) == minDistanceToChosen)
        val toMove = moves.min
        current.copy(location = toMove)
      }
    } else {
      current
    }
  }

  def printGrid(units: List[Unit], walls: Set[Point]) {
    val maxx = walls.map(_.x).max
    val maxy = walls.map(_.y).max

    val grid = Array.ofDim[Char](maxy + 1, maxx + 1).map(_.map(_ => ' '))

    for (w <- walls) {
      grid(w.y)(w.x) = '#'
    }
    for (u <- units) {
      u.kind match {
        case Elf    => grid(u.location.y)(u.location.x) = 'E'
        case Goblin => grid(u.location.y)(u.location.x) = 'G'
      }
    }

    val infos = for (y <- 0 to maxx) yield {
      units
        .filter(_.location.y == y)
        .sortBy(_.location.x)
        .map { u =>
          u.kind match {
            case Elf    => s"E(${u.hitPoint})"
            case Goblin => s"G(${u.hitPoint})"
          }
        }
        .mkString(",")
    }
    println(
      grid
        .zip(infos)
        .map {
          case (line, info) => line.mkString + "   " + info
        }
        .mkString("\n")
    )
  }

  def battle(initialUnits: List[Unit],
             walls: Set[Point],
             attackElf: Int = 3): (Int, List[Unit]) = {
    def loop(count: Int = 0,
             toMove: List[Unit] = initialUnits.sortBy(_.location),
             moved: List[Unit] = Nil): (Int, List[Unit]) = {
      toMove match {
        case Nil =>
          loop(count + 1, moved.sortBy(_.location), Nil)
        case current :: t =>
          val otherUnits = t.toSet ++ moved
          val enemies = otherUnits.filter(_.kind == current.kind.enemy)
          if (enemies.isEmpty) {
            (count, toMove ++ moved)
          } else {
            val currentMoved =
              move(current, enemies, walls ++ otherUnits.map(_.location))

            val targets =
              enemies.filter(_.location.manhattan(currentMoved.location) == 1)
            if (targets.isEmpty) {
              loop(count, t, currentMoved :: moved)
            } else {
              val attack = if (current.kind == Elf) attackElf else 3
              val target =
                targets.minBy(t => (t.hitPoint, t.location.y, t.location.x))
              if (target.hitPoint <= attack) {
                loop(
                  count,
                  t.filterNot(_ == target),
                  currentMoved :: moved.filterNot(_ == target)
                )
              } else {
                def withAttack(before: List[Unit]): List[Unit] = before.map {
                  case a if a == target =>
                    a.copy(hitPoint = a.hitPoint - attack)
                  case a => a
                }
                loop(count, withAttack(t), currentMoved :: withAttack(moved))
              }
            }
          }
      }
    }
    loop()
  }

  def part1(input: String): Int = {
    val (initialUnits, walls) = parse(input)
    val (count, units) = battle(initialUnits, walls)
    val hps = units.map(_.hitPoint).sum
    count * hps
  }

  def part2(input: String): Int = {
    val (initialUnits, walls) = parse(input)
    val initialElfs = initialUnits.count(_.kind == Elf)
    val (count, units) = Stream
      .from(4)
      .map { attack => battle(initialUnits, walls, attack)
      }
      .dropWhile {
        case (_, result) => result.count(_.kind == Elf) != initialElfs
      }
      .head
    val hps = units.map(_.hitPoint).sum
    count * hps
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
