package aoc2018

object day07 extends App {

  def part1(input: String) = {
    val LineReg = "Step (.*) must be finished before step (.*) can begin.".r
    val rules = input.split("\n").map {
      case LineReg(a, b) => (a, b)
    }

    def loop(rules: Set[(String, String)],
             possible: Set[String],
             result: String): String = {
      if (possible.isEmpty) {
        result
      } else {
        val next =
          possible.toList.sorted.find(p => !rules.map(_._2).contains(p)).get
        loop(rules.filterNot(_._1 == next), possible - next, result + next)
      }
    }
    loop(rules.toSet, rules.flatMap { case (a, b) => List(a, b) }.toSet, "")
  }

  def part2(input: String, stepDuration: Int = 60, workers: Int = 5) = {
    sealed abstract class Worker
    case class Busy(duration: Int, step: String) extends Worker
    case object Available extends Worker

    val LineReg = "Step (.*) must be finished before step (.*) can begin.".r
    val rules = input.split("\n").map {
      case LineReg(a, b) => (a, b)
    }

    def loop(rules: Set[(String, String)],
             remaining: Set[String],
             workers: Map[Int, Worker],
             current: Int): Int = {
      if (remaining.isEmpty && workers.values.forall(_ == Available))
        current
      else {
        val finish = workers.toList.find {
          case (_, Busy(0, _)) => true
          case _               => false
        }
        finish match {
          case Some((i, Busy(0, step))) =>
            loop(
              rules.filterNot(_._1 == step),
              remaining,
              workers + (i -> Available),
              current
            )
          case _ =>
            val available = workers.toList.find {
              case (_, Available) => true
              case _              => false
            }
            available match {
              case Some((i, _)) =>
                remaining.toList.sorted
                  .find(p => !rules.map(_._2).contains(p)) match {
                  case Some(next) =>
                    loop(
                      rules,
                      remaining - next,
                      workers + (i -> Busy(
                        stepDuration + next(0) - 'A' + 1,
                        next
                      )),
                      current
                    )
                  case _ =>
                    val duration =
                      workers
                        .collect { case (_, a: Busy) => a }
                        .map(_.duration)
                        .min
                    loop(rules, remaining, workers.map {
                      case (id, Available)  => id -> Available
                      case (id, Busy(d, s)) => id -> Busy(d - duration, s)
                    }, current + duration)
                }
              case _ =>
                val duration =
                  workers.collect { case (_, a: Busy) => a }.map(_.duration).min
                loop(rules, remaining, workers.map {
                  case (i, Available)  => i -> Available
                  case (i, Busy(d, s)) => i -> Busy(d - duration, s)
                }, current + duration)
            }
        }
      }
    }
    loop(
      rules.toSet,
      rules.flatMap { case (a, b) => List(a, b) }.toSet,
      (1 to workers).map { (_, Available) }.toMap,
      0
    )

  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
