package aoc2018
import scala.util.matching.Regex

object day04 extends App {
  sealed abstract class Event {
    val time: String
    val TimeReg: Regex = ".*:(.*)".r
    val minutes: Int = time match {
      case TimeReg(m) => m.toInt
    }
  }

  object Event {
    def apply(str: String): Event = {
      val ShiftReg = "\\[(.*)\\] Guard #(.*) begins shift".r
      val AsleepReg = "\\[(.*)\\] falls asleep".r
      val AwakeReg = "\\[(.*)\\] wakes up".r
      str match {
        case ShiftReg(time, id) => Shift(time, id.toInt)
        case AsleepReg(time)    => Asleep(time)
        case AwakeReg(time)     => Awake(time)
      }
    }
  }
  case class Shift(time: String, id: Int) extends Event
  case class Asleep(time: String) extends Event
  case class Awake(time: String) extends Event

  def toSleeps(input: String): Map[Int, List[(Int, Int)]] = {
    val events = input.split("\n").map(Event.apply).sortBy(_.time).toList
    def loop(
      remain: List[Event],
      current: Int,
      lastSleep: Option[Int],
      results: Map[Int, List[(Int, Int)]]
    ): Map[Int, List[(Int, Int)]] = {
      remain match {
        case Nil =>
          results
        case (s: Shift) :: t  => loop(t, s.id, None, results)
        case (a: Asleep) :: t => loop(t, current, Some(a.minutes), results)
        case (a: Awake) :: t =>
          loop(
            t,
            current,
            lastSleep,
            results + (current -> ((lastSleep.get, a.minutes) :: results
              .getOrElse(current, Nil)))
          )
      }
    }
    loop(events, 0, None, Map.empty)
  }

  def part1(input: String) = {
    val sleeps = toSleeps(input)
    val (candidateId, candidateSleeps) = sleeps.toList.maxBy {
      _._2.map {
        case (a, b) => b - a
      }.sum
    }
    val minute = (for {
      (a, b) <- candidateSleeps
      c <- a until b
    } yield c).groupBy(identity).maxBy(_._2.length)._1
    candidateId * minute
  }

  def part2(input: String) = {
    val sleeps = toSleeps(input)
    val allMinutes = for {
      (id, sleeps) <- sleeps.toList
      (a, b) <- sleeps
      m <- a until b
    } yield (id, m)
    val (id, minute) = allMinutes.groupBy(identity).maxBy(_._2.length)._1
    id * minute
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
