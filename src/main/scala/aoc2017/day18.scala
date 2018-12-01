package aoc2017

object day18 extends App {
  case class Instructions(instr: Array[String]) {
    def apply(i: Int): String = instr(i)
  }

  sealed abstract class Status
  case object Running extends Status
  case object Halt extends Status
  case object Sending extends Status
  case object Receiving extends Status
  case object Wait extends Status

  case class Proc(registers: Map[String, Long] = Map.empty,
                  count: Int = 0,
                  status: Status = Running,
                  sending: Option[Long] = None,
                  receiving: Option[String] = None) {
    val RegisterReg = "([a-z])".r
    val LiteralReg = "(-?[0-9]+)".r
    def valueOf(s: String): Long = s match {
      case RegisterReg(r) => registers.getOrElse(r, 0)
      case LiteralReg(l)  => l.toInt
    }
    def execute(implicit instr: Instructions): Proc = {
      instr(count).split(" ") match {
        case Array("snd", x) =>
          this.copy(status = Sending, sending = Some(valueOf(x)))
        case Array("set", x, y) =>
          this
            .copy(count = count + 1, registers = registers + (x -> valueOf(y)))
        case Array("add", x, y) =>
          this.copy(
            count = count + 1,
            registers = registers + (x -> (valueOf(x) + valueOf(y)))
          )
        case Array("mul", x, y) =>
          this.copy(
            count = count + 1,
            registers = registers + (x -> (valueOf(x) * valueOf(y)))
          )
        case Array("mod", x, y) =>
          this.copy(
            count = count + 1,
            registers = registers + (x -> (valueOf(x) % valueOf(y)))
          )
        case Array("rcv", x) =>
          this.copy(status = Receiving, receiving = Some(x))
        case Array("jgz", x, y) =>
          if (valueOf(x) > 0)
            this.copy(count = count + valueOf(y).toInt)
          else
            this.copy(count = count + 1)
      }
    }

    def send: (Long, Proc) =
      (
        sending.get,
        this.copy(count = count + 1, status = Running, sending = None)
      )

    def receive(l: Long): Proc =
      this.copy(
        count = count + 1,
        status = Running,
        registers = registers + (receiving.get -> l),
        receiving = None
      )
  }

  def part1(input: String): Long = {
    implicit val instr = Instructions(input.split("\n"))

    def loop(proc: Proc, lastPlayed: Long): Long = {
      proc.status match {
        case Running =>
          loop(proc.execute, lastPlayed)
        case Sending =>
          val (sent, next) = proc.send
          loop(next, sent)
        case Receiving =>
          lastPlayed
      }
    }
    loop(Proc(), 0)
  }

  def part2(input: String): Int = {
    implicit val instr = Instructions(input.split("\n"))

    def loop(proc0: Proc,
             proc1: Proc,
             to0: List[Long],
             to1: List[Long],
             res: Int): Int = {
      (proc0.status, proc1.status) match {
        case (Sending, _) =>
          val (s, n) = proc0.send
          loop(n, proc1, to0, to1 :+ s, res)
        case (_, Sending) =>
          val (s, n) = proc1.send
          loop(proc0, n, to0 :+ s, to1, res + 1)
        case (Receiving, _) if to0.nonEmpty =>
          val r :: t = to0
          loop(proc0.receive(r), proc1, t, to1, res)
        case (_, Receiving) if to1.nonEmpty =>
          val r :: t = to1
          loop(proc0, proc1.receive(r), to0, t, res)
        case (Running, Running) =>
          loop(proc0.execute, proc1.execute, to0, to1, res)
        case (Running, _) =>
          loop(proc0.execute, proc1, to0, to1, res)
        case (_, Running) =>
          loop(proc0, proc1.execute, to0, to1, res)
        case (Receiving, Receiving) =>
          res
      }
    }
    loop(Proc(Map("p" -> 0)), Proc(Map("p" -> 1)), Nil, Nil, 0)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
