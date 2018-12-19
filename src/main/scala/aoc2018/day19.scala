package aoc2018
import aoc2018.opcode._

object day19 extends App {

  case class Proc(ipRegister: Int, operations: Array[OpCode]) {
    def ip(registers: Map[Int, Int]): Int = registers(ipRegister)

    def next(registers: Map[Int, Int]): Map[Int, Int] = {
      val current = operations(ip(registers))(registers)
      val result = current + (ipRegister -> (ip(current) + 1))
      result
    }

    def execute(start: Map[Int, Int]): Map[Int, Int] = {
      def loop(ip: Int = 0, registers: Map[Int, Int] = start): Map[Int, Int] =
        if (0 <= ip && ip < this.operations.length) {
          val next = this.next(registers)
          loop(this.ip(next), next)
        } else {
          registers
        }
      loop()
    }
  }

  object Proc {
    def apply(ipRegister: String, operations: List[String]): Proc = {
      val IpRegisterReg = "#ip ([0-9])".r
      ipRegister match {
        case IpRegisterReg(ip) =>
          Proc(ip.toInt, operations.map(parseOperation).toArray)
      }
    }

    def parseOperation(operation: String): OpCode = {
      val OpReg = "(.*) ([0-9]+) ([0-9]+) ([0-9]+)".r
      val strToOp = Map(
        "addr" -> AddR,
        "addi" -> AddI,
        "mulr" -> MulR,
        "muli" -> MulI,
        "banr" -> BanR,
        "bani" -> BanI,
        "borr" -> BorR,
        "bori" -> BorI,
        "setr" -> SetR,
        "seti" -> SetI,
        "gtir" -> GtIR,
        "gtri" -> GtRI,
        "gtrr" -> GtRR,
        "eqir" -> EqIR,
        "eqri" -> EqRI,
        "eqrr" -> EqRR
      )
      operation match {
        case OpReg(id, a, b, c) => strToOp(id)(a.toInt, b.toInt, c.toInt)
      }
    }
  }

  def part1(input: String): Int = {
    val lines = input.split("\n").toList
    val proc = Proc(lines.head, lines.tail)

    proc.execute((0 to 5).map(_ -> 0).toMap)(0)
  }

  // def part2(input: String) = ???
  // part2 is the sum of divisors of your value

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  //println("part2 = " + part2(input))
}
