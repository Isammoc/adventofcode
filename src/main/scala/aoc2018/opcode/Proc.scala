package aoc2018.opcode

case class Proc(ipRegister: Int, operations: Array[OpCode]) {
  def ip(registers: Map[Int, Int]): Int = registers(ipRegister)

  def current(registers: Map[Int, Int]): Option[OpCode] = {
    val ip = this.ip(registers)
    if (0 <= ip && ip < this.operations.length) {
      Some(operations(ip))
    } else {
      None
    }
  }

  def next(registers: Map[Int, Int]): Map[Int, Int] = {
    val current = operations(ip(registers))(registers)
    val result = current + (ipRegister -> (ip(current) + 1))
    result
  }

  def execute(start: Map[Int, Int]): Map[Int, Int] = {
    def loop(ip: Int = 0, registers: Map[Int, Int] = start): Map[Int, Int] =
      current(registers) match {
        case Some(_) =>
          val next = this.next(registers)
          loop(this.ip(next), next)
        case None => registers
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
