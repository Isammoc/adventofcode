package aoc2018

object day16 extends App {

  sealed abstract class OpCode {
    val a: Int
    val b: Int
    val c: Int
    def apply(registers: Map[Int, Int]): Map[Int, Int]
    def isPossible: Boolean
    def isRegister(x: Int): Boolean = 0 <= x && x <= 3
    val isRegisterA: Boolean = isRegister(a)
    val isRegisterB: Boolean = isRegister(b)
    val isRegisterC: Boolean = isRegister(c)
  }
  sealed abstract class OpCodeRR extends OpCode {
    def isPossible: Boolean = isRegisterA && isRegisterB && isRegisterC
  }
  sealed abstract class OpCodeIR extends OpCode {
    def isPossible: Boolean = isRegisterB && isRegisterC
  }
  sealed abstract class OpCodeRI extends OpCode {
    def isPossible: Boolean = isRegisterA && isRegisterC
  }
  sealed abstract class OpCodeII extends OpCode {
    def isPossible: Boolean = isRegisterC
  }
  case class AddR(a: Int, b: Int, c: Int) extends OpCodeRR {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (registers(a) + registers(b)))
  }
  case class AddI(a: Int, b: Int, c: Int) extends OpCodeRI {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (registers(a) + b))
  }
  case class MulR(a: Int, b: Int, c: Int) extends OpCodeRR {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (registers(a) * registers(b)))
  }
  case class MulI(a: Int, b: Int, c: Int) extends OpCodeRI {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (registers(a) * b))
  }
  case class BanR(a: Int, b: Int, c: Int) extends OpCodeRR {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (registers(a) & registers(b)))
  }
  case class BanI(a: Int, b: Int, c: Int) extends OpCodeRI {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (registers(a) & b))
  }
  case class BorR(a: Int, b: Int, c: Int) extends OpCodeRR {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (registers(a) | registers(b)))
  }
  case class BorI(a: Int, b: Int, c: Int) extends OpCodeRI {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (registers(a) | b))
  }
  case class SetR(a: Int, b: Int, c: Int) extends OpCodeRI {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> registers(a))
  }
  case class SetI(a: Int, b: Int, c: Int) extends OpCodeII {
    def apply(registers: Map[Int, Int]): Map[Int, Int] = registers + (c -> a)
  }
  case class GtIR(a: Int, b: Int, c: Int) extends OpCodeIR {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (if (a > registers(b)) 1 else 0))
  }
  case class GtRI(a: Int, b: Int, c: Int) extends OpCodeRI {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (if (registers(a) > b) 1 else 0))
  }
  case class GtRR(a: Int, b: Int, c: Int) extends OpCodeRR {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (if (registers(a) > registers(b)) 1 else 0))
  }
  case class EqIR(a: Int, b: Int, c: Int) extends OpCodeIR {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (if (a == registers(b)) 1 else 0))
  }
  case class EqRI(a: Int, b: Int, c: Int) extends OpCodeRI {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (if (registers(a) == b) 1 else 0))
  }
  case class EqRR(a: Int, b: Int, c: Int) extends OpCodeRR {
    def apply(registers: Map[Int, Int]): Map[Int, Int] =
      registers + (c -> (if (registers(a) == registers(b)) 1 else 0))
  }

  object OpCode {
    val all = List(
      AddR,
      AddI,
      MulR,
      MulI,
      BanR,
      BanI,
      BorR,
      BorI,
      SetR,
      SetI,
      GtIR,
      GtRI,
      GtRR,
      EqIR,
      EqRI,
      EqRR
    )
    def getAll(a: Int, b: Int, c: Int): List[OpCode] =
      all.map(_.apply(a, b, c))
  }

  case class Sample(before: Map[Int, Int],
                    code: Int,
                    a: Int,
                    b: Int,
                    c: Int,
                    after: Map[Int, Int]) {
    def filter(
      possibles: List[(Int, Int, Int) => OpCode]
    ): List[(Int, Int, Int) => OpCode] =
      possibles
        .filter { _.apply(a, b, c).isPossible }
        .filter {
          _.apply(a, b, c).apply(before) == after
        }
  }

  case class Operation(code: Int, a: Int, b: Int, c: Int) {
    def filter(
      possibles: List[(Int, Int, Int) => OpCode]
    ): List[(Int, Int, Int) => OpCode] =
      possibles.filter { _.apply(a, b, c).isPossible }
  }

  def part1(input: String): Int = {
    val Array(samplesInput, _) = input.split("\n\n\n\n")
    val samples: List[Sample] =
      parseSamples(samplesInput)

    samples.map(_.filter(OpCode.all).size).count(_ >= 3)
  }

  private def parseSamples(input: String) = {
    val lines = input.split("\n\n").toList

    val BeforeReg =
      """Before:\s*\[\s*([0-9]+)\s*,\s*([0-9]+)\s*,\s*([0-9]+)\s*,\s*([0-9]+)\s*\]""".r
    val OpCodeReg = """\s*([0-9]+)\s*([0-9]+)\s*([0-9]+)\s*([0-9]+)\s*""".r
    val AfterReg =
      """After:\s*\[\s*([0-9]+)\s*,\s*([0-9]+)\s*,\s*([0-9]+)\s*,\s*([0-9]+)\s*\]""".r

    val samples = lines
      .map(_.split("\n"))
      .map {
        case Array(
            BeforeReg(ba, bb, bc, bd),
            OpCodeReg(opcode, a, b, c),
            AfterReg(aa, ab, ac, ad)
            ) =>
          Sample(
            Map(0 -> ba.toInt, 1 -> bb.toInt, 2 -> bc.toInt, 3 -> bd.toInt),
            opcode.toInt,
            a.toInt,
            b.toInt,
            c.toInt,
            Map(0 -> aa.toInt, 1 -> ab.toInt, 2 -> ac.toInt, 3 -> ad.toInt)
          )
      }
    samples
  }

  def parseOperations(input: String): List[Operation] = {
    val InstrReg = """([0-9]+)\s+([0-9]+)\s+([0-9]+)\s+([0-9]+)""".r
    input
      .split("\n")
      .toList
      .map {
        case InstrReg(code, a, b, c) =>
          Operation(code.toInt, a.toInt, b.toInt, c.toInt)
      }
  }

  def part2(input: String): Int = {
    val Array(samplesInput, operationsInput) = input.split("\n\n\n\n")
    val samples = parseSamples(samplesInput)

    val operations = parseOperations(operationsInput)

    val allCodes = samples.map(_.code).toSet ++ operations.map(_.code).toSet

    type OpCodeName = (Int, Int, Int) => OpCode

    def loop(remain: List[OpCodeName],
             result: Map[Int, OpCodeName]): Map[Int, OpCodeName] =
      if (remain.isEmpty) {
        result
      } else {
        val found = for {
          code <- allCodes
          afterSamples = samples
            .filter(_.code == code)
            .foldLeft(remain) {
              case (r, current) => current.filter(r)
            }
          afterOperations = operations
            .filter(_.code == code)
            .foldLeft(afterSamples) {
              case (r, current) => current.filter(r)
            }
          result <- afterOperations if afterOperations.size == 1
        } yield code -> result

        loop(remain.filterNot(found.map(_._2).contains), result ++ found)
      }

    val operationForCode = loop(OpCode.all, Map.empty)
    val program = operations.map(op => operationForCode(op.code)(op.a, op.b, op.c))
    val initialRegisters = Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0)
    val finalRegisters =
      program.foldLeft(initialRegisters) {
        case (registers, instr) =>
          instr(registers)
      }
    finalRegisters(0)
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
