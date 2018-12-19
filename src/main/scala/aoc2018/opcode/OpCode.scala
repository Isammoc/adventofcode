package aoc2018.opcode

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
