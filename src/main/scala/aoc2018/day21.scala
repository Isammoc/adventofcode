package aoc2018
import aoc2018.opcode._

object day21 extends App {

  def part1(input: String) = {
    val lines = input.split("\n").toList
    val proc = Proc(lines.head, lines.tail)
    val init = (0 to 5).map(_ -> 0).toMap

    def loop(registers: Map[Int, Int] = init): Int =
      proc.current(registers) match {
        case None =>
          -1
        case Some(opcode) =>
          if (opcode.registers.contains(0)) {
            opcode match {
              case EqRR(a, 0, _) => registers(a)
              case EqRR(0, a, _) => registers(a)
              case _ =>
                Console.err.println("WTF!!!")
                -1
            }
          } else {
            loop(proc.next(registers))
          }
      }
    loop()
  }

  def part2(input: String) = {
    val lines = input.split("\n").toList
    val proc = Proc(lines.head, lines.tail)
    val init = (0 to 5).map(_ -> 0).toMap

    def loop(registers: Map[Int, Int] = init,
             last: Int = 0,
             visited: Set[Int] = Set.empty): Int =
      proc.current(registers) match {
        case None =>
          -1
        case Some(opcode) =>
          if (opcode.registers.contains(0)) {
            opcode match {
              case EqRR(a, 0, _) =>
                val current = registers(a)
                if (visited.contains(current))
                  last
                else {
                  println("current size = " + visited.size)
                  loop(proc.next(registers), current, visited + current)
                }
              case EqRR(0, a, _) =>
                val current = registers(a)
                if (visited.contains(current))
                  last
                else {
                  println("current size = " + visited.size)
                  loop(proc.next(registers), current, visited + current)
                }
              case _ =>
                Console.err.println("WTF!!!")
                -1
            }
          } else {
            loop(proc.next(registers), last, visited)
          }
      }
    loop()
  }

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
