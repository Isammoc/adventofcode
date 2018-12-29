package aoc2015
import java.security.MessageDigest

object day04 extends App {

  implicit class Hexable(val buf: Array[Byte]) {
    def toHexString: String = buf.map("%02X" format _).mkString
  }

  def md5(s: String) = MessageDigest.getInstance("MD5").digest(s.getBytes).toHexString

  def part1(input: String) = Stream.from(1).filter(i => md5(input + i).startsWith("00000")).head

  def part2(input: String) = Stream.from(1).filter(i => md5(input + i).startsWith("000000")).head

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
