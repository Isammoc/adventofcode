package aoc2018

object day08 extends App {

  case class Node(children: List[Node], metadata: List[Int]) {
    val totalSum: Int = children.map(_.totalSum).sum + metadata.sum
    val check: Int = if (children.isEmpty) {
      metadata.sum
    } else {
      val childrenMap = children.zipWithIndex.map {
        case (a, b) => (b, a)
      }.toMap
      metadata.map { i => childrenMap.get(i - 1).map(_.check).getOrElse(0)
      }.sum
    }
  }

  object Node {
    def apply(input: String): Node =
      Node(input.split("\\s+").map(_.toInt).toList)

    def apply(input: List[Int]): Node = {
      def create(input: List[Int]): (Node, List[Int]) = input match {
        case childrenCount :: metadataCount :: t =>
          def loop(todo: Int,
                   children: List[Node],
                   input: List[Int]): (Node, List[Int]) = todo match {
            case 0 =>
              (
                Node(children.reverse, input.take(metadataCount)),
                input.drop(metadataCount)
              )
            case i =>
              val (child, t) = create(input)
              loop(i - 1, child :: children, t)
          }
          loop(childrenCount, Nil, t)
      }
      create(input)._1
    }
  }

  def part1(input: String) = Node(input).totalSum

  def part2(input: String) = Node(input).check

  val input = io.Source.stdin.getLines.mkString("\n")
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
