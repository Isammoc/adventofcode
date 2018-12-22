package common.algo.graph
import scala.collection.immutable.Queue
import scala.collection.mutable

object Dijkstra {
  def simpleDistance[Node](
    start: Node
  )(implicit neighbors: Node => Iterable[Node]): Map[Node, Long] = {
    def loop(toVisit: Queue[Node] = Queue(start),
             visited: Set[Node] = Set(start),
             result: Map[Node, Long] = Map(start -> 0)): Map[Node, Long] =
      toVisit match {
        case x +: xs =>
          val ns = neighbors(x).filterNot(visited.contains)
          loop(xs ++ ns, visited ++ ns, result ++ ns.map(_ -> (result(x) + 1)))
        case _ => result
      }
    loop().withDefault(_ => Long.MaxValue)
  }

  def reach[Node](
    start: Node
  )(target: Node)(neighbors: Node => Iterable[(Node, Long)]): Long = {
    val queue = mutable.PriorityQueue((start, 0L))(Ordering.by(-_._2))
    def loop(visited: Set[Node]): Long = queue.dequeue match {
      case (t, res) if t == target       => res
      case (v, _) if visited.contains(v) => loop(visited)
      case (current, distance) =>
        for {
          (n, w) <- neighbors(current)
        } {
          queue.enqueue(n -> (distance + w))
        }
        loop(visited + current)
    }
    loop(Set.empty)
  }
}
