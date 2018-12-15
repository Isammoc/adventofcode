package common.algo.graph
import scala.collection.immutable.Queue

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
}
