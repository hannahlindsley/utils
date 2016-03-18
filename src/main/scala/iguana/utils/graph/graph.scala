import scala.collection.mutable

case class Node[A](id: Int, value: A)

case class Edge[A](origin: Node[A], dest: Node[A])

trait Label[L] {
  def label: L
}

class Graph[A](arr: Array[Set[Node[A]]]) {

}

object Graph {

  def apply[A](edges: Traversable[Edge[A]], size: Int): Graph[A] = {
    val v = Array.ofDim[mutable.Builder[Node[A], Set[Node[A]]]](size)
    (0 to v.length).foreach(v(_) = Set.newBuilder)

    for (edge <- edges) v(edge.origin.id) += edge.dest

    val array = Array.ofDim[Set[Node[A]]](size)
    (0 to v.length).foreach(i => array(i) = v(i).result())

    new Graph(array)
  }

  def apply[A](map: Map[A, A]): Graph[A] = {
    var i = 0
    val array = Array.ofDim[Set[Node[A]]](map.size)
    for ((a, set) <- map) {
      array(i) = set
    }
    new Graph(array)
  }



}

