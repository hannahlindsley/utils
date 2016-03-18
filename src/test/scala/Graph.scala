import iguana.utils.Graph
import org.scalatest.FunSuite

class GraphTest extends FunSuite {

  test("transitive closure 1") {
    val g = Map(1 -> Set(3), 2 -> Set(1, 4), 4 -> Set(2))
    val expected = Map(1 -> Set(3), 2 -> Set(1, 4, 3, 2), 4 -> Set(2, 1, 4, 3))

    assertResult(expected)(Graph.transitiveClosure(g))
  }

}
