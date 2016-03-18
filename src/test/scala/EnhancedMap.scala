package iguana.utils

import org.scalatest.FunSuite

class EnhancedMapTest extends FunSuite {

  test("merge empty") {
    val m1 = Map.empty[Int, Iterable[Int]]
    val m2 = Map.empty[Int, Iterable[Int]]

    val expected = Map.empty[Int, Iterable[Int]]
    val res = m1 merge m2
    assertResult(expected)(res)
  }

  test("merge 1") {
    val m1 = Map(1 -> Set("a", "b"), 2 -> Set("c"), 3 -> Set("d", "e"))
    val m2 = Map(1 -> Set("a", "b"), 2 -> Set("d"), 3 -> Set("d", "f"))

    val expected = Map(1 -> Set("a", "b"), 2 -> Set("c", "d"), 3 -> Set("d", "e", "f"))
    val res = m1 merge m2

    assertResult(expected)(res)
  }

  test("reverse 1") {
    val m = Map(1 -> Set("a", "b", "d"), 2 -> Set("a", "c"), 3 -> Set("a", "b", "d", "e"))
    val expected = Map("a" -> Set(1, 2, 3), "b" -> Set(1, 3), "c" -> Set(2), "d" -> Set(3, 1), "e" -> Set(3))

    assertResult(expected)(m.reverse)
  }

  test("reverse 2") {
    val m = Map(1 -> Set("a", "b"), 2 -> Set[String](), 3 -> Set("b"))
    val expected = Map("a" -> Set(1), "b" -> Set(1, 3))

    assertResult(expected)(m.reverse)
  }

}
