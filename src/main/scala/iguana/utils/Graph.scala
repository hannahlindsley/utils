package iguana.utils

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.collection.mutable.Builder

object Graph {

  type Graph[A] = Map[A, Set[A]]

  def transitiveClosure[A](r: Graph[A]): Graph[A] = {

    type M[A] = mutable.Map[A, mutable.Set[A]]

    @tailrec
    def loop(x: M[A], revX: M[A], result: M[A]): Graph[A] = {
      if (x.isEmpty) {
        ???
      } else {
        val newX = mutable.Map.empty[A, mutable.Set[A]]
        val xRev = mutable.Map.empty[A, mutable.Set[A]]

        for ((b, as) <- revX)
          for (c <- r.getOrElse(b, Set()))
            for (a <- as)
              if (!contains(result, c)) {
                newX.getOrElseUpdate(a, mutable.Set.empty[A]) += c
                xRev.getOrElseUpdate(c, mutable.Set.empty[A]) += a
              }
        loop(newX, xRev, newX merge result)
      }
    }

    val initial = mutable.Map.empty[A, mutable.Set[A]]
    for ((a, set) <- r)
      initial.getOrElseUpdate(a, mutable.Set.empty) ++ set

    loop(initial, r.reverse, r)
  }

  def contains[A](m: mutable.Map[A, mutable.Set[A]], v: A): Boolean =
    m.get(v) match {
      case Some(b) => b
      case None => false
    }

}