package iguana

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.{immutable, TraversableLike, mutable}
import scala.collection.mutable.{Builder, ArrayBuffer}

package object utils {

  val EOF: Int = -1

  implicit class EnhancedMap[A, B, T[B] <: TraversableLike[B, T[B]], This, M <: mutable.MapLike[A, T[B], This]](map: M) {

    def merge(other: M)(implicit cbf: CanBuildFrom[T[B], B, T[B]],
                                 mCbf: CanBuildFrom[M, (A, T[B]), M]): M = {

      val m = mutable.Map.empty[A, Builder[B, T[B]]]

      for ((k, v) <- map)
        m.getOrElseUpdate(k, cbf()) ++= v

      for ((k, v) <- other)
        m.getOrElseUpdate(k, cbf()) ++= v

      val builder = mCbf()
      for ((k, v) <- m)
        builder += k -> v.result()

      builder.result()
    }

    def reverse(implicit bf: CanBuildFrom[T[B], A, T[A]], mCbf: CanBuildFrom[M, (B, T[A]), M]): M = {

      val m = mutable.Map.empty[B, Builder[A, T[A]]]

      for ((a, t) <- map)
        for (b <- t)
          m.getOrElseUpdate(b, bf()) += a

      val res = mCbf()

      for ((k, v) <- m)
        res += k -> v.result()

      res.result()
    }

  }

  implicit class EnhancedSeq[A](seq: Seq[A]) {
    /**
      * Creates partitions from the consecutive elements of a sequence while they meet
      * the condition of the provided predicate
      */
    def takeWhen(f: (A, A) => Boolean): Seq[Seq[A]] = {
      if (seq.isEmpty) return List()

      @tailrec
      def loop(i: Int, prev: A, curr: mutable.Buffer[A], res: mutable.Buffer[mutable.Buffer[A]]): Unit = {
        if (i >= seq.length) {
          res += curr
          return
        }
        val x = seq(i)
        if (f(x, prev))
          loop(i + 1, x, curr += x, res)
        else
          loop(i + 1, x, ArrayBuffer(x), res += curr)
      }

      val res = ArrayBuffer.empty[mutable.Buffer[A]]
      loop(0, seq.head, ArrayBuffer(), res)
      res
    }
  }

}
