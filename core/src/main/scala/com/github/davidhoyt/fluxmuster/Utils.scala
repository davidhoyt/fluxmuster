package com.github.davidhoyt.fluxmuster

private[fluxmuster] object Utils {
  /**
   * Given a sequence it produces another sequence of tuples that each contain the
   * previous element, the current, and the next element in the sequence.
   *
   * For example:
   * {{{
   *   scala> prevCurrentNext(Seq(0, 1, 2, 3)){case x => x}
   *   res0: Seq[(Int, Int, Int)] = List((None, 0, Some(1)), (Some(0), 1, Some(2)), (Some(1), 2, Some(3)), (Some(2), 3, None))
   * }}}
   *
   * @param xs The sequence to use
   * @param fn A partial function that maps the previous, current, and next elements
   * @tparam T Type of the sequence to use
   * @tparam U Type of the sequence that will be output after mapping through `fn`
   * @return A new sequence after applying `fn` to the previous, current, and next elements
   */
  def prevCurrentNext[T, U](xs: Seq[T])(fn: PartialFunction[(Option[T], T, Option[T]), U]): Seq[U] = {
    def step(prev: Option[T], xs: Seq[T], build: Seq[U]): Seq[U] = {
      val (current, next) = xs match {
        case x +: y +: _ => (Some(x), Some(y))
        case x +: _ => (Some(x), None)
        case _ => (None, None)
      }

      if (xs.nonEmpty) {
        val buildNext =
          if (fn.isDefinedAt(prev, current.get, next))
            build :+ fn(prev, current.get, next)
          else
            build
        step(current, xs.tail, buildNext)
      } else
        build
    }
    step(None, xs, Seq.empty)
  }
}
