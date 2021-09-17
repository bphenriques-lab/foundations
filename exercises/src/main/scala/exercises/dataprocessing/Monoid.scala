package exercises.dataprocessing

trait Monoid[A] {
  def default: A

  def combine(first: A, second: A): A
}

object Monoid {
  val sumInt: Monoid[Int] = new Monoid[Int] {
    def default: Int                          = 0
    def combine(first: Int, second: Int): Int = first + second
  }

  val sumDouble: Monoid[Double] = new Monoid[Double] {
    def default: Double                                = 0.0
    def combine(first: Double, second: Double): Double = first + second
  }

  def maxOption[A](implicit sort: Ordering[A]): Monoid[Option[A]] = option(sort.max)
  def minOption[A](implicit sort: Ordering[A]): Monoid[Option[A]] = option(sort.min)
  def option[A](combine2: (A, A) => A): Monoid[Option[A]] = new Monoid[Option[A]] {
    override def default: Option[A] = None
    override def combine(first: Option[A], second: Option[A]): Option[A] = (first, second) match {
      case (None, None) => None
      case (Some(a), None) => Some(a)
      case (None, Some(b)) => Some(b)
      case (Some(a), Some(b)) => Some(combine2(a, b))
    }
  }

  def zip[A, B](monoidA: Monoid[A], monoidB: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def default: (A, B) = (monoidA.default, monoidB.default)

    override def combine(first: (A, B), second: (A, B)): (A, B) = (
      monoidA.combine(first._1, second._1),
      monoidB.combine(first._2, second._2)
    )
  }

  def zip4[A, B, C, D](monoidA: Monoid[A], monoidB: Monoid[B], monoidC: Monoid[C], monoidD: Monoid[D]): Monoid[(A, B, C, D)] = new Monoid[(A, B, C, D)] {
    override def default: (A, B, C, D) = (monoidA.default, monoidB.default, monoidC.default, monoidD.default)

    override def combine(first: (A, B, C, D), second: (A, B, C, D)): (A, B, C, D) = (
      monoidA.combine(first._1, second._1),
      monoidB.combine(first._2, second._2),
      monoidC.combine(first._3, second._3),
      monoidD.combine(first._4, second._4)
    )
  }
}
