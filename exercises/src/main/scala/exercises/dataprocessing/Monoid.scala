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

  def zip[A, B](monoidA: Monoid[A], monoidB: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def default: (A, B) = (monoidA.default, monoidB.default)

    override def combine(first: (A, B), second: (A, B)): (A, B) = (
      monoidA.combine(first._1, second._1),
      monoidB.combine(first._2, second._2)
    )
  }
}
