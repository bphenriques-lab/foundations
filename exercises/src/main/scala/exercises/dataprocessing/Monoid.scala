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
}
