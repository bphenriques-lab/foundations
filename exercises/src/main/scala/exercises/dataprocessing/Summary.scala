package exercises.dataprocessing

case class Summary(
  min: Option[Sample], // Sample with lowest temperature
  max: Option[Sample], // Sample with highest temperature
  sum: Double, // sum of all temperatures in Fahrenheit
  size: Int // number of Samples
) {

  def average: Option[Double] =
    Option.unless(size == 0)(sum / size)

  override def toString: String =
    f"Summary(avg = ${average.getOrElse(0.0)}%.2f, " +
      s"size = $size,\n  " +
      s"min = $min,\n  " +
      s"max = $max\n)"
}

object Summary {
  implicit val sampleSort: Ordering[Sample] = new Ordering[Sample] {
    override def compare(x: Sample, y: Sample): Int = x.temperatureFahrenheit.compare(y.temperatureFahrenheit)
  }

  // Can this be implemented as Monoid.zip4?
  val monoid: Monoid[Summary] = new Monoid[Summary] {
    override def default: Summary = Summary(None, None, 0.0, 0)

    override def combine(first: Summary, second: Summary): Summary =
      Summary(
        min = Monoid.minOption.combine(first.min, second.min),
        max = Monoid.maxOption.combine(first.max, second.max),
        sum = first.sum + second.sum,
        size = first.size + second.size
      )
  }
}
