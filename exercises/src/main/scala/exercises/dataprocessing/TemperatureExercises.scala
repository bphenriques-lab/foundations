package exercises.dataprocessing

object TemperatureExercises {
  // b. Implement `minSampleByTemperature` which finds the `Sample` with the coldest temperature.
  // `minSampleByTemperature` should work as follow:
  // Step 1: Find the local minimums (for each partition the `Sample` with the coldest temperature).
  // Step 2: Find the minimum value among the local minimums.
  // Note: We'll write test in the file `ParListTest.scala`
  def minSampleByTemperature(samples: ParList[Sample]): Option[Sample] = {
    def minTemperatureList(partition: List[Sample]): Option[Sample] = partition.minOption

    samples.partitions.flatMap(minTemperatureList).minOption
  }

  // c. Implement `averageTemperature` which finds the average temperature across all `Samples`.
  // `averageTemperature` should work as follow:
  // Step 1: Compute the sum of all samples temperatures
  //   a) Compute the sum per partition
  //   b) Sum-up the sum of each partition
  // Step 2: Compute the size of the dataset
  //   a) Compute the size of each partition
  //   b) Sum-up the size of each partition
  // Step 3: Divide the total temperature by the size of dataset.
  // In case the input `ParList` is empty we return `None`.
  // Bonus: Can you calculate the size and sum in one go?
  // Answer: I believe so, with foldLeft where the initial state is a Pair<Sum,Size>.
  // Alternatively we can use running average.
  def averageTemperature(samples: ParList[Sample]): Option[Double] = {
    val numberSamples = samples.size
    val sum           = sumTemperatures(samples)
    Option.unless(numberSamples == 0)(sum / numberSamples)
  }

  def averageTemperatureV2(samples: ParList[Sample]): Option[Double] = {
    def partitionSumSize(partition: List[Sample]) = partition.foldLeft[(Double, Int)]((0.0, 0)) {
      case ((sum, size), sample) =>
        (sum + sample.temperatureFahrenheit, size + 1)
    }

    def tuplesSum(tuples: List[(Double, Int)]): (Double, Int) = tuples.foldLeft[(Double, Int)]((0.0, 0)) {
      case ((totalSum, totalSize), (sum, size)) =>
        (totalSum + sum, totalSize + size)
    }

    val (sum, numberSamples) = tuplesSum(samples.partitions.map(p => partitionSumSize(p)))
    Option.unless(numberSamples == 0)(sum / numberSamples)
  }

  def averageTemperatureV3(samples: ParList[Sample]): Option[Double] = {
    val numberSamples = samples.sizeV2
    val sum           = sumTemperaturesV2(samples)
    Option.unless(numberSamples == 0)(sum / numberSamples)
  }

  def averageTemperatureV4(samples: ParList[Sample]): Option[Double] = {
    val (numberSamples, sum) = samples
      .map(sample => (1, sample.temperatureFahrenheit))
      .monoFoldLeft(Monoid.zip(Monoid.sumInt, Monoid.sumDouble))

    Option.unless(numberSamples == 0)(sum / numberSamples)
  }

  def sumTemperatures(samples: ParList[Sample]): Double =
    samples.partitions
      .map(_.map(_.temperatureFahrenheit).sum)
      .sum

  def sumTemperaturesV2(samples: ParList[Sample]): Double =
    samples.map(_.temperatureFahrenheit).monoFoldLeft(Monoid.sumDouble)

  // `summaryList` iterate 4 times over `samples`, one for each field.
  def summaryList(samples: List[Sample]): Summary =
    Summary(
      min = samples.minByOption(_.temperatureFahrenheit),
      max = samples.maxByOption(_.temperatureFahrenheit),
      sum = samples.foldLeft(0.0)((state, sample) => state + sample.temperatureFahrenheit),
      size = samples.size
    )

  def summaryListOnePass(samples: List[Sample]): Summary =
    samples.foldLeft(
      Summary(
        min = None,
        max = None,
        sum = 0.0,
        size = 0
      )
    )((state, sample) =>
      Summary(
        min = state.min.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit <= sample.temperatureFahrenheit) Some(current)
          else Some(sample)
        ),
        max = state.max.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit >= sample.temperatureFahrenheit) Some(current)
          else Some(sample)
        ),
        sum = state.sum + sample.temperatureFahrenheit,
        size = state.size + 1
      )
    )

  // Implement `summaryParList` by calling `parFoldMap` once for each field of Summary.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParList`
  // should return the same result as `summaryList`
  def summaryParList(samples: ParList[Sample]): Summary =
    Summary(
      min = ???,
      max = ???,
      sum = ???,
      size = ???
    )

  // Implement `summaryParListOnePass` using `parFoldMap` only ONCE.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParListOnePass`
  // should return the same result as `summaryList`
  def summaryParListOnePass(samples: ParList[Sample]): Summary =
    ???
}
