package exercises.dataprocessing

import exercises.dataprocessing.TemperatureExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.ExecutionContext.Implicits.global

class ParListTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {

  test("minSampleByTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(
      minSampleByTemperature(parSamples) ==
        Some(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1))
    )
  }

  test("minSampleByTemperature consistent with List minByOption") {
    forAll { (samples: ParList[Sample]) =>
      assert(
        TemperatureExercises.minSampleByTemperature(samples) ==
          samples.partitions.flatten.minByOption(_.temperatureCelsius)
      )
    }
  }

  test("minSampleByTemperature returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples)

      for {
        coldest <- minSampleByTemperature(parSamples)
        sample <- samples
      } assert(coldest.temperatureFahrenheit <= sample.temperatureFahrenheit)
    }
  }

  test("minSampleByTemperatureV2 example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(
      minSampleByTemperatureV2(parSamples) ==
        Some(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1))
    )
  }

  test("minSampleByTemperatureV2 consistent with List minByOption") {
    forAll { (samples: ParList[Sample]) =>
      assert(
        TemperatureExercises.minSampleByTemperatureV2(samples) ==
          samples.partitions.flatten.minByOption(_.temperatureCelsius)
      )
    }
  }

  test("minSampleByTemperatureV2 returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples)

      for {
        coldest <- minSampleByTemperatureV2(parSamples)
        sample <- samples
      } assert(coldest.temperatureFahrenheit <= sample.temperatureFahrenheit)
    }
  }

  test("averageTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(averageTemperature(parSamples) == Some(53.6))
  }

  test("averageTemperature: min <= avg <= max ") {
    forAll { (samples: ParList[Sample]) =>
      val optMin = samples.partitions.flatten.map(_.temperatureFahrenheit).minOption
      val optMax = samples.partitions.flatten.map(_.temperatureFahrenheit).maxOption
      val optAvg = TemperatureExercises.averageTemperature(samples)

      (optMin, optMax, optAvg) match {
        case (None, None, None) => succeed
        case (Some(min), Some(max), Some(avg)) =>
          assert(min <= avg)
          assert(avg <= max)
        case _ => fail(s"inconsistent $optMin, $optMax, $optAvg")
      }
    }
  }

  test("averageTemperatureV2 example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(averageTemperatureV2(parSamples) == Some(53.6))
  }

  test("averageTemperatureV2: min <= avg <= max ") {
    forAll { (samples: ParList[Sample]) =>
      val optMin = samples.partitions.flatten.map(_.temperatureFahrenheit).minOption
      val optMax = samples.partitions.flatten.map(_.temperatureFahrenheit).maxOption
      val optAvg = TemperatureExercises.averageTemperatureV2(samples)

      (optMin, optMax, optAvg) match {
        case (None, None, None) => succeed
        case (Some(min), Some(max), Some(avg)) =>
          assert(min <= avg)
          assert(avg <= max)
        case _ => fail(s"inconsistent $optMin, $optMax, $optAvg")
      }
    }
  }

  test("averageTemperatureV3 example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(averageTemperatureV3(parSamples) == Some(53.6))
  }

  test("averageTemperatureV3: min <= avg <= max ") {
    forAll { (samples: ParList[Sample]) =>
      val optMin = samples.partitions.flatten.map(_.temperatureFahrenheit).minOption
      val optMax = samples.partitions.flatten.map(_.temperatureFahrenheit).maxOption
      val optAvg = TemperatureExercises.averageTemperatureV3(samples)

      (optMin, optMax, optAvg) match {
        case (None, None, None) => succeed
        case (Some(min), Some(max), Some(avg)) =>
          assert(min <= avg)
          assert(avg <= max)
        case _ => fail(s"inconsistent $optMin, $optMax, $optAvg")
      }
    }
  }

  test("averageTemperatureV4 example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(averageTemperatureV4(parSamples) == Some(53.6))
  }

  test("averageTemperatureV4: min <= avg <= max ") {
    forAll { (samples: ParList[Sample]) =>
      val optMin = samples.partitions.flatten.map(_.temperatureFahrenheit).minOption
      val optMax = samples.partitions.flatten.map(_.temperatureFahrenheit).maxOption
      val optAvg = TemperatureExercises.averageTemperatureV4(samples)

      (optMin, optMax, optAvg) match {
        case (None, None, None) => succeed
        case (Some(min), Some(max), Some(avg)) =>
          assert(min <= avg)
          assert(avg <= max)
        case _ => fail(s"inconsistent $optMin, $optMax, $optAvg")
      }
    }
  }

  test("averageTemperatureV5 example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(averageTemperatureV5(parSamples) == Some(53.6))
  }

  test("averageTemperatureV5: min <= avg <= max ") {
    forAll { (samples: ParList[Sample]) =>
      val optMin = samples.partitions.flatten.map(_.temperatureFahrenheit).minOption
      val optMax = samples.partitions.flatten.map(_.temperatureFahrenheit).maxOption
      val optAvg = TemperatureExercises.averageTemperatureV5(samples)

      (optMin, optMax, optAvg) match {
        case (None, None, None) => succeed
        case (Some(min), Some(max), Some(avg)) =>
          assert(min <= avg)
          assert(avg <= max)
        case _ => fail(s"inconsistent $optMin, $optMax, $optAvg")
      }
    }
  }

  test("summary is consistent between implementations") {
    forAll { (samples: ParList[Sample]) =>
      val samplesList = samples.partitions.flatten
      val reference = summaryList(samples.partitions.flatten)
      List(
        summaryListOnePass(samplesList),
        summaryParList(samples),
        summaryParListOnePass(samples)
      ).foreach { other =>
        assert(reference.size == other.size)
        assert((reference.sum - other.sum).abs < 0.00001)
        assert(reference.min == other.min)
        assert(reference.max == other.max)
      }
    }
  }

  test("monoFoldLeft sum") {
    forAll { (numbers: ParList[Int]) =>
      assert(numbers.monoFoldLeft(0)(_ + _) == numbers.toList.sum)
    }
  }

  test("foldMap is consistent with monoFoldLeft") {
    forAll { (numbers: ParList[Int]) =>
      assert(numbers.foldMap(identity)(Monoid.sumInt) == numbers.monoFoldLeft(Monoid.sumInt))
    }
  }

  test("parFoldMap is consistent with foldMap") {
    forAll { (numbers: ParList[Int]) =>
      assert(numbers.parFoldMap(identity)(Monoid.sumInt) == numbers.foldMap(identity)(Monoid.sumInt))
    }
  }

  val genInt: Gen[Int]              = Gen.choose(Int.MinValue, Int.MaxValue)
  val genDouble: Gen[Double]        = Gen.choose(Float.MinValue, Float.MaxValue).map(x => x: Double)

  checkMonoid("sumInt", Monoid.sumInt, genInt)
  checkMonoid("sumDouble", Monoid.sumDouble, genDouble)
  checkMonoid("zip", Monoid.zip(Monoid.sumInt, Monoid.sumDouble), Gen.zip(genInt, genDouble))

  // Gen: Explicit
  // Arbitrary: Implicit
  def checkMonoid[A](name: String, monoid: Monoid[A], gen: Gen[A]) = {
    test(s"$name Monoid default is a noop") {
      forAll(gen) { (value: A) =>
        assert(monoid.combine(value, monoid.default) == value)
        assert(monoid.combine(monoid.default, value) == value)
      }
    }
    test(s"$name Monoid combine is associative") {
      forAll(gen, gen, gen) { (first: A, second: A, third: A) =>
        assert(
          monoid.combine(first, monoid.combine(second, third)) ==
            monoid.combine(monoid.combine(first, second), third)
        )
      }
    }
  }
}
