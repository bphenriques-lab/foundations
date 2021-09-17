package exercises.dataprocessing

import exercises.dataprocessing.TemperatureExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

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

  ignore("summary is consistent between implementations") {
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

  val genInt: Gen[Int]              = Gen.choose(Int.MinValue, Int.MaxValue)
  val genDouble: Gen[Double]        = Gen.choose(Float.MinValue, Float.MaxValue).map(x => x: Double)

  checkMonoid("sumInt", Monoid.sumInt, genInt)
  checkMonoid("sumDouble", Monoid.sumDouble, genDouble)

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
