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

  checkMonoid("sumInt", Monoid.sumInt)

  // Gen: Explicit
  // Arbitrary: Implicit
  def checkMonoid[A: Arbitrary](name: String, monoid: Monoid[A]): Unit = {
    test(s"Monoid $name - combine to be a no-op with default") {
      forAll { (v: A) =>
        assert(monoid.combine(v, monoid.default) == v)
        assert(monoid.combine(monoid.default, v) == v)
      }
    }

    test(s"Monoid $name - combine has to be associative") {
      forAll { (v1: A, v2: A, v3: A) =>
        assert(monoid.combine(v1, monoid.combine(v2, v3)) == monoid.combine(monoid.combine(v1, v2), v3))
      }
    }
  }
}
