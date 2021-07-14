package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    assert(Pair(0, 1).swap == Pair(1, 0))
  }

  // Question: Is this enough coverage?
  // https://fp-tower.slack.com/archives/G01EQCN2M25/p1626273276068600
  test("Option A: Pair map using identity") {
    assert(Pair("123", "456789").map(identity) == Pair("123", "456789"))
  }

  test("Option B: Pair map using custom function") {
    assert(Pair("123", "456789").map(_.length) == Pair(3, 6))
  }

  test("Pair decoded") {
    assert(GenericFunctionExercises.decoded == Pair("Programming", "Functional"))
  }

  test("Pair zipWith") {}

  test("Pair productNames") {}

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate &&") {}

  test("Predicate ||") {}

  test("Predicate flip") {}

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {}

  test("JsonDecoder LocalDate") {}

  test("JsonDecoder weirdLocalDateDecoder") {}

}
