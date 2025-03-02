package exercises.generic

import exercises.generic.GenericFunctionExercises.Predicate.{False, True}
import exercises.generic.GenericFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.LocalDate
import java.time.format.DateTimeFormatter
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

  test("Pair zipWith") {
    assert(Pair(0, 2).zipWith(Pair(3, 4))((x, y) => x + y) == Pair(3, 6))
  }

  test("Pair productNames") {
    assert(GenericFunctionExercises.products == Pair(Product("Coffee", 2.5), Product("Plane ticket", 329.99)))
  }

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate &&") {
    forAll { (n: Int, f1: Int => Boolean) =>
      val p1 = Predicate(f1)

      assert((p1 && False)(n) == false)
      assert((p1 && True)(n) == p1(n))
    }
  }

  test("Predicate ||") {
    forAll { (n: Int, f1: Int => Boolean) =>
      val p1 = Predicate(f1)

      assert((p1 || False)(n) == p1(n))
      assert((p1 || True)(n) == true)
    }
  }

  test("Predicate flip") {
    forAll { (n: Int) =>
      assert((Predicate.True flip)(n) == false)
      assert((Predicate.False flip)(n) == true)
    }
  }

  test("Predicate Valid User") {
    assert(isValidUser(User("John", 20)) == true)
    assert(isValidUser(User("John", 17)) == false)
    assert(isValidUser(User("john", 20)) == false)
    assert(isValidUser(User("x"   , 23)) == false)
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    assert(userIdDecoder.decode("1234") == UserId(1234))
    assert(userIdDecoder.decode("-1234") == UserId(-1234))
    assert(Try(userIdDecoder.decode("abc")).isFailure)
  }

  test("JsonDecoder UserId round-trip") {
    forAll { (v: Int) =>
      assert(userIdDecoder.decode(v.toString) == UserId(v))
    }
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoderString.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26))
    assert(Try(localDateDecoderString.decode("2020-03-26")).isFailure)
    assert(Try(localDateDecoderString.decode("hello")).isFailure)
  }

  test("JsonDecoder LocalDate round-trip") {
    val genLocalDate: Gen[LocalDate] = Gen
      .choose(LocalDate.MIN.toEpochDay, LocalDate.MAX.toEpochDay)
      .map(LocalDate.ofEpochDay)

    forAll(genLocalDate) { (v: LocalDate) =>
      assert(localDateDecoderString.decode("\"" + DateTimeFormatter.ISO_LOCAL_DATE.format(v) + "\"") == v)
    }
  }

  test("JsonDecoder weirdLocalDateDecoder") {
    assert(weirdLocalDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020,3,26))
    assert(weirdLocalDateDecoder.decode("18347")          == LocalDate.of(2020,3,26))
    assert(Try(weirdLocalDateDecoder.decode("hello")).isFailure)
  }

  test("JsonDecoder Option") {
    assert(optionDecoder(stringDecoder).decode("null") == None)
    assert(optionDecoder(stringDecoder).decode("\"1234\"") == Some("1234"))
  }

  ////////////////////////////
  // Exercise Extension: JsonDecoderV2
  ////////////////////////////

  test("JsonDecoderV2 LocalDate") {
    assert(JsonDecoderV2.localDateDecoderStringV2.decode("\"2020-03-26\"") == Right(LocalDate.of(2020,3,26)))
    assert(JsonDecoderV2.localDateDecoderStringV2.decode("2020-03-26").isLeft)
    assert(JsonDecoderV2.localDateDecoderStringV2.decode("hello").isLeft)
  }

  test("JsonDecoderV2 LocalDate round-trip") {
    val genLocalDate: Gen[LocalDate] = Gen
      .choose(LocalDate.MIN.toEpochDay, LocalDate.MAX.toEpochDay)
      .map(LocalDate.ofEpochDay)

    forAll(genLocalDate) { (v: LocalDate) =>
      assert(JsonDecoderV2.localDateDecoderStringV2.decode("\"" + DateTimeFormatter.ISO_LOCAL_DATE.format(v) + "\"") == Right(v))
    }
  }

  test("JsonDecoderV2 weirdLocalDateDecoder") {
    assert(JsonDecoderV2.weirdLocalDateDecoderV2.decode("\"2020-03-26\"") == Right(LocalDate.of(2020,3,26)))
    assert(JsonDecoderV2.weirdLocalDateDecoderV2.decode("18347")          == Right(LocalDate.of(2020,3,26)))
    assert(JsonDecoderV2.weirdLocalDateDecoderV2.decode("hello").isLeft)
  }

  test("JsonDecoderV2 Option") {
    assert(JsonDecoderV2.optionDecoderV2(JsonDecoderV2.stringDecoderV2).decode("null") == Right(None))
    assert(JsonDecoderV2.optionDecoderV2(JsonDecoderV2.stringDecoderV2).decode("\"1234\"") == Right(Some("1234")))
  }
}
