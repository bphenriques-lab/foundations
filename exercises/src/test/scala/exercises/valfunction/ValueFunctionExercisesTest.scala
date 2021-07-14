package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.listOfN
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  test("selectDigits length is smaller") {
    forAll { (text: String) =>
      assert(selectDigits(text).length <= text.length)
    }
  }

  test("selectDigits only contains digits") {
    forAll { (text: String) =>
      selectDigits(text).foreach(c => assert(c.isDigit))
    }
  }

  test("secret examples") {
    assert(secret("") == "")
    assert(secret("**") == "**")
    assert(secret("hello4world-80") == "**************")
  }

  test("secret is idempotent") {
    val once = secret("abc")
    val twice = secret(secret("abc"))
    assert(once == twice)
  }

  test("isValidUsernameCharacter examples") {
    assert(isValidUsernameCharacter('-'))
    assert(isValidUsernameCharacter('_'))
    assert(!isValidUsernameCharacter('!'))
  }

  test("isValidUsernameCharacter accepts alphaNumeric characters") {
    val usernameCharGen = Gen.oneOf(Char.MinValue to Char.MaxValue).filter(c => c.isLetterOrDigit || c == '-' || c == '_')
    forAll(usernameCharGen) { char => assert(isValidUsernameCharacter(char)) }
  }

  test("isValidUsername examples") {
    assert(isValidUsername("bAnA3434-"))
    assert(isValidUsername("b___AnA--3434-"))
    assert(!isValidUsername("bananas!00"))
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("Point.isPositive ") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point(x.max(0), y.max(0), z.max(0)).isPositive) // abs is not good due to edge-cases with Int.MinValue
    }
  }

  test("Point.isPositive with positive generator") {
    forAll(Gen.posNum[Int], Gen.posNum[Int], Gen.posNum[Int]) { (x: Int, y: Int, z: Int) =>
      assert(Point(x, y, z).isPositive)
    }
  }

  // TODO: Why Gen.posNum[Int].filter does not work?
  lazy val evenGenerator = for {
    x <- Gen.choose(0, 1000)
  } yield x * 2

  lazy val oddGenerator = for {
    x <- Gen.choose(0, 1000)
  } yield x * 2 + 1

  test("Point.isEven returns true when all are even") {
    forAll(evenGenerator, evenGenerator, evenGenerator) { (x: Int, y: Int, z: Int) =>
      assert(Point(x, y, z).isEven)
    }
  }

  test("Point.isEven returns false when some are odd") {
    forAll(oddGenerator, evenGenerator, evenGenerator) { (x: Int, y: Int, z: Int) =>
      assert(!Point(x, y, z).isEven)
    }

    forAll(evenGenerator, oddGenerator, evenGenerator) { (x: Int, y: Int, z: Int) =>
      assert(!Point(x, y, z).isEven)
    }

    forAll(evenGenerator, evenGenerator, oddGenerator) { (x: Int, y: Int, z: Int) =>
      assert(!Point(x, y, z).isEven)
    }
  }

  test("Point.forAll") {
    forAll { (x: Int, y: Int, z: Int, predicate: Int => Boolean) =>
      assert(Point(x, y, z).forAll(_ => true))
      assert(!Point(x, y, z).forAll(_ => false))
      assert(Point(x, y, z).forAll(predicate) == List(x, y, z).forall(predicate))
    }
  }
}
