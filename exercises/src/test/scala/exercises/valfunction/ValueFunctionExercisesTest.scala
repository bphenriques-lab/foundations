package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalacheck.Gen
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

  // TODO Use custom generator
  test("isValidUsernameCharacter accepts alphaNumeric characters") {
    forAll(Gen.alphaNumChar) { char => assert(isValidUsernameCharacter(char)) }
  }

  test("isValidUsername examples") {
    assert(isValidUsername("bAnA3434-"))
    assert(isValidUsername("b___AnA--3434-"))
    assert(!isValidUsername("bananas!00"))
  }

  // TODO Use custom generator
  test("isValidUsername accepts alphaNumeric strings") {
    forAll(Gen.alphaNumStr) { str => assert(isValidUsername(str)) }
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////

  test("Point.isPositive") {
    forAll { (x: Int, y: Int, z: Int) =>
      assert(Point(x.max(0), y.max(0), z.max(0)).isPositive) // abs is not good due to edge-cases with Int.MinValue
    }
  }
}
