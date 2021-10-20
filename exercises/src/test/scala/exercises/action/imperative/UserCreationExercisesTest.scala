package exercises.action.imperative

import exercises.action.DateGenerator._
import exercises.action.imperative.UserCreationExercises._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Instant, LocalDate}
import scala.collection.mutable.ListBuffer
import scala.util.Try

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.imperative.UserCreationExercisesTest
class UserCreationExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("parseYesNo") {
    assertThrows[IllegalArgumentException](parseYesNo("y"))
    assertThrows[IllegalArgumentException](parseYesNo("n"))
    assertThrows[IllegalArgumentException](parseYesNo(" "))
    assert(parseYesNo("Y"))
    assert(!parseYesNo("N"))
  }

  test("readSubscribeToMailingList example") {
    forAll { (yesNo: Boolean) =>
      val inputs  = ListBuffer(formatYesNo(yesNo))
      val outputs = ListBuffer.empty[String]
      val console = Console.mock(inputs, outputs)
      val result  = new UserCreationExercises(console, Clock.system).readSubscribeToMailingList()

      assert(result == yesNo)
      assert(outputs.toList == List("Would you like to subscribe to our mailing list? [Y/N]"))
    }
  }

  test("readSubscribeToMailingList example failure") {
    val console = Console.mock(ListBuffer("Never"), ListBuffer())
    val result  = Try(new UserCreationExercises(console, Clock.system).readSubscribeToMailingList())

    assert(result.isFailure)
  }

  test("readDateOfBirth example success") {
    forAll { (date: LocalDate) =>
      val console = Console.mock(ListBuffer(dateOfBirthFormatter.format(date)), ListBuffer())
      val result  = new UserCreationExercises(console, Clock.system).readDateOfBirth()

      assert(result == date)
    }
  }

  test("readDateOfBirth example failure") {
    val console = Console.mock(ListBuffer("21/07/1986"), ListBuffer())
    val result  = Try(new UserCreationExercises(console, Clock.system).readDateOfBirth())

    assert(result.isFailure)
  }

  test("readUser example") {
    val inputs  = ListBuffer("Eda", "18-03-2001", "Y")
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(inputs, outputs)
    val now = Instant.now()
    val result  = new UserCreationExercises(console, Clock.constant(now)).readUser()

    val expected = User(
      name = "Eda",
      dateOfBirth = LocalDate.of(2001, 3, 18),
      subscribedToMailingList = true,
      createdAt = now
    )

    assert(result == expected)
  }

  //////////////////////////////////////////////
  // PART 2: Error handling
  //////////////////////////////////////////////

  test("readSubscribeToMailingListRetry negative maxAttempt") {
    val console = Console.mock(ListBuffer.empty[String], ListBuffer.empty[String])
    val result  = Try(new UserCreationExercises(console, Clock.system).readSubscribeToMailingListRetry(maxAttempt = -1))

    assert(result.isFailure)
  }

  test("readSubscribeToMailingListRetry example success") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("Never", "N"), outputs)
    val result  = new UserCreationExercises(console, Clock.system).readSubscribeToMailingListRetry(maxAttempt = 2)

    assert(result == false)
    assert(
      outputs.toList == List(
        "Would you like to subscribe to our mailing list? [Y/N]",
        """Incorrect format, enter "Y" for Yes or "N" for "No"""",
        "Would you like to subscribe to our mailing list? [Y/N]"
      )
    )
  }

  test("readSubscribeToMailingListRetry example invalid input") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("Never"), outputs)
    val result  = Try(new UserCreationExercises(console, Clock.system).readSubscribeToMailingListRetry(maxAttempt = 1))

    assert(result.isFailure)
    assert(
      outputs.toList == List(
        "Would you like to subscribe to our mailing list? [Y/N]",
        """Incorrect format, enter "Y" for Yes or "N" for "No""""
      )
    )

    // check that the error message is the same as `readSubscribeToMailingList`
    val console2 = Console.mock(ListBuffer("Never"), ListBuffer.empty[String])
    val result2  = Try(new UserCreationExercises(console2, Clock.system).readSubscribeToMailingList())
    assert(result.failed.get.getMessage == result2.failed.get.getMessage)
  }

  test("readDateOfBirthRetry negative maxAttempt") {
    val console = Console.mock(ListBuffer.empty[String], ListBuffer.empty[String])
    val result  = Try(new UserCreationExercises(console, Clock.system).readSubscribeToMailingListRetry(maxAttempt = -1))

    assert(result.isFailure)
  }

  test("readDateOfBirthRetry example success") {
    val outputs = ListBuffer.empty[String]
    val console = Console.mock(ListBuffer("July 21st 1986", "21-07-1986"), outputs)
    val result  = new UserCreationExercises(console, Clock.system).readDateOfBirthRetry(maxAttempt = 2)

    assert(result == LocalDate.of(1986, 7, 21))
    assert(
      outputs.toList == List(
        """What's your date of birth? [dd-mm-yyyy]""",
        """Incorrect format, for example enter "18-03-2001" for 18th of March 2001""",
        """What's your date of birth? [dd-mm-yyyy]"""
      )
    )
  }

  test("readDateOfBirthRetry example failure") {
    val outputs        = ListBuffer.empty[String]
    val invalidAttempt = "July 21st 1986"
    val console        = Console.mock(ListBuffer(invalidAttempt), outputs)
    val result         = Try(new UserCreationExercises(console, Clock.system).readDateOfBirthRetry(maxAttempt = 1))

    assert(result.isFailure)
    assert(
      outputs.toList == List(
        """What's your date of birth? [dd-mm-yyyy]""",
        """Incorrect format, for example enter "18-03-2001" for 18th of March 2001"""
      )
    )
  }

}
