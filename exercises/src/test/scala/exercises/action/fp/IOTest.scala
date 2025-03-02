package exercises.action.fp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.fp.IOTest
class IOTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("apply is lazy and repeatable") {
    var counter = 0

    val action = IO(counter += 1)
    assert(counter == 0) // nothing happened

    action.unsafeRun()
    assert(counter == 1)

    action.unsafeRun()
    assert(counter == 2)
  }

  test("dispatch is lazy and repeatable") {
    var counter = 0

    val action = IO.dispatch(counter += 1)(ExecutionContext.global)
    assert(counter == 0) // nothing happened

    action.unsafeRun()
    assert(counter == 1)

    action.unsafeRun()
    assert(counter == 2)
  }

  // replace `ignore` by `test` to enable this test
  test("andThen") {
    var counter = 0

    val first  = IO(counter += 1)
    val second = IO(counter *= 2)

    val action = first.andThen(second)
    assert(counter == 0) // nothing happened before unsafeRun

    action.unsafeRun()
    assert(counter == 2) // first and second were executed in the expected order
  }

  test("map") {
    var counter = 0

    val first  = IO(counter += 1)
    val action = first.map(_ => "Hello")
    assert(counter == 0) // nothing happened before unsafeRun

    action.unsafeRun()
    assert(counter == 1) // first was executed
  }

  test("flatMap") {
    var counter = 0

    val first  = IO(counter += 1)
    val second = IO(counter *= 2)

    val action = first.flatMap(_ => second)
    assert(counter == 0) // nothing happened before unsafeRun

    action.unsafeRun()
    assert(counter == 2) // first and second were executed
  }

  //////////////////////////////////////////////
  // PART 3: Error handling
  //////////////////////////////////////////////

  test("onError success") {
    var counter = 0

    val action = IO { counter += 1; "" }.onError(_ => IO(counter *= 2))
    assert(counter == 0) // nothing happened before unsafeRun

    val result = action.attempt.unsafeRun()
    assert(counter == 1) // first action was executed but not the callback
    assert(result == Success(""))
  }

  test("onError failure") {
    var counter = 0
    val error1  = new Exception("Boom 1")

    val action = IO(throw error1).onError(_ => IO(counter += 1))
    assert(counter == 0) // nothing happened before unsafeRun

    val result = Try(action.unsafeRun())
    assert(counter == 1) // callback was executed
    assert(result == Failure(error1))
  }

  test("onError with two failures") {
    var counter = 0
    val error1  = new Exception("Boom 1")
    val error2  = new Exception("Boom 2")

    val action = IO(throw error1)
      .onError(_ => IO(counter += 1).andThen(IO(throw error2)))
    assert(counter == 0) // nothing happened before unsafeRun

    val result = Try(action.unsafeRun())
    assert(counter == 1)              // callback was executed
    assert(result == Failure(error1)) // callback error was swallowed
  }

  test("retry, maxAttempt must be greater than 0") {
    val retryAction = IO(1).retry(0)
    val result      = Try(retryAction.unsafeRun())

    assert(result.isFailure)
  }

  test("retry until action succeeds") {
    var counter = 0
    val error   = new Exception("Boom")
    val action = IO {
      counter += 1
      if (counter >= 3) "Hello"
      else throw error
    }

    val retryAction = action.retry(5)
    assert(counter == 0)

    val result = Try(retryAction.unsafeRun())
    assert(result == Success("Hello"))
    assert(counter == 3)
  }

  test("retry fails if maxAttempt is too low") {
    var counter = 0
    val error   = new Exception("Boom")
    val action = IO {
      counter += 1
      if (counter >= 3) "Hello"
      else throw error
    }

    val retryAction = action.retry(2)
    assert(counter == 0)

    val result = Try(retryAction.unsafeRun())
    assert(result == Failure(error))
    assert(counter == 2)
  }

  //////////////////////////////////////////////
  // PART 4: IO clean-up
  //////////////////////////////////////////////

  test("attempt success") {
    var counter = 0

    val action = IO(counter += 1).attempt
    assert(counter == 0) // nothing happened before unsafeRun

    val result = action.unsafeRun()
    assert(counter == 1) // action was executed only once
    assert(result.isSuccess)
  }

  test("attempt failure") {
    var counter = 0

    val exception = new Exception("Boom")
    val action    = IO { counter += 1; throw exception }.attempt
    assert(counter == 0) // nothing happened before unsafeRun

    val result = action.unsafeRun()
    assert(counter == 1) // action was executed only once
    assert(result == Failure(exception))
  }

  test("handleErrorWith success") {
    var counter = 0

    val first  = IO(counter += 1) andThen IO("A")
    val second = IO(counter *= 2) andThen IO("B")
    val action = first.handleErrorWith(_ => second)
    assert(counter == 0) // nothing happened before unsafeRun

    val result = action.unsafeRun()
    assert(result == "A")
    assert(counter == 1) // only first is executed
  }

  test("handleErrorWith failure") {
    var counter = 0

    val first  = IO(counter += 1) andThen IO.fail[Unit](new Exception("Boom"))
    val second = IO(counter *= 2)
    val action = first.handleErrorWith(_ => second)
    assert(counter == 0) // nothing happened before unsafeRun

    action.unsafeRun()
    assert(counter == 2) // first and second were executed in the expected order
  }

  //////////////////////////////////////////////
  // Search Flight Exercises
  //////////////////////////////////////////////

  test("sequence") {
    var counter = 0

    val action = IO.sequence(
      List(
        IO { counter += 2; counter },
        IO { counter *= 3; counter },
        IO { counter -= 1; counter }
      )
    )
    assert(counter == 0)

    assert(action.unsafeRun() == List(2, 6, 5))
    assert(counter == 5)
  }

  test("traverse") {
    var counter = 0

    val values: List[Int => Int] = List(_ + 2, _ * 3, _ - 1)

    val action = IO.traverse(values)(f => IO { counter = f(counter); counter })
    assert(counter == 0)

    assert(action.unsafeRun() == List(2, 6, 5))
    assert(counter == 5)
  }

  //////////////////////////////////////////////
  // Concurrent IO
  //////////////////////////////////////////////

  // flaky
  test("parZip second faster than first") {
    var counter = 0

    val first  = IO.sleep(10.millis) *> IO { counter += 1; counter }
    val second = IO { counter *= 2; counter }

    val action = first.parZip(second)(global)
    assert(counter == 0)

    assert(action.unsafeRun() == (1, 0))
    assert(counter == 1)
  }

  // flaky
  test("parZip first faster than second") {
    var counter = 0

    val first  = IO { counter += 1; counter }
    val second = IO.sleep(10.millis) *> IO { counter *= 2; counter }

    val action = first.parZip(second)(global)
    assert(counter == 0)

    assert(action.unsafeRun() == (1, 2))
    assert(counter == 2)
  }

  // flaky
  test("parSequence") {
    var counter = 0

    val action = List(
      IO.sleep(10.millis) *> IO { counter *= 3; counter },
      IO { counter += 2; counter },
      IO.sleep(50.millis) *> IO { counter -= 1; counter }
    ).parSequence(global)
    assert(counter == 0)

    assert(action.unsafeRun() == List(6, 2, 5))
    assert(counter == 5)
  }

  // flaky
  test("parTraverse") {
    var counter = 0

    def sleepAndIncrement(sleepMillis: Int): IO[Int] =
      IO.sleep(sleepMillis.millis) *> IO { counter += 1; counter }

    val action = List(10, 0, 50).parTraverse(sleepAndIncrement)(global)
    assert(counter == 0)

    assert(action.unsafeRun() == List(2, 1, 3))
    assert(counter == 3)
  }

}
