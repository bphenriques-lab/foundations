package exercises.action.fp

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

trait IO[A] {

  // This is the ONLY abstract method of the `IO` trait.
  def unsafeRunAsync(callback: Try[A] => Unit): Unit

  // Executes the action.
  def unsafeRun(): A = {
    var result: Option[Try[A]] = None
    val latch = new CountDownLatch(1)
    unsafeRunAsync { res =>
      result = Some(res)
      latch.countDown()
    }
    latch.await()
    result.get.get
  }

  // Runs the current IO (`this`), discards its result and runs the second IO (`other`).
  // For example,
  // val action1: IO[Unit] = IO(println("Fetching user"))
  // val action2: IO[User] = db.getUser(1234)
  // val action3: IO[User] = action1.andThen(action2)
  // action3.unsafeRun()
  // prints "Fetching user", fetches user 1234 from db and returns it.
  // Note: There is a test for `andThen` in `exercises.action.fp.IOTest`.
  def andThen[Other](other: IO[Other]): IO[Other] =
    flatMap(_ => other)

  // Popular alias for `andThen` (cat-effect, Monix, ZIO).
  // For example,
  // action1 *> action2 *> action3
  // Note: The arrow head points toward the result we keep.
  //       Another popular symbol is <* so that `action1 <* action2`
  //       executes `action1` and then `action2` but returns the result of `action1`
  def *>[Other](other: IO[Other]): IO[Other] =
    andThen(other)

  // Runs the current action (`this`) and update the result with `callback`.
  // For example,
  // IO(1).map(x => x + 1).unsafeRun()
  // returns 2
  // db.getUser(1234).map(_.name).unsafeRun()
  // Fetches the user with id 1234 from the database and returns its name.
  // Note: `callback` is expected to be an FP function (total, deterministic, no action).
  //       Use `flatMap` if `callBack` is not an FP function.
  def map[Next](callBack: A => Next): IO[Next] =
    flatMap(res => IO(callBack(res)))

  // Runs the current action (`this`), if it succeeds passes the result to `callback` and
  // runs the second action.
  // For example,
  // val action = db.getUser(1234).flatMap{ user =>
  //   emailClient.send(user.email, "Welcome to the FP Tower!")
  // }
  // action.unsafeRun()
  // Fetches the user with id 1234 from the database and send them an email using the email
  // address found in the database.
  def flatMap[Next](next: A => IO[Next]): IO[Next] =
    IO.async { callback =>
      this.unsafeRunAsync {
        case Failure(exception) => callback(Failure(exception))
        case Success(value) => next(value).unsafeRunAsync(callback)
      }
    }

  // Runs the current action, if it fails it executes `cleanup` and rethrows the original error.
  // If the current action is a success, it will return the result.
  // For example,
  // def logError(e: Throwable): IO[Unit] =
  //   IO{ println("Got an error: ${e.getMessage}") }
  //
  // IO(1).onError(logError).unsafeRun()
  // returns 1 and nothing is printed to the console
  //
  // IO(throw new Exception("Boom!")).onError(logError).unsafeRun()
  // prints "Got an error: Boom!" and throws new Exception("Boom!")
  def onError[Other](cleanup: Throwable => IO[Other]): IO[A] =
    handleErrorWith(e => cleanup(e).attempt *> IO.fail(e))

  // Retries this action until either:
  // * It succeeds.
  // * Or the number of attempts have been exhausted.
  // For example,
  // var counter = 0
  // val action: IO[String] = {
  //   counter += 1
  //   require(counter >= 3, "Counter is too low")
  //   "Hello"
  // }
  // action.retry(maxAttempt = 5).unsafeRun()
  // Returns "Hello" because `action` fails twice and then succeeds when counter reaches 3.
  // Note: `maxAttempt` must be greater than 0, otherwise the `IO` should fail.
  // Note: `retry` is a no-operation when `maxAttempt` is equal to 1.
  def retry(maxAttempt: Int): IO[A] =
    if (maxAttempt <= 0) IO.fail(new IllegalArgumentException("maxAttempt must be greater than 0"))
    else if (maxAttempt == 1) this
    else handleErrorWith(_ => retry(maxAttempt - 1))

  // Checks if the current IO is a failure or a success.
  // For example,
  // val action: IO[User] = db.getUser(1234)
  // action.attempt.unsafeRun()
  // returns either:
  // 1. Success(User(1234, "Bob", ...)) if `action` was successful or
  // 2. Failure(new Exception("User 1234 not found")) if `action` throws an exception
  def attempt: IO[Try[A]] =
    IO.async { complete =>
      this.unsafeRunAsync(result => complete(Success(result)))
    }

  // If the current IO is a success, do nothing.
  // If the current IO is a failure, execute `callback` and keep its result.
  // For example,
  // val user: User = ...
  // val action: IO[Unit] = closeAccount(user.id).handleErrorWith(e =>
  //   logError(e).andThen(emailClient.send(user.email, "Sorry something went wrong"))
  // )
  def handleErrorWith(callback: Throwable => IO[A]): IO[A] =
    attempt.flatMap {
      case Failure(exception) => callback(exception)
      case Success(value) => IO(value)
    }

  //////////////////////////////////////////////
  // Concurrent IO
  //////////////////////////////////////////////

  // Runs both the current IO and `other` sequentially,
  // then combine their results into a tuple
  def zip[Other](other: IO[Other]): IO[(A, Other)] =
    for {
      first  <- this
      second <- other
    } yield (first, second)

  // Runs both the current IO and `other` concurrently,
  // then combine their results into a tuple
  def parZip[Other](other: IO[Other])(ec: ExecutionContext): IO[(A, Other)] =
    IO.async { callback =>
      val promise1 = Promise[A]()
      val promise2 = Promise[Other]()
      ec.execute(() => this.unsafeRunAsync(promise1.complete))
      ec.execute(() => other.unsafeRunAsync(promise2.complete))

      promise1.future.zip(promise2.future).onComplete(callback)(ec)
    }
}

object IO {

  def async[A](onComplete: (Try[A] => Unit) => Unit): IO[A] = new IO[A] {
    override def unsafeRunAsync(callback: Try[A] => Unit): Unit = onComplete(callback)
  }
  // Constructor for IO. For example,
  // val greeting: IO[Unit] = IO { println("Hello") }
  // greeting.unsafeRun()
  // prints "Hello"
  def apply[A](action: => A): IO[A] =
    async { callback => callback(Try(action)) }

  def dispatch[A](action: => A)(ec: ExecutionContext): IO[A] =
    async { callback => ec.execute(() => callback(Try(action))) }

  // Construct an IO which throws `error` everytime it is called.
  def fail[A](error: Throwable): IO[A] =
    IO(throw error)

  //////////////////////////////////////////////
  // Search Flight Exercises
  //////////////////////////////////////////////

  def sleep(duration: FiniteDuration): IO[Unit] =
    IO(Thread.sleep(duration.toMillis))

  def debug(message: String): IO[Unit] =
    IO(Predef.println(s"[${Thread.currentThread().getName}] " + message))

  // Runs all the actions sequentially (one after the other)
  // and collect the results in the same order.
  // For example,
  // val actions : List[IO[User]] = List(db.getUser(1111), db.getUser(2222), db.getUser(3333))
  // val combined: IO[List[User]] = sequence(actions)
  // combined.unsafeRun
  // fetches user 1111, then fetches user 2222 and finally fetches user 3333.
  // If no error occurs, it returns the users in the same order:
  // List(User(1111, ...), User(2222, ...), User(3333, ...))
  def sequenceV1[A](actions: List[IO[A]]): IO[List[A]] = // why not like this? :thinking: is it due to unsafeRun()?
    IO { actions.map(_.unsafeRun()) }

  def sequence[A](actions: List[IO[A]]): IO[List[A]] =
    actions
      .foldLeft(IO(List.empty[A])) { (state, action) =>
        state.zip(action).map { case (list, a) => a :: list }
      }
      .map(_.reverse) // Optimization b/c adding to the end of the list is more complex


  // `traverse` is a shortcut for `map` followed by `sequence`, similar to how
  // `flatMap`  is a shortcut for `map` followed by `flatten`
  // For example,
  // traverse(List(1111, 2222, 3333))(db.getUser) is equivalent to
  // sequence(List(db.getUser(1111), db.getUser(2222), db.getUser(3333)))
  def traverse[A, B](values: List[A])(action: A => IO[B]): IO[List[B]] =
    sequence(values.map(action))

  //////////////////////////////////////////////
  // Concurrent IO
  //////////////////////////////////////////////

  // Runs all the actions concurrently and collect the results in the same order.
  // For example,
  // val actions : List[IO[User]] = List(db.getUser(1111), db.getUser(2222), db.getUser(3333))
  // val combined: IO[List[User]] = parSequence(actions)
  // combined.unsafeRun
  // sends requests to fetch user 1111, 2222 and 3333 roughly at the same time.
  // If no error occurs, `parSequence` returns the users in the same order:
  // List(User(1111, ...), User(2222, ...), User(3333, ...))
  // Note: You may want to use `parZip` to implement `parSequence`.
  def parSequence[A](actions: List[IO[A]])(ec: ExecutionContext): IO[List[A]] =
    actions
      .foldLeft(IO(List.empty[A])) { (state, action) =>
        state.parZip(action)(ec).map { case (list, a) => a :: list }
      }
      .map(_.reverse) // Optimization b/c adding to the end of the list is more complex

  // `parTraverse` is a shortcut for `map` followed by `parSequence`, similar to how
  // `flatMap`     is a shortcut for `map` followed by `flatten`
  // For example,
  // parTraverse(List(1111, 2222, 3333))(db.getUser) is equivalent to
  // parSequence(List(db.getUser(1111), db.getUser(2222), db.getUser(3333)))
  def parTraverse[A, B](values: List[A])(action: A => IO[B])(ec: ExecutionContext): IO[List[B]] =
    parSequence(values.map(action))(ec)

}
