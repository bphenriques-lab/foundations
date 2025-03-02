package exercises.errorhandling.either

import exercises.errorhandling.option.OptionExercises.{Email, User, UserId}
import exercises.errorhandling.either.EitherExercises1.UserEmailError._

object EitherExercises1 {

  // 1. Implement `getUserEmail` which looks up the email address of a user.
  // The email may be missing if:
  // * the user doesn't exist, or
  // * the user exists but doesn't have an email address
  // For example,
  // val users = Map(
  //   222 -> User(222, "john" , Admin, Some("j@x.com")),
  //   123 -> User(123, "elisa", Admin, Some("e@y.com")),
  //   444 -> User(444, "bob"  , Admin, None)
  // )
  // getUserEmail(123, users) == Right("e@y.com")
  // getUserEmail(111, users) == Left("User 111 is missing")
  // getUserEmail(444, users) == Left("User 444 has no email address")
  def getUserEmail(userId: UserId, users: Map[UserId, User]): Either[UserEmailError, Email] =
    for {
      user  <- users.get(userId).toRight(UserNotFound(userId))
      email <- user.email.toRight(EmailNotFound(userId))
    } yield email

  // 2. Refactor `getUserEmail` so that it uses an `UserEmailError` instead of `String`
  // in the error channel.
  sealed trait UserEmailError {
    def errorMessage: String
  }
  object UserEmailError {
    case class UserNotFound(userId: UserId)  extends UserEmailError {
      override def errorMessage: String = s"User ${userId.value} is missing"
    }
    case class EmailNotFound(userId: UserId) extends UserEmailError {
      override def errorMessage: String = s"User ${userId.value} has no email address"
    }
  }

  // In Scala 3,
  // enum UserEmailError {
  //   case UserNotFound, EmailNotFound
  // }
}
