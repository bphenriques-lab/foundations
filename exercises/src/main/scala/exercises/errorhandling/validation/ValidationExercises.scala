package exercises.errorhandling.validation

import exercises.errorhandling.NEL
import exercises.errorhandling.validation.ValidationExercises.FormError._

object ValidationExercises {

  case class User(username: Username, countryOfResidence: Country)
  case class Username(value: String)

  sealed abstract class Country(val code: String)
  object Country {
    val all: List[Country] = List(France, Germany, Switzerland, UnitedKingdom)

    case object France        extends Country("FRA")
    case object Germany       extends Country("DEU")
    case object Switzerland   extends Country("CHE")
    case object UnitedKingdom extends Country("GBR")
  }

  sealed trait FormError
  object FormError {
    case class InvalidFormat(input: String)        extends FormError
    case class NotSupported(input: String)         extends FormError
    case class TooSmall(inputLength: Int)          extends FormError
    case class InvalidCharacters(char: List[Char]) extends FormError
  }

  // 1. Copy-paste `validateCountry` from `EitherExercises2` and adapt it to `Validation`.
  // For example,
  // validateCountry("FRA") == Valid(France)
  // validateCountry("UK")  == Invalid(NEL(InvalidFormat("UK")))
  // validateCountry("ARG") == Invalid(NEL(NotSupported("ARG")))
  // Note: You can find several helpers methods in the companion object of Validation,
  //       as well as many extension methods in `package.scala`.
  def validateCountry(countryCode: String): Validation[FormError, Country] =
    for {
      _ <- Validation.cond(
        countryCode.length == 3 && countryCode.forall(char => char.isLetter && char.isUpper),
        countryCode,
        FormError.InvalidFormat(countryCode)
      )
      country <- Country.all.find(_.code == countryCode).toValid(FormError.NotSupported(countryCode))
    } yield country

  // 2. Copy-paste `checkUsernameSize` from `EitherExercises2` and adapt it to `Validation`.
  def checkUsernameSize(username: String): Validation[TooSmall, Unit] =
    Validation.cond(username.length >= 5, (), TooSmall(username.length))

  // 3. Copy-paste `checkUsernameCharacters` from `EitherExercises2` and adapt it to `Validation`.
  def checkUsernameCharacters(username: String): Validation[InvalidCharacters, Unit] =
    username.toList.filterNot(isValidUsernameCharacter) match {
      case Nil               => Validation.Valid()
      case invalidCharacters => Validation.invalid(InvalidCharacters(invalidCharacters))
    }

  def isValidUsernameCharacter(c: Char): Boolean =
    c.isLetter || c.isDigit || c == '_' || c == '-'

  // 4. Implement `validateUsername` which verifies that the username size and content
  // is correct according to `checkUsernameSize` and `checkUsernameCharacters`.
  // If the username is both too small and contains invalid characters, we want to get two `FormError`.
  // For example,
  // validateUsername("!") == Invalid(NEL(TooSmall(1), InvalidCharacters(List('!'))))
  // Note: Check the methods `zip` and `zipWith` of `Validation`.
  def validateUsername(username: String): Validation[FormError, Username] =
    for {
      _ <- checkUsernameSize(username).zip(checkUsernameCharacters(username))
    } yield Username(username)

  // 5. Implement `validateUser` so that it reports all errors.
  def validateUser(usernameStr: String, countryStr: String): Validation[FieldError, User] =
    (
      field(FieldIds.username, validateUsername(usernameStr)),
      field(FieldIds.countryOfResidence, validateCountry(countryStr))
    ).zipWith(User.apply)

  def field[A](fieldId: String, validation: Validation[FormError, A]): Validation[FieldError, A] =
    validation.mapErrorAll { formErrors =>
      NEL(FieldError(fieldId, formErrors))
    }

  // 6. When validateUser` produces a `TooSmall(2)`, how do we know that it is about the username?
  // Update `validateUser` so that it groups all the errors by field (see `FieldError` below).
  // For example,
  // validateUser("b!", "UK") == Invalid(NEL(
  //   FieldError(FieldIds.username          , NEL(TooSmall(2), InvalidCharacters(List('!')))),
  //   FieldError(FieldIds.countryOfResidence, NEL(InvalidFormat("UK")))
  // ))
  // Note: Check the methods `mapError` and `mapErrorAll` of `Validation`.

  case class FieldError(fieldId: String, errors: NEL[FormError])
  object FieldIds {
    val username           = "username"
    val countryOfResidence = "country_of_residence"
  }

}
