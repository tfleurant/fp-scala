package chapter04.errorhandling

import Either.*

import scala.util.control.NonFatal

object EitherExample {
  def mean(xs: Seq[Double]): Either[String, Double] =
    if xs.isEmpty then Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def parseInsuranceRateQuote(
      age: String,
      numberOfSpeedingTickets: String,
  ): Either[Throwable, Double] = for {
    a <- Either.catchNonFatal(age.toInt)
    tickets <- Either.catchNonFatal(numberOfSpeedingTickets.toInt)
  } yield OptionExample.insuranceRateQuote(a, tickets)
}

// Example of map2 usage for validation
case class Name private (value: String)
object Name {
  def apply(name: String): Either[String, Name] =
    if name.isBlank || name == null then Left("Name is empty.")
    else Right(new Name(name))
}

case class Age private (value: Int)
object Age {
  def apply(age: Int): Either[String, Age] =
    if age < 0 then Left("Age is out of range.")
    else Right(new Age(age))
}

case class Person(name: Name, age: Age)
case object Person {
  def make(name: String, age: Int): Either[String, Person] =
    Name(name).map2(Age(age))(Person(_, _))

  // With error accumulation
  def makeBoth(name: String, age: Int): Either[List[String], Person] =
    map2Both(Name(name), Age(age), Person(_, _))
}
