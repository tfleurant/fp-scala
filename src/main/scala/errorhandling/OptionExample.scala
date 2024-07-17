package errorhandling

import errorhandling.Option._

object OptionExample {
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = (100 - age) * 0.2 * numberOfSpeedingTickets

  def toIntOption(s: String): Option[Int] =
    try Some(s.toInt)
    catch case _: NumberFormatException => None

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] =
    val optAge: Option[Int] = toIntOption(age)
    val optTickets: Option[Int] = toIntOption(numberOfSpeedingTickets)
    map2(optAge, optTickets)(insuranceRateQuote)

  // Inefficient as we have to traverse the list twice
  def parseInts(as: List[String]): Option[List[Int]] =
    sequenceAlt(as.map(toIntOption))

}
