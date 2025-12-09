package dev.rohenkohl.advent
package types.advent

import extension.mkString

import java.time.LocalDate

final case class Year private(value: Long):
  def mkString: String = value.mkString(getClass)

class YearSmallerFirstEventException extends RuntimeException("There was no event before 2015")
class YearGreaterThanCurrentYearException extends RuntimeException("Year can't be in the future")

object Year:
  def from(long: Long): Year = Year(long) match
    case Year(value) if value < 2015 => throw YearSmallerFirstEventException()
    case Year(value) if value > LocalDate.now().getYear => throw YearGreaterThanCurrentYearException()
    case valid => valid

  given Conversion[Byte, Year] with
    def apply(byte: Byte): Year = from(byte)

  given Conversion[Short, Year] with
    def apply(short: Short): Year = from(short)

  given Conversion[Int, Year] with
    def apply(int: Int): Year = from(int)

  given Conversion[Long, Year] with
    def apply(long: Long): Year = from(long.toInt)
