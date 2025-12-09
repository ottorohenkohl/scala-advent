package dev.rohenkohl.advent
package types.advent

import extension.mkString

final case class Day private(value: Long):
  def mkString: String = value.mkString(getClass)

class DaySmallerThanOneException extends RuntimeException("A valid day has to be at least 1")
class DayGreaterThanTwentyFiveException extends RuntimeException("A valid day has to be 25 at maximum")

object Day:
  def from(long: Long): Day = Day(long) match
    case Day(value) if value < 1 => throw DaySmallerThanOneException()
    case Day(value) if value > 25 => throw DayGreaterThanTwentyFiveException()
    case valid => valid

  given Conversion[Byte, Day] with
    def apply(byte: Byte): Day = from(byte)

  given Conversion[Short, Day] with
    def apply(short: Short): Day = from(short)

  given Conversion[Int, Day] with
    def apply(int: Int): Day = from(int)

  given Conversion[Long, Day] with
    def apply(long: Long): Day = from(long.toInt)
