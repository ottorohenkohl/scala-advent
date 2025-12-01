package types.advent

import extension.mkString

final case class Part private(value: Long):
  def mkString: String = value.mkString(getClass)

class PartSmallerThanOneException extends RuntimeException("A valid part has to be at least 1")
class PartGreaterThanTwoException extends RuntimeException("A valid part has to be 2 at maximum")

object Part:
  def from(long: Long): Part = Part(long) match
    case Part(value) if value < 1 => throw PartSmallerThanOneException()
    case Part(value) if value > 2 => throw PartGreaterThanTwoException()
    case valid => valid

  given Conversion[Byte, Part] with
    def apply(byte: Byte): Part = from(byte)

  given Conversion[Short, Part] with
    def apply(short: Short): Part = from(short)

  given Conversion[Int, Part] with
    def apply(int: Int): Part = from(int)

  given Conversion[Long, Part] with
    def apply(long: Long): Part = from(long.toInt)
