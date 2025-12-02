package types.grid

import extension.*

final case class Area private(start: Cell, end: Cell):
  def mkString: String = (start, end).mkString(getClass)

class AreaHeightZeroException extends RuntimeException("The areas height can't be zero")
class AreaWidthZeroException extends RuntimeException("The areas width be zero")

object Area:
  def from(tuple: (Cell, Cell)): Area = Area(tuple._1, tuple._2) match
    case Area(start, end) if start.row == end.row => throw AreaHeightZeroException()
    case Area(start, end) if start.column == end.column => throw AreaWidthZeroException()
    case valid => valid

  given Conversion[(Cell, Cell), Area] with
    def apply(tuple: (Cell, Cell)): Area = from(tuple)