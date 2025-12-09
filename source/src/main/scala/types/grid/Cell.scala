package dev.rohenkohl.advent
package types.grid

import extension.*

final case class Cell private(row: Int, column: Int):
  def mkString: String = (row, column).mkString(getClass)

  infix def +(other: Cell): Cell = Cell(row + other.row, column + other.column)
  infix def -(other: Cell): Cell = Cell(row - other.row, column - other.column)

class RowNegativeException extends RuntimeException("The row can't be negative")
class ColumnNegativeException extends RuntimeException("The column can't be negative")

object Cell:
  def from(tuple: (Int, Int)): Cell = Cell(tuple._1, tuple._2) match
    case Cell(row, column) if row.isNegative => throw RowNegativeException()
    case Cell(row, column) if column.isNegative => throw ColumnNegativeException()
    case valid => valid

  def from(y: Int, column: Int): Cell = Cell(y, column) match
    case Cell(row, column) if row.isNegative => throw RowNegativeException()
    case Cell(row, column) if column.isNegative => throw ColumnNegativeException()
    case valid => valid

  given Conversion[(Int, Int), Cell] with
    def apply(tuple: (Int, Int)): Cell = from(tuple)