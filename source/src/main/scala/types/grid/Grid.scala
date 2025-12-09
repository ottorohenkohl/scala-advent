package dev.rohenkohl.advent
package types.grid

import extension.*

final class Grid private(val offset: Option[Cell], val values: Vector[Vector[Char]]):
  def mkString: String = (offset, values.map(_.mkString).mkLines.mkPadded).mkString(getClass)

  def apply(row: Int): Vector[Char] = values(row)

  def rotatedClockwise: Grid = Grid(offset, columns.reverse)
  def rotatedCounter: Grid = Grid(offset, rows.reverse.transpose())
  def verticallyMirrored: Grid = Grid(offset, values.reverse)
  def horizontallyMirrored: Grid = Grid(offset, values.map(_.reverse))
  def indexedRows: Vector[(Vector[Char], Int)] = rows.zipWithIndex
  def indexedColumns: Vector[(Vector[Char], Int)] = columns.zipWithIndex
  def rows: Vector[Vector[Char]] = values
  def columns: Vector[Vector[Char]] = values.transpose()
  def cut(area: Area): Grid = offset match
    case None => Grid(Some(area.start), values.slice(area.start.row, area.end.row).map(_.slice(area.start.column, area.end.column)))
    case Some(offset) => Grid(Some(offset + area.start), values.slice(area.start.row, area.end.row).map(_.slice(area.start.column, area.end.column)))

class EmptyRowsException extends RuntimeException("There's not a single row in the input")
class EmptyColumnException extends RuntimeException("There's not a single column in the input")

object Grid:
  def from(input: String): Grid = Grid(None, input.asRows.map(_.toVector.padTo(input.asRows.map(_.length).max, ' '))) match
    case grid if grid.rows.length.isZero => throw EmptyRowsException()
    case grid if grid.columns.length.isZero => throw EmptyColumnException()
    case valid => valid

  given Conversion[String, Grid] with
    def apply(string: String): Grid = from(string)
