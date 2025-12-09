package dev.rohenkohl
package advent

import dev.rohenkohl.advent.types.grid.Grid

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import scala.collection.mutable.Map

val filename = "input"

@main
def main(): Unit =
  val grid: Grid = os.read(os.pwd / filename)
  val marks: Map[(Int, Int), Int] = Map()

  val columns = grid.columns.indices
  val cells = columns.flatMap(column => grid.rows.indices.map((_, column)))

  cells.foreach(cell => mark(cell._1, cell._2, grid, marks))
  marks.map(entry => (entry._1, entry._2 - 1))

  println(marks.count(cell => grid.rows(cell._1._1)(cell._1._2) == '@' && cell._2 < 4))

def mark(row: Int, column: Int, grid: Grid, marks: Map[(Int, Int), Int]): Unit =
  if grid.rows(row)(column) != '@' then return

  marks.put((row, column), marks.getOrElse((row, column), 0))

  val columns = column - 1 to column + 1
  val cells = columns.flatMap(column => (row - 1 to row + 1).map((_, column))).filterNot((row, column).equals)
  val relevant = cells.filter(cell => grid.rows.indices.contains(cell._1) && grid.columns.indices.contains(cell._2))

  relevant.foreach(cell => marks.put(cell, marks.getOrElse(cell, 0) + 1))