package dev.rohenkohl
package advent

import dev.rohenkohl.advent.types.grid.Grid
import collection.mutable.Map
import os.Path

extension (char: Char)
  def isPaperroll: Boolean = char == '@'
extension (path: Path)
  def inputFile: Path = path / "input"
extension (map: Map[(Int, Int), Int])
  def countedNeighbours: Map[(Int, Int), Int] = map.map(entry => (entry._1, entry._1.listNeighbours.map(map(_)).sum - 1))
extension (point: (Int, Int))
  def listNeighbours: Seq[(Int, Int)] = (point._1 - 1 to point._1 + 1).flatMap(row => (point._2 - 1 to point._2 + 1).map(column => (row, column)))

@main
def main(): Unit =
  val grid: Grid = os.read(os.pwd.inputFile)

  val rows = grid.rows.indices
  val columns = grid.columns.indices
  val cells = columns.flatMap(column => rows.map((_, column)))

  val mapped = Map(cells.filter(point => grid.rows(point._1)(point._2).isPaperroll).map((_, 1)).toMap.toSeq: _*).withDefaultValue(0)
  val initial = mapped.size

  while true do
    val countedNeighbours = mapped.countedNeighbours

    mapped.filterInPlace((key, _) => countedNeighbours(key) >= 4)

    if countedNeighbours.size == mapped.size then return println(initial - mapped.size)