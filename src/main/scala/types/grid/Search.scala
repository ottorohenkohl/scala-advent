package types.grid

import extension.{mkLines, mkPadded, mkString}

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

final case class Search private(regexps: Vector[Regex]):
  def mkString: String = regexps.map(_.regex).mkLines.mkPadded.mkString(getClass)

  @tailrec
  def in(grid: Grid, todo: Vector[Regex] = regexps, tried: Vector[Area] = Vector(), acc: Vector[Grid] = Vector()): Vector[Grid] = todo.size match
    case size if size == 0 => in(grid, regexps, tried, acc.appended(grid.cut(tried.last)))
    case size if size > rowsRemaining(grid, tried) => acc
    case size if size == regexps.size => remainingMatches(grid, todo.head, tried) match
      case matched if matched.isEmpty => acc
      case matched => in(grid, todo.drop(1), tried.appended(matchedArea(matched.head)), acc)
    case _ => matchesInRow(grid, todo.head, lastRow(tried) + 1).filter(_.start == tried.last.start.column) match
      case matched if matched.isEmpty => in(grid, regexps, tried, acc)
      case matched => in(grid, todo.drop(1), tried.dropRight(1).appended(expandArea(tried.last, matched.next())), acc)

  private def lastRow(tried: Vector[Area]) = tried.lastOption.map(_.end.row - 1).getOrElse(0)
  private def rowsRemaining(grid: Grid, tried: Vector[Area]) = grid.rows.length - lastRow(tried)
  private def matchesInRow(grid: Grid, regex: Regex) = grid.indexedRows.flatten(row => regex.findAllMatchIn(row._1.mkString).map((_, row._2)))
  private def filterTried(indexedMatches: Vector[(Match, Int)], tried: Vector[Area]) = indexedMatches.filterNot(matched => tried.exists(area => area.start == Cell.from(matched._2, matched._1.start)))
  private def remainingMatches(grid: Grid, regex: Regex, tried: Vector[Area]) = filterTried(matchesInRow(grid, regex), tried)
  private def matchedArea(matched: (Match, Int)) = Area.from(Cell.from(matched._2, matched._1.start), Cell.from(matched._2 + 1, matched._1.end))
  private def matchesInRow(grid: Grid, regex: Regex, row: Int) = regex.findAllMatchIn(grid.rows(row).mkString)
  private def expandArea(area: Area, added: Match) = Area.from(area.start, Cell.from(area.end.row + 1, List(area.end.column, added.end).max))

class PatternEmptyException extends RuntimeException("There's not a single pattern defined")
class PatternTooLongException extends RuntimeException("The pattern exceeds a singular line")

object Search:
  def from(regexps: Vector[Regex]): Search = Search(regexps) match
    case Search(regexps) if regexps.isEmpty => throw PatternEmptyException()
    case valid => valid

  def from(string: String): Search = string.split("\n").toVector match
    case vector if vector.isEmpty => throw PatternEmptyException()
    case vector => from(vector.map(_.r))

  given Conversion[Vector[Regex], Search] with
    def apply(regexps: Vector[Regex]): Search = from(regexps)

  given Conversion[String, Search] with
    def apply(string: String): Search = from(string)