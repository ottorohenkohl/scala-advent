package extension

import scala.util.matching.Regex.Match

extension(vector: Vector[Any])
  def mkLines: String = vector.mkString("\n")

extension(vector: Vector[Match])
  def start: Int = vector(0).start
  def end: Int = vector.map(_.end).max()