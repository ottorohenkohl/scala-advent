package dev.rohenkohl
package advent

case class Range(lower: Long, upper: Long):
  def contains(long: Long) = lower <= long && long <= upper

@main
def main() =
  val input = os.read.lines(os.pwd / "input.aoc")

  val parsedIds = input.dropWhile(_.nonEmpty).tail.map(_.toLong)
  val parsedRanges = input
    .takeWhile(_.nonEmpty)
    .map("(\\d+)-(\\d+)".r.findFirstMatchIn(_).get)
    .map(_.subgroups)
    .map(matched => Range(matched(0).toLong, matched(1).toLong))

  println(parsedIds.count(id => parsedRanges.exists(_.contains(id))))
