package dev.rohenkohl
package advent

case class Range(lower: Long, upper: Long):
  def contains(long: Long) = lower <= long && long <= upper
  def elements() = upper - lower + 1

@main
def main() =
  val combinedRanges = os.read
    .lines(os.pwd / "input.aoc")
    .takeWhile(_.nonEmpty)
    .map("(\\d+)-(\\d+)".r.findFirstMatchIn(_).get)
    .map(_.subgroups)
    .map(matched => Range(matched(0).toLong, matched(1).toLong))
    .sortBy(_.lower)
    .foldLeft(List[Range]()) { (accumulator, range) =>
      accumulator match
        case head :: tail =>
          if head.upper < range.lower then range :: accumulator
          else Range(head.lower, (head.upper, range.upper).toList.max) :: tail
        case _ => List(range)
    }

  println(combinedRanges.map(_.elements()).sum)
