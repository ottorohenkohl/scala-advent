package dev.rohenkohl
package advent

val rangePattern = "(\\d+)-(\\d+)".r
val duplicationPattern = "(\\d+)\\1".r
val inputText = os.read(os.pwd / "input")

@main
def main(): Unit = println(rangePattern.findAllMatchIn(inputText)
  .map(found => found.group(1).toLong to found.group(2).toLong).fold(Seq())(_ ++ _)
  .filter(_.toString.matches(duplicationPattern.toString())).sum)