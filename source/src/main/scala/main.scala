package dev.rohenkohl
package advent

val filename = "input"

@main
def main(): Unit =
  val inputLines = os.read.lines(os.pwd / filename)
  val bankValues = inputLines.map(_.toCharArray.map(_.asDigit))
  val indexedBanks = bankValues.map(_.zipWithIndex)

  val highestLefts = indexedBanks.zipWithIndex.map(_._1.dropRight(1).maxBy(_._1))
  val remainingRights = indexedBanks.zipWithIndex.map(bank => bank._1.drop(highestLefts(bank._2)._2 + 1))
  val highestRights = remainingRights.zipWithIndex.map(_._1.maxBy(_._1))

  val highestJoltages = highestLefts.map(_._1).zip(highestRights.map(_._1)).map(_.toList.mkString.toInt).sum

  println(highestJoltages)