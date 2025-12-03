package dev.rohenkohl
package advent

import scala.annotation.tailrec

val filename = "input"

@main
def main(): Unit =
  val input = os.read.lines(os.pwd / filename)

  val batteries = input.map(_.zipWithIndex.map(digit => Battery(digit._2, digit._1.asDigit)))
  val banks = batteries.zipWithIndex.map(battery => Bank(battery._2, battery._1))

  println(banks.map(bank => combinations(12, bank, List()).map(_.value).mkString.toLong).sum)

@tailrec
def combinations(batteries: Int, bank: Bank, accumulator: List[Battery]): List[Battery] = batteries match
  case done if batteries == 0 => accumulator.reverse
  case _ => accumulator match
    case head :: _ => combinations(batteries - 1, bank, bank.batteriesAfter(head).highestBattery(batteries) :: accumulator)
    case _ => combinations(batteries - 1, bank, bank.highestBattery(batteries) :: accumulator)

case class Battery(position: Int, value: Long)
case class Bank(position: Int, batteries: Seq[Battery]):
  def highestBattery(position: Int): Battery = batteries.dropRight(position - 1).maxBy(_.value)
  def batteriesAfter(battery: Battery): Bank = Bank(position, batteries.drop(battery.position + 1))