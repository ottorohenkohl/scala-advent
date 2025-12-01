package extension

extension(int: Int)
  def isZero: Boolean = int == 0
  def isNegative: Boolean = int < 0
  def range: Range = Range.inclusive(0, int - 1)
