package extension

extension(string: String)
  def mkPadded: String = string.prepended('\n').appended('\n')
  def asRows: Vector[String] = string.split('\n').toVector