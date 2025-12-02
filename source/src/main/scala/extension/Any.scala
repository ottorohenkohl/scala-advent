package extension

extension(any: Any)
  def mkString(name: Class[?]): String = s"${name.getSimpleName}($any)"
