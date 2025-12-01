import types.grid.Grid

@main
def main(): Unit =
  val input: Grid = os.read(os.pwd / "input")

  println(input.mkString)