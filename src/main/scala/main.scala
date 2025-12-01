import Direction.{Left, Right}

@main
def main(): Unit =
  given Conversion[Char, Direction] with
    def apply(char: Char): Direction = char match
      case 'L' => Left
      case 'R' => Right

  val rotations = os.read.lines(os.pwd / "input").map(line => Rotation(line.charAt(0), line.splitAt(1)(1).toInt))
  val dials = rotations.foldLeft(List(Dial(50))) { (dials, rotation) => dials :+ dials.last.rotate(rotation) }

  println(dials.count(_.position == 0))

case class Rotation(direction: Direction, amount: Int)
case class Dial(position: Int):
  def rotate(rotation: Rotation): Dial = rotation.direction match
    case Left => Dial((position + (100 - rotation.amount)) % 100)
    case Right => Dial((position + rotation.amount) % 100)

enum Direction:
  case Left
  case Right