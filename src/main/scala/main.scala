import Direction.{Left, Right}

@main
def main(): Unit =
  given Conversion[Char, Direction] with
    def apply(char: Char): Direction = char match
      case 'L' => Left
      case 'R' => Right

  val rotations = os.read.lines(os.pwd / "input").map(line => Rotation(line.charAt(0), line.splitAt(1)(1).toInt))
  val dials = rotations.foldLeft(List(Dial(50, 0))) { (dials, rotation) => dials :+ dials.last.rotate(rotation) }

  println(dials.count(_.position == 0))
  println(dials.map(_.zeros).sum)

case class Rotation(direction: Direction, amount: Int)
case class Dial(position: Int, zeros: Int):
  private def passedZeros(rotation: Rotation) = rotation.direction match
    case Left => ((100 - position) % 100 + rotation.amount) / 100
    case Right => (position + rotation.amount) / 100

  private def newPosition(rotation: Rotation) = rotation.direction match
    case Right => (position + rotation.amount) % 100
    case Left => (position + (100 - rotation.amount)) % 100 match
      case negative if negative < 0 => 100 + negative
      case positive => positive

  def rotate(rotation: Rotation): Dial = Dial(newPosition(rotation), passedZeros(rotation))

enum Direction:
  case Left
  case Right