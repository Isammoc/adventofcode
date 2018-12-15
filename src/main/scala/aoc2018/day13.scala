package aoc2018
import common.grid._
import common.grid.Direction._

object day13 extends App {

  val input = io.Source.stdin.getLines.mkString("\n")

  def part1(input: String): String = {
    val paths = (for {
      (line, y) <- input.split("\n").zipWithIndex
      (c, x) <- line.zipWithIndex
      path <- Path(c)
    } yield Point(x, y) -> path).toMap

    val carts = for {
      (line, y) <- input.split("\n").toList.zipWithIndex
      (c, x) <- line.zipWithIndex
      dir <- parseDirection(c)
    } yield Cart(Point(x, y), dir, Left)

    def loop(carts: List[Cart] = carts.sortBy(_.location),
             next: List[Cart] = Nil,
             blocked: Set[Point] = carts.map(_.location).toSet): String =
      carts match {
        case Nil => loop(next.sortBy(_.location), Nil, blocked)
        case current :: _
            if blocked contains current
              .move(paths(current.location))
              .location =>
          val crash = current.move(paths(current.location)).location
          s"${crash.x},${crash.y}"
        case current :: t =>
          loop(
            t,
            current.move(paths(current.location)) :: next,
            blocked - current.location + current
              .move(paths(current.location))
              .location
          )
      }
    loop()
  }

  def part2(input: String) = {
    val paths = (for {
      (line, y) <- input.split("\n").zipWithIndex
      (c, x) <- line.zipWithIndex
      path <- Path(c)
    } yield Point(x, y) -> path).toMap

    val carts = for {
      (line, y) <- input.split("\n").toList.zipWithIndex
      (c, x) <- line.zipWithIndex
      dir <- parseDirection(c)
    } yield Cart(Point(x, y), dir, Left)

    def loop(carts: List[Cart] = carts.sortBy(_.location),
             next: List[Cart] = Nil,
             blocked: Set[Point] = carts.map(_.location).toSet,
             startTick: Boolean): String = {
      carts match {
        case finalCart :: Nil if startTick =>
          s"${finalCart.location.x},${finalCart.location.y}"
        case Nil =>
          loop(next.sortBy(_.location), Nil, blocked, startTick = true)
        case current :: t
            if blocked contains current
              .move(paths(current.location))
              .location =>
          val crash = current.move(paths(current.location)).location
          loop(
            t.filterNot(_.location == crash),
            next.filterNot(_.location == crash),
            blocked - crash - current.location,
            startTick = false
          )
        case current :: t =>
          loop(
            t,
            current.move(paths(current.location)) :: next,
            blocked - current.location + current
              .move(paths(current.location))
              .location,
            startTick = false
          )
      }
    }
    loop(startTick = true)
  }

  def printTrack(paths: Map[Point, Path], carts: List[Cart]): Unit = {
    val maxx = paths.keys.map(_.x).max
    val maxy = paths.keys.map(_.y).max

    val track: Array[Array[Char]] =
      Array.ofDim[Char](maxy + 1, maxx + 1).map(_.map(_ => ' '))

    paths.foreach {
      case (point, Pipe) =>
        track(point.y)(point.x) = '|'
      case (point, Caret) =>
        track(point.y)(point.x) = '-'
      case (point, Slash) =>
        track(point.y)(point.x) = '/'
      case (point, AntiSlash) =>
        track(point.y)(point.x) = '\\'
      case (point, Plus) =>
        track(point.y)(point.x) = '+'
    }
    carts.foreach { cart =>
      cart.direction match {
        case North => track(cart.location.y)(cart.location.x) = '^'
        case South => track(cart.location.y)(cart.location.x) = 'v'
        case West  => track(cart.location.y)(cart.location.x) = '<'
        case East  => track(cart.location.y)(cart.location.x) = '>'
      }
    }

    println(track.map(_.mkString).mkString("\n"))
  }

  sealed abstract class Way {
    val next: Way

    def turn(dir: Direction): Direction
  }

  sealed abstract class Path

  case class Cart(location: Point, direction: Direction, way: Way = Left) {
    def move(path: Path): Cart = path match {
      case Pipe =>
        Cart(location.move(direction), direction, way)
      case Caret =>
        Cart(location.move(direction), direction, way)
      case Slash =>
        direction match {
          case North => Cart(location.move(East), East, way)
          case South => Cart(location.move(West), West, way)
          case East  => Cart(location.move(North), North, way)
          case West  => Cart(location.move(South), South, way)
        }
      case AntiSlash =>
        direction match {
          case North => Cart(location.move(West), West, way)
          case South => Cart(location.move(East), East, way)
          case East  => Cart(location.move(South), South, way)
          case West  => Cart(location.move(North), North, way)
        }
      case Plus =>
        val nextDirection = way.turn(direction)
        Cart(location.move(nextDirection), nextDirection, way.next)
    }
  }

  def parseDirection(c: Char): Option[Direction] = c match {
    case '^' => Some(North)
    case 'v' => Some(South)
    case '<' => Some(West)
    case '>' => Some(East)
    case _   => None
  }

  case object Left extends Way {
    val next: Way = Straight

    override def turn(dir: Direction): Direction = dir match {
      case North => West
      case West  => South
      case South => East
      case East  => North
    }
  }

  case object Straight extends Way {
    val next: Way = Right

    override def turn(dir: Direction): Direction = dir
  }

  case object Right extends Way {
    val next: Way = Left

    override def turn(dir: Direction): Direction = dir match {
      case North => East
      case West  => North
      case South => West
      case East  => South
    }
  }

  case object Pipe extends Path

  case object Caret extends Path

  case object Slash extends Path

  case object AntiSlash extends Path

  case object Plus extends Path

  object Path {
    def apply(c: Char): Option[Path] = c match {
      case '|'  => Some(Pipe)
      case '^'  => Some(Pipe)
      case 'v'  => Some(Pipe)
      case '-'  => Some(Caret)
      case '>'  => Some(Caret)
      case '<'  => Some(Caret)
      case '/'  => Some(Slash)
      case '\\' => Some(AntiSlash)
      case '+'  => Some(Plus)
      case _    => None
    }
  }
  println("part1 = " + part1(input))
  println("part2 = " + part2(input))
}
