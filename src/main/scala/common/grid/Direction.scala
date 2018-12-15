package common.grid

sealed abstract class Direction {
  def left: Direction
  def right: Direction
  def reverse: Direction
}
object Direction {
  case object North extends Direction {
    override def left: Direction = West
    override def right: Direction = East
    override def reverse: Direction = South
  }
  case object South extends Direction {
    override def left: Direction = East
    override def right: Direction = West
    override def reverse: Direction = North
  }
  case object West extends Direction {
    override def left: Direction = South
    override def right: Direction = North
    override def reverse: Direction = East
  }
  case object East extends Direction {
    override def left: Direction = North
    override def right: Direction = South
    override def reverse: Direction = West
  }
}
