package common.util

object Repeat {
  def apply[T](count: Int)(initial: T)(f: T => T): T =
    (0 until count).foldLeft(initial) { case (a, _) => f(a) }
}
