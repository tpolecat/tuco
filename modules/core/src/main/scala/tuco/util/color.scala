package tuco.util

sealed abstract class Color private (val toInt: Int) extends Product with Serializable
object Color {
  case object Black   extends Color(30)
  case object Red     extends Color(31)
  case object Green   extends Color(32)
  case object Yellow  extends Color(33)
  case object Blue    extends Color(34)
  case object Magenta extends Color(35)
  case object Cyan    extends Color(36)
  case object White   extends Color(37)
}
