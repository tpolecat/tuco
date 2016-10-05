package can

sealed abstract class Event extends Product with Serializable
object Event {

  /** Occurs when a connection requested logout by sending a <Ctrl>-<D> key combination. */
  case object LogoutRequest extends Event

  /** Occurs when the connection sent a NVT BREAK signal. */
  case object Break extends Event

  /** Occurs if a connection has been idle exceeding the configured time to warning. */
  case object Idle extends Event

  /** Occurs if a connection has been idle exceeding the configured time to warning and the configured time to timedout. */
  case object TimedOut extends Event

}
