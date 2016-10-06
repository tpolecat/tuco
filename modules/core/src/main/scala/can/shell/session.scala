package can.shell

import scalaz._, Scalaz._

/**
 * State for a `CommandShell` session with user-defined payload of type `A`.
 * @param commands available to the user.
 * @param history the accumulated command history, used for command recall.
 * @param prompt the prompt to be displayed to the user; `"woozle> "` for example.
 * @param done a command can set this to `true` to indicated that the session should end.
 * @param data an arbitrary payload, for application-specific functionality.
 */
final case class Session[A](
  commands: Commands[A],
  history:  Session.History,
  prompt:   String,
  done:     Boolean,
  data:     A
) {

  /** Construct an equivalent `Session` with the given value prepended to the history. */
  def ::(s: String) = copy(history = s :: history)

}

object Session {

  /**
   * Construct a `Session` with no commands, no history, `"> "` as the prompt, and the given value
   * as the applicaton-specific payload.
   */
  def initial[A](a: A): Session[A] =
    Session(Commands.empty, History.empty, "> ", false, a)

  /** Equivalent to `initial` but uses the `mzero[A]` for the initial payload value. */
  def empty[A: Monoid]: Session[A] =
    initial(mzero[A])

  /** Module of lenses. */
  object L {
    def commands[A]: Session[A] @> Commands[A] = Lens.lensu((a, b) => a.copy(commands = b), _.commands)
    def history[A]:  Session[A] @> History     = Lens.lensu((a, b) => a.copy(history = b), _.history)
    def prompt[A]:   Session[A] @> String      = Lens.lensu((a, b) => a.copy(prompt = b), _.prompt)
    def data[A]:     Session[A] @> A           = Lens.lensu((a, b) => a.copy(data = b), _.data)
    def done[A]:     Session[A] @> Boolean     = Lens.lensu((a, b) => a.copy(done = b), _.done)
  }

  case class History(toList: List[String]) {
    def ::(s: String) = toList.headOption match {
      case Some(`s`) => this
      case _         => History(s :: toList)
    }
    def headOption = toList.headOption
    def toZipper = Zipper(toList.toStream, "", Stream.empty)
    def recall(s: String): Option[String] =
      s.parseInt.toOption match {
        case Some(n) => toList.reverse.lift(n)
        case None    =>
          s match {
            case "!" => toList.headOption
            case s   => toList.find(_.startsWith(s))
          }
      }
  }
  object History {
    val empty = History(Nil)
  }

}
