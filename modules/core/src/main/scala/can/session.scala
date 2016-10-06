package can

import scalaz._, Scalaz._

final case class Session[A](
  commands: Commands[A],
  history:  Session.History,
  prompt:   String,
  done:     Boolean,
  state:    A
) {
  def ::(s: String) = copy(history = s :: history)
}

object Session {

  def initial[A](a: A): Session[A] =
    Session(Builtins[A], History.empty, "> ", false, a)

  object L {
    def commands[A]: Session[A] @> Commands[A] = Lens.lensu((a, b) => a.copy(commands = b), _.commands)
    def prompt[A]:   Session[A] @> String      = Lens.lensu((a, b) => a.copy(prompt = b), _.prompt)
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
