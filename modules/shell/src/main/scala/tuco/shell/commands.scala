package tuco.shell

import cats._, cats.implicits._
import com.monovore.decline.{ Command => Cmd, _ }
import tuco.free._
import tuco.free.{ connection => FC }
import tuco.free.connection.ConnectionIO
import tuco.hi.{ connection   => HC }

/**
 * A list of `Command` values for a given session type, with methods to interpret an input string,
 * yielding an effectful state transition.
 */
case class Commands[A](toList: List[Command[ConnectionIO, Session[A]]]) {
  import FC.AsyncConnectionIO

  // Split a string into a list of tokens, possibly wrapped in double-quotes
  private val R = """"([^"]*)"|(\S+)""".r
  private def tokenize(s: String): List[String] =
    R.findAllMatchIn(s).map { m =>
      Option(m.group(1)).getOrElse(m.group(2))
    } .toList

  /**
   * Tokenize the given input and delegate to `interpT` to compute an effectful state transition.
   * If the input contains no tokens the returned transition is a no-op.
   */
  def interp(s: String): Session[A] => ConnectionIO[Session[A]] =
    tokenize(s) match {
      case Nil    => _.pure[ConnectionIO]
      case h :: t => interpT(h, t)
    }

  /**
   * Parse the given tokenized input and return an appropriate effectful state transition. In the
   * case of ambiguous or unparseable input the transition will leave the state unaffected and emit a
   * helpful error message.
   */
  def interpT(c: String, args: List[String]): Session[A] => ConnectionIO[Session[A]] =
    toList.filter(_.name.startsWith(c)) match {
      case Nil      => s => HC.writeLn("Unknown command. Try 'help' for help.").as(s)
      case i :: Nil =>
        val c = Cmd(name = i.name, header = i.desc)(i.parser)
        c.parse(args) match {
          case Right(f) => f
          case Left(h)  => s => HC.writeLn(h.toString).as(s)
        }
      case is      => s => HC.writeLn("Ambiguous command matches " + is.map(_.name).mkString(" ")).as(s)
    }

}
object Commands {

  /** Construct an empty `Commands`. */
  def empty[A]: Commands[A] =
    Commands(Nil)

  /** Construct an empty `Commands` with the given command values. */
  def apply[A](is: Command[ConnectionIO, Session[A]]*): Commands[A] =
    Commands(is.toList)

  implicit def CommandsMonoid[A]: Monoid[Commands[A]] =
    new Monoid[Commands[A]] {
      def empty = Commands.empty[A]
      def combine(a: Commands[A], b: Commands[A]) = Commands(a.toList ++ b.toList)
    }

}
