package tuco.shell

import tuco.free._
import tuco.free.{ connection => FC }
import tuco.hi.{ connection => HC }

import net.bmjames.opts._

import scalaz._, Scalaz.{ some => _, _ }, scalaz.effect._

object CommandShell {

  private val Bang = "!(.*)".r

  private val eventNotFound =
    HC.writeLn(s"history: event not found")

  /**
   * Run a command REPL using the given initial `Session`, yielding the final session value. This
   * action repeats until an executed action yields a session where `.done` is true.
   */
  def run[A](s: Session[A]): FC.ConnectionIO[Session[A]] =
    if (s.done) {
      s.point[FC.ConnectionIO]
    } else {
      def cmd(c: String) = s.commands.interp(c)(s).flatMap(s => run(c :: s))
      HC.readLn(s.prompt, s.history.toZipper).flatMap {
        case Bang(c) => s.history.recall(c).fold(eventNotFound.as(s))(cmd)
        case c       => cmd(c)
      }
    }

}
