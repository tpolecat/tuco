package can.shell

import can.free._
import can.free.{ connection => FC }
import can.hi.{ connection => HC }

import net.bmjames.opts._

import scalaz._, Scalaz.{ some => _, _ }, scalaz.effect._

object CommandShell {

  private val Bang = "!(.*)".r

  private val eventNotFound =
    HC.writeLn(s"history: event not found")

  def run[A](s: Session[A]): FC.ConnectionIO[Unit] = {
    def cmd(c: String) = s.commands.interp(c)(s).flatMap(s => run(c :: s))
    HC.readLn(s.prompt, s.history.toZipper).flatMap {
      case Bang(c) => s.history.recall(c).fold(eventNotFound)(cmd)
      case c       => cmd(c)
    } .unlessM(s.done)
  }

}
