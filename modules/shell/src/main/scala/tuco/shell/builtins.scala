package tuco.shell

import cats.implicits._

import com.monovore.decline.{ Command => Cmd, _ }
import tuco.free._
import tuco.free.{ connection => FC }
import tuco.free.connection.ConnectionIO
import tuco.hi.{ connection => HC }
import tuco.util._

object Builtins {
  import FC.AsyncConnectionIO

  def exit[A] = Command(
    "exit", "Exit the shell.",
    Opts((a: Boolean) => true.pure[ConnectionIO])
  ).zoom(Session.done[A])

  def history[A] = Command(
    "history", "Show command history.",
    Opts { (h: Session.History) =>
      h.toList.reverse.zipWithIndex.traverse { case (s, n) =>
        HC.writeLn(s"$n: $s")
      } .as(h) : ConnectionIO[Session.History] // ascription needed :-\
    }
  ).zoom(Session.history[A])

  def help[A] = Command(
    "help", "Show command help.",
    Opts { (cs: Commands[A]) =>
      val infos = cs.toList.sortBy(_.name)
      val w = infos.map(_.name.length).max + 3 // TODO: use kiama/PP
      HC.writeLn("Available commands: <command> -h for more info.") *>
      infos.traverse(i => HC.writeLn("  " + i.name.padTo(w, ' ') + i.desc)).as(cs)
    }
  ).zoom(Session.commands[A])

  def apply[A] = Commands(exit[A], history[A], help[A])

}
