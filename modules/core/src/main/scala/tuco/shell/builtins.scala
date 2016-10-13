package tuco.shell

import tuco.free._
import tuco.free.{ connection => FC }
import tuco.free.connection.ConnectionIO
import tuco.hi.{ connection => HC }
import tuco.util._

import net.bmjames.opts._

import scalaz._, Scalaz.{ some => _, _ }, scalaz.effect._

object Builtins {

  import net.bmjames.opts. { Parser => _, _}
  import net.bmjames.opts.types._

  import Session.L

  def exit[A] = Command(
    ":exit", "Exit the shell.",
    Parser.pure((a: Boolean) => true.point[ConnectionIO])
  ).zoom(L.done[A])

  def history[A] = Command(
    ":history", "Show command history.",
    Parser.pure { (h: Session.History) =>
      h.toList.reverse.zipWithIndex.traverseU { case (s, n) =>
        HC.writeLn(s"$n: $s")
      } .as(h) : ConnectionIO[Session.History] // ascription needed :-\
    }
  ).zoom(L.history[A])

  def help[A] = Command(
    ":help", "Show command help.",
    Parser.pure{ (cs: Commands[A]) =>
      val infos = cs.toList.sortBy(_.name)
      val w = infos.map(_.name.length).max + 3 // TODO: use kiama/PP
      HC.writeLn("Available commands: <command> -h for more info.") *>
      infos.traverseU(i => HC.writeLn("  " + i.name.padTo(w, ' ') + i.desc)).as(cs)
    }
  ).zoom(L.commands[A])

  def apply[A] = Commands(exit[A], history[A], help[A])

}
