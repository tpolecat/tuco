package tuco.shell

import tuco.free._
import tuco.free.{ connection => FC }
import tuco.hi.{ connection => HC }
import tuco.util._

import net.bmjames.opts._

import scalaz._, Scalaz.{ some => _, _ }, scalaz.effect._

object Builtins {

  import net.bmjames.opts. { Parser => _, _}
  import net.bmjames.opts.types._

  import Session.L

  def exit[A] = Command[A](
    "exit", "Exit the shell.",
    Parser.pure(L.done[A].liftEndoT(_=> true))
  )

  def history[A] = Command[A](
    "history", "Show command history.",
    Parser.pure(L.history[A].endoT_[FC.ConnectionIO] { h =>
      h.toList.reverse.zipWithIndex.traverseU { case (s, n) =>
        HC.writeLn(s"$n: $s")
      }
    })
  )

  def help[A] = Command[A](
    "help", "Show command help.",
    Parser.pure(L.commands[A].endoT_[FC.ConnectionIO] { cs =>
      val infos = cs.toList.sortBy(_.name)
      val w = infos.map(_.name.length).max + 3 // TODO: use kiama/PP
      HC.writeLn("Available commands: <command> -h for more info.") *>
      infos.traverseU(i => HC.writeLn("  " + i.name.padTo(w, ' ') + i.desc))
    })
  )

  def apply[A] = Commands(exit[A], history[A], help[A])

}
