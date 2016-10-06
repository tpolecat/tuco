package can.shell

import can.free._
import can.free.{ connection => FC }
import can.hi.{ connection => HC }
import can.util._

import net.bmjames.opts._

import scalaz._, Scalaz.{ some => _, _ }, scalaz.effect._

object Builtins {

  import net.bmjames.opts. { Parser => _, _}
  import net.bmjames.opts.types._

  import Session.L

  def exit[A] = Command[A](
    ":exit", "Exit the shell.",
    Parser.pure(L.done[A].liftEndoT(_=> true))
  )

  def history[A] = Command[A](
    ":history", "Show command history.",
    Parser.pure(L.history[A].endoT_[FC.ConnectionIO] { h =>
      h.toList.reverse.zipWithIndex.traverseU { case (s, n) =>
        HC.writeLn(s"$n: $s")
      }
    })
  )

  def help[A] = Command[A](
    ":help", "Show command help.",
    Parser.pure(L.commands[A].endoT_[FC.ConnectionIO] { cs =>
      val infos = cs.toList.sortBy(_.name)
      val w = infos.map(_.name.length).max + 3 // TODO: use kiama/PP
      infos.traverseU(i => HC.writeLn(i.name.padTo(w, ' ') + i.desc))
    })
  )

  def apply[A] = Commands(exit[A], history[A], help[A])

}
