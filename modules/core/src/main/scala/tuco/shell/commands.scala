package tuco.shell

import tuco.free._
import tuco.free.{ connection => FC }
import tuco.hi.{ connection   => HC }
import tuco.util.EndoT

import scalaz.{ Success => _, Failure => _, _ }, Scalaz._

import net.bmjames.opts. { Parser => _, _}
import net.bmjames.opts.types._

/**
 * Metadata and implementation of a shell command.
 * @param name The name of the command, as you expect it to be typed by the user. `ls` for example.
 * @param desc A description of the command. `Lists files in the curren directory` for example.
 * @param parser An optparse-applicative `Parser` yielding a effectful state transition.
 */
case class Command[A](name: String, desc: String, parser: Parser[EndoT[FC.ConnectionIO, Session[A]]])

/**
 * A list of `Command` values for a given session type, with methods to interpret an input string,
 * yielding an effectful state transition.
 */
case class Commands[A](toList: List[Command[A]]) {

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
  def interp(s: String): EndoT[FC.ConnectionIO, Session[A]] =
    tokenize(s) match {
      case Nil    => mzero[EndoT[FC.ConnectionIO, Session[A]]]
      case h :: t => interpT(h, t)
    }

  /**
   * Parse the given tokenized input and return an appropriate effectful state transition. In the
   * case of ambiguous or unparseable input the transition will leave the state unaffected and emit a
   * helpful error message.
   */
  def interpT(c: String, args: List[String]): EndoT[FC.ConnectionIO, Session[A]] =
    toList.filter(_.name.startsWith(c)) match {
      case Nil      => EndoT.effect(HC.writeLn("Unknown command. Try ':help' for help."))
      case i :: Nil =>
        val pinfo  = info(i.parser <*> helper, progDesc(i.desc))
        val pprefs = prefs(idm[PrefsMod])
        execParserPure(pprefs, pinfo, args) match {
          case Success(f) => f
          case Failure(f) => EndoT.effect(HC.writeLn(renderFailure(f, i.name)._1))
        }
      case is      => EndoT.effect(HC.writeLn("Ambiguous command matches " + is.map(_.name).mkString(" ")))
    }

}
object Commands {

  /** Construct an empty `Commands`. */
  def empty[A]: Commands[A] =
    Commands(Nil)

  /** Construct an empty `Commands` with the given command values. */
  def apply[A](is: Command[A]*): Commands[A] =
    Commands(is.toList)

  implicit def CommandsMonoid[A]: Monoid[Commands[A]] =
    Monoid.instance((a, b) => Commands(a.toList ++ b.toList), empty)

}
