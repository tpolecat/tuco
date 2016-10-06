package can

import can.free._
import can.free.{ connection => FC }
import can.hi.{ connection   => HC }

import scalaz.{ Success => _, Failure => _, _ }, Scalaz._

import net.bmjames.opts. { Parser => _, _}
import net.bmjames.opts.types._


case class Command[A](name: String, desc: String, parser: Parser[Session[A] => FC.ConnectionIO[Session[A]]])

case class Commands[A](toList: List[Command[A]]) {

  object tokenize {
    private val R = """"([^"]*)"|(\S+)""".r
    def apply(s: String): List[String] =
      R.findAllMatchIn(s).map { m =>
        Option(m.group(1)).getOrElse(m.group(2))
      } .toList
  }

  def interp(s: String): Session[A] => FC.ConnectionIO[Session[A]] =
    tokenize(s) match {
      case Nil => _.point[FC.ConnectionIO]
      case h :: t => interpT(h, t)
    }

  def interpT(c: String, args: List[String]): Session[A] => FC.ConnectionIO[Session[A]] =
    toList.filter(_.name.startsWith(c)) match {
      case Nil      => s => HC.writeLn("Unknown command. Try :help for help.").as(s)
      case i :: Nil => s =>
        val pinfo  = info(i.parser <*> helper, progDesc(i.desc))
        val pprefs = prefs(idm[PrefsMod])
        execParserPure(pprefs, pinfo, args) match {
          case Success(f) => f(s)
          case Failure(f) => HC.writeLn(renderFailure(f, i.name)._1).as(s)
        }
      case is      => s => HC.writeLn("Ambiguous command matches " + is.map(_.name).mkString(" ")).as(s)
    }

}
object Commands {

  def apply[A](is: Command[A]*): Commands[A] =
    Commands(is.toList)

  def empty[A]: Commands[A] = Commands(Nil)

  implicit def CommandsMonoid[A]: Monoid[Commands[A]] =
    Monoid.instance((a, b) => Commands(a.toList ++ b.toList), empty)

}
