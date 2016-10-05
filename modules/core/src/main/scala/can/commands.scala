package can

import can.free._
import can.free.{ connection => FC }
import can.hi.{ connection   => HC }

import net.bmjames.opts. { Parser => _, _}
import net.bmjames.opts.types._

import scalaz._, Scalaz._


case class Info[A](name: String, desc: String, parser: Parser[Session[A] => FC.ConnectionIO[Session[A]]])

case class Commands[A](toList: List[Info[A]]) {

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
      case Nil     => s => HC.writeLn("Unknown shell command. Try :help for help.").as(s)
      case List(i) => s => HC.writeLn("TODO: parse and run!").as(s)
      case is      => s => HC.writeLn("Ambiguous shell command matches " + is.map(_.name).mkString(" ")).as(s)
    }

}
object Commands {

  def apply[A](is: Info[A]*): Commands[A] =
    Commands(is.toList)

  def empty[A]: Commands[A] = Commands(Nil)

  implicit def CommandsMonoid[A]: Monoid[Commands[A]] =
    Monoid.instance((a, b) => Commands(a.toList ++ b.toList), empty)

}
