package tuco

import tuco.free.{ connection    => FC }
import tuco.hi.{ connection      => HC }
import tuco.hi.{ basicterminalio => HBT }

import scalaz._, Scalaz._

/** Module of SesionIO constructors. */
object SessionIO extends SessionIOFunctions {

    /** Construct a new primitive operation from side-effecting expression `a`. */
    def delay[A](a: => A): SessionIO[A] = FC.delay(a)

    /** Construct a new constant-value program. */
    def pure[A](a: A): SessionIO[A] = a.point[SessionIO]

    /** The unit program, . */
    val unit: SessionIO[Unit] = pure(())

}

trait SessionIOFunctions {

  def write(s: String): SessionIO[Unit] =
    HC.write(s)

  def writeLn(s: String): SessionIO[Unit] =
    HC.writeLn(s)

  val getRows: SessionIO[Int] =
    HC.getRows

  val getColumns: SessionIO[Int] =
    HC.getColumns

  def runShell[A](init: Session[A]): SessionIO[Session[A]] =
    CommandShell.run(init)

  /** Read given a prompt and history. */
  def readLn(
    prompt: String,
    history: Zipper[String] = NonEmptyList("").toZipper,
    mask: Option[Char] = None,
    complete: HBT.Completer = HBT.Completer.empty
  ): SessionIO[String] =
    HC.readLn(prompt, history, mask, complete)

}
