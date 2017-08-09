package tuco

import tuco.free.{ connection    => FC }
import tuco.hi.{ connection      => HC }
import tuco.hi.{ basicterminalio => HBT }
import tuco.util.Zipper

import cats.implicits._

/** Module of SesionIO constructors. */
object SessionIO extends SessionIOFunctions {
  import FC.AsyncConnectionIO

  /** Construct a new primitive operation from side-effecting expression `a`. */
  def delay[A](a: => A): SessionIO[A] = FC.delay(a)

  /** Construct a new constant-value program. */
  def pure[A](a: A): SessionIO[A] = a.pure[SessionIO]

  /** The unit program, . */
  val unit: SessionIO[Unit] = pure(())

}

trait SessionIOFunctions {

  // N.B. this alias cannot appear in the package object due to SI-7139
  type SessionIO[A] = tuco.free.connection.ConnectionIO[A]

  def write(s: String): SessionIO[Unit] =
    HC.write(s)

  def writeLn(s: String): SessionIO[Unit] =
    HC.writeLn(s)

  val getRows: SessionIO[Int] =
    HC.getRows

  val getColumns: SessionIO[Int] =
    HC.getColumns

  /** Read given a prompt and history. */
  def readLn(
    prompt: String,
    history: Zipper[String] = Zipper.single(""),
    mask: Option[Char] = None,
    complete: HBT.Completer = HBT.Completer.empty
  ): SessionIO[String] =
    HC.readLn(prompt, history, mask, complete)

}
