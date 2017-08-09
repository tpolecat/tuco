package tuco.hi

import tuco.free.{ connection => FC }
import tuco.free.{ basicterminalio => FBT }
import tuco.hi.{ basicterminalio => HBT }
import tuco.util.Zipper

import cats._
import cats.data._
import cats.implicits._

import net.wimpi.telnetd.io.BasicTerminalIO.CRLF

object connection {

  import FC.AsyncConnectionIO
  import FBT.AsyncBasicTerminalIOIO

  private def liftBT[A](a: FBT.BasicTerminalIOIO[A]): FC.ConnectionIO[A] =
    FC.getTerminalIO.flatMap(t => FC.embed(t, a))

  /** Write and then flush. */
  def write(s: String): FC.ConnectionIO[Unit] =
    liftBT(FBT.write(s) *> FBT.flush)

  /** Write and then flush. */
  def writeLn(s: String): FC.ConnectionIO[Unit] =
    write(s + CRLF)

  /** Read given a prompt and history. */
  def readLn(
    prompt: String,
    history: Zipper[String] = Zipper.single(""),
    mask: Option[Char] = None,
    complete: HBT.Completer = HBT.Completer.empty
  ): FC.ConnectionIO[String] =
    liftBT(HBT.readLn(prompt, history, mask, complete))

  val getRows: FC.ConnectionIO[Int] =
    liftBT(FBT.getRows)

  val getColumns: FC.ConnectionIO[Int] =
    liftBT(FBT.getColumns)

}
