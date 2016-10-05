package can.hi

import can.free.{ connection      => FC }
import can.free.{ basicterminalio => FBT }
import can.hi.{ basicterminalio => HBT }

import scalaz._, Scalaz._

import net.wimpi.telnetd.io.BasicTerminalIO.CRLF

object connection {

  private def liftBT[A](a: FBT.BasicTerminalIOIO[A]): FC.ConnectionIO[A] =
    FC.getTerminalIO.flatMap(t => FC.lift(t, a))

  /** Write and then flush. */
  def write(s: String): FC.ConnectionIO[Unit] =
    liftBT(FBT.write(s) *> FBT.flush)

  /** Write and then flush. */
  def writeLn(s: String): FC.ConnectionIO[Unit] =
    write(s + CRLF)

  /** Read given a prompt and history. */
  def readLn(prompt: String, history: Zipper[String]): FC.ConnectionIO[String] =
    liftBT(HBT.readLn(prompt, history))

}
