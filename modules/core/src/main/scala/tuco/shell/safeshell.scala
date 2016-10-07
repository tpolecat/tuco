package tuco.shell

import tuco.free._
import tuco.free.{ connection => FC }
import tuco.hi.{ connection => HC }

import net.bmjames.opts._

import scalaz._, Scalaz.{ some => _, _ }, scalaz.effect._

/**
 * `Shell` implementation defined by a `ConnectionIO[Unit]`. This is an adapter that allows the
 * reflection-based machinery to construct and manage a shell defined in a reasomable way. With
 * some luck this will go away.
 */
trait SafeShell extends net.wimpi.telnetd.shell.Shell {

  def shellMain: FC.ConnectionIO[Unit]

  final def run(c: net.wimpi.telnetd.net.Connection): Unit =
    shellMain.transK[IO].run(c).unsafePerformIO

  import net.wimpi.telnetd.net.ConnectionEvent
  final def connectionIdle(ce: ConnectionEvent): Unit = ()
  final def connectionLogoutRequest(ce: ConnectionEvent): Unit = ()
  final def connectionSentBreak(ce: ConnectionEvent): Unit = ()
  final def connectionTimedOut(ce: ConnectionEvent): Unit = ()
}
