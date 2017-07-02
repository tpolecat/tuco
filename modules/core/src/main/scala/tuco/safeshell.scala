package tuco

import tuco.free._
import tuco.free.{ connection => FC }
import tuco.hi.{ connection => HC }

import cats.effect.IO

/**
 * `Shell` implementation defined by a `ConnectionIO[Unit]`. This is an adapter that allows the
 * reflection-based machinery to construct and manage a shell defined in a reasomable way. With
 * some luck this will go away.
 */
abstract class SafeShell(val shellMain: FC.ConnectionIO[Unit]) extends net.wimpi.telnetd.shell.Shell {

  final def run(c: net.wimpi.telnetd.net.Connection): Unit =
    shellMain.transK[IO].run(c).unsafeRunSync

  import net.wimpi.telnetd.net.ConnectionEvent
  final def connectionIdle(ce: ConnectionEvent): Unit = ()
  final def connectionLogoutRequest(ce: ConnectionEvent): Unit = ()
  final def connectionSentBreak(ce: ConnectionEvent): Unit = ()
  final def connectionTimedOut(ce: ConnectionEvent): Unit = ()
}
