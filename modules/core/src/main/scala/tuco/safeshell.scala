package tuco

import net.wimpi.telnetd.net.Connection

import tuco.free._
import tuco.free.{ connection => FC }
import tuco.hi.{ connection => HC }

import cats.{ ~>, Monad }
import cats.data.Kleisli
import cats.effect.Effect
import cats.effect.implicits._
import cats.implicits._

/**
 * `Shell` implementation defined by a `ConnectionIO[Unit]`. This is an adapter that allows the
 * reflection-based machinery to construct and manage a shell defined in a reasomable way. With
 * some luck this will go away.
 */
abstract class SafeShell[F[_]: Effect](
  val shellMain: FC.ConnectionIO[Unit],
  val interpreter: FC.ConnectionOp ~> Kleisli[F, Connection, ?]
) extends net.wimpi.telnetd.shell.Shell {

  final def run(c: net.wimpi.telnetd.net.Connection): Unit =
    shellMain.foldMap(interpreter).run(c).toIO.unsafeRunSync

  import net.wimpi.telnetd.net.ConnectionEvent
  final def connectionIdle(ce: ConnectionEvent): Unit = ()
  final def connectionLogoutRequest(ce: ConnectionEvent): Unit = ()
  final def connectionSentBreak(ce: ConnectionEvent): Unit = ()
  final def connectionTimedOut(ce: ConnectionEvent): Unit = ()
}
