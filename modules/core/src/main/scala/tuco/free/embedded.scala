package tuco.free

import cats.free.Free

import connection.ConnectionIO
import connectiondata.ConnectionDataIO
import connectionevent.ConnectionEventIO
import connectionlistener.ConnectionListenerIO
import basicterminalio.BasicTerminalIOIO
import telnetd.TelnetDIO

// A pair (J, Free[F, A]) with constructors that tie down J and F.
sealed trait Embedded[A]
object Embedded {
  final case class Connection[A](j: net.wimpi.telnetd.net.Connection, fa: ConnectionIO[A]) extends Embedded[A]
  final case class ConnectionData[A](j: net.wimpi.telnetd.net.ConnectionData, fa: ConnectionDataIO[A]) extends Embedded[A]
  final case class ConnectionEvent[A](j: net.wimpi.telnetd.net.ConnectionEvent, fa: ConnectionEventIO[A]) extends Embedded[A]
  final case class ConnectionListener[A](j: net.wimpi.telnetd.net.ConnectionListener, fa: ConnectionListenerIO[A]) extends Embedded[A]
  final case class BasicTerminalIO[A](j: net.wimpi.telnetd.io.BasicTerminalIO, fa: BasicTerminalIOIO[A]) extends Embedded[A]
  final case class TelnetD[A](j: net.wimpi.telnetd.TelnetD, fa: TelnetDIO[A]) extends Embedded[A]
}

// Typeclass for embeddable pairs (J, F)
trait Embeddable[F[_], J] {
  def embed[A](j: J, fa: Free[F, A]): Embedded[A]
}

