package tuco

import tuco.free.{ telnetd    => FT }
import cats.implicits._

object ServerIO extends ServerIOFunctions {
  import FT.AsyncTelnetDIO

  /** Construct a new primitive operation from side-effecting expression `a`. */
  def delay[A](a: => A): ServerIO[A] = FT.delay(a)

  /** Construct a new constant-value program. */
  def pure[A](a: A): ServerIO[A] = a.pure[ServerIO]

  /** The unit program, . */
  val unit: ServerIO[Unit] = pure(())

}

trait ServerIOFunctions {

  // N.B. this alias cannot appear in the package object due to SI-7139
  type ServerIO[A]  = tuco.free.telnetd.TelnetDIO[A]

  val startServer: ServerIO[Unit] = FT.start

  val stopServer:  ServerIO[Unit] = FT.stop

  val simpleServer: ServerIO[Unit] =
    for {
      _ <- startServer // returns immediately
      _ <- ServerIO.delay(System.out.println("Press <enter> to exit..."))
      _ <- ServerIO.delay(System.in.read)
      _ <- stopServer  // closes all connections immediately
      _ <- ServerIO.delay(System.out.println("Bye."))
    } yield ()

}
