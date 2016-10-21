package example

import scalaz.effect.{ IO, SafeApp }
import tuco._, Tuco._

object HelloWorld extends SafeApp {

  // Let's define a user session.
  val hello: SessionIO[Unit] =
    for {
      _ <- writeLn("Hello World!")
      n <- readLn("What is your name? ")
      _ <- writeLn(s"Hello $n, and goodbye!")
    } yield ()

  // Simple server on the given port.
  override def runc: IO[Unit] =
    Config(hello, 6666).run(simpleServer)

}
