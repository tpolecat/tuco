package example

import cats.effect.IO
import tuco._, Tuco._

object HelloWorld {

  // Let's define a user session.
  val hello: SessionIO[Unit] =
    for {
      _ <- writeLn("Hello World!")
      n <- readLn("What is your name? ")
      _ <- writeLn(s"Hello $n, and goodbye!")
    } yield ()

  // Simple server on the given port.
  def main(args: Array[String]): Unit =
    Config(hello, 6666).run(simpleServer).unsafeRunSync

}
