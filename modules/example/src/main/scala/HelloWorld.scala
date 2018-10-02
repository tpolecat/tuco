package example

import cats.effect._, cats.implicits._
import tuco._, Tuco._

object HelloWorld extends IOApp {

  // Let's define a user session.
  val hello: SessionIO[Unit] =
    for {
      _ <- writeLn("Hello World!")
      n <- readLn("What is your name? ")
      _ <- writeLn(s"Hello $n, and goodbye!")
    } yield ()

  // Simple server on the given port.
  def run(args: List[String]): IO[ExitCode] =
    Config[IO](hello, 6666)
      .run(simpleServer)
      .as(ExitCode.Success)

}
