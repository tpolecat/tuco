package example

import scalaz.effect.{ IO, SafeApp }
import tuco._, Tuco._

object HelloWorld extends SafeApp {

  // Let's define a user session. This is what happens when a user telnets in.
  val hello: SessionIO[Unit] =
    for {
      _ <- writeLn("Hello World!")
      n <- readLn("What is your name? ")
      _ <- writeLn(s"Hello $n, and goodbye!")
    } yield ()

  // The underlying Java code relies on reflection to instantiate sessions, so we need to wrap
  // our session in a new class that extends SafeShell. This implementation leakage will go away.
  class HelloClass extends SafeShell(hello)

  // Our configuration is parameterized on the session class. The only config value we can provide
  // right now is the port. Other things will be available soon.
  val config = Config[HelloClass](6666)

  // With our config we can run a trivial server. This is our program's entry point.
  override def runc: IO[Unit] =
    config.run(simpleServer)

}
