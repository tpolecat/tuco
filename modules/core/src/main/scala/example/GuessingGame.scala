package example

import scala.util.Random
import scalaz._, Scalaz._, scalaz.effect.{ IO, SafeApp }
import scalaz.Ordering.{ LT, GT, EQ }
import tuco._, Tuco._

object GuessingGame extends SafeApp {

  // A guessing game.
  val game: SessionIO[Unit] =
    for {
      _ <- writeLn("Welcome to the guessing game!")
      _ <- writeLn("I have chosen a number between 1 and 10, what is it?")
      a <- SessionIO.delay(Random.nextInt(10) + 1)
      _ <- loop(a, 1)
      _ <- writeLn("Goodbye.")
    } yield ()

  // Our main game loop.
  def loop(answer: Int, guesses: Int): SessionIO[Unit] =
    readInt("Your guess? ").map(_ cmp answer).flatMap {
      case LT => writeLn("Nope, higher!") *> loop(answer, guesses + 1)
      case GT => writeLn("Nope, lower!")  *> loop(answer, guesses + 1)
      case EQ => if (guesses == 1) writeLn("Good guess! You won on the first try!")
                 else              writeLn(s"Right! It took $guesses tries.")
    }

  // A session program to read an Int.
  def readInt(prompt: String): SessionIO[Int] =
    readLn(prompt).map(_.parseInt.toOption).flatMap {
      case None    => writeLn("That's not a number! Try again.") *> readInt(prompt)
      case Some(n) => SessionIO.pure(n)
    }

  // The underlying Java code relies on reflection to instantiate sessions, so we need to wrap
  // our session in a new class that extends SafeShell. This implementation leakage will go away.
  class GameClass extends SafeShell(game)

  // Our configuration is parameterized on the session class. The only config value we can provide
  // right now is the port. Other things will be available soon.
  val config = Config[GameClass](6666)

  // With our config we can run a trivial server. This is our program's entry point.
  override def runc: IO[Unit] =
    config.run(simpleServer)

}
