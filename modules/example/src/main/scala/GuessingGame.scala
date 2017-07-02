package example

import scala.util.{ Random, Try, Success, Failure }
import cats._, cats.implicits._, cats.effect.IO
import cats.kernel.Comparison.{ LessThan => LT, GreaterThan => GT, EqualTo => EQ }
import tuco._, Tuco._

object GuessingGame {

  // A guessing game.
  def game(r: Random): SessionIO[Unit] =
    for {
      _ <- writeLn("Welcome to the guessing game!")
      a <- SessionIO.delay(r.nextInt(10) + 1)
      _ <- writeLn("I have chosen a number between 1 and 10, what is it?")
      _ <- loop(a, 1) // guessing loop with initial turn count of 1
      _ <- writeLn("Goodbye.")
    } yield ()

  // Our main game loop.
  def loop(answer: Int, guesses: Int): SessionIO[Unit] =
    readInt("Your guess? ").map(_ comparison answer).flatMap {
      case LT => writeLn("Nope, higher!") *> loop(answer, guesses + 1)
      case GT => writeLn("Nope, lower!")  *> loop(answer, guesses + 1)
      case EQ => if (guesses == 1) writeLn("Good guess! You won on the first try!")
                 else              writeLn(s"Right! It took $guesses tries.")
    }

  // A session program to read an Int.
  def readInt(prompt: String): SessionIO[Int] =
    readLn(prompt).map(s => Try(s.toInt)).flatMap {
      case Failure(_) => writeLn("That's not a number! Try again.") *> readInt(prompt)
      case Success(n) => SessionIO.pure(n)
    }

  // Simple server on the given port.
  def main(args: Array[String]): Unit =
    Config(game(new Random), 6666).run(simpleServer).unsafeRunSync

}
