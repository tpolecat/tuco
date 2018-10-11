---
layout: docs
title: A Guessing Game
---

*The program described here is also available as `GuessingGame.scala` in the `examples` project.*

## A Guessing Game

For our next **Tuco** program we will build a telnet server that plays a guessing game with the user. We will need some additional imports.

```tut:silent
import scala.util.{ Random, Try, Success, Failure }
import cats._, cats.implicits._, cats.kernel.Comparison._, cats.effect._
import tuco._, Tuco._
```

This time our behavior is a bit different. We say hello and goodbye much a before, but the main gameplay is delegated to a `loop` program that we will define later. (If you're following along in the REPL skip this definition until we have defined `loop`.)

```tut:invisible
// hidden forward definition
def loop(answer: Int, guesses: Int): SessionIO[Unit] = null
```

```tut:silent
def game(r: Random): SessionIO[Unit] =
  for {
    _ <- writeLn("Welcome to the guessing game!")
    a <- SessionIO.delay(r.nextInt(10) + 1)
    _ <- writeLn("I have chosen a number between 1 and 10, what is it?")
    _ <- loop(a, 1) // guessing loop with initial turn count of 1
    _ <- writeLn("Goodbye.")
  } yield ()
```

Note that the random number generation is a side-effect, which we must capture via the `SessionIO.delay` operation. This gives us an FFI to the unsafe part of the language analogous to scalaz `Task.delay` or `IO.apply`.

Because we will be reading an `Int` from the user, we need to provide a program that knows how to do this. So we define the following program that prompts the user, attempts to parse the input, and either returns the value or reports an error and tries again.

```tut:silent
def readInt(prompt: String): SessionIO[Int] =
  readLn(prompt).map(s => Try(s.toInt)).flatMap {
    case Failure(_) => writeLn("That's not a number! Try again.") *> readInt(prompt)
    case Success(n) => SessionIO.pure(n)
  }
```

With that out of the way we can implement our game loop. We read an `Int` then compare it with our answer, either giving the user a clue and looping, or congratulating the user if the guess is correct.

```tut:silent
def loop(answer: Int, guesses: Int): SessionIO[Unit] =
  readInt("Your guess? ").map(_ comparison answer).flatMap {
    case LessThan    => writeLn("Nope, higher!") *> loop(answer, guesses + 1)
    case GreaterThan => writeLn("Nope, lower!")  *> loop(answer, guesses + 1)
    case EqualTo     => if (guesses == 1) writeLn("Good guess! You won on the first try!")
                        else writeLn(s"Right! It took $guesses tries.")
  }
```

```tut:invisible
// redefine
def game(r: Random): SessionIO[Unit] =
  for {
    _ <- writeLn("Welcome to the guessing game!")
    a <- SessionIO.delay(r.nextInt(10) + 1)
    _ <- writeLn("I have chosen a number between 1 and 10, what is it?")
    _ <- loop(a, 1) // guessing loop with initial turn count of 1
    _ <- writeLn("Goodbye.")
  } yield ()
```

Our game is fully implemented and we can define the server config.

```tut:silent
val conf = Config[IO](game(new Random(123L)), 6666)
```

```tut:invisible
// define this before starting the server to ensure it compiles
val test = Expect(conf).dialog(
  "Your guess? " -> "five",
  "Your guess? " -> "5",
  "Your guess? " -> "3"
).expect("Goodbye.").test

```

Start the server up.

```tut
val stop = conf.start.unsafeRunSync
```

We can now connect on port 6666 via telnet and enjoy our fabulous game.

```tut:evaluated
// run our test and ensure the server stops; the call to stop below is a no-op
println(test.handleErrorWith(_ => stop.as("oops")).unsafeRunSync)
```

Shut the server down when you're done playing.

```tut
stop.unsafeRunSync
```
