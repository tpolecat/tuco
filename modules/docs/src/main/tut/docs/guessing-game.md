---
layout: docs
title: A Guessing Game
---

*The program described here is also available as `GuessingGame.scala` in the `examples` project.*

## A Guessing Game

For our next **Tuco** program we will build a telnet server that plays a guessing game with the user. We will need some additional imports.

```tut:silent
import scala.util.Random
import scalaz._, Scalaz._, Ordering._
import tuco._, Tuco._
```

This time our behavior is a bit different. We say hello and goodbye much a before, but the main gameplay is delegated to a `loop` program that we will define later.

```tut:invisible
// hidden forward definition
def loop(answer: Int, guesses: Int): SessionIO[Unit] = null
```

```tut:silent
val game: SessionIO[Unit] =
  for {
    _ <- writeLn("Welcome to the guessing game!")
    a <- SessionIO.delay(Random.nextInt(10) + 1)
    _ <- writeLn("I have chosen a number between 1 and 10, what is it?")
    _ <- loop(a, 1) // guessing loop with initial turn count of 1
    _ <- writeLn("Goodbye.")
  } yield ()
```

Note that the random number generation is a side-effect, which we must capture via the `SessionIO.delay` operation. This gives us an FFI to the unsafe part of the language analogous to scalaz `Task.delay` or `IO.apply`.

Because we will be reading an `Int` from the user, we need to provide a program that knows how to do this. So we define the following program that prompts the user, attempts to parse the input, and either returns the value or reports and error and tries again.

```tut:silent
def readInt(prompt: String): SessionIO[Int] =
  readLn(prompt).map(_.parseInt).flatMap {
    case Failure(_) => writeLn("That's not a number! Try again.") *> readInt(prompt)
    case Success(n) => SessionIO.pure(n)
  }
```

With that out of the way we can implement our game loop. We read an `Int` then compare it with our answer, either giving the user a clue and looping, or congratulating the user if the guess is correct.

```tut:silent
def loop(answer: Int, guesses: Int): SessionIO[Unit] =
  readInt("Your guess? ").map(_ cmp answer).flatMap {
    case LT => writeLn("Nope, higher!") *> loop(answer, guesses + 1)
    case GT => writeLn("Nope, lower!")  *> loop(answer, guesses + 1)
    case EQ => if (guesses == 1) writeLn("Good guess! You won on the first try!")
               else              writeLn(s"Right! It took $guesses tries.")
  }
```

Our behavior is now fully specified and we can run our `simpleServer` as before.

```
scala> Config(game, 6666).run(simpleServer).unsafePerformIO
Press <enter> to exit...
Oct 23, 2016 1:15:02 PM net.wimpi.telnetd.net.PortListener run
INFO: Listening to Port 6,666 with a connectivity queue size of 5.
```

When we connect via telnet we can play our game.

```
$ telnet localhost 6666
Trying ::1...
Connected to localhost.
Escape character is '^]'.
Welcome to the guessing game!
I have chosen a number between 1 and 10, what is it?
Your guess? three
That's not a number! Try again.
Your guess? 3
Nope, higher!
Your guess? 7
Nope, lower!
Your guess? 5
Nope, lower!
Your guess? 4
Right! It took 4 tries.
Goodbye.
Connection closed by foreign host.
$
```
