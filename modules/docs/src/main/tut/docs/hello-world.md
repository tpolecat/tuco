---
layout: docs
title: Hello World
---

*The program described here is also available as `HelloWorld.scala` in the `examples` project.*

## Hello World

For our first **Tuco** program we will write a telnet server that greets the user, asks for her name, says hello, and then ends the session.

The first thing we need to do is bring **Tuco** types and constructors into scope. Fine-grained imports are also possible but in most cases this is the expected way to do things.

```tut:silent
import cats._, cats.implicits._, cats.effect._
import tuco._, Tuco._
```

We define the behavior of our server as a value of type `SessionIO[Unit]`. The server will take care of accepting the connection, negotiating a terminal type, etc., and will then interact with the user as we specify here.

```tut:silent
val hello: SessionIO[Unit] =
  for {
    _ <- writeLn("Hello World!")
    n <- readLn("What is your name? ")
    _ <- writeLn(s"Hello $n, and goodbye!")
  } yield ()
```

To complete the specification of our telnet server we construct a `Config` with our behavior and various server options. The defaults are reasonable so we will just provide a port, which is the only required option.

```tut:silent
val conf = Config[IO](hello, 6666)
```

We can now start our server by running the `.start` action, which starts up the server and returns an `IO` action that we can use to stop the server.

```tut:invisible
// define this before starting the server to ensure it compiles
val test = Expect(conf).dialog(
  "What is your name? " -> "Bob"
).expect("Hello Bob, and goodbye!").test
```

```tut
val stop = conf.start.unsafeRunSync
```

We can now connect to the server from another terminal window via `telnet`.

```tut:evaluated:plain
// run our test and ensure the server stops; the call to stop below is a no-op
println(test.handleErrorWith(_ => stop.as("oops")).unsafeRunSync)
```

We can do this as many times as we like, with a the default maximum of 25 simultaneous connections. To terminate the server go back to the REPL and run the `stop` program.

```tut
stop.unsafeRunSync
```

In the next chapter we will try a more complex interaction by implementing a [Guessing Game](guessing-game.html).
