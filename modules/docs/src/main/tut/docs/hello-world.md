---
layout: docs
title: Hello World
---

*The program described here is also available as `HelloWorld.scala` in the `examples` project.*

## Hello World

For our first **Tuco** program we will write a telnet server that greets the user, asks for her name, says hello, and then ends the session.

The first thing we need to do is bring **Tuco** types and constructors into scope. Fine-grained imports are also possible but in most cases this is the expected way to do things.

```tut:silent
import tuco._, Tuco._
```

We define the behavior of our server as a value of type `SessionIO[Unit]`. The server will take care of accepting the connection, negotiating a terminal type, etc., and will then interact with the user as we specify here.

```tut:silent
val hello: SessionIO[Unit] =
  for {
    _ <- writeLn("Hello World!")             // write a CRLF-terminated line and flush
    n <- readLn("What is your name? ")       // prompt and await input
    _ <- writeLn(s"Hello $n, and goodbye!")  // write again, and we're done
  } yield ()
```

To build an `IO` program that runs a server with our `hello` behavior we construct a `Config`, and then call `run` with a server implementation of type `ServerIO[Unit]`. In this case we will use the provided `simpleServer`; we will talk about writing a "real" server in a later chapter.

```tut:silent
val io = Config(hello, 6666).run(simpleServer) // port 6666
```

Running this program starts a telnet server that runs until we press Enter. Note that the logging is an artifact of the underlying `net.wimpi.telnetd` library; it will be brought under better control in a later version.

```
scala> io.unsafePerformIO
Press <enter> to exit...
Oct 23, 2016 1:15:02 PM net.wimpi.telnetd.net.PortListener run
INFO: Listening to Port 6,666 with a connectivity queue size of 5.
```

We can now connect to the server from another terminal window via `telnet`.

```
$ telnet localhost 6666
Trying ::1...
Connected to localhost.
Escape character is '^]'.
Hello World!
What is your name? Bob
Hello Bob, and goodbye!
Connection closed by foreign host.
$
```

We can do this as many times as we like, with a the default maximum of 25 simultaneous connections. To terminate the server go back to the REPL and press Enter.

```
Oct 23, 2016 1:18:06 PM net.wimpi.telnetd.net.PortListener stop
INFO: stop()::Stopped net.wimpi.telnetd.net.PortListener@47196bec
Bye.

scala>
```

In the next chapter we will try a more complex interaction by implementing a [Guessing Game](guessing-game.html).
