---
layout: docs
title: Command Shell Example
---

# Command Shell Example

Here is a minimal **tuco** program that sets up a shell with one command that allows the user to increment a counter. First let's do the imports.

```tut:silent
import tuco.imports._
import net.bmjames.opts._
import scalaz._, Scalaz._, scalaz.effect._

import Session.L // module of Session lenses
```

### Defining a Shell Command

A shell command for a telnet server that carries user-defined state of type Int. Arguments are the command name, a help string, and an optparse-applicative `Parser[Session[Int] => ConnectionIO[Session[Int]]]`; i.e., a parser that yields an effectful state transition, which is how we define a command's implementation.

```tut:silent
val add = Command(
  "add", "Add a number to the current count.",
  intArgument(metavar("<number>"), help("Number to add.")).map { n =>
     (data: Int) => HC.writeLn(s"${data} + $n = ${data + n}").as(data + n)
  }
).zoom(L.data[Int])
```

Our initial `CommandShell` state, which is a `Session` whose `data` slot carries an `Int`. Available commands are the builtins (`:help`, `:history`, `:exit`) plus our `add` command defined above, and we set a custom user prompt.

```tut:silent
val initialState: Session[Int] =
  Session.initial(42).copy(
    commands = Builtins[Int] |+| Commands(add),
    prompt   = "tuco> "
  )
```

Note that a command can change the state arbitrarily, which means it can add or remove commands, change the prompt, examine command history, etc.

### Defining the Interaction

When a user connects they have the following interaction. `CommandShell.run` enters a REPL loop with the given initial session state, runs until the user exits, and yields the final state.

```tut:silent
val interact: ConnectionIO[Unit] =
  for {
    _ <- HC.writeLn("Welcome to the Tuco demo.")
    n <- HC.readLn("Your Name? ")
    _ <- HC.writeLn(s"Hello $n. Initial count is ${initialState.data}")
    f <- CommandShell.run(initialState)
    _ <- HC.writeLn("Goodbye. Final count was " + f.data)
  } yield ()
```

Because the old TelnetD insists on doing everything with reflection we have to wrap our above definition in a new class with an empty constructor. This restriction will go away.

```tut:silent
class Example extends SafeShell(interact)
```

### Running the Server

Our top-level program starts up the `TelnetD`, prints some stuff, waits for the someone to press enter, then exits. While it's waiting users can connect via telnet.

```tut:silent
val telnetMain: TelnetDIO[Unit] =
  for {
    _ <- FT.start // returns immediately
    _ <- FT.delay(System.out.println("Press <enter> to exit..."))
    _ <- FT.delay(System.in.read)
    _ <- FT.stop  // closes all connections
    _ <- FT.delay(System.out.println("Bye."))
  } yield ()
```


Our `TelnetD` configuration specifies a lot of things by default, in a `Properties` sadly, but at least the important bits (shell type and port) can be specified safely. This will get better. The type argument `Example` is the wrapper class defined above.

```tut:silent
val config = Config[Example](6666)
```

Given a `Config` we can interpret our `TelnetDIO` program into `IO`.

```tut
val go: IO[Unit] = config.run(telnetMain)
```
