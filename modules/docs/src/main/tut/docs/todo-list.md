---
layout: docs
title: Command Shell To-Do List
---

*The program described here is also available as `TodoList.scala` in the `examples` project.*

## Command Shell To-Do List

In this tutorial we will step things up a bit by introducing the **Command Shell** API, which provides a way to define behavior that works like a Unix shell, with commands, help, history, tab completion, and so on.

### Preliminaries

First let's deal with imports. Note that we're importing the contents `com.monovore.decline` from the [**decline**](https://github.com/bkirwi/decline) library, which is included as a dependency of **Tuco**. We will use this functionality to define *parsers* for our commands. We also need `tuco.shell._` here.

```tut:silent
import com.monovore.decline.{ Command => Cmd, _ }
import cats._, cats.implicits._, cats.effect._
import tuco._, Tuco._
import tuco.shell._
```

Our application will manage a to-do list, providing the user with commands to add, delete, clear, and display the list. Our `Todo` type just wraps a `String` for now. As an exercise you may wish to add other fields (and commands to manipulate them).

```tut:silent
case class Todo(text: String)
```

Let's define some type aliases to ease things along.

```tut:silent
type TodoState = List[Todo]
object TodoState {
  val empty = Nil
}

type TodoAction  = TodoState => SessionIO[TodoState]
```

Let's decode these a bit.

- Our `TodoState` represents the behavior-specific state that this server will manage for each user session. Here it's just a list of to-do items.
- A `TodoAction` is a state transition that takes the current state and computes a new state, perhaps performing `SessionIO` actions as well.

### The Game Plan

We will define our telnet behavior in three steps.

1. We will first define `TodoAction`s that encapsulate the "business logic" of our commands.
2. We will construct *parsers* that construct these actions. This is how we translates something the user types into a program we can run.
3. We will construct an initial session state that includes our commands, a prompt string, our [initially empty] to-do list. From here the shell implementation is provided for us.

### Defining our Actions

A `TodoAction` is an effectful state transition that takes a `TodoState` (a list of to-do items) and computes a new state, perhaps interacting with the user via `SessionIO` effects.

Our first action will insert a new item at offset `i` in the to-do list. By using `patch` we make this operation safe for out-of-bounds indices. The implementation has no `SessionIO` effect; it simply computes the new state.

```tut:silent
def add(index: Int, text: String): TodoAction = ts =>
  ts.patch(index, List(Todo(text)), 0).pure[SessionIO]
```

Our next action deletes the to-do item at the given index. Here we check the index and either point the computed state or complain to the user that the index is out of bound and return the state unchanged.

```tut:silent
def delete(index: Int): TodoAction = { ts =>
  if (ts.isDefinedAt(index)) ts.patch(index, Nil, 1).pure[SessionIO]
  else writeLn(s"No such todo!").as(ts)
}
```

Our action that clears the list prompts the user to confirm, and returns either `Nil` or the current state depending on the user's answer.

```tut:silent
val clear: TodoAction = ts =>
  readLn("Are you sure (yes/no)? ").map(_.trim.toLowerCase).map {
    case "yes" => Nil
    case _     => ts
  }
```

To display the to-do list to the user we number them, then truncate the resulting string at the terminal's column limit in order to avoid wrapping. In the real world you might incorporate a word-wrapping algorithm here.

```tut:silent
val list: TodoAction = ts =>
  for {
    cs <- getColumns
    ss  = ts.zipWithIndex.map { case (Todo(s), n) => f"${n + 1}%3d. $s".take(cs) }
    _  <- ss.traverse(writeLn)
  } yield ts
```

We have now defined our four actions. Next we will define *commands* that give our actions a textual representation for our users.

### Defining Commands

`Command[F[_], A]` is a data type with four fields:

1. A **name**, which is simply a `String` like `"add"` or `"delete"`.
2. A **description**, which is a `String` that will be used to generate help documentation.
3. A **parser** that consumes `String` command arguments and returns an state transition `A => F[A]` for some arbitrary effect and state type (such as our actions above).
4. An optional **tab completer** for expanding command arguments when the user hits the Tab key. We will not use tab completion in this exercise.

Let's walk through the first command slowly to be sure it all makes sense.

### Implementing the Add Command

Recall the `add` action we defined above.

```tut:silent
def add(index: Int, text: String): TodoAction = ts =>
  ts.patch(index, List(Todo(text)), 0).pure[SessionIO]
```

In order to call this action we need to parse two arguments from the commandline that the user types in: the index and the text. And in order to generate useful help documentation we need to provide some metadata about what the options mean. This is exactly what the [**scala-optparse-applicative**](https://github.com/bmjames/scala-optparse-applicative) library does, so **Tuco** relies on it directly.

Here is the parser for our `index` argument.

```tut:silent
val ind: Opts[Int] =
  Opts.option[Int](
    help = "List index where the todo should appear.",
    short = "i",
    long = "index",
    metavar = "index"
  ).withDefault(1).map(_ - 1) // 1-based for the user, 0-based internally
```

We call `intOption` which constructs a `Parser[Int]` whose behavior is defined by the provided sequence of modifiers:

- A `help` string describing the option.
- A `short` option flag. This means we can say `-i 42`.
- A `long` option flag. This means we can say `--index 42`.
- A `metavar` for the generated help string (see below).
- A default value of `1`, if the user doesn't specify.

The second argument to `add` is the to-do item text, which is an arbitrary string. In our command this will be a required *argument* so we use `strArgument` to construct its parser.

```tut:silent
val txt: Opts[String] =
  Opts.argument[String](metavar = "\"text\"")
```

We now have what we need to construct a `Command`. We combine the parsers with `mapN` to yield a `Parser[TodoAction]` which is what we need for the third construtor argument.

```tut:silent
val addCommand: Command[SessionIO, TodoState] =
  Command("add", "Add a new todo.", (ind, txt).mapN(add))
```

### Generalizing our State

We have defined a command that is specific to our domain model: the state type we are passing is `TodoState`. But the command shell also has other state that it passes along, including the user prompt, command history, and the list of available commands. *All of this state is available to our commands.* But for now we only need our domain-specific hunk.

The state passed by the command shell is called `Session[A]` where the domain-specific hunk is passed via the `data` field of type `A`. So what we really need in order to make our command compatible with the shell machinery is a `Command[SessionIO, Session[TodoState]]`. We can do this by applying the `zoom` operator that lenses down from the `Session` to the `TodoState`.

```tut:silent
val addCommand: Command[SessionIO, Session[TodoState]] = {
  Command("add", "Add a new todo.", (ind, txt).mapN(add))
    .zoom(Session.data[TodoState]) // Session.data is a lens
}
```

This is our final `addCommand` implementation.

### Implementing the Remaining Commands

The `delete` command takes a single argument so it's reasonable to write the parser inline.

```tut:silent
val deleteCommand = {
  Command("delete", "Delete the specified item.",
    Opts.argument[Int](
      metavar = "index"
    ).map(n => delete(n - 1)))
    .zoom(Session.data[List[Todo]])
}
```

The list and clear commands takes no arguments at all, so the parsers are simply lifted values.

```tut:silent
val listCommand = {
  Command("list", "List the todo items.", list.pure[Opts])
    .zoom(Session.data[List[Todo]])
}

val clearCommand = {
  Command("clear", "Clears the todo list.", clear.pure[Opts])
    .zoom(Session.data[List[Todo]])
}
```

### Putting it All Together

Now that we have defined our commands all that is remaining is to construct our initial session state to pass to `runShell`. We construct an initial `Session` with an empty to-do list, a custom command prompt, and a set of commands that includes `Builtins` which gives us our `help`, `history`, and `exit` commands.

```tut:silent
val TodoCommands = Commands(addCommand, deleteCommand, listCommand, clearCommand)

val initialState: Session[TodoState] =
  Session.initial(TodoState.empty).copy(
    prompt   = "todo> ",
    commands = Builtins[TodoState] |+| TodoCommands
  )
```

Our session behavior greets the user and runs the command shell with our initial state, yielding the final state. We report the length of our list and then exit.

```tut:silent
val todo: SessionIO[Unit] =
  for {
    _ <- writeLn("Welcome to TODO!")
    s <- runShell(initialState)
    _ <- writeLn(s"Exiting with ${s.data.length} item(s) on your list.")
  } yield ()
```

Our configuration is as before.

```tut:silent
val conf = Config[IO](todo, 6666)
```

We can now run our to-do server and connect from another terminal window via `telnet`.

```tut:invisible
// define this before starting the server to ensure it compiles
val test = Expect(conf).dialog(
  "todo> "     -> "help",
  "todo> "     -> "add -h",
  "todo> "     -> """add \"Buy eggs.\"""",
  "todo> "     -> """add \"Wash the cat.\"""",
  "todo> "     -> "list",
  "todo> "     -> "delete 42",
  "todo> "     -> "clear",
  "(yes/no)? " -> "yes",
  "todo> "     -> "list",
  "todo> "     -> "exit"
).expect("in your to-do list.").test
```

```tut
val stop = conf.start.unsafeRunSync
```

Here is an example session.

```tut:evaluated:plain
// run our test and ensure the server stops; the call to stop below is a no-op
println(test.handleErrorWith(_ => stop.as("oops")).unsafeRunSync)
```

Shut the server down when you're done.

```tut
stop.unsafeRunSync
```
