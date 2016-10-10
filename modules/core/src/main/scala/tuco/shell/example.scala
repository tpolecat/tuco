package tuco.example

import tuco.imports._
import net.bmjames.opts._
import scalaz._, Scalaz._, scalaz.effect._

object Example extends SafeApp {
  import Session.L // module of Session lenses

  // A shell command for an telnet server that carries user-defined state of type `Int`. Here we
  // just keep a per-session counter that the user can add to. The third argument is the interesting
  // one. It's an optparse-applicative `Parser[EndoT[ConnectionIO, Session[Int]]]` that we have
  // lensed down to `EndoT[ConnectionIO, Int]`. This is very simple but needs doc, sorry.
  val add: Command[Int] =
    Command(
      "add", "Add a number to the current count.",
      intArgument(metavar("<number>"), help("Number to add.")).map { n =>
        L.data[Int].endoT[ConnectionIO] { data =>
          HC.writeLn(s"${data} + $n = ${data + n}").as(data + n)
        }
      }
    )

  // Our initial `CommandShell` state, which is a `Session` whose `data` slot carries an `Int`.
  // Available commands are the builtins (help, history, exit) plus our `add` command defined
  // above, and we set a custom user prompt.
  val initialState: Session[Int] =
    Session.initial(42).copy(
      commands = Builtins[Int] |+| Commands(add),
      prompt   = "tuco> "
    )

  // When a user connects they have the following conversation. `CommandShell.run` enters a REPL
  // loop with the given initial session state, runs until the user exits, and yields the Final
  // state.
  val interact: ConnectionIO[Unit] =
    for {
      _ <- HC.writeLn("Welcome to the Tuco demo.")
      n <- HC.readLn("Your Name? ")
      _ <- HC.writeLn(s"Hello $n. Initial count is ${initialState.data}")
      f <- CommandShell.run(initialState)
      _ <- HC.writeLn("Goodbye. Final count was " + f.data)
    } yield ()

  // Because the old TelnetD insists on doing everything with reflection we have to wrap our
  // above definition in a new class with an empty constructor. This restriction will go away.
  class Example extends SafeShell(interact)

  // Our top-level program starts up the `TelnetD`, prints some stuff, waits for the user to press
  // enter, then exits. While it's waiting users can connect on port 6666.
  val telnetMain: TelnetDIO[Unit] =
    for {
      _ <- FT.start // returns immediately
      _ <- FT.delay(System.out.println("Press <enter> to exit..."))
      _ <- FT.delay(System.in.read)
      _ <- FT.stop  // closes all connections
      _ <- FT.delay(System.out.println("Bye."))
    } yield ()

  // Our `TelnetD` configuration specifies a lot of things by default, in a `Properties` sadly, but
  // at least the important bits (shell type and port) can be specified safely. This will get better.
  val config = Config[Example](6666)

  // Given a `Config` we can interpret our `TelnetDIO` program into `IO`, which is what we need to
  // implement `runc`. So now we're done.
  override def runc: IO[Unit] =
    config.run(telnetMain)

}
