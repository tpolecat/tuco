package example

import net.bmjames.opts._
import scalaz._, Scalaz._, scalaz.effect._
import tuco._, Tuco._

object ShellExample extends SafeApp {

  // A shell command for a telnet server that carries user-defined state of type Int. Arguments
  // are the command name, a help string, and an optparse-applicative
  // Parser[Int => SessionIO[Int]]; i.e., a parser that yields an effectful
  // state transition, which is how we define a command's implementation. We then use `zoom` to
  // widen `Int` to `Session[Int]` which is what we need to construct a `Commands`.
  val add = Command(
    "add", "Add a number to the current count.",
    intArgument(metavar("<number>"), help("Number to add.")).map { n =>
       (data: Int) => writeLn(s"${data} + $n = ${data + n}").as(data + n)
    },
    (n: Int, s: String) => List.fill(1000)(scala.util.Random.nextInt(10000).toString).filter(_.startsWith(s)).point[SessionIO]
  ).zoom(Session.L.data[Int])

  // Our initial `CommandShell` state, which is a `Session` whose `data` slot carries an `Int`.
  // Available commands are the builtins (:help, :history, :exit) plus our `add` command defined
  // above, and we set a custom user prompt.
  val initialState: Session[Int] =
    Session.initial(42).copy(
      commands = Builtins[Int] |+| Commands(add),
      prompt   = "tuco> "
    )

  // When a user connects they have the following conversation. `CommandShell.run` enters a REPL
  // loop with the given initial session state, runs until the user exits, and yields the Final
  // state.
  val interact: SessionIO[Unit] =
    for {
      _ <- writeLn("Welcome to the Tuco demo.")
      n <- readLn("Your Name? ")
      _ <- readLn("Password? ", mask = Some('*'))
      _ <- writeLn(s"Hello $n. Initial count is ${initialState.data}")
      f <- runShell(initialState)
      _ <- writeLn("Goodbye. Final count was " + f.data)
    } yield ()

  // Simple server on the given port.
  override def runc: IO[Unit] =
    Config(interact, 6666).run(simpleServer)

}
