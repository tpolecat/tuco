package example

import com.monovore.decline.{ Command => Cmd, _ }
import cats._, cats.implicits._, cats.effect._
import tuco._, Tuco._
import tuco.shell._

object TodoList extends IOApp {

  case class Todo(text: String)

  type TodoState   = List[Todo]
  type TodoAction  = TodoState => SessionIO[TodoState]
  type TodoSession = Session[TodoState]
  type TodoCommand = Command[SessionIO, TodoSession]

  // Action that inserts a new item at offset `i` in the list (or first/last when out of bounds).
  def add(index: Int, text: String): TodoAction = ts =>
    ts.patch(index, List(Todo(text)), 0).pure[SessionIO]

  // Action that deletes the todo at the given index.
  def delete(index: Int): TodoAction = ts =>
    if (ts.isDefinedAt(index)) ts.patch(index, Nil, 1).pure[SessionIO]
    else writeLn(s"No such todo!").as(ts)

  // Action that clears the list out.
  val clear: TodoAction = ts =>
    readLn("Are you sure (yes/no)? ").map(_.trim.toLowerCase).map {
      case "yes" => Nil
      case _     => ts
    }

  // Action that lists the current todos. We number them and truncate the resulting string at the
  // column limit in order to avoid wrapping. In the real world you might incorporate a world-
  // wrapping algorithm here.
  val list: TodoAction = ts =>
    for {
      cs <- getColumns
      ss  = ts.zipWithIndex.map { case (Todo(s), n) => f"${n + 1}%3d. $s".take(cs) }
      _  <- ss.traverse(writeLn)
    } yield ts

  // A command that makes the `add` action above available to the user.
  val addCommand: TodoCommand = {

    // Opts for the index option, which is a 1-based index into the list where we want the new
    // todo item to appear. We subtract one so we can deal with the zero-based index internally.
    val ind: Opts[Int] =
      Opts.option[Int](
        help = "List index where the todo should appear.",
        short = "i",
        long = "index",
        metavar = "index"
      ).withDefault(1).map(_ - 1)

    // Opts for the text argument.
    val txt: Opts[String] =
      Opts.argument[String](metavar = "\"text\"")

    // We can now construct the final command.
    Command("add", "Add a new todo.", (ind, txt).mapN(add))
      .zoom(Session.data[List[Todo]])

  }

  // Delete command with its argument defined inlined.
  val deleteCommand: TodoCommand =
    Command("delete", "Delete the specified item.",
      Opts.argument[Int](
        // help("Todo item to delete."),
        metavar = "index"
      ).map(n => delete(n - 1)))
      .zoom(Session.data[List[Todo]])

  // The list and clear commands takes no arguments at all.
  val listCommand: TodoCommand =
    Command("list", "List the todo items.", list.pure[Opts])
      .zoom(Session.data[List[Todo]])

  val clearCommand: TodoCommand =
    Command("clear", "Clears the todo list.", clear.pure[Opts])
      .zoom(Session.data[List[Todo]])

  val TodoCommands = Commands(addCommand, deleteCommand, listCommand, clearCommand)

  val initialState: Session[List[Todo]] =
    Session.initial(Nil).copy(
      commands = Builtins[List[Todo]] |+| TodoCommands,
      prompt   = "todo> "
    )

  val interact: SessionIO[Unit] =
    for {
      _ <- writeLn("Welcome to TODO!")
      f <- runShell(initialState)
      _ <- writeLn("Goodbye.")
    } yield ()

  // Simple server on the given port.
  def run(args: List[String]): IO[ExitCode] =
    Config[IO](interact, 6666)
      .run(simpleServer)
      .as(ExitCode.Success)

}
