package example

import net.bmjames.opts._
import scalaz._, Scalaz._, scalaz.effect._
import tuco._, Tuco._
import tuco.shell._

object TodoList extends SafeApp {

  case class Todo(text: String)

  type TodoState   = List[Todo]
  type TodoAction  = TodoState => SessionIO[TodoState]
  type TodoSession = Session[TodoState]
  type TodoCommand = Command[SessionIO, TodoSession]

  // Action that inserts a new item at offset `i` in the list (or first/last when out of bounds).
  def add(index: Int, text: String): TodoAction = ts =>
    ts.patch(index, List(Todo(text)), 0).point[SessionIO]

  // Action that deletes the todo at the given index.
  def delete(index: Int): TodoAction = ts =>
    if (ts.isDefinedAt(index)) ts.patch(index, Nil, 1).point[SessionIO]
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
      _  <- ss.traverseU(writeLn)
    } yield ts

  // A command that makes the `add` action above available to the user.
  val addCommand: TodoCommand = {

    // Parser for the index option, which is a 1-based index into the list where we want the new
    // todo item to appear. We subtract one so we can deal with the zero-based index internally.
    val ind: Parser[Int] =
      intOption(
        help("List index where the todo should appear."),
        short('i'),
        long("index"),
        metavar("<index>"),
        value(1)
      ).map(_ - 1)

    // Parser for the text argument.
    val txt: Parser[String] =
      strArgument(help("Todo item text."), metavar("\"<text>\""))

    // We can now construct the final command.
    Command("add", "Add a new todo.", (ind |@| txt)(add))
      .zoom(Session.L.data[List[Todo]])

  }

  // Delete command with its argument defined inlined.
  val deleteCommand: TodoCommand =
    Command("delete", "Delete the specified item.",
      intArgument(
        help("Todo item to delete."),
        metavar("<index>")
      ).map(n => delete(n - 1)))
      .zoom(Session.L.data[List[Todo]])

  // The list and clear commands takes no arguments at all.
  val listCommand: TodoCommand =
    Command("list", "List the todo items.", list.point[Parser])
      .zoom(Session.L.data[List[Todo]])

  val clearCommand: TodoCommand =
    Command("clear", "Clears the todo list.", clear.point[Parser])
      .zoom(Session.L.data[List[Todo]])

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
  override def runc: IO[Unit] =
    Config(interact, 6666).run(simpleServer)

}
