package tuco.example

import tuco.imports._
import net.bmjames.opts._
import scalaz._, Scalaz._, scalaz.effect._

class Example extends SafeShell {
  import Session.L

  val add = Command(
    "add", "Add a number to the current count.",
    intArgument(metavar("<number>"), help("Number to add.")).map { n =>
      L.data[Int].endoT[FC.ConnectionIO] { data =>
        for {
          _ <- HC.writeLn(s"${data} + $n = ${data + n}")
        } yield data + n
      }
    }
  )

  val initialState: Session[Int] =
    Session.initial(42).copy(
      commands = Builtins[Int] |+| Commands(add),
      prompt   = "tuco> "
    )

  val shellMain: FC.ConnectionIO[Unit] =
    for {
      _ <- HC.writeLn("Welcome to the Tuco demo.")
      n <- HC.readLn("Your Name? ")
      _ <- HC.writeLn(s"Hello $n. Initial count is ${initialState.data}")
      f <- CommandShell.run(initialState)
      _ <- HC.writeLn("Goodbye. Final count was " + f.data)
    } yield ()

}


object Example extends SafeApp {

  val config = Config[Example](6666)

  val telnetMain: TelnetDIO[Unit] =
    for {
      _ <- FT.start // returns immediately
      _ <- FT.delay(System.out.println("Press a key to exit..."))
      _ <- FT.delay(System.in.read)
      _ <- FT.stop  // closes all connections
      _ <- FT.delay(System.out.println("Bye."))
    } yield ()

  override def runc: IO[Unit] =
    config.run(telnetMain)

}
