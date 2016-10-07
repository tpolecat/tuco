package tuco.example

import tuco.imports._
import net.bmjames.opts._
import scalaz._, Scalaz._

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

object Example {
  def main(args: Array[String]): Unit =
    net.wimpi.telnetd.TelnetD.main(Array("file:config.properties"))
}
