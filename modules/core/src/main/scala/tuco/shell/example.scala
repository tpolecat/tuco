package tuco.shell

import tuco.free._
import tuco.free.{ connection => FC }
import tuco.hi.{ connection => HC }
import tuco.util._

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
      prompt   = "tuco>"
    )

  val shellMain: FC.ConnectionIO[Unit] =
    HC.writeLn("Herro.")   *>
    CommandShell.run(initialState) *>
    HC.writeLn("Bye!")

}

object Main {
  def main(args: Array[String]): Unit =
    net.wimpi.telnetd.TelnetD.main(Array("file:config.properties"))
}
