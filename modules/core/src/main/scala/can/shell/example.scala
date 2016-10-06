package can.shell

import can.free._
import can.free.{ connection => FC }
import can.hi.{ connection => HC }
import can.util._

import net.bmjames.opts._

import scalaz._, Scalaz.{ some => _, _ }, scalaz.effect._

class Example extends SafeShell {

  val add = Command[Int](
    "add", "Adds a number to the current count.",
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
