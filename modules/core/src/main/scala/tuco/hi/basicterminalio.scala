package tuco.hi

import tuco.free._
import tuco.free.{ connection      => FC }
import tuco.free.{ basicterminalio => FBT }
import tuco.util.Zipper

import cats._
import cats.data.NonEmptyList
import cats.implicits._

object basicterminalio {

  import FC.AsyncConnectionIO
  import FBT.AsyncBasicTerminalIOIO

  type Completer = String => FBT.BasicTerminalIOIO[Either[String, List[String]]]
  object Completer {
    val empty: Completer = _ => (Right(List.empty[String]) : Either[String, List[String]]).pure[FBT.BasicTerminalIOIO]
  }

  // not a zipper because there is not necessarily a focus
  case class LineState(head: String, tail: String, history: Zipper[String], complete: Completer) {
    def insert(c: Char) = copy(head + c, tail)
    def offset    = head.length
    def done      = head + tail
    def start     = copy("", head + tail)
    def end       = copy(head + tail, "")
    def atStart   = head.isEmpty
    def atEnd     = tail.isEmpty
    def backspace = if (atStart) None else Some(copy(head.init, tail))
    def delete    = if (atEnd)   None else Some(copy(head, tail.tail))
    def left      = if (atStart) None else Some(copy(head.init, head.last + tail))
    def right     = if (atEnd)   None else Some(copy(head + tail.head, tail.tail))
    def kill      = copy(head, "")
    def up        = history.previous.map(z => copy(z.focus, "", history = z))
    def down      = history.next.map(z => copy(z.focus, "", history = z))
    def comp(c: String) = copy(head = head + c, tail = "")
  }

  private val NoHistory = Zipper.single("")

  def readLn(prompt: String, history: Zipper[String], mask: Option[Char], complete: Completer): FBT.BasicTerminalIOIO[String] = {
    import net.wimpi.telnetd.io.BasicTerminalIO.{ COLORINIT => CTRL_A, _ }

    def writeMC(c: Char): FBT.BasicTerminalIOIO[Unit] =
      mask.fold(FBT.write(c))(FBT.write)

    def writeMS(s: String): FBT.BasicTerminalIOIO[Unit] =
      mask.fold(FBT.write(s))(c => List.fill(s.length)(c).traverse(c => FBT.write(c)).void)

    val KILL = 11
    val FORWARD_DELETE = 1305
    val END = 5

    def go(s: LineState): FBT.BasicTerminalIOIO[String] =
      FBT.flush *>
      FBT.read.flatMap {

        case UP =>
          s.up.fold(go(s)) { s0 =>
            FBT.moveLeft(s.offset)      *>
            FBT.eraseToEndOfLine        *>
            writeMS(s0.head)            *> go(s0)
          }

        case DOWN =>
          s.down.fold(go(s)) { s0 =>
            FBT.moveLeft(s.offset)      *>
            FBT.eraseToEndOfLine        *>
            writeMS(s0.head)            *> go(s0)
          }

        case LEFT =>
          s.left.fold(go(s)) { s0 =>
            FBT.moveLeft(1) *> go(s0)
          }

        case RIGHT =>
          s.right.fold(go(s)) { s0 =>
            FBT.moveRight(1) *> go(s0)
          }

        case ENTER =>
          FBT.write(CRLF) *> FBT.flush *>
          s.done.pure[FBT.BasicTerminalIOIO]

        case CTRL_A =>
          FBT.moveLeft(s.offset) *>
          go(s.start)

        case END =>
          FBT.moveRight(s.tail.length) *>
          go(s.end)

        case DELETE =>
          s.backspace.fold(go(s)) { s0 =>
            FBT.moveLeft(1)      *>
            FBT.eraseToEndOfLine *>
            FBT.storeCursor      *>
            writeMS(s0.tail)     *>
            FBT.restoreCursor    *> go(s0)
          }

        case KILL =>
          FBT.eraseToEndOfLine   *> go(s.kill)

        case FORWARD_DELETE =>
          s.delete.fold(go(s))(s0 =>
            FBT.eraseToEndOfLine *>
            FBT.storeCursor      *>
            writeMS(s0.tail)     *>
            FBT.restoreCursor    *> go(s0)
          )

        case TABULATOR =>
          complete(s.head) flatMap {
            case Left(suf) => FBT.eraseToEndOfLine *> writeMS(suf) *> go(s.comp(suf))
            case Right(Nil) => go(s)
            case Right(cs)  =>
              // TODO: prompt if more than N completions
              for {
                _ <- FBT.write(CRLF)
                n <- FBT.getColumns
                _ <- columnize(cs, n).traverse(s => FBT.write(s) *> FBT.write(CRLF))
                _ <- FBT.write(prompt)
                _ <- writeMS(s.head)
                _ <- FBT.storeCursor
                _ <- writeMS(s.tail)
                _ <- FBT.restoreCursor
                a <- go(s)
              } yield a
          }

        case n =>
          // println("*** code: " + n)
          writeMC(n.toChar)   *>
          FBT.storeCursor     *>
          writeMS(s.tail)     *>
          FBT.restoreCursor   *> go(s.insert(n.toChar))
        }

    // go!
    FBT.write(prompt)        *>
    FBT.storeCursor          *>
    writeMS(history.focus)   *>
    FBT.restoreCursor        *> go(LineState("", history.focus, history, complete))

  }

  private def columnize(ss: List[String], width: Int): List[String] = {
    val cw = ss.foldRight(0)(_.length max _) + 2 // the width of each column
    val cn = (width / cw) max 1                  // number of columns
    val rs = (ss.length / cn) + 1                  // number of rows
    val cs = ss.grouped(rs).toList.map(_.map(_.padTo(cw, ' ').mkString).padTo(rs, ""))
    cs.transpose.map(_.mkString)
  }

}
