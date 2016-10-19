package tuco.hi
import tuco.free._
import tuco.free.{ connection      => FC }
import tuco.free.{ basicterminalio => FBT }

import scalaz._, Scalaz._, scalaz.effect._

object basicterminalio {

  // not a zipper because there is not necessarily a focus
  case class LineState(head: String, tail: String, history: Zipper[String], completions: List[String]) {
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
    def complete(c: String) = copy(head = c, tail = "")
  }

  private val NoHistory = NonEmptyList("").toZipper

  def readLn(prompt: String, history: Zipper[String], mask: Option[Char], completions: List[String]): FBT.BasicTerminalIOIO[String] = {
    import net.wimpi.telnetd.io.BasicTerminalIO.{ COLORINIT => CTRL_A, _ }

    def writeMC(c: Char): FBT.BasicTerminalIOIO[Unit] =
      mask.fold(FBT.write(c))(FBT.write)

    def writeMS(s: String): FBT.BasicTerminalIOIO[Unit] =
      mask.fold(FBT.write(s))(FBT.write(_).replicateM_(s.length))

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
          s.done.point[FBT.BasicTerminalIOIO]

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
          completions.filter(_.startsWith(s.head)) match {
            case Nil      => go(s)
            case c :: Nil =>
              FBT.eraseToEndOfLine           *>
              writeMS(c.drop(s.head.length)) *> go(s.complete(c))
            case cs       => // TODO: prompt if more than N completions
              FBT.write(CRLF)             *>
              FBT.write(cs.mkString(" ")) *> // TODO: this, better
              FBT.write(CRLF)             *>
              FBT.write(prompt)           *>
              writeMS(s.head)             *>
              FBT.storeCursor             *>
              writeMS(s.tail)             *>
              FBT.restoreCursor           *> go(s)
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
    FBT.restoreCursor        *> go(LineState("", history.focus, history, completions))

  }

}
