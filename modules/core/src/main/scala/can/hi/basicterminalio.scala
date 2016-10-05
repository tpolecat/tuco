package can.hi
import can.free._
import can.free.{ connection      => FC }
import can.free.{ basicterminalio => FBT }

import scalaz._, Scalaz._, scalaz.effect._

object basicterminalio {

  // not a zipper because there is not necessarily a focus
  case class LineState(head: String, tail: String, history: Zipper[String]) {
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
  }

  def readLn(prompt: String, history: Zipper[String]): FBT.BasicTerminalIOIO[String] = {
    import net.wimpi.telnetd.io.BasicTerminalIO.{ COLORINIT => CTRL_A, _ }

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
            FBT.write(s0.head)          *> go(s0)
          }

        case DOWN =>
          s.down.fold(go(s)) { s0 =>
            FBT.moveLeft(s.offset)      *>
            FBT.eraseToEndOfLine        *>
            FBT.write(s0.head)          *> go(s0)
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
            FBT.write(s0.tail)   *>
            FBT.restoreCursor    *> go(s0)
          }

        case KILL =>
          FBT.eraseToEndOfLine   *> go(s.kill)

        case FORWARD_DELETE =>
          s.delete.fold(go(s))(s0 =>
            FBT.eraseToEndOfLine *>
            FBT.storeCursor      *>
            FBT.write(s0.tail)   *>
            FBT.restoreCursor    *> go(s0)
          )

        case n =>
          println("*** code: " + n)
          FBT.write(n.toChar) *>
          FBT.storeCursor     *>
          FBT.write(s.tail)   *>
          FBT.restoreCursor   *> go(s.insert(n.toChar))
        }

    // go!
    FBT.write(prompt + history.focus) *> go(LineState(history.focus, "", history))

  }

}
