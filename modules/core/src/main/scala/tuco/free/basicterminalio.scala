package tuco.free
import tuco.util.Capture

import scalaz.{ Catchable, Free => F, Kleisli, Monad, ~>, \/ }

import java.lang.String
import net.wimpi.telnetd.TelnetD
import net.wimpi.telnetd.io.BasicTerminalIO
import net.wimpi.telnetd.net.Connection
import net.wimpi.telnetd.net.ConnectionData
import net.wimpi.telnetd.net.ConnectionEvent
import net.wimpi.telnetd.net.ConnectionListener

import connection.ConnectionIO
import connectiondata.ConnectionDataIO
import connectionevent.ConnectionEventIO
import connectionlistener.ConnectionListenerIO
import basicterminalio.BasicTerminalIOIO
import telnetd.TelnetDIO

/**
 * Algebra and free monad for primitive operations over a `net.wimpi.telnetd.io.BasicTerminalIO`.
 * @group Modules
 */
object basicterminalio {

  /**
   * Sum type of primitive operations over a `net.wimpi.telnetd.io.BasicTerminalIO`.
   * @group Algebra
   */
  sealed trait BasicTerminalIOOp[A] {
    protected def primitive[M[_]: Monad: Capture](f: BasicTerminalIO => A): Kleisli[M, BasicTerminalIO, A] =
      Kleisli((s: BasicTerminalIO) => Capture[M].apply(f(s)))
    def defaultTransK[M[_]: Monad: Catchable: Capture]: Kleisli[M, BasicTerminalIO, A]
  }

  /**
   * Module of constructors for `BasicTerminalIOOp`. These are rarely useful outside of the implementation;
   * prefer the smart constructors provided by the `basicterminalio` module.
   * @group Algebra
   */
  object BasicTerminalIOOp {

    // This algebra has a default interpreter
    implicit val BasicTerminalIOKleisliTrans: KleisliTrans.Aux[BasicTerminalIOOp, BasicTerminalIO] =
      new KleisliTrans[BasicTerminalIOOp] {
        type J = BasicTerminalIO
        def interpK[M[_]: Monad: Catchable: Capture]: BasicTerminalIOOp ~> Kleisli[M, BasicTerminalIO, ?] =
          new (BasicTerminalIOOp ~> Kleisli[M, BasicTerminalIO, ?]) {
            def apply[A](op: BasicTerminalIOOp[A]): Kleisli[M, BasicTerminalIO, A] =
              op.defaultTransK[M]
          }
      }

    // Lifting
    case class Lift[Op[_], A, J](j: J, action: F[Op, A], mod: KleisliTrans.Aux[Op, J]) extends BasicTerminalIOOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = Kleisli(_ => mod.transK[M].apply(action).run(j))
    }

    // Combinators
    case class Attempt[A](action: BasicTerminalIOIO[A]) extends BasicTerminalIOOp[Throwable \/ A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] =
        Predef.implicitly[Catchable[Kleisli[M, BasicTerminalIO, ?]]].attempt(action.transK[M])
    }
    case class Pure[A](a: () => A) extends BasicTerminalIOOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_ => a())
    }
    case class Raw[A](f: BasicTerminalIO => A) extends BasicTerminalIOOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(f)
    }

    // Primitive Operations
    case object Bell extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.bell())
    }
    case object Close extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.close())
    }
    case class  DefineScrollRegion(a: Int, b: Int) extends BasicTerminalIOOp[Boolean] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.defineScrollRegion(a, b))
    }
    case object EraseLine extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.eraseLine())
    }
    case object EraseScreen extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.eraseScreen())
    }
    case object EraseToBeginOfLine extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.eraseToBeginOfLine())
    }
    case object EraseToBeginOfScreen extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.eraseToBeginOfScreen())
    }
    case object EraseToEndOfLine extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.eraseToEndOfLine())
    }
    case object EraseToEndOfScreen extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.eraseToEndOfScreen())
    }
    case object Flush extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.flush())
    }
    case class  ForceBold(a: Boolean) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.forceBold(a))
    }
    case object GetColumns extends BasicTerminalIOOp[Int] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getColumns())
    }
    case object GetRows extends BasicTerminalIOOp[Int] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getRows())
    }
    case object HomeCursor extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.homeCursor())
    }
    case object IsAutoflushing extends BasicTerminalIOOp[Boolean] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.isAutoflushing())
    }
    case object IsLineWrapping extends BasicTerminalIOOp[Boolean] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.isLineWrapping())
    }
    case object IsSignalling extends BasicTerminalIOOp[Boolean] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.isSignalling())
    }
    case class  MoveCursor(a: Int, b: Int) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.moveCursor(a, b))
    }
    case class  MoveDown(a: Int) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.moveDown(a))
    }
    case class  MoveLeft(a: Int) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.moveLeft(a))
    }
    case class  MoveRight(a: Int) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.moveRight(a))
    }
    case class  MoveUp(a: Int) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.moveUp(a))
    }
    case object Read extends BasicTerminalIOOp[Int] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.read())
    }
    case object ResetAttributes extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.resetAttributes())
    }
    case object ResetTerminal extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.resetTerminal())
    }
    case object RestoreCursor extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.restoreCursor())
    }
    case class  SetAutoflushing(a: Boolean) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setAutoflushing(a))
    }
    case class  SetBackgroundColor(a: Int) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setBackgroundColor(a))
    }
    case class  SetBlink(a: Boolean) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setBlink(a))
    }
    case class  SetBold(a: Boolean) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setBold(a))
    }
    case class  SetCursor(a: Int, b: Int) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setCursor(a, b))
    }
    case object SetDefaultTerminal extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setDefaultTerminal())
    }
    case class  SetForegroundColor(a: Int) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setForegroundColor(a))
    }
    case class  SetItalic(a: Boolean) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setItalic(a))
    }
    case class  SetLinewrapping(a: Boolean) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setLinewrapping(a))
    }
    case class  SetSignalling(a: Boolean) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setSignalling(a))
    }
    case class  SetTerminal(a: String) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setTerminal(a))
    }
    case class  SetUnderlined(a: Boolean) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setUnderlined(a))
    }
    case object StoreCursor extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.storeCursor())
    }
    case class  Write(a: Byte) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.write(a))
    }
    case class  Write1(a: Char) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.write(a))
    }
    case class  Write2(a: String) extends BasicTerminalIOOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.write(a))
    }

  }
  import BasicTerminalIOOp._ // We use these immediately

  /**
   * Free monad over a free functor of [[BasicTerminalIOOp]]; abstractly, a computation that consumes
   * a `net.wimpi.telnetd.io.BasicTerminalIO` and produces a value of type `A`.
   * @group Algebra
   */
  type BasicTerminalIOIO[A] = F[BasicTerminalIOOp, A]

  /**
   * Catchable instance for [[BasicTerminalIOIO]].
   * @group Typeclass Instances
   */
  implicit val CatchableBasicTerminalIOIO: Catchable[BasicTerminalIOIO] =
    new Catchable[BasicTerminalIOIO] {
      def attempt[A](f: BasicTerminalIOIO[A]): BasicTerminalIOIO[Throwable \/ A] = basicterminalio.attempt(f)
      def fail[A](err: Throwable): BasicTerminalIOIO[A] = basicterminalio.delay(throw err)
    }

  /**
   * Capture instance for [[BasicTerminalIOIO]].
   * @group Typeclass Instances
   */
  implicit val CaptureBasicTerminalIOIO: Capture[BasicTerminalIOIO] =
    new Capture[BasicTerminalIOIO] {
      def apply[A](a: => A): BasicTerminalIOIO[A] = basicterminalio.delay(a)
    }

  /**
   * Lift a different type of program that has a default Kleisli interpreter.
   * @group Constructors (Lifting)
   */
  def lift[Op[_], A, J](j: J, action: F[Op, A])(implicit mod: KleisliTrans.Aux[Op, J]): BasicTerminalIOIO[A] =
    F.liftF(Lift(j, action, mod))

  /**
   * Lift a BasicTerminalIOIO[A] into an exception-capturing BasicTerminalIOIO[Throwable \/ A].
   * @group Constructors (Lifting)
   */
  def attempt[A](a: BasicTerminalIOIO[A]): BasicTerminalIOIO[Throwable \/ A] =
    F.liftF[BasicTerminalIOOp, Throwable \/ A](Attempt(a))

  /**
   * Non-strict unit for capturing effects.
   * @group Constructors (Lifting)
   */
  def delay[A](a: => A): BasicTerminalIOIO[A] =
    F.liftF(Pure(a _))

  /**
   * Backdoor for arbitrary computations on the underlying BasicTerminalIO.
   * @group Constructors (Lifting)
   */
  def raw[A](f: BasicTerminalIO => A): BasicTerminalIOIO[A] =
    F.liftF(Raw(f))

  /**
   * @group Constructors (Primitives)
   */
  val bell: BasicTerminalIOIO[Unit] =
    F.liftF(Bell)

  /**
   * @group Constructors (Primitives)
   */
  val close: BasicTerminalIOIO[Unit] =
    F.liftF(Close)

  /**
   * @group Constructors (Primitives)
   */
  def defineScrollRegion(a: Int, b: Int): BasicTerminalIOIO[Boolean] =
    F.liftF(DefineScrollRegion(a, b))

  /**
   * @group Constructors (Primitives)
   */
  val eraseLine: BasicTerminalIOIO[Unit] =
    F.liftF(EraseLine)

  /**
   * @group Constructors (Primitives)
   */
  val eraseScreen: BasicTerminalIOIO[Unit] =
    F.liftF(EraseScreen)

  /**
   * @group Constructors (Primitives)
   */
  val eraseToBeginOfLine: BasicTerminalIOIO[Unit] =
    F.liftF(EraseToBeginOfLine)

  /**
   * @group Constructors (Primitives)
   */
  val eraseToBeginOfScreen: BasicTerminalIOIO[Unit] =
    F.liftF(EraseToBeginOfScreen)

  /**
   * @group Constructors (Primitives)
   */
  val eraseToEndOfLine: BasicTerminalIOIO[Unit] =
    F.liftF(EraseToEndOfLine)

  /**
   * @group Constructors (Primitives)
   */
  val eraseToEndOfScreen: BasicTerminalIOIO[Unit] =
    F.liftF(EraseToEndOfScreen)

  /**
   * @group Constructors (Primitives)
   */
  val flush: BasicTerminalIOIO[Unit] =
    F.liftF(Flush)

  /**
   * @group Constructors (Primitives)
   */
  def forceBold(a: Boolean): BasicTerminalIOIO[Unit] =
    F.liftF(ForceBold(a))

  /**
   * @group Constructors (Primitives)
   */
  val getColumns: BasicTerminalIOIO[Int] =
    F.liftF(GetColumns)

  /**
   * @group Constructors (Primitives)
   */
  val getRows: BasicTerminalIOIO[Int] =
    F.liftF(GetRows)

  /**
   * @group Constructors (Primitives)
   */
  val homeCursor: BasicTerminalIOIO[Unit] =
    F.liftF(HomeCursor)

  /**
   * @group Constructors (Primitives)
   */
  val isAutoflushing: BasicTerminalIOIO[Boolean] =
    F.liftF(IsAutoflushing)

  /**
   * @group Constructors (Primitives)
   */
  val isLineWrapping: BasicTerminalIOIO[Boolean] =
    F.liftF(IsLineWrapping)

  /**
   * @group Constructors (Primitives)
   */
  val isSignalling: BasicTerminalIOIO[Boolean] =
    F.liftF(IsSignalling)

  /**
   * @group Constructors (Primitives)
   */
  def moveCursor(a: Int, b: Int): BasicTerminalIOIO[Unit] =
    F.liftF(MoveCursor(a, b))

  /**
   * @group Constructors (Primitives)
   */
  def moveDown(a: Int): BasicTerminalIOIO[Unit] =
    F.liftF(MoveDown(a))

  /**
   * @group Constructors (Primitives)
   */
  def moveLeft(a: Int): BasicTerminalIOIO[Unit] =
    F.liftF(MoveLeft(a))

  /**
   * @group Constructors (Primitives)
   */
  def moveRight(a: Int): BasicTerminalIOIO[Unit] =
    F.liftF(MoveRight(a))

  /**
   * @group Constructors (Primitives)
   */
  def moveUp(a: Int): BasicTerminalIOIO[Unit] =
    F.liftF(MoveUp(a))

  /**
   * @group Constructors (Primitives)
   */
  val read: BasicTerminalIOIO[Int] =
    F.liftF(Read)

  /**
   * @group Constructors (Primitives)
   */
  val resetAttributes: BasicTerminalIOIO[Unit] =
    F.liftF(ResetAttributes)

  /**
   * @group Constructors (Primitives)
   */
  val resetTerminal: BasicTerminalIOIO[Unit] =
    F.liftF(ResetTerminal)

  /**
   * @group Constructors (Primitives)
   */
  val restoreCursor: BasicTerminalIOIO[Unit] =
    F.liftF(RestoreCursor)

  /**
   * @group Constructors (Primitives)
   */
  def setAutoflushing(a: Boolean): BasicTerminalIOIO[Unit] =
    F.liftF(SetAutoflushing(a))

  /**
   * @group Constructors (Primitives)
   */
  def setBackgroundColor(a: Int): BasicTerminalIOIO[Unit] =
    F.liftF(SetBackgroundColor(a))

  /**
   * @group Constructors (Primitives)
   */
  def setBlink(a: Boolean): BasicTerminalIOIO[Unit] =
    F.liftF(SetBlink(a))

  /**
   * @group Constructors (Primitives)
   */
  def setBold(a: Boolean): BasicTerminalIOIO[Unit] =
    F.liftF(SetBold(a))

  /**
   * @group Constructors (Primitives)
   */
  def setCursor(a: Int, b: Int): BasicTerminalIOIO[Unit] =
    F.liftF(SetCursor(a, b))

  /**
   * @group Constructors (Primitives)
   */
  val setDefaultTerminal: BasicTerminalIOIO[Unit] =
    F.liftF(SetDefaultTerminal)

  /**
   * @group Constructors (Primitives)
   */
  def setForegroundColor(a: Int): BasicTerminalIOIO[Unit] =
    F.liftF(SetForegroundColor(a))

  /**
   * @group Constructors (Primitives)
   */
  def setItalic(a: Boolean): BasicTerminalIOIO[Unit] =
    F.liftF(SetItalic(a))

  /**
   * @group Constructors (Primitives)
   */
  def setLinewrapping(a: Boolean): BasicTerminalIOIO[Unit] =
    F.liftF(SetLinewrapping(a))

  /**
   * @group Constructors (Primitives)
   */
  def setSignalling(a: Boolean): BasicTerminalIOIO[Unit] =
    F.liftF(SetSignalling(a))

  /**
   * @group Constructors (Primitives)
   */
  def setTerminal(a: String): BasicTerminalIOIO[Unit] =
    F.liftF(SetTerminal(a))

  /**
   * @group Constructors (Primitives)
   */
  def setUnderlined(a: Boolean): BasicTerminalIOIO[Unit] =
    F.liftF(SetUnderlined(a))

  /**
   * @group Constructors (Primitives)
   */
  val storeCursor: BasicTerminalIOIO[Unit] =
    F.liftF(StoreCursor)

  /**
   * @group Constructors (Primitives)
   */
  def write(a: Byte): BasicTerminalIOIO[Unit] =
    F.liftF(Write(a))

  /**
   * @group Constructors (Primitives)
   */
  def write(a: Char): BasicTerminalIOIO[Unit] =
    F.liftF(Write1(a))

  /**
   * @group Constructors (Primitives)
   */
  def write(a: String): BasicTerminalIOIO[Unit] =
    F.liftF(Write2(a))

 /**
  * Natural transformation from `BasicTerminalIOOp` to `Kleisli` for the given `M`, consuming a `net.wimpi.telnetd.io.BasicTerminalIO`.
  * @group Algebra
  */
  def interpK[M[_]: Monad: Catchable: Capture]: BasicTerminalIOOp ~> Kleisli[M, BasicTerminalIO, ?] =
   BasicTerminalIOOp.BasicTerminalIOKleisliTrans.interpK

 /**
  * Natural transformation from `BasicTerminalIOIO` to `Kleisli` for the given `M`, consuming a `net.wimpi.telnetd.io.BasicTerminalIO`.
  * @group Algebra
  */
  def transK[M[_]: Monad: Catchable: Capture]: BasicTerminalIOIO ~> Kleisli[M, BasicTerminalIO, ?] =
   BasicTerminalIOOp.BasicTerminalIOKleisliTrans.transK

 /**
  * Natural transformation from `BasicTerminalIOIO` to `M`, given a `net.wimpi.telnetd.io.BasicTerminalIO`.
  * @group Algebra
  */
 def trans[M[_]: Monad: Catchable: Capture](c: BasicTerminalIO): BasicTerminalIOIO ~> M =
   BasicTerminalIOOp.BasicTerminalIOKleisliTrans.trans[M](c)

  /**
   * Syntax for `BasicTerminalIOIO`.
   * @group Algebra
   */
  implicit class BasicTerminalIOIOOps[A](ma: BasicTerminalIOIO[A]) {
    def transK[M[_]: Monad: Catchable: Capture]: Kleisli[M, BasicTerminalIO, A] =
      BasicTerminalIOOp.BasicTerminalIOKleisliTrans.transK[M].apply(ma)
  }

}

