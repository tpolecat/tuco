package tuco.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext

import java.lang.String
import net.wimpi.telnetd.io.BasicTerminalIO

@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
object basicterminalio { module =>

  // Algebra of operations for BasicTerminalIO. Each accepts a visitor as an alternatie to pattern-matching.
  sealed trait BasicTerminalIOOp[A] {
    def visit[F[_]](v: BasicTerminalIOOp.Visitor[F]): F[A]
  }

  // Free monad over BasicTerminalIOOp.
  type BasicTerminalIOIO[A] = FF[BasicTerminalIOOp, A]

  // Module of instances and constructors of BasicTerminalIOOp.
  object BasicTerminalIOOp {

    // Given a BasicTerminalIO we can embed a BasicTerminalIOIO program in any algebra that understands embedding.
    implicit val BasicTerminalIOOpEmbeddable: Embeddable[BasicTerminalIOOp, BasicTerminalIO] =
      new Embeddable[BasicTerminalIOOp, BasicTerminalIO] {
        def embed[A](j: BasicTerminalIO, fa: FF[BasicTerminalIOOp, A]) = Embedded.BasicTerminalIO(j, fa)
      }

    // Interface for a natural tansformation BasicTerminalIOOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (BasicTerminalIOOp ~> F) {
      final def apply[A](fa: BasicTerminalIOOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: BasicTerminalIO => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: BasicTerminalIOIO[A], f: Throwable => BasicTerminalIOIO[A]): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => BasicTerminalIOIO[Unit]): F[A]
      def bracketCase[A, B](acquire: BasicTerminalIOIO[A])(use: A => BasicTerminalIOIO[B])(release: (A, ExitCase[Throwable]) => BasicTerminalIOIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: BasicTerminalIOIO[A]): F[A]

      // BasicTerminalIO
      def bell: F[Unit]
      def close: F[Unit]
      def defineScrollRegion(a: Int, b: Int): F[Boolean]
      def eraseLine: F[Unit]
      def eraseScreen: F[Unit]
      def eraseToBeginOfLine: F[Unit]
      def eraseToBeginOfScreen: F[Unit]
      def eraseToEndOfLine: F[Unit]
      def eraseToEndOfScreen: F[Unit]
      def flush: F[Unit]
      def forceBold(a: Boolean): F[Unit]
      def getColumns: F[Int]
      def getRows: F[Int]
      def homeCursor: F[Unit]
      def isAutoflushing: F[Boolean]
      def isLineWrapping: F[Boolean]
      def isSignalling: F[Boolean]
      def moveCursor(a: Int, b: Int): F[Unit]
      def moveDown(a: Int): F[Unit]
      def moveLeft(a: Int): F[Unit]
      def moveRight(a: Int): F[Unit]
      def moveUp(a: Int): F[Unit]
      def read: F[Int]
      def resetAttributes: F[Unit]
      def resetTerminal: F[Unit]
      def restoreCursor: F[Unit]
      def setAutoflushing(a: Boolean): F[Unit]
      def setBackgroundColor(a: Int): F[Unit]
      def setBlink(a: Boolean): F[Unit]
      def setBold(a: Boolean): F[Unit]
      def setCursor(a: Int, b: Int): F[Unit]
      def setDefaultTerminal: F[Unit]
      def setForegroundColor(a: Int): F[Unit]
      def setItalic(a: Boolean): F[Unit]
      def setLinewrapping(a: Boolean): F[Unit]
      def setSignalling(a: Boolean): F[Unit]
      def setTerminal(a: String): F[Unit]
      def setUnderlined(a: Boolean): F[Unit]
      def storeCursor: F[Unit]
      def write(a: Byte): F[Unit]
      def write(a: Char): F[Unit]
      def write(a: String): F[Unit]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: BasicTerminalIO => A) extends BasicTerminalIOOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends BasicTerminalIOOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends BasicTerminalIOOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: BasicTerminalIOIO[A], f: Throwable => BasicTerminalIOIO[A]) extends BasicTerminalIOOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends BasicTerminalIOOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => BasicTerminalIOIO[Unit]) extends BasicTerminalIOOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: BasicTerminalIOIO[A], use: A => BasicTerminalIOIO[B], release: (A, ExitCase[Throwable]) => BasicTerminalIOIO[Unit]) extends BasicTerminalIOOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: BasicTerminalIOIO[A]) extends BasicTerminalIOOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }

    // BasicTerminalIO-specific operations.
    final case object Bell extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.bell
    }
    final case object Close extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.close
    }
    final case class  DefineScrollRegion(a: Int, b: Int) extends BasicTerminalIOOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.defineScrollRegion(a, b)
    }
    final case object EraseLine extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.eraseLine
    }
    final case object EraseScreen extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.eraseScreen
    }
    final case object EraseToBeginOfLine extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.eraseToBeginOfLine
    }
    final case object EraseToBeginOfScreen extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.eraseToBeginOfScreen
    }
    final case object EraseToEndOfLine extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.eraseToEndOfLine
    }
    final case object EraseToEndOfScreen extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.eraseToEndOfScreen
    }
    final case object Flush extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.flush
    }
    final case class  ForceBold(a: Boolean) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.forceBold(a)
    }
    final case object GetColumns extends BasicTerminalIOOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getColumns
    }
    final case object GetRows extends BasicTerminalIOOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getRows
    }
    final case object HomeCursor extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.homeCursor
    }
    final case object IsAutoflushing extends BasicTerminalIOOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isAutoflushing
    }
    final case object IsLineWrapping extends BasicTerminalIOOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isLineWrapping
    }
    final case object IsSignalling extends BasicTerminalIOOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isSignalling
    }
    final case class  MoveCursor(a: Int, b: Int) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.moveCursor(a, b)
    }
    final case class  MoveDown(a: Int) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.moveDown(a)
    }
    final case class  MoveLeft(a: Int) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.moveLeft(a)
    }
    final case class  MoveRight(a: Int) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.moveRight(a)
    }
    final case class  MoveUp(a: Int) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.moveUp(a)
    }
    final case object Read extends BasicTerminalIOOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.read
    }
    final case object ResetAttributes extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.resetAttributes
    }
    final case object ResetTerminal extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.resetTerminal
    }
    final case object RestoreCursor extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.restoreCursor
    }
    final case class  SetAutoflushing(a: Boolean) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setAutoflushing(a)
    }
    final case class  SetBackgroundColor(a: Int) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setBackgroundColor(a)
    }
    final case class  SetBlink(a: Boolean) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setBlink(a)
    }
    final case class  SetBold(a: Boolean) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setBold(a)
    }
    final case class  SetCursor(a: Int, b: Int) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setCursor(a, b)
    }
    final case object SetDefaultTerminal extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setDefaultTerminal
    }
    final case class  SetForegroundColor(a: Int) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setForegroundColor(a)
    }
    final case class  SetItalic(a: Boolean) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setItalic(a)
    }
    final case class  SetLinewrapping(a: Boolean) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setLinewrapping(a)
    }
    final case class  SetSignalling(a: Boolean) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setSignalling(a)
    }
    final case class  SetTerminal(a: String) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setTerminal(a)
    }
    final case class  SetUnderlined(a: Boolean) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setUnderlined(a)
    }
    final case object StoreCursor extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.storeCursor
    }
    final case class  Write(a: Byte) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.write(a)
    }
    final case class  Write1(a: Char) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.write(a)
    }
    final case class  Write2(a: String) extends BasicTerminalIOOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.write(a)
    }

  }
  import BasicTerminalIOOp._

  // Smart constructors for operations common to all algebras.
  val unit: BasicTerminalIOIO[Unit] = FF.pure[BasicTerminalIOOp, Unit](())
  def pure[A](a: A): BasicTerminalIOIO[A] = FF.pure[BasicTerminalIOOp, A](a)
  def raw[A](f: BasicTerminalIO => A): BasicTerminalIOIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[BasicTerminalIOOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): BasicTerminalIOIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: BasicTerminalIOIO[A], f: Throwable => BasicTerminalIOIO[A]): BasicTerminalIOIO[A] = FF.liftF[BasicTerminalIOOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): BasicTerminalIOIO[A] = delay(throw err)
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): BasicTerminalIOIO[A] = FF.liftF[BasicTerminalIOOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => BasicTerminalIOIO[Unit]): BasicTerminalIOIO[A] = FF.liftF[BasicTerminalIOOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: BasicTerminalIOIO[A])(use: A => BasicTerminalIOIO[B])(release: (A, ExitCase[Throwable]) => BasicTerminalIOIO[Unit]): BasicTerminalIOIO[B] = FF.liftF[BasicTerminalIOOp, B](BracketCase(acquire, use, release))
  val shift: BasicTerminalIOIO[Unit] = FF.liftF[BasicTerminalIOOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: BasicTerminalIOIO[A]) = FF.liftF[BasicTerminalIOOp, A](EvalOn(ec, fa))

  // Smart constructors for BasicTerminalIO-specific operations.
  val bell: BasicTerminalIOIO[Unit] = FF.liftF(Bell)
  val close: BasicTerminalIOIO[Unit] = FF.liftF(Close)
  def defineScrollRegion(a: Int, b: Int): BasicTerminalIOIO[Boolean] = FF.liftF(DefineScrollRegion(a, b))
  val eraseLine: BasicTerminalIOIO[Unit] = FF.liftF(EraseLine)
  val eraseScreen: BasicTerminalIOIO[Unit] = FF.liftF(EraseScreen)
  val eraseToBeginOfLine: BasicTerminalIOIO[Unit] = FF.liftF(EraseToBeginOfLine)
  val eraseToBeginOfScreen: BasicTerminalIOIO[Unit] = FF.liftF(EraseToBeginOfScreen)
  val eraseToEndOfLine: BasicTerminalIOIO[Unit] = FF.liftF(EraseToEndOfLine)
  val eraseToEndOfScreen: BasicTerminalIOIO[Unit] = FF.liftF(EraseToEndOfScreen)
  val flush: BasicTerminalIOIO[Unit] = FF.liftF(Flush)
  def forceBold(a: Boolean): BasicTerminalIOIO[Unit] = FF.liftF(ForceBold(a))
  val getColumns: BasicTerminalIOIO[Int] = FF.liftF(GetColumns)
  val getRows: BasicTerminalIOIO[Int] = FF.liftF(GetRows)
  val homeCursor: BasicTerminalIOIO[Unit] = FF.liftF(HomeCursor)
  val isAutoflushing: BasicTerminalIOIO[Boolean] = FF.liftF(IsAutoflushing)
  val isLineWrapping: BasicTerminalIOIO[Boolean] = FF.liftF(IsLineWrapping)
  val isSignalling: BasicTerminalIOIO[Boolean] = FF.liftF(IsSignalling)
  def moveCursor(a: Int, b: Int): BasicTerminalIOIO[Unit] = FF.liftF(MoveCursor(a, b))
  def moveDown(a: Int): BasicTerminalIOIO[Unit] = FF.liftF(MoveDown(a))
  def moveLeft(a: Int): BasicTerminalIOIO[Unit] = FF.liftF(MoveLeft(a))
  def moveRight(a: Int): BasicTerminalIOIO[Unit] = FF.liftF(MoveRight(a))
  def moveUp(a: Int): BasicTerminalIOIO[Unit] = FF.liftF(MoveUp(a))
  val read: BasicTerminalIOIO[Int] = FF.liftF(Read)
  val resetAttributes: BasicTerminalIOIO[Unit] = FF.liftF(ResetAttributes)
  val resetTerminal: BasicTerminalIOIO[Unit] = FF.liftF(ResetTerminal)
  val restoreCursor: BasicTerminalIOIO[Unit] = FF.liftF(RestoreCursor)
  def setAutoflushing(a: Boolean): BasicTerminalIOIO[Unit] = FF.liftF(SetAutoflushing(a))
  def setBackgroundColor(a: Int): BasicTerminalIOIO[Unit] = FF.liftF(SetBackgroundColor(a))
  def setBlink(a: Boolean): BasicTerminalIOIO[Unit] = FF.liftF(SetBlink(a))
  def setBold(a: Boolean): BasicTerminalIOIO[Unit] = FF.liftF(SetBold(a))
  def setCursor(a: Int, b: Int): BasicTerminalIOIO[Unit] = FF.liftF(SetCursor(a, b))
  val setDefaultTerminal: BasicTerminalIOIO[Unit] = FF.liftF(SetDefaultTerminal)
  def setForegroundColor(a: Int): BasicTerminalIOIO[Unit] = FF.liftF(SetForegroundColor(a))
  def setItalic(a: Boolean): BasicTerminalIOIO[Unit] = FF.liftF(SetItalic(a))
  def setLinewrapping(a: Boolean): BasicTerminalIOIO[Unit] = FF.liftF(SetLinewrapping(a))
  def setSignalling(a: Boolean): BasicTerminalIOIO[Unit] = FF.liftF(SetSignalling(a))
  def setTerminal(a: String): BasicTerminalIOIO[Unit] = FF.liftF(SetTerminal(a))
  def setUnderlined(a: Boolean): BasicTerminalIOIO[Unit] = FF.liftF(SetUnderlined(a))
  val storeCursor: BasicTerminalIOIO[Unit] = FF.liftF(StoreCursor)
  def write(a: Byte): BasicTerminalIOIO[Unit] = FF.liftF(Write(a))
  def write(a: Char): BasicTerminalIOIO[Unit] = FF.liftF(Write1(a))
  def write(a: String): BasicTerminalIOIO[Unit] = FF.liftF(Write2(a))

  // BasicTerminalIOIO is an Async
  implicit val AsyncBasicTerminalIOIO: Async[BasicTerminalIOIO] =
    new Async[BasicTerminalIOIO] {
      val asyncM = FF.catsFreeMonadForFree[BasicTerminalIOOp]
      def bracketCase[A, B](acquire: BasicTerminalIOIO[A])(use: A => BasicTerminalIOIO[B])(release: (A, ExitCase[Throwable]) => BasicTerminalIOIO[Unit]): BasicTerminalIOIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): BasicTerminalIOIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: BasicTerminalIOIO[A])(f: Throwable => BasicTerminalIOIO[A]): BasicTerminalIOIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): BasicTerminalIOIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): BasicTerminalIOIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => BasicTerminalIOIO[Unit]): BasicTerminalIOIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: BasicTerminalIOIO[A])(f: A => BasicTerminalIOIO[B]): BasicTerminalIOIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => BasicTerminalIOIO[Either[A, B]]): BasicTerminalIOIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => BasicTerminalIOIO[A]): BasicTerminalIOIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // BasicTerminalIOIO is a ContextShift
  implicit val ContextShiftBasicTerminalIOIO: ContextShift[BasicTerminalIOIO] =
    new ContextShift[BasicTerminalIOIO] {
      def shift: BasicTerminalIOIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: BasicTerminalIOIO[A]) = module.evalOn(ec)(fa)
    }
}

