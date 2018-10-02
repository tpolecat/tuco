package tuco.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext

import java.lang.String
import net.wimpi.telnetd.io.BasicTerminalIO
import net.wimpi.telnetd.net.Connection
import net.wimpi.telnetd.net.ConnectionData
import net.wimpi.telnetd.net.ConnectionEvent
import net.wimpi.telnetd.net.ConnectionListener

@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
object connection { module =>

  // Algebra of operations for Connection. Each accepts a visitor as an alternatie to pattern-matching.
  sealed trait ConnectionOp[A] {
    def visit[F[_]](v: ConnectionOp.Visitor[F]): F[A]
  }

  // Free monad over ConnectionOp.
  type ConnectionIO[A] = FF[ConnectionOp, A]

  // Module of instances and constructors of ConnectionOp.
  object ConnectionOp {

    // Given a Connection we can embed a ConnectionIO program in any algebra that understands embedding.
    implicit val ConnectionOpEmbeddable: Embeddable[ConnectionOp, Connection] =
      new Embeddable[ConnectionOp, Connection] {
        def embed[A](j: Connection, fa: FF[ConnectionOp, A]) = Embedded.Connection(j, fa)
      }

    // Interface for a natural tansformation ConnectionOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (ConnectionOp ~> F) {
      final def apply[A](fa: ConnectionOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: Connection => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: ConnectionIO[A], f: Throwable => ConnectionIO[A]): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionIO[Unit]): F[A]
      def bracketCase[A, B](acquire: ConnectionIO[A])(use: A => ConnectionIO[B])(release: (A, ExitCase[Throwable]) => ConnectionIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: ConnectionIO[A]): F[A]

      // Connection
      def addConnectionListener(a: ConnectionListener): F[Unit]
      def close: F[Unit]
      def getConnectionData: F[ConnectionData]
      def getTerminalIO: F[BasicTerminalIO]
      def isActive: F[Boolean]
      def processConnectionEvent(a: ConnectionEvent): F[Unit]
      def removeConnectionListener(a: ConnectionListener): F[Unit]
      def run: F[Unit]
      def setNextShell(a: String): F[Boolean]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: Connection => A) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: ConnectionIO[A], f: Throwable => ConnectionIO[A]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionIO[Unit]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: ConnectionIO[A], use: A => ConnectionIO[B], release: (A, ExitCase[Throwable]) => ConnectionIO[Unit]) extends ConnectionOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: ConnectionIO[A]) extends ConnectionOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }

    // Connection-specific operations.
    final case class  AddConnectionListener(a: ConnectionListener) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.addConnectionListener(a)
    }
    final case object Close extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.close
    }
    final case object GetConnectionData extends ConnectionOp[ConnectionData] {
      def visit[F[_]](v: Visitor[F]) = v.getConnectionData
    }
    final case object GetTerminalIO extends ConnectionOp[BasicTerminalIO] {
      def visit[F[_]](v: Visitor[F]) = v.getTerminalIO
    }
    final case object IsActive extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isActive
    }
    final case class  ProcessConnectionEvent(a: ConnectionEvent) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.processConnectionEvent(a)
    }
    final case class  RemoveConnectionListener(a: ConnectionListener) extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.removeConnectionListener(a)
    }
    final case object Run extends ConnectionOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.run
    }
    final case class  SetNextShell(a: String) extends ConnectionOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.setNextShell(a)
    }

  }
  import ConnectionOp._

  // Smart constructors for operations common to all algebras.
  val unit: ConnectionIO[Unit] = FF.pure[ConnectionOp, Unit](())
  def pure[A](a: A): ConnectionIO[A] = FF.pure[ConnectionOp, A](a)
  def raw[A](f: Connection => A): ConnectionIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[ConnectionOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): ConnectionIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: ConnectionIO[A], f: Throwable => ConnectionIO[A]): ConnectionIO[A] = FF.liftF[ConnectionOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): ConnectionIO[A] = delay(throw err)
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): ConnectionIO[A] = FF.liftF[ConnectionOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionIO[Unit]): ConnectionIO[A] = FF.liftF[ConnectionOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: ConnectionIO[A])(use: A => ConnectionIO[B])(release: (A, ExitCase[Throwable]) => ConnectionIO[Unit]): ConnectionIO[B] = FF.liftF[ConnectionOp, B](BracketCase(acquire, use, release))
  val shift: ConnectionIO[Unit] = FF.liftF[ConnectionOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: ConnectionIO[A]) = FF.liftF[ConnectionOp, A](EvalOn(ec, fa))

  // Smart constructors for Connection-specific operations.
  def addConnectionListener(a: ConnectionListener): ConnectionIO[Unit] = FF.liftF(AddConnectionListener(a))
  val close: ConnectionIO[Unit] = FF.liftF(Close)
  val getConnectionData: ConnectionIO[ConnectionData] = FF.liftF(GetConnectionData)
  val getTerminalIO: ConnectionIO[BasicTerminalIO] = FF.liftF(GetTerminalIO)
  val isActive: ConnectionIO[Boolean] = FF.liftF(IsActive)
  def processConnectionEvent(a: ConnectionEvent): ConnectionIO[Unit] = FF.liftF(ProcessConnectionEvent(a))
  def removeConnectionListener(a: ConnectionListener): ConnectionIO[Unit] = FF.liftF(RemoveConnectionListener(a))
  val run: ConnectionIO[Unit] = FF.liftF(Run)
  def setNextShell(a: String): ConnectionIO[Boolean] = FF.liftF(SetNextShell(a))

  // ConnectionIO is an Async
  implicit val AsyncConnectionIO: Async[ConnectionIO] =
    new Async[ConnectionIO] {
      val asyncM = FF.catsFreeMonadForFree[ConnectionOp]
      def bracketCase[A, B](acquire: ConnectionIO[A])(use: A => ConnectionIO[B])(release: (A, ExitCase[Throwable]) => ConnectionIO[Unit]): ConnectionIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): ConnectionIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: ConnectionIO[A])(f: Throwable => ConnectionIO[A]): ConnectionIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): ConnectionIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): ConnectionIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => ConnectionIO[Unit]): ConnectionIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: ConnectionIO[A])(f: A => ConnectionIO[B]): ConnectionIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => ConnectionIO[Either[A, B]]): ConnectionIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => ConnectionIO[A]): ConnectionIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // ConnectionIO is a ContextShift
  implicit val ContextShiftConnectionIO: ContextShift[ConnectionIO] =
    new ContextShift[ConnectionIO] {
      def shift: ConnectionIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: ConnectionIO[A]) = module.evalOn(ec)(fa)
    }
}

