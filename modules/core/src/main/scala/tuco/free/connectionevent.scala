package tuco.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext

import net.wimpi.telnetd.net.Connection
import net.wimpi.telnetd.net.ConnectionEvent

@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
object connectionevent { module =>

  // Algebra of operations for ConnectionEvent. Each accepts a visitor as an alternatie to pattern-matching.
  sealed trait ConnectionEventOp[A] {
    def visit[F[_]](v: ConnectionEventOp.Visitor[F]): F[A]
  }

  // Free monad over ConnectionEventOp.
  type ConnectionEventIO[A] = FF[ConnectionEventOp, A]

  // Module of instances and constructors of ConnectionEventOp.
  object ConnectionEventOp {

    // Given a ConnectionEvent we can embed a ConnectionEventIO program in any algebra that understands embedding.
    implicit val ConnectionEventOpEmbeddable: Embeddable[ConnectionEventOp, ConnectionEvent] =
      new Embeddable[ConnectionEventOp, ConnectionEvent] {
        def embed[A](j: ConnectionEvent, fa: FF[ConnectionEventOp, A]) = Embedded.ConnectionEvent(j, fa)
      }

    // Interface for a natural tansformation ConnectionEventOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (ConnectionEventOp ~> F) {
      final def apply[A](fa: ConnectionEventOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: ConnectionEvent => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: ConnectionEventIO[A], f: Throwable => ConnectionEventIO[A]): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionEventIO[Unit]): F[A]
      def bracketCase[A, B](acquire: ConnectionEventIO[A])(use: A => ConnectionEventIO[B])(release: (A, ExitCase[Throwable]) => ConnectionEventIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: ConnectionEventIO[A]): F[A]

      // ConnectionEvent
      def getConnection: F[Connection]
      def getSource: F[Connection]
      def isType(a: Int): F[Boolean]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: ConnectionEvent => A) extends ConnectionEventOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends ConnectionEventOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends ConnectionEventOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: ConnectionEventIO[A], f: Throwable => ConnectionEventIO[A]) extends ConnectionEventOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends ConnectionEventOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionEventIO[Unit]) extends ConnectionEventOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: ConnectionEventIO[A], use: A => ConnectionEventIO[B], release: (A, ExitCase[Throwable]) => ConnectionEventIO[Unit]) extends ConnectionEventOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends ConnectionEventOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: ConnectionEventIO[A]) extends ConnectionEventOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }

    // ConnectionEvent-specific operations.
    final case object GetConnection extends ConnectionEventOp[Connection] {
      def visit[F[_]](v: Visitor[F]) = v.getConnection
    }
    final case object GetSource extends ConnectionEventOp[Connection] {
      def visit[F[_]](v: Visitor[F]) = v.getSource
    }
    final case class  IsType(a: Int) extends ConnectionEventOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isType(a)
    }

  }
  import ConnectionEventOp._

  // Smart constructors for operations common to all algebras.
  val unit: ConnectionEventIO[Unit] = FF.pure[ConnectionEventOp, Unit](())
  def pure[A](a: A): ConnectionEventIO[A] = FF.pure[ConnectionEventOp, A](a)
  def raw[A](f: ConnectionEvent => A): ConnectionEventIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[ConnectionEventOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): ConnectionEventIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: ConnectionEventIO[A], f: Throwable => ConnectionEventIO[A]): ConnectionEventIO[A] = FF.liftF[ConnectionEventOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): ConnectionEventIO[A] = delay(throw err)
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): ConnectionEventIO[A] = FF.liftF[ConnectionEventOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionEventIO[Unit]): ConnectionEventIO[A] = FF.liftF[ConnectionEventOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: ConnectionEventIO[A])(use: A => ConnectionEventIO[B])(release: (A, ExitCase[Throwable]) => ConnectionEventIO[Unit]): ConnectionEventIO[B] = FF.liftF[ConnectionEventOp, B](BracketCase(acquire, use, release))
  val shift: ConnectionEventIO[Unit] = FF.liftF[ConnectionEventOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: ConnectionEventIO[A]) = FF.liftF[ConnectionEventOp, A](EvalOn(ec, fa))

  // Smart constructors for ConnectionEvent-specific operations.
  val getConnection: ConnectionEventIO[Connection] = FF.liftF(GetConnection)
  val getSource: ConnectionEventIO[Connection] = FF.liftF(GetSource)
  def isType(a: Int): ConnectionEventIO[Boolean] = FF.liftF(IsType(a))

  // ConnectionEventIO is an Async
  implicit val AsyncConnectionEventIO: Async[ConnectionEventIO] =
    new Async[ConnectionEventIO] {
      val asyncM = FF.catsFreeMonadForFree[ConnectionEventOp]
      def bracketCase[A, B](acquire: ConnectionEventIO[A])(use: A => ConnectionEventIO[B])(release: (A, ExitCase[Throwable]) => ConnectionEventIO[Unit]): ConnectionEventIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): ConnectionEventIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: ConnectionEventIO[A])(f: Throwable => ConnectionEventIO[A]): ConnectionEventIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): ConnectionEventIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): ConnectionEventIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => ConnectionEventIO[Unit]): ConnectionEventIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: ConnectionEventIO[A])(f: A => ConnectionEventIO[B]): ConnectionEventIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => ConnectionEventIO[Either[A, B]]): ConnectionEventIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => ConnectionEventIO[A]): ConnectionEventIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // ConnectionEventIO is a ContextShift
  implicit val ContextShiftConnectionEventIO: ContextShift[ConnectionEventIO] =
    new ContextShift[ConnectionEventIO] {
      def shift: ConnectionEventIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: ConnectionEventIO[A]) = module.evalOn(ec)(fa)
    }
}

