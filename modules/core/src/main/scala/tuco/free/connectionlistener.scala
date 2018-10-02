package tuco.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext

import net.wimpi.telnetd.net.ConnectionEvent
import net.wimpi.telnetd.net.ConnectionListener

@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
object connectionlistener { module =>

  // Algebra of operations for ConnectionListener. Each accepts a visitor as an alternatie to pattern-matching.
  sealed trait ConnectionListenerOp[A] {
    def visit[F[_]](v: ConnectionListenerOp.Visitor[F]): F[A]
  }

  // Free monad over ConnectionListenerOp.
  type ConnectionListenerIO[A] = FF[ConnectionListenerOp, A]

  // Module of instances and constructors of ConnectionListenerOp.
  object ConnectionListenerOp {

    // Given a ConnectionListener we can embed a ConnectionListenerIO program in any algebra that understands embedding.
    implicit val ConnectionListenerOpEmbeddable: Embeddable[ConnectionListenerOp, ConnectionListener] =
      new Embeddable[ConnectionListenerOp, ConnectionListener] {
        def embed[A](j: ConnectionListener, fa: FF[ConnectionListenerOp, A]) = Embedded.ConnectionListener(j, fa)
      }

    // Interface for a natural tansformation ConnectionListenerOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (ConnectionListenerOp ~> F) {
      final def apply[A](fa: ConnectionListenerOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: ConnectionListener => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: ConnectionListenerIO[A], f: Throwable => ConnectionListenerIO[A]): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionListenerIO[Unit]): F[A]
      def bracketCase[A, B](acquire: ConnectionListenerIO[A])(use: A => ConnectionListenerIO[B])(release: (A, ExitCase[Throwable]) => ConnectionListenerIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: ConnectionListenerIO[A]): F[A]

      // ConnectionListener
      def connectionIdle(a: ConnectionEvent): F[Unit]
      def connectionLogoutRequest(a: ConnectionEvent): F[Unit]
      def connectionSentBreak(a: ConnectionEvent): F[Unit]
      def connectionTimedOut(a: ConnectionEvent): F[Unit]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: ConnectionListener => A) extends ConnectionListenerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends ConnectionListenerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends ConnectionListenerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: ConnectionListenerIO[A], f: Throwable => ConnectionListenerIO[A]) extends ConnectionListenerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends ConnectionListenerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionListenerIO[Unit]) extends ConnectionListenerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: ConnectionListenerIO[A], use: A => ConnectionListenerIO[B], release: (A, ExitCase[Throwable]) => ConnectionListenerIO[Unit]) extends ConnectionListenerOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends ConnectionListenerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: ConnectionListenerIO[A]) extends ConnectionListenerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }

    // ConnectionListener-specific operations.
    final case class  ConnectionIdle(a: ConnectionEvent) extends ConnectionListenerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.connectionIdle(a)
    }
    final case class  ConnectionLogoutRequest(a: ConnectionEvent) extends ConnectionListenerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.connectionLogoutRequest(a)
    }
    final case class  ConnectionSentBreak(a: ConnectionEvent) extends ConnectionListenerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.connectionSentBreak(a)
    }
    final case class  ConnectionTimedOut(a: ConnectionEvent) extends ConnectionListenerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.connectionTimedOut(a)
    }

  }
  import ConnectionListenerOp._

  // Smart constructors for operations common to all algebras.
  val unit: ConnectionListenerIO[Unit] = FF.pure[ConnectionListenerOp, Unit](())
  def pure[A](a: A): ConnectionListenerIO[A] = FF.pure[ConnectionListenerOp, A](a)
  def raw[A](f: ConnectionListener => A): ConnectionListenerIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[ConnectionListenerOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): ConnectionListenerIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: ConnectionListenerIO[A], f: Throwable => ConnectionListenerIO[A]): ConnectionListenerIO[A] = FF.liftF[ConnectionListenerOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): ConnectionListenerIO[A] = delay(throw err)
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): ConnectionListenerIO[A] = FF.liftF[ConnectionListenerOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionListenerIO[Unit]): ConnectionListenerIO[A] = FF.liftF[ConnectionListenerOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: ConnectionListenerIO[A])(use: A => ConnectionListenerIO[B])(release: (A, ExitCase[Throwable]) => ConnectionListenerIO[Unit]): ConnectionListenerIO[B] = FF.liftF[ConnectionListenerOp, B](BracketCase(acquire, use, release))
  val shift: ConnectionListenerIO[Unit] = FF.liftF[ConnectionListenerOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: ConnectionListenerIO[A]) = FF.liftF[ConnectionListenerOp, A](EvalOn(ec, fa))

  // Smart constructors for ConnectionListener-specific operations.
  def connectionIdle(a: ConnectionEvent): ConnectionListenerIO[Unit] = FF.liftF(ConnectionIdle(a))
  def connectionLogoutRequest(a: ConnectionEvent): ConnectionListenerIO[Unit] = FF.liftF(ConnectionLogoutRequest(a))
  def connectionSentBreak(a: ConnectionEvent): ConnectionListenerIO[Unit] = FF.liftF(ConnectionSentBreak(a))
  def connectionTimedOut(a: ConnectionEvent): ConnectionListenerIO[Unit] = FF.liftF(ConnectionTimedOut(a))

  // ConnectionListenerIO is an Async
  implicit val AsyncConnectionListenerIO: Async[ConnectionListenerIO] =
    new Async[ConnectionListenerIO] {
      val asyncM = FF.catsFreeMonadForFree[ConnectionListenerOp]
      def bracketCase[A, B](acquire: ConnectionListenerIO[A])(use: A => ConnectionListenerIO[B])(release: (A, ExitCase[Throwable]) => ConnectionListenerIO[Unit]): ConnectionListenerIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): ConnectionListenerIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: ConnectionListenerIO[A])(f: Throwable => ConnectionListenerIO[A]): ConnectionListenerIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): ConnectionListenerIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): ConnectionListenerIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => ConnectionListenerIO[Unit]): ConnectionListenerIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: ConnectionListenerIO[A])(f: A => ConnectionListenerIO[B]): ConnectionListenerIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => ConnectionListenerIO[Either[A, B]]): ConnectionListenerIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => ConnectionListenerIO[A]): ConnectionListenerIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // ConnectionListenerIO is a ContextShift
  implicit val ContextShiftConnectionListenerIO: ContextShift[ConnectionListenerIO] =
    new ContextShift[ConnectionListenerIO] {
      def shift: ConnectionListenerIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: ConnectionListenerIO[A]) = module.evalOn(ec)(fa)
    }
}

