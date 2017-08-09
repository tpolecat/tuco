package tuco.free

import cats.~>
import cats.effect.Async
import cats.free.{ Free => FF } // alias because some algebras have an op called Free

import net.wimpi.telnetd.net.ConnectionEvent
import net.wimpi.telnetd.net.ConnectionListener

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

      // ConnectionListener
      def connectionIdle(a: ConnectionEvent): F[Unit]
      def connectionLogoutRequest(a: ConnectionEvent): F[Unit]
      def connectionSentBreak(a: ConnectionEvent): F[Unit]
      def connectionTimedOut(a: ConnectionEvent): F[Unit]

    }

    // Common operations for all algebras.
    case class Raw[A](f: ConnectionListener => A) extends ConnectionListenerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    case class Embed[A](e: Embedded[A]) extends ConnectionListenerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    case class Delay[A](a: () => A) extends ConnectionListenerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    case class HandleErrorWith[A](fa: ConnectionListenerIO[A], f: Throwable => ConnectionListenerIO[A]) extends ConnectionListenerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends ConnectionListenerOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }

    // ConnectionListener-specific operations.
    case class  ConnectionIdle(a: ConnectionEvent) extends ConnectionListenerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.connectionIdle(a)
    }
    case class  ConnectionLogoutRequest(a: ConnectionEvent) extends ConnectionListenerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.connectionLogoutRequest(a)
    }
    case class  ConnectionSentBreak(a: ConnectionEvent) extends ConnectionListenerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.connectionSentBreak(a)
    }
    case class  ConnectionTimedOut(a: ConnectionEvent) extends ConnectionListenerOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.connectionTimedOut(a)
    }

  }
  import ConnectionListenerOp._

  // Smart constructors for operations common to all algebras.
  val unit: ConnectionListenerIO[Unit] = FF.pure[ConnectionListenerOp, Unit](())
  def raw[A](f: ConnectionListener => A): ConnectionListenerIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[ConnectionListenerOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): ConnectionListenerIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: ConnectionListenerIO[A], f: Throwable => ConnectionListenerIO[A]): ConnectionListenerIO[A] = FF.liftF[ConnectionListenerOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): ConnectionListenerIO[A] = delay(throw err)
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): ConnectionListenerIO[A] = FF.liftF[ConnectionListenerOp, A](Async1(k))

  // Smart constructors for ConnectionListener-specific operations.
  def connectionIdle(a: ConnectionEvent): ConnectionListenerIO[Unit] = FF.liftF(ConnectionIdle(a))
  def connectionLogoutRequest(a: ConnectionEvent): ConnectionListenerIO[Unit] = FF.liftF(ConnectionLogoutRequest(a))
  def connectionSentBreak(a: ConnectionEvent): ConnectionListenerIO[Unit] = FF.liftF(ConnectionSentBreak(a))
  def connectionTimedOut(a: ConnectionEvent): ConnectionListenerIO[Unit] = FF.liftF(ConnectionTimedOut(a))

  // ConnectionListenerIO is an Async
  implicit val AsyncConnectionListenerIO: Async[ConnectionListenerIO] =
    new Async[ConnectionListenerIO] {
      val M = FF.catsFreeMonadForFree[ConnectionListenerOp]
      def pure[A](x: A): ConnectionListenerIO[A] = M.pure(x)
      def handleErrorWith[A](fa: ConnectionListenerIO[A])(f: Throwable => ConnectionListenerIO[A]): ConnectionListenerIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): ConnectionListenerIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): ConnectionListenerIO[A] = module.async(k)
      def flatMap[A, B](fa: ConnectionListenerIO[A])(f: A => ConnectionListenerIO[B]): ConnectionListenerIO[B] = M.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => ConnectionListenerIO[Either[A, B]]): ConnectionListenerIO[B] = M.tailRecM(a)(f)
      def suspend[A](thunk: => ConnectionListenerIO[A]): ConnectionListenerIO[A] = M.flatten(module.delay(thunk))
    }

}

