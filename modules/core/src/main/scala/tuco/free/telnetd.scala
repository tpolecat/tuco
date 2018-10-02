package tuco.free

import cats.~>
import cats.effect.{ Async, ContextShift, ExitCase }
import cats.free.{ Free => FF } // alias because some algebras have an op called Free
import scala.concurrent.ExecutionContext

import java.lang.String
import java.util.Properties
import net.wimpi.telnetd.TelnetD
import net.wimpi.telnetd.net.PortListener

@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
object telnetd { module =>

  // Algebra of operations for TelnetD. Each accepts a visitor as an alternatie to pattern-matching.
  sealed trait TelnetDOp[A] {
    def visit[F[_]](v: TelnetDOp.Visitor[F]): F[A]
  }

  // Free monad over TelnetDOp.
  type TelnetDIO[A] = FF[TelnetDOp, A]

  // Module of instances and constructors of TelnetDOp.
  object TelnetDOp {

    // Given a TelnetD we can embed a TelnetDIO program in any algebra that understands embedding.
    implicit val TelnetDOpEmbeddable: Embeddable[TelnetDOp, TelnetD] =
      new Embeddable[TelnetDOp, TelnetD] {
        def embed[A](j: TelnetD, fa: FF[TelnetDOp, A]) = Embedded.TelnetD(j, fa)
      }

    // Interface for a natural tansformation TelnetDOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (TelnetDOp ~> F) {
      final def apply[A](fa: TelnetDOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: TelnetD => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: TelnetDIO[A], f: Throwable => TelnetDIO[A]): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]
      def asyncF[A](k: (Either[Throwable, A] => Unit) => TelnetDIO[Unit]): F[A]
      def bracketCase[A, B](acquire: TelnetDIO[A])(use: A => TelnetDIO[B])(release: (A, ExitCase[Throwable]) => TelnetDIO[Unit]): F[B]
      def shift: F[Unit]
      def evalOn[A](ec: ExecutionContext)(fa: TelnetDIO[A]): F[A]

      // TelnetD
      def getPortListener(a: String): F[PortListener]
      def prepareListener(a: String, b: Properties): F[Unit]
      def start: F[Unit]
      def stop: F[Unit]

    }

    // Common operations for all algebras.
    final case class Raw[A](f: TelnetD => A) extends TelnetDOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    final case class Embed[A](e: Embedded[A]) extends TelnetDOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    final case class Delay[A](a: () => A) extends TelnetDOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    final case class HandleErrorWith[A](fa: TelnetDIO[A], f: Throwable => TelnetDIO[A]) extends TelnetDOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    final case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends TelnetDOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }
    final case class AsyncF[A](k: (Either[Throwable, A] => Unit) => TelnetDIO[Unit]) extends TelnetDOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.asyncF(k)
    }
    final case class BracketCase[A, B](acquire: TelnetDIO[A], use: A => TelnetDIO[B], release: (A, ExitCase[Throwable]) => TelnetDIO[Unit]) extends TelnetDOp[B] {
      def visit[F[_]](v: Visitor[F]) = v.bracketCase(acquire)(use)(release)
    }
    final case object Shift extends TelnetDOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.shift
    }
    final case class EvalOn[A](ec: ExecutionContext, fa: TelnetDIO[A]) extends TelnetDOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.evalOn(ec)(fa)
    }

    // TelnetD-specific operations.
    final case class  GetPortListener(a: String) extends TelnetDOp[PortListener] {
      def visit[F[_]](v: Visitor[F]) = v.getPortListener(a)
    }
    final case class  PrepareListener(a: String, b: Properties) extends TelnetDOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.prepareListener(a, b)
    }
    final case object Start extends TelnetDOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.start
    }
    final case object Stop extends TelnetDOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.stop
    }

  }
  import TelnetDOp._

  // Smart constructors for operations common to all algebras.
  val unit: TelnetDIO[Unit] = FF.pure[TelnetDOp, Unit](())
  def pure[A](a: A): TelnetDIO[A] = FF.pure[TelnetDOp, A](a)
  def raw[A](f: TelnetD => A): TelnetDIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[TelnetDOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): TelnetDIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: TelnetDIO[A], f: Throwable => TelnetDIO[A]): TelnetDIO[A] = FF.liftF[TelnetDOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): TelnetDIO[A] = delay(throw err)
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): TelnetDIO[A] = FF.liftF[TelnetDOp, A](Async1(k))
  def asyncF[A](k: (Either[Throwable, A] => Unit) => TelnetDIO[Unit]): TelnetDIO[A] = FF.liftF[TelnetDOp, A](AsyncF(k))
  def bracketCase[A, B](acquire: TelnetDIO[A])(use: A => TelnetDIO[B])(release: (A, ExitCase[Throwable]) => TelnetDIO[Unit]): TelnetDIO[B] = FF.liftF[TelnetDOp, B](BracketCase(acquire, use, release))
  val shift: TelnetDIO[Unit] = FF.liftF[TelnetDOp, Unit](Shift)
  def evalOn[A](ec: ExecutionContext)(fa: TelnetDIO[A]) = FF.liftF[TelnetDOp, A](EvalOn(ec, fa))

  // Smart constructors for TelnetD-specific operations.
  def getPortListener(a: String): TelnetDIO[PortListener] = FF.liftF(GetPortListener(a))
  def prepareListener(a: String, b: Properties): TelnetDIO[Unit] = FF.liftF(PrepareListener(a, b))
  val start: TelnetDIO[Unit] = FF.liftF(Start)
  val stop: TelnetDIO[Unit] = FF.liftF(Stop)

  // TelnetDIO is an Async
  implicit val AsyncTelnetDIO: Async[TelnetDIO] =
    new Async[TelnetDIO] {
      val asyncM = FF.catsFreeMonadForFree[TelnetDOp]
      def bracketCase[A, B](acquire: TelnetDIO[A])(use: A => TelnetDIO[B])(release: (A, ExitCase[Throwable]) => TelnetDIO[Unit]): TelnetDIO[B] = module.bracketCase(acquire)(use)(release)
      def pure[A](x: A): TelnetDIO[A] = asyncM.pure(x)
      def handleErrorWith[A](fa: TelnetDIO[A])(f: Throwable => TelnetDIO[A]): TelnetDIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): TelnetDIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): TelnetDIO[A] = module.async(k)
      def asyncF[A](k: (Either[Throwable,A] => Unit) => TelnetDIO[Unit]): TelnetDIO[A] = module.asyncF(k)
      def flatMap[A, B](fa: TelnetDIO[A])(f: A => TelnetDIO[B]): TelnetDIO[B] = asyncM.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => TelnetDIO[Either[A, B]]): TelnetDIO[B] = asyncM.tailRecM(a)(f)
      def suspend[A](thunk: => TelnetDIO[A]): TelnetDIO[A] = asyncM.flatten(module.delay(thunk))
    }

  // TelnetDIO is a ContextShift
  implicit val ContextShiftTelnetDIO: ContextShift[TelnetDIO] =
    new ContextShift[TelnetDIO] {
      def shift: TelnetDIO[Unit] = module.shift
      def evalOn[A](ec: ExecutionContext)(fa: TelnetDIO[A]) = module.evalOn(ec)(fa)
    }
}

