package tuco.free
import tuco.util.Capture

import scalaz.{ Catchable, Free => F, Kleisli, Monad, ~>, \/ }

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

/**
 * Algebra and free monad for primitive operations over a `net.wimpi.telnetd.net.ConnectionListener`.
 * @group Modules
 */
object connectionlistener {

  /**
   * Sum type of primitive operations over a `net.wimpi.telnetd.net.ConnectionListener`.
   * @group Algebra
   */
  sealed trait ConnectionListenerOp[A] {
    protected def primitive[M[_]: Monad: Capture](f: ConnectionListener => A): Kleisli[M, ConnectionListener, A] =
      Kleisli((s: ConnectionListener) => Capture[M].apply(f(s)))
    def defaultTransK[M[_]: Monad: Catchable: Capture]: Kleisli[M, ConnectionListener, A]
  }

  /**
   * Module of constructors for `ConnectionListenerOp`. These are rarely useful outside of the implementation;
   * prefer the smart constructors provided by the `connectionlistener` module.
   * @group Algebra
   */
  object ConnectionListenerOp {

    // This algebra has a default interpreter
    implicit val ConnectionListenerKleisliTrans: KleisliTrans.Aux[ConnectionListenerOp, ConnectionListener] =
      new KleisliTrans[ConnectionListenerOp] {
        type J = ConnectionListener
        def interpK[M[_]: Monad: Catchable: Capture]: ConnectionListenerOp ~> Kleisli[M, ConnectionListener, ?] =
          new (ConnectionListenerOp ~> Kleisli[M, ConnectionListener, ?]) {
            def apply[A](op: ConnectionListenerOp[A]): Kleisli[M, ConnectionListener, A] =
              op.defaultTransK[M]
          }
      }

    // Lifting
    case class Lift[Op[_], A, J](j: J, action: F[Op, A], mod: KleisliTrans.Aux[Op, J]) extends ConnectionListenerOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = Kleisli(_ => mod.transK[M].apply(action).run(j))
    }

    // Combinators
    case class Attempt[A](action: ConnectionListenerIO[A]) extends ConnectionListenerOp[Throwable \/ A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] =
        Predef.implicitly[Catchable[Kleisli[M, ConnectionListener, ?]]].attempt(action.transK[M])
    }
    case class Pure[A](a: () => A) extends ConnectionListenerOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_ => a())
    }
    case class Raw[A](f: ConnectionListener => A) extends ConnectionListenerOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(f)
    }

    // Primitive Operations
    case class  ConnectionIdle(a: ConnectionEvent) extends ConnectionListenerOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.connectionIdle(a))
    }
    case class  ConnectionLogoutRequest(a: ConnectionEvent) extends ConnectionListenerOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.connectionLogoutRequest(a))
    }
    case class  ConnectionSentBreak(a: ConnectionEvent) extends ConnectionListenerOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.connectionSentBreak(a))
    }
    case class  ConnectionTimedOut(a: ConnectionEvent) extends ConnectionListenerOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.connectionTimedOut(a))
    }

  }
  import ConnectionListenerOp._ // We use these immediately

  /**
   * Free monad over a free functor of [[ConnectionListenerOp]]; abstractly, a computation that consumes
   * a `net.wimpi.telnetd.net.ConnectionListener` and produces a value of type `A`.
   * @group Algebra
   */
  type ConnectionListenerIO[A] = F[ConnectionListenerOp, A]

  /**
   * Catchable instance for [[ConnectionListenerIO]].
   * @group Typeclass Instances
   */
  implicit val CatchableConnectionListenerIO: Catchable[ConnectionListenerIO] =
    new Catchable[ConnectionListenerIO] {
      def attempt[A](f: ConnectionListenerIO[A]): ConnectionListenerIO[Throwable \/ A] = connectionlistener.attempt(f)
      def fail[A](err: Throwable): ConnectionListenerIO[A] = connectionlistener.delay(throw err)
    }

  /**
   * Capture instance for [[ConnectionListenerIO]].
   * @group Typeclass Instances
   */
  implicit val CaptureConnectionListenerIO: Capture[ConnectionListenerIO] =
    new Capture[ConnectionListenerIO] {
      def apply[A](a: => A): ConnectionListenerIO[A] = connectionlistener.delay(a)
    }

  /**
   * Lift a different type of program that has a default Kleisli interpreter.
   * @group Constructors (Lifting)
   */
  def lift[Op[_], A, J](j: J, action: F[Op, A])(implicit mod: KleisliTrans.Aux[Op, J]): ConnectionListenerIO[A] =
    F.liftF(Lift(j, action, mod))

  /**
   * Lift a ConnectionListenerIO[A] into an exception-capturing ConnectionListenerIO[Throwable \/ A].
   * @group Constructors (Lifting)
   */
  def attempt[A](a: ConnectionListenerIO[A]): ConnectionListenerIO[Throwable \/ A] =
    F.liftF[ConnectionListenerOp, Throwable \/ A](Attempt(a))

  /**
   * Non-strict unit for capturing effects.
   * @group Constructors (Lifting)
   */
  def delay[A](a: => A): ConnectionListenerIO[A] =
    F.liftF(Pure(a _))

  /**
   * Backdoor for arbitrary computations on the underlying ConnectionListener.
   * @group Constructors (Lifting)
   */
  def raw[A](f: ConnectionListener => A): ConnectionListenerIO[A] =
    F.liftF(Raw(f))

  /**
   * @group Constructors (Primitives)
   */
  def connectionIdle(a: ConnectionEvent): ConnectionListenerIO[Unit] =
    F.liftF(ConnectionIdle(a))

  /**
   * @group Constructors (Primitives)
   */
  def connectionLogoutRequest(a: ConnectionEvent): ConnectionListenerIO[Unit] =
    F.liftF(ConnectionLogoutRequest(a))

  /**
   * @group Constructors (Primitives)
   */
  def connectionSentBreak(a: ConnectionEvent): ConnectionListenerIO[Unit] =
    F.liftF(ConnectionSentBreak(a))

  /**
   * @group Constructors (Primitives)
   */
  def connectionTimedOut(a: ConnectionEvent): ConnectionListenerIO[Unit] =
    F.liftF(ConnectionTimedOut(a))

 /**
  * Natural transformation from `ConnectionListenerOp` to `Kleisli` for the given `M`, consuming a `net.wimpi.telnetd.net.ConnectionListener`.
  * @group Algebra
  */
  def interpK[M[_]: Monad: Catchable: Capture]: ConnectionListenerOp ~> Kleisli[M, ConnectionListener, ?] =
   ConnectionListenerOp.ConnectionListenerKleisliTrans.interpK

 /**
  * Natural transformation from `ConnectionListenerIO` to `Kleisli` for the given `M`, consuming a `net.wimpi.telnetd.net.ConnectionListener`.
  * @group Algebra
  */
  def transK[M[_]: Monad: Catchable: Capture]: ConnectionListenerIO ~> Kleisli[M, ConnectionListener, ?] =
   ConnectionListenerOp.ConnectionListenerKleisliTrans.transK

 /**
  * Natural transformation from `ConnectionListenerIO` to `M`, given a `net.wimpi.telnetd.net.ConnectionListener`.
  * @group Algebra
  */
 def trans[M[_]: Monad: Catchable: Capture](c: ConnectionListener): ConnectionListenerIO ~> M =
   ConnectionListenerOp.ConnectionListenerKleisliTrans.trans[M](c)

  /**
   * Syntax for `ConnectionListenerIO`.
   * @group Algebra
   */
  implicit class ConnectionListenerIOOps[A](ma: ConnectionListenerIO[A]) {
    def transK[M[_]: Monad: Catchable: Capture]: Kleisli[M, ConnectionListener, A] =
      ConnectionListenerOp.ConnectionListenerKleisliTrans.transK[M].apply(ma)
  }

}

