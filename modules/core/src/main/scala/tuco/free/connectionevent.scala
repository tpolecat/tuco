package tuco.free

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
 * Algebra and free monad for primitive operations over a `net.wimpi.telnetd.net.ConnectionEvent`.
 * @group Modules
 */
object connectionevent {

  /**
   * Sum type of primitive operations over a `net.wimpi.telnetd.net.ConnectionEvent`.
   * @group Algebra
   */
  sealed trait ConnectionEventOp[A] {
    protected def primitive[M[_]: Monad: Capture](f: ConnectionEvent => A): Kleisli[M, ConnectionEvent, A] =
      Kleisli((s: ConnectionEvent) => Capture[M].apply(f(s)))
    def defaultTransK[M[_]: Monad: Catchable: Capture]: Kleisli[M, ConnectionEvent, A]
  }

  /**
   * Module of constructors for `ConnectionEventOp`. These are rarely useful outside of the implementation;
   * prefer the smart constructors provided by the `connectionevent` module.
   * @group Algebra
   */
  object ConnectionEventOp {

    // This algebra has a default interpreter
    implicit val ConnectionEventKleisliTrans: KleisliTrans.Aux[ConnectionEventOp, ConnectionEvent] =
      new KleisliTrans[ConnectionEventOp] {
        type J = ConnectionEvent
        def interpK[M[_]: Monad: Catchable: Capture]: ConnectionEventOp ~> Kleisli[M, ConnectionEvent, ?] =
          new (ConnectionEventOp ~> Kleisli[M, ConnectionEvent, ?]) {
            def apply[A](op: ConnectionEventOp[A]): Kleisli[M, ConnectionEvent, A] =
              op.defaultTransK[M]
          }
      }

    // Lifting
    case class Lift[Op[_], A, J](j: J, action: F[Op, A], mod: KleisliTrans.Aux[Op, J]) extends ConnectionEventOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = Kleisli(_ => mod.transK[M].apply(action).run(j))
    }

    // Combinators
    case class Attempt[A](action: ConnectionEventIO[A]) extends ConnectionEventOp[Throwable \/ A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] =
        Predef.implicitly[Catchable[Kleisli[M, ConnectionEvent, ?]]].attempt(action.transK[M])
    }
    case class Pure[A](a: () => A) extends ConnectionEventOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_ => a())
    }
    case class Raw[A](f: ConnectionEvent => A) extends ConnectionEventOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(f)
    }

    // Primitive Operations
    case object GetConnection extends ConnectionEventOp[Connection] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getConnection())
    }
    case object GetSource extends ConnectionEventOp[Connection] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getSource())
    }
    case class  IsType(a: Int) extends ConnectionEventOp[Boolean] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.isType(a))
    }

  }
  import ConnectionEventOp._ // We use these immediately

  /**
   * Free monad over a free functor of [[ConnectionEventOp]]; abstractly, a computation that consumes
   * a `net.wimpi.telnetd.net.ConnectionEvent` and produces a value of type `A`.
   * @group Algebra
   */
  type ConnectionEventIO[A] = F[ConnectionEventOp, A]

  /**
   * Catchable instance for [[ConnectionEventIO]].
   * @group Typeclass Instances
   */
  implicit val CatchableConnectionEventIO: Catchable[ConnectionEventIO] =
    new Catchable[ConnectionEventIO] {
      def attempt[A](f: ConnectionEventIO[A]): ConnectionEventIO[Throwable \/ A] = connectionevent.attempt(f)
      def fail[A](err: Throwable): ConnectionEventIO[A] = connectionevent.delay(throw err)
    }

  /**
   * Capture instance for [[ConnectionEventIO]].
   * @group Typeclass Instances
   */
  implicit val CaptureConnectionEventIO: Capture[ConnectionEventIO] =
    new Capture[ConnectionEventIO] {
      def apply[A](a: => A): ConnectionEventIO[A] = connectionevent.delay(a)
    }

  /**
   * Lift a different type of program that has a default Kleisli interpreter.
   * @group Constructors (Lifting)
   */
  def lift[Op[_], A, J](j: J, action: F[Op, A])(implicit mod: KleisliTrans.Aux[Op, J]): ConnectionEventIO[A] =
    F.liftF(Lift(j, action, mod))

  /**
   * Lift a ConnectionEventIO[A] into an exception-capturing ConnectionEventIO[Throwable \/ A].
   * @group Constructors (Lifting)
   */
  def attempt[A](a: ConnectionEventIO[A]): ConnectionEventIO[Throwable \/ A] =
    F.liftF[ConnectionEventOp, Throwable \/ A](Attempt(a))

  /**
   * Non-strict unit for capturing effects.
   * @group Constructors (Lifting)
   */
  def delay[A](a: => A): ConnectionEventIO[A] =
    F.liftF(Pure(a _))

  /**
   * Backdoor for arbitrary computations on the underlying ConnectionEvent.
   * @group Constructors (Lifting)
   */
  def raw[A](f: ConnectionEvent => A): ConnectionEventIO[A] =
    F.liftF(Raw(f))

  /**
   * @group Constructors (Primitives)
   */
  val getConnection: ConnectionEventIO[Connection] =
    F.liftF(GetConnection)

  /**
   * @group Constructors (Primitives)
   */
  val getSource: ConnectionEventIO[Connection] =
    F.liftF(GetSource)

  /**
   * @group Constructors (Primitives)
   */
  def isType(a: Int): ConnectionEventIO[Boolean] =
    F.liftF(IsType(a))

 /**
  * Natural transformation from `ConnectionEventOp` to `Kleisli` for the given `M`, consuming a `net.wimpi.telnetd.net.ConnectionEvent`.
  * @group Algebra
  */
  def interpK[M[_]: Monad: Catchable: Capture]: ConnectionEventOp ~> Kleisli[M, ConnectionEvent, ?] =
   ConnectionEventOp.ConnectionEventKleisliTrans.interpK

 /**
  * Natural transformation from `ConnectionEventIO` to `Kleisli` for the given `M`, consuming a `net.wimpi.telnetd.net.ConnectionEvent`.
  * @group Algebra
  */
  def transK[M[_]: Monad: Catchable: Capture]: ConnectionEventIO ~> Kleisli[M, ConnectionEvent, ?] =
   ConnectionEventOp.ConnectionEventKleisliTrans.transK

 /**
  * Natural transformation from `ConnectionEventIO` to `M`, given a `net.wimpi.telnetd.net.ConnectionEvent`.
  * @group Algebra
  */
 def trans[M[_]: Monad: Catchable: Capture](c: ConnectionEvent): ConnectionEventIO ~> M =
   ConnectionEventOp.ConnectionEventKleisliTrans.trans[M](c)

  /**
   * Syntax for `ConnectionEventIO`.
   * @group Algebra
   */
  implicit class ConnectionEventIOOps[A](ma: ConnectionEventIO[A]) {
    def transK[M[_]: Monad: Catchable: Capture]: Kleisli[M, ConnectionEvent, A] =
      ConnectionEventOp.ConnectionEventKleisliTrans.transK[M].apply(ma)
  }

}

