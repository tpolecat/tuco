package tuco.free

import scalaz.{ Catchable, Free => F, Kleisli, Monad, ~>, \/ }

import java.lang.String
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
 * Algebra and free monad for primitive operations over a `net.wimpi.telnetd.net.Connection`.
 * @group Modules
 */
object connection {

  /**
   * Sum type of primitive operations over a `net.wimpi.telnetd.net.Connection`.
   * @group Algebra
   */
  sealed trait ConnectionOp[A] {
    protected def primitive[M[_]: Monad: Capture](f: Connection => A): Kleisli[M, Connection, A] =
      Kleisli((s: Connection) => Capture[M].apply(f(s)))
    def defaultTransK[M[_]: Monad: Catchable: Capture]: Kleisli[M, Connection, A]
  }

  /**
   * Module of constructors for `ConnectionOp`. These are rarely useful outside of the implementation;
   * prefer the smart constructors provided by the `connection` module.
   * @group Algebra
   */
  object ConnectionOp {

    // This algebra has a default interpreter
    implicit val ConnectionKleisliTrans: KleisliTrans.Aux[ConnectionOp, Connection] =
      new KleisliTrans[ConnectionOp] {
        type J = Connection
        def interpK[M[_]: Monad: Catchable: Capture]: ConnectionOp ~> Kleisli[M, Connection, ?] =
          new (ConnectionOp ~> Kleisli[M, Connection, ?]) {
            def apply[A](op: ConnectionOp[A]): Kleisli[M, Connection, A] =
              op.defaultTransK[M]
          }
      }

    // Lifting
    case class Lift[Op[_], A, J](j: J, action: F[Op, A], mod: KleisliTrans.Aux[Op, J]) extends ConnectionOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = Kleisli(_ => mod.transK[M].apply(action).run(j))
    }

    // Combinators
    case class Attempt[A](action: ConnectionIO[A]) extends ConnectionOp[Throwable \/ A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] =
        Predef.implicitly[Catchable[Kleisli[M, Connection, ?]]].attempt(action.transK[M])
    }
    case class Pure[A](a: () => A) extends ConnectionOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_ => a())
    }
    case class Raw[A](f: Connection => A) extends ConnectionOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(f)
    }

    // Primitive Operations
    case class  AddConnectionListener(a: ConnectionListener) extends ConnectionOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.addConnectionListener(a))
    }
    case object Close extends ConnectionOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.close())
    }
    case object GetConnectionData extends ConnectionOp[ConnectionData] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getConnectionData())
    }
    case object GetTerminalIO extends ConnectionOp[BasicTerminalIO] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getTerminalIO())
    }
    case object IsActive extends ConnectionOp[Boolean] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.isActive())
    }
    case class  ProcessConnectionEvent(a: ConnectionEvent) extends ConnectionOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.processConnectionEvent(a))
    }
    case class  RemoveConnectionListener(a: ConnectionListener) extends ConnectionOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.removeConnectionListener(a))
    }
    case object Run extends ConnectionOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.run())
    }
    case class  SetNextShell(a: String) extends ConnectionOp[Boolean] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setNextShell(a))
    }

  }
  import ConnectionOp._ // We use these immediately

  /**
   * Free monad over a free functor of [[ConnectionOp]]; abstractly, a computation that consumes
   * a `net.wimpi.telnetd.net.Connection` and produces a value of type `A`.
   * @group Algebra
   */
  type ConnectionIO[A] = F[ConnectionOp, A]

  /**
   * Catchable instance for [[ConnectionIO]].
   * @group Typeclass Instances
   */
  implicit val CatchableConnectionIO: Catchable[ConnectionIO] =
    new Catchable[ConnectionIO] {
      def attempt[A](f: ConnectionIO[A]): ConnectionIO[Throwable \/ A] = connection.attempt(f)
      def fail[A](err: Throwable): ConnectionIO[A] = connection.delay(throw err)
    }

  /**
   * Capture instance for [[ConnectionIO]].
   * @group Typeclass Instances
   */
  implicit val CaptureConnectionIO: Capture[ConnectionIO] =
    new Capture[ConnectionIO] {
      def apply[A](a: => A): ConnectionIO[A] = connection.delay(a)
    }

  /**
   * Lift a different type of program that has a default Kleisli interpreter.
   * @group Constructors (Lifting)
   */
  def lift[Op[_], A, J](j: J, action: F[Op, A])(implicit mod: KleisliTrans.Aux[Op, J]): ConnectionIO[A] =
    F.liftF(Lift(j, action, mod))

  /**
   * Lift a ConnectionIO[A] into an exception-capturing ConnectionIO[Throwable \/ A].
   * @group Constructors (Lifting)
   */
  def attempt[A](a: ConnectionIO[A]): ConnectionIO[Throwable \/ A] =
    F.liftF[ConnectionOp, Throwable \/ A](Attempt(a))

  /**
   * Non-strict unit for capturing effects.
   * @group Constructors (Lifting)
   */
  def delay[A](a: => A): ConnectionIO[A] =
    F.liftF(Pure(a _))

  /**
   * Backdoor for arbitrary computations on the underlying Connection.
   * @group Constructors (Lifting)
   */
  def raw[A](f: Connection => A): ConnectionIO[A] =
    F.liftF(Raw(f))

  /**
   * @group Constructors (Primitives)
   */
  def addConnectionListener(a: ConnectionListener): ConnectionIO[Unit] =
    F.liftF(AddConnectionListener(a))

  /**
   * @group Constructors (Primitives)
   */
  val close: ConnectionIO[Unit] =
    F.liftF(Close)

  /**
   * @group Constructors (Primitives)
   */
  val getConnectionData: ConnectionIO[ConnectionData] =
    F.liftF(GetConnectionData)

  /**
   * @group Constructors (Primitives)
   */
  val getTerminalIO: ConnectionIO[BasicTerminalIO] =
    F.liftF(GetTerminalIO)

  /**
   * @group Constructors (Primitives)
   */
  val isActive: ConnectionIO[Boolean] =
    F.liftF(IsActive)

  /**
   * @group Constructors (Primitives)
   */
  def processConnectionEvent(a: ConnectionEvent): ConnectionIO[Unit] =
    F.liftF(ProcessConnectionEvent(a))

  /**
   * @group Constructors (Primitives)
   */
  def removeConnectionListener(a: ConnectionListener): ConnectionIO[Unit] =
    F.liftF(RemoveConnectionListener(a))

  /**
   * @group Constructors (Primitives)
   */
  val run: ConnectionIO[Unit] =
    F.liftF(Run)

  /**
   * @group Constructors (Primitives)
   */
  def setNextShell(a: String): ConnectionIO[Boolean] =
    F.liftF(SetNextShell(a))

 /**
  * Natural transformation from `ConnectionOp` to `Kleisli` for the given `M`, consuming a `net.wimpi.telnetd.net.Connection`.
  * @group Algebra
  */
  def interpK[M[_]: Monad: Catchable: Capture]: ConnectionOp ~> Kleisli[M, Connection, ?] =
   ConnectionOp.ConnectionKleisliTrans.interpK

 /**
  * Natural transformation from `ConnectionIO` to `Kleisli` for the given `M`, consuming a `net.wimpi.telnetd.net.Connection`.
  * @group Algebra
  */
  def transK[M[_]: Monad: Catchable: Capture]: ConnectionIO ~> Kleisli[M, Connection, ?] =
   ConnectionOp.ConnectionKleisliTrans.transK

 /**
  * Natural transformation from `ConnectionIO` to `M`, given a `net.wimpi.telnetd.net.Connection`.
  * @group Algebra
  */
 def trans[M[_]: Monad: Catchable: Capture](c: Connection): ConnectionIO ~> M =
   ConnectionOp.ConnectionKleisliTrans.trans[M](c)

  /**
   * Syntax for `ConnectionIO`.
   * @group Algebra
   */
  implicit class ConnectionIOOps[A](ma: ConnectionIO[A]) {
    def transK[M[_]: Monad: Catchable: Capture]: Kleisli[M, Connection, A] =
      ConnectionOp.ConnectionKleisliTrans.transK[M].apply(ma)
  }

}

