package can.free

import scalaz.{ Catchable, Free => F, Kleisli, Monad, ~>, \/ }

import java.lang.String
import java.net.InetAddress
import java.net.Socket
import java.util.HashMap
import java.util.Locale
import net.wimpi.telnetd.io.BasicTerminalIO
import net.wimpi.telnetd.net.Connection
import net.wimpi.telnetd.net.ConnectionData
import net.wimpi.telnetd.net.ConnectionEvent
import net.wimpi.telnetd.net.ConnectionListener
import net.wimpi.telnetd.net.ConnectionManager

import connection.ConnectionIO
import connectiondata.ConnectionDataIO
import connectionevent.ConnectionEventIO
import connectionlistener.ConnectionListenerIO
import basicterminalio.BasicTerminalIOIO

/**
 * Algebra and free monad for primitive operations over a `net.wimpi.telnetd.net.ConnectionData`.
 * @group Modules
 */
object connectiondata {

  /**
   * Sum type of primitive operations over a `net.wimpi.telnetd.net.ConnectionData`.
   * @group Algebra
   */
  sealed trait ConnectionDataOp[A] {
    protected def primitive[M[_]: Monad: Capture](f: ConnectionData => A): Kleisli[M, ConnectionData, A] =
      Kleisli((s: ConnectionData) => Capture[M].apply(f(s)))
    def defaultTransK[M[_]: Monad: Catchable: Capture]: Kleisli[M, ConnectionData, A]
  }

  /**
   * Module of constructors for `ConnectionDataOp`. These are rarely useful outside of the implementation;
   * prefer the smart constructors provided by the `connectiondata` module.
   * @group Algebra
   */
  object ConnectionDataOp {

    // This algebra has a default interpreter
    implicit val ConnectionDataKleisliTrans: KleisliTrans.Aux[ConnectionDataOp, ConnectionData] =
      new KleisliTrans[ConnectionDataOp] {
        type J = ConnectionData
        def interpK[M[_]: Monad: Catchable: Capture]: ConnectionDataOp ~> Kleisli[M, ConnectionData, ?] =
          new (ConnectionDataOp ~> Kleisli[M, ConnectionData, ?]) {
            def apply[A](op: ConnectionDataOp[A]): Kleisli[M, ConnectionData, A] =
              op.defaultTransK[M]
          }
      }

    // Lifting
    case class Lift[Op[_], A, J](j: J, action: F[Op, A], mod: KleisliTrans.Aux[Op, J]) extends ConnectionDataOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = Kleisli(_ => mod.transK[M].apply(action).run(j))
    }

    // Combinators
    case class Attempt[A](action: ConnectionDataIO[A]) extends ConnectionDataOp[Throwable \/ A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] =
        Predef.implicitly[Catchable[Kleisli[M, ConnectionData, ?]]].attempt(action.transK[M])
    }
    case class Pure[A](a: () => A) extends ConnectionDataOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_ => a())
    }
    case class Raw[A](f: ConnectionData => A) extends ConnectionDataOp[A] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(f)
    }

    // Primitive Operations
    case object Activity extends ConnectionDataOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.activity())
    }
    case object GetEnvironment extends ConnectionDataOp[HashMap[AnyRef, AnyRef]] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getEnvironment())
    }
    case object GetHostAddress extends ConnectionDataOp[String] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getHostAddress())
    }
    case object GetHostName extends ConnectionDataOp[String] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getHostName())
    }
    case object GetInetAddress extends ConnectionDataOp[InetAddress] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getInetAddress())
    }
    case object GetLastActivity extends ConnectionDataOp[Long] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getLastActivity())
    }
    case object GetLocale extends ConnectionDataOp[Locale] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getLocale())
    }
    case object GetLoginShell extends ConnectionDataOp[String] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getLoginShell())
    }
    case object GetManager extends ConnectionDataOp[ConnectionManager] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getManager())
    }
    case object GetNegotiatedTerminalType extends ConnectionDataOp[String] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getNegotiatedTerminalType())
    }
    case object GetPort extends ConnectionDataOp[Int] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getPort())
    }
    case object GetSocket extends ConnectionDataOp[Socket] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getSocket())
    }
    case object GetTerminalColumns extends ConnectionDataOp[Int] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getTerminalColumns())
    }
    case object GetTerminalGeometry extends ConnectionDataOp[Array[Int]] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getTerminalGeometry())
    }
    case object GetTerminalRows extends ConnectionDataOp[Int] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.getTerminalRows())
    }
    case object IsLineMode extends ConnectionDataOp[Boolean] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.isLineMode())
    }
    case object IsTerminalGeometryChanged extends ConnectionDataOp[Boolean] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.isTerminalGeometryChanged())
    }
    case object IsWarned extends ConnectionDataOp[Boolean] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.isWarned())
    }
    case class  SetLineMode(a: Boolean) extends ConnectionDataOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setLineMode(a))
    }
    case class  SetLoginShell(a: String) extends ConnectionDataOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setLoginShell(a))
    }
    case class  SetNegotiatedTerminalType(a: String) extends ConnectionDataOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setNegotiatedTerminalType(a))
    }
    case class  SetTerminalGeometry(a: Int, b: Int) extends ConnectionDataOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setTerminalGeometry(a, b))
    }
    case class  SetWarned(a: Boolean) extends ConnectionDataOp[Unit] {
      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_.setWarned(a))
    }

  }
  import ConnectionDataOp._ // We use these immediately

  /**
   * Free monad over a free functor of [[ConnectionDataOp]]; abstractly, a computation that consumes
   * a `net.wimpi.telnetd.net.ConnectionData` and produces a value of type `A`.
   * @group Algebra
   */
  type ConnectionDataIO[A] = F[ConnectionDataOp, A]

  /**
   * Catchable instance for [[ConnectionDataIO]].
   * @group Typeclass Instances
   */
  implicit val CatchableConnectionDataIO: Catchable[ConnectionDataIO] =
    new Catchable[ConnectionDataIO] {
      def attempt[A](f: ConnectionDataIO[A]): ConnectionDataIO[Throwable \/ A] = connectiondata.attempt(f)
      def fail[A](err: Throwable): ConnectionDataIO[A] = connectiondata.delay(throw err)
    }

  /**
   * Capture instance for [[ConnectionDataIO]].
   * @group Typeclass Instances
   */
  implicit val CaptureConnectionDataIO: Capture[ConnectionDataIO] =
    new Capture[ConnectionDataIO] {
      def apply[A](a: => A): ConnectionDataIO[A] = connectiondata.delay(a)
    }

  /**
   * Lift a different type of program that has a default Kleisli interpreter.
   * @group Constructors (Lifting)
   */
  def lift[Op[_], A, J](j: J, action: F[Op, A])(implicit mod: KleisliTrans.Aux[Op, J]): ConnectionDataIO[A] =
    F.liftF(Lift(j, action, mod))

  /**
   * Lift a ConnectionDataIO[A] into an exception-capturing ConnectionDataIO[Throwable \/ A].
   * @group Constructors (Lifting)
   */
  def attempt[A](a: ConnectionDataIO[A]): ConnectionDataIO[Throwable \/ A] =
    F.liftF[ConnectionDataOp, Throwable \/ A](Attempt(a))

  /**
   * Non-strict unit for capturing effects.
   * @group Constructors (Lifting)
   */
  def delay[A](a: => A): ConnectionDataIO[A] =
    F.liftF(Pure(a _))

  /**
   * Backdoor for arbitrary computations on the underlying ConnectionData.
   * @group Constructors (Lifting)
   */
  def raw[A](f: ConnectionData => A): ConnectionDataIO[A] =
    F.liftF(Raw(f))

  /**
   * @group Constructors (Primitives)
   */
  val activity: ConnectionDataIO[Unit] =
    F.liftF(Activity)

  /**
   * @group Constructors (Primitives)
   */
  val getEnvironment: ConnectionDataIO[HashMap[AnyRef, AnyRef]] =
    F.liftF(GetEnvironment)

  /**
   * @group Constructors (Primitives)
   */
  val getHostAddress: ConnectionDataIO[String] =
    F.liftF(GetHostAddress)

  /**
   * @group Constructors (Primitives)
   */
  val getHostName: ConnectionDataIO[String] =
    F.liftF(GetHostName)

  /**
   * @group Constructors (Primitives)
   */
  val getInetAddress: ConnectionDataIO[InetAddress] =
    F.liftF(GetInetAddress)

  /**
   * @group Constructors (Primitives)
   */
  val getLastActivity: ConnectionDataIO[Long] =
    F.liftF(GetLastActivity)

  /**
   * @group Constructors (Primitives)
   */
  val getLocale: ConnectionDataIO[Locale] =
    F.liftF(GetLocale)

  /**
   * @group Constructors (Primitives)
   */
  val getLoginShell: ConnectionDataIO[String] =
    F.liftF(GetLoginShell)

  /**
   * @group Constructors (Primitives)
   */
  val getManager: ConnectionDataIO[ConnectionManager] =
    F.liftF(GetManager)

  /**
   * @group Constructors (Primitives)
   */
  val getNegotiatedTerminalType: ConnectionDataIO[String] =
    F.liftF(GetNegotiatedTerminalType)

  /**
   * @group Constructors (Primitives)
   */
  val getPort: ConnectionDataIO[Int] =
    F.liftF(GetPort)

  /**
   * @group Constructors (Primitives)
   */
  val getSocket: ConnectionDataIO[Socket] =
    F.liftF(GetSocket)

  /**
   * @group Constructors (Primitives)
   */
  val getTerminalColumns: ConnectionDataIO[Int] =
    F.liftF(GetTerminalColumns)

  /**
   * @group Constructors (Primitives)
   */
  val getTerminalGeometry: ConnectionDataIO[Array[Int]] =
    F.liftF(GetTerminalGeometry)

  /**
   * @group Constructors (Primitives)
   */
  val getTerminalRows: ConnectionDataIO[Int] =
    F.liftF(GetTerminalRows)

  /**
   * @group Constructors (Primitives)
   */
  val isLineMode: ConnectionDataIO[Boolean] =
    F.liftF(IsLineMode)

  /**
   * @group Constructors (Primitives)
   */
  val isTerminalGeometryChanged: ConnectionDataIO[Boolean] =
    F.liftF(IsTerminalGeometryChanged)

  /**
   * @group Constructors (Primitives)
   */
  val isWarned: ConnectionDataIO[Boolean] =
    F.liftF(IsWarned)

  /**
   * @group Constructors (Primitives)
   */
  def setLineMode(a: Boolean): ConnectionDataIO[Unit] =
    F.liftF(SetLineMode(a))

  /**
   * @group Constructors (Primitives)
   */
  def setLoginShell(a: String): ConnectionDataIO[Unit] =
    F.liftF(SetLoginShell(a))

  /**
   * @group Constructors (Primitives)
   */
  def setNegotiatedTerminalType(a: String): ConnectionDataIO[Unit] =
    F.liftF(SetNegotiatedTerminalType(a))

  /**
   * @group Constructors (Primitives)
   */
  def setTerminalGeometry(a: Int, b: Int): ConnectionDataIO[Unit] =
    F.liftF(SetTerminalGeometry(a, b))

  /**
   * @group Constructors (Primitives)
   */
  def setWarned(a: Boolean): ConnectionDataIO[Unit] =
    F.liftF(SetWarned(a))

 /**
  * Natural transformation from `ConnectionDataOp` to `Kleisli` for the given `M`, consuming a `net.wimpi.telnetd.net.ConnectionData`.
  * @group Algebra
  */
  def interpK[M[_]: Monad: Catchable: Capture]: ConnectionDataOp ~> Kleisli[M, ConnectionData, ?] =
   ConnectionDataOp.ConnectionDataKleisliTrans.interpK

 /**
  * Natural transformation from `ConnectionDataIO` to `Kleisli` for the given `M`, consuming a `net.wimpi.telnetd.net.ConnectionData`.
  * @group Algebra
  */
  def transK[M[_]: Monad: Catchable: Capture]: ConnectionDataIO ~> Kleisli[M, ConnectionData, ?] =
   ConnectionDataOp.ConnectionDataKleisliTrans.transK

 /**
  * Natural transformation from `ConnectionDataIO` to `M`, given a `net.wimpi.telnetd.net.ConnectionData`.
  * @group Algebra
  */
 def trans[M[_]: Monad: Catchable: Capture](c: ConnectionData): ConnectionDataIO ~> M =
   ConnectionDataOp.ConnectionDataKleisliTrans.trans[M](c)

  /**
   * Syntax for `ConnectionDataIO`.
   * @group Algebra
   */
  implicit class ConnectionDataIOOps[A](ma: ConnectionDataIO[A]) {
    def transK[M[_]: Monad: Catchable: Capture]: Kleisli[M, ConnectionData, A] =
      ConnectionDataOp.ConnectionDataKleisliTrans.transK[M].apply(ma)
  }

}

