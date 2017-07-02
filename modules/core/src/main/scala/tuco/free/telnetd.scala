package tuco.free
//import tuco.util.Capture

import cats.{ Monad, MonadError, ~> }
import cats.data.Kleisli
import cats.free.{ Free => F }
import cats.effect.Sync

import java.lang.String
import java.util.Properties
import net.wimpi.telnetd.TelnetD
import net.wimpi.telnetd.io.BasicTerminalIO
import net.wimpi.telnetd.net.Connection
import net.wimpi.telnetd.net.ConnectionData
import net.wimpi.telnetd.net.ConnectionEvent
import net.wimpi.telnetd.net.ConnectionListener
import net.wimpi.telnetd.net.PortListener

import connection.ConnectionIO
import connectiondata.ConnectionDataIO
import connectionevent.ConnectionEventIO
import connectionlistener.ConnectionListenerIO
import basicterminalio.BasicTerminalIOIO
import telnetd.TelnetDIO

/**
 * Algebra and free monad for primitive operations over a `net.wimpi.telnetd.TelnetD`.
 * @group Modules
 */
object telnetd {

  /**
   * Sum type of primitive operations over a `net.wimpi.telnetd.TelnetD`.
   * @group Algebra
   */
  sealed trait TelnetDOp[A] {
    protected def primitive[M[_]: Sync](f: TelnetD => A): Kleisli[M, TelnetD, A] =
      Kleisli((s: TelnetD) => Sync[M].delay(f(s)))
    def defaultTransK[M[_]: Sync]: Kleisli[M, TelnetD, A]
  }

  /**
   * Module of constructors for `TelnetDOp`. These are rarely useful outside of the implementation;
   * prefer the smart constructors provided by the `telnetd` module.
   * @group Algebra
   */
  object TelnetDOp {

    // This algebra has a default interpreter
    implicit val TelnetDKleisliTrans: KleisliTrans.Aux[TelnetDOp, TelnetD] =
      new KleisliTrans[TelnetDOp] {
        type J = TelnetD
        def interpK[M[_]: Sync]: TelnetDOp ~> Kleisli[M, TelnetD, ?] =
          new (TelnetDOp ~> Kleisli[M, TelnetD, ?]) {
            def apply[A](op: TelnetDOp[A]): Kleisli[M, TelnetD, A] =
              op.defaultTransK[M]
          }
      }

    // Lifting
    case class Lift[Op[_], A, J](j: J, action: F[Op, A], mod: KleisliTrans.Aux[Op, J]) extends TelnetDOp[A] {
      override def defaultTransK[M[_]: Sync] = Kleisli(_ => mod.transK[M].apply(action).run(j))
    }

    // Combinators
    case class Attempt[A](action: TelnetDIO[A]) extends TelnetDOp[Either[Throwable, A]] {
      override def defaultTransK[M[_]: Sync] =
        Kleisli((e: TelnetD) => Sync[M].attempt(action.transK[M].apply(e)))
    }
    case class Pure[A](a: () => A) extends TelnetDOp[A] {
      override def defaultTransK[M[_]: Sync] = primitive(_ => a())
    }
    case class Raw[A](f: TelnetD => A) extends TelnetDOp[A] {
      override def defaultTransK[M[_]: Sync] = primitive(f)
    }

    // Primitive Operations
    case class  GetPortListener(a: String) extends TelnetDOp[PortListener] {
      override def defaultTransK[M[_]: Sync] = primitive(_.getPortListener(a))
    }
    case class  PrepareListener(a: String, b: Properties) extends TelnetDOp[Unit] {
      override def defaultTransK[M[_]: Sync] = primitive(_.prepareListener(a, b))
    }
    case object Start extends TelnetDOp[Unit] {
      override def defaultTransK[M[_]: Sync] = primitive(_.start())
    }
    case object Stop extends TelnetDOp[Unit] {
      override def defaultTransK[M[_]: Sync] = primitive(_.stop())
    }

  }
  import TelnetDOp._ // We use these immediately

  /**
   * Free monad over a free functor of [[TelnetDOp]]; abstractly, a computation that consumes
   * a `net.wimpi.telnetd.TelnetD` and produces a value of type `A`.
   * @group Algebra
   */
  type TelnetDIO[A] = F[TelnetDOp, A]

  /**
   * Catchable instance for [[TelnetDIO]].
   * @group Typeclass Instances
   */
//  implicit val CatchableTelnetDIO: Catchable[TelnetDIO] =
//    new Catchable[TelnetDIO] {
//      def attempt[A](f: TelnetDIO[A]): TelnetDIO[Throwable Either A] = telnetd.attempt(f)
//      def fail[A](err: Throwable): TelnetDIO[A] = telnetd.delay(throw err)
//    }

  /**
   * Capture instance for [[TelnetDIO]].
   * @group Typeclass Instances
   */
//  implicit val CaptureTelnetDIO: Capture[TelnetDIO] =
//    new Capture[TelnetDIO] {
//      def apply[A](a: => A): TelnetDIO[A] = telnetd.delay(a)
//    }

  /**
   * Lift a different type of program that has a default Kleisli interpreter.
   * @group Constructors (Lifting)
   */
  def lift[Op[_], A, J](j: J, action: F[Op, A])(implicit mod: KleisliTrans.Aux[Op, J]): TelnetDIO[A] =
    F.liftF(Lift(j, action, mod))

  /**
   * Lift a TelnetDIO[A] into an exception-capturing TelnetDIO[Throwable Either A].
   * @group Constructors (Lifting)
   */
  def attempt[A](a: TelnetDIO[A]): TelnetDIO[Throwable Either A] =
    F.liftF[TelnetDOp, Throwable Either A](Attempt(a))

  /**
   * Non-strict unit for capturing effects.
   * @group Constructors (Lifting)
   */
  def delay[A](a: => A): TelnetDIO[A] =
    F.liftF(Pure(a _))

  /**
   * Backdoor for arbitrary computations on the underlying TelnetD.
   * @group Constructors (Lifting)
   */
  def raw[A](f: TelnetD => A): TelnetDIO[A] =
    F.liftF(Raw(f))

  /**
   * @group Constructors (Primitives)
   */
  def getPortListener(a: String): TelnetDIO[PortListener] =
    F.liftF(GetPortListener(a))

  /**
   * @group Constructors (Primitives)
   */
  def prepareListener(a: String, b: Properties): TelnetDIO[Unit] =
    F.liftF(PrepareListener(a, b))

  /**
   * @group Constructors (Primitives)
   */
  val start: TelnetDIO[Unit] =
    F.liftF(Start)

  /**
   * @group Constructors (Primitives)
   */
  val stop: TelnetDIO[Unit] =
    F.liftF(Stop)

 /**
  * Natural transformation from `TelnetDOp` to `Kleisli` for the given `M`, consuming a `net.wimpi.telnetd.TelnetD`.
  * @group Algebra
  */
  def interpK[M[_]: Sync]: TelnetDOp ~> Kleisli[M, TelnetD, ?] =
   TelnetDOp.TelnetDKleisliTrans.interpK

 /**
  * Natural transformation from `TelnetDIO` to `Kleisli` for the given `M`, consuming a `net.wimpi.telnetd.TelnetD`.
  * @group Algebra
  */
  def transK[M[_]: Sync]: TelnetDIO ~> Kleisli[M, TelnetD, ?] =
   TelnetDOp.TelnetDKleisliTrans.transK

 /**
  * Natural transformation from `TelnetDIO` to `M`, given a `net.wimpi.telnetd.TelnetD`.
  * @group Algebra
  */
 def trans[M[_]: Sync](c: TelnetD): TelnetDIO ~> M =
   TelnetDOp.TelnetDKleisliTrans.trans[M](c)

  /**
   * Syntax for `TelnetDIO`.
   * @group Algebra
   */
  implicit class TelnetDIOOps[A](ma: TelnetDIO[A]) {
    def transK[M[_]: Sync]: Kleisli[M, TelnetD, A] =
      TelnetDOp.TelnetDKleisliTrans.transK[M].apply(ma)
  }

}

