package tuco.free

import cats.~>
import cats.effect.Async
import cats.free.{ Free => FF } // alias because some algebras have an op called Free

import java.lang.String
import java.net.InetAddress
import java.net.Socket
import java.util.HashMap
import java.util.Locale
import net.wimpi.telnetd.net.ConnectionData
import net.wimpi.telnetd.net.ConnectionManager

object connectiondata { module =>

  // Algebra of operations for ConnectionData. Each accepts a visitor as an alternatie to pattern-matching.
  sealed trait ConnectionDataOp[A] {
    def visit[F[_]](v: ConnectionDataOp.Visitor[F]): F[A]
  }

  // Free monad over ConnectionDataOp.
  type ConnectionDataIO[A] = FF[ConnectionDataOp, A]

  // Module of instances and constructors of ConnectionDataOp.
  object ConnectionDataOp {

    // Given a ConnectionData we can embed a ConnectionDataIO program in any algebra that understands embedding.
    implicit val ConnectionDataOpEmbeddable: Embeddable[ConnectionDataOp, ConnectionData] =
      new Embeddable[ConnectionDataOp, ConnectionData] {
        def embed[A](j: ConnectionData, fa: FF[ConnectionDataOp, A]) = Embedded.ConnectionData(j, fa)
      }

    // Interface for a natural tansformation ConnectionDataOp ~> F encoded via the visitor pattern.
    // This approach is much more efficient than pattern-matching for large algebras.
    trait Visitor[F[_]] extends (ConnectionDataOp ~> F) {
      final def apply[A](fa: ConnectionDataOp[A]): F[A] = fa.visit(this)

      // Common
      def raw[A](f: ConnectionData => A): F[A]
      def embed[A](e: Embedded[A]): F[A]
      def delay[A](a: () => A): F[A]
      def handleErrorWith[A](fa: ConnectionDataIO[A], f: Throwable => ConnectionDataIO[A]): F[A]
      def async[A](k: (Either[Throwable, A] => Unit) => Unit): F[A]

      // ConnectionData
      def activity: F[Unit]
      def getEnvironment: F[HashMap[AnyRef, AnyRef]]
      def getHostAddress: F[String]
      def getHostName: F[String]
      def getInetAddress: F[InetAddress]
      def getLastActivity: F[Long]
      def getLocale: F[Locale]
      def getLoginShell: F[String]
      def getManager: F[ConnectionManager]
      def getNegotiatedTerminalType: F[String]
      def getPort: F[Int]
      def getSocket: F[Socket]
      def getTerminalColumns: F[Int]
      def getTerminalGeometry: F[Array[Int]]
      def getTerminalRows: F[Int]
      def isLineMode: F[Boolean]
      def isTerminalGeometryChanged: F[Boolean]
      def isWarned: F[Boolean]
      def setLineMode(a: Boolean): F[Unit]
      def setLoginShell(a: String): F[Unit]
      def setNegotiatedTerminalType(a: String): F[Unit]
      def setTerminalGeometry(a: Int, b: Int): F[Unit]
      def setWarned(a: Boolean): F[Unit]

    }

    // Common operations for all algebras.
    case class Raw[A](f: ConnectionData => A) extends ConnectionDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.raw(f)
    }
    case class Embed[A](e: Embedded[A]) extends ConnectionDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.embed(e)
    }
    case class Delay[A](a: () => A) extends ConnectionDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.delay(a)
    }
    case class HandleErrorWith[A](fa: ConnectionDataIO[A], f: Throwable => ConnectionDataIO[A]) extends ConnectionDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.handleErrorWith(fa, f)
    }
    case class Async1[A](k: (Either[Throwable, A] => Unit) => Unit) extends ConnectionDataOp[A] {
      def visit[F[_]](v: Visitor[F]) = v.async(k)
    }

    // ConnectionData-specific operations.
    case object Activity extends ConnectionDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.activity
    }
    case object GetEnvironment extends ConnectionDataOp[HashMap[AnyRef, AnyRef]] {
      def visit[F[_]](v: Visitor[F]) = v.getEnvironment
    }
    case object GetHostAddress extends ConnectionDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getHostAddress
    }
    case object GetHostName extends ConnectionDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getHostName
    }
    case object GetInetAddress extends ConnectionDataOp[InetAddress] {
      def visit[F[_]](v: Visitor[F]) = v.getInetAddress
    }
    case object GetLastActivity extends ConnectionDataOp[Long] {
      def visit[F[_]](v: Visitor[F]) = v.getLastActivity
    }
    case object GetLocale extends ConnectionDataOp[Locale] {
      def visit[F[_]](v: Visitor[F]) = v.getLocale
    }
    case object GetLoginShell extends ConnectionDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getLoginShell
    }
    case object GetManager extends ConnectionDataOp[ConnectionManager] {
      def visit[F[_]](v: Visitor[F]) = v.getManager
    }
    case object GetNegotiatedTerminalType extends ConnectionDataOp[String] {
      def visit[F[_]](v: Visitor[F]) = v.getNegotiatedTerminalType
    }
    case object GetPort extends ConnectionDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getPort
    }
    case object GetSocket extends ConnectionDataOp[Socket] {
      def visit[F[_]](v: Visitor[F]) = v.getSocket
    }
    case object GetTerminalColumns extends ConnectionDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getTerminalColumns
    }
    case object GetTerminalGeometry extends ConnectionDataOp[Array[Int]] {
      def visit[F[_]](v: Visitor[F]) = v.getTerminalGeometry
    }
    case object GetTerminalRows extends ConnectionDataOp[Int] {
      def visit[F[_]](v: Visitor[F]) = v.getTerminalRows
    }
    case object IsLineMode extends ConnectionDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isLineMode
    }
    case object IsTerminalGeometryChanged extends ConnectionDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isTerminalGeometryChanged
    }
    case object IsWarned extends ConnectionDataOp[Boolean] {
      def visit[F[_]](v: Visitor[F]) = v.isWarned
    }
    case class  SetLineMode(a: Boolean) extends ConnectionDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setLineMode(a)
    }
    case class  SetLoginShell(a: String) extends ConnectionDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setLoginShell(a)
    }
    case class  SetNegotiatedTerminalType(a: String) extends ConnectionDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setNegotiatedTerminalType(a)
    }
    case class  SetTerminalGeometry(a: Int, b: Int) extends ConnectionDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setTerminalGeometry(a, b)
    }
    case class  SetWarned(a: Boolean) extends ConnectionDataOp[Unit] {
      def visit[F[_]](v: Visitor[F]) = v.setWarned(a)
    }

  }
  import ConnectionDataOp._

  // Smart constructors for operations common to all algebras.
  val unit: ConnectionDataIO[Unit] = FF.pure[ConnectionDataOp, Unit](())
  def raw[A](f: ConnectionData => A): ConnectionDataIO[A] = FF.liftF(Raw(f))
  def embed[F[_], J, A](j: J, fa: FF[F, A])(implicit ev: Embeddable[F, J]): FF[ConnectionDataOp, A] = FF.liftF(Embed(ev.embed(j, fa)))
  def delay[A](a: => A): ConnectionDataIO[A] = FF.liftF(Delay(() => a))
  def handleErrorWith[A](fa: ConnectionDataIO[A], f: Throwable => ConnectionDataIO[A]): ConnectionDataIO[A] = FF.liftF[ConnectionDataOp, A](HandleErrorWith(fa, f))
  def raiseError[A](err: Throwable): ConnectionDataIO[A] = delay(throw err)
  def async[A](k: (Either[Throwable, A] => Unit) => Unit): ConnectionDataIO[A] = FF.liftF[ConnectionDataOp, A](Async1(k))

  // Smart constructors for ConnectionData-specific operations.
  val activity: ConnectionDataIO[Unit] = FF.liftF(Activity)
  val getEnvironment: ConnectionDataIO[HashMap[AnyRef, AnyRef]] = FF.liftF(GetEnvironment)
  val getHostAddress: ConnectionDataIO[String] = FF.liftF(GetHostAddress)
  val getHostName: ConnectionDataIO[String] = FF.liftF(GetHostName)
  val getInetAddress: ConnectionDataIO[InetAddress] = FF.liftF(GetInetAddress)
  val getLastActivity: ConnectionDataIO[Long] = FF.liftF(GetLastActivity)
  val getLocale: ConnectionDataIO[Locale] = FF.liftF(GetLocale)
  val getLoginShell: ConnectionDataIO[String] = FF.liftF(GetLoginShell)
  val getManager: ConnectionDataIO[ConnectionManager] = FF.liftF(GetManager)
  val getNegotiatedTerminalType: ConnectionDataIO[String] = FF.liftF(GetNegotiatedTerminalType)
  val getPort: ConnectionDataIO[Int] = FF.liftF(GetPort)
  val getSocket: ConnectionDataIO[Socket] = FF.liftF(GetSocket)
  val getTerminalColumns: ConnectionDataIO[Int] = FF.liftF(GetTerminalColumns)
  val getTerminalGeometry: ConnectionDataIO[Array[Int]] = FF.liftF(GetTerminalGeometry)
  val getTerminalRows: ConnectionDataIO[Int] = FF.liftF(GetTerminalRows)
  val isLineMode: ConnectionDataIO[Boolean] = FF.liftF(IsLineMode)
  val isTerminalGeometryChanged: ConnectionDataIO[Boolean] = FF.liftF(IsTerminalGeometryChanged)
  val isWarned: ConnectionDataIO[Boolean] = FF.liftF(IsWarned)
  def setLineMode(a: Boolean): ConnectionDataIO[Unit] = FF.liftF(SetLineMode(a))
  def setLoginShell(a: String): ConnectionDataIO[Unit] = FF.liftF(SetLoginShell(a))
  def setNegotiatedTerminalType(a: String): ConnectionDataIO[Unit] = FF.liftF(SetNegotiatedTerminalType(a))
  def setTerminalGeometry(a: Int, b: Int): ConnectionDataIO[Unit] = FF.liftF(SetTerminalGeometry(a, b))
  def setWarned(a: Boolean): ConnectionDataIO[Unit] = FF.liftF(SetWarned(a))

  // ConnectionDataIO is an Async
  implicit val AsyncConnectionDataIO: Async[ConnectionDataIO] =
    new Async[ConnectionDataIO] {
      val M = FF.catsFreeMonadForFree[ConnectionDataOp]
      def pure[A](x: A): ConnectionDataIO[A] = M.pure(x)
      def handleErrorWith[A](fa: ConnectionDataIO[A])(f: Throwable => ConnectionDataIO[A]): ConnectionDataIO[A] = module.handleErrorWith(fa, f)
      def raiseError[A](e: Throwable): ConnectionDataIO[A] = module.raiseError(e)
      def async[A](k: (Either[Throwable,A] => Unit) => Unit): ConnectionDataIO[A] = module.async(k)
      def flatMap[A, B](fa: ConnectionDataIO[A])(f: A => ConnectionDataIO[B]): ConnectionDataIO[B] = M.flatMap(fa)(f)
      def tailRecM[A, B](a: A)(f: A => ConnectionDataIO[Either[A, B]]): ConnectionDataIO[B] = M.tailRecM(a)(f)
      def suspend[A](thunk: => ConnectionDataIO[A]): ConnectionDataIO[A] = M.flatten(module.delay(thunk))
    }

}

