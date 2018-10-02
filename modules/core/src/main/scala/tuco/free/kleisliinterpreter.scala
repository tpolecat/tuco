package tuco.free

// Library imports
import cats.~>
import cats.data.Kleisli
import cats.effect.{ Async, ContextShift, ExitCase }
import scala.concurrent.ExecutionContext

// Types referenced in the JDBC API
import java.lang.String
import java.net.InetAddress
import java.net.Socket
import java.util.HashMap
import java.util.Locale
import java.util.Properties
import net.wimpi.telnetd.TelnetD
import net.wimpi.telnetd.io.BasicTerminalIO
import net.wimpi.telnetd.net.Connection
import net.wimpi.telnetd.net.ConnectionData
import net.wimpi.telnetd.net.ConnectionEvent
import net.wimpi.telnetd.net.ConnectionListener
import net.wimpi.telnetd.net.ConnectionManager
import net.wimpi.telnetd.net.PortListener

// Algebras and free monads thereof referenced by our interpreter.
import tuco.free.connection.{ ConnectionIO, ConnectionOp }
import tuco.free.connectiondata.{ ConnectionDataIO, ConnectionDataOp }
import tuco.free.connectionevent.{ ConnectionEventIO, ConnectionEventOp }
import tuco.free.connectionlistener.{ ConnectionListenerIO, ConnectionListenerOp }
import tuco.free.basicterminalio.{ BasicTerminalIOIO, BasicTerminalIOOp }
import tuco.free.telnetd.{ TelnetDIO, TelnetDOp }

object KleisliInterpreter {

  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  def apply[M[_]](
    implicit am: Async[M],
             cs: ContextShift[M]
  ): KleisliInterpreter[M] =
    new KleisliInterpreter[M] {
      val asyncM = am
      val contextShiftM = cs
    }

}

// Family of interpreters into Kleisli arrows for some monad M.
trait KleisliInterpreter[M[_]] { outer =>

  implicit val asyncM: Async[M]
  val contextShiftM: ContextShift[M]

  // The 6 interpreters, with definitions below. These can be overridden to customize behavior.
  lazy val ConnectionInterpreter: ConnectionOp ~> Kleisli[M, Connection, ?] = new ConnectionInterpreter { }
  lazy val ConnectionDataInterpreter: ConnectionDataOp ~> Kleisli[M, ConnectionData, ?] = new ConnectionDataInterpreter { }
  lazy val ConnectionEventInterpreter: ConnectionEventOp ~> Kleisli[M, ConnectionEvent, ?] = new ConnectionEventInterpreter { }
  lazy val ConnectionListenerInterpreter: ConnectionListenerOp ~> Kleisli[M, ConnectionListener, ?] = new ConnectionListenerInterpreter { }
  lazy val BasicTerminalIOInterpreter: BasicTerminalIOOp ~> Kleisli[M, BasicTerminalIO, ?] = new BasicTerminalIOInterpreter { }
  lazy val TelnetDInterpreter: TelnetDOp ~> Kleisli[M, TelnetD, ?] = new TelnetDInterpreter { }

  // Some methods are common to all interpreters and can be overridden to change behavior globally.
  def primitive[J, A](f: J => A): Kleisli[M, J, A] = Kleisli(a => asyncM.delay(f(a)))
  def delay[J, A](a: () => A): Kleisli[M, J, A] = Kleisli(_ => asyncM.delay(a()))
  def raw[J, A](f: J => A): Kleisli[M, J, A] = primitive(f)
  def async[J, A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, J, A] = Kleisli(_ => asyncM.async(k))
  def embed[J, A](e: Embedded[A]): Kleisli[M, J, A] =
    e match {
      case Embedded.Connection(j, fa) => Kleisli(_ => fa.foldMap(ConnectionInterpreter).run(j))
      case Embedded.ConnectionData(j, fa) => Kleisli(_ => fa.foldMap(ConnectionDataInterpreter).run(j))
      case Embedded.ConnectionEvent(j, fa) => Kleisli(_ => fa.foldMap(ConnectionEventInterpreter).run(j))
      case Embedded.ConnectionListener(j, fa) => Kleisli(_ => fa.foldMap(ConnectionListenerInterpreter).run(j))
      case Embedded.BasicTerminalIO(j, fa) => Kleisli(_ => fa.foldMap(BasicTerminalIOInterpreter).run(j))
      case Embedded.TelnetD(j, fa) => Kleisli(_ => fa.foldMap(TelnetDInterpreter).run(j))
    }

  // Interpreters
  trait ConnectionInterpreter extends ConnectionOp.Visitor[Kleisli[M, Connection, ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: Connection => A): Kleisli[M, Connection, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, Connection, A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, Connection, A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, Connection, A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionIO[Unit]): Kleisli[M, Connection, A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: ConnectionIO[A], f: Throwable => ConnectionIO[A]): Kleisli[M, Connection, A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: ConnectionIO[A])(use: A => ConnectionIO[B])(release: (A, ExitCase[Throwable]) => ConnectionIO[Unit]): Kleisli[M, Connection, B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, Connection, Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: ConnectionIO[A]): Kleisli[M, Connection, A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    // domain-specific operations are implemented in terms of `primitive`
    override def addConnectionListener(a: ConnectionListener) = primitive(_.addConnectionListener(a))
    override def close = primitive(_.close)
    override def getConnectionData = primitive(_.getConnectionData)
    override def getTerminalIO = primitive(_.getTerminalIO)
    override def isActive = primitive(_.isActive)
    override def processConnectionEvent(a: ConnectionEvent) = primitive(_.processConnectionEvent(a))
    override def removeConnectionListener(a: ConnectionListener) = primitive(_.removeConnectionListener(a))
    override def run = primitive(_.run)
    override def setNextShell(a: String) = primitive(_.setNextShell(a))

  }

  trait ConnectionDataInterpreter extends ConnectionDataOp.Visitor[Kleisli[M, ConnectionData, ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: ConnectionData => A): Kleisli[M, ConnectionData, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, ConnectionData, A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, ConnectionData, A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, ConnectionData, A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionDataIO[Unit]): Kleisli[M, ConnectionData, A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: ConnectionDataIO[A], f: Throwable => ConnectionDataIO[A]): Kleisli[M, ConnectionData, A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: ConnectionDataIO[A])(use: A => ConnectionDataIO[B])(release: (A, ExitCase[Throwable]) => ConnectionDataIO[Unit]): Kleisli[M, ConnectionData, B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, ConnectionData, Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: ConnectionDataIO[A]): Kleisli[M, ConnectionData, A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    // domain-specific operations are implemented in terms of `primitive`
    override def activity = primitive(_.activity)
    override def getEnvironment = primitive(_.getEnvironment)
    override def getHostAddress = primitive(_.getHostAddress)
    override def getHostName = primitive(_.getHostName)
    override def getInetAddress = primitive(_.getInetAddress)
    override def getLastActivity = primitive(_.getLastActivity)
    override def getLocale = primitive(_.getLocale)
    override def getLoginShell = primitive(_.getLoginShell)
    override def getManager = primitive(_.getManager)
    override def getNegotiatedTerminalType = primitive(_.getNegotiatedTerminalType)
    override def getPort = primitive(_.getPort)
    override def getSocket = primitive(_.getSocket)
    override def getTerminalColumns = primitive(_.getTerminalColumns)
    override def getTerminalGeometry = primitive(_.getTerminalGeometry)
    override def getTerminalRows = primitive(_.getTerminalRows)
    override def isLineMode = primitive(_.isLineMode)
    override def isTerminalGeometryChanged = primitive(_.isTerminalGeometryChanged)
    override def isWarned = primitive(_.isWarned)
    override def setLineMode(a: Boolean) = primitive(_.setLineMode(a))
    override def setLoginShell(a: String) = primitive(_.setLoginShell(a))
    override def setNegotiatedTerminalType(a: String) = primitive(_.setNegotiatedTerminalType(a))
    override def setTerminalGeometry(a: Int, b: Int) = primitive(_.setTerminalGeometry(a, b))
    override def setWarned(a: Boolean) = primitive(_.setWarned(a))

  }

  trait ConnectionEventInterpreter extends ConnectionEventOp.Visitor[Kleisli[M, ConnectionEvent, ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: ConnectionEvent => A): Kleisli[M, ConnectionEvent, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, ConnectionEvent, A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, ConnectionEvent, A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, ConnectionEvent, A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionEventIO[Unit]): Kleisli[M, ConnectionEvent, A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: ConnectionEventIO[A], f: Throwable => ConnectionEventIO[A]): Kleisli[M, ConnectionEvent, A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: ConnectionEventIO[A])(use: A => ConnectionEventIO[B])(release: (A, ExitCase[Throwable]) => ConnectionEventIO[Unit]): Kleisli[M, ConnectionEvent, B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, ConnectionEvent, Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: ConnectionEventIO[A]): Kleisli[M, ConnectionEvent, A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    // domain-specific operations are implemented in terms of `primitive`
    override def getConnection = primitive(_.getConnection)
    override def getSource = primitive(_.getSource)
    override def isType(a: Int) = primitive(_.isType(a))

  }

  trait ConnectionListenerInterpreter extends ConnectionListenerOp.Visitor[Kleisli[M, ConnectionListener, ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: ConnectionListener => A): Kleisli[M, ConnectionListener, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, ConnectionListener, A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, ConnectionListener, A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, ConnectionListener, A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => ConnectionListenerIO[Unit]): Kleisli[M, ConnectionListener, A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: ConnectionListenerIO[A], f: Throwable => ConnectionListenerIO[A]): Kleisli[M, ConnectionListener, A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: ConnectionListenerIO[A])(use: A => ConnectionListenerIO[B])(release: (A, ExitCase[Throwable]) => ConnectionListenerIO[Unit]): Kleisli[M, ConnectionListener, B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, ConnectionListener, Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: ConnectionListenerIO[A]): Kleisli[M, ConnectionListener, A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    // domain-specific operations are implemented in terms of `primitive`
    override def connectionIdle(a: ConnectionEvent) = primitive(_.connectionIdle(a))
    override def connectionLogoutRequest(a: ConnectionEvent) = primitive(_.connectionLogoutRequest(a))
    override def connectionSentBreak(a: ConnectionEvent) = primitive(_.connectionSentBreak(a))
    override def connectionTimedOut(a: ConnectionEvent) = primitive(_.connectionTimedOut(a))

  }

  trait BasicTerminalIOInterpreter extends BasicTerminalIOOp.Visitor[Kleisli[M, BasicTerminalIO, ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: BasicTerminalIO => A): Kleisli[M, BasicTerminalIO, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, BasicTerminalIO, A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, BasicTerminalIO, A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, BasicTerminalIO, A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => BasicTerminalIOIO[Unit]): Kleisli[M, BasicTerminalIO, A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: BasicTerminalIOIO[A], f: Throwable => BasicTerminalIOIO[A]): Kleisli[M, BasicTerminalIO, A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: BasicTerminalIOIO[A])(use: A => BasicTerminalIOIO[B])(release: (A, ExitCase[Throwable]) => BasicTerminalIOIO[Unit]): Kleisli[M, BasicTerminalIO, B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, BasicTerminalIO, Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: BasicTerminalIOIO[A]): Kleisli[M, BasicTerminalIO, A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    // domain-specific operations are implemented in terms of `primitive`
    override def bell = primitive(_.bell)
    override def close = primitive(_.close)
    override def defineScrollRegion(a: Int, b: Int) = primitive(_.defineScrollRegion(a, b))
    override def eraseLine = primitive(_.eraseLine)
    override def eraseScreen = primitive(_.eraseScreen)
    override def eraseToBeginOfLine = primitive(_.eraseToBeginOfLine)
    override def eraseToBeginOfScreen = primitive(_.eraseToBeginOfScreen)
    override def eraseToEndOfLine = primitive(_.eraseToEndOfLine)
    override def eraseToEndOfScreen = primitive(_.eraseToEndOfScreen)
    override def flush = primitive(_.flush)
    override def forceBold(a: Boolean) = primitive(_.forceBold(a))
    override def getColumns = primitive(_.getColumns)
    override def getRows = primitive(_.getRows)
    override def homeCursor = primitive(_.homeCursor)
    override def isAutoflushing = primitive(_.isAutoflushing)
    override def isLineWrapping = primitive(_.isLineWrapping)
    override def isSignalling = primitive(_.isSignalling)
    override def moveCursor(a: Int, b: Int) = primitive(_.moveCursor(a, b))
    override def moveDown(a: Int) = primitive(_.moveDown(a))
    override def moveLeft(a: Int) = primitive(_.moveLeft(a))
    override def moveRight(a: Int) = primitive(_.moveRight(a))
    override def moveUp(a: Int) = primitive(_.moveUp(a))
    override def read = primitive(_.read)
    override def resetAttributes = primitive(_.resetAttributes)
    override def resetTerminal = primitive(_.resetTerminal)
    override def restoreCursor = primitive(_.restoreCursor)
    override def setAutoflushing(a: Boolean) = primitive(_.setAutoflushing(a))
    override def setBackgroundColor(a: Int) = primitive(_.setBackgroundColor(a))
    override def setBlink(a: Boolean) = primitive(_.setBlink(a))
    override def setBold(a: Boolean) = primitive(_.setBold(a))
    override def setCursor(a: Int, b: Int) = primitive(_.setCursor(a, b))
    override def setDefaultTerminal = primitive(_.setDefaultTerminal)
    override def setForegroundColor(a: Int) = primitive(_.setForegroundColor(a))
    override def setItalic(a: Boolean) = primitive(_.setItalic(a))
    override def setLinewrapping(a: Boolean) = primitive(_.setLinewrapping(a))
    override def setSignalling(a: Boolean) = primitive(_.setSignalling(a))
    override def setTerminal(a: String) = primitive(_.setTerminal(a))
    override def setUnderlined(a: Boolean) = primitive(_.setUnderlined(a))
    override def storeCursor = primitive(_.storeCursor)
    override def write(a: Byte) = primitive(_.write(a))
    override def write(a: Char) = primitive(_.write(a))
    override def write(a: String) = primitive(_.write(a))

  }

  trait TelnetDInterpreter extends TelnetDOp.Visitor[Kleisli[M, TelnetD, ?]] {

    // common operations delegate to outer interpeter
    override def raw[A](f: TelnetD => A): Kleisli[M, TelnetD, A] = outer.raw(f)
    override def embed[A](e: Embedded[A]): Kleisli[M, TelnetD, A] = outer.embed(e)
    override def delay[A](a: () => A): Kleisli[M, TelnetD, A] = outer.delay(a)
    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): Kleisli[M, TelnetD, A] = outer.async(k)

    // for asyncF we must call ourself recursively
    override def asyncF[A](k: (Either[Throwable, A] => Unit) => TelnetDIO[Unit]): Kleisli[M, TelnetD, A] =
      Kleisli(j => asyncM.asyncF(k.andThen(_.foldMap(this).run(j))))

    // for handleErrorWith we must call ourself recursively
    override def handleErrorWith[A](fa: TelnetDIO[A], f: Throwable => TelnetDIO[A]): Kleisli[M, TelnetD, A] =
      Kleisli { j =>
        val faʹ = fa.foldMap(this).run(j)
        val fʹ  = f.andThen(_.foldMap(this).run(j))
        asyncM.handleErrorWith(faʹ)(fʹ)
      }

    def bracketCase[A, B](acquire: TelnetDIO[A])(use: A => TelnetDIO[B])(release: (A, ExitCase[Throwable]) => TelnetDIO[Unit]): Kleisli[M, TelnetD, B] =
      Kleisli(j => asyncM.bracketCase(acquire.foldMap(this).run(j))(use.andThen(_.foldMap(this).run(j)))((a, e) => release(a, e).foldMap(this).run(j)))

    val shift: Kleisli[M, TelnetD, Unit] =
      Kleisli(j => contextShiftM.shift)

    def evalOn[A](ec: ExecutionContext)(fa: TelnetDIO[A]): Kleisli[M, TelnetD, A] =
      Kleisli(j => contextShiftM.evalOn(ec)(fa.foldMap(this).run(j)))

    // domain-specific operations are implemented in terms of `primitive`
    override def getPortListener(a: String) = primitive(_.getPortListener(a))
    override def prepareListener(a: String, b: Properties) = primitive(_.prepareListener(a, b))
    override def start = primitive(_.start)
    override def stop = primitive(_.stop)

  }


}

