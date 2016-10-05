package object can extends WimpiConversions {

  type ConnectionIO[A] = can.free.connection.ConnectionIO[A]

  type Listener = Event => ConnectionIO[Unit]



}

trait WimpiConversions {

  import can.{ Event, Listener }
  import can.free.Capture
  import can.free.connection._
  import net.wimpi.telnetd.net._
  import scalaz.effect._
  import can.Event._

  implicit val CaptureIO: Capture[IO] =
    new Capture[IO] {
      def apply[A](a: => A) = IO(a)
    }

  implicit def liftListener(a: Listener): ConnectionListener =
    new ConnectionListener {

      // We delegate every event to our listener
      def connectionIdle(ce: ConnectionEvent)          = go(ce, Idle)
      def connectionLogoutRequest(ce: ConnectionEvent) = go(ce, LogoutRequest)
      def connectionSentBreak(ce: ConnectionEvent)     = go(ce, Break)
      def connectionTimedOut(ce: ConnectionEvent)      = go(ce, TimedOut)

      // Because it's a callback it's the end of the world
      def go(ce: ConnectionEvent, e: Event): Unit = {
        println("** " + e)
        a(e).transK[IO].run(ce.getConnection).unsafePerformIO
      }

    }

}
