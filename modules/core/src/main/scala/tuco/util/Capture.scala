package tuco.util

import scalaz.effect.IO

trait Capture[F[_]] {
  def apply[A](a: => A): F[A]
}
object Capture{
  def apply[F[_]](implicit ev: Capture[F]) = ev

  implicit val IOCapture: Capture[IO] =
    new Capture[IO] {
      def apply[A](a: => A) = IO(a)
    }
}
