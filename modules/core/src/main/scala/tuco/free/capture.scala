package tuco.free

trait Capture[F[_]] {
  def apply[A](a: => A): F[A]
}
object Capture{
  def apply[F[_]](implicit ev: Capture[F]) = ev
}
