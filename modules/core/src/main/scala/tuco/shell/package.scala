package tuco

import tuco.util.EndoT
import scalaz._, Scalaz._

package object shell {

  implicit class MoreLensOps[A,B](l: A @> B) {
    def endoT[F[_]: Functor](f: B => F[B]): EndoT[F, A] = EndoT[F, B](f).out(l)
    def endoT_[F[_]: Functor](f: B => F[_]): EndoT[F, A] = EndoT[F, B](a => f(a).as(a)).out(l)
    def liftEndoT[F[_]: Applicative](f: B => B): EndoT[F, A] = EndoT.lift[F, B](f).out(l)
  }

}
