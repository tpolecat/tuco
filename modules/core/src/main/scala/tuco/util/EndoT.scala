package tuco.util

import scalaz._, Scalaz._

// N.B this is Endomorphism[Kleisli[F, ?, ?], ?]

/** Abstraction over functions of the shape `A => F[A]`. */
case class EndoT[F[_], A](run: A => F[A]) {

  def apply(a: A): F[A] =
    run(a)

  def andThen(e: EndoT[F, A])(implicit ev: Bind[F]): EndoT[F, A] =
    e.compose(this)

  def compose(e: EndoT[F, A])(implicit ev: Bind[F]): EndoT[F, A] =
    EndoT(a => e.run(a).flatMap(run))

  def xmap[B](f: A => B, g: B => A)(implicit ev: Functor[F]): EndoT[F, B] =
    EndoT(b => run(g(b)).map(f))

  def trans[G[_]](t: F ~> G): EndoT[G, A] =
    EndoT(a => t(run(a)))

  def zip[B](eb: EndoT[F, B])(implicit ev: Zip[F]): EndoT[F, (A, B)] =
    EndoT { case (a, b) => run(a) fzip eb.run(b) }

  def out[B](l: B @> A)(implicit ev: Functor[F]): EndoT[F, B] =
    EndoT(b => run(l.get(b)).map(l.set(b, _)))

}

object EndoT {

  /** Identity EndoT. */
  def id[F[_]: Applicative, A]: EndoT[F, A] =
    lift(identity[A])

  /** EndoT with a constant effect. */
  def effect[F[_]: Functor, A](f: F[_]): EndoT[F, A] =
    EndoT(f.as(_))

  /** EndoT with a constant value and effect. */
  def const[F[_], A](fa: F[A]): EndoT[F, A] =
    EndoT(_ => fa)

  /** Lift a pure function into EndoT. */
  def lift[F[_]: Applicative, A](f: A => A): EndoT[F, A] =
    EndoT(f.map(_.point[F]))

  /** Lift a pure Endo into EndoT. */
  def liftA[F[_]: Applicative, A](ea: Endo[A]): EndoT[F, A] =
    lift(ea.run)

  /** EndoT is a Monoid if F is a Monad. */
  implicit def EndoTMonoid[F[_]: Monad, A]: Monoid[EndoT[F, A]] =
    new Monoid[EndoT[F, A]] {
      val zero = EndoT.id[F, A]
      def append(a: EndoT[F, A], b: => EndoT[F, A]) = a compose b
    }

  /** EndoT is an InvariantFunctor if F is a Functor. */
  implicit def EndoTInvariant[F[_]: Functor]: InvariantFunctor[EndoT[F, ?]] =
    new InvariantFunctor[EndoT[F, ?]] {
      def xmap[A, B](ma: EndoT[F,A], f: A => B, g: B => A): EndoT[F,B] =
        ma.xmap(f, g)
    }

  /** EndoT is a Zip if F is a Zip. */
  implicit def EndoTZip[F[_]: Zip]: Zip[EndoT[F, ?]] =
    new Zip[EndoT[F, ?]] {
      def zip[A, B](a: => EndoT[F, A], b: => EndoT[F, B]): EndoT[F, (A, B)] = a zip b
    }

}
