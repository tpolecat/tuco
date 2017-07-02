package tuco.util

import cats.Functor
import cats.data.State
import cats.implicits._

case class Lens[A, B](get: A => B, set: (A, B) => A) {

  def andThen[C](bc: Lens[B, C]): Lens[A, C] =
    Lens(a => bc.get(get(a)), (a, c) => set(a, bc.set(get(a), c)))

  def >=>[C](bc: Lens[B, C]): Lens[A, C] =
    andThen(bc)

  def compose[C](bc: Lens[C, A]): Lens[C, B] =
    bc andThen this

  def modify(a: A, f: B => B): A =
    set(a, f(get(a)))

  def xmapA[AA](f: AA => A, g: A => AA): Lens[AA, B] =
    Lens(aa => get(f(aa)), (aa, b) => g(set(f(aa), b)))

  def xmapB[BB](f: BB => B, g: B => BB): Lens[A, BB] =
    Lens(a => g(get(a)), (a, bb) => set(a, f(bb)))

  def =>>=[F[_]: Functor](f: B => F[B]): A => F[A] =
    a => f(get(a)).map(set(a, _))

  def mods(f: B => B): State[A, B] =
    State { a =>
      val b  = get(a)
      val bʹ = f(b)
      val aʹ = set(a, bʹ)
      (aʹ, bʹ)
    }

  def %=(f: B => B): State[A, B] =
    mods(f)

  def :=(b: => B): State[A, B] =
    mods(_ => b)

}

object Lens {

  type @>[A,B] = Lens[A,B]

  def lensu[A, B](set: (A, B) => A, get: A => B): Lens[A, B] =
    Lens(get, set)

}
