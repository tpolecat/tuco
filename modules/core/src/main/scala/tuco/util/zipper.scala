package tuco.util

import cats.data.NonEmptyList

/** A very simple zipper for the implementation. No instances because we don't need them. */
case class Zipper[A](lefts: List[A], focus: A, rights: List[A]) {

  def previous: Option[Zipper[A]] =
    lefts match {
      case Nil     => None
      case a :: as => Some(Zipper(as, a, focus :: rights))
    }

  def next: Option[Zipper[A]] =
    rights match {
      case Nil     => None
      case a :: as => Some(Zipper(focus :: lefts, a, as))
    }

}

object Zipper {
  def single[A](a: A): Zipper[A] = Zipper(Nil, a, Nil)
  def fromNonEmptyList[A](nel: NonEmptyList[A]): Zipper[A] = Zipper(Nil, nel.head, nel.tail)
}
