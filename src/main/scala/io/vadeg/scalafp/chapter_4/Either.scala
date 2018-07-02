package io.vadeg.scalafp.chapter_4

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case x@Left(_) => x
  }

  def mapL[B](f: E => B): Either[B, A] = this match {
    case Left(v) => Left(f(v))
    case x@Right(_) => x
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case x@Left(_) => x
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case x@Right(_) => x
    case _@Left(_) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    b.flatMap(vb => this.map(va => f(va, vb)))
}

object Either {

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case x :: xs => f(x).map2(traverse(xs)(f))(_ +: _)
      case Nil => Right(Nil)
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]