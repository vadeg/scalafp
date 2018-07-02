package io.vadeg.scalafp.chapter_4

trait Option[+A] {
  self =>

  //  case
  def map[B](f: A => B): Option[B] = self match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    self.map(f).getOrElse(None)

  //  case
  def getOrElse[B >: A](default: => B): B = self match {
    case Some(x) => x
    case None => default
  }

  def orElse_[B >: A](ob: => Option[B]): Option[B] =
    self.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    self.flatMap { x =>
      if (f(x)) Some(x) else None
    }
}

object Option {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(av => b.map(bv => f(av, bv)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case x :: xs => map2(x, sequence(xs))(_ +: _)
    case Nil => Some(Nil)
  }

  def sequence_[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(f => f)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case x :: xs => map2(f(x), traverse(xs)(f))(_ +: _)
    case Nil => Some(Nil)
  }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Chapter4 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs)
      .map(m => xs.map(x => math.pow(x - m, 2)))
      .flatMap(mean)
  }

  val v1: Either[Int, Int] = Right(5)
  val v2: Either[Int, Int] = Left(10)
  println(v2.map(v => v + 1))

}
