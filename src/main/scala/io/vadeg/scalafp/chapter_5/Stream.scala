package io.vadeg.scalafp.chapter_5

sealed trait Stream[+A] {

  import Stream._

  /*
    Ex 5.1
   */
  def toList: List[A] =
    this match {
      case Empty => Nil
      case Cons(h, t) => h() +: t().toList
    }

  /*
    Ex 5.2
   */
  def take(n: Int): Stream[A] =
    this match {
      case Empty => this
      case Cons(h, t) if n > 1 => Cons(h, () => t().take(n - 1))
      case Cons(h, _) if n == 1 => Cons(h, () => Empty)
    }

  /*
    Ex 5.3
   */
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Empty => this
      case Cons(h, t) =>
        lazy val lh = h()
        if (p(lh)) {
          Cons(() => lh, () => t().takeWhile(p))
        } else Empty
    }

  /*
    Ex 5.4
   */
  def forAll(p: A => Boolean): Boolean =
    this match {
      case Empty => true
      case Cons(h, t) => p(h()) && t().forAll(p)
      case _ => false
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  /*
    Ex 5.5
   */
  def takeWhile_(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => {
      if (p(a)) {
        cons(a, b)
      } else Empty
    })

  def headOpt(): Option[A] =
    foldRight[Option[A]](None)((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => Cons(() => f(a), () => b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => {
      if (f(a)) {
        cons(a, b)
      } else b
    })

  def append_1[B >: A](f: => B): Stream[B] =
    foldRight(empty[B])((a, b) => b match {
      case Empty => cons(a, cons(f, Empty))
      case Cons(_, _) => cons(a, b)
    })

  def append[B >: A](f: => Stream[B]): Stream[B] =
    foldRight(f)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /**
    * Ex 5.8
    */
  def constant[A](a: A): Stream[A] = {
    lazy val c: Stream[A] = Stream.cons(a, c)
    c
  }
}

object StreamOps {
  /**
    * Ex 5.9
    */
  def from(n: Int): Stream[Int] =
    Stream.cons[Int](n, from(n + 1))


}

object StreamTest extends App {

  val s = Stream(1, 2, 3, 4, 5)
  val result = s.takeWhile_(_ < 4).toList
  println(s"Result: $result")

  val r1 = s.headOpt()
  println(s"headOpt: $r1")

  val r2 = Stream().headOpt()
  println(s"headOpt: $r2")

  val mapR = s.map(_ + 1)
  println(s"map: ${mapR.toList}")

  val filterR = s.filter(_ > 3)
  println(s"filter: ${filterR.toList}")

  val appendR = s.append_1(6)
  println(s"append: ${appendR.toList}")

  val appendStreamR = s.append(Stream(7, 8, 9))
  println(s"appendStream: ${appendStreamR.toList}")

  val flatMapR = s.flatMap(_ => Stream(7, 8, 9))
  println(s"flatMap: ${flatMapR.toList}")

  val constantR = Stream.constant(10).take(5).toList
  println(s"constant: $constantR")

  val fromR = StreamOps.from(5).take(5).toList
  println(s"from(5): $fromR")


}