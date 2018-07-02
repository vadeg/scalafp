package io.vadeg.scalafp.chapter_3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs: List[A]): List[A] =
    drop(xs, 1)

  def setHead[A](xs: List[A], v: A): List[A] = xs match {
    case Nil => Nil
    case Cons(_, t) => Cons(v, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case x => x
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, t) if f(x) => dropWhile(t)(f)
    case x => x
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, t) => Cons(x, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, y) => f(x, foldRight(y, z)(f))
  }

  def product(as: List[Int]): Int =
    foldRight(as, 1)(_ * _)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, y) => foldLeft(y, f(z, x))(f)
  }

  //  3.12
  def reverse[A](as: List[A]) =
    foldLeft(as, Nil: List[A])((x, y) => Cons(y, x))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  //  3.14 (foldLeft)
  def appendL[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(List.reverse(a1), a2)((acc, l) => Cons(l, acc))

  //  3.14 (foldRight)
  def appendR[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((l, acc) => Cons(l, acc))

  //  3.15 (not sure about time)
  def concat[A](as: List[List[A]]): List[A] =
    foldLeft(as, Nil: List[A])((acc, v) => List.append(acc, v))


}

object Chapter3 {

  def ex32(): Unit = {
    println(s"3.2: [1,2,3] > [2,3]: ${List.tail(List(1, 2, 3))}")
  }

  def ex33(): Unit = {
    println(s"3.3: ${List.setHead(List(1, 2, 3), 4)} should be 4,2,3")
  }

  def ex34() = {
    val r = List.drop(List(1, 2, 3), 2)
    println(s"3.3: [1,2,3](2) - [3]: $r")
  }

}