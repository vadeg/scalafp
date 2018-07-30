package io.vadeg.scalafp.chapter_6

trait RNG {

  def nextInt(): (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  override def nextInt(): (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {

  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, rng1) = rng.nextInt()
    if (v > 0) {
      (v, rng1)
    } else nonNegativeInt(rng1)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (v, rng1) = rng.nextInt()
    if (v == Int.MaxValue) {
      double(rng1)
    } else (v.toDouble / Int.MaxValue, rng1)
  }

  def double_1: Rand[Double] = {
    map(nonNegativeInt)(_.toDouble / Int.MaxValue)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intV, r) = nonNegativeInt(rng)
    val d = double(r)
    ((intV, d._1), d._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (t, r) = intDouble(rng)
    (t.swap, r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (i, nextRng) = nonNegativeInt(rng)
    if (count > 0) {
      val t = ints(count - 1)(nextRng)
      (t._1 :+ i, t._2)
    } else (List(i), nextRng)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
}

object Chapter6 extends App {

  private val rng1 = SimpleRNG(10)
  val (n, rng2) = rng1.nextInt()
  val (v, _) = rng2.nextInt()
  println(s"n: $n; v: $v")

  val (nonNegativeInt, _) = SimpleRNG.nonNegativeInt(rng1)
  println(s"nnInt: $nonNegativeInt")

  val (d1, rngd2) = SimpleRNG.double(rng1)
  println(s"double1: $d1")
  val d2 = SimpleRNG.double_1
  println(s"double2: ${d2(rngd2)._1}")

  val ((id1, id2), _) = SimpleRNG.intDouble(rng1)
  println(s"int: $id1, double: $id2")

  val ((di1, di2), _) = SimpleRNG.doubleInt(rng1)
  println(s"int: $id1, double: $id2")

  val ((dd1, dd2, dd3), _) = SimpleRNG.double3(rng1)
  println(s"d1: $dd1, d2: $dd2, d3: $dd3")

  val (l, _) = SimpleRNG.ints(10)(rng1)
  println(s"list: $l")

}
