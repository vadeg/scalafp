package io.vadeg.scalafp.chapter_4

import org.scalatest.{FlatSpec, Matchers}

class EitherSpec extends FlatSpec with Matchers {

  "Either" should "map when it is Right" in {
    Right(5).map(_ + 1) shouldBe Right(6)
  }


}
