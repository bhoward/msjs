package edu.depauw.scales

import utest._

import scala.meta._

object UtilsTests extends TestSuite {
  val tests = Tests {
    def runTest(src: String, expected: String) = {
      val stree = src.parse[Source].get
      val dtree = Utils.desugar(stree)
      val etree = expected.parse[Source].get
      assert(etree.structure == dtree.structure)
    }

    test("binary operator") {
      runTest("object a { val b = c + d * e }",
              "object a { val b = c.+(d.*(e)) }")
    }

    test("right-associative binary operator") {
      runTest("object a { val b = c :: d :: e }",
              "object a { val b = e.::(d).::(c) }")
    }

    test("unary operator") {
      runTest("object a { val b = + c + d + }",
              "object a { val b = c.unary_+().+(d).+ }")
    }

    test("string interpolation") {
      runTest("object a { val b = s\"hello $c world ${d+e}!\" }",
              "object a { val b = new StringContext(\"hello \", \" world \", \"!\").s(c, d.+(e)) }")
    }

    test("for comprehension") {
      runTest("object a { val b = for (c <- d) yield e }",
              "object a { val b = d.map({ case c => e }) }")
      runTest("object a { val b = for (c <- d; e <- f) yield (c * e) }",
              "object a { val b = d.flatMap({ case c => f.map({ case e => c.*(e) }) }) }")
      runTest("object a { val b = for (c <- d + e; f <- g * h) yield (c % f) }",
              "object a { val b = d.+(e).flatMap({ case c => g.*(h).map({ case f => c.%(f) }) }) }")
    }

    test("for comprehension guard") {
      runTest("object a { val b = for (c <- d; if e) yield f }",
              "object a { val b = d.withFilter({ case c => e }).map({ case c => f}) }")
      // Example from the spec
      val src = """object a {
                  |  for  { i <- 1 until n
                  |         j <- 1 until i
                  |         if isPrime(i+j)
                  |  } yield (i, j)
                  |}""".stripMargin
      val out = "object a { 1.until(n).flatMap({ case i => 1.until(i).withFilter({ case j => isPrime(i.+(j)) }).map({ case j => (i, j) }) }) }"
      runTest(src, out)
    }
  }
}