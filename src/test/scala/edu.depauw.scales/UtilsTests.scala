package edu.depauw.scales

import utest._

import scala.meta._

object UtilsTests extends TestSuite {
  val tests = Tests {
    def runTest(src: String, expected: String) = {
      val tree = src.parse[Source].get
      val utree = Utils.desugar(tree)
      assert(expected == utree.toString)
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
  }
}