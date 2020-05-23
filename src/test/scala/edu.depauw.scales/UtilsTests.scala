package edu.depauw.scales

import utest._

import scala.meta._

object UtilsTests extends TestSuite {
  val tests = Tests {
    def runTest(src: String, expected: String) = {
      val tree = src.parse[Source].get
      val utree = Utils.desugar(tree)
      assert(expected == utree.syntax)
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
              "object a {\n  val b = d.map({\n    case c => e\n  })\n}")
      runTest("object a { val b = for (c <- d; e <- f) yield (c * e) }",
              "object a {\n  val b = d.flatMap({\n    case c =>\n      f.map({\n        case e =>\n          c.*(e)\n      })\n  })\n}")
      runTest("object a { val b = for (c <- d + e; f <- g * h) yield (c % f) }",
              "object a {\n  val b = d.+(e).flatMap({\n    case c =>\n      g.*(h).map({\n        case f =>\n          c.%(f)\n      })\n  })\n}")
    }
  }
}