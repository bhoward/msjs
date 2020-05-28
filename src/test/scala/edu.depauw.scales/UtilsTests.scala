package edu.depauw.scales

import utest._

import scala.meta._

object UtilsTests extends TestSuite {
  val tests = Tests {
    def runTest(src: String, expected: String) = {
      val stree = src.parse[Source].get
      val dtree = Utils.desugar(stree)
      val found = dtree.syntax.replaceAll("\\s+", " ")
      val xpect = expected.replaceAll("\\s+", " ")
      assert(xpect == found)
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
              "object a { val b = c.unary_+.+(d).+ }")
    }

    test("string interpolation") {
      runTest("object a { val b = s\"hello $c world ${d+e}!\" }",
              "object a { val b = new StringContext(\"hello \", \" world \", \"!\").s(c, d.+(e)) }")
    }

    test("for comprehension") {
      runTest("object a { val b = for (c <- d) yield e }",
              """object a { val b = d.map(new { this =>
                |  def apply(fresh$1) = { val c = fresh$1
                |    Some(e) }.getOrElse(throw new MatchError(fresh$1))
                |  def isDefinedAt(fresh$1) = { val c = fresh$1
                |    Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$1))
                |}) }""".stripMargin)
      runTest("object a { val b = for (c <- d; e <- f) yield (c * e) }",
              """object a { val b = d.flatMap(new { this =>
                |  def apply(fresh$2) = { val c = fresh$2
                |    Some(f.map(new { this =>
                |      def apply(fresh$3) = { val e = fresh$3
                |        Some(c.*(e)) }.getOrElse(throw new MatchError(fresh$3))
                |      def isDefinedAt(fresh$3) = { val e = fresh$3
                |        Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$3)) })) }.getOrElse(throw new MatchError(fresh$2))
                |  def isDefinedAt(fresh$2) = { val c = fresh$2
                |    Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$2))
                |}) }""".stripMargin)
      runTest("object a { val b = for (c <- d + e; f <- g * h) yield (c % f) }",
              """object a { val b = d.+(e).flatMap(new { this =>
                |  def apply(fresh$4) = { val c = fresh$4
                |    Some(g.*(h).map(new { this =>
                |      def apply(fresh$5) = { val f = fresh$5
                |        Some(c.%(f)) }.getOrElse(throw new MatchError(fresh$5))
                |      def isDefinedAt(fresh$5) = { val f = fresh$5
                |        Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$5)) })) }.getOrElse(throw new MatchError(fresh$4))
                |  def isDefinedAt(fresh$4) = { val c = fresh$4
                |    Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$4))
                |}) }""".stripMargin)
    }

    test("for comprehension guard") {
      runTest("object a { val b = for (c <- d; if e) yield f }",
              """object a { val b = d.withFilter(new { this =>
                |  def apply(fresh$6) = { val c = fresh$6
                |    Some(e) }.getOrElse(throw new MatchError(fresh$6))
                |  def isDefinedAt(fresh$6) = { val c = fresh$6
                |    Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$6)) }).map(new { this =>
                |      def apply(fresh$7) = { val c = fresh$7
                |        Some(f) }.getOrElse(throw new MatchError(fresh$7))
                |      def isDefinedAt(fresh$7) = { val c = fresh$7
                |        Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$7))
                |}) }""".stripMargin)
      // Example from the spec
      val src = """object a {
                  |  for  { i <- 1 until n
                  |         j <- 1 until i
                  |         if isPrime(i+j)
                  |  } yield (i, j)
                  |}""".stripMargin
      val out = """object a { 1.until(n).flatMap(new { this =>
                  |  def apply(fresh$8) = { val i = fresh$8
                  |    Some(1.until(i).withFilter(new { this =>
                  |      def apply(fresh$9) = { val j = fresh$9
                  |        Some(isPrime(i.+(j))) }.getOrElse(throw new MatchError(fresh$9))
                  |      def isDefinedAt(fresh$9) = { val j = fresh$9
                  |        Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$9)) }).map(new { this =>
                  |          def apply(fresh$10) = { val j = fresh$10
                  |            Some((i, j)) }.getOrElse(throw new MatchError(fresh$10))
                  |          def isDefinedAt(fresh$10) = { val j = fresh$10
                  |            Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$10)) })) }.getOrElse(throw new MatchError(fresh$8))
                  |  def isDefinedAt(fresh$8) = { val i = fresh$8
                  |    Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$8))
                  |}) }""".stripMargin
      runTest(src, out)
    }

    test("for comprehension refutable") {
      runTest("object a { val b = for ((c, 0) <- d) yield e }",
              """object a { val b = d.withFilter(new { this =>
                |  def apply(fresh$11) = { val c = fresh$11._1
                |    { val fresh$12 = fresh$11._2
                |      { if (fresh$12.!=(0)) None else { Some { Some(true) } } } }.getOrElse(throw new MatchError(fresh$11._2)) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$11))
                |  def isDefinedAt(fresh$11) = { val c = fresh$11._1
                |    { val fresh$13 = fresh$11._2
                |      { if (fresh$13.!=(0)) None else { Some { Some(true) } } } }.getOrElse(throw new MatchError(fresh$11._2)) }.orElse { Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$11)) }).map(new { this =>
                |      def apply(fresh$14) = { val c = fresh$14._1
                |        { val fresh$15 = fresh$14._2
                |          { if (fresh$15.!=(0)) None else { Some { Some(e) } } } }.getOrElse(throw new MatchError(fresh$14._2)) }.getOrElse(throw new MatchError(fresh$14))
                |      def isDefinedAt(fresh$14) = { val c = fresh$14._1
                |        { val fresh$16 = fresh$14._2
                |          { if (fresh$16.!=(0)) None else { Some { Some(true) } } } }.getOrElse(throw new MatchError(fresh$14._2)) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$14))
                |}) }""".stripMargin)
    }

    test("for comprehension value") {
      runTest("object a { val b = for (c <- d; e = f) yield g}",
              """object a { val b = d.map(new { this =>
                |  def apply(fresh$19) = { val fresh$17 = fresh$19
                |    val c = fresh$19
                |    Some { { val fresh$18 = f
                |      val e = f
                |      Some { (fresh$17, fresh$18) } }.getOrElse(throw new MatchError(f)) } }.getOrElse(throw new MatchError(fresh$19))
                |  def isDefinedAt(fresh$19) = { val fresh$17 = fresh$19
                |    val c = fresh$19
                |    Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$19)) }).map(new { this =>
                |      def apply(fresh$20) = { val c = fresh$20._1
                |        val e = fresh$20._2
                |        Some(g) }.getOrElse(throw new MatchError(fresh$20))
                |      def isDefinedAt(fresh$20) = { val c = fresh$20._1
                |        val e = fresh$20._2
                |        Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$20))
                |}) }""".stripMargin)
    }

    test("partial function") {
      runTest("object a { val b = { case c => d; case (e, f) => g } }",
              """object a { val b = new { this =>
                |  def apply(fresh$21) = { val c = fresh$21
                |    Some(d) }.orElse { val e = fresh$21._1
                |      val f = fresh$21._2
                |      Some(g) }.getOrElse(throw new MatchError(fresh$21))
                |  def isDefinedAt(fresh$21) = { val c = fresh$21
                |    Some(true) }.orElse { val e = fresh$21._1
                |      val f = fresh$21._2
                |      Some(true) }.orElse { Some(false) }.getOrElse(throw new MatchError(fresh$21))
                |} }""".stripMargin)
    }

    test("val pattern") {
      runTest("object a { val b = { val _ = 1; val () = (); val (c, d) = e; val F(G(_), h @ I()) = j; k } }",
              """object a {
                |  val b = {
                |    {
                |      val fresh$22 = 1 {
                |        Some {
                |          {
                |            val fresh$23 = () {
                |              if (fresh$23.!=(())) None
                |              else {
                |                Some {
                |                  { val c = e._1
                |                    val d = e._2
                |                    Some {
                |                      {
                |                        F.unapply(j).flatMap(fresh$24 => {
                |                          { val fresh$25 = fresh$24._1
                |                            {
                |                              G.unapply(fresh$25).flatMap(fresh$26 => {
                |                                {
                |                                  Some {
                |                                    Some {
                |                                      { val fresh$27 = fresh$24._2
                |                                        { val h = fresh$27
                |                                          if (I.unapply(fresh$27).unary_!) None
                |                                          else { Some { Some { k } } }
                |                                        }
                |                                      }.getOrElse(throw new MatchError(fresh$24._2))
                |                                    }
                |                                  }
                |                                }.getOrElse(throw new MatchError(fresh$26))
                |                              })
                |                            }
                |                          }.getOrElse(throw new MatchError(fresh$24._1))
                |                        })
                |                      }.getOrElse(throw new MatchError(j))
                |                    }
                |                  }.getOrElse(throw new MatchError(e))
                |                }
                |              }
                |            }
                |          }.getOrElse(throw new MatchError(()))
                |        }
                |      }
                |    }.getOrElse(throw new MatchError(1))
                |  }
                |}""".stripMargin)
    }

    test("match") {
      runTest("""object a { val b = c(d) match {
                |  case e => f(e)
                |  case g @ H => i(g)
                |  case j : K => l(j)
                |  case (m, 0) => s(m)
                |  case N("Hello", O(), P((q, r))) => v(q, r)
                |  case t :: u :: Nil => t + u
                |} }""".stripMargin,
              """object a {
                |  val b = {
                |    val fresh$28 = c(d)
                |    {
                |      val e = fresh$28
                |      Some(f(e))
                |    }.orElse {
                |      val g = fresh$28
                |      if (fresh$28.!=(H)) None else {
                |        Some(i(g))
                |      }
                |    }.orElse {
                |      if (fresh$28.isInstanceOf[K].unary_!) None else {
                |        val j = fresh$28
                |        Some(l(j))
                |      }
                |    }.orElse {
                |      val m = fresh$28._1
                |      {
                |        val fresh$31 = fresh$28._2
                |        {
                |          if (fresh$31.!=(0)) None else {
                |            Some {
                |              Some(s(m))
                |            }
                |          }
                |        }
                |      }.getOrElse(throw new MatchError(fresh$28._2))
                |    }.orElse {
                |      N.unapply(fresh$28).flatMap(fresh$29 => {
                |        {
                |          val fresh$32 = fresh$29._1
                |          {
                |            if (fresh$32.!=("Hello")) None else {
                |              Some {
                |                {
                |                  val fresh$33 = fresh$29._2
                |                  {
                |                    if (O.unapply(fresh$33).unary_!) None else {
                |                      Some {
                |                        {
                |                          val fresh$34 = fresh$29._3
                |                          {
                |                            P.unapply(fresh$34).flatMap(fresh$35 => {
                |                              {
                |                                val q = fresh$35._1
                |                                val r = fresh$35._2
                |                                Some {
                |                                  Some {
                |                                    Some(v(q, r))
                |                                  }
                |                                }
                |                              }.getOrElse(throw new MatchError(fresh$35))
                |                            })
                |                          }
                |                        }.getOrElse(throw new MatchError(fresh$29._3))
                |                      }
                |                    }
                |                  }
                |                }.getOrElse(throw new MatchError(fresh$29._2))
                |              }
                |            }
                |          }
                |        }.getOrElse(throw new MatchError(fresh$29._1))
                |      })
                |    }.orElse {
                |      ::.unapply(fresh$28).flatMap(fresh$30 => {
                |        val t = fresh$30._1
                |        {
                |          val fresh$36 = fresh$30._2
                |          {
                |            ::.unapply(fresh$36).flatMap(fresh$37 => {
                |              val u = fresh$37._1
                |              if (fresh$37._2.!=(Nil)) None else {
                |                Some {
                |                  Some(t.+(u))
                |                }
                |              }
                |            })
                |          }
                |        }.getOrElse(throw new MatchError(fresh$30._2))
                |      })
                |    }
                |  }.getOrElse(throw new MatchError(c(d)))
                |}""".stripMargin)
    }
  }
}