package edu.depauw.scales

import scala.meta._

object Utils {
  val desugar = new Transformer {
    override def apply(tree: Tree): Tree = tree match {
      // Replace infix operators with method calls
      case Term.ApplyInfix(lhs, op, Nil, args) => {
        if (op.value.endsWith(":")) {
          // TODO this is not quite correct for call-by-value op
          apply(Term.Apply(Term.Select(args.head, op), List(lhs)))
        } else {
          apply(Term.Apply(Term.Select(lhs, op), args))
        }
      }
      // Replace unary operators with method calls
      case Term.ApplyUnary(Term.Name(op), arg) => {
        apply(Term.Apply(Term.Select(arg, Term.Name("unary_" + op)), Nil))
      }
      // Replace string interpolation with method calls
      case Term.Interpolate(nm, lits, args) => {
        apply(Term.Apply(Term.Select(Term.New(Init(Type.Name("StringContext"), Name.Anonymous(), List(lits))), nm), args))
      }
      // Replace for-yield loops with method calls
      case Term.ForYield(enums, body) => {
        val Enumerator.Generator(gpat, gterm) :: es = mapRefutable(enums) // MatchError shouldn't happen
        apply(collectForYield(gpat, gterm, es, body))
      }
      // Replace partial functions with matches -- need types for this?
      case Term.PartialFunction(cases) => {
        val x = Term.fresh("fresh$")
        val defCases = (cases.map {
            case Case(pat, cond, _) => Case(pat, cond, Lit.Boolean(true))
          }) :+ Case(Pat.Wildcard(), None, Lit.Boolean(false))
        apply(Term.NewAnonymous(Template(Nil, Nil, Self(Name("this"), None), List(
          Defn.Def(Nil, Term.Name("apply"), Nil, List(List(Term.Param(Nil, x, None, None))), None,
            Term.Match(x, cases)
          ),
          Defn.Def(Nil, Term.Name("isDefinedAt"), Nil, List(List(Term.Param(Nil, x, None, None))), None,
            Term.Match(x, defCases)
          )
        ))))
      }
      // Replace val pattern bindings (in blocks) with matches
      case Term.Block(stats) if stats.exists {
          case Defn.Val(_, List(_ : Pat.Var), _, _) => false
          case Defn.Val(_, _, _, _) => true
          case _ => false
        } => {
        apply(Term.Block(stats.foldRight(List[Stat]()) {
          case (vardef @ Defn.Val(_, List(_ : Pat.Var), _, rhs), body) =>
            vardef :: body
          case (Defn.Val(_, List(pat), _, rhs), body) =>
            List(Term.Match(rhs, List(Case(pat, None, Term.Block(body)))))
          case (stat, body) =>
            stat :: body
        }))
      }
      // Replace pattern matching with conditionals, options, extractors, ...
      case Term.Match(v, case0 :: cases) => {
        val t1 = cases.foldLeft(expandMatch(v, case0)) {
          case (t, c) => q"$t.orElse(${expandMatch(v, c)})"
        }
        apply(q"$t1.getOrElse(throw new MatchError($v))")
      }
      case _ => super.apply(tree)
    }
  }

  def mapRefutable(enums: List[Enumerator]): List[Enumerator] = enums map {
    case Enumerator.Generator(pat, term) if isRefutable(pat) =>
      val cs = List(
        p"case $pat => true",
        p"case _ => false"
      )
      val t2 = q"$term.withFilter { ..case $cs }"
      Enumerator.Generator(pat, t2)
    case e => e
  }

  def isRefutable(pat: Pat): Boolean = pat match {
    // TODO really need to know the expected type to do this properly
    case Pat.Var(_) => false
    case _: Pat.Wildcard => false
    case Pat.Tuple(pats) => pats.exists(isRefutable)
    case Pat.Bind(_, p) => isRefutable(p)
    case _ => true // this is overly pessimistic, e.g., on "x : T" where T is known to include type being matched
  }

  def collectForYield(gpat: Pat, gterm: Term, enums: List[Enumerator], body: Term): Term = enums match {
    case Nil => 
      val cs = List(p"case $gpat => $body")
      q"$gterm.map { ..case $cs }"
    case Enumerator.Generator(pat, term) :: rest =>
      val rhs = collectForYield(pat, term, rest, body)
      val cs = List(p"case $gpat => $rhs")
      q"$gterm.flatMap { ..case $cs }"
    case Enumerator.Guard(test) :: rest =>
      val cs = List(p"case $gpat => $test")
      val gt2 = q"$gterm.withFilter { ..case $cs }"
      collectForYield(gpat, gt2, rest, body)
    case Enumerator.Val(pat, term) :: rest =>
      val x = Term.fresh("fresh$")
      val x2 = Term.fresh("fresh$")
      val b2 = Term.Block(List(
        Defn.Val(Nil, List(Pat.Bind(Pat.Var(x2), pat)), None, term),
        Term.Tuple(List(x, x2))
      ))
      val p2 = Pat.Tuple(List(gpat, pat))
      val t2 = collectForYield(Pat.Bind(Pat.Var(x), gpat), gterm, Nil, b2)
      collectForYield(p2, t2, rest, body)
  }

  def expandMatch(v: Term, c: Case): Term = {
    def aux(t: Term, pats: List[Pat], body: Term): List[Stat] = pats match {
      case Nil => List(q"Some($body)")
      case (x @ Pat.Var(_)) :: rest => q"val $x = $t" :: aux(t, rest, body)
      case Pat.Bind(x @ Pat.Var(_), p) :: rest => q"val $x = $t" :: aux(t, p :: rest, body)
      case (lit : Lit) :: rest => List(q"if ($t != $lit) None else { ..${aux(t, rest, body)} }")
      // TODO: tuples, extractors, wildcards, typed, interpolations? alternatives?
      case _ :: rest => List(q"???")
    }

    Term.Block(aux(v, List(c.pat), c.body))
  }
}