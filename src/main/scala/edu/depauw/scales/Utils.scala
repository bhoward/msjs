package edu.depauw.scales

import scala.meta._

object Utils {
  val desugar = new Transformer {
    override def apply(tree: Tree): Tree = tree match {
      case Term.ApplyInfix(lhs, op, Nil, args) =>
        if (op.value.endsWith(":")) {
          // TODO this is not quite correct for call-by-value op
          apply(Term.Apply(Term.Select(args.head, op), List(lhs)))
        } else {
          apply(Term.Apply(Term.Select(lhs, op), args))
        }
      case Term.ApplyUnary(Term.Name(op), arg) =>
        apply(Term.Apply(Term.Select(arg, Term.Name("unary_" + op)), Nil))
      case Term.Interpolate(nm, lits, args) =>
        apply(Term.Apply(Term.Select(Term.New(Init(Type.Name("StringContext"), Name.Anonymous(), List(lits))), nm), args))
      case Term.For(enums, body) => {
        val Enumerator.Generator(gpat, gterm) :: es = mapRefutable(enums) // MatchError shouldn't happen
        apply(collectForEach(gpat, gterm, es, body))
      }
      case Term.ForYield(enums, body) => {
        val Enumerator.Generator(gpat, gterm) :: es = mapRefutable(enums) // MatchError shouldn't happen
        apply(collectForYield(gpat, gterm, es, body))
      }
      case _ => super.apply(tree)
    }
  }

  def mapRefutable(enums: List[Enumerator]): List[Enumerator] = enums map {
    case Enumerator.Generator(pat, term) if isRefutable(pat) => ???
    case e => e
  }

  def isRefutable(pat: Pat): Boolean = pat match {
    // TODO really need to know the expected type to do this properly
    case Pat.Var(_) => false
    case _: Pat.Wildcard => false
    case Pat.Typed(_, _) => false
    case Pat.Tuple(pats) => pats.exists(isRefutable)
    case Pat.Bind(_, p) => isRefutable(p)
    case _ => true
  }

  // TODO do we even want this for a functional subset?
  def collectForEach(gpat: Pat, gterm: Term, enums: List[Enumerator], body: Term): Term = ???

  def collectForYield(gpat: Pat, gterm: Term, enums: List[Enumerator], body: Term): Term = enums match {
    case Nil => 
      val cs = List(p"case $gpat => $body")
      q"$gterm.map { ..case $cs }"
    case Enumerator.Generator(pat, term) :: rest =>
      val rhs = collectForYield(pat, term, rest, body)
      val cs = List(p"case $gpat => $rhs")
      q"$gterm.flatMap { ..case $cs }"
    case Enumerator.Guard(test) :: rest =>
      ???
    case Enumerator.Val(pat, term) :: rest =>
      ???
  }
}