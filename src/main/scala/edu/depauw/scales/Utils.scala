package edu.depauw.scales

import scala.meta._

object Utils {
  val desugar = new Transformer {
    override def apply(tree: Tree): Tree = tree match {
      case Term.ApplyInfix(lhs, op, Nil, args) =>
        if (op.value.endsWith(":")) {
          apply(Term.Apply(Term.Select(args.head, op), List(lhs)))
        } else {
          apply(Term.Apply(Term.Select(lhs, op), args))
        }
      case Term.ApplyUnary(Term.Name(op), arg) =>
        apply(Term.Apply(Term.Select(arg, Term.Name("unary_" + op)), Nil))
      case Term.Interpolate(nm, lits, args) =>
        apply(Term.Apply(Term.Select(Term.New(Init(Type.Name("StringContext"), Name.Anonymous(), List(lits))), nm), args))
      case _ => super.apply(tree)
    }
  }
}