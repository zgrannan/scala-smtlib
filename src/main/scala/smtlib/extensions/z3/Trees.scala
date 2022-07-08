package smtlib.extensions.z3

import smtlib.printer.PrintingContext
import smtlib.trees.Commands.CommandExtension
import smtlib.trees.Terms.{SSymbol, Sort, Term, TermExtension}
import smtlib.trees.{Commands, TreeTransformer}

object Terms {
  case class DefineConst(symbol: SSymbol, sort: Sort, value: Term) extends CommandExtension {
    override def print(ctx: PrintingContext): Unit = {
      ctx.print("(define-const ")
      ctx.print(symbol)
      ctx.print(" ")
      ctx.print(sort)
      ctx.print(" ")
      ctx.print(value)
      ctx.print(")")
    }

    override def transform(tt: TreeTransformer)(context: tt.C): (Commands.Command, tt.R) = {
      val (newSym, symRes) = tt.transformSymbol(symbol, context)
      val (newSort, sortRes) = tt.transform(sort, context)
      val (newValue, valueRes) = tt.transform(value, context)
      DefineConst(newSym, newSort, newValue) -> tt.combine(this, context, List(symRes, sortRes, valueRes))
    }
  }

}
