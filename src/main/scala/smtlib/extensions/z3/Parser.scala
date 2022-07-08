package smtlib
package extensions.z3

import smtlib.common.Position
import smtlib.lexer.{Tokens => LT}
import smtlib.trees.Terms._
import smtlib.trees.Commands._

object Tokens {
  import LT.ReservedWord

  case object DefineConst extends ReservedWord

}

class Lexer(reader: java.io.Reader) extends lexer.Lexer(reader) {
  import LT.Token

  override protected def toReserved(s: String): Option[Token] = s match {
    case "define-const" => Some(Token(Tokens.DefineConst))
    case _ => super.toReserved(s)
  }

}

class Parser(lexer: Lexer) extends parser.Parser(lexer) {
  import Terms._

  override protected def parseCommandWithoutParens: Command = getPeekToken.kind match {
    case LT.Push =>
      eat(LT.Push)
      peekToken match {
        case t@LT.NumeralLit(n) =>
          eat(t)
          Push(n.toInt)
        case _ =>
          Push(1)
      }
    case LT.Pop =>
      eat(LT.Pop)
      peekToken match {
        case t@LT.NumeralLit(n) =>
          eat(t)
          Pop(n.toInt)
        case _ =>
          Pop(1)
      }
    case Tokens.DefineConst =>
      eat(Tokens.DefineConst)
      val symbol = parseSymbol
      val sort = parseSort
      val value = parseTerm
      DefineConst(symbol, sort, value)
    case _ => super.parseCommandWithoutParens
  }
}
