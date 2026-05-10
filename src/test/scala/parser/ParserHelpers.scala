import Expr.*
import Stmt.*

trait ParserHelpers:

  def parseExpr(source: String): Expr =
    Parser(Scanner(source).scan()).expression()

  def parseProgram(source: String): Seq[Stmt] =
    Parser(Scanner(source).scan()).parse()

  def mkToken(tt: TokenType, lexeme: String, literal: Option[TokenLiteralType] = None, line: Int = 1): Token =
    Token(tt, lexeme, literal, line)
