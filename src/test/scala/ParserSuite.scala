import Expr.*
import scala.collection.mutable.ArrayBuffer

class ParserSuite extends munit.FunSuite:

  // Helper: scan source and parse a single expression
  private def parseExpr(source: String): Expr =
    val tokens = ArrayBuffer.from(Scanner(source).scan())
    Parser(tokens).expression()

  // Helper: scan source and call parse() (top-level entry point)
  private def parseProgram(source: String): Expr =
    val tokens = ArrayBuffer.from(Scanner(source).scan())
    Parser(tokens).parse()

  // Helper: build tokens manually (useful when scanner doesn't support a token)
  private def mkToken(tt: TokenType, lexeme: String, literal: Option[TokenLiteralType] = None, line: Int = 1): Token =
    Token(tt, lexeme, literal, line)

  // ─── Literals ───────────────────────────────────────────────────────

  test("parse number literal"):
    val expr = parseExpr("123")
    assert(expr.isInstanceOf[LiteralExpr])
    assertEquals(expr.asInstanceOf[LiteralExpr].value, Some(123.0))

  test("parse floating point number literal"):
    val expr = parseExpr("3.14")
    assert(expr.isInstanceOf[LiteralExpr])
    assertEquals(expr.asInstanceOf[LiteralExpr].value, Some(3.14))

  test("parse string literal with double quotes"):
    val expr = parseExpr("\"hello\"")
    assert(expr.isInstanceOf[LiteralExpr])
    assertEquals(expr.asInstanceOf[LiteralExpr].value, Some("hello"))

  test("parse string literal with single quotes"):
    val expr = parseExpr("'world'")
    assert(expr.isInstanceOf[LiteralExpr])
    assertEquals(expr.asInstanceOf[LiteralExpr].value, Some("world"))

  test("parse true literal"):
    val expr = parseExpr("true")
    assert(expr.isInstanceOf[LiteralExpr])
    assertEquals(expr.asInstanceOf[LiteralExpr].value, Some(true))

  test("parse false literal"):
    val expr = parseExpr("false")
    assert(expr.isInstanceOf[LiteralExpr])
    assertEquals(expr.asInstanceOf[LiteralExpr].value, Some(false))

  test("parse nil literal"):
    val expr = parseExpr("nil")
    assert(expr.isInstanceOf[LiteralExpr])
    assertEquals(expr.asInstanceOf[LiteralExpr].value, None)

  test("parse empty input returns nil via parse()"):
    val expr = parseProgram("")
    assert(expr.isInstanceOf[LiteralExpr])
    assertEquals(expr.asInstanceOf[LiteralExpr].value, None)

  // ─── Basic binary expression ───────────────────────────────────────

  test("parse 2 + 2"):
    val expr = parseExpr("2 + 2")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.PLUS)
    assertEquals(bin.left.asInstanceOf[LiteralExpr].value, Some(2.0))
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(2.0))

  test("parse subtraction"):
    val expr = parseExpr("10 - 4")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.MINUS)
    assertEquals(bin.left.asInstanceOf[LiteralExpr].value, Some(10.0))
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(4.0))

  test("parse division"):
    val expr = parseExpr("6 / 3")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.SLASH)
    assertEquals(bin.left.asInstanceOf[LiteralExpr].value, Some(6.0))
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(3.0))

  // ─── Multiplication via manually constructed tokens ─────────────────
  // The scanner doesn't produce STAR tokens yet, so we build them by hand

  test("parse multiplication with manual tokens"):
    val tokens = ArrayBuffer(
      mkToken(TokenType.NUMBER, "2", Some(2.0)),
      mkToken(TokenType.STAR, "*"),
      mkToken(TokenType.NUMBER, "3", Some(3.0)),
      mkToken(TokenType.EOF, ""),
    )
    val expr = Parser(tokens).expression()
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.STAR)
    assertEquals(bin.left.asInstanceOf[LiteralExpr].value, Some(2.0))
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(3.0))

  // ─── Comparison operators ───────────────────────────────────────────

  test("parse less than"):
    val expr = parseExpr("1 < 2")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.LESS)

  test("parse less than or equal"):
    val expr = parseExpr("1 <= 2")
    assert(expr.isInstanceOf[BinaryExpr])
    assertEquals(expr.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.LESS_EQUAL)

  test("parse greater than"):
    val expr = parseExpr("3 > 1")
    assert(expr.isInstanceOf[BinaryExpr])
    assertEquals(expr.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.GREATER)

  test("parse greater than or equal"):
    val expr = parseExpr("3 >= 1")
    assert(expr.isInstanceOf[BinaryExpr])
    assertEquals(expr.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.GREATER_EQUAL)

  // ─── Equality operators ─────────────────────────────────────────────

  test("parse equal equal"):
    val expr = parseExpr("1 == 1")
    assert(expr.isInstanceOf[BinaryExpr])
    assertEquals(expr.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.EQUAL_EQUAL)

  test("parse bang equal"):
    val expr = parseExpr("1 != 2")
    assert(expr.isInstanceOf[BinaryExpr])
    assertEquals(expr.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.BANG_EQUAL)

  // ─── Grouping expressions ──────────────────────────────────────────

  test("parse simple grouping"):
    val expr = parseExpr("(2 + 2)")
    assert(expr.isInstanceOf[GroupingExpr])
    val inner = expr.asInstanceOf[GroupingExpr].expression
    assert(inner.isInstanceOf[BinaryExpr])
    val bin = inner.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.PLUS)
    assertEquals(bin.left.asInstanceOf[LiteralExpr].value, Some(2.0))
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(2.0))

  test("parse nested groupings"):
    val expr = parseExpr("((1 + 2))")
    assert(expr.isInstanceOf[GroupingExpr])
    val inner1 = expr.asInstanceOf[GroupingExpr].expression
    assert(inner1.isInstanceOf[GroupingExpr])
    val inner2 = inner1.asInstanceOf[GroupingExpr].expression
    assert(inner2.isInstanceOf[BinaryExpr])
    assertEquals(inner2.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.PLUS)

  test("parse deeply nested grouping around literal"):
    val expr = parseExpr("(((1)))")
    assert(expr.isInstanceOf[GroupingExpr])
    val g1 = expr.asInstanceOf[GroupingExpr].expression
    assert(g1.isInstanceOf[GroupingExpr])
    val g2 = g1.asInstanceOf[GroupingExpr].expression
    assert(g2.isInstanceOf[GroupingExpr])
    val lit = g2.asInstanceOf[GroupingExpr].expression
    assert(lit.isInstanceOf[LiteralExpr])
    assertEquals(lit.asInstanceOf[LiteralExpr].value, Some(1.0))

  test("parse grouping used in binary expression"):
    // (1 + 2) + (3 + 4)
    val expr = parseExpr("(1 + 2) + (3 + 4)")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.PLUS)
    assert(bin.left.isInstanceOf[GroupingExpr])
    assert(bin.right.isInstanceOf[GroupingExpr])

  // ─── Unary expressions ─────────────────────────────────────────────

  test("parse unary negation"):
    val expr = parseExpr("-123")
    assert(expr.isInstanceOf[UnaryExpr])
    val un = expr.asInstanceOf[UnaryExpr]
    assertEquals(un.operator.tokenType, TokenType.MINUS)
    assert(un.right.isInstanceOf[LiteralExpr])
    assertEquals(un.right.asInstanceOf[LiteralExpr].value, Some(123.0))

  test("parse unary bang"):
    val expr = parseExpr("!true")
    assert(expr.isInstanceOf[UnaryExpr])
    val un = expr.asInstanceOf[UnaryExpr]
    assertEquals(un.operator.tokenType, TokenType.BANG)
    assert(un.right.isInstanceOf[LiteralExpr])
    assertEquals(un.right.asInstanceOf[LiteralExpr].value, Some(true))

  test("parse double bang"):
    val expr = parseExpr("!!false")
    assert(expr.isInstanceOf[UnaryExpr])
    val outer = expr.asInstanceOf[UnaryExpr]
    assertEquals(outer.operator.tokenType, TokenType.BANG)
    assert(outer.right.isInstanceOf[UnaryExpr])
    val inner = outer.right.asInstanceOf[UnaryExpr]
    assertEquals(inner.operator.tokenType, TokenType.BANG)
    assertEquals(inner.right.asInstanceOf[LiteralExpr].value, Some(false))

  test("parse chained unary bang-minus"):
    // !-1 → UnaryExpr(!, UnaryExpr(-, 1))
    val expr = parseExpr("!-1")
    assert(expr.isInstanceOf[UnaryExpr])
    val outer = expr.asInstanceOf[UnaryExpr]
    assertEquals(outer.operator.tokenType, TokenType.BANG)
    assert(outer.right.isInstanceOf[UnaryExpr])
    val inner = outer.right.asInstanceOf[UnaryExpr]
    assertEquals(inner.operator.tokenType, TokenType.MINUS)
    assertEquals(inner.right.asInstanceOf[LiteralExpr].value, Some(1.0))

  test("parse unary minus on grouped expression"):
    // -(1 + 2) → UnaryExpr(-, GroupingExpr(BinaryExpr(1, +, 2)))
    val expr = parseExpr("-(1 + 2)")
    assert(expr.isInstanceOf[UnaryExpr])
    val un = expr.asInstanceOf[UnaryExpr]
    assertEquals(un.operator.tokenType, TokenType.MINUS)
    assert(un.right.isInstanceOf[GroupingExpr])
    val inner = un.right.asInstanceOf[GroupingExpr].expression
    assert(inner.isInstanceOf[BinaryExpr])

  // ─── Associativity (left-to-right) ─────────────────────────────────

  test("subtraction is left-associative: 5 - 3 - 1 == (5 - 3) - 1"):
    val expr = parseExpr("5 - 3 - 1")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.MINUS)

    // Right side is 1
    assert(bin.right.isInstanceOf[LiteralExpr])
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(1.0))

    // Left side is (5 - 3)
    val left = bin.left.asInstanceOf[BinaryExpr]
    assertEquals(left.operator.tokenType, TokenType.MINUS)
    assertEquals(left.left.asInstanceOf[LiteralExpr].value, Some(5.0))
    assertEquals(left.right.asInstanceOf[LiteralExpr].value, Some(3.0))

  test("addition is left-associative: 1 + 2 + 3 == (1 + 2) + 3"):
    val expr = parseExpr("1 + 2 + 3")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.PLUS)
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(3.0))

    val left = bin.left.asInstanceOf[BinaryExpr]
    assertEquals(left.operator.tokenType, TokenType.PLUS)
    assertEquals(left.left.asInstanceOf[LiteralExpr].value, Some(1.0))
    assertEquals(left.right.asInstanceOf[LiteralExpr].value, Some(2.0))

  test("division is left-associative: 12 / 3 / 2 == (12 / 3) / 2"):
    val expr = parseExpr("12 / 3 / 2")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.SLASH)
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(2.0))

    val left = bin.left.asInstanceOf[BinaryExpr]
    assertEquals(left.operator.tokenType, TokenType.SLASH)
    assertEquals(left.left.asInstanceOf[LiteralExpr].value, Some(12.0))
    assertEquals(left.right.asInstanceOf[LiteralExpr].value, Some(3.0))

  test("equality is left-associative: true == true == true"):
    // (true == true) == true
    val expr = parseExpr("true == true == true")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.EQUAL_EQUAL)
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(true))

    val left = bin.left.asInstanceOf[BinaryExpr]
    assertEquals(left.operator.tokenType, TokenType.EQUAL_EQUAL)
    assertEquals(left.left.asInstanceOf[LiteralExpr].value, Some(true))
    assertEquals(left.right.asInstanceOf[LiteralExpr].value, Some(true))

  test("comparison is left-associative: 1 < 2 < 3"):
    // (1 < 2) < 3
    val expr = parseExpr("1 < 2 < 3")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.LESS)
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(3.0))

    val left = bin.left.asInstanceOf[BinaryExpr]
    assertEquals(left.operator.tokenType, TokenType.LESS)
    assertEquals(left.left.asInstanceOf[LiteralExpr].value, Some(1.0))
    assertEquals(left.right.asInstanceOf[LiteralExpr].value, Some(2.0))

  // ─── Precedence ────────────────────────────────────────────────────

  test("multiplication has higher precedence than addition (manual tokens)"):
    // 1 + 2 * 3 → 1 + (2 * 3)
    val tokens = ArrayBuffer(
      mkToken(TokenType.NUMBER, "1", Some(1.0)),
      mkToken(TokenType.PLUS, "+"),
      mkToken(TokenType.NUMBER, "2", Some(2.0)),
      mkToken(TokenType.STAR, "*"),
      mkToken(TokenType.NUMBER, "3", Some(3.0)),
      mkToken(TokenType.EOF, ""),
    )
    val expr = Parser(tokens).expression()
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.PLUS)
    assertEquals(bin.left.asInstanceOf[LiteralExpr].value, Some(1.0))

    val right = bin.right.asInstanceOf[BinaryExpr]
    assertEquals(right.operator.tokenType, TokenType.STAR)
    assertEquals(right.left.asInstanceOf[LiteralExpr].value, Some(2.0))
    assertEquals(right.right.asInstanceOf[LiteralExpr].value, Some(3.0))

  test("unary has higher precedence than binary: -1 + 2 == (-1) + 2"):
    val expr = parseExpr("-1 + 2")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.PLUS)

    assert(bin.left.isInstanceOf[UnaryExpr])
    val left = bin.left.asInstanceOf[UnaryExpr]
    assertEquals(left.operator.tokenType, TokenType.MINUS)
    assertEquals(left.right.asInstanceOf[LiteralExpr].value, Some(1.0))

    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(2.0))

  test("comparison has higher precedence than equality"):
    // 1 < 2 == true → (1 < 2) == true
    val expr = parseExpr("1 < 2 == true")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.EQUAL_EQUAL)
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(true))

    val left = bin.left.asInstanceOf[BinaryExpr]
    assertEquals(left.operator.tokenType, TokenType.LESS)
    assertEquals(left.left.asInstanceOf[LiteralExpr].value, Some(1.0))
    assertEquals(left.right.asInstanceOf[LiteralExpr].value, Some(2.0))

  test("term has higher precedence than comparison: 1 + 2 > 2"):
    // (1 + 2) > 2
    val expr = parseExpr("1 + 2 > 2")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.GREATER)

    val left = bin.left.asInstanceOf[BinaryExpr]
    assertEquals(left.operator.tokenType, TokenType.PLUS)
    assertEquals(left.left.asInstanceOf[LiteralExpr].value, Some(1.0))
    assertEquals(left.right.asInstanceOf[LiteralExpr].value, Some(2.0))

    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(2.0))

  test("full precedence: 1 + 2 * 3 - 4 (manual tokens)"):
    // Expected: (1 + (2 * 3)) - 4
    val tokens = ArrayBuffer(
      mkToken(TokenType.NUMBER, "1", Some(1.0)),
      mkToken(TokenType.PLUS, "+"),
      mkToken(TokenType.NUMBER, "2", Some(2.0)),
      mkToken(TokenType.STAR, "*"),
      mkToken(TokenType.NUMBER, "3", Some(3.0)),
      mkToken(TokenType.MINUS, "-"),
      mkToken(TokenType.NUMBER, "4", Some(4.0)),
      mkToken(TokenType.EOF, ""),
    )
    val expr = Parser(tokens).expression()
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.MINUS)

    // Left: 1 + (2 * 3)
    val left = bin.left.asInstanceOf[BinaryExpr]
    assertEquals(left.operator.tokenType, TokenType.PLUS)
    assertEquals(left.left.asInstanceOf[LiteralExpr].value, Some(1.0))

    val mul = left.right.asInstanceOf[BinaryExpr]
    assertEquals(mul.operator.tokenType, TokenType.STAR)
    assertEquals(mul.left.asInstanceOf[LiteralExpr].value, Some(2.0))
    assertEquals(mul.right.asInstanceOf[LiteralExpr].value, Some(3.0))

    // Right: 4
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(4.0))

  // ─── Complex expressions ───────────────────────────────────────────

  test("complex: 1 - (2 + 3) < 4 == false"):
    // Structure: ((1 - (2 + 3)) < 4) == false
    val expr = parseExpr("1 - (2 + 3) < 4 == false")

    // Top-level == false
    assert(expr.isInstanceOf[BinaryExpr])
    val top = expr.asInstanceOf[BinaryExpr]
    assertEquals(top.operator.tokenType, TokenType.EQUAL_EQUAL)
    assertEquals(top.right.asInstanceOf[LiteralExpr].value, Some(false))

    // Left of == is: (1 - (2 + 3)) < 4
    val lessThan = top.left.asInstanceOf[BinaryExpr]
    assertEquals(lessThan.operator.tokenType, TokenType.LESS)
    assertEquals(lessThan.right.asInstanceOf[LiteralExpr].value, Some(4.0))

    // Left of < is: 1 - (2 + 3)
    val minus = lessThan.left.asInstanceOf[BinaryExpr]
    assertEquals(minus.operator.tokenType, TokenType.MINUS)
    assertEquals(minus.left.asInstanceOf[LiteralExpr].value, Some(1.0))

    // Right of - is: (2 + 3)
    assert(minus.right.isInstanceOf[GroupingExpr])
    val group = minus.right.asInstanceOf[GroupingExpr].expression.asInstanceOf[BinaryExpr]
    assertEquals(group.operator.tokenType, TokenType.PLUS)
    assertEquals(group.left.asInstanceOf[LiteralExpr].value, Some(2.0))
    assertEquals(group.right.asInstanceOf[LiteralExpr].value, Some(3.0))

  test("complex: (1 + 2) == (3 + 0)"):
    val expr = parseExpr("(1 + 2) == (3 + 0)")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.EQUAL_EQUAL)
    assert(bin.left.isInstanceOf[GroupingExpr])
    assert(bin.right.isInstanceOf[GroupingExpr])

    val leftInner = bin.left.asInstanceOf[GroupingExpr].expression.asInstanceOf[BinaryExpr]
    assertEquals(leftInner.left.asInstanceOf[LiteralExpr].value, Some(1.0))
    assertEquals(leftInner.right.asInstanceOf[LiteralExpr].value, Some(2.0))

    val rightInner = bin.right.asInstanceOf[GroupingExpr].expression.asInstanceOf[BinaryExpr]
    assertEquals(rightInner.left.asInstanceOf[LiteralExpr].value, Some(3.0))
    assertEquals(rightInner.right.asInstanceOf[LiteralExpr].value, Some(0.0))

  test("complex: !true == false"):
    // (!true) == false — unary binds tighter than equality
    val expr = parseExpr("!true == false")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.EQUAL_EQUAL)

    assert(bin.left.isInstanceOf[UnaryExpr])
    val un = bin.left.asInstanceOf[UnaryExpr]
    assertEquals(un.operator.tokenType, TokenType.BANG)
    assertEquals(un.right.asInstanceOf[LiteralExpr].value, Some(true))

    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(false))

  test("complex: -1 - -2"):
    // (-1) - (-2)
    val expr = parseExpr("-1 - -2")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.MINUS)

    val left = bin.left.asInstanceOf[UnaryExpr]
    assertEquals(left.operator.tokenType, TokenType.MINUS)
    assertEquals(left.right.asInstanceOf[LiteralExpr].value, Some(1.0))

    val right = bin.right.asInstanceOf[UnaryExpr]
    assertEquals(right.operator.tokenType, TokenType.MINUS)
    assertEquals(right.right.asInstanceOf[LiteralExpr].value, Some(2.0))

  test("complex: mixed comparison operators"):
    // 1 <= 2 >= 0 → (1 <= 2) >= 0
    val expr = parseExpr("1 <= 2 >= 0")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.GREATER_EQUAL)
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(0.0))

    val left = bin.left.asInstanceOf[BinaryExpr]
    assertEquals(left.operator.tokenType, TokenType.LESS_EQUAL)
    assertEquals(left.left.asInstanceOf[LiteralExpr].value, Some(1.0))
    assertEquals(left.right.asInstanceOf[LiteralExpr].value, Some(2.0))

  test("complex: mixed equality and inequality"):
    // 1 != 2 == false → (1 != 2) == false
    val expr = parseExpr("1 != 2 == false")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.EQUAL_EQUAL)
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(false))

    val left = bin.left.asInstanceOf[BinaryExpr]
    assertEquals(left.operator.tokenType, TokenType.BANG_EQUAL)
    assertEquals(left.left.asInstanceOf[LiteralExpr].value, Some(1.0))
    assertEquals(left.right.asInstanceOf[LiteralExpr].value, Some(2.0))

  // ─── toString output ───────────────────────────────────────────────

  test("toString for literal expressions"):
    assertEquals(parseExpr("123").toString, "<123.0>")
    assertEquals(parseExpr("\"hi\"").toString, "<\"hi\">")
    assertEquals(parseExpr("true").toString, "<TRUE>")
    assertEquals(parseExpr("false").toString, "<FALSE>")
    assertEquals(parseExpr("nil").toString, "<NIL>")

  test("toString for binary expression"):
    assertEquals(parseExpr("1 + 2").toString, "(+ <1.0> <2.0>)")

  test("toString for unary expression"):
    assertEquals(parseExpr("-1").toString, "(- <1.0>)")

  test("toString for grouping expression"):
    assertEquals(parseExpr("(1)").toString, "(<1.0>)")

  test("toString for complex expression"):
    assertEquals(parseExpr("(1 + 2)").toString, "((+ <1.0> <2.0>))")

  // ─── Error cases ───────────────────────────────────────────────────

  test("error: unclosed parenthesis"):
    val ex = intercept[RuntimeException] {
      parseExpr("(2 + 2")
    }
    assert(clue(ex.getMessage).contains("Expected ')'"))

  test("error: incomplete binary expression"):
    val ex = intercept[RuntimeException] {
      parseExpr("1 + ")
    }
    assert(clue(ex.getMessage).contains("Expected expression"))

  test("error: completely unexpected token"):
    // An EQUAL sign alone is not a valid expression start
    val tokens = ArrayBuffer(
      mkToken(TokenType.EQUAL, "="),
      mkToken(TokenType.EOF, ""),
    )
    val ex = intercept[RuntimeException] {
      Parser(tokens).expression()
    }
    assert(clue(ex.getMessage).contains("Expected expression"))

  test("error: nested unclosed parentheses"):
    val ex = intercept[RuntimeException] {
      parseExpr("((1 + 2)")
    }
    assert(clue(ex.getMessage).contains("Expected ')'"))

  test("error: empty parentheses"):
    val ex = intercept[RuntimeException] {
      parseExpr("()")
    }
    assert(clue(ex.getMessage).contains("Expected expression"))

  // ─── Edge cases ────────────────────────────────────────────────────

  test("single literal is valid"):
    val expr = parseExpr("42")
    assert(expr.isInstanceOf[LiteralExpr])
    assertEquals(expr.asInstanceOf[LiteralExpr].value, Some(42.0))

  test("grouping around single literal"):
    val expr = parseExpr("(42)")
    assert(expr.isInstanceOf[GroupingExpr])
    val inner = expr.asInstanceOf[GroupingExpr].expression
    assert(inner.isInstanceOf[LiteralExpr])
    assertEquals(inner.asInstanceOf[LiteralExpr].value, Some(42.0))

  test("unary on grouped expression: !(1 == 2)"):
    val expr = parseExpr("!(1 == 2)")
    assert(expr.isInstanceOf[UnaryExpr])
    val un = expr.asInstanceOf[UnaryExpr]
    assertEquals(un.operator.tokenType, TokenType.BANG)
    assert(un.right.isInstanceOf[GroupingExpr])
    val inner = un.right.asInstanceOf[GroupingExpr].expression.asInstanceOf[BinaryExpr]
    assertEquals(inner.operator.tokenType, TokenType.EQUAL_EQUAL)

  test("long chain of additions: 1 + 2 + 3 + 4"):
    // ((1 + 2) + 3) + 4
    val expr = parseExpr("1 + 2 + 3 + 4")
    assert(expr.isInstanceOf[BinaryExpr])
    val top = expr.asInstanceOf[BinaryExpr]
    assertEquals(top.operator.tokenType, TokenType.PLUS)
    assertEquals(top.right.asInstanceOf[LiteralExpr].value, Some(4.0))

    val mid = top.left.asInstanceOf[BinaryExpr]
    assertEquals(mid.operator.tokenType, TokenType.PLUS)
    assertEquals(mid.right.asInstanceOf[LiteralExpr].value, Some(3.0))

    val inner = mid.left.asInstanceOf[BinaryExpr]
    assertEquals(inner.operator.tokenType, TokenType.PLUS)
    assertEquals(inner.left.asInstanceOf[LiteralExpr].value, Some(1.0))
    assertEquals(inner.right.asInstanceOf[LiteralExpr].value, Some(2.0))

  test("string in binary comparison is valid syntax"):
    // "a" == "b"
    val expr = parseExpr("\"a\" == \"b\"")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.EQUAL_EQUAL)
    assertEquals(bin.left.asInstanceOf[LiteralExpr].value, Some("a"))
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some("b"))

  test("nil equality: nil == nil"):
    val expr = parseExpr("nil == nil")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.EQUAL_EQUAL)
    assertEquals(bin.left.asInstanceOf[LiteralExpr].value, None)
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, None)

  test("boolean in arithmetic (syntactically valid): true + false"):
    val expr = parseExpr("true + false")
    assert(expr.isInstanceOf[BinaryExpr])
    val bin = expr.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.PLUS)
    assertEquals(bin.left.asInstanceOf[LiteralExpr].value, Some(true))
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(false))

