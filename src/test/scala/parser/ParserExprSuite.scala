import Expr.*
import Stmt.*

class ParserExprSuite extends munit.FunSuite, ParserHelpers:

  // ─── Literals ────────────────────────────────────────────────────────

  List(
    ("123",   Some(123.0)),
    ("3.14",  Some(3.14)),
    ("true",  Some(true)),
    ("false", Some(false)),
  ).foreach: (source, expected) =>
    test(s"parse literal: $source"):
      assertEquals(parseExpr(source).asInstanceOf[LiteralExpr].value, expected)

  List(
    ("\"hello\"", Some("hello")),
    ("'world'",   Some("world")),
  ).foreach: (source, expected) =>
    test(s"parse string literal $source"):
      assertEquals(parseExpr(source).asInstanceOf[LiteralExpr].value, expected)

  test("parse nil literal"):
    assertEquals(parseExpr("nil").asInstanceOf[LiteralExpr].value, None)

  // ─── Binary expressions ───────────────────────────────────────────────

  List(
    ("2 + 2",  TokenType.PLUS,          2.0,  2.0),
    ("10 - 4", TokenType.MINUS,         10.0, 4.0),
    ("6 / 3",  TokenType.SLASH,         6.0,  3.0),
    ("1 < 2",  TokenType.LESS,          1.0,  2.0),
    ("1 <= 2", TokenType.LESS_EQUAL,    1.0,  2.0),
    ("3 > 1",  TokenType.GREATER,       3.0,  1.0),
    ("3 >= 1", TokenType.GREATER_EQUAL, 3.0,  1.0),
    ("1 == 1", TokenType.EQUAL_EQUAL,   1.0,  1.0),
    ("1 != 2", TokenType.BANG_EQUAL,    1.0,  2.0),
  ).foreach: (source, op, left, right) =>
    test(s"parse binary: $source"):
      val bin = parseExpr(source).asInstanceOf[BinaryExpr]
      assertEquals(bin.operator.tokenType, op)
      assertEquals(bin.left.asInstanceOf[LiteralExpr].value, Some(left))
      assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(right))

  test("parse multiplication with manual tokens"):
    val tokens = Seq(
      mkToken(TokenType.NUMBER, "2", Some(2.0)),
      mkToken(TokenType.STAR, "*"),
      mkToken(TokenType.NUMBER, "3", Some(3.0)),
      mkToken(TokenType.EOF, ""),
    )
    val bin = Parser(tokens).expression().asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.STAR)
    assertEquals(bin.left.asInstanceOf[LiteralExpr].value, Some(2.0))
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(3.0))

  // ─── Grouping ─────────────────────────────────────────────────────────

  test("parse simple grouping"):
    val bin = parseExpr("(2 + 2)").asInstanceOf[GroupingExpr].expression.asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.PLUS)

  test("parse nested groupings"):
    val inner = parseExpr("((1 + 2))").asInstanceOf[GroupingExpr].expression.asInstanceOf[GroupingExpr].expression
    assert(inner.isInstanceOf[BinaryExpr])

  test("parse deeply nested grouping around literal"):
    val g3 = parseExpr("(((1)))").asInstanceOf[GroupingExpr]
      .expression.asInstanceOf[GroupingExpr]
      .expression.asInstanceOf[GroupingExpr]
    assertEquals(g3.expression.asInstanceOf[LiteralExpr].value, Some(1.0))

  test("parse grouping used in binary expression"):
    val bin = parseExpr("(1 + 2) + (3 + 4)").asInstanceOf[BinaryExpr]
    assert(bin.left.isInstanceOf[GroupingExpr])
    assert(bin.right.isInstanceOf[GroupingExpr])

  // ─── Unary ────────────────────────────────────────────────────────────

  test("parse unary negation"):
    val un = parseExpr("-123").asInstanceOf[UnaryExpr]
    assertEquals(un.operator.tokenType, TokenType.MINUS)
    assertEquals(un.right.asInstanceOf[LiteralExpr].value, Some(123.0))

  test("parse unary bang"):
    val un = parseExpr("!true").asInstanceOf[UnaryExpr]
    assertEquals(un.operator.tokenType, TokenType.BANG)

  test("parse double bang"):
    val outer = parseExpr("!!false").asInstanceOf[UnaryExpr]
    val inner = outer.right.asInstanceOf[UnaryExpr]
    assertEquals(outer.operator.tokenType, TokenType.BANG)
    assertEquals(inner.operator.tokenType, TokenType.BANG)
    assertEquals(inner.right.asInstanceOf[LiteralExpr].value, Some(false))

  test("parse chained unary bang-minus"):
    val outer = parseExpr("!-1").asInstanceOf[UnaryExpr]
    assertEquals(outer.operator.tokenType, TokenType.BANG)
    assertEquals(outer.right.asInstanceOf[UnaryExpr].operator.tokenType, TokenType.MINUS)

  test("parse unary minus on grouped expression"):
    val un = parseExpr("-(1 + 2)").asInstanceOf[UnaryExpr]
    assertEquals(un.operator.tokenType, TokenType.MINUS)
    assert(un.right.isInstanceOf[GroupingExpr])

  // ─── Associativity ────────────────────────────────────────────────────

  List(
    ("5 - 3 - 1",  TokenType.MINUS, 1.0,  5.0,  3.0),
    ("1 + 2 + 3",  TokenType.PLUS,  3.0,  1.0,  2.0),
    ("12 / 3 / 2", TokenType.SLASH, 2.0, 12.0,  3.0),
    ("1 < 2 < 3",  TokenType.LESS,  3.0,  1.0,  2.0),
  ).foreach: (source, op, rightVal, llVal, lrVal) =>
    test(s"left-associative: $source"):
      val bin = parseExpr(source).asInstanceOf[BinaryExpr]
      assertEquals(bin.operator.tokenType, op)
      assertEquals(bin.right.asInstanceOf[LiteralExpr].value, Some(rightVal))
      val left = bin.left.asInstanceOf[BinaryExpr]
      assertEquals(left.left.asInstanceOf[LiteralExpr].value, Some(llVal))
      assertEquals(left.right.asInstanceOf[LiteralExpr].value, Some(lrVal))

  test("equality is left-associative: true == true == true"):
    val bin = parseExpr("true == true == true").asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.EQUAL_EQUAL)
    assertEquals(bin.left.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.EQUAL_EQUAL)

  // ─── Precedence ────────────────────────────────────────────────────────

  test("multiplication has higher precedence than addition (manual tokens)"):
    val tokens = Seq(
      mkToken(TokenType.NUMBER, "1", Some(1.0)),
      mkToken(TokenType.PLUS, "+"),
      mkToken(TokenType.NUMBER, "2", Some(2.0)),
      mkToken(TokenType.STAR, "*"),
      mkToken(TokenType.NUMBER, "3", Some(3.0)),
      mkToken(TokenType.EOF, ""),
    )
    val bin = Parser(tokens).expression().asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.PLUS)
    assertEquals(bin.right.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.STAR)

  test("unary has higher precedence than binary"):
    val bin = parseExpr("-1 + 2").asInstanceOf[BinaryExpr]
    assert(bin.left.isInstanceOf[UnaryExpr])

  test("comparison has higher precedence than equality"):
    val bin = parseExpr("1 < 2 == true").asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.EQUAL_EQUAL)
    assertEquals(bin.left.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.LESS)

  test("term has higher precedence than comparison"):
    val bin = parseExpr("1 + 2 > 2").asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.GREATER)
    assertEquals(bin.left.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.PLUS)

  test("full precedence: 1 + 2 * 3 - 4 (manual tokens)"):
    val tokens = Seq(
      mkToken(TokenType.NUMBER, "1", Some(1.0)),
      mkToken(TokenType.PLUS, "+"),
      mkToken(TokenType.NUMBER, "2", Some(2.0)),
      mkToken(TokenType.STAR, "*"),
      mkToken(TokenType.NUMBER, "3", Some(3.0)),
      mkToken(TokenType.MINUS, "-"),
      mkToken(TokenType.NUMBER, "4", Some(4.0)),
      mkToken(TokenType.EOF, ""),
    )
    val bin = Parser(tokens).expression().asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.MINUS)
    assertEquals(bin.left.asInstanceOf[BinaryExpr].right.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.STAR)

  // ─── Complex expressions ──────────────────────────────────────────────

  test("complex: 1 - (2 + 3) < 4 == false"):
    val top = parseExpr("1 - (2 + 3) < 4 == false").asInstanceOf[BinaryExpr]
    assertEquals(top.operator.tokenType, TokenType.EQUAL_EQUAL)
    val minus = top.left.asInstanceOf[BinaryExpr].left.asInstanceOf[BinaryExpr]
    assertEquals(minus.right.asInstanceOf[GroupingExpr].expression.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.PLUS)

  test("complex: !true == false"):
    val bin = parseExpr("!true == false").asInstanceOf[BinaryExpr]
    assertEquals(bin.operator.tokenType, TokenType.EQUAL_EQUAL)
    assert(bin.left.isInstanceOf[UnaryExpr])

  test("complex: -1 - -2"):
    val bin = parseExpr("-1 - -2").asInstanceOf[BinaryExpr]
    assertEquals(bin.left.asInstanceOf[UnaryExpr].operator.tokenType, TokenType.MINUS)
    assertEquals(bin.right.asInstanceOf[UnaryExpr].operator.tokenType, TokenType.MINUS)

  test("nil equality: nil == nil"):
    val bin = parseExpr("nil == nil").asInstanceOf[BinaryExpr]
    assertEquals(bin.left.asInstanceOf[LiteralExpr].value, None)
    assertEquals(bin.right.asInstanceOf[LiteralExpr].value, None)

  test("long chain of additions: 1 + 2 + 3 + 4"):
    val top = parseExpr("1 + 2 + 3 + 4").asInstanceOf[BinaryExpr]
    assertEquals(top.right.asInstanceOf[LiteralExpr].value, Some(4.0))
    val inner = top.left.asInstanceOf[BinaryExpr].left.asInstanceOf[BinaryExpr]
    assertEquals(inner.left.asInstanceOf[LiteralExpr].value, Some(1.0))
    assertEquals(inner.right.asInstanceOf[LiteralExpr].value, Some(2.0))

  // ─── Logical operators ────────────────────────────────────────────────

  test("parse 'and' produces LogicExpr"):
    val logic = parseExpr("true and false").asInstanceOf[LogicExpr]
    assertEquals(logic.operator.tokenType, TokenType.AND)

  test("parse 'or' produces LogicExpr"):
    assertEquals(parseExpr("false or true").asInstanceOf[LogicExpr].operator.tokenType, TokenType.OR)

  test("'and' has higher precedence than 'or'"):
    val or = parseExpr("false or true and false").asInstanceOf[LogicExpr]
    assertEquals(or.operator.tokenType, TokenType.OR)
    assertEquals(or.right.asInstanceOf[LogicExpr].operator.tokenType, TokenType.AND)

  test("'and' is left-associative"):
    val right = parseExpr("true and false and true").asInstanceOf[LogicExpr]
    assertEquals(right.right.asInstanceOf[LiteralExpr].value, Some(true))
    assert(right.left.isInstanceOf[LogicExpr])

  test("'or' is left-associative"):
    val right = parseExpr("false or false or true").asInstanceOf[LogicExpr]
    assertEquals(right.right.asInstanceOf[LiteralExpr].value, Some(true))
    assert(right.left.isInstanceOf[LogicExpr])

  test("equality has higher precedence than 'and'"):
    val and = parseExpr("true == true and false").asInstanceOf[LogicExpr]
    assertEquals(and.left.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.EQUAL_EQUAL)

  test("logical operators work with grouped expressions"):
    val logic = parseExpr("(1 == 1) and (2 == 2)").asInstanceOf[LogicExpr]
    assert(logic.left.isInstanceOf[GroupingExpr])
    assert(logic.right.isInstanceOf[GroupingExpr])

  test("unary binds tighter than 'and'"):
    assert(parseExpr("!true and false").asInstanceOf[LogicExpr].left.isInstanceOf[UnaryExpr])

  // ─── toString ─────────────────────────────────────────────────────────

  List(
    ("123",     "<123.0>"),
    ("\"hi\"",  "<\"hi\">"),
    ("true",    "<TRUE>"),
    ("false",   "<FALSE>"),
    ("nil",     "<NIL>"),
    ("1 + 2",   "(<1.0> PLUS <2.0>)"),
    ("-1",      "(MINUS <1.0>)"),
    ("(1)",     "(<1.0>)"),
    ("(1 + 2)", "((<1.0> PLUS <2.0>))"),
  ).foreach: (source, expected) =>
    test(s"toString: $source"):
      assertEquals(parseExpr(source).toString, expected)

  // ─── Errors ───────────────────────────────────────────────────────────

  List(
    ("(2 + 2",   "Expected ')'",        "unclosed parenthesis"),
    ("1 + ",     "Expected expression",  "incomplete binary expression"),
    ("((1 + 2)", "Expected ')'",        "nested unclosed parentheses"),
    ("()",       "Expected expression",  "empty parentheses"),
  ).foreach: (source, fragment, description) =>
    test(s"error: $description"):
      val ex = intercept[RuntimeException](parseExpr(source))
      assert(clue(ex.getMessage).contains(fragment))

  test("error: completely unexpected token"):
    val ex = intercept[RuntimeException]:
      Parser(Seq(mkToken(TokenType.EQUAL, "="), mkToken(TokenType.EOF, ""))).expression()
    assert(clue(ex.getMessage).contains("Expected expression"))
