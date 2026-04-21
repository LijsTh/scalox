import Expr.*
import Stmt.*
import scala.collection.mutable.ArrayBuffer

class ParserSuite extends munit.FunSuite:

  // Helper: scan source and parse a single expression
  private def parseExpr(source: String): Expr =
    val tokens = ArrayBuffer.from(Scanner(source).scan())
    Parser(tokens).expression()

  // Helper: scan source and call parse() (top-level entry point)
  private def parseProgram(source: String): ArrayBuffer[Stmt] =
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

  test("parse empty input returns empty program via parse()"):
    // Example: "" -> no statements
    val stmts = parseProgram("")
    assertEquals(stmts.length, 0)

  // ─── Statements ─────────────────────────────────────────────────────

  test("parse expression statement"):
    // Example: "123;" -> ExpressionStmt(LiteralExpr(123.0))
    val stmts = parseProgram("123;")
    assertEquals(stmts.length, 1)
    assert(stmts.head.isInstanceOf[ExpressionStmt])
    val expr = stmts.head.asInstanceOf[ExpressionStmt].expr
    assert(expr.isInstanceOf[LiteralExpr])
    assertEquals(expr.asInstanceOf[LiteralExpr].value, Some(123.0))

  test("parse multiple expression statements"):
    // Example: "1; 2;" -> two ExpressionStmt
    val stmts = parseProgram("1; 2;")
    assertEquals(stmts.length, 2)
    assert(stmts(0).isInstanceOf[ExpressionStmt])
    assert(stmts(1).isInstanceOf[ExpressionStmt])

  test("parse print statement"):
    // Example: "print \"hola\";" -> PrintStmt(LiteralExpr("hola"))
    val stmts = parseProgram("print \"hola\";")
    assertEquals(stmts.length, 1)
    assert(stmts.head.isInstanceOf[PrintStmt])
    val expr = stmts.head.asInstanceOf[PrintStmt].expr
    assert(expr.isInstanceOf[LiteralExpr])
    assertEquals(expr.asInstanceOf[LiteralExpr].value, Some("hola"))

  test("parse variable declaration with initializer"):
    // Example: "var x = 5;" -> VarDecl("x", Some(LiteralExpr(5.0)))
    val stmts = parseProgram("var x = 5;")
    assertEquals(stmts.length, 1)
    assert(stmts.head.isInstanceOf[VarDecl])
    val decl = stmts.head.asInstanceOf[VarDecl]
    assertEquals(decl.name, "x")
    assert(decl.initializer.nonEmpty)
    assert(decl.initializer.get.isInstanceOf[LiteralExpr])
    assertEquals(decl.initializer.get.asInstanceOf[LiteralExpr].value, Some(5.0))

  test("parse variable declaration without initializer"):
    // Example: "var x;" -> VarDecl("x", None)
    val stmts = parseProgram("var x;")
    assertEquals(stmts.length, 1)
    assert(stmts.head.isInstanceOf[VarDecl])
    val decl = stmts.head.asInstanceOf[VarDecl]
    assertEquals(decl.name, "x")
    assertEquals(decl.initializer, None)

  test("parse empty block statement"):
    // Example: "{}" -> BlockStmt([])
    val stmts = parseProgram("{}")
    assertEquals(stmts.length, 1)
    assert(stmts.head.isInstanceOf[BlockStmt])
    val block = stmts.head.asInstanceOf[BlockStmt]
    assertEquals(block.statements.length, 0)

  test("parse block statement with mixed statements"):
    // Example: "{ print 1; var x = 2; x; }" -> BlockStmt(PrintStmt, VarDecl, ExpressionStmt)
    val stmts = parseProgram("{ print 1; var x = 2; x; }")
    assertEquals(stmts.length, 1)
    assert(stmts.head.isInstanceOf[BlockStmt])
    val block = stmts.head.asInstanceOf[BlockStmt]
    assertEquals(block.statements.length, 3)
    assert(block.statements(0).isInstanceOf[PrintStmt])
    assert(block.statements(1).isInstanceOf[VarDecl])
    assert(block.statements(2).isInstanceOf[ExpressionStmt])

  test("parse nested block statements"):
    // "{ { var x = 1; } }" -> BlockStmt([BlockStmt([VarDecl])])
    val stmts = parseProgram("{ { var x = 1; } }")
    assertEquals(stmts.length, 1)
    val outer = stmts.head.asInstanceOf[BlockStmt]
    assertEquals(outer.statements.length, 1)
    val inner = outer.statements(0).asInstanceOf[BlockStmt]
    assertEquals(inner.statements.length, 1)
    assert(inner.statements(0).isInstanceOf[VarDecl])

  test("parse variable declaration with expression initializer"):
    // "var z = 2 + 3;" -> VarDecl("z", Some(BinaryExpr(+)))
    val stmts = parseProgram("var z = 2 + 3;")
    val decl = stmts.head.asInstanceOf[VarDecl]
    assertEquals(decl.name, "z")
    assert(decl.initializer.get.isInstanceOf[BinaryExpr])

  test("parse assignment expression statement"):
    // "x = 7;" -> ExpressionStmt(AssignmentExpr(x, 7))
    val stmts = parseProgram("x = 7;")
    assertEquals(stmts.length, 1)
    val expr = stmts.head.asInstanceOf[ExpressionStmt].expr
    assert(expr.isInstanceOf[AssignmentExpr])
    val assign = expr.asInstanceOf[AssignmentExpr]
    assertEquals(assign.name.lexeme, "x")
    assert(assign.value.isInstanceOf[LiteralExpr])
    assertEquals(assign.value.asInstanceOf[LiteralExpr].value, Some(7.0))

  test("parse block with var decl then assignment"):
    // "{ var a = 1; a = 2; }" -> BlockStmt([VarDecl, ExpressionStmt(AssignmentExpr)])
    val stmts = parseProgram("{ var a = 1; a = 2; }")
    val block = stmts.head.asInstanceOf[BlockStmt]
    assertEquals(block.statements.length, 2)
    assert(block.statements(0).isInstanceOf[VarDecl])
    val assignStmt = block.statements(1).asInstanceOf[ExpressionStmt].expr
    assert(assignStmt.isInstanceOf[AssignmentExpr])
    assertEquals(assignStmt.asInstanceOf[AssignmentExpr].name.lexeme, "a")

  test("parse variable shadowing across nested blocks"):
    // "{ var x = 1; { var x = 2; } }" -> two VarDecl named x in nested scopes
    val stmts = parseProgram("{ var x = 1; { var x = 2; } }")
    val outer = stmts.head.asInstanceOf[BlockStmt]
    assert(outer.statements(0).isInstanceOf[VarDecl])
    val innerBlock = outer.statements(1).asInstanceOf[BlockStmt]
    assert(innerBlock.statements(0).isInstanceOf[VarDecl])
    assertEquals(innerBlock.statements(0).asInstanceOf[VarDecl].name, "x")

  test("error: unterminated block"):
    val ex = intercept[RuntimeException] { parseProgram("{ var x = 1;") }
    assert(clue(ex.getMessage).contains("Expected '}'"))

  test("error: assignment to undeclared variable parses fine"):
    // Parser does not check scope — it just produces an AssignmentExpr
    val stmts = parseProgram("y = 42;")
    val expr = stmts.head.asInstanceOf[ExpressionStmt].expr
    assert(expr.isInstanceOf[AssignmentExpr])

  // ─── if statements ──────────────────────────────────────────────────

  test("parse if with then branch only"):
    // if (true) print 1;
    val stmts = parseProgram("if (true) print 1;")
    assertEquals(stmts.length, 1)
    val ifStmt = stmts.head.asInstanceOf[IfStmt]
    assert(ifStmt.condition.isInstanceOf[LiteralExpr])
    assertEquals(ifStmt.condition.asInstanceOf[LiteralExpr].value, Some(true))
    assert(ifStmt.thenBranch.isInstanceOf[PrintStmt])
    assertEquals(ifStmt.elseBranch, None)

  test("parse if-else"):
    // if (true) print 1; else print 2;
    val stmts = parseProgram("if (true) print 1; else print 2;")
    val ifStmt = stmts.head.asInstanceOf[IfStmt]
    assert(ifStmt.thenBranch.isInstanceOf[PrintStmt])
    assert(ifStmt.elseBranch.nonEmpty)
    assert(ifStmt.elseBranch.get.isInstanceOf[PrintStmt])

  test("parse if with block body"):
    // if (true) { print 1; }
    val stmts = parseProgram("if (true) { print 1; }")
    val ifStmt = stmts.head.asInstanceOf[IfStmt]
    assert(ifStmt.thenBranch.isInstanceOf[BlockStmt])
    assertEquals(ifStmt.thenBranch.asInstanceOf[BlockStmt].statements.length, 1)

  test("parse if-else with block bodies"):
    val stmts = parseProgram("if (false) { print 1; } else { print 2; }")
    val ifStmt = stmts.head.asInstanceOf[IfStmt]
    assert(ifStmt.thenBranch.isInstanceOf[BlockStmt])
    assert(ifStmt.elseBranch.get.isInstanceOf[BlockStmt])

  test("parse nested if-else (dangling else binds to nearest if)"):
    // if (a) if (b) print 1; else print 2;
    // else binds to inner if
    val stmts = parseProgram("if (true) if (false) print 1; else print 2;")
    val outer = stmts.head.asInstanceOf[IfStmt]
    assertEquals(outer.elseBranch, None)
    val inner = outer.thenBranch.asInstanceOf[IfStmt]
    assert(inner.elseBranch.nonEmpty)

  test("parse if condition is arbitrary expression"):
    val stmts = parseProgram("if (1 + 2 > 0) print 1;")
    val ifStmt = stmts.head.asInstanceOf[IfStmt]
    assert(ifStmt.condition.isInstanceOf[BinaryExpr])

  test("error: if without '('"):
    val ex = intercept[RuntimeException] { parseProgram("if true print 1;") }
    assert(clue(ex.getMessage).contains("Expected '('"))

  test("error: if without ')'"):
    val ex = intercept[RuntimeException] { parseProgram("if (true print 1;") }
    assert(clue(ex.getMessage).contains("Expected ')'"))

  // ─── while statements ───────────────────────────────────────────────

  test("parse while statement"):
    // while (false) print 1;
    val stmts = parseProgram("while (false) print 1;")
    assertEquals(stmts.length, 1)
    val whileStmt = stmts.head.asInstanceOf[WhileStmt]
    assert(whileStmt.condition.isInstanceOf[LiteralExpr])
    assertEquals(whileStmt.condition.asInstanceOf[LiteralExpr].value, Some(false))
    assert(whileStmt.body.isInstanceOf[PrintStmt])

  test("parse while with block body"):
    val stmts = parseProgram("while (true) { print 1; }")
    val whileStmt = stmts.head.asInstanceOf[WhileStmt]
    assert(whileStmt.body.isInstanceOf[BlockStmt])

  test("parse while with compound condition"):
    val stmts = parseProgram("while (x > 0) print x;")
    val whileStmt = stmts.head.asInstanceOf[WhileStmt]
    assert(whileStmt.condition.isInstanceOf[BinaryExpr])
    assertEquals(whileStmt.condition.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.GREATER)

  test("error: while without '('"):
    val ex = intercept[RuntimeException] { parseProgram("while true print 1;") }
    assert(clue(ex.getMessage).contains("Expected '('"))

  test("error: while without ')'"):
    val ex = intercept[RuntimeException] { parseProgram("while (true print 1;") }
    assert(clue(ex.getMessage).contains("Expected ')'"))

  // ─── for statements (desugared to while) ────────────────────────────────

  test("for loop desugars to BlockStmt(VarDecl, WhileStmt)"):
    // for (var i = 0; i < 3; i = i + 1) print i;
    // -> BlockStmt([VarDecl(i,0), WhileStmt(i<3, BlockStmt([print i, i=i+1]))])
    val stmts = parseProgram("for (var i = 0; i < 3; i = i + 1) print i;")
    assertEquals(stmts.length, 1)
    val outer = stmts.head.asInstanceOf[BlockStmt]
    assertEquals(outer.statements.length, 2)
    assert(outer.statements(0).isInstanceOf[VarDecl])
    assertEquals(outer.statements(0).asInstanceOf[VarDecl].name, "i")
    val whileStmt = outer.statements(1).asInstanceOf[WhileStmt]
    assert(whileStmt.condition.isInstanceOf[BinaryExpr])
    assertEquals(whileStmt.condition.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.LESS)
    // body is BlockStmt([print i, ExpressionStmt(i=i+1)])
    val body = whileStmt.body.asInstanceOf[BlockStmt]
    assertEquals(body.statements.length, 2)
    assert(body.statements(0).isInstanceOf[PrintStmt])
    assert(body.statements(1).isInstanceOf[ExpressionStmt])
    assert(body.statements(1).asInstanceOf[ExpressionStmt].expr.isInstanceOf[AssignmentExpr])

  test("for with expression initializer desugars to BlockStmt"):
    // for (i = 0; i < 3; i = i + 1) print i;
    val stmts = parseProgram("for (i = 0; i < 3; i = i + 1) print i;")
    val outer = stmts.head.asInstanceOf[BlockStmt]
    assert(outer.statements(0).isInstanceOf[ExpressionStmt])
    assert(outer.statements(0).asInstanceOf[ExpressionStmt].expr.isInstanceOf[AssignmentExpr])
    assert(outer.statements(1).isInstanceOf[WhileStmt])

  test("for with no initializer desugars to WhileStmt directly"):
    // for (; i < 3; i = i + 1) print i;
    val stmts = parseProgram("for (; i < 3; i = i + 1) print i;")
    assertEquals(stmts.length, 1)
    assert(stmts.head.isInstanceOf[WhileStmt])

  test("for with no condition uses true (infinite loop guard)"):
    // for (var i = 0;; i = i + 1) print i;
    val stmts = parseProgram("for (var i = 0;; i = i + 1) print i;")
    val outer = stmts.head.asInstanceOf[BlockStmt]
    val whileStmt = outer.statements(1).asInstanceOf[WhileStmt]
    // condition is LiteralExpr(Some(true))
    assert(whileStmt.condition.isInstanceOf[LiteralExpr])
    assertEquals(whileStmt.condition.asInstanceOf[LiteralExpr].value, Some(true))

  test("for with no increment: body is not wrapped in extra block"):
    // for (var i = 0; i < 3;) print i;
    val stmts = parseProgram("for (var i = 0; i < 3;) print i;")
    val outer = stmts.head.asInstanceOf[BlockStmt]
    val whileStmt = outer.statements(1).asInstanceOf[WhileStmt]
    // body is just PrintStmt, not wrapped
    assert(whileStmt.body.isInstanceOf[PrintStmt])

  test("for with block body"):
    val stmts = parseProgram("for (var i = 0; i < 3; i = i + 1) { print i; }")
    val outer = stmts.head.asInstanceOf[BlockStmt]
    val whileStmt = outer.statements(1).asInstanceOf[WhileStmt]
    val body = whileStmt.body.asInstanceOf[BlockStmt]
    // body block has original block + increment
    assert(body.statements(0).isInstanceOf[BlockStmt])
    assert(body.statements(1).isInstanceOf[ExpressionStmt])

  test("error: for without '('"):
    val ex = intercept[RuntimeException] { parseProgram("for var i = 0; i < 3; i = i+1) print i;") }
    assert(clue(ex.getMessage).contains("Expected '('"))

  test("error: for missing ';' after condition"):
    val ex = intercept[RuntimeException] { parseProgram("for (var i = 0; i < 3 i = i+1) print i;") }
    assert(clue(ex.getMessage).contains("Expected ';' after for loop condition"))

  test("error: for missing ')'"):
    val ex = intercept[RuntimeException] { parseProgram("for (var i = 0; i < 3; i = i+1 print i;") }
    assert(clue(ex.getMessage).contains("Expected ')' after for loop clauses"))

  test("error: expression statement without semicolon"):
    // Example: "123" -> error (missing ';')
    val ex = intercept[RuntimeException] {
      parseProgram("123")
    }
    assert(clue(ex.getMessage).contains("Expected ';' after expression"))

  test("error: print statement without semicolon"):
    // Example: "print 1" -> error (missing ';')
    val ex = intercept[RuntimeException] {
      parseProgram("print 1")
    }
    assert(clue(ex.getMessage).contains("Expected ';' after value to print"))

  test("error: variable declaration without variable name"):
    // Example: "var = 1;" -> error (missing identifier)
    val ex = intercept[RuntimeException] {
      parseProgram("var = 1;")
    }
    assert(clue(ex.getMessage).contains("Expected variable name"))

  test("error: variable declaration without semicolon"):
    // Example: "var x = 1" -> error (missing ';')
    val ex = intercept[RuntimeException] {
      parseProgram("var x = 1")
    }
    assert(clue(ex.getMessage).contains("Expected ';' after variable declaration"))

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
    assertEquals(parseExpr("1 + 2").toString, "(<1.0> + <2.0>)")

  test("toString for unary expression"):
    assertEquals(parseExpr("-1").toString, "(- <1.0>)")

  test("toString for grouping expression"):
    assertEquals(parseExpr("(1)").toString, "(<1.0>)")

  test("toString for complex expression"):
    assertEquals(parseExpr("(1 + 2)").toString, "((<1.0> + <2.0>))")

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

  // ─── Logical operators (and / or) ─────────────────────────────────

  test("parse 'and' produces LogicExpr"):
    val expr = parseExpr("true and false")
    assert(expr.isInstanceOf[LogicExpr])
    val logic = expr.asInstanceOf[LogicExpr]
    assertEquals(logic.operator.tokenType, TokenType.AND)
    assertEquals(logic.left.asInstanceOf[LiteralExpr].value, Some(true))
    assertEquals(logic.right.asInstanceOf[LiteralExpr].value, Some(false))

  test("parse 'or' produces LogicExpr"):
    val expr = parseExpr("false or true")
    assert(expr.isInstanceOf[LogicExpr])
    val logic = expr.asInstanceOf[LogicExpr]
    assertEquals(logic.operator.tokenType, TokenType.OR)
    assertEquals(logic.left.asInstanceOf[LiteralExpr].value, Some(false))
    assertEquals(logic.right.asInstanceOf[LiteralExpr].value, Some(true))

  test("'and' has higher precedence than 'or': a or b and c == a or (b and c)"):
    // false or true and false -> false or (true and false)
    val expr = parseExpr("false or true and false")
    assert(expr.isInstanceOf[LogicExpr])
    val or = expr.asInstanceOf[LogicExpr]
    assertEquals(or.operator.tokenType, TokenType.OR)
    assertEquals(or.left.asInstanceOf[LiteralExpr].value, Some(false))
    // right side is (true and false)
    assert(or.right.isInstanceOf[LogicExpr])
    val and = or.right.asInstanceOf[LogicExpr]
    assertEquals(and.operator.tokenType, TokenType.AND)
    assertEquals(and.left.asInstanceOf[LiteralExpr].value, Some(true))
    assertEquals(and.right.asInstanceOf[LiteralExpr].value, Some(false))

  test("'and' is left-associative: a and b and c == (a and b) and c"):
    val expr = parseExpr("true and false and true")
    assert(expr.isInstanceOf[LogicExpr])
    val right = expr.asInstanceOf[LogicExpr]
    assertEquals(right.operator.tokenType, TokenType.AND)
    assertEquals(right.right.asInstanceOf[LiteralExpr].value, Some(true))
    val left = right.left.asInstanceOf[LogicExpr]
    assertEquals(left.operator.tokenType, TokenType.AND)
    assertEquals(left.left.asInstanceOf[LiteralExpr].value, Some(true))
    assertEquals(left.right.asInstanceOf[LiteralExpr].value, Some(false))

  test("'or' is left-associative: a or b or c == (a or b) or c"):
    val expr = parseExpr("false or false or true")
    assert(expr.isInstanceOf[LogicExpr])
    val right = expr.asInstanceOf[LogicExpr]
    assertEquals(right.operator.tokenType, TokenType.OR)
    assertEquals(right.right.asInstanceOf[LiteralExpr].value, Some(true))
    val left = right.left.asInstanceOf[LogicExpr]
    assertEquals(left.operator.tokenType, TokenType.OR)

  test("equality has higher precedence than 'and': a == b and c == (a == b) and c"):
    // grammar order: logicOr -> logicAnd -> equality, so equality binds tighter
    // true == true and false  ->  (true == true) and false
    val expr = parseExpr("true == true and false")
    assert(expr.isInstanceOf[LogicExpr])
    val and = expr.asInstanceOf[LogicExpr]
    assertEquals(and.operator.tokenType, TokenType.AND)
    assertEquals(and.right.asInstanceOf[LiteralExpr].value, Some(false))
    assert(and.left.isInstanceOf[BinaryExpr])
    assertEquals(and.left.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.EQUAL_EQUAL)

  test("logical operators work with grouped expressions"):
    val expr = parseExpr("(1 == 1) and (2 == 2)")
    assert(expr.isInstanceOf[LogicExpr])
    val logic = expr.asInstanceOf[LogicExpr]
    assertEquals(logic.operator.tokenType, TokenType.AND)
    assert(logic.left.isInstanceOf[GroupingExpr])
    assert(logic.right.isInstanceOf[GroupingExpr])

  test("'!expr and ...' — unary binds tighter than 'and'"):
    val expr = parseExpr("!true and false")
    assert(expr.isInstanceOf[LogicExpr])
    val logic = expr.asInstanceOf[LogicExpr]
    assertEquals(logic.operator.tokenType, TokenType.AND)
    assert(logic.left.isInstanceOf[UnaryExpr])
    assertEquals(logic.left.asInstanceOf[UnaryExpr].operator.tokenType, TokenType.BANG)

  // ─── Function declarations ─────────────────────────────────────────

  test("parse function declaration with no parameters"):
    val stmts = parseProgram("fun greet() { print 1; }")
    assertEquals(stmts.length, 1)
    assert(stmts.head.isInstanceOf[FunDecl])
    val fn = stmts.head.asInstanceOf[FunDecl]
    assertEquals(fn.name.lexeme, "greet")
    assertEquals(fn.params.length, 0)
    assertEquals(fn.body.length, 1)
    assert(fn.body.head.isInstanceOf[PrintStmt])

  test("parse function declaration with one parameter"):
    val stmts = parseProgram("fun double(n) { return n; }")
    val fn = stmts.head.asInstanceOf[FunDecl]
    assertEquals(fn.name.lexeme, "double")
    assertEquals(fn.params.length, 1)
    assertEquals(fn.params(0).lexeme, "n")

  test("parse function declaration with multiple parameters"):
    val stmts = parseProgram("fun add(a, b, c) { return a; }")
    val fn = stmts.head.asInstanceOf[FunDecl]
    assertEquals(fn.name.lexeme, "add")
    assertEquals(fn.params.length, 3)
    assertEquals(fn.params(0).lexeme, "a")
    assertEquals(fn.params(1).lexeme, "b")
    assertEquals(fn.params(2).lexeme, "c")

  test("parse function with empty body"):
    val stmts = parseProgram("fun empty() { }")
    val fn = stmts.head.asInstanceOf[FunDecl]
    assertEquals(fn.name.lexeme, "empty")
    assertEquals(fn.body.length, 0)

  test("parse function with multiple statements in body"):
    val stmts = parseProgram("fun f() { var x = 1; print x; return x; }")
    val fn = stmts.head.asInstanceOf[FunDecl]
    assertEquals(fn.body.length, 3)
    assert(fn.body(0).isInstanceOf[VarDecl])
    assert(fn.body(1).isInstanceOf[PrintStmt])
    assert(fn.body(2).isInstanceOf[ReturnStmt])

  test("parse nested function declarations"):
    val stmts = parseProgram("fun outer() { fun inner() { return 1; } }")
    val outer = stmts.head.asInstanceOf[FunDecl]
    assertEquals(outer.name.lexeme, "outer")
    assertEquals(outer.body.length, 1)
    val inner = outer.body.head.asInstanceOf[FunDecl]
    assertEquals(inner.name.lexeme, "inner")

  test("parse multiple function declarations"):
    val stmts = parseProgram("fun a() { } fun b() { } fun c() { }")
    assertEquals(stmts.length, 3)
    assertEquals(stmts(0).asInstanceOf[FunDecl].name.lexeme, "a")
    assertEquals(stmts(1).asInstanceOf[FunDecl].name.lexeme, "b")
    assertEquals(stmts(2).asInstanceOf[FunDecl].name.lexeme, "c")

  // ─── Function declaration errors ───────────────────────────────────

  test("error: fun without name"):
    val ex = intercept[RuntimeException] { parseProgram("fun () { }") }
    assert(clue(ex.getMessage).contains("Expected function name"))

  test("error: fun without '(' after name"):
    val ex = intercept[RuntimeException] { parseProgram("fun foo { }") }
    assert(clue(ex.getMessage).contains("Expected '('"))

  test("error: fun without ')' after params"):
    val ex = intercept[RuntimeException] { parseProgram("fun foo(a, b { }") }
    assert(clue(ex.getMessage).contains("Expected ')'"))

  test("error: fun without '{' before body"):
    val ex = intercept[RuntimeException] { parseProgram("fun foo() return 1;") }
    assert(clue(ex.getMessage).contains("Expected '{'"))

  test("error: fun parameter not an identifier"):
    val ex = intercept[RuntimeException] { parseProgram("fun foo(123) { }") }
    assert(clue(ex.getMessage).contains("Expected parameter name"))

  // ─── Return statements ─────────────────────────────────────────────

  test("parse return with value"):
    val stmts = parseProgram("return 42;")
    assertEquals(stmts.length, 1)
    assert(stmts.head.isInstanceOf[ReturnStmt])
    val ret = stmts.head.asInstanceOf[ReturnStmt]
    assert(ret.value.isDefined)
    assert(ret.value.get.isInstanceOf[LiteralExpr])
    assertEquals(ret.value.get.asInstanceOf[LiteralExpr].value, Some(42.0))

  test("parse return without value"):
    val stmts = parseProgram("return;")
    val ret = stmts.head.asInstanceOf[ReturnStmt]
    assertEquals(ret.value, None)

  test("parse return with expression"):
    val stmts = parseProgram("return 1 + 2;")
    val ret = stmts.head.asInstanceOf[ReturnStmt]
    assert(ret.value.get.isInstanceOf[BinaryExpr])

  test("parse return with string"):
    val stmts = parseProgram("return \"hello\";")
    val ret = stmts.head.asInstanceOf[ReturnStmt]
    assertEquals(ret.value.get.asInstanceOf[LiteralExpr].value, Some("hello"))

  test("error: return without semicolon"):
    val ex = intercept[RuntimeException] { parseProgram("return 42") }
    assert(clue(ex.getMessage).contains("Expected ';' after return"))

  // ─── Call expressions ──────────────────────────────────────────────

  test("parse simple function call with no arguments"):
    val expr = parseExpr("foo()")
    assert(expr.isInstanceOf[CallExpr])
    val call = expr.asInstanceOf[CallExpr]
    assert(call.callee.isInstanceOf[VariableExpr])
    assertEquals(call.callee.asInstanceOf[VariableExpr].name.lexeme, "foo")
    assertEquals(call.arguments.length, 0)

  test("parse function call with one argument"):
    val expr = parseExpr("foo(1)")
    val call = expr.asInstanceOf[CallExpr]
    assertEquals(call.arguments.length, 1)
    assertEquals(call.arguments.head.asInstanceOf[LiteralExpr].value, Some(1.0))

  test("parse function call with multiple arguments"):
    val expr = parseExpr("foo(1, 2, 3)")
    val call = expr.asInstanceOf[CallExpr]
    assertEquals(call.arguments.length, 3)
    assertEquals(call.arguments(0).asInstanceOf[LiteralExpr].value, Some(1.0))
    assertEquals(call.arguments(1).asInstanceOf[LiteralExpr].value, Some(2.0))
    assertEquals(call.arguments(2).asInstanceOf[LiteralExpr].value, Some(3.0))

  test("parse function call with expression arguments"):
    val expr = parseExpr("foo(1 + 2, 3 * 4)")
    val call = expr.asInstanceOf[CallExpr]
    assertEquals(call.arguments.length, 2)
    assert(call.arguments(0).isInstanceOf[BinaryExpr])
    assert(call.arguments(1).isInstanceOf[BinaryExpr])

  test("parse chained function calls"):
    // foo()() → CallExpr(CallExpr(foo, []), [])
    val expr = parseExpr("foo()()")
    assert(expr.isInstanceOf[CallExpr])
    val outer = expr.asInstanceOf[CallExpr]
    assertEquals(outer.arguments.length, 0)
    assert(outer.callee.isInstanceOf[CallExpr])
    val inner = outer.callee.asInstanceOf[CallExpr]
    assertEquals(inner.arguments.length, 0)
    assert(inner.callee.isInstanceOf[VariableExpr])
    assertEquals(inner.callee.asInstanceOf[VariableExpr].name.lexeme, "foo")

  test("parse chained call with arguments"):
    // foo(1)(2) → CallExpr(CallExpr(foo, [1]), [2])
    val expr = parseExpr("foo(1)(2)")
    val outer = expr.asInstanceOf[CallExpr]
    assertEquals(outer.arguments.length, 1)
    assertEquals(outer.arguments.head.asInstanceOf[LiteralExpr].value, Some(2.0))
    val inner = outer.callee.asInstanceOf[CallExpr]
    assertEquals(inner.arguments.length, 1)
    assertEquals(inner.arguments.head.asInstanceOf[LiteralExpr].value, Some(1.0))

  test("parse function call in expression statement"):
    val stmts = parseProgram("foo(1, 2);")
    assertEquals(stmts.length, 1)
    assert(stmts.head.isInstanceOf[ExpressionStmt])
    val expr = stmts.head.asInstanceOf[ExpressionStmt].expr
    assert(expr.isInstanceOf[CallExpr])

  test("parse function call as print argument"):
    val stmts = parseProgram("print foo();")
    assert(stmts.head.isInstanceOf[PrintStmt])
    val expr = stmts.head.asInstanceOf[PrintStmt].expr
    assert(expr.isInstanceOf[CallExpr])

  test("error: function call without closing paren"):
    val ex = intercept[RuntimeException] { parseExpr("foo(1, 2") }
    assert(clue(ex.getMessage).contains("Expected ')'"))

  // ─── Function toString ─────────────────────────────────────────────

  test("toString for FunDecl"):
    val stmts = parseProgram("fun add(a, b) { return a; }")
    val fn = stmts.head.asInstanceOf[FunDecl]
    val s = fn.toString
    assert(s.contains("FUN"))
    assert(s.contains("add"))
    assert(s.contains("a"))
    assert(s.contains("b"))

  test("toString for ReturnStmt with value"):
    val stmts = parseProgram("return 42;")
    val ret = stmts.head.asInstanceOf[ReturnStmt]
    assert(ret.toString.contains("RETURN"))

  test("toString for ReturnStmt without value"):
    val stmts = parseProgram("return;")
    val ret = stmts.head.asInstanceOf[ReturnStmt]
    assert(ret.toString.contains("RETURN"))
    assert(ret.toString.contains("NIL"))

  test("toString for CallExpr"):
    val expr = parseExpr("foo(1, 2)")
    assert(expr.toString.contains("foo"))
    assert(expr.toString.contains("1.0"))
    assert(expr.toString.contains("2.0"))

