import Expr.*
import Stmt.*

class ParserStmtSuite extends munit.FunSuite, ParserHelpers:

  // ─── Basic statements ────────────────────────────────────────────────

  test("parse empty input returns empty program"):
    assertEquals(parseProgram("").length, 0)

  test("parse expression statement"):
    val stmts = parseProgram("123;")
    assertEquals(stmts.length, 1)
    assertEquals(stmts.head.asInstanceOf[ExpressionStmt].expr.asInstanceOf[LiteralExpr].value, Some(123.0))

  test("parse multiple expression statements"):
    val stmts = parseProgram("1; 2;")
    assertEquals(stmts.length, 2)
    assert(stmts(0).isInstanceOf[ExpressionStmt])
    assert(stmts(1).isInstanceOf[ExpressionStmt])

  test("parse print statement"):
    val expr = parseProgram("print \"hola\";").head.asInstanceOf[PrintStmt].expr
    assertEquals(expr.asInstanceOf[LiteralExpr].value, Some("hola"))

  test("parse variable declaration with initializer"):
    val decl = parseProgram("var x = 5;").head.asInstanceOf[VarDecl]
    assertEquals(decl.name, "x")
    assertEquals(decl.initializer.get.asInstanceOf[LiteralExpr].value, Some(5.0))

  test("parse variable declaration without initializer"):
    val decl = parseProgram("var x;").head.asInstanceOf[VarDecl]
    assertEquals(decl.name, "x")
    assertEquals(decl.initializer, None)

  test("parse variable declaration with expression initializer"):
    val decl = parseProgram("var z = 2 + 3;").head.asInstanceOf[VarDecl]
    assert(decl.initializer.get.isInstanceOf[BinaryExpr])

  test("parse empty block statement"):
    assertEquals(parseProgram("{}").head.asInstanceOf[BlockStmt].statements.length, 0)

  test("parse block statement with mixed statements"):
    val block = parseProgram("{ print 1; var x = 2; x; }").head.asInstanceOf[BlockStmt]
    assert(block.statements(0).isInstanceOf[PrintStmt])
    assert(block.statements(1).isInstanceOf[VarDecl])
    assert(block.statements(2).isInstanceOf[ExpressionStmt])

  test("parse nested block statements"):
    val inner = parseProgram("{ { var x = 1; } }").head.asInstanceOf[BlockStmt]
      .statements(0).asInstanceOf[BlockStmt]
    assert(inner.statements(0).isInstanceOf[VarDecl])

  test("parse assignment expression statement"):
    val expr = parseProgram("x = 7;").head.asInstanceOf[ExpressionStmt].expr.asInstanceOf[AssignmentExpr]
    assertEquals(expr.name.lexeme, "x")
    assertEquals(expr.value.asInstanceOf[LiteralExpr].value, Some(7.0))

  test("parse block with var decl then assignment"):
    val block = parseProgram("{ var a = 1; a = 2; }").head.asInstanceOf[BlockStmt]
    assertEquals(block.statements(1).asInstanceOf[ExpressionStmt].expr.asInstanceOf[AssignmentExpr].name.lexeme, "a")

  test("parse variable shadowing across nested blocks"):
    val outer = parseProgram("{ var x = 1; { var x = 2; } }").head.asInstanceOf[BlockStmt]
    assertEquals(outer.statements(1).asInstanceOf[BlockStmt].statements(0).asInstanceOf[VarDecl].name, "x")

  test("error: assignment to undeclared variable parses fine"):
    assert(parseProgram("y = 42;").head.asInstanceOf[ExpressionStmt].expr.isInstanceOf[AssignmentExpr])

  // ─── Statement errors ────────────────────────────────────────────────

  List(
    ("{ var x = 1;", "Expected '}'",                           "unterminated block"),
    ("123",          "Expected ';' after expression",           "expression statement without semicolon"),
    ("print 1",      "Expected ';' after value to print",       "print statement without semicolon"),
    ("var = 1;",     "Expected variable name",                  "variable declaration without variable name"),
    ("var x = 1",    "Expected ';' after variable declaration", "variable declaration without semicolon"),
  ).foreach: (source, fragment, description) =>
    test(s"error: $description"):
      val ex = intercept[RuntimeException](parseProgram(source))
      assert(clue(ex.getMessage).contains(fragment))

  // ─── if statements ───────────────────────────────────────────────────

  test("parse if with then branch only"):
    val ifStmt = parseProgram("if (true) print 1;").head.asInstanceOf[IfStmt]
    assertEquals(ifStmt.condition.asInstanceOf[LiteralExpr].value, Some(true))
    assert(ifStmt.thenBranch.isInstanceOf[PrintStmt])
    assertEquals(ifStmt.elseBranch, None)

  test("parse if-else"):
    val ifStmt = parseProgram("if (true) print 1; else print 2;").head.asInstanceOf[IfStmt]
    assert(ifStmt.elseBranch.get.isInstanceOf[PrintStmt])

  test("parse if with block body"):
    val ifStmt = parseProgram("if (true) { print 1; }").head.asInstanceOf[IfStmt]
    assert(ifStmt.thenBranch.isInstanceOf[BlockStmt])

  test("parse if-else with block bodies"):
    val ifStmt = parseProgram("if (false) { print 1; } else { print 2; }").head.asInstanceOf[IfStmt]
    assert(ifStmt.elseBranch.get.isInstanceOf[BlockStmt])

  test("parse nested if-else (dangling else binds to nearest if)"):
    val outer = parseProgram("if (true) if (false) print 1; else print 2;").head.asInstanceOf[IfStmt]
    assertEquals(outer.elseBranch, None)
    assert(outer.thenBranch.asInstanceOf[IfStmt].elseBranch.nonEmpty)

  test("parse if condition is arbitrary expression"):
    assert(parseProgram("if (1 + 2 > 0) print 1;").head.asInstanceOf[IfStmt].condition.isInstanceOf[BinaryExpr])

  List(
    ("if true print 1;",  "Expected '('", "if without '('"),
    ("if (true print 1;", "Expected ')'", "if without ')'"),
  ).foreach: (source, fragment, description) =>
    test(s"error: $description"):
      val ex = intercept[RuntimeException](parseProgram(source))
      assert(clue(ex.getMessage).contains(fragment))

  // ─── while statements ────────────────────────────────────────────────

  test("parse while statement"):
    val w = parseProgram("while (false) print 1;").head.asInstanceOf[WhileStmt]
    assertEquals(w.condition.asInstanceOf[LiteralExpr].value, Some(false))
    assert(w.body.isInstanceOf[PrintStmt])

  test("parse while with block body"):
    assert(parseProgram("while (true) { print 1; }").head.asInstanceOf[WhileStmt].body.isInstanceOf[BlockStmt])

  test("parse while with compound condition"):
    val w = parseProgram("while (x > 0) print x;").head.asInstanceOf[WhileStmt]
    assertEquals(w.condition.asInstanceOf[BinaryExpr].operator.tokenType, TokenType.GREATER)

  List(
    ("while true print 1;",  "Expected '('", "while without '('"),
    ("while (true print 1;", "Expected ')'", "while without ')'"),
  ).foreach: (source, fragment, description) =>
    test(s"error: $description"):
      val ex = intercept[RuntimeException](parseProgram(source))
      assert(clue(ex.getMessage).contains(fragment))

  // ─── for statements (desugared to while) ─────────────────────────────

  test("for loop desugars to BlockStmt(VarDecl, WhileStmt)"):
    val outer = parseProgram("for (var i = 0; i < 3; i = i + 1) print i;").head.asInstanceOf[BlockStmt]
    assertEquals(outer.statements(0).asInstanceOf[VarDecl].name, "i")
    val body = outer.statements(1).asInstanceOf[WhileStmt].body.asInstanceOf[BlockStmt]
    assert(body.statements(0).isInstanceOf[PrintStmt])
    assert(body.statements(1).asInstanceOf[ExpressionStmt].expr.isInstanceOf[AssignmentExpr])

  test("for with expression initializer desugars to BlockStmt"):
    val outer = parseProgram("for (i = 0; i < 3; i = i + 1) print i;").head.asInstanceOf[BlockStmt]
    assert(outer.statements(0).asInstanceOf[ExpressionStmt].expr.isInstanceOf[AssignmentExpr])

  test("for with no initializer desugars to WhileStmt directly"):
    assert(parseProgram("for (; i < 3; i = i + 1) print i;").head.isInstanceOf[WhileStmt])

  test("for with no condition uses true"):
    val outer = parseProgram("for (var i = 0;; i = i + 1) print i;").head.asInstanceOf[BlockStmt]
    assertEquals(outer.statements(1).asInstanceOf[WhileStmt].condition.asInstanceOf[LiteralExpr].value, Some(true))

  test("for with no increment: body is not wrapped in extra block"):
    val outer = parseProgram("for (var i = 0; i < 3;) print i;").head.asInstanceOf[BlockStmt]
    assert(outer.statements(1).asInstanceOf[WhileStmt].body.isInstanceOf[PrintStmt])

  test("for with block body"):
    val outer = parseProgram("for (var i = 0; i < 3; i = i + 1) { print i; }").head.asInstanceOf[BlockStmt]
    val body = outer.statements(1).asInstanceOf[WhileStmt].body.asInstanceOf[BlockStmt]
    assert(body.statements(0).isInstanceOf[BlockStmt])
    assert(body.statements(1).isInstanceOf[ExpressionStmt])

  List(
    ("for var i = 0; i < 3; i = i+1) print i;", "Expected '('",                        "for without '('"),
    ("for (var i = 0; i < 3 i = i+1) print i;", "Expected ';' after for loop condition", "for missing ';' after condition"),
    ("for (var i = 0; i < 3; i = i+1 print i;", "Expected ')' after for loop clauses",  "for missing ')'"),
  ).foreach: (source, fragment, description) =>
    test(s"error: $description"):
      val ex = intercept[RuntimeException](parseProgram(source))
      assert(clue(ex.getMessage).contains(fragment))
