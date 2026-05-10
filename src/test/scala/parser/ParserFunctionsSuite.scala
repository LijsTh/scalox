import Expr.*
import Stmt.*

class ParserFunctionsSuite extends munit.FunSuite, ParserHelpers:

  // ─── Function declarations ────────────────────────────────────────────

  test("parse function declaration with no parameters"):
    val fn = parseProgram("fun greet() { print 1; }").head.asInstanceOf[FunDecl]
    assertEquals(fn.name.lexeme, "greet")
    assertEquals(fn.params.length, 0)
    assert(fn.body.head.isInstanceOf[PrintStmt])

  test("parse function declaration with one parameter"):
    val fn = parseProgram("fun double(n) { return n; }").head.asInstanceOf[FunDecl]
    assertEquals(fn.params(0).lexeme, "n")

  test("parse function declaration with multiple parameters"):
    val fn = parseProgram("fun add(a, b, c) { return a; }").head.asInstanceOf[FunDecl]
    assertEquals(fn.params.map(_.lexeme), List("a", "b", "c"))

  test("parse function with empty body"):
    assertEquals(parseProgram("fun empty() { }").head.asInstanceOf[FunDecl].body.length, 0)

  test("parse function with multiple statements in body"):
    val fn = parseProgram("fun f() { var x = 1; print x; return x; }").head.asInstanceOf[FunDecl]
    assertEquals(fn.body.length, 3)
    assert(fn.body(0).isInstanceOf[VarDecl])
    assert(fn.body(1).isInstanceOf[PrintStmt])
    assert(fn.body(2).isInstanceOf[ReturnStmt])

  test("parse nested function declarations"):
    val outer = parseProgram("fun outer() { fun inner() { return 1; } }").head.asInstanceOf[FunDecl]
    assertEquals(outer.body.head.asInstanceOf[FunDecl].name.lexeme, "inner")

  test("parse multiple function declarations"):
    val stmts = parseProgram("fun a() { } fun b() { } fun c() { }")
    assertEquals(stmts.map(_.asInstanceOf[FunDecl].name.lexeme), List("a", "b", "c"))

  // ─── Function declaration errors ──────────────────────────────────────

  List(
    ("fun () { }",       "Expected function name",   "fun without name"),
    ("fun foo { }",      "Expected '('",             "fun without '(' after name"),
    ("fun foo(a, b { }", "Expected ')'",             "fun without ')' after params"),
    ("fun foo() return 1;", "Expected '{'",          "fun without '{' before body"),
    ("fun foo(123) { }", "Expected parameter name",  "fun parameter not an identifier"),
  ).foreach: (source, fragment, description) =>
    test(s"error: $description"):
      val ex = intercept[RuntimeException](parseProgram(source))
      assert(clue(ex.getMessage).contains(fragment))

  // ─── Return statements ────────────────────────────────────────────────

  test("parse return with value"):
    assertEquals(parseProgram("return 42;").head.asInstanceOf[ReturnStmt].value.get.asInstanceOf[LiteralExpr].value, Some(42.0))

  test("parse return without value"):
    assertEquals(parseProgram("return;").head.asInstanceOf[ReturnStmt].value, None)

  test("parse return with expression"):
    assert(parseProgram("return 1 + 2;").head.asInstanceOf[ReturnStmt].value.get.isInstanceOf[BinaryExpr])

  test("parse return with string"):
    assertEquals(
      parseProgram("return \"hello\";").head.asInstanceOf[ReturnStmt].value.get.asInstanceOf[LiteralExpr].value,
      Some("hello")
    )

  test("error: return without semicolon"):
    val ex = intercept[RuntimeException](parseProgram("return 42"))
    assert(clue(ex.getMessage).contains("Expected ';' after return"))

  // ─── Call expressions ─────────────────────────────────────────────────

  test("parse simple function call with no arguments"):
    val call = parseExpr("foo()").asInstanceOf[CallExpr]
    assertEquals(call.callee.asInstanceOf[VariableExpr].name.lexeme, "foo")
    assertEquals(call.arguments.length, 0)

  test("parse function call with one argument"):
    val call = parseExpr("foo(1)").asInstanceOf[CallExpr]
    assertEquals(call.arguments.head.asInstanceOf[LiteralExpr].value, Some(1.0))

  test("parse function call with multiple arguments"):
    val call = parseExpr("foo(1, 2, 3)").asInstanceOf[CallExpr]
    assertEquals(call.arguments.map(_.asInstanceOf[LiteralExpr].value), List(Some(1.0), Some(2.0), Some(3.0)))

  test("parse function call with expression arguments"):
    val call = parseExpr("foo(1 + 2, 3 * 4)").asInstanceOf[CallExpr]
    assert(call.arguments(0).isInstanceOf[BinaryExpr])
    assert(call.arguments(1).isInstanceOf[BinaryExpr])

  test("parse chained function calls"):
    val outer = parseExpr("foo()()").asInstanceOf[CallExpr]
    assertEquals(outer.callee.asInstanceOf[CallExpr].callee.asInstanceOf[VariableExpr].name.lexeme, "foo")

  test("parse chained call with arguments"):
    val outer = parseExpr("foo(1)(2)").asInstanceOf[CallExpr]
    assertEquals(outer.arguments.head.asInstanceOf[LiteralExpr].value, Some(2.0))
    assertEquals(outer.callee.asInstanceOf[CallExpr].arguments.head.asInstanceOf[LiteralExpr].value, Some(1.0))

  test("parse function call in expression statement"):
    assert(parseProgram("foo(1, 2);").head.asInstanceOf[ExpressionStmt].expr.isInstanceOf[CallExpr])

  test("parse function call as print argument"):
    assert(parseProgram("print foo();").head.asInstanceOf[PrintStmt].expr.isInstanceOf[CallExpr])

  test("error: function call without closing paren"):
    val ex = intercept[RuntimeException](parseExpr("foo(1, 2"))
    assert(clue(ex.getMessage).contains("Expected ')'"))

  // ─── toString ─────────────────────────────────────────────────────────

  test("toString for FunDecl"):
    val s = parseProgram("fun add(a, b) { return a; }").head.asInstanceOf[FunDecl].toString
    assert(s.contains("FUN") && s.contains("add") && s.contains("a") && s.contains("b"))

  test("toString for ReturnStmt with value"):
    assert(parseProgram("return 42;").head.asInstanceOf[ReturnStmt].toString.contains("RETURN"))

  test("toString for ReturnStmt without value"):
    val s = parseProgram("return;").head.asInstanceOf[ReturnStmt].toString
    assert(s.contains("RETURN") && s.contains("NIL"))

  test("toString for CallExpr"):
    val s = parseExpr("foo(1, 2)").toString
    assert(s.contains("foo") && s.contains("1.0") && s.contains("2.0"))
