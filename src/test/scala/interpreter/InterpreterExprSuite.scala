class InterpreterExprSuite extends munit.FunSuite, InterpreterHelpers:

  // ─── Literals ────────────────────────────────────────────────────────

  List(
    ("42",  42.0),
    ("123", 123.0),
    ("0",   0.0),
  ).foreach: (expr, expected) =>
    test(s"number literal $expr"):
      assertEquals(eval(expr), expected)

  test("string literal"):
    assertEquals(eval("\"aaa\""), "aaa")

  List(
    ("true",  true),
    ("false", false),
  ).foreach: (expr, expected) =>
    test(s"boolean literal $expr"):
      assertEquals(eval(expr), expected)

  test("nil literal"):
    assertEquals(eval("nil"), null)

  // ─── Unary ───────────────────────────────────────────────────────────

  List(
    ("-42",    -42.0),
    ("-(-42)", 42.0),
    ("--42",   42.0),
  ).foreach: (expr, expected) =>
    test(s"unary minus: $expr"):
      assertEquals(eval(expr), expected)

  List(
    ("!true",  false),
    ("!false", true),
    ("!nil",   true),
  ).foreach: (expr, expected) =>
    test(s"logical not: $expr"):
      assertEquals(eval(expr), expected)

  // ─── Arithmetic ──────────────────────────────────────────────────────

  List(
    ("0 + 1",   1.0),
    ("2 + 2",   4.0),
    ("4 - 2",   2.0),
    ("1.5 * 2", 3.0),
    ("5 * 5",   25.0),
    ("1 * 0",   0.0),
    ("8 / 2",   4.0),
    ("4 % 3",   1.0),
    ("10 % 4",  2.0),
    ("8 % 2",   0.0),
  ).foreach: (expr, expected) =>
    test(s"arithmetic: $expr = $expected"):
      assertEquals(eval(expr), expected)

  test("precedence: 1 + 2 * 3 - 4 + 2 = 5"):
    assertEquals(eval("1 + 2 * 3 - 4 + 2"), 5.0)

  test("grouping: 3 * (3 - 1) = 6"):
    assertEquals(eval("3 * (3 - 1)"), 6.0)

  test("complex grouping: ((1 + 2) * (3 + 4)) / 3 = 7"):
    assertEquals(eval("((1 + 2) * (3 + 4)) / 3"), 7.0)

  test("6 / 3 - 1 = 1"):
    assertEquals(eval("6 / 3 - 1"), 1.0)

  test("((((0)))) = 0"):
    assertEquals(eval("((((0))))"), 0.0)

  test("string concatenation"):
    assertEquals(eval("\"a\" + \"b\""), "ab")

  // ─── Comparisons ─────────────────────────────────────────────────────

  List(
    ("3 > 2",            true),
    ("3 > 4",            false),
    ("3 >= 3",           true),
    ("3 >= 4",           false),
    ("1 < 2",            true),
    ("2 < 1",            false),
    ("2 <= 2",           true),
    ("3 == 3",           true),
    ("3 == 4",           false),
    ("3 != 4",           true),
    ("3 != 3",           false),
    ("\"aa\" == \"aa\"", true),
    ("\"ab\" == \"aa\"", false),
  ).foreach: (expr, expected) =>
    test(s"comparison: $expr"):
      assertEquals(eval(expr), expected)

  // ─── Errors ──────────────────────────────────────────────────────────

  List(
    ("\"aaa\" + 5",       "'+' with mixed types"),
    ("-\"aaa\"",          "unary '-' on string"),
    ("\"aaa\" - \"bbb\"", "'-' on two strings"),
  ).foreach: (expr, description) =>
    test(s"error: $description throws"):
      intercept[RuntimeException](eval(expr))

  test("error: division by zero"):
    val ex = intercept[RuntimeException](eval("5 / 0"))
    assert(ex.getMessage.contains("Division by zero"))
