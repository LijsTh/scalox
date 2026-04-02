import Expr.*

class InterpreterSuite extends munit.FunSuite:

  private def eval(source: String): Any =
    val tokens = Array.from(Scanner(source).scan())
    val expr = Parser(tokens).expression()
    Interpreter().evaluate(expr)

  // ─── Literals ────────────────────────────────────────────────────────

  test("number literal 42"):
    assertEquals(eval("42"), 42.0)

  test("number literal 123"):
    assertEquals(eval("123"), 123.0)

  test("string literal"):
    assertEquals(eval("\"aaa\""), "aaa")

  test("true literal"):
    assertEquals(eval("true"), true)

  test("false literal"):
    assertEquals(eval("false"), false)

  test("nil literal"):
    assertEquals(eval("nil"), null)

  // ─── Unary ───────────────────────────────────────────────────────────

  test("-42 = -42"):
    assertEquals(eval("-42"), -42.0)

  test("-(-42) = 42"):
    assertEquals(eval("-(-42)"), 42.0)

  test("!true = false"):
    assertEquals(eval("!true"), false)

  test("!false = true"):
    assertEquals(eval("!false"), true)

  test("!nil = true"):
    assertEquals(eval("!nil"), true)

  // ─── Grouping ────────────────────────────────────────────────────────

  test("((((0)))) = 0"):
    assertEquals(eval("((((0))))"), 0.0)

  // ─── Arithmetic ──────────────────────────────────────────────────────

  test("0 + 1 = 1"):
    assertEquals(eval("0 + 1"), 1.0)

  test("2 + 2 = 4"):
    assertEquals(eval("2 + 2"), 4.0)

  test("4 - 2 = 2"):
    assertEquals(eval("4 - 2"), 2.0)

  test("1.5 * 2 = 3"):
    assertEquals(eval("1.5 * 2"), 3.0)

  test("5 * 5 = 25"):
    assertEquals(eval("5 * 5"), 25.0)

  test("1 * 0 = 0"):
    assertEquals(eval("1 * 0"), 0.0)

  test("8 / 2 = 4"):
    assertEquals(eval("8 / 2"), 4.0)

  test("1 + 2 * 3 - 4 + 2 = 5 (precedence)"):
    assertEquals(eval("1 + 2 * 3 - 4 + 2"), 5.0)

  test("3 * (3 - 1) = 6"):
    assertEquals(eval("3 * (3 - 1)"), 6.0)

  test("((1 + 2) * (3 + 4)) / 3 = 7"):
    assertEquals(eval("((1 + 2) * (3 + 4)) / 3"), 7.0)

  test("6 / 3 - 1 = 1"):
    assertEquals(eval("6 / 3 - 1"), 1.0)

  // ─── String concatenation ────────────────────────────────────────────

  test("\"a\" + \"b\" = \"ab\""):
    assertEquals(eval("\"a\" + \"b\""), "ab")

  // ─── Comparisons ─────────────────────────────────────────────────────

  test("3 > 2 = true"):
    assertEquals(eval("3 > 2"), true)

  test("3 > 4 = false"):
    assertEquals(eval("3 > 4"), false)

  test("3 >= 3 = true"):
    assertEquals(eval("3 >= 3"), true)

  test("3 >= 4 = false"):
    assertEquals(eval("3 >= 4"), false)

  test("1 < 2 = true"):
    assertEquals(eval("1 < 2"), true)

  test("2 < 1 = false"):
    assertEquals(eval("2 < 1"), false)

  test("2 <= 2 = true"):
    assertEquals(eval("2 <= 2"), true)

  test("3 == 3 = true"):
    assertEquals(eval("3 == 3"), true)

  test("3 == 4 = false"):
    assertEquals(eval("3 == 4"), false)

  test("3 != 4 = true"):
    assertEquals(eval("3 != 4"), true)

  test("3 != 3 = false"):
    assertEquals(eval("3 != 3"), false)

  test("\"aa\" == \"aa\" = true"):
    assertEquals(eval("\"aa\" == \"aa\""), true)

  test("\"ab\" == \"aa\" = false"):
    assertEquals(eval("\"ab\" == \"aa\""), false)

  // ─── Errors ───────────────────────────────────────────────────────────

  test("'+' with mixed types throws"):
    intercept[RuntimeException](eval("\"aaa\" + 5"))

  test("unary '-' on string throws"):
    intercept[RuntimeException](eval("-\"aaa\""))

  test("'-' on two strings throws"):
    intercept[RuntimeException](eval("\"aaa\" - \"bbb\""))

  test("division by zero throws"):
    val ex = intercept[RuntimeException](eval("5 / 0"))
    assert(ex.getMessage.contains("Division by zero"))
