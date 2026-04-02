import Expr.*
import java.io.ByteArrayOutputStream
import java.io.PrintStream

class InterpreterSuite extends munit.FunSuite:

  private def eval(source: String): Any =
    val tokens = Array.from(Scanner(source).scan())
    val expr = Parser(tokens).expression()
    Interpreter().evaluate(expr)

  private def runProgram(source: String): Unit =
    val tokens = Array.from(Scanner(source).scan())
    val stmts = Parser(tokens).parse()
    Interpreter().interpret(stmts)

  private def runProgramAndCaptureStdOut(source: String): String =
    val out = ByteArrayOutputStream()
    Console.withOut(PrintStream(out)) {
      runProgram(source)
    }
    out.toString("UTF-8")

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

  // ─── Variables ───────────────────────────────────────────────────────

  test("var declaration and read"):
    val interp = Interpreter()
    val stmts = Parser(Array.from(Scanner("var x = 42;").scan())).parse()
    interp.interpret(stmts)
    assertEquals(interp.evaluate(Parser(Array.from(Scanner("x").scan())).expression()), 42.0)

  test("var declared without initializer is nil"):
    val interp = Interpreter()
    val stmts = Parser(Array.from(Scanner("var x;").scan())).parse()
    interp.interpret(stmts)
    assertEquals(interp.evaluate(Parser(Array.from(Scanner("x").scan())).expression()), null)

  test("var assignment updates value"):
    val interp = Interpreter()
    val stmts = Parser(Array.from(Scanner("var x = 1; x = 99;").scan())).parse()
    interp.interpret(stmts)
    assertEquals(interp.evaluate(Parser(Array.from(Scanner("x").scan())).expression()), 99.0)

  test("reading undeclared variable throws"):
    val interp = Interpreter()
    intercept[RuntimeException](interp.evaluate(Parser(Array.from(Scanner("z").scan())).expression()))

  test("assigning undeclared variable throws"):
    val interp = Interpreter()
    val stmts = Parser(Array.from(Scanner("z = 1;").scan())).parse()
    intercept[RuntimeException](interp.interpret(stmts))

  // ─── Blocks and scoping ──────────────────────────────────────────────

  test("block introduces new scope, inner var hidden after block"):
    val interp = Interpreter()
    val stmts = Parser(Array.from(Scanner("var x = 1; { var x = 2; } ").scan())).parse()
    interp.interpret(stmts)
    // outer x is still 1
    assertEquals(interp.evaluate(Parser(Array.from(Scanner("x").scan())).expression()), 1.0)

  test("block can read outer variable"):
    val out = runProgramAndCaptureStdOut("var x = 10; { print x; }")
    assertEquals(out.trim, "10.0")

  test("block can modify outer variable"):
    val interp = Interpreter()
    val stmts = Parser(Array.from(Scanner("var x = 1; { x = 2; }").scan())).parse()
    interp.interpret(stmts)
    assertEquals(interp.evaluate(Parser(Array.from(Scanner("x").scan())).expression()), 2.0)

  test("block mutation is visible after block: var x = 2; { x = 3; } print x;"):
    val out = runProgramAndCaptureStdOut("var x = 2; { x = 3; } print x;")
    assertEquals(out.trim, "3.0")

  test("inner var shadows outer var inside block"):
    val out = runProgramAndCaptureStdOut("var x = 1; { var x = 99; print x; }")
    assertEquals(out.trim, "99.0")

  test("outer var unchanged after inner shadow"):
    val out = runProgramAndCaptureStdOut("var x = 1; { var x = 99; } print x;")
    assertEquals(out.trim, "1.0")

  test("nested blocks each have own scope"):
    val out = runProgramAndCaptureStdOut(
      "var a = 1; { var b = 2; { var c = 3; print a; print b; print c; } }"
    )
    assertEquals(out.trim.linesIterator.toList, List("1.0", "2.0", "3.0"))

  test("modification in nested block visible in enclosing block"):
    val out = runProgramAndCaptureStdOut(
      "var x = 0; { { x = 42; } print x; }"
    )
    assertEquals(out.trim, "42.0")

  test("var in inner block not accessible after it closes"):
    val interp = Interpreter()
    val stmts = Parser(Array.from(Scanner("{ var inner = 7; }").scan())).parse()
    interp.interpret(stmts)
    intercept[RuntimeException](
      interp.evaluate(Parser(Array.from(Scanner("inner").scan())).expression())
    )

  test("var computed from expression in initializer"):
    val out = runProgramAndCaptureStdOut("var y = 2 + 3; print y;")
    assertEquals(out.trim, "5.0")

  test("print statement prints var value"):
    val out = runProgramAndCaptureStdOut("var msg = \"hello\"; print msg;")
    assertEquals(out.trim, "hello")

  // ─── Logical and / or ────────────────────────────────────────────────

  // Basic truth tables
  test("true and true = true"):
    assertEquals(eval("true and true"),  true)
  test("true and false = false"):
    assertEquals(eval("true and false"), false)
  test("false and true = false"):
    assertEquals(eval("false and true"), false)
  test("false and false = false"):
    assertEquals(eval("false and false"), false)
  test("true or true = true"):
    assertEquals(eval("true or true"),  true)
  test("true or false = true"):
    assertEquals(eval("true or false"), true)
  test("false or true = true"):
    assertEquals(eval("false or true"), true)
  test("false or false = false"):
    assertEquals(eval("false or false"), false)

  // Returns the actual short-circuit value (Lox-style: returns operand, not bool)
  test("'and' returns right operand when left is truthy"):
    assertEquals(eval("1 and 2"), 2.0)

  test("'and' returns left operand when left is falsy"):
    assertEquals(eval("nil and 2"), null)

  test("'or' returns left operand when left is truthy"):
    assertEquals(eval("1 or 2"), 1.0)

  test("'or' returns right operand when left is falsy"):
    assertEquals(eval("nil or 2"), 2.0)

  // Short-circuit: right side must not be evaluated
  test("'and' short-circuits: false and <error> does not throw"):
    assertEquals(eval("false and (1 / 0)"), false)

  test("'or' short-circuits: true or <error> does not throw"):
    assertEquals(eval("true or (1 / 0)"), true)

  // Precedence: 'and' binds tighter than 'or'
  test("'and' has higher precedence than 'or'"):
    // false or true and false -> false or (true and false) -> false or false -> false
    assertEquals(eval("false or true and false"), false)

  test("'and' higher prec than 'or' (2)"):
    // true or false and false -> true or (false and false) -> true or false -> true
    assertEquals(eval("true or false and false"), true)

  // Chaining
  test("chained 'and': true and true and true"):
    assertEquals(eval("true and true and true"), true)

  test("chained 'or': false or false or true"):
    assertEquals(eval("false or false or true"), true)

  test("mixed chain: true and false or true"):
    // (true and false) or true -> false or true -> true
    assertEquals(eval("true and false or true"), true)

  // Interaction with '!' (unary binds tighter)
  test("!true and false"):
    // (!true) and false -> false and false -> false
    assertEquals(eval("!true and false"), false)

  test("!false or false"):
    // (!false) or false -> true or false -> true
    assertEquals(eval("!false or false"), true)

  // Interaction with comparison operators
  test("(1 < 2) and (3 > 0)"):
    assertEquals(eval("1 < 2 and 3 > 0"), true)

  test("(1 > 2) or (3 > 0)"):
    assertEquals(eval("1 > 2 or 3 > 0"), true)

  // Nil and truthy semantics
  test("nil is falsy in 'and'"):
    assertEquals(eval("nil and true"), null)

  test("nil is falsy in 'or'"):
    assertEquals(eval("nil or 42"), 42.0)

  test("0 is truthy (only nil and false are falsy)"):
    assertEquals(eval("0 and true"), true)

  test("empty string is truthy"):
    assertEquals(eval("\"\" and true"), true)

  // ─── if statement ────────────────────────────────────────────────────

  test("if true executes then branch"):
    val out = runProgramAndCaptureStdOut("if (true) print 1;")
    assertEquals(out.trim, "1.0")

  test("if false skips then branch"):
    val out = runProgramAndCaptureStdOut("if (false) print 1;")
    assertEquals(out.trim, "")

  test("if-else: true takes then branch"):
    val out = runProgramAndCaptureStdOut("if (true) print 1; else print 2;")
    assertEquals(out.trim, "1.0")

  test("if-else: false takes else branch"):
    val out = runProgramAndCaptureStdOut("if (false) print 1; else print 2;")
    assertEquals(out.trim, "2.0")

  test("if with block body"):
    val out = runProgramAndCaptureStdOut("if (true) { print 1; print 2; }")
    assertEquals(out.trim.linesIterator.toList, List("1.0", "2.0"))

  test("if-else with block bodies"):
    val out = runProgramAndCaptureStdOut("if (false) { print 1; } else { print 2; print 3; }")
    assertEquals(out.trim.linesIterator.toList, List("2.0", "3.0"))

  test("if with variable condition"):
    val out = runProgramAndCaptureStdOut("var x = 5; if (x > 3) print 1; else print 0;")
    assertEquals(out.trim, "1.0")

  test("if with assignment in branch"):
    val out = runProgramAndCaptureStdOut("var x = 0; if (true) x = 1; print x;")
    assertEquals(out.trim, "1.0")

  test("nested if-else"):
    val out = runProgramAndCaptureStdOut(
      "var x = 2; if (x == 1) print 1; else if (x == 2) print 2; else print 3;"
    )
    assertEquals(out.trim, "2.0")

  test("if with logical condition"):
    val out = runProgramAndCaptureStdOut("if (true and false) print 1; else print 0;")
    assertEquals(out.trim, "0.0")

  test("nil is falsy in if condition"):
    val out = runProgramAndCaptureStdOut("if (nil) print 1; else print 0;")
    assertEquals(out.trim, "0.0")

  test("0 is truthy in if condition"):
    val out = runProgramAndCaptureStdOut("if (0) print 1; else print 0;")
    assertEquals(out.trim, "1.0")

  // ─── while statement ─────────────────────────────────────────────────

  test("while false body never executes"):
    val out = runProgramAndCaptureStdOut("while (false) print 1;")
    assertEquals(out.trim, "")

  test("while counts up to 3"):
    val out = runProgramAndCaptureStdOut(
      "var i = 0; while (i < 3) { print i; i = i + 1; }"
    )
    assertEquals(out.trim.linesIterator.toList, List("0.0", "1.0", "2.0"))

  test("while accumulates sum"):
    val out = runProgramAndCaptureStdOut(
      "var sum = 0; var i = 1; while (i <= 5) { sum = sum + i; i = i + 1; } print sum;"
    )
    assertEquals(out.trim, "15.0")

  test("while with logical condition"):
    val out = runProgramAndCaptureStdOut(
      "var x = 0; while (x < 3 and x >= 0) { x = x + 1; } print x;"
    )
    assertEquals(out.trim, "3.0")

  test("while modifies outer var"):
    val out = runProgramAndCaptureStdOut(
      "var x = 10; while (x > 0) { x = x - 3; } print x;"
    )
    assertEquals(out.trim, "-2.0")

  test("nested while loops"):
    val out = runProgramAndCaptureStdOut(
      "var i = 0; var sum = 0; while (i < 3) { var j = 0; while (j < 3) { sum = sum + 1; j = j + 1; } i = i + 1; } print sum;"
    )
    assertEquals(out.trim, "9.0")

  test("while with if inside"):
    val out = runProgramAndCaptureStdOut(
      "var i = 0; var evens = 0; while (i < 6) { if (i == 0 or i == 2 or i == 4) evens = evens + 1; i = i + 1; } print evens;"
    )
    assertEquals(out.trim, "3.0")

  test("if with while inside (counting up)"):
    val out = runProgramAndCaptureStdOut(
      "var x = 1; if (true) { while (x < 4) x = x + 1; } print x;"
    )
    assertEquals(out.trim, "4.0")

  // ─── for loop ──────────────────────────────────────────────────────

  test("for loop prints 0 1 2"):
    val out = runProgramAndCaptureStdOut(
      "for (var i = 0; i < 3; i = i + 1) print i;"
    )
    assertEquals(out.trim.linesIterator.toList, List("0.0", "1.0", "2.0"))

  test("for loop with block body"):
    val out = runProgramAndCaptureStdOut(
      "for (var i = 0; i < 3; i = i + 1) { print i; }"
    )
    assertEquals(out.trim.linesIterator.toList, List("0.0", "1.0", "2.0"))

  test("for loop var is scoped: i not accessible outside"):
    val interp = Interpreter()
    val stmts = Parser(Array.from(Scanner("for (var i = 0; i < 3; i = i + 1) print i;").scan())).parse()
    interp.interpret(stmts)
    intercept[RuntimeException](interp.evaluate(Parser(Array.from(Scanner("i").scan())).expression()))

  test("for loop computes sum 1..5"):
    val out = runProgramAndCaptureStdOut(
      "var sum = 0; for (var i = 1; i <= 5; i = i + 1) sum = sum + i; print sum;"
    )
    assertEquals(out.trim, "15.0")

  test("for loop counts down"):
    val out = runProgramAndCaptureStdOut(
      "for (var i = 3; i > 0; i = i - 1) print i;"
    )
    assertEquals(out.trim.linesIterator.toList, List("3.0", "2.0", "1.0"))

  test("for loop with no initializer"):
    val out = runProgramAndCaptureStdOut(
      "var i = 0; for (; i < 3; i = i + 1) print i;"
    )
    assertEquals(out.trim.linesIterator.toList, List("0.0", "1.0", "2.0"))

  test("for loop with no increment (manual)"):
    val out = runProgramAndCaptureStdOut(
      "for (var i = 0; i < 3;) { print i; i = i + 1; }"
    )
    assertEquals(out.trim.linesIterator.toList, List("0.0", "1.0", "2.0"))

  test("for loop body never runs when condition is false"):
    val out = runProgramAndCaptureStdOut(
      "for (var i = 5; i < 3; i = i + 1) print i;"
    )
    assertEquals(out.trim, "")

  test("nested for loops: multiplication table"):
    // 3x3 = 9 iterations, each prints product
    val out = runProgramAndCaptureStdOut(
      "for (var i = 1; i <= 3; i = i + 1) { for (var j = 1; j <= 3; j = j + 1) { print i * j; } }"
    )
    assertEquals(
      out.trim.linesIterator.map(_.toDouble).toList,
      List(1,2,3, 2,4,6, 3,6,9).map(_.toDouble)
    )

  test("for loop with if inside"):
    // print only even numbers 0..4
    val out = runProgramAndCaptureStdOut(
      "for (var i = 0; i < 5; i = i + 1) { if (i == 0 or i == 2 or i == 4) print i; }"
    )
    assertEquals(out.trim.linesIterator.toList, List("0.0", "2.0", "4.0"))

  test("for loop with expression initializer (assignment)"):
    val out = runProgramAndCaptureStdOut(
      "var i; for (i = 0; i < 3; i = i + 1) print i;"
    )
    assertEquals(out.trim.linesIterator.toList, List("0.0", "1.0", "2.0"))


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

  // ─── Statements ──────────────────────────────────────────────────────

  test("expression statement executes and produces no output"):
    // Example: "1 + 2;" should evaluate but not print anything
    val output = runProgramAndCaptureStdOut("1 + 2;")
    assertEquals(output, "")

  test("print statement outputs evaluated value"):
    // Example: "print 1 + 2;" -> "3.0\\n"
    val output = runProgramAndCaptureStdOut("print 1 + 2;")
    assertEquals(output, "3.0\n")

  test("variable declaration with initializer defines value"):
    // Example: "var x = 5; print x;" -> "5.0\\n"
    val output = runProgramAndCaptureStdOut("var x = 5; print x;")
    assertEquals(output, "5.0\n")

  test("variable declaration without initializer defines nil (null)"):
    // Example: "var x; print x;" -> "null\\n"
    val output = runProgramAndCaptureStdOut("var x; print x;")
    assertEquals(output, "null\n")

  test("block statement creates inner scope and allows shadowing"):
    // Example: inner x shadows outer x only inside block
    val program = "var x = \"global\"; { var x = \"local\"; print x; } print x;"
    val output = runProgramAndCaptureStdOut(program)
    assertEquals(output, "local\nglobal\n")

  test("block statement can read variable from outer scope"):
    // Example: block can access enclosing env variable
    val output = runProgramAndCaptureStdOut("var x = 10; { print x; }")
    assertEquals(output, "10.0\n")

  test("variables declared in block are not visible outside"):
    // Example: "{ var y = 2; } print y;" -> undefined variable runtime error
    val ex = intercept[RuntimeException] {
      runProgram("{ var y = 2; } print y;")
    }
    assert(clue(ex.getMessage).contains("Undefined variable 'y'"))
