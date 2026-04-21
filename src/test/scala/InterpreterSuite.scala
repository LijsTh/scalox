import Expr.*
import scala.collection.mutable.ArrayBuffer
import java.io.ByteArrayOutputStream
import java.io.PrintStream

class InterpreterSuite extends munit.FunSuite:

  private def eval(source: String): Any =
    val tokens = ArrayBuffer.from(Scanner(source).scan())
    val expr = Parser(tokens).expression()
    val interp = Interpreter()
    interp.evaluate(expr)

  private def runProgram(source: String): Unit =
    val tokens = ArrayBuffer.from(Scanner(source).scan())
    val stmts = Parser(tokens).parse()
    val interp = Interpreter()
    val resolver = Resolver(interp)
    stmts.foreach(resolver.resolve)
    interp.interpret(stmts)

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
    val stmts = Parser(ArrayBuffer.from(Scanner("var x = 42;").scan())).parse()
    interp.interpret(stmts)
    assertEquals(interp.evaluate(Parser(ArrayBuffer.from(Scanner("x").scan())).expression()), 42.0)

  test("var declared without initializer is nil"):
    val interp = Interpreter()
    val stmts = Parser(ArrayBuffer.from(Scanner("var x;").scan())).parse()
    interp.interpret(stmts)
    assertEquals(interp.evaluate(Parser(ArrayBuffer.from(Scanner("x").scan())).expression()), null)

  test("var assignment updates value"):
    val interp = Interpreter()
    val stmts = Parser(ArrayBuffer.from(Scanner("var x = 1; x = 99;").scan())).parse()
    interp.interpret(stmts)
    assertEquals(interp.evaluate(Parser(ArrayBuffer.from(Scanner("x").scan())).expression()), 99.0)

  test("reading undeclared variable throws"):
    val interp = Interpreter()
    intercept[RuntimeException](interp.evaluate(Parser(ArrayBuffer.from(Scanner("z").scan())).expression()))

  test("assigning undeclared variable throws"):
    val interp = Interpreter()
    val stmts = Parser(ArrayBuffer.from(Scanner("z = 1;").scan())).parse()
    intercept[RuntimeException](interp.interpret(stmts))

  // ─── Blocks and scoping ──────────────────────────────────────────────

  test("block introduces new scope, inner var hidden after block"):
    val interp = Interpreter()
    val stmts = Parser(ArrayBuffer.from(Scanner("var x = 1; { var x = 2; } ").scan())).parse()
    interp.interpret(stmts)
    // outer x is still 1
    assertEquals(interp.evaluate(Parser(ArrayBuffer.from(Scanner("x").scan())).expression()), 1.0)

  test("block can read outer variable"):
    val out = runProgramAndCaptureStdOut("var x = 10; { print x; }")
    assertEquals(out.trim, "10.0")

  test("block can modify outer variable"):
    val interp = Interpreter()
    val stmts = Parser(ArrayBuffer.from(Scanner("var x = 1; { x = 2; }").scan())).parse()
    interp.interpret(stmts)
    assertEquals(interp.evaluate(Parser(ArrayBuffer.from(Scanner("x").scan())).expression()), 2.0)

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
    val stmts = Parser(ArrayBuffer.from(Scanner("{ var inner = 7; }").scan())).parse()
    interp.interpret(stmts)
    intercept[RuntimeException](
      interp.evaluate(Parser(ArrayBuffer.from(Scanner("inner").scan())).expression())
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
    val stmts = Parser(ArrayBuffer.from(Scanner("for (var i = 0; i < 3; i = i + 1) print i;").scan())).parse()
    val resolver = Resolver(interp)
    stmts.foreach(resolver.resolve)
    interp.interpret(stmts)
    intercept[RuntimeException](interp.evaluate(Parser(ArrayBuffer.from(Scanner("i").scan())).expression()))

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

  // ─── Functions: basic declaration and calling ────────────────────────

  test("function declaration and call with return value"):
    val out = runProgramAndCaptureStdOut(
      "fun returnthree() { return 3; } print returnthree();"
    )
    assertEquals(out.trim, "3.0")

  test("printing a function shows its representation"):
    val out = runProgramAndCaptureStdOut(
      "fun returnthree() { return 3; } print returnthree;"
    )
    assert(out.trim.contains("fn"))
    assert(out.trim.contains("returnthree"))

  test("function without return statement returns nil"):
    val out = runProgramAndCaptureStdOut(
      "fun noreturn() { } print noreturn();"
    )
    assertEquals(out.trim, "null")

  test("function with empty return returns nil"):
    val out = runProgramAndCaptureStdOut(
      "fun emptyreturn() { return; } print emptyreturn();"
    )
    assertEquals(out.trim, "null")

  test("function with body but no explicit return returns nil"):
    val out = runProgramAndCaptureStdOut(
      "fun greet() { var x = 1; var y = 2; } print greet();"
    )
    assertEquals(out.trim, "null")

  // ─── Functions: parameters and arguments ─────────────────────────────

  test("function with one parameter"):
    val out = runProgramAndCaptureStdOut(
      "fun double(n) { return n * 2; } print double(5);"
    )
    assertEquals(out.trim, "10.0")

  test("function with multiple parameters"):
    val out = runProgramAndCaptureStdOut(
      "fun add(a, b, c) { return a + b + c; } print add(1, 2, 3);"
    )
    assertEquals(out.trim, "6.0")

  test("function parameters are local to the function"):
    val out = runProgramAndCaptureStdOut(
      """var a = "global";
        |fun f(a) { print a; }
        |f("local");
        |print a;""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("local", "global"))

  test("function parameters shadow outer variables"):
    val out = runProgramAndCaptureStdOut(
      "var x = 10; fun f(x) { return x + 1; } print f(20);"
    )
    assertEquals(out.trim, "21.0")

  // ─── Functions: arity errors ─────────────────────────────────────────

  test("too many arguments throws error"):
    val ex = intercept[RuntimeException] {
      runProgram("fun add(a, b, c) { return a + b + c; } add(1,2,3,4);")
    }
    assert(ex.getMessage.contains("Expected 3 arguments but got 4"))

  test("too few arguments throws error"):
    val ex = intercept[RuntimeException] {
      runProgram("fun add(a, b, c) { return a + b + c; } add(1,2);")
    }
    assert(ex.getMessage.contains("Expected 3 arguments but got 2"))

  test("zero-arg function called with arguments throws error"):
    val ex = intercept[RuntimeException] {
      runProgram("fun f() { return 1; } f(42);")
    }
    assert(ex.getMessage.contains("Expected 0 arguments but got 1"))

  // ─── Functions: calling non-callable values ──────────────────────────

  test("calling a string throws error"):
    val ex = intercept[RuntimeException] {
      runProgram("var x = \"x\"; x();")
    }
    assert(ex.getMessage.contains("Can only call functions and classes"))

  test("calling a number throws error"):
    val ex = intercept[RuntimeException] {
      runProgram("var x = 42; x();")
    }
    assert(ex.getMessage.contains("Can only call functions and classes"))

  test("calling nil throws error"):
    val ex = intercept[RuntimeException] {
      runProgram("var x = nil; x();")
    }
    assert(ex.getMessage.contains("Can only call functions and classes"))

  test("calling a boolean throws error"):
    val ex = intercept[RuntimeException] {
      runProgram("var x = true; x();")
    }
    assert(ex.getMessage.contains("Can only call functions and classes"))

  // ─── Functions: return statement behavior ────────────────────────────

  test("return exits function early"):
    val out = runProgramAndCaptureStdOut(
      """fun f() {
        |  print "before";
        |  return;
        |  print "after";
        |}
        |f();""".stripMargin
    )
    assertEquals(out.trim, "before")

  test("return with value exits function early"):
    val out = runProgramAndCaptureStdOut(
      """fun f() {
        |  return 42;
        |  print "unreachable";
        |}
        |print f();""".stripMargin
    )
    assertEquals(out.trim, "42.0")

  test("return inside if branch"):
    val out = runProgramAndCaptureStdOut(
      """fun abs(n) {
        |  if (n < 0) return -n;
        |  return n;
        |}
        |print abs(-5);
        |print abs(3);""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("5.0", "3.0"))

  test("return inside while loop"):
    val out = runProgramAndCaptureStdOut(
      """fun findFirst() {
        |  var i = 0;
        |  while (i < 10) {
        |    if (i == 5) return i;
        |    i = i + 1;
        |  }
        |  return -1;
        |}
        |print findFirst();""".stripMargin
    )
    assertEquals(out.trim, "5.0")

  test("return string value"):
    val out = runProgramAndCaptureStdOut(
      """fun greeting() { return "hello"; }
        |print greeting();""".stripMargin
    )
    assertEquals(out.trim, "hello")

  test("return boolean value"):
    val out = runProgramAndCaptureStdOut(
      """fun isPositive(n) { return n > 0; }
        |print isPositive(5);
        |print isPositive(-3);""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("true", "false"))

  // ─── Functions: first-class values ───────────────────────────────────

  test("function stored in variable can be called"):
    val out = runProgramAndCaptureStdOut(
      """fun add(a, b) { return a + b; }
        |var myFn = add;
        |print myFn(3, 4);""".stripMargin
    )
    assertEquals(out.trim, "7.0")

  test("function returned from another function (callbacks)"):
    val out = runProgramAndCaptureStdOut(
      """fun fn() {
        |  print "soy un callback!";
        |}
        |fun get_fn() {
        |  return fn;
        |}
        |get_fn()();""".stripMargin
    )
    assertEquals(out.trim, "soy un callback!")

  test("chained function calls"):
    val out = runProgramAndCaptureStdOut(
      """fun makeAdder(x) {
        |  fun adder(y) { return x + y; }
        |  return adder;
        |}
        |print makeAdder(2)(3);""".stripMargin
    )
    assertEquals(out.trim, "5.0")

  test("function passed to variable and called later"):
    val out = runProgramAndCaptureStdOut(
      """fun square(n) { return n * n; }
        |var op = square;
        |print op(4);
        |print op(5);""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("16.0", "25.0"))

  // ─── Functions: nested functions ─────────────────────────────────────

  test("nested function declaration and call"):
    val out = runProgramAndCaptureStdOut(
      """fun nest() {
        |  fun nested() {
        |    print "soy una funcion interna!";
        |  }
        |  nested();
        |}
        |nest();""".stripMargin
    )
    assertEquals(out.trim, "soy una funcion interna!")

  test("nested function can access outer function's variables"):
    val out = runProgramAndCaptureStdOut(
      """fun outer() {
        |  var x = 10;
        |  fun inner() {
        |    print x;
        |  }
        |  inner();
        |}
        |outer();""".stripMargin
    )
    assertEquals(out.trim, "10.0")

  test("deeply nested functions"):
    val out = runProgramAndCaptureStdOut(
      """fun a() {
        |  fun b() {
        |    fun c() {
        |      return "deep";
        |    }
        |    return c();
        |  }
        |  return b();
        |}
        |print a();""".stripMargin
    )
    assertEquals(out.trim, "deep")

  test("inner function not accessible outside outer function"):
    val ex = intercept[RuntimeException] {
      runProgram(
        """fun outer() {
          |  fun inner() { return 1; }
          |}
          |outer();
          |inner();""".stripMargin
      )
    }
    assert(ex.getMessage.contains("Undefined variable"))

  // ─── Functions: closures ─────────────────────────────────────────────

  test("closure captures enclosing variable"):
    val out = runProgramAndCaptureStdOut(
      """fun makeGreeter(name) {
        |  fun greet() {
        |    print "Hello, " + name + "!";
        |  }
        |  return greet;
        |}
        |var greeter = makeGreeter("World");
        |greeter();""".stripMargin
    )
    assertEquals(out.trim, "Hello, World!")

  test("closure as counter"):
    val out = runProgramAndCaptureStdOut(
      """fun makeCounter() {
        |  var count = 0;
        |  fun increment() {
        |    count = count + 1;
        |    return count;
        |  }
        |  return increment;
        |}
        |var counter = makeCounter();
        |print counter();
        |print counter();
        |print counter();""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("1.0", "2.0", "3.0"))

  test("two closures from same factory are independent"):
    val out = runProgramAndCaptureStdOut(
      """fun makeCounter() {
        |  var count = 0;
        |  fun increment() {
        |    count = count + 1;
        |    return count;
        |  }
        |  return increment;
        |}
        |var c1 = makeCounter();
        |var c2 = makeCounter();
        |print c1();
        |print c1();
        |print c2();""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("1.0", "2.0", "1.0"))

  test("closure captures variable by reference"):
    val out = runProgramAndCaptureStdOut(
      """var x = "before";
        |fun f() { print x; }
        |f();
        |x = "after";
        |f();""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("before", "after"))

  test("closure over function parameter"):
    val out = runProgramAndCaptureStdOut(
      """fun multiplier(factor) {
        |  fun multiply(n) {
        |    return factor * n;
        |  }
        |  return multiply;
        |}
        |var double = multiplier(2);
        |var triple = multiplier(3);
        |print double(5);
        |print triple(5);""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("10.0", "15.0"))

  // ─── Functions: recursion ────────────────────────────────────────────

  test("recursive factorial"):
    val out = runProgramAndCaptureStdOut(
      """fun factorial(n) {
        |  if (n <= 1) return 1;
        |  return n * factorial(n - 1);
        |}
        |print factorial(1);
        |print factorial(5);
        |print factorial(6);""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("1.0", "120.0", "720.0"))

  test("recursive fibonacci"):
    val out = runProgramAndCaptureStdOut(
      """fun fib(n) {
        |  if (n <= 1) return n;
        |  return fib(n - 1) + fib(n - 2);
        |}
        |print fib(0);
        |print fib(1);
        |print fib(2);
        |print fib(5);
        |print fib(8);""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("0.0", "1.0", "1.0", "5.0", "21.0"))

  test("mutual recursion"):
    val out = runProgramAndCaptureStdOut(
      """fun isEven(n) {
        |  if (n == 0) return true;
        |  return isOdd(n - 1);
        |}
        |fun isOdd(n) {
        |  if (n == 0) return false;
        |  return isEven(n - 1);
        |}
        |print isEven(4);
        |print isOdd(3);""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("true", "true"))

  test("recursive countdown"):
    val out = runProgramAndCaptureStdOut(
      """fun countdown(n) {
        |  if (n <= 0) return;
        |  print n;
        |  countdown(n - 1);
        |}
        |countdown(3);""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("3.0", "2.0", "1.0"))

  // ─── Functions: interaction with control flow ────────────────────────

  test("function with if-else inside"):
    val out = runProgramAndCaptureStdOut(
      """fun max(a, b) {
        |  if (a > b) return a;
        |  else return b;
        |}
        |print max(3, 7);
        |print max(10, 2);""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("7.0", "10.0"))

  test("function with while loop inside"):
    val out = runProgramAndCaptureStdOut(
      """fun sum(n) {
        |  var total = 0;
        |  var i = 1;
        |  while (i <= n) {
        |    total = total + i;
        |    i = i + 1;
        |  }
        |  return total;
        |}
        |print sum(5);""".stripMargin
    )
    assertEquals(out.trim, "15.0")

  test("function with for loop inside"):
    val out = runProgramAndCaptureStdOut(
      """fun sumFor(n) {
        |  var total = 0;
        |  for (var i = 1; i <= n; i = i + 1) {
        |    total = total + i;
        |  }
        |  return total;
        |}
        |print sumFor(10);""".stripMargin
    )
    assertEquals(out.trim, "55.0")

  test("function called inside a loop"):
    val out = runProgramAndCaptureStdOut(
      """fun square(n) { return n * n; }
        |var result = 0;
        |for (var i = 1; i <= 3; i = i + 1) {
        |  result = result + square(i);
        |}
        |print result;""".stripMargin
    )
    assertEquals(out.trim, "14.0")

  test("function called in if condition"):
    val out = runProgramAndCaptureStdOut(
      """fun isPositive(n) { return n > 0; }
        |if (isPositive(5)) print "positive";
        |else print "non-positive";""".stripMargin
    )
    assertEquals(out.trim, "positive")

  test("function called in while condition"):
    val out = runProgramAndCaptureStdOut(
      """var count = 0;
        |fun shouldContinue() {
        |  count = count + 1;
        |  return count < 4;
        |}
        |while (shouldContinue()) {
        |  print count;
        |}""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("1.0", "2.0", "3.0"))

  // ─── Functions: used in expressions ──────────────────────────────────

  test("function return value used in arithmetic"):
    val out = runProgramAndCaptureStdOut(
      """fun three() { return 3; }
        |fun four() { return 4; }
        |print three() + four();""".stripMargin
    )
    assertEquals(out.trim, "7.0")

  test("function return value used in comparison"):
    val out = runProgramAndCaptureStdOut(
      """fun getValue() { return 10; }
        |print getValue() > 5;""".stripMargin
    )
    assertEquals(out.trim, "true")

  test("function return value assigned to variable"):
    val out = runProgramAndCaptureStdOut(
      """fun compute() { return 2 * 3 + 1; }
        |var result = compute();
        |print result;""".stripMargin
    )
    assertEquals(out.trim, "7.0")

  test("nested function calls as arguments"):
    val out = runProgramAndCaptureStdOut(
      """fun add(a, b) { return a + b; }
        |fun double(n) { return n * 2; }
        |print add(double(3), double(4));""".stripMargin
    )
    assertEquals(out.trim, "14.0")

  // ─── Functions: edge cases ───────────────────────────────────────────

  test("function with same name as variable shadows variable"):
    val out = runProgramAndCaptureStdOut(
      """var x = 10;
        |fun x() { return 20; }
        |print x();""".stripMargin
    )
    assertEquals(out.trim, "20.0")

  test("function can be redefined"):
    val out = runProgramAndCaptureStdOut(
      """fun f() { return 1; }
        |print f();
        |fun f() { return 2; }
        |print f();""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("1.0", "2.0"))

  test("function with print side effect and return"):
    val out = runProgramAndCaptureStdOut(
      """fun sideEffect() {
        |  print "effect";
        |  return 42;
        |}
        |var r = sideEffect();
        |print r;""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("effect", "42.0"))

  test("function calling another function"):
    val out = runProgramAndCaptureStdOut(
      """fun add(a, b) { return a + b; }
        |fun addThree(a, b, c) { return add(add(a, b), c); }
        |print addThree(1, 2, 3);""".stripMargin
    )
    assertEquals(out.trim, "6.0")

  test("function with string concatenation"):
    val out = runProgramAndCaptureStdOut(
      """fun fullName(first, last) { return first + " " + last; }
        |print fullName("John", "Doe");""".stripMargin
    )
    assertEquals(out.trim, "John Doe")

  test("multiple functions defined and called"):
    val out = runProgramAndCaptureStdOut(
      """fun a() { return 1; }
        |fun b() { return 2; }
        |fun c() { return 3; }
        |print a() + b() + c();""".stripMargin
    )
    assertEquals(out.trim, "6.0")

  test("function with local variables does not pollute outer scope"):
    val out = runProgramAndCaptureStdOut(
      """fun f() {
        |  var local = 42;
        |  return local;
        |}
        |print f();""".stripMargin
    )
    assertEquals(out.trim, "42.0")
    val ex = intercept[RuntimeException] {
      runProgram(
        """fun f() { var local = 42; return local; }
          |f();
          |print local;""".stripMargin
      )
    }
    assert(ex.getMessage.contains("Undefined variable"))

  test("function return value used in logical expression"):
    val out = runProgramAndCaptureStdOut(
      """fun truthy() { return true; }
        |fun falsy() { return false; }
        |print truthy() and falsy();
        |print truthy() or falsy();""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("false", "true"))

  // ─── Functions: closure + recursion combined ─────────────────────────

  test("closure with recursion - accumulator pattern"):
    val out = runProgramAndCaptureStdOut(
      """fun makeAccumulator(initial) {
        |  var total = initial;
        |  fun add(n) {
        |    total = total + n;
        |    return total;
        |  }
        |  return add;
        |}
        |var acc = makeAccumulator(0);
        |print acc(5);
        |print acc(3);
        |print acc(2);""".stripMargin
    )
    assertEquals(out.trim.linesIterator.toList, List("5.0", "8.0", "10.0"))

  test("function declaration inside block scope"):
    val out = runProgramAndCaptureStdOut(
      """{
        |  fun localFn() { return 99; }
        |  print localFn();
        |}""".stripMargin
    )
    assertEquals(out.trim, "99.0")

  test("function uses global variable"):
    val out = runProgramAndCaptureStdOut(
      """var greeting = "Hi";
        |fun greet(name) { return greeting + ", " + name; }
        |print greet("Alice");""".stripMargin
    )
    assertEquals(out.trim, "Hi, Alice")
