class InterpreterControlFlowSuite extends munit.FunSuite, InterpreterHelpers:

  // ─── if statement ────────────────────────────────────────────────────

  test("if true executes then branch"):
    assertEquals(runAndTrim("if (true) print 1;"), "1.0")

  test("if false skips then branch"):
    assertEquals(runAndTrim("if (false) print 1;"), "")

  test("if-else: true takes then branch"):
    assertEquals(runAndTrim("if (true) print 1; else print 2;"), "1.0")

  test("if-else: false takes else branch"):
    assertEquals(runAndTrim("if (false) print 1; else print 2;"), "2.0")

  test("if with block body"):
    assertEquals(runAndLines("if (true) { print 1; print 2; }"), List("1.0", "2.0"))

  test("if-else with block bodies"):
    assertEquals(runAndLines("if (false) { print 1; } else { print 2; print 3; }"), List("2.0", "3.0"))

  test("if with variable condition"):
    assertEquals(runAndTrim("var x = 5; if (x > 3) print 1; else print 0;"), "1.0")

  test("if with assignment in branch"):
    assertEquals(runAndTrim("var x = 0; if (true) x = 1; print x;"), "1.0")

  test("nested if-else"):
    assertEquals(
      runAndTrim("var x = 2; if (x == 1) print 1; else if (x == 2) print 2; else print 3;"),
      "2.0"
    )

  test("if with logical condition"):
    assertEquals(runAndTrim("if (true and false) print 1; else print 0;"), "0.0")

  test("nil is falsy in if condition"):
    assertEquals(runAndTrim("if (nil) print 1; else print 0;"), "0.0")

  test("0 is truthy in if condition"):
    assertEquals(runAndTrim("if (0) print 1; else print 0;"), "1.0")

  test("if with while inside"):
    assertEquals(runAndTrim("var x = 1; if (true) { while (x < 4) x = x + 1; } print x;"), "4.0")

  // ─── while statement ─────────────────────────────────────────────────

  test("while false body never executes"):
    assertEquals(runAndTrim("while (false) print 1;"), "")

  test("while counts up to 3"):
    assertEquals(runAndLines("var i = 0; while (i < 3) { print i; i = i + 1; }"), List("0.0", "1.0", "2.0"))

  test("while accumulates sum"):
    assertEquals(runAndTrim("var sum = 0; var i = 1; while (i <= 5) { sum = sum + i; i = i + 1; } print sum;"), "15.0")

  test("while with logical condition"):
    assertEquals(runAndTrim("var x = 0; while (x < 3 and x >= 0) { x = x + 1; } print x;"), "3.0")

  test("while modifies outer var"):
    assertEquals(runAndTrim("var x = 10; while (x > 0) { x = x - 3; } print x;"), "-2.0")

  test("nested while loops"):
    assertEquals(
      runAndTrim("var i = 0; var sum = 0; while (i < 3) { var j = 0; while (j < 3) { sum = sum + 1; j = j + 1; } i = i + 1; } print sum;"),
      "9.0"
    )

  test("while with if inside"):
    assertEquals(
      runAndTrim("var i = 0; var evens = 0; while (i < 6) { if (i == 0 or i == 2 or i == 4) evens = evens + 1; i = i + 1; } print evens;"),
      "3.0"
    )

  // ─── for loop ────────────────────────────────────────────────────────

  test("for loop prints 0 1 2"):
    assertEquals(runAndLines("for (var i = 0; i < 3; i = i + 1) print i;"), List("0.0", "1.0", "2.0"))

  test("for loop with block body"):
    assertEquals(runAndLines("for (var i = 0; i < 3; i = i + 1) { print i; }"), List("0.0", "1.0", "2.0"))

  test("for loop var is scoped: i not accessible outside"):
    val interp = Interpreter()
    val stmts = Parser(Scanner("for (var i = 0; i < 3; i = i + 1) print i;").scan()).parse()
    val resolver = Resolver(interp)
    stmts.foreach(resolver.resolve)
    interp.interpret(stmts)
    intercept[RuntimeException](interp.evaluate(Parser(Scanner("i").scan()).expression()))

  test("for loop computes sum 1..5"):
    assertEquals(runAndTrim("var sum = 0; for (var i = 1; i <= 5; i = i + 1) sum = sum + i; print sum;"), "15.0")

  test("for loop counts down"):
    assertEquals(runAndLines("for (var i = 3; i > 0; i = i - 1) print i;"), List("3.0", "2.0", "1.0"))

  test("for loop with no initializer"):
    assertEquals(runAndLines("var i = 0; for (; i < 3; i = i + 1) print i;"), List("0.0", "1.0", "2.0"))

  test("for loop with no increment"):
    assertEquals(runAndLines("for (var i = 0; i < 3;) { print i; i = i + 1; }"), List("0.0", "1.0", "2.0"))

  test("for loop body never runs when condition is false"):
    assertEquals(runAndTrim("for (var i = 5; i < 3; i = i + 1) print i;"), "")

  test("nested for loops: multiplication table"):
    assertEquals(
      runAndLines("for (var i = 1; i <= 3; i = i + 1) { for (var j = 1; j <= 3; j = j + 1) { print i * j; } }")
        .map(_.toDouble),
      List(1, 2, 3, 2, 4, 6, 3, 6, 9).map(_.toDouble)
    )

  test("for loop with if inside"):
    assertEquals(
      runAndLines("for (var i = 0; i < 5; i = i + 1) { if (i == 0 or i == 2 or i == 4) print i; }"),
      List("0.0", "2.0", "4.0")
    )

  test("for loop with expression initializer"):
    assertEquals(runAndLines("var i; for (i = 0; i < 3; i = i + 1) print i;"), List("0.0", "1.0", "2.0"))
