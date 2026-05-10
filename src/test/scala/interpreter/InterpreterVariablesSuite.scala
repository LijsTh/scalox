class InterpreterVariablesSuite extends munit.FunSuite, InterpreterHelpers:

  // ─── Variables ───────────────────────────────────────────────────────

  test("var declaration and read"):
    val interp = Interpreter()
    interp.interpret(Parser(Scanner("var x = 42;").scan()).parse())
    assertEquals(interp.evaluate(Parser(Scanner("x").scan()).expression()), 42.0)

  test("var declared without initializer is nil"):
    val interp = Interpreter()
    interp.interpret(Parser(Scanner("var x;").scan()).parse())
    assertEquals(interp.evaluate(Parser(Scanner("x").scan()).expression()), null)

  test("var assignment updates value"):
    val interp = Interpreter()
    interp.interpret(Parser(Scanner("var x = 1; x = 99;").scan()).parse())
    assertEquals(interp.evaluate(Parser(Scanner("x").scan()).expression()), 99.0)

  test("reading undeclared variable throws"):
    intercept[RuntimeException]:
      Interpreter().evaluate(Parser(Scanner("z").scan()).expression())

  test("assigning undeclared variable throws"):
    intercept[RuntimeException]:
      Interpreter().interpret(Parser(Scanner("z = 1;").scan()).parse())

  // ─── Statements ──────────────────────────────────────────────────────

  test("expression statement produces no output"):
    assertEquals(captureOutput("1 + 2;"), "")

  test("print statement outputs value"):
    assertEquals(captureOutput("print 1 + 2;"), "3.0\n")

  test("var declaration with initializer"):
    assertEquals(captureOutput("var x = 5; print x;"), "5.0\n")

  test("var declaration without initializer prints nil"):
    assertEquals(captureOutput("var x; print x;"), "nil\n")

  // ─── Blocks and scoping ──────────────────────────────────────────────

  test("block introduces new scope, inner var hidden after block"):
    val interp = Interpreter()
    interp.interpret(Parser(Scanner("var x = 1; { var x = 2; }").scan()).parse())
    assertEquals(interp.evaluate(Parser(Scanner("x").scan()).expression()), 1.0)

  test("block can read outer variable"):
    assertEquals(runAndTrim("var x = 10; { print x; }"), "10.0")

  test("block can modify outer variable"):
    val interp = Interpreter()
    interp.interpret(Parser(Scanner("var x = 1; { x = 2; }").scan()).parse())
    assertEquals(interp.evaluate(Parser(Scanner("x").scan()).expression()), 2.0)

  test("block mutation is visible after block"):
    assertEquals(runAndTrim("var x = 2; { x = 3; } print x;"), "3.0")

  test("inner var shadows outer var inside block"):
    assertEquals(runAndTrim("var x = 1; { var x = 99; print x; }"), "99.0")

  test("outer var unchanged after inner shadow"):
    assertEquals(runAndTrim("var x = 1; { var x = 99; } print x;"), "1.0")

  test("nested blocks each have own scope"):
    assertEquals(
      runAndLines("var a = 1; { var b = 2; { var c = 3; print a; print b; print c; } }"),
      List("1.0", "2.0", "3.0")
    )

  test("modification in nested block visible in enclosing block"):
    assertEquals(runAndTrim("var x = 0; { { x = 42; } print x; }"), "42.0")

  test("var in inner block not accessible after it closes"):
    val interp = Interpreter()
    interp.interpret(Parser(Scanner("{ var inner = 7; }").scan()).parse())
    intercept[RuntimeException]:
      interp.evaluate(Parser(Scanner("inner").scan()).expression())

  test("block statement creates inner scope with shadowing"):
    assertEquals(
      captureOutput("var x = \"global\"; { var x = \"local\"; print x; } print x;"),
      "local\nglobal\n"
    )

  test("variables declared in block are not visible outside"):
    val ex = intercept[RuntimeException](runProgram("{ var y = 2; } print y;"))
    assert(clue(ex.getMessage).contains("Undefined variable 'y'"))

  test("var computed from expression in initializer"):
    assertEquals(runAndTrim("var y = 2 + 3; print y;"), "5.0")
