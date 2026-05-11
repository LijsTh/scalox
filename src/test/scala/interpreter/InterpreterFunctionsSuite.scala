class InterpreterFunctionsSuite extends munit.FunSuite, InterpreterHelpers:

  // ─── Basic declaration and calling ───────────────────────────────────

  test("function declaration and call with return value"):
    assertEquals(runAndTrim("fun returnthree() { return 3; } print returnthree();"), "3.0")

  test("printing a function shows its representation"):
    val out = runAndTrim("fun returnthree() { return 3; } print returnthree;")
    assert(out.contains("fn") && out.contains("returnthree"))

  test("function without return statement returns nil"):
    assertEquals(runAndTrim("fun noreturn() { } print noreturn();"), "nil")

  test("function with empty return returns nil"):
    assertEquals(runAndTrim("fun emptyreturn() { return; } print emptyreturn();"), "nil")

  test("function with body but no explicit return returns nil"):
    assertEquals(runAndTrim("fun greet() { var x = 1; var y = 2; } print greet();"), "nil")

  // ─── Parameters and arguments ─────────────────────────────────────────

  test("function with one parameter"):
    assertEquals(runAndTrim("fun double(n) { return n * 2; } print double(5);"), "10.0")

  test("function with multiple parameters"):
    assertEquals(runAndTrim("fun add(a, b, c) { return a + b + c; } print add(1, 2, 3);"), "6.0")

  test("function parameters are local to the function"):
    assertEquals(
      runAndLines("""var a = "global"; fun f(a) { print a; } f("local"); print a;"""),
      List("local", "global")
    )

  test("function parameters shadow outer variables"):
    assertEquals(runAndTrim("var x = 10; fun f(x) { return x + 1; } print f(20);"), "21.0")

  // ─── Arity errors ────────────────────────────────────────────────────

  List(
    ("fun add(a, b, c) { return a + b + c; } add(1,2,3,4);", "Expected 3 arguments but got 4"),
    ("fun add(a, b, c) { return a + b + c; } add(1,2);",     "Expected 3 arguments but got 2"),
    ("fun f() { return 1; } f(42);",                         "Expected 0 arguments but got 1"),
  ).foreach: (program, message) =>
    test(s"arity error: $message"):
      val ex = intercept[RuntimeException](runProgram(program))
      assert(ex.getMessage.contains(message))

  // ─── Calling non-callable values ──────────────────────────────────────

  List(
    ("var x = \"x\"; x();", "string"),
    ("var x = 42; x();",    "number"),
    ("var x = nil; x();",   "nil"),
    ("var x = true; x();",  "boolean"),
  ).foreach: (program, typeName) =>
    test(s"calling a $typeName throws"):
      val ex = intercept[RuntimeException](runProgram(program))
      assert(ex.getMessage.contains("Can only call functions"))

  // ─── Return behavior ─────────────────────────────────────────────────

  test("return exits function early"):
    assertEquals(
      runAndTrim("""fun f() { print "before"; return; print "after"; } f();"""),
      "before"
    )

  test("return with value exits function early"):
    assertEquals(
      runAndTrim("""fun f() { return 42; print "unreachable"; } print f();"""),
      "42.0"
    )

  test("return inside if branch"):
    assertEquals(
      runAndLines("""fun abs(n) { if (n < 0) return -n; return n; } print abs(-5); print abs(3);"""),
      List("5.0", "3.0")
    )

  test("return inside while loop"):
    assertEquals(
      runAndTrim(
        """fun findFirst() {
          |  var i = 0;
          |  while (i < 10) { if (i == 5) return i; i = i + 1; }
          |  return -1;
          |}
          |print findFirst();""".stripMargin
      ),
      "5.0"
    )

  test("return string value"):
    assertEquals(runAndTrim("""fun greeting() { return "hello"; } print greeting();"""), "hello")

  test("return boolean value"):
    assertEquals(
      runAndLines("""fun isPositive(n) { return n > 0; } print isPositive(5); print isPositive(-3);"""),
      List("true", "false")
    )

  // ─── First-class values ───────────────────────────────────────────────

  test("function stored in variable can be called"):
    assertEquals(runAndTrim("""fun add(a, b) { return a + b; } var myFn = add; print myFn(3, 4);"""), "7.0")

  test("function returned from another function"):
    assertEquals(
      runAndTrim("""fun fn() { print "soy un callback!"; } fun get_fn() { return fn; } get_fn()();"""),
      "soy un callback!"
    )

  test("chained function calls"):
    assertEquals(
      runAndTrim("""fun makeAdder(x) { fun adder(y) { return x + y; } return adder; } print makeAdder(2)(3);"""),
      "5.0"
    )

  test("function passed to variable and called later"):
    assertEquals(
      runAndLines("""fun square(n) { return n * n; } var op = square; print op(4); print op(5);"""),
      List("16.0", "25.0")
    )

  // ─── Nested functions ─────────────────────────────────────────────────

  test("nested function declaration and call"):
    assertEquals(
      runAndTrim("""fun nest() { fun nested() { print "soy una funcion interna!"; } nested(); } nest();"""),
      "soy una funcion interna!"
    )

  test("nested function can access outer function's variables"):
    assertEquals(
      runAndTrim("""fun outer() { var x = 10; fun inner() { print x; } inner(); } outer();"""),
      "10.0"
    )

  test("deeply nested functions"):
    assertEquals(
      runAndTrim(
        """fun a() {
          |  fun b() { fun c() { return "deep"; } return c(); }
          |  return b();
          |}
          |print a();""".stripMargin
      ),
      "deep"
    )

  test("inner function not accessible outside outer function"):
    val ex = intercept[RuntimeException]:
      runProgram("""fun outer() { fun inner() { return 1; } } outer(); inner();""")
    assert(ex.getMessage.contains("Undefined variable"))

  // ─── Used in expressions ──────────────────────────────────────────────

  test("function return value used in arithmetic"):
    assertEquals(runAndTrim("""fun three() { return 3; } fun four() { return 4; } print three() + four();"""), "7.0")

  test("function return value used in comparison"):
    assertEquals(runAndTrim("""fun getValue() { return 10; } print getValue() > 5;"""), "true")

  test("function return value assigned to variable"):
    assertEquals(runAndTrim("""fun compute() { return 2 * 3 + 1; } var result = compute(); print result;"""), "7.0")

  test("nested function calls as arguments"):
    assertEquals(
      runAndTrim("""fun add(a, b) { return a + b; } fun double(n) { return n * 2; } print add(double(3), double(4));"""),
      "14.0"
    )

  // ─── Edge cases ───────────────────────────────────────────────────────

  test("function with same name as variable shadows variable"):
    assertEquals(runAndTrim("""var x = 10; fun x() { return 20; } print x();"""), "20.0")

  test("function can be redefined"):
    assertEquals(
      runAndLines("""fun f() { return 1; } print f(); fun f() { return 2; } print f();"""),
      List("1.0", "2.0")
    )

  test("function with print side effect and return"):
    assertEquals(
      runAndLines("""fun sideEffect() { print "effect"; return 42; } var r = sideEffect(); print r;"""),
      List("effect", "42.0")
    )

  test("function calling another function"):
    assertEquals(
      runAndTrim("""fun add(a, b) { return a + b; } fun addThree(a, b, c) { return add(add(a, b), c); } print addThree(1, 2, 3);"""),
      "6.0"
    )

  test("function with string concatenation"):
    assertEquals(
      runAndTrim("""fun fullName(first, last) { return first + " " + last; } print fullName("John", "Doe");"""),
      "John Doe"
    )

  test("multiple functions defined and called"):
    assertEquals(
      runAndTrim("""fun a() { return 1; } fun b() { return 2; } fun c() { return 3; } print a() + b() + c();"""),
      "6.0"
    )

  test("function with local variables does not pollute outer scope"):
    assertEquals(runAndTrim("fun f() { var local = 42; return local; } print f();"), "42.0")
    val ex = intercept[RuntimeException]:
      runProgram("fun f() { var local = 42; return local; } f(); print local;")
    assert(ex.getMessage.contains("Undefined variable"))

  test("function return value used in logical expression"):
    assertEquals(
      runAndLines("""fun truthy() { return true; } fun falsy() { return false; } print truthy() and falsy(); print truthy() or falsy();"""),
      List("false", "true")
    )

  // ─── Interaction with control flow ────────────────────────────────────

  test("function with if-else inside"):
    assertEquals(
      runAndLines("""fun max(a, b) { if (a > b) return a; else return b; } print max(3, 7); print max(10, 2);"""),
      List("7.0", "10.0")
    )

  test("function with while loop inside"):
    assertEquals(
      runAndTrim("""fun sum(n) { var total = 0; var i = 1; while (i <= n) { total = total + i; i = i + 1; } return total; } print sum(5);"""),
      "15.0"
    )

  test("function with for loop inside"):
    assertEquals(
      runAndTrim("""fun sumFor(n) { var total = 0; for (var i = 1; i <= n; i = i + 1) { total = total + i; } return total; } print sumFor(10);"""),
      "55.0"
    )

  test("function called inside a loop"):
    assertEquals(
      runAndTrim("""fun square(n) { return n * n; } var result = 0; for (var i = 1; i <= 3; i = i + 1) { result = result + square(i); } print result;"""),
      "14.0"
    )

  test("function called in if condition"):
    assertEquals(
      runAndTrim("""fun isPositive(n) { return n > 0; } if (isPositive(5)) print "positive"; else print "non-positive";"""),
      "positive"
    )

  test("function called in while condition"):
    assertEquals(
      runAndLines(
        """var count = 0;
          |fun shouldContinue() { count = count + 1; return count < 4; }
          |while (shouldContinue()) { print count; }""".stripMargin
      ),
      List("1.0", "2.0", "3.0")
    )
