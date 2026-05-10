class InterpreterClosuresSuite extends munit.FunSuite, InterpreterHelpers:

  test("closure captures enclosing variable"):
    assertEquals(
      runAndTrim(
        """fun makeGreeter(name) {
          |  fun greet() { print "Hello, " + name + "!"; }
          |  return greet;
          |}
          |var greeter = makeGreeter("World");
          |greeter();""".stripMargin
      ),
      "Hello, World!"
    )

  test("closure as counter"):
    assertEquals(
      runAndLines(
        """fun makeCounter() {
          |  var count = 0;
          |  fun increment() { count = count + 1; return count; }
          |  return increment;
          |}
          |var counter = makeCounter();
          |print counter(); print counter(); print counter();""".stripMargin
      ),
      List("1.0", "2.0", "3.0")
    )

  test("two closures from same factory are independent"):
    assertEquals(
      runAndLines(
        """fun makeCounter() {
          |  var count = 0;
          |  fun increment() { count = count + 1; return count; }
          |  return increment;
          |}
          |var c1 = makeCounter(); var c2 = makeCounter();
          |print c1(); print c1(); print c2();""".stripMargin
      ),
      List("1.0", "2.0", "1.0")
    )

  test("closure captures variable by reference"):
    assertEquals(
      runAndLines("""var x = "before"; fun f() { print x; } f(); x = "after"; f();"""),
      List("before", "after")
    )

  test("closure over function parameter"):
    assertEquals(
      runAndLines(
        """fun multiplier(factor) {
          |  fun multiply(n) { return factor * n; }
          |  return multiply;
          |}
          |var double = multiplier(2); var triple = multiplier(3);
          |print double(5); print triple(5);""".stripMargin
      ),
      List("10.0", "15.0")
    )

  // ─── Recursion ────────────────────────────────────────────────────────

  test("recursive factorial"):
    assertEquals(
      runAndLines(
        """fun factorial(n) { if (n <= 1) return 1; return n * factorial(n - 1); }
          |print factorial(1); print factorial(5); print factorial(6);""".stripMargin
      ),
      List("1.0", "120.0", "720.0")
    )

  test("recursive fibonacci"):
    assertEquals(
      runAndLines(
        """fun fib(n) { if (n <= 1) return n; return fib(n - 1) + fib(n - 2); }
          |print fib(0); print fib(1); print fib(2); print fib(5); print fib(8);""".stripMargin
      ),
      List("0.0", "1.0", "1.0", "5.0", "21.0")
    )

  test("mutual recursion"):
    assertEquals(
      runAndLines(
        """fun isEven(n) { if (n == 0) return true; return isOdd(n - 1); }
          |fun isOdd(n) { if (n == 0) return false; return isEven(n - 1); }
          |print isEven(4); print isOdd(3);""".stripMargin
      ),
      List("true", "true")
    )

  test("recursive countdown"):
    assertEquals(
      runAndLines("""fun countdown(n) { if (n <= 0) return; print n; countdown(n - 1); } countdown(3);"""),
      List("3.0", "2.0", "1.0")
    )

  // ─── Closure + recursion combined ─────────────────────────────────────

  test("accumulator pattern"):
    assertEquals(
      runAndLines(
        """fun makeAccumulator(initial) {
          |  var total = initial;
          |  fun add(n) { total = total + n; return total; }
          |  return add;
          |}
          |var acc = makeAccumulator(0);
          |print acc(5); print acc(3); print acc(2);""".stripMargin
      ),
      List("5.0", "8.0", "10.0")
    )

  test("function declaration inside block scope"):
    assertEquals(runAndTrim("{ fun localFn() { return 99; } print localFn(); }"), "99.0")

  test("function uses global variable"):
    assertEquals(
      runAndTrim("""var greeting = "Hi"; fun greet(name) { return greeting + ", " + name; } print greet("Alice");"""),
      "Hi, Alice"
    )
