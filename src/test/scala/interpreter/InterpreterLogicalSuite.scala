class InterpreterLogicalSuite extends munit.FunSuite, InterpreterHelpers:

  List(
    ("true and true",   true),
    ("true and false",  false),
    ("false and true",  false),
    ("false and false", false),
    ("true or true",    true),
    ("true or false",   true),
    ("false or true",   true),
    ("false or false",  false),
  ).foreach: (expr, expected) =>
    test(s"logical: $expr"):
      assertEquals(eval(expr), expected)

  test("'and' returns right operand when left is truthy"):
    assertEquals(eval("1 and 2"), 2.0)

  test("'and' returns left operand when left is falsy"):
    assertEquals(eval("nil and 2"), null)

  test("'or' returns left operand when left is truthy"):
    assertEquals(eval("1 or 2"), 1.0)

  test("'or' returns right operand when left is falsy"):
    assertEquals(eval("nil or 2"), 2.0)

  test("'and' short-circuits on false"):
    assertEquals(eval("false and (1 / 0)"), false)

  test("'or' short-circuits on true"):
    assertEquals(eval("true or (1 / 0)"), true)

  List(
    ("false or true and false", false),  // and binds tighter
    ("true or false and false", true),
    ("true and true and true",  true),
    ("false or false or true",  true),
    ("true and false or true",  true),
    ("!true and false",         false),
    ("!false or false",         true),
    ("1 < 2 and 3 > 0",         true),
    ("1 > 2 or 3 > 0",          true),
  ).foreach: (expr, expected) =>
    test(s"logical chain: $expr"):
      assertEquals(eval(expr), expected)

  test("nil is falsy in 'and'"):
    assertEquals(eval("nil and true"), null)

  test("nil is falsy in 'or'"):
    assertEquals(eval("nil or 42"), 42.0)

  test("0 is truthy (only nil and false are falsy)"):
    assertEquals(eval("0 and true"), true)

  test("empty string is truthy"):
    assertEquals(eval("\"\" and true"), true)
