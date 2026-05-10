class ScannerSuite extends munit.FunSuite:

  // ─── Test Fixtures ───────────────────────────────────────────────────

  private def scan(source: String): List[Token] =
    Scanner(source).scan().toList

  private def tokenTypes(source: String): List[TokenType] =
    scan(source).map(_.tokenType)

  // ─── EOF & Whitespace ────────────────────────────────────────────────

  test("scan returns EOF for empty source"):
    assertEquals(tokenTypes(""), List(TokenType.EOF))

  test("scan returns EOF when source contains only whitespace"):
    assertEquals(tokenTypes("   \r\t\n"), List(TokenType.EOF))

  // ─── Comments ────────────────────────────────────────────────────────

  test("scan ignores line comments and continues on the next line"):
    val tokens = scan("// comentario\nvar")
    assertEquals(tokens.map(_.tokenType), List(TokenType.VAR, TokenType.EOF))
    assertEquals(tokens.head.line, 2)

  test("scan ignores line comments until EOF when no newline is present"):
    assertEquals(tokenTypes("// comment"), List(TokenType.EOF))

  test("scan ignores block comments and nested block comments"):
    val tokens = scan("/* a\n/* b */\nc */\nvar")
    assertEquals(tokens.map(_.tokenType), List(TokenType.VAR, TokenType.EOF))
    assertEquals(tokens.head.line, 4)

  test("scan fails on unterminated block comment"):
    val ex = intercept[RuntimeException](Scanner("/* comment").scan())
    assert(clue(ex.getMessage).contains("Unterminated block comment"))

  test("scan handles empty block comment"):
    assertEquals(tokenTypes("/**/"), List(TokenType.EOF))

  test("scan handles deeply nested block comments"):
    assertEquals(tokenTypes("/* /* /* x */ */ */"), List(TokenType.EOF))

  test("scan handles asterisks inside block comments without false close"):
    assertEquals(tokenTypes("/* *** */"), List(TokenType.EOF))

  test("scan treats line comment syntax as inert inside block comments"):
    assertEquals(tokenTypes("/* // */"), List(TokenType.EOF))

  test("scan handles multiple consecutive line comments"):
    val tokens = scan("// a\n// b\n// c\nvar")
    assertEquals(tokens.map(_.tokenType), List(TokenType.VAR, TokenType.EOF))
    assertEquals(tokens.head.line, 4)

  test("scan handles block comment followed immediately by code"):
    assertEquals(tokenTypes("/* comment */var"), List(TokenType.VAR, TokenType.EOF))

  test("scan fails on imbalanced nested block comment"):
    val ex = intercept[RuntimeException](Scanner("/* /* */").scan())
    assert(clue(ex.getMessage).contains("Unterminated block comment"))

  // ─── Single and multi-char tokens ────────────────────────────────────

  test("scan recognizes supported single-character tokens"):
    assertEquals(
      tokenTypes("(){},;:%?/"),
      List(
        TokenType.LEFT_PAREN,
        TokenType.RIGHT_PAREN,
        TokenType.LEFT_BRACE,
        TokenType.RIGHT_BRACE,
        TokenType.COMMA,
        TokenType.SEMICOLON,
        TokenType.COLON,
        TokenType.PERCENT,
        TokenType.QUESTION,
        TokenType.SLASH,
        TokenType.EOF
      )
    )

  test("scan recognizes mixed one-char and two-char operators"):
    assertEquals(
      tokenTypes("+++ --- != ! == = <= < >= >"),
      List(
        TokenType.PLUS,
        TokenType.PLUS,
        TokenType.PLUS,
        TokenType.MINUS,
        TokenType.MINUS,
        TokenType.MINUS,
        TokenType.BANG_EQUAL,
        TokenType.BANG,
        TokenType.EQUAL_EQUAL,
        TokenType.EQUAL,
        TokenType.LESS_EQUAL,
        TokenType.LESS,
        TokenType.GREATER_EQUAL,
        TokenType.GREATER,
        TokenType.EOF
      )
    )

  test("scan fails on unexpected characters"):
    val ex = intercept[RuntimeException](Scanner("@").scan())
    assert(clue(ex.getMessage).contains("Unexpected character"))

  // ─── String literals ─────────────────────────────────────────────────

  test("scan recognizes single-quoted string literals"):
    val tokens = scan("'hello'")
    assertEquals(tokens.map(_.tokenType), List(TokenType.STRING, TokenType.EOF))
    assertEquals(tokens.head.literal, Some("hello"))

  test("scan recognizes empty single-quoted string"):
    val tokens = scan("''")
    assertEquals(tokens.map(_.tokenType), List(TokenType.STRING, TokenType.EOF))
    assertEquals(tokens.head.literal, Some(""))

  test("scan recognizes empty double-quoted string"):
    val tokens = scan("\"\"")
    assertEquals(tokens.map(_.tokenType), List(TokenType.STRING, TokenType.EOF))
    assertEquals(tokens.head.literal, Some(""))

  test("scan recognizes multi-line double-quoted strings"):
    val tokens = scan("\"hello\nworld\"")
    assertEquals(tokens.map(_.tokenType), List(TokenType.STRING, TokenType.EOF))
    assertEquals(tokens.head.literal, Some("hello\nworld"))
    assertEquals(tokens.head.line, 2)

  List(
    ("'hello\nworld'", "unterminated single-quoted string with newline"),
    ("'hello",         "unterminated single-quoted string at EOF"),
    ("\"hello",        "unterminated double-quoted string"),
    ("\"hello\nworld", "unterminated multi-line double-quoted string at EOF"),
  ).foreach: (source, description) =>
    test(s"scan fails on $description"):
      val ex = intercept[RuntimeException](Scanner(source).scan())
      assert(clue(ex.getMessage).contains("Unterminated string"))

  // ─── Number literals ─────────────────────────────────────────────────

  test("scan recognizes integer and floating-point numbers"):
    val tokens = scan("123 45.67")
    assertEquals(tokens.map(_.tokenType), List(TokenType.NUMBER, TokenType.NUMBER, TokenType.EOF))
    assertEquals(tokens(0).literal, Some(123.0))
    assertEquals(tokens(1).literal, Some(45.67))

  test("scan recognizes zero as a number"):
    val t1 = scan("0")
    assertEquals(t1.map(_.tokenType), List(TokenType.NUMBER, TokenType.EOF))
    assertEquals(t1.head.literal, Some(0.0))

    val t2 = scan("0.0")
    assertEquals(t2.map(_.tokenType), List(TokenType.NUMBER, TokenType.EOF))
    assertEquals(t2.head.literal, Some(0.0))

  test("scan parses numbers with leading zeros"):
    val tokens = scan("007")
    assertEquals(tokens.map(_.tokenType), List(TokenType.NUMBER, TokenType.EOF))
    assertEquals(tokens.head.literal, Some(7.0))

  test("scan recognizes very small and very large numbers"):
    assertEquals(scan("0.001").head.literal, Some(0.001))
    assertEquals(scan("999999999").head.literal, Some(999999999.0))

  test("scan treats negative sign as MINUS followed by NUMBER"):
    val tokens = scan("-1")
    assertEquals(tokens.map(_.tokenType), List(TokenType.MINUS, TokenType.NUMBER, TokenType.EOF))
    assertEquals(tokens(1).literal, Some(1.0))

  test("scan fails on invalid number formats"):
    List("1..2", "1.2.3", "1.").foreach: source =>
      val ex = intercept[RuntimeException](Scanner(source).scan())
      assert(clue(ex.getMessage).contains("Invalid number format"))

  // ─── Identifiers and keywords ─────────────────────────────────────────

  test("scan recognizes identifiers and keywords"):
    val tokens = scan("foo _bar t0 and class this while")
    assertEquals(
      tokens.map(_.tokenType),
      List(
        TokenType.IDENTIFIER,
        TokenType.IDENTIFIER,
        TokenType.IDENTIFIER,
        TokenType.AND,
        TokenType.CLASS,
        TokenType.THIS,
        TokenType.WHILE,
        TokenType.EOF
      )
    )
    assertEquals(tokens(0).lexeme, "foo")
    assertEquals(tokens(1).lexeme, "_bar")
    assertEquals(tokens(2).lexeme, "t0")

  test("scan recognizes all language keywords"):
    assertEquals(
      tokenTypes("and class else false fun for if nil or print return super this true var while"),
      List(
        TokenType.AND,
        TokenType.CLASS,
        TokenType.ELSE,
        TokenType.FALSE,
        TokenType.FUN,
        TokenType.FOR,
        TokenType.IF,
        TokenType.NIL,
        TokenType.OR,
        TokenType.PRINT,
        TokenType.RETURN,
        TokenType.SUPER,
        TokenType.THIS,
        TokenType.TRUE,
        TokenType.VAR,
        TokenType.WHILE,
        TokenType.EOF
      )
    )

  test("scan treats keyword-prefixed identifiers as identifiers"):
    val tokens = scan("classroom variable")
    assertEquals(tokens.map(_.tokenType), List(TokenType.IDENTIFIER, TokenType.IDENTIFIER, TokenType.EOF))
    assertEquals(tokens(0).lexeme, "classroom")
    assertEquals(tokens(1).lexeme, "variable")

  List(
    ("_",        "_"),
    ("__init__", "__init__"),
  ).foreach: (source, lexeme) =>
    test(s"scan recognizes identifier: $source"):
      val tokens = scan(source)
      assertEquals(tokens.map(_.tokenType), List(TokenType.IDENTIFIER, TokenType.EOF))
      assertEquals(tokens.head.lexeme, lexeme)

  test("scan separates numbers and identifiers in sequence"):
    val tokens = scan("123abc")
    assertEquals(tokens.map(_.tokenType), List(TokenType.NUMBER, TokenType.IDENTIFIER, TokenType.EOF))
    assertEquals(tokens(0).literal, Some(123.0))
    assertEquals(tokens(1).lexeme, "abc")

  test("scan separates scientific-notation-like input into NUMBER and IDENTIFIER"):
    // Scientific notation is not supported at the lexer level
    val tokens = scan("1e10")
    assertEquals(tokens.map(_.tokenType), List(TokenType.NUMBER, TokenType.IDENTIFIER, TokenType.EOF))
    assertEquals(tokens(0).literal, Some(1.0))
    assertEquals(tokens(1).lexeme, "e10")

  // ─── Line number tracking ────────────────────────────────────────────

  test("scan tracks line numbers across mixed content"):
    val tokens = scan("var\n\nfun").filterNot(_.tokenType == TokenType.EOF)
    assertEquals(tokens(0).tokenType, TokenType.VAR)
    assertEquals(tokens(0).line, 1)
    assertEquals(tokens(1).tokenType, TokenType.FUN)
    assertEquals(tokens(1).line, 3)

  test("scan advances line numbers through multi-line block comments"):
    val tokens = scan("/* line\n2\n3\n*/ var")
    assertEquals(tokens.map(_.tokenType), List(TokenType.VAR, TokenType.EOF))
    assertEquals(tokens.head.line, 4)

  test("scan advances line numbers inside multi-line double-quoted strings"):
    val tokens = scan("\"a\nb\nc\"")
    assertEquals(tokens.map(_.tokenType), List(TokenType.STRING, TokenType.EOF))
    assertEquals(tokens.head.line, 3)
    assertEquals(tokens.head.literal, Some("a\nb\nc"))

  test("scan correctly counts multiple sequential newlines"):
    val tokens = scan("\n\n\nvar")
    assertEquals(tokens.map(_.tokenType), List(TokenType.VAR, TokenType.EOF))
    assertEquals(tokens.head.line, 4)
