class ScannerSuite extends munit.FunSuite:

  private def scan(source: String): List[Token] =
    Scanner(source).scan().toList

  private def tokenTypes(source: String): List[TokenType] =
    scan(source).map(_.tokenType)

  test("scan returns EOF for empty source"):
    // Example: "" -> EOF
    val tokens = scan("")
    assertEquals(tokens.map(_.tokenType), List(TokenType.EOF))

  test("scan returns EOF when source contains only whitespace"):
    // Example: "   \r\t\n" -> EOF
    val tokens = scan("   \r\t\n")
    assertEquals(tokens.map(_.tokenType), List(TokenType.EOF))

  test("scan ignores line comments and continues on the next line"):
    // Example: "// comment\nvar" -> VAR EOF
    val tokens = scan("// comentario\nvar")
    assertEquals(tokens.map(_.tokenType), List(TokenType.VAR, TokenType.EOF))
    assertEquals(tokens.head.line, 2)

  test("scan ignores line comments until EOF when no newline is present"):
    // Example: "// comment" -> EOF
    val tokens = scan("// comment")
    assertEquals(tokens.map(_.tokenType), List(TokenType.EOF))

  test("scan ignores block comments and nested block comments"):
    // Example: "/* a /* b */ c */ var" -> VAR EOF
    val tokens = scan("/* a\n/* b */\nc */\nvar")
    assertEquals(tokens.map(_.tokenType), List(TokenType.VAR, TokenType.EOF))
    assertEquals(tokens.head.line, 4)

  test("scan fails on unterminated block comment"):
    // Example: "/* comment" -> RuntimeException Unterminated block comment
    val ex = intercept[RuntimeException] {
      Scanner("/* comment").scan()
    }
    assert(clue(ex.getMessage).contains("Unterminated block comment"))

  test("scan recognizes supported single-character tokens"):
    // Example: "(){},;:%?/" -> LEFT_PAREN ... SLASH EOF
    val types = tokenTypes("(){},;:%?/")
    assertEquals(
      types,
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
    // Example: "+++ --- !! != ! == = <= < >= >" -> mix of one-char and two-char tokens
    val types = tokenTypes("+++ --- != ! == = <= < >= >")
    assertEquals(
      types,
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

  test("scan recognizes single-quoted string literals"):
    // Example: "'hello'" -> STRING EOF with literal "hello"
    val tokens = scan("'hello'")
    assertEquals(tokens.map(_.tokenType), List(TokenType.STRING, TokenType.EOF))
    assertEquals(tokens.head.literal, Some("hello"))

  test("scan fails on unterminated single-quoted string with newline"):
    // Example: "'hello\nworld'" -> RuntimeException Unterminated string
    val ex = intercept[RuntimeException] {
      Scanner("'hello\nworld'").scan()
    }
    assert(clue(ex.getMessage).contains("Unterminated string"))

  test("scan fails on unterminated single-quoted string at EOF"):
    // Example: "'hello" -> RuntimeException Unterminated string
    val ex = intercept[RuntimeException] {
      Scanner("'hello").scan()
    }
    assert(clue(ex.getMessage).contains("Unterminated string"))

  test("scan recognizes multi-line double-quoted strings"):
    // Example: "\"hello\nworld\"" -> STRING EOF with literal containing \n
    val tokens = scan("\"hello\nworld\"")
    assertEquals(tokens.map(_.tokenType), List(TokenType.STRING, TokenType.EOF))
    assertEquals(tokens.head.literal, Some("hello\nworld"))
    assertEquals(tokens.head.line, 2)

  test("scan fails on unterminated double-quoted string"):
    // Example: "\"hello" -> RuntimeException Unterminated string
    val ex = intercept[RuntimeException] {
      Scanner("\"hello").scan()
    }
    assert(clue(ex.getMessage).contains("Unterminated string"))

  test("scan fails on unterminated multi-line double-quoted string at EOF"):
    // Example: "\"hello\nworld" -> RuntimeException Unterminated string
    val ex = intercept[RuntimeException] {
      Scanner("\"hello\nworld").scan()
    }
    assert(clue(ex.getMessage).contains("Unterminated string"))

  test("scan recognizes integer and floating-point numbers"):
    // Example: "123 45.67" -> NUMBER NUMBER EOF
    val tokens = scan("123 45.67")
    assertEquals(tokens.map(_.tokenType), List(TokenType.NUMBER, TokenType.NUMBER, TokenType.EOF))
    assertEquals(tokens(0).literal, Some(123.0))
    assertEquals(tokens(1).literal, Some(45.67))

  test("scan fails on invalid number formats"):
    // Example: "1..2", "1.2.3", "1." -> RuntimeException Invalid number format
    val ex1 = intercept[RuntimeException] {
      Scanner("1..2").scan()
    }
    assert(clue(ex1.getMessage).contains("Invalid number format"))

    val ex2 = intercept[RuntimeException] {
      Scanner("1.2.3").scan()
    }
    assert(clue(ex2.getMessage).contains("Invalid number format"))

    val ex3 = intercept[RuntimeException] {
      Scanner("1.").scan()
    }
    assert(clue(ex3.getMessage).contains("Invalid number format"))

  test("scan recognizes identifiers and keywords"):
    // Example: "foo _bar t0 and class this while" -> IDENTIFIER...KEYWORDS...EOF
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
    // Example: "and class else false fun for if nil or print return super this true var while"
    val tokens = scan("and class else false fun for if nil or print return super this true var while")
    assertEquals(
      tokens.map(_.tokenType),
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

  test("scan separates numbers and identifiers in sequence"):
    // Example: "123abc" -> NUMBER IDENTIFIER EOF
    val tokens = scan("123abc")
    assertEquals(tokens.map(_.tokenType), List(TokenType.NUMBER, TokenType.IDENTIFIER, TokenType.EOF))
    assertEquals(tokens(0).literal, Some(123.0))
    assertEquals(tokens(1).lexeme, "abc")

  test("scan fails on unexpected characters"):
    // Example: "@", ".", "*" -> RuntimeException Unexpected character
    val ex1 = intercept[RuntimeException] {
      Scanner("@").scan()
    }
    assert(clue(ex1.getMessage).contains("Unexpected character"))

  test("scan recognizes empty single-quoted string"):
    // Example: "''" -> STRING EOF with literal ""
    val tokens = scan("''")
    assertEquals(tokens.map(_.tokenType), List(TokenType.STRING, TokenType.EOF))
    assertEquals(tokens.head.literal, Some(""))

  test("scan recognizes empty double-quoted string"):
    // Example: "\"\"" -> STRING EOF with literal ""
    val tokens = scan("\"\"")
    assertEquals(tokens.map(_.tokenType), List(TokenType.STRING, TokenType.EOF))
    assertEquals(tokens.head.literal, Some(""))

  test("scan treats keyword-prefixed identifiers as identifiers"):
    // Example: "classroom" is not CLASS + ROOM; "variable" is not VAR + IABLE
    val tokens = scan("classroom variable")
    assertEquals(tokens.map(_.tokenType), List(TokenType.IDENTIFIER, TokenType.IDENTIFIER, TokenType.EOF))
    assertEquals(tokens(0).lexeme, "classroom")
    assertEquals(tokens(1).lexeme, "variable")

  test("scan recognizes underscore-only identifier"):
    // Example: "_" -> IDENTIFIER EOF
    val tokens = scan("_")
    assertEquals(tokens.map(_.tokenType), List(TokenType.IDENTIFIER, TokenType.EOF))
    assertEquals(tokens.head.lexeme, "_")

  test("scan recognizes double-underscore identifiers"):
    // Example: "__init__" -> IDENTIFIER EOF
    val tokens = scan("__init__")
    assertEquals(tokens.map(_.tokenType), List(TokenType.IDENTIFIER, TokenType.EOF))
    assertEquals(tokens.head.lexeme, "__init__")

  test("scan recognizes zero as a number"):
    // Example: "0" -> NUMBER 0.0; "0.0" -> NUMBER 0.0
    val t1 = scan("0")
    assertEquals(t1.map(_.tokenType), List(TokenType.NUMBER, TokenType.EOF))
    assertEquals(t1.head.literal, Some(0.0))

    val t2 = scan("0.0")
    assertEquals(t2.map(_.tokenType), List(TokenType.NUMBER, TokenType.EOF))
    assertEquals(t2.head.literal, Some(0.0))

  test("scan parses numbers with leading zeros"):
    // Example: "007" -> NUMBER 7.0 (toDouble strips leading zeros)
    val tokens = scan("007")
    assertEquals(tokens.map(_.tokenType), List(TokenType.NUMBER, TokenType.EOF))
    assertEquals(tokens.head.literal, Some(7.0))

  test("scan recognizes very small and very large numbers"):
    // Example: "0.001" -> NUMBER 0.001; "999999999" -> NUMBER 999999999.0
    val small = scan("0.001")
    assertEquals(small.head.literal, Some(0.001))
    val large = scan("999999999")
    assertEquals(large.head.literal, Some(999999999.0))

  test("scan treats negative sign as MINUS followed by NUMBER"):
    // Negative number literals don't exist at the lexer level; -1 is MINUS + NUMBER(1.0)
    val tokens = scan("-1")
    assertEquals(tokens.map(_.tokenType), List(TokenType.MINUS, TokenType.NUMBER, TokenType.EOF))
    assertEquals(tokens(1).literal, Some(1.0))

  test("scan separates scientific-notation-like input into NUMBER and IDENTIFIER"):
    // Example: "1e10" -> NUMBER(1.0) IDENTIFIER("e10") EOF
    // Scientific notation is not supported at the lexer level
    val tokens = scan("1e10")
    assertEquals(tokens.map(_.tokenType), List(TokenType.NUMBER, TokenType.IDENTIFIER, TokenType.EOF))
    assertEquals(tokens(0).literal, Some(1.0))
    assertEquals(tokens(1).lexeme, "e10")

  test("scan handles empty block comment"):
    // Example: "/**/" -> EOF
    val tokens = scan("/**/")
    assertEquals(tokens.map(_.tokenType), List(TokenType.EOF))

  test("scan handles deeply nested block comments"):
    // Example: "/* /* /* x */ */ */" -> EOF
    val tokens = scan("/* /* /* x */ */ */")
    assertEquals(tokens.map(_.tokenType), List(TokenType.EOF))

  test("scan handles asterisks inside block comments without false close"):
    // Example: "/* *** */" -> EOF
    val tokens = scan("/* *** */")
    assertEquals(tokens.map(_.tokenType), List(TokenType.EOF))

  test("scan treats line comment syntax as inert inside block comments"):
    // Example: "/* // */" -> EOF  (// has no special meaning inside /* */)
    val tokens = scan("/* // */")
    assertEquals(tokens.map(_.tokenType), List(TokenType.EOF))

  test("scan handles multiple consecutive line comments"):
    // Example: "// a\n// b\n// c\nvar" -> VAR at line 4 EOF
    val tokens = scan("// a\n// b\n// c\nvar")
    assertEquals(tokens.map(_.tokenType), List(TokenType.VAR, TokenType.EOF))
    assertEquals(tokens.head.line, 4)

  test("scan handles block comment followed immediately by code"):
    // Example: "/* comment */var" -> VAR EOF
    val tokens = scan("/* comment */var")
    assertEquals(tokens.map(_.tokenType), List(TokenType.VAR, TokenType.EOF))

  test("scan fails on imbalanced nested block comment"):
    // Example: "/* /* */" opens two levels but only closes one -> Unterminated block comment
    val ex = intercept[RuntimeException] {
      Scanner("/* /* */").scan()
    }
    assert(clue(ex.getMessage).contains("Unterminated block comment"))

  test("scan tracks line numbers across mixed content"):
    // Example: "var\n\nfun" -> VAR at line 1, FUN at line 3
    val tokens = scan("var\n\nfun")
    val nonEof = tokens.filterNot(_.tokenType == TokenType.EOF)
    assertEquals(nonEof(0).tokenType, TokenType.VAR)
    assertEquals(nonEof(0).line, 1)
    assertEquals(nonEof(1).tokenType, TokenType.FUN)
    assertEquals(nonEof(1).line, 3)

  test("scan advances line numbers through multi-line block comments"):
    // Example: "/* line\n2\n3\n*/ var" -> VAR at line 4
    val tokens = scan("/* line\n2\n3\n*/ var")
    assertEquals(tokens.map(_.tokenType), List(TokenType.VAR, TokenType.EOF))
    assertEquals(tokens.head.line, 4)

  test("scan advances line numbers inside multi-line double-quoted strings"):
    // Example: "\"a\nb\nc\"" -> STRING at line 3 (newlines inside double-quoted strings are counted)
    val tokens = scan("\"a\nb\nc\"")
    assertEquals(tokens.map(_.tokenType), List(TokenType.STRING, TokenType.EOF))
    assertEquals(tokens.head.line, 3)
    assertEquals(tokens.head.literal, Some("a\nb\nc"))

  test("scan correctly counts multiple sequential newlines"):
    // Example: "\n\n\nvar" -> VAR at line 4
    val tokens = scan("\n\n\nvar")
    assertEquals(tokens.map(_.tokenType), List(TokenType.VAR, TokenType.EOF))
    assertEquals(tokens.head.line, 4)
