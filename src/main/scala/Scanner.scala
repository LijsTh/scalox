import scala.collection.mutable.ArrayBuffer

class Scanner(private val source: String):
    private val tokens: ArrayBuffer[Token] = ArrayBuffer()
    private var start: Int = 0
    private var current: Int = 0
    private var line: Int = 1

    def scan(): Seq[Token] =
        while !isAtEnd() do
            start = current
            scanToken()
        
        start = current
        addToken(TokenType.EOF)
        tokens.toSeq

    private def scanToken(): Unit =
        consume() match
            // Whitespace
            case ' ' | '\r' | '\t' => ()
            case '\n' => line += 1

            // Single-character tokens
            case '(' => addToken(TokenType.LEFT_PAREN)
            case ')' => addToken(TokenType.RIGHT_PAREN)
            case '{' => addToken(TokenType.LEFT_BRACE)
            case '}' => addToken(TokenType.RIGHT_BRACE)
            case ',' => addToken(TokenType.COMMA)
            case '.' => addToken(TokenType.DOT)
            case ';' => addToken(TokenType.SEMICOLON)
            case ':' => addToken(TokenType.COLON)
            case '%' => addToken(TokenType.PERCENT)
            case '?' => addToken(TokenType.QUESTION)

            // Slash or comment
            case '/' =>
                if tryConsume('/') then
                    while !isAtEnd() && peek() != '\n' do consume()
                else if tryConsume('*') then
                    var level = 1
                    while level > 0 && !isAtEnd() do
                        if match2('*', '/') then
                            level -= 1
                        else if match2('/', '*') then
                            level += 1
                        else if tryConsume('\n') then
                            line += 1
                        else
                            consume()
                    if level > 0 then
                        throw RuntimeException(s"Unterminated block comment at line $line")
                else
                    addToken(TokenType.SLASH)

            // Single-character operators
            case '+' => addToken(TokenType.PLUS)
            case '-' => addToken(TokenType.MINUS)
            case '!' => addToken(if tryConsume('=') then TokenType.BANG_EQUAL else TokenType.BANG)
            case '=' => addToken(if tryConsume('=') then TokenType.EQUAL_EQUAL else TokenType.EQUAL)
            case '<' => addToken(if tryConsume('=') then TokenType.LESS_EQUAL else TokenType.LESS)
            case '>' => addToken(if tryConsume('=') then TokenType.GREATER_EQUAL else TokenType.GREATER)
            case '*' => addToken(TokenType.STAR)

            // String and char literals
            case '\'' =>
                while !isAtEnd() && peek() != '\'' && peek() != '\n' do consume()
                if isAtEnd() || peek() == '\n' then
                    throw RuntimeException(s"Unterminated string $lexeme at line $line")
                consume()
                val value = source.substring(start + 1, current - 1)
                addToken(TokenType.STRING, Some(value))

            case '"' =>
                while !isAtEnd() && peek() != '"' do
                    if peek() == '\n' then line += 1
                    consume()
                if isAtEnd() then
                    throw RuntimeException(s"Unterminated string at line $line")
                consume()
                val value = source.substring(start + 1, current - 1)
                addToken(TokenType.STRING, Some(value))

            // Numbers
            case c if c.isDigit =>
                var scannedDots = 0
                while !isAtEnd() && (peek().isDigit || peek() == '.') do
                    if peek() == '.' then scannedDots += 1
                    consume()
                if scannedDots > 1 || previous() == '.' then
                    throw RuntimeException(s"Invalid number format $lexeme")
                addToken(TokenType.NUMBER, Some(lexeme.toDouble))

            // Identifiers and they keywords
            case c if isAlpha(c) =>
                while !isAtEnd() && isAlphanumeric(peek()) do consume()
                TokenType.keywords.get(lexeme) match
                    case Some(keyword) => addToken(keyword)
                    case None => addToken(TokenType.IDENTIFIER)

            case c => throw RuntimeException(s"Unexpected character '$c' at line $line")

    // HELPER METHODS

    private def lexeme: String = 
        source.substring(start, current)

    private def addToken(tokenType: TokenType): Unit =
        addToken(tokenType, None)

    private def addToken(tokenType: TokenType, literal: Option[TokenLiteralType]): Unit =
        tokens += Token(tokenType, lexeme, literal, line)

    private def isAtEnd(): Boolean = 
        current >= source.length

    private def peek(): Char =
        if isAtEnd() then '\u0000' else source.charAt(current)

    private def previous(): Char = 
        source.charAt(current - 1)

    private def peekNext(): Char =
        if current + 1 >= source.length then '\u0000' else source.charAt(current + 1)

    private def consume(): Char =
        val c = source.charAt(current)
        current += 1
        c

    private def tryConsume(expected: Char): Boolean =
        if !isAtEnd() && peek() == expected then
            consume()
            true
        else
            false

    private def match2(first: Char, second: Char): Boolean =
        if peek() == first && peekNext() == second then
            consume()
            consume()
            true
        else
            false

    private def isAlpha(c: Char): Boolean =
        c.isLetter || c == '_'

    private def isAlphanumeric(c: Char): Boolean =
        c.isLetterOrDigit || c == '_'
