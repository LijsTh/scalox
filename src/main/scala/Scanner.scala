import scala.collection.mutable.ArrayBuffer

class Scanner(private val source: String):
    private val tokens: ArrayBuffer[Token] = ArrayBuffer()
    private var start: Int = 0
    private var current_pos: Int = 0
    private var line: Int = 1

    def scan(): Array[Token] =
        while (!isAtEnd) do
            start = current_pos
            scanToken()
        
        start = current_pos
        addToken(TokenType.EOF)
        tokens.toArray

    private def lexeme: String =
        source.substring(start, current_pos)

    private def addToken(tokenType: TokenType): Unit =
        addToken(tokenType, None)

    private def addToken(tokenType: TokenType, literal: Option[TokenLiteralTypes]): Unit =
        val text = lexeme
        tokens += Token(tokenType, text, literal, line)

    // Check if we've reached the end of the source code
    private def isAtEnd: Boolean =
        current_pos >= source.length

    // Look at the current character without consuming it
    private def lookAhead: Char =
        if isAtEnd then '\u0000' else source.charAt(current_pos)
    
    // Look at the next character without consuming it
    private def peekNext: Char =
        if current_pos + 1 >= source.length() then '\u0000' else source.charAt(current_pos + 1)
    
    // Consume the current character and move to the next one
    private def advance: Char =
        val c = source.charAt(current_pos)
        current_pos += 1
        c

    private def previous: Char =
        source.charAt(current_pos - 1)

    private def consumeIf(expected: Char): Boolean =
        if lookAhead != expected then false
        else
            current_pos += 1
            true

    private def isAlpha(c: Char): Boolean =
        c.isLetter || c == '_'

    private def isAlphanumeric(c: Char): Boolean =
        c.isLetterOrDigit || c == '_'

    private def scanToken(): Unit =
        val c = advance
        // c is the actual character but current_pos has already moved to the next character
        // so lookAhead is the next character after c
        c match
            // Ignore whitespace
            case ' ' | '\r' | '\t' => () 

            // Add a newline
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

            // Comment
            case '/' => 
                if consumeIf('/') then
                    // inline comment, consume until end of line
                    while lookAhead != '\n' && !isAtEnd do advance
                else if consumeIf('*') then
                    var level = 1
                    while level > 0 && !isAtEnd do
                        if consumeIf('\n') then line += 1
    
                        else if consumeIf('*') && consumeIf('/') then level -= 1

                        else if consumeIf('/') && consumeIf('*') then level += 1

                        else advance

                    if level > 0 then 
                        throw new RuntimeException(s"Unterminated block comment at line $line")

                else addToken(TokenType.SLASH)

            // Two-character tokens
            case '+' => 
                addToken(if consumeIf('+') then TokenType.PLUS_PLUS else TokenType.PLUS)

            case '-' =>
                addToken(if consumeIf('-') then TokenType.MINUS_MINUS else TokenType.MINUS)

            case '!' => 
                addToken(if consumeIf('=') then TokenType.BANG_EQUAL else TokenType.BANG)

            case '=' =>
                addToken(if consumeIf('=') then TokenType.EQUAL_EQUAL else TokenType.EQUAL)

            case '<' =>
                addToken(if consumeIf('=') then TokenType.LESS_EQUAL else TokenType.LESS)

            case '>' =>
                addToken(if consumeIf('=') then TokenType.GREATER_EQUAL else TokenType.GREATER)

            // Character literals
            case '\'' => 
                // Consume characters until we find a closing single quote or a newline (unterminated string)
                while !isAtEnd &&
                    lookAhead != '\'' && 
                    lookAhead != '\n' do advance
                if isAtEnd || lookAhead == '\n' then
                    throw new RuntimeException(s"Unterminated string $lexeme")

                advance 

                val value = source.substring(start + 1, current_pos - 1)
                addToken(TokenType.STRING, Some(value))

            // String literals
            case '"' =>
                // Consume characters until we find a closing double quote or a newline
                while !isAtEnd &&
                    lookAhead != '"' do
                    if lookAhead == '\n' then line += 1
                    advance
                
                if isAtEnd then
                    throw new RuntimeException(s"Unterminated string $lexeme")
                
                advance

                val strValue = source.substring(start + 1, current_pos - 1)
                addToken(TokenType.STRING, Some(strValue))

            // Numbers (integers and floats)
            case d if d.isDigit =>
                var scannedDots = 0

                // Consume digits and at most one dot for floating-point numbers
                while !isAtEnd && (lookAhead.isDigit || lookAhead == '.') do
                    if lookAhead == '.' then scannedDots += 1
                    advance

                if scannedDots > 1 then
                    throw new RuntimeException(s"Invalid number format $lexeme")

                if previous == '.' then
                    throw new RuntimeException(s"Invalid number format $lexeme")
                
                addToken(TokenType.NUMBER, Some(lexeme.toDouble))

            // Identifiers and keywords
            case a if isAlpha(a) =>
                while !isAtEnd && isAlphanumeric(lookAhead) do advance
                val text = lexeme
                
                TokenType.keywords.get(text) match
                    case Some(lexeme) => addToken(lexeme)
                    case None => addToken(TokenType.IDENTIFIER)

            case _ => 
                throw new RuntimeException(s"Unexpected character '$c' at line $line")
