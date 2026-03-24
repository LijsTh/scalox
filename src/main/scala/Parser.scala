import Expr.*
import scala.collection.mutable.ArrayBuffer

class Parser(private val tokens: ArrayBuffer[Token]):
    private var current: Int = 0

    def parse(): Expr = 
        if isAtEnd() then
            LiteralExpr(None)
        else 
            expression()
            // if !isAtEnd() then
            //     throw new RuntimeException("Unexpected token: " + lookAhead().lexeme)
            // expr

    def expression(): Expr = 
        equality()

    // Equality expressions: '==' and '!=' || Comparison expressions
    def equality(): Expr = 
        var expr = comparison()

        // While the next token is an equality operator, consume it and parse the right-hand side
        while !isAtEnd() && matchToken(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL) do
            val operator = previous()
            val right = comparison()
            expr = BinaryExpr(expr, operator, right)
        expr


    // Comparison expressions: '>', '<', '>=', '<=' || Term expressions
    def comparison(): Expr = 
        var expr = term()

        // While the next token is a comparison operator, consume it and parse the right-hand side
        while !isAtEnd() && matchToken(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL) do
            val operator = previous()
            val right = term()
            expr = BinaryExpr(expr, operator, right)
        expr

    // Term expressions: '+' and '-' || Factor expressions
    def term(): Expr = 
        var expr = factor()

        // While the next token is a term operator, consume it and parse the right-hand side
        while !isAtEnd() && matchToken(TokenType.PLUS, TokenType.MINUS) do
            val operator = previous()
            val right = factor()
            expr = BinaryExpr(expr, operator, right)
        expr

    // Factor expressions: '*' and '/' || Unary expressions
    def factor(): Expr = 
        var expr = unary()

        // While the next token is a factor operator, consume it and parse the right-hand side
        while !isAtEnd() && matchToken(TokenType.STAR, TokenType.SLASH) do
            val operator = previous()
            val right = unary()
            expr = BinaryExpr(expr, operator, right)
        expr

    // Unary expressions: '!' and '-' || Primary expressions
    def unary(): Expr = 
        // Prefix operators: '!' and '-'
        if matchToken(TokenType.BANG, TokenType.MINUS) then
            val operator = previous()
            val right = unary()
            UnaryExpr(operator, right)
        else
            primary()

    // Primary expressions: NUMBER, STRING, TRUE, FALSE, NIL, and parenthesized expressions
    def primary (): Expr = 
        if matchToken(TokenType.FALSE) then
            LiteralExpr(Some(false))
        else if matchToken(TokenType.TRUE) then
            LiteralExpr(Some(true))
        else if matchToken(TokenType.NIL) then
            LiteralExpr(None)
        
        // Handle literals (numbers and strings)
        else if matchToken(TokenType.NUMBER, TokenType.STRING) then
            LiteralExpr(previous().literal)
        
        // Handle parenthesized expressions recursively
        else if matchToken(TokenType.LEFT_PAREN) then
            val expr = expression()
            if !matchToken(TokenType.RIGHT_PAREN) then
                throw new RuntimeException("Expected ')' after expression.")
            GroupingExpr(expr)
        else
            throw new RuntimeException("Expected expression, got '" + lookAhead().lexeme + "' instead.")


            
    // HELPER METHODS 
    private def isAtEnd(): Boolean = 
        return lookAhead().tokenType == TokenType.EOF

    private def previous(): Token =
        return tokens(current - 1)

    private def lookAhead(): Token = 
        return tokens(current)

    private def advance(): Token = 
        val token = lookAhead()
        if !isAtEnd() then
            current += 1
        return token

    private def matchToken(tokenTypes: TokenType*): Boolean =
        val matches = tokenTypes.contains(lookAhead().tokenType)
        if matches then advance()
        matches
