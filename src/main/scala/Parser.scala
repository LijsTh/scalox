import Expr.*
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

class Parser(private val tokens: Array[Token]):
    private var current: Int = 0

    def parse(): Expr = 
        if isAtEnd() then
            LiteralExpr(None)
        else 
            expression()
            // if !isAtEnd() then
            //     throw new RuntimeException("Unexpected token: " + peek().lexeme)
            // expr

    def expression(): Expr = equality()

    // Equality expressions: '==' and '!=' || Comparison expressions
    def equality(): Expr = 
        parseLeftAssociative(comparison, TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)

    // Comparison expressions: '>', '<', '>=', '<=' || Term expressions
    def comparison(): Expr = 
        parseLeftAssociative(term, TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)

    // Term expressions: '+' and '-' || Factor expressions
    def term(): Expr = 
        parseLeftAssociative(factor, TokenType.PLUS, TokenType.MINUS)

    // Factor expressions: '*' and '/' || Unary expressions
    def factor(): Expr = 
        parseLeftAssociative(unary, TokenType.STAR, TokenType.SLASH, TokenType.PERCENT)

    // Unary expressions: '!' and '-' || Primary expressions
    def unary(): Expr = 
        tryConsume(TokenType.BANG, TokenType.MINUS) match
            case Some(operator) => UnaryExpr(operator, unary())
            case None => primary()

    // Primary expressions: NUMBER, STRING, TRUE, FALSE, NIL, and parenthesized expressions
    def primary (): Expr = 
        val token = consume()
        token.tokenType match
            case TokenType.FALSE => LiteralExpr(Some(false))
            case TokenType.TRUE => LiteralExpr(Some(true))
            case TokenType.NIL => LiteralExpr(None)
            case TokenType.NUMBER | TokenType.STRING => LiteralExpr(token.literal)
            case TokenType.LEFT_PAREN => 
                val expr = expression()
                tryConsume(TokenType.RIGHT_PAREN) match
                    case Some(_) => GroupingExpr(expr)
                    case None => throw new RuntimeException("Expected ')' after expression.")
            case _ => throw new RuntimeException(s"Expected expression, got '${token.lexeme}' instead.")

    // HELPER METHODS 
    private def peek(): Token = 
        tokens(current)

    private def isAtEnd(): Boolean = 
        peek().tokenType == TokenType.EOF

    private def check(tokenTypes: TokenType*): Boolean = 
        !isAtEnd() && tokenTypes.contains(peek().tokenType)

    private def consume(): Token =
        val token = peek()
        if !isAtEnd() then
            current += 1
        token

    private def parseLeftAssociative(nextLevel: () => Expr, operators: TokenType*): Expr = 
        @tailrec
        def loop(left: Expr): Expr = 
            tryConsume(operators*) match
                case Some(operator) => loop(BinaryExpr(left, operator, nextLevel()))
                case None => left
        loop(nextLevel())

    private def tryConsume(tokenTypes: TokenType*): Option[Token] =
        if check(tokenTypes*) then Some(consume()) else None
