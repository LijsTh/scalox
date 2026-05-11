import Expr.*
import Stmt.*
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

class Parser(private val tokens: Seq[Token]):
    private var current: Int = 0

    def parse(): Seq[Stmt] = 
        val statements = ArrayBuffer[Stmt]()
        while !isAtEnd() do 
            statements += statement()
        statements.toSeq

    // STATEMENTS ------------------------------------------------

    def statement(): Stmt = 
        peek().tokenType match
            case TokenType.PRINT       => consume(); printStatement()
            case TokenType.LEFT_BRACE  => consume(); blockStatement()
            case TokenType.VAR         => consume(); variableDeclaration()
            case TokenType.FUN         => consume(); funDeclStmt()
            case TokenType.RETURN      => consume(); returnStatement()
            case TokenType.IF          => consume(); ifStatement()
            case TokenType.WHILE       => consume(); whileStatement()
            case TokenType.FOR         => consume(); forStatement()
            case _                     => expressionStatement()

    def printStatement(): PrintStmt = 
        val value = expression()

        tryConsume(TokenType.SEMICOLON) match
            case Some(_) => PrintStmt(value)
            case None => throw RuntimeException(s"Expected ';' after value to print. got '${peek().lexeme}' instead.")

    def expressionStatement(): ExpressionStmt = 
        val expr = expression()

        tryConsume(TokenType.SEMICOLON) match
            case Some(_) => ExpressionStmt(expr)
            case None => throw RuntimeException(s"Expected ';' after expression. got '${peek().lexeme}' instead.")

    def blockStatement(): BlockStmt = 
        BlockStmt(block())

    def block(): Seq[Stmt] = 
        val statements = ArrayBuffer[Stmt]()

        while !check(TokenType.RIGHT_BRACE) && !isAtEnd() do 
            statements += statement()

        tryConsume(TokenType.RIGHT_BRACE) match
            case Some(_) => statements.toSeq
            case None => throw RuntimeException(s"Expected '}' after block. got '${peek().lexeme}' instead.")

    def variableDeclaration(): VarDecl = 
        val variableName = tryConsume(TokenType.IDENTIFIER).getOrElse(
            throw RuntimeException(s"Expected variable name, got '${peek().lexeme}'")
        )
        val variableValue = tryConsume(TokenType.EQUAL).map(_ => expression())
        tryConsume(TokenType.SEMICOLON).getOrElse(
            throw RuntimeException(s"Expected ';' after variable declaration, got '${peek().lexeme}'")
        )
        VarDecl(variableName.lexeme, variableValue)

    def ifStatement(): IfStmt = 
        tryConsume(TokenType.LEFT_PAREN).getOrElse(
            throw RuntimeException(s"Expected '(' after 'if', got '${peek().lexeme}'")
        )
        val condition = expression()
        tryConsume(TokenType.RIGHT_PAREN).getOrElse(
            throw RuntimeException(s"Expected ')' after if condition, got '${peek().lexeme}'")
        )
        val thenBranch = statement()

        val elseBranch = tryConsume(TokenType.ELSE).map(_ => statement())

        IfStmt(condition, thenBranch, elseBranch)

    def whileStatement(): WhileStmt =
        tryConsume(TokenType.LEFT_PAREN).getOrElse(
            throw RuntimeException(s"Expected '(' after 'while', got '${peek().lexeme}'")
        )
        val condition = expression()
        tryConsume(TokenType.RIGHT_PAREN).getOrElse(
            throw RuntimeException(s"Expected ')' after while condition, got '${peek().lexeme}'")
        )
        WhileStmt(condition, statement())

    // For statement parses a for loop and desugars it into a while loop with an optional initializer and increment expression => for (initializer; condition; increment) body  => initializer; while (condition) { body; increment }
    def forStatement(): Stmt = 
        tryConsume(TokenType.LEFT_PAREN).getOrElse(
            throw RuntimeException(s"Expected '(' after 'for', got '${peek().lexeme}'")
        )
        
        val initializer: Option[Stmt] = 
            if check(TokenType.SEMICOLON) then
                consume()
                None
            else if check(TokenType.VAR) then
                consume()
                Some(variableDeclaration())
            else
                Some(expressionStatement())
        
        val condition: Expr = 
            if check(TokenType.SEMICOLON) then
                LiteralExpr(Some(true))  // No condition = infinite loop
            else
                expression()
        
        tryConsume(TokenType.SEMICOLON).getOrElse(
            throw RuntimeException(s"Expected ';' after for loop condition, got '${peek().lexeme}'")
        )
        
        val increment: Option[Expr] = 
            if check(TokenType.RIGHT_PAREN) then
                None
            else
                Some(expression())
        
        tryConsume(TokenType.RIGHT_PAREN).getOrElse(
            throw RuntimeException(s"Expected ')' after for loop clauses, got '${peek().lexeme}'")
        )
        
        var body = statement()
        
        body = increment.fold(body)(inc => 
            BlockStmt(Seq(body, ExpressionStmt(inc)))
        )
        
        body = WhileStmt(condition, body)
        
        initializer.fold(body)(init => 
            BlockStmt(Seq(init, body))
        )

    def funDeclStmt(): FunDecl = 
        val name = tryConsume(TokenType.IDENTIFIER).getOrElse(
            throw RuntimeException(s"Expected function name, got '${peek()}'")
        )
        tryConsume(TokenType.LEFT_PAREN).getOrElse(
            throw RuntimeException(s"Expected '(' after function name, got '${peek()}'")
        )

        val params = ArrayBuffer[Token]()
        if !check(TokenType.RIGHT_PAREN) then
            params += tryConsume(TokenType.IDENTIFIER).getOrElse(
                throw RuntimeException(s"Expected parameter name, got '${peek()}'")
            )
            while tryConsume(TokenType.COMMA).isDefined do
                params += tryConsume(TokenType.IDENTIFIER).getOrElse(
                    throw RuntimeException(s"Expected parameter name after ',', got '${peek()}'")
                )

        tryConsume(TokenType.RIGHT_PAREN).getOrElse(
            throw RuntimeException(s"Expected ')' after function parameters, got '${peek()}'")
        )

        tryConsume(TokenType.LEFT_BRACE).getOrElse(
            throw RuntimeException(s"Expected '{' before function body, got '${peek()}'")
        )

        val body = block()
        FunDecl(name, params.toSeq, body)

    def returnStatement(): ReturnStmt =
        val value =
            if check(TokenType.SEMICOLON) then None
            else Some(expression())

        tryConsume(TokenType.SEMICOLON).getOrElse(
            throw RuntimeException(s"Expected ';' after return value, got '${peek().lexeme}'")
        )

        ReturnStmt(value)

    // EXPRESSIONS ------------------------------------------------ 

    def expression(): Expr = assignment()

    // Assignment expressions: identifier "=" expression || Equality expressions
    def assignment(): Expr = 
        val expr = logicOr() 
        tryConsume(TokenType.EQUAL).map{_ => 
            expr match
                case VariableExpr(name) => 
                    val value = assignment()
                    AssignmentExpr(name, value)
                case _ => throw RuntimeException("Invalid assignment target.")            
        }.getOrElse(expr)

    def logicOr(): Expr = parseLogical(logicAnd, TokenType.OR)

    def logicAnd(): Expr = parseLogical(equality, TokenType.AND)

    // Equality expressions: '==' and '!=' || Comparison expressions
    def equality(): Expr = 
        parseLeftAssociative(comparison, TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)

    // Comparison expressions: '>', '<', '>=', '<=' || Term expressions
    def comparison(): Expr = 
        parseLeftAssociative(term, TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)

    // Term expressions: '+' and '-' || Factor expressions
    def term(): Expr = 
        parseLeftAssociative(factor, TokenType.PLUS, TokenType.MINUS)

    // Factor expressions: '*' and '/' and '%' || Unary expressions
    def factor(): Expr = 
        parseLeftAssociative(unary, TokenType.STAR, TokenType.SLASH, TokenType.PERCENT)

    // Unary expressions: '!' and '-' || Primary expressions
    def unary(): Expr = 
        tryConsume(TokenType.BANG, TokenType.MINUS) match
            case Some(operator) => UnaryExpr(operator, unary())
            case None => call()

    // Call expressions: primary "(" arguments? ")" || Primary expressions 
    def call(): Expr = 
        var expr = primary()
        
        // Supports chained function calls like: foo()(1)
        while tryConsume(TokenType.LEFT_PAREN).isDefined do
            val arguments = ArrayBuffer[Expr]()
            if !check(TokenType.RIGHT_PAREN) then
                arguments += expression()
                while tryConsume(TokenType.COMMA).nonEmpty do
                    arguments += expression()
            
            tryConsume(TokenType.RIGHT_PAREN).getOrElse(
                throw RuntimeException(s"Expected ')' after arguments, got '${peek().lexeme}'")
            )
            expr = CallExpr(expr, arguments.toList)
        expr

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
                    case None => throw RuntimeException("Expected ')' after expression.")
            case TokenType.IDENTIFIER => 
                VariableExpr(token)
            case _ => throw RuntimeException(s"Expected expression, got '${token.lexeme}' instead.")

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

    private def parseLogical(nextLevel: () => Expr, operator: TokenType): Expr = 
        @tailrec
        def loop(left: Expr): Expr = 
            tryConsume(operator) match
                case Some(op) => loop(LogicExpr(left, op, nextLevel()))
                case None => left
        loop(nextLevel())

    private def tryConsume(tokenTypes: TokenType*): Option[Token] =
        if check(tokenTypes*) then Some(consume()) else None
