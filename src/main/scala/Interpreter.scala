import Expr.*

class Interpreter: 

    // TODO: Handle null to nil (at least in prints) 
    def interpret(expr: Expr): Unit = 
        val value = evaluate(expr)
        println((value))

    def evaluate(expr: Expr): Any =
        expr match
            case literal: LiteralExpr => evaluateLiteral(literal)
            case grouping: GroupingExpr => evaluateGrouping(grouping)
            case unary: UnaryExpr => evaluateUnary(unary)
            case binary: BinaryExpr => evaluateBinary(binary)
            case null => throw new RuntimeException(s"Unknown expression type: ${expr.getClass.getSimpleName}")

    private def evaluateLiteral(literal: LiteralExpr): Any =
        literal.value.getOrElse(null)

    private def evaluateGrouping(grouping: GroupingExpr): Any =
        evaluate(grouping.expression)

    private def evaluateUnary(unary: UnaryExpr): Any =
        val right = evaluate(unary.right)
        unary.operator.tokenType match
            case TokenType.MINUS =>
                right match
                    case d: Double => -d
                    case _ => throw new RuntimeException(s"Operand must be a number for '-' operator.")
            case TokenType.BANG =>
                !is_truthy(right)
            case _ => throw new RuntimeException(s"Unknown unary operator: ${unary.operator.lexeme}")

    private def evaluateBinary(binary: BinaryExpr): Any =
        val left = evaluate(binary.left)
        val right = evaluate(binary.right)

        binary.operator.tokenType match 
            case TokenType.PLUS => 
                (left, right) match
                    // Lox allows both number addition and string concatenation with the '+' operator
                    case (d1: Double, d2: Double) => d1 + d2
                    case (s1: String, s2: String) => s1 + s2
                    case _ => throw new RuntimeException(s"Operands must be two numbers or two strings for '+' operator.")

            case TokenType.MINUS =>
                (left, right) match
                    case (d1: Double, d2: Double) => d1 - d2
                    case _ => throw new RuntimeException(s"Operands must be numbers for '-' operator.")

            case TokenType.STAR =>
                (left, right) match
                    case (d1: Double, d2: Double) => d1 * d2
                    case _ => throw new RuntimeException(s"Operands must be numbers for '*' operator.")

            case TokenType.SLASH =>
                (left, right) match
                    case (d1: Double, d2: Double) =>
                        if d2 == 0 then throw new RuntimeException("Division by zero.")
                        else d1 / d2
                    case _ => throw new RuntimeException(s"Operands must be numbers for '/' operator.")

            case TokenType.GREATER => 
                (left, right) match
                    case (d1: Double, d2: Double) => d1 > d2
                    case _ => throw new RuntimeException(s"Operands must be numbers for '>' operator.")

            case TokenType.GREATER_EQUAL =>
                (left, right) match
                    case (d1: Double, d2: Double) => d1 >= d2
                    case _ => throw new RuntimeException(s"Operands must be numbers for '>=' operator.")

            case TokenType.LESS =>
                (left, right) match
                    case (d1: Double, d2: Double) => d1 < d2
                    case _ => throw new RuntimeException(s"Operands must be numbers for '<' operator.")

            case TokenType.LESS_EQUAL =>
                (left, right) match
                    case (d1: Double, d2: Double) => d1 <= d2
                    case _ => throw new RuntimeException(s"Operands must be numbers for '<=' operator.")

            case TokenType.EQUAL_EQUAL =>
                left == right

            case TokenType.BANG_EQUAL =>
                left != right

            case _ => throw new RuntimeException(s"Unknown binary operator: ${binary.operator}")

    // HELPER METHODS
    private def is_truthy(value: Any): Boolean =
        value match
            case null => false
            case b: Boolean => b
            case _ => true
