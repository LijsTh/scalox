enum Expr:
    // LITERAL: NUMBER | STRING | BOOLEAN | NIL
    case LiteralExpr(value: Option[TokenLiteralType])

    // UNARY: "-" | "!" | expression 
    case UnaryExpr(operator: Token, right: Expr)

    // BINARY: expression operator expression
    // operator can be: "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">" | ">="
    case BinaryExpr(left: Expr, operator: Token, right: Expr)

    // GROUPING: "(" expression ")"
    case GroupingExpr(expression: Expr)

    /*
    TODO: 
        - Add more expression types as needed (call, variable, assignment, logic, postfix, ternary, etc.)
    */

    override def toString(): String =
        this match
            case LiteralExpr(value) => 
                value match
                    case Some(s: String) => s"<\"$s\">"
                    case Some(d: Double) => s"<$d>"
                    case Some(b: Boolean) => if b then "<TRUE>" else "<FALSE>"
                    case None  => "<NIL>"
            case UnaryExpr(operator, right) => s"(${operator.lexeme} ${right.toString})"
            case BinaryExpr(left, operator, right) => s"(${left.toString} ${operator.lexeme} ${right.toString})"
            case GroupingExpr(expression) => s"(${expression.toString})"

