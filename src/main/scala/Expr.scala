enum Expr:
    // LITERAL: NUMBER | STRING | BOOLEAN | NIL
    case LiteralExpr(value: Option[TokenLiteralType])

    // UNARY: "-" | "!" | expression 
    case UnaryExpr(operator: Token, right: Expr)

    // CALL: primary "(" arguments? ")"
    case CallExpr(callee: Expr, arguments: List[Expr])

    // BINARY: expression operator expression
    // operator can be: "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">" | ">="
    case BinaryExpr(left: Expr, operator: Token, right: Expr)

    // GROUPING: "(" expression ")"
    case GroupingExpr(expression: Expr)

    // VARIABLE: identifier;
    case VariableExpr(name: Token)

    // ASSIGNMENT: identifier "=" expression`
    case AssignmentExpr(name: Token, value: Expr)

    // LOGIC: expression operator expression -- for "and" and "or" logical operators
    case LogicExpr(left: Expr, operator: Token, right: Expr)

    override def toString(): String =
        this match
            case LiteralExpr(value) => 
                value match
                    case Some(s: String) => s"<\"$s\">"
                    case Some(d: Double) => s"<$d>"
                    case Some(b: Boolean) => if b then "<TRUE>" else "<FALSE>"
                    case None  => "<NIL>"
            case UnaryExpr(operator, right) => s"(${operator.lexeme} ${right.toString})"
            case CallExpr(callee, arguments) => 
                val argsStr = arguments.map(_.toString).mkString(", ") 
                s"${callee.toString}($argsStr)"
            case BinaryExpr(left, operator, right) => s"(${left.toString} ${operator.lexeme} ${right.toString})"
            case GroupingExpr(expression) => s"(${expression.toString})"
            case VariableExpr(name) => s"<${name.lexeme}>"
            case AssignmentExpr(name, value) => s"<${name.lexeme} = ${value.toString}>"
            case LogicExpr(left, operator, right) => s"(${left.toString} ${operator.lexeme} ${right.toString})"