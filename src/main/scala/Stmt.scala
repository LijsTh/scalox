import scala.collection.mutable.ArrayBuffer

enum Stmt:
    // expressions like : 2;
    case ExpressionStmt(expr: Expr)

    // print statements like: print 2;
    case PrintStmt(expr: Expr)

    // variable declaration statements like: var x = 2;
    case VarDecl(name: String, initializer: Option[Expr])

    // function declaration statements like: fun add(a, b) { return a + b; }
    case FunDecl(name: Token , params: Seq[Token], body: Seq[Stmt])

    case ReturnStmt(value: Option[Expr])

    // block statements like: { print 1; print 2; }
    case BlockStmt(statements: Seq[Stmt])

    // if statements like: if (condition) thenBranch else elseBranch
    case IfStmt(condition: Expr, thenBranch: Stmt, elseBranch: Option[Stmt])

    // while statements like: while (condition) body
    case WhileStmt(condition: Expr, body: Stmt)

    override def toString(): String =
        this match
            case ExpressionStmt(expr) => s"${expr.toString}"
            case PrintStmt(expr) => s"PRINT ${expr.toString}"
            case VarDecl(name, initializer) => s"VAR $name = ${initializer.getOrElse("NIL")}"
            case FunDecl(name, params, body) => 
                s"FUN fn<${name.lexeme}(${params.map(_.lexeme).mkString(", ")})> { ${body.map(_.toString).mkString("; ")} }"
            case ReturnStmt(value) => s"RETURN ${value.map(_.toString).getOrElse("NIL")}"
            case BlockStmt(statements) => s"{ ${statements.map(_.toString).mkString("; ")} }"
            case IfStmt(condition, thenBranch, elseBranch) => 
                s"IF ${condition.toString} THEN ${thenBranch.toString} ELSE ${elseBranch.map(_.toString).getOrElse("NIL")}"
            case WhileStmt(condition, body) => s"WHILE ${condition.toString} ${body.toString}"
