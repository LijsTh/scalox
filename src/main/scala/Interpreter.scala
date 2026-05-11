import Expr.*
import Stmt.*
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import java.util.IdentityHashMap
import scala.jdk.CollectionConverters.*

class Interpreter(globalEnv: Env = new Env()):

    private var currentEnv: Env = globalEnv

    // Interpreter knows the depth that it should resolve each variable. 
    // Uses identity-based map so different AST nodes with the same content
    // (e.g., two `VariableExpr("i")` at different positions) are distinct keys.
    private val localScopeDepths: IdentityHashMap[VariableExpr | AssignmentExpr, Int] = new IdentityHashMap()

    def interpret(statements: Seq[Stmt]): Unit = 
        for stmt <- statements do
            execute(stmt)

    def resolveDepth(expr: VariableExpr | AssignmentExpr, depth: Int): Unit = 
        localScopeDepths.put(expr, depth)

    def getLocalScopeDepths: IdentityHashMap[VariableExpr | AssignmentExpr, Int] = localScopeDepths

    // STATEMENTS EXECUTIONS ------------------------------------------------

    def execute(stmt: Stmt): Unit = 
        stmt match 
            case ExpressionStmt(expr) => executeExpression(ExpressionStmt(expr))
            case PrintStmt(expr) => executePrint(PrintStmt(expr))
            case VarDecl(name, initializer) => executeVarDecl(VarDecl(name, initializer))
            case BlockStmt(statements) => executeBlockStmt(BlockStmt(statements))
            case IfStmt(condition, thenBranch, elseBranch) => executeIfStmt(IfStmt(condition, thenBranch, elseBranch))
            case WhileStmt(condition, body) => executeWhileStmt(WhileStmt(condition, body))
            case FunDecl(name, params, body) => executeFunctionDeclaration(FunDecl(name, params, body))
            case ReturnStmt(value) => executeReturn(ReturnStmt(value))

    def executeExpression(stmt: ExpressionStmt): Unit = 
        evaluate(stmt.expr)

    def executePrint(stmt: PrintStmt): Unit = 
        val value = evaluate(stmt.expr)
        println(if value == null then "nil" else value)

    def executeVarDecl(stmt: VarDecl): Unit = 
        val value = stmt.initializer.map(evaluate).getOrElse(null)
        currentEnv.define(stmt.name, value)

    def executeBlockStmt(stmt: BlockStmt): Unit = 
        val newEnv = new Env(Some(currentEnv))
        executeBlock(stmt.statements, newEnv) 
            
    def executeBlock(statements: Seq[Stmt], env: Env): Unit = 
        val previousEnv = currentEnv
        try
            currentEnv = env
            for stmt <- statements do
                execute(stmt) 
        finally
            currentEnv = previousEnv

    def executeIfStmt(stmt: IfStmt): Unit = 
        if isTruthy(evaluate(stmt.condition)) then 
            execute(stmt.thenBranch)
        else 
            stmt.elseBranch.foreach(execute)

    def executeWhileStmt(stmt: WhileStmt): Unit = 
        while isTruthy(evaluate(stmt.condition)) do
            execute(stmt.body)

    def executeFunctionDeclaration(stmt: FunDecl): Unit = 
        val function = Function(stmt, currentEnv)
        currentEnv.define(stmt.name.lexeme, function)

    def executeReturn(stmt: ReturnStmt): Unit =
        val value = stmt.value.map(evaluate).getOrElse(null)
        throw ReturnException(value)
    

    // EXPRESSIONS EVALUATIONS ------------------------------------------------

    def evaluate(expr: Expr): Any =
        expr match
            case literal: LiteralExpr => evaluateLiteral(literal)
            case grouping: GroupingExpr => evaluateGrouping(grouping)
            case unary: UnaryExpr => evaluateUnary(unary)
            case binary: BinaryExpr => evaluateBinary(binary)
            case variable: VariableExpr => evaluateVariable(variable)
            case assignment: AssignmentExpr => evaluateVariableAssignment(assignment)
            case logic: LogicExpr => evaluateLogic(logic)
            case call: CallExpr => evaluateCall(call)
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
                !isTruthy(right)
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

    def evaluateVariable(variable: VariableExpr): Any =
        Option(localScopeDepths.get(variable)) match
            case Some(depth) => 
                currentEnv.get(variable.name.lexeme, Some(depth))
            case None => globalEnv.get(variable.name.lexeme)
                
    def evaluateVariableAssignment(assignment: AssignmentExpr): Any =
        val value = evaluate(assignment.value)
        
        Option(localScopeDepths.get(assignment)) match
            case Some(depth) => 
                currentEnv.assign(assignment.name.lexeme, value, Some(depth))
            case None => 
                globalEnv.assign(assignment.name.lexeme, value)
        


    def evaluateLogic(logic: LogicExpr): Any = 
        val left = evaluate(logic.left)
        val shortCircuit = logic.operator.tokenType match
            case TokenType.OR => isTruthy(left)
            case TokenType.AND => !isTruthy(left)
            case _ => throw new RuntimeException(s"Unknown logical operator: ${logic.operator.lexeme}")

        if shortCircuit then left else evaluate(logic.right)

    def evaluateCall(call: CallExpr): Any = 
        val callee = evaluate(call.callee) 
        val arguments = call.arguments.map(evaluate)

        callee match
            case function: Callable =>
                if arguments.length != function.arity then
                    throw new RuntimeException(s"Expected ${function.arity} arguments but got ${arguments.length}.")
                function.apply(this, arguments)
            case _ => throw new RuntimeException("Can only call functions.")

    // HELPER METHODS ---------------------------------------------------------------       
    private def isTruthy(value: Any): Boolean =
        value match
            case null => false
            case b: Boolean => b
            case _ => true
