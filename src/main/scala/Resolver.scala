import scala.collection.mutable

case class ResolveError(message: String) extends Exception(message)

class Resolver(interpreter: Interpreter):
  // empty starting scope 
  val scopes = mutable.Stack[mutable.Map[String, Boolean]]()

  def beginScope(): Unit = scopes.push(mutable.Map.empty)

  def endScope(): Unit = scopes.pop()

  def declare(name: String): Unit = 
    // declaring a variable is saving a false value.
    // Example: var x;
    scopes.headOption.foreach: scope =>
      if scope.contains(name) then 
        throw ResolveError(s"Variable '$name' already declared.")
      scope(name) = false

  def define(name: String): Unit =
    // defining a variable is saving a true value, meaning it's ready to be used.
    // Example: var x = 1;
    scopes.headOption.foreach(_(name) = true)

  def resolve(statements: Stmt | Expr): Unit = 
    statements match 
      case stmt: Stmt => resolveStmt(stmt)
      case expr: Expr => resolveExpr(expr)

  def resolveStmt(stmt: Stmt): Unit =
    stmt match 
      case s: Stmt.BlockStmt => resolveBlock(s)
      case s: Stmt.VarDecl => resolveVarDecl(s)
      case s: Stmt.FunDecl => resolveFunctionDeclaration(s)
      case s: Stmt.ExpressionStmt => resolve(s.expr)
      case s: Stmt.PrintStmt => resolve(s.expr)
      case s: Stmt.ReturnStmt => s.value.foreach(resolve)
      case s: Stmt.IfStmt => resolveIfStatement(s)
      case s: Stmt.WhileStmt => resolveWhileStatement(s)

  def resolveExpr(expr: Expr): Unit =
    expr match
      case e: Expr.VariableExpr => resolveVariable(e)
      case e: Expr.AssignmentExpr => resolveAssignment(e)
      case e: Expr.LiteralExpr => ()
      case e: Expr.GroupingExpr => resolve(e.expression)
      case e: Expr.UnaryExpr => resolve(e.right)
      case e: Expr.BinaryExpr => resolve(e.left); resolve(e.right)
      case e: Expr.LogicExpr => resolve(e.left); resolve(e.right)
      case e: Expr.CallExpr => resolveCall(e)

  // STATEMENT RESOLUTIONS ------------------------------------------------
  
  private def resolveBlock(statement: Stmt.BlockStmt): Unit = 
    // blocks have own scopes 
    beginScope()
    statement.statements.foreach(resolve)
    endScope()

  private def resolveVarDecl(statement: Stmt.VarDecl): Unit =
    // Variables are declared in the current scope.
    // Then we try to solve the initializer and define them. 
    // This way we can catch cases like: var x = x;
    declare(statement.name)
    statement.initializer.foreach(resolve)
    define(statement.name)

  private def resolveFunctionDeclaration(statement: Stmt.FunDecl): Unit = 
    // Functions are declared in the current scope, then we resolve their body in a new scope. 
    declare(statement.name.lexeme)
    define(statement.name.lexeme)

    beginScope()
    statement.params.foreach: param =>
      declare(param.lexeme)
      define(param.lexeme)
    statement.body.foreach(resolve)
    endScope()

  private def resolveIfStatement(statement: Stmt.IfStmt): Unit = 
    resolve(statement.condition)
    resolve(statement.thenBranch)
    statement.elseBranch.foreach(resolve)

  private def resolveWhileStatement(statement: Stmt.WhileStmt): Unit = 
    resolve(statement.condition)
    resolve(statement.body)

  // EXPRESSION RESOLUTIONS ------------------------------------------------

  private def resolveVariable(expr: Expr.VariableExpr): Unit = 
    // Look for the variable in the scopes, starting from the innermost one. 
    // Get error in cases such as var x = x;
    scopes.headOption.foreach: scope =>
      if scope.get(expr.name.lexeme).contains(false) then 
        throw ResolveError(s"Variable '${expr.name.lexeme}' was declared but not defined.")

    // We find the depth of the variable and tell the interpreter to resolve it.
    findDepth(expr.name.lexeme).foreach(interpreter.resolveDepth(expr, _))

  private def resolveAssignment(expr: Expr.AssignmentExpr): Unit = 
    resolve(expr.value)

    // We find the depth of the variable and tell the interpreter to resolve it.
    findDepth(expr.name.lexeme).foreach(interpreter.resolveDepth(expr, _))

  private def resolveCall(expr: Expr.CallExpr): Unit = 
    resolve(expr.callee)
    expr.arguments.foreach(resolve)

  // HELPERS ------------------------------------------------

  private def findDepth(name: String): Option[Int] =
    val depth = scopes.indexWhere(_.contains(name))
    if depth != -1 then Some(depth) else None
