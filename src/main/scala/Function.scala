import Stmt.FunDecl 
import Env.*

case class ReturnException(value: Any) 
    extends RuntimeException(s"Return Value: $value"):
    override def fillInStackTrace(): Throwable = this // Avoid filling the stack trace for control flow exceptions

trait Callable:
    def arity: Int
    def apply(interpreter: Interpreter, arguments: List[Any]): Any

class Function(declaration: FunDecl, closure: Env) extends Callable:
    
    override val arity: Int = declaration.params.length

    def apply(interpreter: Interpreter, arguments: List[Any]): Any = 
        val env = new Env(Some(closure))
        for (param, arg) <- declaration.params.zip(arguments) do
            env.define(param.lexeme, arg)
        try
            interpreter.executeBlock(declaration.body, env)
            null // If the function completes without a return statement, return null
        catch
            case ReturnException(value) => value
        
    override def toString(): String =
        val params = declaration.params.map(_.lexeme).mkString(", ")
        s"<fn ${declaration.name.lexeme}($params)>"
