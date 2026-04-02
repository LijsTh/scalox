import scala.collection.mutable

class Env(val enclosing: Option[Env] = None) {
    private val values: mutable.Map[String, Any] = mutable.Map.empty

    override def toString: String =
        val allValues = mutable.ArrayBuffer[String]()
        var current = enclosing
        while current.nonEmpty do 
            val env = current.get
            allValues += env.values.toString
            current = env.enclosing
        allValues.mkString(" << ")

    def define(name: String, value: Any): Unit =
        // Lox permite redeclarar: var x=1; var x=2; print x; // 2
        values(name) = value

    def get(name: String): Any =
        values.get(name) match
            case Some(value) => value
            case None =>
                enclosing match
                    case Some(env) => env.get(name)
                    case None => throw new RuntimeException(s"Undefined variable '$name'.")

    def assign(name: String, value: Any): Any =
        if values.contains(name) then
            values(name) = value
            value
        else
            enclosing match
                case Some(env) => env.assign(name, value)
                case None => throw new RuntimeException(s"Undefined variable '$name'.")
}