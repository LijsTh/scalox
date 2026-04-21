import scala.collection.mutable
import scala.annotation.tailrec

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

    def ancestor(distance: Int): Env = 
        var currentEnv = this
        for _ <- 1 to distance do 
            currentEnv = currentEnv.enclosing.getOrElse(
                throw RuntimeException(s"No enclosing environment at distance $distance")
            )
        currentEnv

    def define(name: String, value: Any): Unit =
        // Lox permite redeclarar: var x=1; var x=2; print x; // 2
        values(name) = value

    def get(name: String, distance: Option[Int]= None): Any =
        var scope = this

        if distance.isDefined then
            scope = ancestor(distance.get)

        scope.values.getOrElse(
            name,
            throw RuntimeException(s"Undefined variable '$name'")
        )

    def assign(name: String, value: Any, distance: Option[Int] = None): Any =
        val scope = scopeAt(distance)
        
        scope.values.get(name) match
        case Some(_) => 
            scope.values(name) = value  
            value
        case None => 
            throw RuntimeException(s"Cannot assign to undefined variable '$name'")


    private def scopeAt(distance: Option[Int]): Env =
        distance.fold(this)(ancestor)

    private def collectScopes(): List[Env] = 
        @tailrec
        def loop(env: Env, acc: List[Env]): List[Env] =
            env.enclosing match
                case Some(parent) => loop(parent, env :: acc)
                case None => env :: acc
        loop(this, Nil).reverse
}