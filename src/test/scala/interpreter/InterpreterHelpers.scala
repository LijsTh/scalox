import java.io.{ByteArrayOutputStream, PrintStream}
import scala.util.Using
import scala.util.chaining.scalaUtilChainingOps

trait InterpreterHelpers:

  def eval(source: String): Any =
    val interp = Interpreter()
    Parser(Scanner(source).scan())
      .expression()
      .pipe(interp.evaluate)

  def runProgram(source: String): Unit =
    val interp = Interpreter()
    val resolver = Resolver(interp)
    val stmts = Parser(Scanner(source).scan()).parse()
    stmts.foreach(resolver.resolve)
    interp.interpret(stmts)

  def captureOutput(source: String): String =
    Using.Manager: use =>
      val out = use(ByteArrayOutputStream())
      Console.withOut(use(PrintStream(out))):
        runProgram(source)
      out.toString("UTF-8")
    .get

  def runAndTrim(source: String): String =
    captureOutput(source).trim

  def runAndLines(source: String): List[String] =
    captureOutput(source).trim.linesIterator.toList
