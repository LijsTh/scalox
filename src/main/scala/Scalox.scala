import scala.io.{Source, StdIn}
import scala.util.{Try, Using}
import utils.Logger

enum Mode:
  case Scanning, Parsing, Resolve

class Scalox(val mode: Option[Mode] = None, val lineByLine: Boolean = false):
  private val interpreter: Interpreter = Interpreter()
  private val resolver: Resolver = Resolver(interpreter)

  def runRepl(): Unit =
    Logger.success("Scalox REPL - Type 'exit' or press Ctrl+D to quit")

    sys.addShutdownHook:
      Logger.success("Goodbye!")
    
    Try:
      Iterator
        .continually(StdIn.readLine(Logger.prompt))
        .takeWhile(_ != null)
        .takeWhile(!isExitCommand(_))
        .foreach(runPipeline)
    .recover:
      case _: InterruptedException => ()

  private def readFile(path: String): Either[String, String] =
    Using(Source.fromFile(path)): source =>
      source.getLines().mkString("\n")
    .toEither.left.map(_.getMessage)

  private def readFileLines(path: String): Either[String, List[String]] =
    Using(Source.fromFile(path)): source =>
      source.getLines().map(_.trim).filter(_.nonEmpty).toList
    .toEither.left.map(_.getMessage)

  def runFile(path: String): Unit =
    if lineByLine then
      readFileLines(path).fold(
        err => Logger.error(s"File Error: $err"),
        lines => lines.foreach: line =>
          println(s"${Logger.prompt}$line")
          runPipeline(line)
      )
    else
      readFile(path).fold(
        err => Logger.error(s"File Error: $err"),
        content => runPipeline(content)
      )

  private def runPipeline(source: String): Either[String, Unit] =
    for
      tokens     <- scanTokens(source)
      _          <- stopIf(Mode.Scanning):
                      tokens.foreach(Logger.output)
      statements <- parseStatements(tokens)
      _          <- stopIf(Mode.Parsing):
                      statements.foreach(stmt => Logger.output(stmt.toString))
      _          <- resolveAll(statements)
      _          <- stopIf(Mode.Resolve):
                      displayDepths()
      _          <- interpretStatements(statements)
    yield ()

  private def scanTokens(source: String): Either[String, Seq[Token]] =
    tryStep("Scanning")(Scanner(source).scan())

  private def parseStatements(tokens: Seq[Token]): Either[String, Seq[Stmt]] =
    tryStep("Parsing")(Parser(tokens).parse())

  private def resolveAll(statements: Seq[Stmt]): Either[String, Unit] =
    statements.foldLeft(Right(()): Either[String, Unit]): (acc, stmt) =>
      acc.flatMap(_ => tryStep("Resolve")(resolver.resolve(stmt)))

  private def interpretStatements(statements: Seq[Stmt]): Either[String, Unit] =
    tryStep("Runtime")(interpreter.interpret(statements))

  private def displayDepths(): Unit =
    import scala.jdk.CollectionConverters.*
    val javaMap = interpreter.getLocalScopeDepths
    val it = javaMap.entrySet().iterator()
    val parts = scala.collection.mutable.ArrayBuffer[String]()
    while it.hasNext do
      val e = it.next()
      val token = e.getKey match
        case Expr.VariableExpr(name)      => name
        case Expr.AssignmentExpr(name, _) => name
      parts += s"${token.tokenType}<${token.lexeme}>: ${e.getValue}"
    val formatted = parts.mkString("{", ", ", "}")
    Logger.output(s"Interpreter Locals: $formatted")
  // HELPERS ------------------------------------------------

  private def isExitCommand(line: String): Boolean =
    line.trim.equalsIgnoreCase("exit") 

  private def stopIf(target: Mode)(action: => Unit): Either[String, Unit] =
    if mode.contains(target) then 
      action
      Left("")  // Intentional stop (not an error)
    else 
      Right(()) // Continue pipeline
 
  private def tryStep[A](stepName: String)(block: => A): Either[String, A] =
    Try(block).toEither.left.map: e =>
      val error = e.getMessage
      Logger.error(s"$stepName Error: $error")
      error

    