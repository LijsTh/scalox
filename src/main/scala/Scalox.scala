import scala.io.{Source, StdIn}
import scala.util.{Try, Using}


import utils.Logger
import scala.collection.mutable.ArrayBuffer

enum Mode:
  case Scanning, Parsing, Resolve

class Scalox(val mode: Option[Mode] = None):
  private val interpreter: Interpreter = Interpreter()
  private val resolver: Resolver = Resolver(interpreter)

  def runFile(path: String): Unit =
    readFile(path) match
      case Right(content) => runPipeline(content)
      case Left(error)    => Logger.error(s"File Error: $error")

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
      case _: InterruptedException => 
    
  private def runPipeline(source: String): Either[String, Unit] =
    for
      tokens     <- scanTokens(source)
      _          <- checkAndStopAt(Mode.Scanning, tokens.foreach(Logger.output))
      statements <- parseStatements(tokens)
      _          <- checkAndStopAt(Mode.Parsing, statements.foreach(stmt => Logger.output(stmt.toString)))
      _          <- resolveStatements(statements)
    yield ()

  private def scanTokens(source: String): Either[String, Seq[Token]] =
    tryStep("Scanning")(Scanner(source).scan())

  private def parseStatements(tokens: Seq[Token]): Either[String, Seq[Stmt]] =
    tryStep("Parsing")(Parser(tokens).parse())

  private def resolveStatements(statements: Seq[Stmt]): Either[String, Unit] =
    for
      _ <- resolveAll(statements)
      _ <- checkAndStopAt(Mode.Resolve, displayDepths())
      _ <- interpretStatements(statements)
    yield ()

  private def resolveAll(statements: Seq[Stmt]): Either[String, Unit] =
    statements.foldLeft(Right(()): Either[String, Unit]): (acc, stmt) =>
      acc.flatMap(_ => tryStep("Resolve")(resolver.resolve(stmt)))

  private def interpretStatements(statements: Seq[Stmt]): Either[String, Unit] =
    tryStep("Runtime")(interpreter.interpret(statements))

  private def displayDepths(): Unit =
    val depths = 
      for
        (scope, depth) <- resolver.scopes.zipWithIndex
        varName <- scope.keys
      yield varName -> depth
    
    Logger.output(s"Variable Depths: ${depths.toMap}")

  private def checkAndStopAt(
    targetMode: Mode,
    action: => Unit
  ): Either[String, Unit] =
    if mode.contains(targetMode) then
      action
      Left("") 
    else
      Right(())

  private def tryStep[A](stepName: String)(block: => A): Either[String, A] =
    Try(block).toEither.left.map: e =>
      val error = e.getMessage
      Logger.error(s"$stepName Error: $error")
      error

  private def readFile(path: String): Either[String, String] =
    Using(Source.fromFile(path)): source =>
      source.getLines().mkString("\n")
    .toEither.left.map(_.getMessage)

  private def isExitCommand(line: String): Boolean =
    line.trim.toLowerCase == "exit"
