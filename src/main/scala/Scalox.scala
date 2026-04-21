import scala.io.{Source, StdIn}
import scala.util.{Try, Using}


import utils.Logger
import scala.collection.mutable.ArrayBuffer

enum Mode:
  case Scanning, Parsing, Resolve

class Scalox(val mode: Option[Mode] = None):

  def run(source: String): Unit =
    runPipeline(source) match
      case Left(error) => () // Error already logged
      case Right(_)    => () // Success

  def runFile(path: String): Unit =
    readFile(path) match
      case Right(content) => run(content)
      case Left(error)    => Logger.error(s"File Error: $error")

  def runRepl(): Unit =
    Logger.success("Scalox REPL - Type 'exit' or press Ctrl+D to quit")
    
    Try:
      Iterator
        .continually(StdIn.readLine(Logger.prompt))
        .takeWhile(_ != null)
        .takeWhile(!isExitCommand(_))
        .foreach(run)
    .recover:
      case _: InterruptedException => println()
    
    Logger.success("Goodbye!")

  // Core pipeline logic
  private def runPipeline(source: String): Either[String, Unit] =
    for
      tokens     <- scanTokens(source)
      _          <- checkAndStopAt(Mode.Scanning, tokens.foreach(Logger.output))
      statements <- parseStatements(tokens)
      _          <- checkAndStopAt(Mode.Parsing, Logger.output(statements.toString))
      _          <- resolveStatements(statements)
    yield ()

  // Pipeline steps
  private def scanTokens(source: String): Either[String, ArrayBuffer[Token]] =
    tryStep("Scanning")(Scanner(source).scan())

  private def parseStatements(tokens: ArrayBuffer[Token]): Either[String, ArrayBuffer[Stmt]] =
    tryStep("Parsing")(Parser(tokens).parse())

  private def resolveStatements(statements: ArrayBuffer[Stmt]): Either[String, Unit] =
    val interpreter = Interpreter()
    val resolver = Resolver(interpreter)
    
    for
      _ <- resolveAll(resolver, statements)
      _ <- checkAndStopAt(Mode.Resolve, displayDepths(resolver))
      _ <- interpretStatements(interpreter, statements)
    yield ()

  private def resolveAll(
    resolver: Resolver,
    statements: ArrayBuffer[Stmt]
  ): Either[String, Unit] =
    statements.foldLeft(Right(()): Either[String, Unit]): (acc, stmt) =>
      acc.flatMap(_ => tryStep("Resolve")(resolver.resolve(stmt)))

  private def interpretStatements(
    interpreter: Interpreter,
    statements: ArrayBuffer[Stmt]
  ): Either[String, Unit] =
    tryStep("Runtime")(interpreter.interpret(statements))

  private def displayDepths(resolver: Resolver): Unit =
    val depths = 
      for
        (scope, depth) <- resolver.scopes.zipWithIndex
        varName <- scope.keys
      yield varName -> depth
    
    Logger.output(s"Variable Depths: ${depths.toMap}")

  // Utilities
  private def checkAndStopAt(
    targetMode: Mode,
    action: => Unit
  ): Either[String, Unit] =
    if mode.contains(targetMode) then
      action
      Left("") // Stop pipeline (empty error = no logging)
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
