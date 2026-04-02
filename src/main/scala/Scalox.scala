import scala.io.Source
import scala.io.StdIn
import scala.util.control.NonFatal
import utils.{Logger}

enum Mode:
    case Scanning, Parsing, Resolve

class Scalox(val mode: Option[Mode] = None):

    def run(source: String): Unit =
        val tokens =
            try Scanner(source).scan()
            catch
                case NonFatal(e) =>
                    Logger.error(s"Scanning Error: ${e.getMessage}")
                    return

        if mode == Some(Mode.Scanning) then
            tokens.foreach(Logger.output)
            return

        val parsed = 
            try Parser(tokens).parse()
            catch
                case NonFatal(e) =>
                    Logger.error(s"Parsing Error: ${e.getMessage}")
                    return

        if mode == Some(Mode.Parsing) then
            Logger.output(parsed.toString)
            return
        
        val interpreter = Interpreter()
        interpreter.interpret(parsed)



    def runFile(path: String): Unit =
        try
            Source.fromFile(path).getLines().foreach { line =>
                println(s"${Logger.prompt}${line.trim}")
                run(line.trim)
            }
        catch
            case NonFatal(e) => Logger.error(s"File Error: ${e.getMessage}")

    def runRepl(): Unit =
        Logger.success("Scalox REPL - Type 'exit' or press Ctrl+D to quit")
        
        try
            Iterator
                .continually(StdIn.readLine(Logger.prompt))
                .takeWhile(_ != null)
                .takeWhile(!isExitCommand(_))
                .foreach(run)
        catch
            case _: InterruptedException => 
                println()
        
        Logger.success("Goodbye!")

    private def isExitCommand(line: String): Boolean =
        val trimmed = line.trim.toLowerCase
        trimmed == "exit"