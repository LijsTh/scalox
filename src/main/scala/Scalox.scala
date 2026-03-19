import scala.io.Source
import scala.io.StdIn
import scala.util.control.NonFatal
import sun.misc.{Signal, SignalHandler}

enum Mode:
    case Scanning, Parsing, Resolve

case class CliConfig(
    debug: Boolean = false,
    showWarnings: Boolean = false,
    mode: Option[Mode] = None,
    lineByLine: Boolean = false,
    file: Option[String] = None
)

class Scalox(config: CliConfig):

    private var inRepl: Boolean = true // Read-eval-print loop mode
    @volatile private var shuttingDown: Boolean = false
    private var signalHandlersInstalled: Boolean = false

    def run(): Unit =
        installSignalHandlers()
        config.file match
            case Some(path) => runFile(path, config.lineByLine)
            case None => runRepl()

    private def runSource(source: String): Unit =
        val scanner = new Scanner(source)

        val tokens =
            try scanner.scan()
            catch
                case NonFatal(e) =>
                    if config.debug then e.printStackTrace()
                    println(s"Scanning Error: ${e.getMessage}")
                    return

        config.mode match
            case Some(Mode.Scanning) =>
                tokens.foreach(token => println(token))
                return
            case Some(Mode.Parsing) =>
                println("Parsing mode is not implemented yet in Scala (missing Parser).")
                return
            case Some(Mode.Resolve) =>
                println("Resolve mode is not implemented yet in Scala (missing Resolver/Interpreter).")
                return
            case None => ()

        if config.showWarnings then
            println("Warning reporting is not implemented yet in Scala (missing Resolver).")

        // Placeholder: once Parser + Interpreter are available, execute statements here.
        if inRepl then println("Scanning completed.")

    private def runFile(path: String, lineByLine: Boolean): Unit =
        inRepl = false
        try
            val source = Source.fromFile(path)
            try
                if lineByLine then
                    source.getLines().foreach { line =>
                        println(s"> ${line.trim}")
                        runSource(line)
                    }
                else
                    runSource(source.mkString)
            finally source.close()
        catch
            case NonFatal(e) =>
                if config.debug then e.printStackTrace()
                println(s"File Error: ${e.getMessage}")
        finally
            inRepl = true

    private def runRepl(): Unit =
        var keepRunning = true
        while keepRunning && !shuttingDown do
            val line = StdIn.readLine("> ")
            if line == null then keepRunning = false
            else
                try runSource(line)
                catch
                    case NonFatal(e) =>
                        if config.debug then e.printStackTrace()
                        println(s"Runtime Error: ${e.getMessage}")

    private def installSignalHandlers(): Unit =
        if signalHandlersInstalled then return
        signalHandlersInstalled = true

        val handler = new SignalHandler:
            override def handle(signal: Signal): Unit =
                if !shuttingDown then
                    shuttingDown = true
                    Console.err.println(s"\nReceived SIG${signal.getName}. Shutting down gracefully...")
                System.exit(0)

        try Signal.handle(Signal("INT"), handler)
        catch case NonFatal(_) => ()

        try Signal.handle(Signal("TERM"), handler)
        catch case NonFatal(_) => ()

        Runtime.getRuntime.addShutdownHook(
            Thread(() =>
                if !shuttingDown then
                    shuttingDown = true
                    Console.err.println("\nShutdown complete.")
            )
        )
