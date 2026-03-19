@main def main(args: String*): Unit =
    parseArgs(args.toArray) match
        case Left(error) =>
            println(s"Argument Error: $error")
            printUsage()
        case Right(config) =>
            Scalox(config).run()

def parseArgs(args: Array[String]): Either[String, CliConfig] =
    var config = CliConfig()
    var index = 0

    while index < args.length do
        args(index) match
            case "--debug" =>
                config = config.copy(debug = true)
            case "--show-warnings" =>
                config = config.copy(showWarnings = true)
            case "--line-by-line" =>
                config = config.copy(lineByLine = true)
            case "--scanning" =>
                if config.mode.nonEmpty then
                    return Left("--scanning, --parsing and --resolve are mutually exclusive")
                config = config.copy(mode = Some(Mode.Scanning))
            case "--parsing" =>
                if config.mode.nonEmpty then
                    return Left("--scanning, --parsing and --resolve are mutually exclusive")
                config = config.copy(mode = Some(Mode.Parsing))
            case "--resolve" =>
                if config.mode.nonEmpty then
                    return Left("--scanning, --parsing and --resolve are mutually exclusive")
                config = config.copy(mode = Some(Mode.Resolve))
            case arg if arg.startsWith("-") =>
                return Left(s"Unknown option: $arg")
            case filePath =>
                if config.file.nonEmpty then
                    return Left("Only one input file is supported")
                config = config.copy(file = Some(filePath))
        index += 1

    Right(config)

def printUsage(): Unit =
    println("""Usage: scalox [options] [file]
      |Options:
      |  --debug          Print stack traces for errors
      |  --scanning       Run in scanning mode
      |  --parsing        Run in parsing mode (pending implementation)
      |  --resolve        Run in resolve mode (pending implementation)
      |  --line-by-line   Execute file line by line
      |  --show-warnings  Report compile-time warnings (pending implementation)
      |""".stripMargin)
