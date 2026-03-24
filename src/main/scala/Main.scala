import utils.Logger

@main def main(args: String*): Unit =
    val (flags, files) = args.partition(_.startsWith("--"))

    val mode = flags.collectFirst {
        case "--scanning" => Mode.Scanning
        case "--parsing"  => Mode.Parsing
        case "--resolve"  => Mode.Resolve
    }

    val scalox = Scalox(mode)

    files.headOption match
        case Some(path) => scalox.runFile(path)
        case None       => scalox.runRepl()

