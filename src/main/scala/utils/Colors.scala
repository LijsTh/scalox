package utils

object Color:
    private val useColor = System.console() != null && sys.env.getOrElse("NO_COLOR", "").isEmpty

    private def wrap(code: String, text: Any): String =
        if useColor then s"\u001b[${code}m${text}\u001b[0m" else text.toString

    def red(text: Any): String     = wrap("91", text)
    def green(text: Any): String   = wrap("92", text)
    def yellow(text: Any): String  = wrap("93", text)
    def blue(text: Any): String    = wrap("94", text)
    def bold(text: Any): String    = wrap("1",  text)

object Logger:
    def error(msg: Any): Unit   = println(Color.red(msg))
    def output(msg: Any): Unit  = println(Color.blue(msg))
    def warn(msg: Any): Unit    = println(Color.yellow(msg))
    def success(msg: Any): Unit = println(Color.green(msg))
    def prompt: String          = s"${Color.bold(">")} "
