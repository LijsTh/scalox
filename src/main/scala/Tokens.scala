enum TokenType:
    /* 
    EXTRA TOKENS BY JUMAS AND THEO:
        minus_minus -> loop control structure, e.g. for (var i = 0; i < 10; i--)
        class -> class declaration
        super -> access superclass methods and properties
        this -> access current class methods and properties
        percent -> modulus operator
    */

    // Single-character tokens.
    case LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
         COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR, 
         PERCENT, QUESTION, COLON

    // One or two character tokens.
    case PLUS_PLUS, MINUS_MINUS, STAR_STAR
    case BANG, BANG_EQUAL
    case EQUAL, EQUAL_EQUAL
    case GREATER, GREATER_EQUAL 
    case LESS, LESS_EQUAL

    // Literals.
    case IDENTIFIER, STRING, NUMBER

    // Keywords.
    case AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
         PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE

    // End of file
    case EOF

object TokenType:
    val keywords: Map[String, TokenType] = Map(
        "and" -> TokenType.AND,
        "class" -> TokenType.CLASS,
        "else" -> TokenType.ELSE,
        "false" -> TokenType.FALSE,
        "for" -> TokenType.FOR,
        "fun" -> TokenType.FUN,
        "if" -> TokenType.IF,
        "nil" -> TokenType.NIL,
        "or" -> TokenType.OR,
        "print" -> TokenType.PRINT,
        "return" -> TokenType.RETURN,
        "super" -> TokenType.SUPER,
        "this" -> TokenType.THIS,
        "true" -> TokenType.TRUE,
        "var" -> TokenType.VAR,
        "while" -> TokenType.WHILE
    )

    val literalTypes: Set[TokenType] = Set(
        IDENTIFIER, STRING, NUMBER
    )


type TokenLiteralType = String | Double | Boolean

case class Token(
  tokenType: TokenType,
  lexeme: String,
  literal: Option[TokenLiteralType],
  line: Int
):
    override def toString: String =
        (tokenType, literal) match
            case (TokenType.IDENTIFIER, _) =>
                s"$tokenType -> $lexeme"

            case (_, Some(value)) =>
                s"$tokenType -> $value"

            case _ =>
                s"$tokenType"