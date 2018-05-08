/**
 * YATOL's lexer.
 */
module yatol.lexer;

import
    core.stdc.stdlib;
import
    std.stdio, std.file;
import
    yatol.token, yatol.session;

/**
 * Scans a yatol source file.
 */
struct Lexer
{

private:

    Tokens _tokens;
    char[] _filename;
    char[] _text;
    char* _tokStart;
    char* _front;
    char* _back;
    bool _anticipated;
    size_t _frontLine;
    size_t _frontColumn;
    size_t _tokStartLine;
    size_t _tokStartColumn;
    TokenType _anticipatedType;

    void warning(const(char[]) message)
    {
        session.warn("%s(%d,%d): warning, %s", _filename, _frontLine,
            _frontColumn, message);
    }

    void error(const(char[]) message)
    {
        session.error("%s(%d,%d): warning, %s", _filename, _frontLine,
            _frontColumn, message);
    }

    // Puts a bookmark when starting to lex something.
    void anticipateToken(TokenType type)
    {
        _tokStart = _front;
        _tokStartLine = _frontLine;
        _tokStartColumn = _frontColumn;
        _anticipatedType = type;
        _anticipated = true;
    }

    // Puts  a new token, as anticipated.
    void validateToken()
    {
        assert(_anticipated);
        if (_tokens.length && _tokens[$-1].isTokDollar &&
            _anticipatedType == TokenType.identifier &&
            Keywords.isKeyword(_tokStart[0.._front-_tokStart]))
        {
            --_tokStart;
        }
        else _tokens.length += 1;
        _tokens[$-1] = Token(_tokStart, _front, _tokStartLine, _tokStartColumn,
            _anticipatedType);
        _anticipated = false;
    }

    // Puts  a new token, correct the anticipation.
    pragma(inline, true)
    void validateToken(TokenType type)
    {
        assert(_anticipated);
        _tokens.length += 1;
        _tokens[$-1] = Token(_tokStart, _front, _tokStartLine, _tokStartColumn, type);
        _anticipated = false;
    }

    // next char, synchronizes column.
    pragma(inline, true)
    void advance()
    {
        ++_front;
        ++_frontColumn;
    }

    pragma(inline, true)
    bool canLookup()
    {
        return _front < _back;
    }

    pragma(inline, true)
    bool canLookup2()
    {
        return _front + 1 < _back;
    }

    pragma(inline, true)
    char* lookup()
    {
        return _front + 1;
    }

    // Must be called when a sub-lexer finds a line ending.
    pragma(inline, true)
    void processlineEnding()
    {
        if (*_front == '\r')
            ++_front;
        if (*_front == '\n')
        {
            ++_front;
            ++_frontLine;
            _frontColumn = 1;
        }
    }

    void lexStringLiteral()
    {
        advance();
        anticipateToken(TokenType.stringLiteral);
        while (true)
        {
            if (_front > _back)
            {
                error("unterminated string literal");
                validateToken(TokenType.invalid);
                return;
            }
            switch(*_front)
            {
            case 0:
                error("null character encountered inside a string literal");
                advance();
                validateToken(TokenType.invalid);
                return;
            case '\\':
                if (canLookup() && *lookup() == '"')
                    advance();
                advance();
                continue;
            case '\r', '\n':
                processlineEnding;
                advance();
                continue;
            case '"':
                validateToken();
                advance();
                return;
            default:
                advance();
            }
        }
    }

    void lexRawStringLiteral()
    {
        advance();
        anticipateToken(TokenType.stringLiteral);
        while (true)
        {
            if (_front > _back)
            {
                error("unterminated raw string literal");
                validateToken(TokenType.invalid);
                return;
            }
            switch(*_front)
            {
            case 0:
                error("null character encountered inside a raw string literal");
                advance();
                validateToken(TokenType.invalid);
                return;
            case '`':
                validateToken();
                advance();
                return;
            default:
                advance();
            }
        }
    }

    void lexLineComment()
    {
        anticipateToken(TokenType.lineComment);
        advance();
        advance();
        while (true)
        {
            if (_front > _back)
            {
                validateToken();
                return;
            }
            switch(*_front)
            {
            case 0:
                error("null character encountered inside a comment");
                advance();
                validateToken(TokenType.invalid);
                return;
            case '\r', '\n':
                validateToken();
                processlineEnding;
                return;
            default:
                advance();
            }
        }
    }

    void lexStarComment()
    {
        anticipateToken(TokenType.starComment);
        advance();
        advance();
        while (true)
        {
            if (_front > _back)
            {
                error("unterminated star comment");
                validateToken(TokenType.invalid);
                return;
            }
            switch(*_front)
            {
            case 0:
                error("null character encountered inside a star comment");
                advance();
                validateToken(TokenType.invalid);
                return;
            case '\r', '\n':
                processlineEnding;
                break;
            case '*':
                if (*(_front + 1) == '/')
                {
                    advance();
                    advance();
                    validateToken();
                    return;
                }
                advance();
                break;
            default:
                advance();
            }
        }
    }

    void lexHexLiteral()
    {
        while (true)
        {
            switch(*_front)
            {
            case 0:
                error("null character encountered inside an hex literal");
                advance();
                validateToken(TokenType.invalid);
                return;
            case '0': .. case '9':
            case 'a': .. case 'f':
            case 'A': .. case 'F':
            case '_':
                advance();
                if (_front > _back)
                    goto default;
                else
                    break;
            case 'g': .. case 'z':
            case 'G': .. case 'Z':
                error("invalid hexadecimal digit");
                advance();
                validateToken(TokenType.invalid);
                return;
            default:
                validateToken();
                return;
            }
        }
    }

    void lexIntegerLiteral()
    {
        anticipateToken(TokenType.intLiteral);
        while (true)
        {
            switch(*_front)
            {
            case 0:
                error("null character encountered inside an int literal");
                advance();
                validateToken(TokenType.invalid);
                return;
            case '0': .. case '9':
            case '_':
                advance();
                if (_front > _back)
                    goto default;
                else
                    break;
            case 'a': .. case 'z':
            case 'A': .. case 'Z':
                error("invalid decimal digit");
                advance();
                validateToken(TokenType.invalid);
                return;
            case '.':
                if (canLookup() && '0' <= *lookup() && *lookup() <= '9')
                {
                    advance();
                    lexFloatingLiteralFractionalPart();
                    return;
                }
                else
                {
                    validateToken();
                    return;
                }
            default:
                validateToken();
                return;
            }
        }
    }

    void lexFloatingLiteralFractionalPart()
    {
        assert (_anticipated);
        while (true)
        {
            switch(*_front)
            {
            case 0:
                error("null character encountered inside a float literal");
                advance();
                validateToken(TokenType.invalid);
                return;
            case '0': .. case '9':
            case '_':
                advance();
                if (_front > _back)
                    goto default;
                else
                    break;
            default:
                validateToken(TokenType.floatLiteral);
                return;
            }
        }
    }

    void lexIdentifier()
    {
        anticipateToken(TokenType.identifier);
        while (true)
        {
            switch(*_front)
            {
            case 0:
                error("null character encountered inside an identifier");
                advance();
                validateToken(TokenType.invalid);
                return;
            case 'a': .. case 'z':
            case 'A': .. case 'Z':
            case '0': .. case '9':
            case '_':
                advance();
                if (_front > _back)
                    goto default;
                else
                    break;
            default:
                validateToken();
                return;
            }
        }
    }

    bool checkBOM()
    {
        if (!_front || _front > _back)
            return true;

        const char c0 = *_front;
        if (c0 < 0x80)
        {
            return true;
        }
        else if (c0 != 0xEF || !canLookup2())
        {
            error("invalid BOM");
            return false;
        }
        else
        {
            advance();
            const char c1 = *_front;
            advance();
            const char c2 = *_front;
            advance();
            if (c1 != 0xBB || c2 != 0xBF)
            {
                error("invalid BOM");
                return false;
            }
            else
            {
                _frontColumn = 1;
                return true;
            }
        }
    }

    void skipSheBang()
    {
        if (canLookup && _front && _front + 1 <= _back && *_front == '#' && *lookup() == '!')
        {
            while (true)
            {
                if (_front > _back)
                    break;
                switch(*_front)
                {
                case '\r', '\n':
                    processlineEnding;
                    return;
                default:
                    advance();
                }
            }
        }
    }

public:

    /**
     * Creates a new lexer.
     *
     * Params:
     *      filename = The name of the file to lex.
     */
    this(const(char)[] filename)
    {
        _filename= filename.dup;
        _text    = cast(char[]) read(filename);
        _front   = _text.ptr;
        _back    = _text.ptr + _text.length - 1;
        _frontLine   = 1;
        _frontColumn = 1;
    }

    /**
     * Resets the lexer and parse a string.
     *
     * Params:
     *      text = The string to parse.
     *      filename = For the errors, the name of the file.
     *      line = For the errors, the line where the text starts.
     *      column = For the errors, the column where the text starts.
     */
    void setSourceFromText(const(char)[] text, const(char)[] filename = "",
        size_t line = 1, size_t column = 1)
    {
        _filename= filename.dup;
        _text    = text.dup;
        _front   = _text.ptr;
        _back    = _text.ptr + _text.length - 1;
        _frontLine   = line;
        _frontColumn = column;
        _tokens.length = 0;
    }

    /**
     * Splits either the content of the file passed in the constructor or
     * the string passed in $(D setSourceFromText) into tokens.
     */
    void lex()
    {
        if (!checkBOM())
        {
            _front = _back + 1;
        }
        skipSheBang();

        if (_front) L0: while (_front <= _back)
        {
            switch(*_front)
            {
            case 0:
                warning("null character encountered between two tokens");
                advance();
                continue;
            case ' ', '\t', '\v', '\f':
                advance();
                continue;
            case '\r', '\n':
                processlineEnding;
                continue;
            case '/':
                if (canLookup() && *lookup() == '/')
                    lexLineComment();
                else if (canLookup && *lookup() == '*')
                    lexStarComment();
                else if (canLookup && *lookup() == '=')
                {
                    anticipateToken(TokenType.divEqual);
                    advance();
                    advance();
                    validateToken();
                }
                else
                {
                    anticipateToken(TokenType.div);
                    advance();
                    validateToken();
                }
                continue;
            case 'a': .. case 'z':
            case 'A': .. case 'Z':
            case '_':
                lexIdentifier();
                continue;
            case '0':
                if (canLookup() && ('x' == *lookup() || *lookup() == 'X'))
                {
                    anticipateToken(TokenType.hexLiteral);
                    advance();
                    advance();
                    lexHexLiteral();
                }
                else
                {
                    lexIntegerLiteral();
                }
                continue;
            case '1': .. case '9':
                lexIntegerLiteral();
                continue;
            case '-':
                if (canLookup())
                {
                    if (*lookup() == '-')
                    {
                        anticipateToken(TokenType.minusMinus);
                        advance();
                        advance();
                        validateToken();
                    }
                    else if (*lookup() == '=')
                    {
                        anticipateToken(TokenType.minusEqual);
                        advance();
                        advance();
                        validateToken();
                    }
                    else
                    {
                        anticipateToken(TokenType.minus);
                        advance();
                        validateToken();
                    }
                }
                else
                {
                    anticipateToken(TokenType.minus);
                    advance();
                    validateToken();
                }
                continue;
            case '"':
                lexStringLiteral();
                continue;
            case '`':
                lexRawStringLiteral();
                continue;
            case '&':
                if (canLookup() && *lookup() == '&')
                {
                    anticipateToken(TokenType.andAnd);
                    advance();
                }
                else if (canLookup() && *lookup() == '=')
                {
                    anticipateToken(TokenType.ampEqual);
                    advance();
                }
                else
                {
                    anticipateToken(TokenType.amp);
                }
                advance();
                validateToken();
                continue;
            case '|':
                if (canLookup() && *lookup() == '|')
                {
                    anticipateToken(TokenType.orOr);
                    advance();
                }
                else if (canLookup() && *lookup() == '=')
                {
                    anticipateToken(TokenType.pipeEqual);
                    advance();
                }
                else
                {
                    anticipateToken(TokenType.pipe);
                }
                advance();
                validateToken();
                continue;
            case '+':
                if (canLookup() && *lookup() == '+')
                {
                    anticipateToken(TokenType.plusPlus);
                    advance();
                }
                else if (canLookup() && *lookup() == '=')
                {
                    anticipateToken(TokenType.plusEqual);
                    advance();
                }
                else
                {
                    anticipateToken(TokenType.plus);
                }
                advance();
                validateToken();
                continue;
            case '*':
                if (canLookup() && *lookup() == '=')
                {
                    anticipateToken(TokenType.mulEqual);
                    advance();
                }
                else
                {
                    anticipateToken(TokenType.mul);
                }
                advance();
                validateToken();
                continue;
            case '%':
                if (canLookup() && *lookup() == '=')
                {
                    anticipateToken(TokenType.modEqual);
                    advance();
                }
                else
                {
                    anticipateToken(TokenType.mod);
                }
                advance();
                validateToken();
                continue;
            case '^':
                if (canLookup() && *lookup() == '=')
                {
                    anticipateToken(TokenType.xorEqual);
                    advance();
                }
                else
                {
                    anticipateToken(TokenType.xor);
                }
                advance();
                validateToken();
                continue;
            case '>':
                if (canLookup2() && (lookup())[0..2] == ">=")
                {
                    anticipateToken(TokenType.rshiftEqual);
                    advance();
                    advance();
                }
                else if (canLookup() && *lookup() == '=')
                {
                    anticipateToken(TokenType.greaterEqual);
                    advance();
                }
                else if (canLookup() && *lookup() == '>')
                {
                    anticipateToken(TokenType.rShift);
                    advance();
                }
                else
                {
                    anticipateToken(TokenType.greater);
                }
                advance();
                validateToken();
                continue;
            case '<':
                if (canLookup2() && (lookup())[0..2] == "<=")
                {
                    anticipateToken(TokenType.lshiftEqual);
                    advance();
                    advance();
                }
                else if (canLookup() && *lookup() == '=')
                {
                    anticipateToken(TokenType.lesserEqual);
                    advance();
                }
                else if (canLookup() && *lookup() == '<')
                {
                    anticipateToken(TokenType.lShift);
                    advance();
                }
                else
                {
                    anticipateToken(TokenType.lesser);
                }
                advance();
                validateToken();
                continue;
            case '.':
                anticipateToken(TokenType.dot);
                if (canLookup() && *lookup() == '.')
                {
                    advance();
                    advance();
                    if (*_front == '.')
                    {
                        advance();
                        validateToken(TokenType.ellipsis);
                    }
                    else validateToken(TokenType.dotDot);
                }
                else
                {
                    advance();
                    validateToken();
                }
                continue;
            case '@':
                anticipateToken(TokenType.at);
                advance();
                validateToken();
                continue;
            case ';':
                anticipateToken(TokenType.semiColon);
                advance();
                validateToken();
                continue;
            case '(':
                anticipateToken(TokenType.leftParen);
                advance();
                validateToken();
                continue;
            case ')':
                anticipateToken(TokenType.rightParen);
                advance();
                validateToken();
                continue;
            case ',':
                anticipateToken(TokenType.comma);
                advance();
                validateToken();
                continue;
            case ':':
                anticipateToken(TokenType.colon);
                advance();
                validateToken();
                continue;
            case '{':
                anticipateToken(TokenType.leftCurly);
                advance();
                validateToken();
                continue;
            case '}':
                anticipateToken(TokenType.rightCurly);
                advance();
                validateToken();
                continue;
            case '[':
                anticipateToken(TokenType.leftSquare);
                advance();
                validateToken();
                continue;
            case ']':
                anticipateToken(TokenType.rightSquare);
                advance();
                validateToken();
                continue;
            case '~':
                anticipateToken(TokenType.tidle);
                advance();
                validateToken();
                continue;
            case '!':
                if (canLookup() && *lookup() == '=')
                {
                    anticipateToken(TokenType.notEqual);
                    advance();
                }
                else
                {
                    anticipateToken(TokenType.bang);
                }
                advance();
                validateToken();
                continue;
            case '?':
                if (canLookup() && *lookup() == '.')
                {
                    anticipateToken(TokenType.optAccess);
                    advance();
                }
                else
                {
                    anticipateToken(TokenType.qmark);
                }
                advance();
                validateToken();
                continue;
            case '$':
                anticipateToken(TokenType.dollar);
                advance();
                validateToken();
                continue;
            case '=':
                if (canLookup() && *lookup() == '=')
                {
                    anticipateToken(TokenType.equalEqual);
                    advance();
                }
                else
                {
                    anticipateToken(TokenType.equal);
                }
                advance();
                validateToken();
                continue;
            default:
                anticipateToken(TokenType.invalid);
                error("invalid input character");
                advance();
                validateToken();
                break L0;
            }
        }
        anticipateToken(TokenType.eof);
        validateToken();
    }

    /// Returns: The tokens produced after lexing.
    Tokens tokens() {return _tokens;}

    /// Returns: The name of the life that's been lexed.
    const(char)[] filename() {return _filename;}

    version(all)
    {
        void printTokens()
        {
            foreach(ref tk; _tokens)
                writeln(tk);
        }
    }
}

///
unittest
{
    int line = __LINE__ + 2;
    enum source =
    q{  unit a.b;
        // comment 1
        virtual unit c;
        // comment 2
        virtual unit d;
        // comment 3
        // comment 4
        ;;;.....};

    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 7);
    lx.lex();

    assert(lx.tokens.length == 23);
    assert(lx.tokens[0].text == "unit");
    assert(lx.tokens[0].isTokKeyword);
    assert(lx.tokens[0].isTokUnit, lx.tokens[0].text);
    assert(lx.tokens[1].text == "a");
    assert(lx.tokens[2].text == ".");
    assert(lx.tokens[3].text == "b");
    assert(lx.tokens[4].text == ";");
    assert(lx.tokens[5].text == "// comment 1");
    assert(lx.tokens[6].text == "virtual");
    assert(lx.tokens[7].text == "unit");
    assert(lx.tokens[8].text == "c");
    assert(lx.tokens[9].text == ";");
    assert(lx.tokens[10].text == "// comment 2");
    assert(lx.tokens[11].text == "virtual");
    assert(lx.tokens[12].text == "unit");
    assert(lx.tokens[13].text == "d");
    assert(lx.tokens[14].text == ";");
    assert(lx.tokens[15].text == "// comment 3");
    assert(lx.tokens[16].text == "// comment 4");
    assert(lx.tokens[17].text == ";");
    assert(lx.tokens[18].text == ";");
    assert(lx.tokens[19].text == ";");
    assert(lx.tokens[20].text == "...");
    assert(lx.tokens[21].text == "..");
}
/// Tests the symbols and single char operators.
unittest
{
    int line = __LINE__ + 1;
    enum source = `.:;,()/[]{}*+-!@=><&$^%?~`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == source.length + 1);
    foreach(i, tk; lx.tokens)
        if (i != lx.tokens.length-1)
            assert(lx.tokens[i].text == source[i..i+1]);
}

/// Tests a token iterator that skips the line comments
unittest
{
    int line = __LINE__ + 1;
    enum source = "://comment\n;//comment\n.";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 6);
    alias NoCommentRange = TokenRange!(TokenType.lineComment);
    NoCommentRange range = NoCommentRange(lx.tokens);
    assert(range.front().type == TokenType.colon);
    range.popFront();
    assert(range.front().type == TokenType.semiColon);
    range.popFront();
    assert(range.front().type == TokenType.dot);
    range.popFront();
    assert(range.empty());
    assert(lx.filename == __FILE_FULL_PATH__);
}

/// Null chars are allowed between two tokens
unittest
{
    int line = __LINE__ + 1;
    enum source = "1a\x00b";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].text == "1a");
    assert(lx.tokens[1].isTokIdentifier);
    assert(lx.tokens[1].text == "b");
}

/**
 * Returns: An array of tokens pointer converted to an array of string.
 */
string tokenPointerArrayText(Token*[] toks)
{
    import std.algorithm.iteration: map;
    import std.conv: to;
    return toks.map!(a => a.text()).to!string;
}

unittest
{
    char c1 = 'a';
    char c2 = 'b';
    Token t1 = Token(&c1, (&c1)+1, 0, 0, TokenType.identifier);
    Token t2 = Token(&c2, (&c2)+1, 0, 0, TokenType.identifier);
    assert([&t1, &t2].tokenPointerArrayText == "[\"a\", \"b\"]");
}

unittest
{
    int line = __LINE__ + 1;
    enum source = q{};
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 1);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `A0m`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].text == source);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `/`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].text == source);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `"
    multiple lines
    "`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokStringLiteral);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "s8\r\ns16";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].type == TokenType.s8);
    assert(lx.tokens[1].type == TokenType.s16);
    assert(lx.tokens[1].line == lx.tokens[0].line + 1);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `//`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].text == source);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `//3456789012`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].text == source);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `//
    /*
    */
    //
    /**/`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 5);
    assert(lx.tokens[0].isTokLineComment);
    assert(lx.tokens[1].isTokStarComment);
    assert(lx.tokens[2].isTokLineComment);
    assert(lx.tokens[3].isTokStarComment);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `/*`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "\xFF";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 1);
    assert(lx.tokens[0].type == TokenType.eof);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "/*nullchar\x00";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `a`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].text == source);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `12345678`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].text == source);
    assert(lx.tokens[0].isTokIntegerLiteral);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `1234.a`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 4);
    assert(lx.tokens[0].isTokIntegerLiteral);
    assert(lx.tokens[0].text == "1234");
    assert(lx.tokens[1].isTokDot);
    assert(lx.tokens[2].isTokIdentifier);
    assert(lx.tokens[2].text == "a");
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `(0)`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 4);
    assert(lx.tokens[0].isTokLeftParen);
    assert(lx.tokens[1].isTokIntegerLiteral);
    assert(lx.tokens[2].isTokRightParen);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `1234.01`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokFloatLiteral);
    assert(lx.tokens[0].text == "1234.01");
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `-1234.01`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokMinus);
    assert(lx.tokens[1].isTokFloatLiteral);
    assert(lx.tokens[1].text == source[1..$]);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `0x12AF20`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokHexLiteral);
    assert(lx.tokens[0].text == source);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `0X1234_abcdef`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokHexLiteral);
    assert(lx.tokens[0].text == source);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `s8 /*s16*/ s32`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 4);
    assert(lx.tokens[0].type == TokenType.s8);
    assert(lx.tokens[1].type == TokenType.starComment);
    assert(lx.tokens[1].text == "/*s16*/");
    assert(lx.tokens[2].type == TokenType.s32);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "s8 /*\n/*\n*/ s32";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 4);
    assert(lx.tokens[0].type == TokenType.s8);
    assert(lx.tokens[1].type == TokenType.starComment);
    assert(lx.tokens[1].text == "/*\n/*\n*/");
    assert(lx.tokens[2].type == TokenType.s32);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `8 >= 1 <= 0`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 6);
    assert(lx.tokens[0].isTokIntegerLiteral);
    assert(lx.tokens[1].isTokGreaterEqual);
    assert(lx.tokens[2].isTokIntegerLiteral);
    assert(lx.tokens[3].isTokLesserEqual);
    assert(lx.tokens[4].isTokIntegerLiteral);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `+++`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokPlusPlus);
    assert(lx.tokens[1].isTokPlus);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `---`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokMinusMinus);
    assert(lx.tokens[1].isTokMinus);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `+ "abcdef" -`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 4);
    assert(lx.tokens[0].isTokPlus);
    assert(lx.tokens[1].isTokStringLiteral);
    assert(lx.tokens[1].text == "abcdef");
    assert(lx.tokens[2].isTokMinus);
    // ! for coverage !
    lx.printTokens();
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `s64 aka a`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 4);
    assert(lx.tokens[0].isTokBasicType);
    assert(lx.tokens[1].isTokAka);
    assert(lx.tokens[2].isTokIdentifier);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `$function $identifier`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 4);
    assert(lx.tokens[0].isTokIdentifier);
    assert(lx.tokens[0].keywordAsIdentifier);
    assert(lx.tokens[0].text == "function");
    assert(lx.tokens[1].isTokDollar);
    assert(lx.tokens[2].isTokIdentifier);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `==|`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokEqualEqual);
    assert(lx.tokens[1].isTokPipe);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `&&&`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokAndAnd);
    assert(lx.tokens[1].isTokAmp);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `|||`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokOrOr);
    assert(lx.tokens[1].isTokPipe);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `>>>`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokRightShift);
    assert(lx.tokens[1].isTokGreater);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `<<<`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokLeftShift);
    assert(lx.tokens[1].isTokLesser);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `!!=`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokBang);
    assert(lx.tokens[1].isTokNotEqual);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "aa\x00";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "/*\x00";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "//\x00";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "0\x00";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "0.0\x00";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "0x1\x00";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "\"stringliteral\x00";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "0x1Y";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "0x1z";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "0/0";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 4);
    assert(lx.tokens[0].isTokIntegerLiteral);
    assert(lx.tokens[1].isTokDiv);
    assert(lx.tokens[2].isTokIntegerLiteral);
    assert(lx.tokens[3].type == TokenType.eof);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "1_000.000_000";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokFloatLiteral);
    assert(lx.tokens[1].type == TokenType.eof);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "#!bin/yatol -until=parsing \r\n unit a;";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 4);
    assert(lx.tokens[0].isTokUnit);
    assert(lx.tokens[0].line == line + 1);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "#!";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 1, lx.tokens[0].text);
    assert(lx.tokens[0].type == TokenType.eof);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `"unterminated`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `"unterminated\"`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `"terminated_by_double_quote\""`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokStringLiteral);
    assert(lx.tokens[0].rawText == source[1..$-1]);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "`rawString`";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokStringLiteral);
    assert(lx.tokens[0].rawText == source[1..$-1]);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "`rawString";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    static immutable source = "00000";
    Lexer lx;
    lx.setSourceFromText(source[0..1], __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokIntegerLiteral);
}

unittest
{
    int line = __LINE__ + 1;
    static immutable source = "0x0AAAA";
    Lexer lx;
    lx.setSourceFromText(source[0..3], __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokHexLiteral);
}

unittest
{
    int line = __LINE__ + 1;
    static immutable source = "0.01";
    Lexer lx;
    lx.setSourceFromText(source[0..3], __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokFloatLiteral);
    assert(lx.tokens[0].text == "0.0");
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "`rawString\x00";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `a+=`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokIdentifier);
    assert(lx.tokens[1].isTokPlusEqual);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `a-=`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokIdentifier);
    assert(lx.tokens[1].isTokMinusEqual);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `a*=`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokIdentifier);
    assert(lx.tokens[1].isTokMulEqual);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `a/=`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokIdentifier);
    assert(lx.tokens[1].isTokDivEqual);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `a%=`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokIdentifier);
    assert(lx.tokens[1].isTokModEqual);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `a&=`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokIdentifier);
    assert(lx.tokens[1].isTokAmpEqual);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `a|=`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokIdentifier);
    assert(lx.tokens[1].isTokPipeEqual);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `a^=`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokIdentifier);
    assert(lx.tokens[1].isTokXorEqual);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `a<<=`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokIdentifier);
    assert(lx.tokens[1].isTokLeftShiftEqual);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `a>>=`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokIdentifier);
    assert(lx.tokens[1].isTokRightShiftEqual);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `a?..`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 4);
    assert(lx.tokens[0].isTokIdentifier);
    assert(lx.tokens[1].isTokOptAccess);
    assert(lx.tokens[2].isTokDot);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "\xEF\xBB\xBF";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 1);
    assert(lx.tokens[0].type == TokenType.eof);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "\xEF\xBB\xBFunit a;";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 4);
    assert(lx.tokens[0].isTokUnit && lx.tokens[0].column == 1);
    assert(lx.tokens[1].isTokIdentifier);
    assert(lx.tokens[2].isTokSemicolon);
    assert(lx.tokens[3].type == TokenType.eof);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "\xEF\xBB\xBB";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 1);
    assert(lx.tokens[0].type == TokenType.eof);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "unit £";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokUnit);
    assert(lx.tokens[1].isTokInvalid);
    assert(lx.tokens[2].type == TokenType.eof);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "^~...? null true false % assert";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 10);
    assert(lx.tokens[0].isTokXor);
    assert(lx.tokens[1].isTokTidle);
    assert(lx.tokens[2].isTokEllipsis);
    assert(lx.tokens[3].isTokQmark);
    assert(lx.tokens[4].isTokNull);
    assert(lx.tokens[5].isTokTrue);
    assert(lx.tokens[6].isTokFalse);
    assert(lx.tokens[7].isTokMod);
    assert(lx.tokens[8].isTokAssert);
    assert(lx.tokens[$-1].type == TokenType.eof);
}

