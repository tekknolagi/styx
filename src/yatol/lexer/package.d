module yatol.lexer;

import
    core.stdc.stdlib;
import
    std.stdio, std.file;
import
    yatol.lexer.types;

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
        writefln("%s(%d,%d): warning, %s", _filename, _frontLine,
            _frontColumn, message);
    }

    void error(const(char[]) message)
    {
        writefln("%s(%d,%d): error, %s", _filename, _frontLine,
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
        if (!_anticipated)
            error("INTERNAL, attempt to validate an unanticipated token");
        if (_tokens.length && _tokens[$-1].isTokDollar &&
            _anticipated == TokenType.identifier &&
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
        if (!_anticipated)
            error("INTERNAL, attempt to validate an unanticipated token");
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
    char* lookup(size_t value)
    {
        return _front + value;
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
                validateToken();
                return;
            }
            switch(*_front)
            {
            case 0:
                error("null character encountered inside a string literal");
                advance();
                validateToken(TokenType.invalid);
                return;
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

    // lexes a line comment
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
        if (*_front == '-')
        {
            advance();
        }
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
                if ('0' <= *lookup(1) && *lookup(1) <= '9')
                {
                    advance();
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

    // lex
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

    // Lex an identifier or a keywords.
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

    void skipSheBang()
    {
        if (_front && _front + 2 <= _back && *_front == '#' && *lookup(1) == '!')
        {
            while (true)
            {
                if (_front > _back)
                {
                    return;
                }
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
        skipSheBang();
        if (_front) while (_front <= _back)
        {
            switch(*_front)
            {
            case 0:
                warning("null character encountered between two tokens");
                advance();
                continue;
            case ' ', '\t':
                advance();
                continue;
            case '\r', '\n':
                processlineEnding;
                continue;
            case '/':
                if (_front <= _back)
                {
                    if ( *lookup(1) == '/')
                        lexLineComment();
                    else if (*lookup(1) == '*')
                        lexStarComment;
                    else
                    {
                        anticipateToken(TokenType.div);
                        advance();
                        validateToken();
                    }
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
                if (_front <= _back && ('x' == *lookup(1) || *lookup(1) == 'X'))
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
                if (_front < _back)
                {
                    if ( '0' <= *lookup(1) && *lookup(1) <= '9')
                    {
                        anticipateToken(TokenType.intLiteral);
                        lexIntegerLiteral();
                    }
                    else if (*lookup(1) == '-')
                    {
                        anticipateToken(TokenType.minusMinus);
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
            case '&':
                anticipateToken(TokenType.amp);
                advance();
                validateToken();
                continue;
            case '|':
                anticipateToken(TokenType.pipe);
                advance();
                validateToken();
                continue;
            case '+':
                anticipateToken(TokenType.plus);
                if (_front <= _back && *lookup(1) == '+')
                {
                    advance();
                    advance();
                    validateToken(TokenType.plusPlus);
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
            case '*':
                anticipateToken(TokenType.mul);
                advance();
                validateToken();
                continue;
            case '.':
                anticipateToken(TokenType.dot);
                if (_front <= _back && *lookup(1) == '.')
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
            case '!':
                anticipateToken(TokenType.bang);
                advance();
                validateToken();
                continue;
            case '$':
                anticipateToken(TokenType.dollar);
                advance();
                validateToken();
                continue;
            case '=':
                anticipateToken(TokenType.equal);
                if (_front <= _back && *lookup(1) == '=')
                {
                    advance();
                    advance();
                    validateToken(TokenType.equalEqual);
                }
                else
                {
                    advance();
                    validateToken();
                }
                continue;
            case '>':
                anticipateToken(TokenType.greater);
                if (_front <= _back && *lookup(1) == '=')
                {
                    advance();
                    advance();
                    validateToken(TokenType.greaterEqual);
                }
                else
                {
                    advance();
                    validateToken();
                }
                continue;
            case '<':
                anticipateToken(TokenType.lesser);
                if (_front <= _back && *lookup(1) == '=')
                {
                    advance();
                    advance();
                    validateToken(TokenType.lesserEqual);
                }
                else
                {
                    advance();
                    validateToken();
                }
                continue;
            default:
                anticipateToken(TokenType.invalid);
                error("invalid input character");
                advance();
                validateToken();
            }
        }
    }

    /// Returns: The tokens produced after lexing.
    Tokens tokens() {return _tokens;}

    /// Returns: The name of the life that's been lexed.
    char[] filename() {return _filename;}

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

    assert(lx.tokens.length == 22);
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
    int line = __LINE__ + 1;
    enum source = q{};
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 0);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `A0m`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 1);
    assert(lx.tokens[0].text == source);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `/`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens.length == 1);
    assert(lx.tokens[0].isTokStringLiteral);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "s8\r\ns16";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 2);
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
    assert(lx.tokens.length == 1);
    assert(lx.tokens[0].text == source);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `//3456789012`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens.length == 4);
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
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "/*nullchar\x00";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 1);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `a`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 1);
    assert(lx.tokens[0].text == source);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `12345678`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens.length == 3);
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
    assert(lx.tokens.length == 3);
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
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens.length == 1);
    assert(lx.tokens[0].isTokFloatLiteral);
    assert(lx.tokens[0].text == source);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `0x12AF20`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens.length == 3);
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
    assert(lx.tokens.length == 3);
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
    assert(lx.tokens.length == 5);
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
    assert(lx.tokens.length == 2);
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
    assert(lx.tokens.length == 2);
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
    assert(lx.tokens.length == 3);
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
    enum source = `is s64 aka a`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 4);
    assert(lx.tokens[0].isTokIs);
    assert(lx.tokens[1].isTokBasicType);
    assert(lx.tokens[2].isTokAka);
    assert(lx.tokens[3].isTokIdentifier);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = `$function $identifier`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == 3);
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
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].isTokEqualEqual);
    assert(lx.tokens[1].isTokPipe);
}

/// Tests the symbols and single char operators.
unittest
{
    int line = __LINE__ + 1;
    enum source = `.:;,()/[]{}*+-@!=><&$`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    assert(lx.tokens.length == source.length);
    foreach(i, tk; lx.tokens)
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
    assert(lx.tokens.length == 5);
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
    assert(lx.tokens.length == 2);
    assert(lx.tokens[0].text == "1a");
    assert(lx.tokens[1].isTokIdentifier);
    assert(lx.tokens[1].text == "b");
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "aa\x00";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens.length == 1);
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
    assert(lx.tokens.length == 1);
    assert(lx.tokens[0].isTokInvalid);
}

unittest
{
    int line = __LINE__ + 1;
    enum source = "#!bin/yatol -until=parsing \r\n unit a;";
    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line, 20);
    lx.lex();
    lx.printTokens;
    assert(lx.tokens.length == 3);
    assert(lx.tokens[0].isTokUnit);
    assert(lx.tokens[0].line == line + 1);
}
