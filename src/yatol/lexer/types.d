module yatol.lexer.types;

/// Enumerates the different token
enum TokenType : ubyte
{
    invalid,
    identifier,
    lineComment,
    // Keywords
    class_,
    f32,
    f64,
    import_,
    protection,
    s16,
    s32,
    s64,
    s8,
    sreg,
    struct_,
    u16,
    u32,
    u64,
    u8,
    unit,
    ureg,
    virtual,
    // symbols
    colon,
    comma,
    dot,
    semiColon,
    leftCurly,
    leftParen,
    leftSquare,
    rightCurly,
    rightParen,
    rightSquare,
    // operators
    div,
}

string tokenString(TokenType type)
{
    with (TokenType) final switch(type)
    {
    case invalid: return "(invalid)";
    case identifier: return "(identifier)";
    case lineComment: return "(line comment)";
    // Keywords
    case class_: return "class";
    case f32: return "f32";
    case f64: return "f64";
    case import_: return "import";
    case protection: return "protection";
    case s16: return "s16";
    case s32: return "s32";
    case s64: return "s64";
    case s8: return "s8";
    case sreg: return "sreg";
    case struct_: return "struct";
    case u16: return "u16";
    case u32: return "u32";
    case u64: return "u64";
    case u8: return "u8";
    case unit: return "unit";
    case ureg: return "ureg";
    case virtual: return "virtual";
    // symbols
    case colon: return ":";
    case comma: return ",";
    case dot: return ".";
    case semiColon: return ";";
    case leftCurly: return "{";
    case leftParen: return "(";
    case leftSquare: return "[";
    case rightCurly: return "}";
    case rightParen: return ")";
    case rightSquare: return "]";
    // operators
    case div: return "/";
    }
}

/// The value of the first keyword.
static immutable firstKeyword = TokenType.class_;
/// The value of the last keyword.
static immutable lastKeyword = TokenType.virtual;

/// The value of the first symbol.
static immutable firstSymbol = TokenType.colon;
/// The value of the last symbol.
static immutable lastSymbol = TokenType.rightParen;

/// The value of the first operator.
static immutable firstOperator = TokenType.div;
/// The value of the last operator.
static immutable lastOperator = TokenType.div;

/**
 * Hashset that allows to distinguish efficiently the identifiers
 * from the keywords.
 */
struct Keywords
{

private:

    /*
        rendered on 2017-Apr-11 07:01:53.8633993 by IsItThere.
         - PRNG seed: 6574
         - map length: 64
         - case sensitive: true
    */

    static const string[64] _words =
    [
        "", "", "struct", "s16", "", "", "s64", "",
        "", "", "", "", "s8", "", "", "",
        "s32", "class", "f64", "", "", "", "", "",
        "", "", "", "", "f32", "", "protection", "",
        "u16", "sreg", "", "u64", "", "", "virtual", "",
        "", "u8", "", "", "", "u32", "", "",
        "", "", "unit", "", "", "", "", "",
        "", "", "", "", "import", "", "ureg", ""
    ];

    static const TokenType[64] _filled =
    [
        TokenType.identifier, TokenType.identifier, TokenType.struct_, TokenType.s16, TokenType.identifier, TokenType.identifier, TokenType.s64, TokenType.identifier,
        TokenType.identifier, TokenType.identifier, TokenType.identifier, TokenType.identifier, TokenType.s8, TokenType.identifier, TokenType.identifier, TokenType.identifier,
        TokenType.s32, TokenType.class_, TokenType.f64, TokenType.identifier, TokenType.identifier, TokenType.identifier, TokenType.identifier, TokenType.identifier,
        TokenType.identifier, TokenType.identifier, TokenType.identifier, TokenType.identifier, TokenType.f32, TokenType.identifier, TokenType.protection, TokenType.identifier,
        TokenType.u16, TokenType.sreg, TokenType.identifier, TokenType.u64, TokenType.identifier, TokenType.identifier, TokenType.virtual, TokenType.identifier,
        TokenType.identifier, TokenType.u8, TokenType.identifier, TokenType.identifier, TokenType.identifier, TokenType.u32, TokenType.identifier, TokenType.identifier,
        TokenType.identifier, TokenType.identifier, TokenType.unit, TokenType.identifier, TokenType.identifier, TokenType.identifier, TokenType.identifier, TokenType.identifier,
        TokenType.identifier, TokenType.identifier, TokenType.identifier, TokenType.identifier, TokenType.import_, TokenType.identifier, TokenType.ureg, TokenType.identifier
    ];

    static const ubyte[256] _coefficients =
    [
        86, 23, 166, 46, 89, 29, 128, 140, 98, 24, 14, 249, 78, 187, 245,
        250, 211, 135, 215, 193, 32, 244, 140, 155, 13, 196, 130, 5, 81,
        181, 63, 164, 27, 137, 87, 249, 178, 169, 105, 201, 51, 247, 227,
        73, 106, 24, 68, 179, 117, 244, 190, 135, 247, 96, 196, 125, 1, 43,
        14, 103, 76, 132, 16, 29, 185, 76, 127, 13, 190, 158, 0, 198, 42,
        116, 193, 253, 223, 165, 148, 9, 115, 40, 183, 250, 77, 19, 170,
        125, 9, 253, 102, 37, 208, 222, 228, 252, 68, 43, 194, 113, 238,
        201, 215, 13, 193, 183, 113, 170, 95, 97, 4, 246, 159, 1, 128, 75,
        207, 40, 46, 12, 73, 115, 58, 64, 236, 176, 28, 238, 131, 67, 100,
        249, 60, 158, 146, 76, 79, 45, 127, 157, 28, 12, 139, 151, 64, 218,
        205, 255, 132, 106, 155, 217, 174, 28, 64, 161, 164, 198, 124, 68,
        145, 112, 94, 211, 192, 131, 196, 192, 3, 193, 222, 33, 197, 105,
        254, 182, 12, 123, 234, 134, 2, 90, 142, 55, 214, 254, 144, 47, 188,
        11, 36, 49, 251, 230, 192, 239, 44, 130, 29, 53, 185, 140, 49, 18,
        208, 193, 176, 234, 77, 66, 80, 197, 150, 65, 200, 75, 93, 108, 119,
        87, 74, 8, 187, 12, 18, 141, 104, 147, 17, 200, 214, 208, 123, 130,
        194, 208, 111, 50, 226, 187, 29, 221, 97, 16, 0, 228, 80, 157, 38,
        123, 238, 8, 239, 255, 133, 75
    ];

    static ushort hash(const char[] word) nothrow pure @safe @nogc
    {
        ushort result;
        foreach(i; 0..word.length)
        {
            result += _coefficients[word[i]];
        }
        return result % 64;
    }

public:

    /**
     * Support for the $(D in) operator.
     *
     * Returns: $(D TokenType.identifier) if the input argument is not a
     *  a keyword otherwise the $(D TokenType) that matches to the  keyword
     *  passed as argument.
     */
    static TokenType opBinaryRight(string op: "in")(const char[] word)
    {
        const ushort h = hash(word);
        TokenType result = _filled[h];
        if (result != TokenType.identifier && _words[h] != word)
            result = TokenType.identifier;
        return result;
    }

    /// Returns: true of the input argugment is a keyword.
    static bool isKeyword(const char[] word)
    {
        return opBinaryRight!"in"(word) != TokenType.identifier;
    }
}

///
unittest
{
    assert(("blalba" in Keywords) == TokenType.identifier);
    assert(("protection" in Keywords) == TokenType.protection);
    assert(("f32" in Keywords) == TokenType.f32);
    assert(("isitthere?" in Keywords) == TokenType.identifier);
    assert(Keywords.isKeyword("import"));
}

alias Tokens = Token[];
alias TokensList = Tokens[];


/**
 * Tokens iterator.
 *
 * Prams:
 *  TokenTypes = The $(D TokenType) to skip.
 */
struct TokenRange(TokenTypes...)
{

private:

    Tokens _tokens;

public:

    this()(auto ref Tokens tokens)
    {
        _tokens = tokens;
    }

    void popFront()
    {
        import std.range: popFront, front, empty;
        popFront(_tokens);

        static if (TokenTypes.length)
        {
            foreach(TokenType t; TokenTypes)
                while (!empty(_tokens) && t == front(_tokens).type)
                    popFront(_tokens);
        }
    }

    ref const(Token) front()
    {
        import std.range: front;
        return front(_tokens);
    }

    bool empty() const
    {
        import std.range: empty;
        return empty(_tokens);
    }
}


/**
 * A lexical token.
 */
struct Token
{

private:

    char* _start;
    size_t _length;
    size_t _line;
    size_t _column;
    TokenType _type;

public:

    /**
     * Constructs a new token.
     *
     * Params:
     *      start = Pointer to the first character. Must live longer than the token.
     *      stop = Pointer to the last character. Must live longer than the token.
     *      line = The line, 1 based, where the token starts.
     *      column = The column, 1 based, where the token starts.
     *      type = The token type. The keywords are only detected from here.
     */
    this(char* start, char* stop, size_t line, size_t column, TokenType type)
    {
        _start   = start;
        _length  = stop - start;
        _line    = line;
        _column  = column;
        _type = type == TokenType.identifier ? text() in Keywords : type;
    }

    /// Returns: The token text.
    TokenType type() const {return _type;}

    /// Returns: The line, 1 based, where the token starts.
    size_t line() const {return _line;}

    /// Returns: The column, 1 based, where the token starts.
    size_t column() const {return _column;}

    /// Returns: The token text.
    const(char[]) text() const {return _start[0.._length];}

    /// Conveniance function used by the parser.
    bool isTokKeyword() const {return firstKeyword <= type && type <= lastKeyword;}

    /// Conveniance function used by the parser.
    bool isTokSymbol() const {return firstSymbol <= type && type <= lastSymbol;}

    /// Conveniance function used by the parser.
    bool isTokIdentifier() const {return type == TokenType.identifier;}

    /// Conveniance function used by the parser.
    bool isTokProtection() const {return type == TokenType.protection;}

    /// Conveniance function used by the parser.
    bool isTokUnit() const {return type == TokenType.unit;}

    /// Conveniance function used by the parser.
    bool isTokVirtual() const {return type == TokenType.virtual;}

    /// Conveniance function used by the parser.
    bool isTokDot() const {return type == TokenType.dot;}

    /// Conveniance function used by the parser.
    bool isTokColon() const {return type == TokenType.colon;}

    /// Conveniance function used by the parser.
    bool isTokComma() const {return type == TokenType.comma;}

    /// Conveniance function used by the parser.
    bool isTokSemicolon() const {return type == TokenType.semiColon;}

    /// Conveniance function used by the parser.
    bool isTokLeftParen() const {return type == TokenType.leftParen;}

    /// Conveniance function used by the parser.
    bool isTokRightParen() const {return type == TokenType.rightParen;}

    /// Conveniance function used by the parser.
    bool isTokLeftCurly() const {return type == TokenType.leftCurly;}

    /// Conveniance function used by the parser.
    bool isTokRightCurly() const {return type == TokenType.rightCurly;}

    /// Conveniance function used by the parser.
    bool isTokLeftSquare() const {return type == TokenType.leftSquare;}

    /// Conveniance function used by the parser.
    bool isTokRightSquare() const {return type == TokenType.rightSquare;}

    /// Conveniance function used by the parser.
    bool isTokDiv() const {return type == TokenType.div;}

    /// Conveniance function used by the parser.
    bool isTokClass() const {return type == TokenType.class_;}

    /// Conveniance function used by the parser.
    bool isTokStruct() const {return type == TokenType.struct_;}
}

