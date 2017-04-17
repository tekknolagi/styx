module yatol.lexer.types;

/// Enumerates the different token
enum TokenType : ubyte
{
    invalid,
    identifier,
    lineComment,
    starComment,
    // Keywords
    class_,
    function_,
    import_,
    protection,
    static_,
    struct_,
    unit,
    virtual,
    f32,
    f64,
    s16,
    s32,
    s64,
    s8,
    sreg,
    u16,
    u32,
    u64,
    u8,
    ureg,
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
    mul,
}

static immutable string[TokenType.max+1] tokenStringTable =
[
    "(invalid)",
    "(identifier)",
    "(lineComment)",
    "(starComment)",
    // Keywords
    "class",
    "function",
    "import",
    "protection",
    "static",
    "struct",
    "unit",
    "virtual",
    "f32",
    "f64",
    "s16",
    "s32",
    "s64",
    "s8",
    "sreg",
    "u16",
    "u32",
    "u64",
    "u8",
    "ureg",
    // symbols
    ":",
    ",",
    ".",
    ";",
    "{",
    "(",
    "[",
    "}",
    ")",
    "]",
    // operators
    "/",
    "*",
];

string tokenString(TokenType type)
{
    return tokenStringTable[type];
}

/// The value of the first keyword.
static immutable firstKeyword = TokenType.class_;
/// The value of the last keyword.
static immutable lastKeyword = TokenType.ureg;

/// The value of the first basic type.
static immutable firstBasicType = TokenType.f32;
/// The value of the last keyword.
static immutable lastBasicType = TokenType.ureg;

/// The value of the first symbol.
static immutable firstSymbol = TokenType.colon;
/// The value of the last symbol.
static immutable lastSymbol = TokenType.rightSquare;

/// The value of the first operator.
static immutable firstOperator = TokenType.div;
/// The value of the last operator.
static immutable lastOperator = TokenType.mul;

/**
 * Hashset that allows to distinguish efficiently the identifiers
 * from the keywords.
 */
struct Keywords
{

private:

    /*
        rendered on 2017-Apr-15 01:22:36.1782244 by IsItThere.
         - PRNG seed: 6574
         - map length: 64
         - case sensitive: true
    */

    static const string[64] _words =
    [
        "", "s32", "", "f64", "", "", "", "ureg",
        "", "class", "", "", "", "s64", "", "",
        "", "", "", "", "", "u32", "", "static",
        "s8", "virtual", "", "", "", "s16", "", "",
        "", "u64", "", "", "", "", "", "",
        "", "", "", "struct", "u8", "", "", "",
        "", "u16", "unit", "sreg", "", "", "function", "f32",
        "", "", "protection", "", "import", "", "", ""
    ];

    static const ubyte[256] _coefficients =
    [
        39, 1, 114, 55, 44, 250, 143, 153, 112, 124, 85, 60, 102, 222, 32, 255,
        233, 140, 79, 202, 232, 90, 141, 219, 150, 206, 253, 87, 52, 14, 224, 94,
        204, 178, 242, 176, 92, 116, 69, 64, 177, 55, 211, 248, 223, 30, 45, 235,
        28, 89, 76, 210, 73, 149, 33, 195, 181, 82, 212, 168, 199, 239, 55, 127,
        196, 80, 176, 17, 88, 242, 241, 127, 176, 46, 200, 110, 2, 154, 107, 249,
        9, 229, 219, 200, 175, 71, 82, 152, 156, 68, 134, 96, 218, 253, 73, 62,
        224, 168, 209, 48, 30, 15, 217, 22, 194, 38, 135, 102, 235, 55, 218, 225,
        152, 116, 107, 163, 59, 119, 67, 205, 109, 36, 195, 145, 10, 156, 252,
        237, 32, 51, 125, 57, 105, 3, 206, 243, 90, 165, 173, 18, 235, 23, 0,
        157, 32, 153, 154, 171, 65, 89, 131, 76, 18, 103, 196, 107, 89, 254, 41,
        37, 172, 19, 18, 94, 44, 144, 58, 163, 43, 235, 186, 167, 17, 252, 32,
        243, 2, 224, 76, 41, 193, 168, 233, 237, 4, 153, 231, 14, 119, 252, 75,
        100, 7, 57, 61, 70, 100, 45, 157, 13, 219, 119, 62, 180, 97, 254, 157,
        137, 231, 2, 170, 153, 218, 81, 119, 56, 173, 0, 156, 248, 68, 49, 9,
        216, 73, 237, 107, 15, 251, 80, 48, 149, 142, 161, 232, 86, 74, 91, 30,
        151, 169, 130, 245, 197, 124, 29, 66, 111, 64, 251, 198, 2, 189, 138,
        110, 79
    ];

    static string generateFilledTable()
    {
        import std.algorithm: countUntil;
        import std.conv: to;
        string result = "static const TokenType[64] _filled = [";
        foreach(w; _words)
        {
            ptrdiff_t index = tokenStringTable[].countUntil(w);
            if (firstKeyword > index || index > lastKeyword)
                result ~= "TokenType.invalid, ";
            else
                result ~= "TokenType." ~ to!string(cast(TokenType) index) ~ ", ";
        }
        result ~= "];";
        return result;
    }

    mixin(generateFilledTable());

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
    bool isTokBasicType() const {return firstBasicType <= type && type <= lastBasicType;}

    /// Conveniance function used by the parser.
    bool isTokKeyword() const {return firstKeyword <= type && type <= lastKeyword;}

    /// Conveniance function used by the parser.
    bool isTokSymbol() const {return firstSymbol <= type && type <= lastSymbol;}

    /// Conveniance function used by the parser.
    bool isTokIdentifier() const {return type == TokenType.identifier;}

    /// Conveniance function used by the parser.
    bool isTokProtection() const {return type == TokenType.protection;}

    /// Conveniance function used by the parser.
    bool isTokImport() const {return type == TokenType.import_;}

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

    /// Conveniance function used by the parser.
    bool isTokStatic() const {return type == TokenType.static_;}

    /// Conveniance function used by the parser.
    bool isTokFunction() const {return type == TokenType.function_;}

    /// Conveniance function used by the parser.
    bool isTokMul() const {return type == TokenType.mul;}
}

