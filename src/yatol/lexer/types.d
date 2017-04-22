module yatol.lexer.types;

/// Enumerates the different token
enum TokenType : ubyte
{
    invalid,
    identifier,
    intLiteral,
    floatLiteral,
    hexLiteral,
    lineComment,
    starComment,
    // Keywords
    break_,
    class_,
    continue_,
    else_,
    function_,
    if_,
    import_,
    interface_,
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
    while_,
    // symbols
    amp,
    bang,
    colon,
    comma,
    dot,
    equal,
    equalEqual,
    greater,
    greaterEqual,
    semiColon,
    leftCurly,
    leftParen,
    leftSquare,
    lesser,
    lesserEqual,
    rightCurly,
    rightParen,
    rightSquare,
    at,
    // operators
    div,
    minus,
    minusMinus,
    mul,
    plus,
    plusPlus,
}

unittest
{
    static assert(tokenStringTable.length == TokenType.max + 1);
}

enum tokenStringTable =
[
    "(invalid)",
    "(identifier)",
    "(integerLiteral)",
    "(floatLiteral)",
    "(hexLiteral)",
    "(lineComment)",
    "(starComment)",
    // Keywords
    "break",
    "class",
    "continue",
    "else",
    "function",
    "if",
    "import",
    "interface",
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
    "while",
    // symbols
    "&",
    "!",
    ":",
    ",",
    ".",
    "=",
    "==",
    ">",
    ">=",
    ";",
    "{",
    "(",
    "[",
    "<",
    "<=",
    "}",
    ")",
    "]",
    "@",
    // operators
    "/",
    "-",
    "--",
    "*",
    "+",
    "++",
];

string tokenString(TokenType type)
{
    return tokenStringTable[type];
}

/// The value of the first keyword.
static immutable firstKeyword = TokenType.break_;
/// The value of the last keyword.
static immutable lastKeyword = TokenType.while_;

/// The value of the first basic type.
static immutable firstBasicType = TokenType.f32;
/// The value of the last keyword.
static immutable lastBasicType = TokenType.ureg;

/// The value of the first symbol.
static immutable firstSymbol = TokenType.amp;
/// The value of the last symbol.
static immutable lastSymbol = TokenType.at;

/// The value of the first operator.
static immutable firstOperator = TokenType.div;
/// The value of the last operator.
static immutable lastOperator = TokenType.plus;

/**
 * Hashset that allows to distinguish efficiently the identifiers
 * from the keywords.
 */
struct Keywords
{

private:

/*
        rendered on 2017-Apr-20 08:16:26.2478601 by IsItThere.
         - PRNG seed: 6574
         - map length: 64
         - case sensitive: true
    */

    static const string[64] _words =
    [
        "", "protection", "class", "", "f32", "", "f64", "",
        "break", "virtual", "", "", "s32", "", "s64", "",
        "import", "u32", "", "u64", "", "", "sreg", "",
        "", "", "unit", "ureg", "", "interface", "", "",
        "if", "", "", "", "", "static", "s16", "",
        "", "struct", "", "u16", "", "else", "s8", "",
        "", "", "", "u8", "continue", "", "", "",
        "", "", "", "", "", "while", "", "function"
    ];

    static const ubyte[256] _coefficients =
    [
        222, 14, 227, 135, 167, 188, 205, 95, 200, 202, 15, 63,
        148, 183, 31, 42, 51, 32, 3, 214, 105, 84, 120, 51, 180,
        46, 164, 228, 163, 73, 205, 56, 97, 211, 135, 129, 83,
        85, 116, 234, 84, 122, 169, 242, 224, 223, 43, 20, 254,
        79, 180, 176, 55, 47, 239, 59, 198, 173, 165, 220, 28,
        226, 217, 14, 23, 115, 186, 38, 58, 143, 199, 106, 40,
        116, 250, 192, 2, 65, 111, 116, 182, 49, 102, 152, 49,
        11, 164, 0, 98, 184, 16, 222, 76, 51, 87, 14, 51, 56, 19,
        31, 97, 21, 96, 10, 89, 192, 169, 153, 219, 188, 90, 12, 38,
        126, 143, 168, 211, 45, 39, 52, 239, 240, 218, 106, 112, 63,
        46, 36, 230, 94, 197, 40, 199, 97, 68, 255, 124, 139, 229, 13,
        182, 170, 60, 181, 100, 52, 210, 25, 24, 10, 206, 75, 22, 164,
        101, 145, 63, 95, 252, 95, 17, 151, 53, 46, 10, 203, 37, 19, 29,
        6, 84, 157, 10, 249, 92, 108, 50, 41, 45, 105, 37, 198, 4, 220,
        209, 47, 222, 55, 20, 200, 192, 244, 166, 48, 145, 80, 34, 57,
        38, 43, 12, 40, 175, 186, 243, 61, 68, 70, 77, 254, 226, 173,
        89, 83, 13, 24, 117, 5, 0, 184, 69, 210, 28, 225, 127, 79, 48,
        189, 69, 240, 151, 0, 219, 123, 144, 23, 140, 101, 186, 103,
        239, 142, 33, 22, 16, 70, 152, 169, 35, 157, 203, 85, 176, 60, 66, 107
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

    /// Conveniance function used by the parser.
    bool isTokInterface() const {return type == TokenType.interface_;}

    /// Conveniance function used by the parser.
    bool isTokIntegerLiteral() const {return type == TokenType.intLiteral;}

    /// Conveniance function used by the parser.
    bool isTokFloatLiteral() const {return type == TokenType.floatLiteral;}

    /// Conveniance function used by the parser.
    bool isTokHexLiteral() const {return type == TokenType.hexLiteral;}

    /// Conveniance function used by the parser.
    bool isTokInvalid() const {return type == TokenType.invalid;}

    /// Conveniance function used by the parser.
    bool isTokMinus() const {return type == TokenType.minus;}

    /// Conveniance function used by the parser.
    bool isTokPlus() const {return type == TokenType.plus;}

    /// Conveniance function used by the parser.
    bool isTokAt() const {return type == TokenType.at;}

    /// Conveniance function used by the parser.
    bool isTokEqualEqual() const {return type == TokenType.equalEqual;}

    /// Conveniance function used by the parser.
    bool isTokEqual() const {return type == TokenType.equal;}

    /// Conveniance function used by the parser.
    bool isTokGreater() const {return type == TokenType.greater;}

    /// Conveniance function used by the parser.
    bool isTokGreaterEqual() const {return type == TokenType.greaterEqual;}

    /// Conveniance function used by the parser.
    bool isTokLesser() const {return type == TokenType.lesser;}

    /// Conveniance function used by the parser.
    bool isTokLesserEqual() const {return type == TokenType.lesserEqual;}

    /// Conveniance function used by the parser.
    bool isTokBang() const {return type == TokenType.bang;}

    /// Conveniance function used by the parser.
    bool isTokIf() const {return type == TokenType.if_;}

    /// Conveniance function used by the parser.
    bool isTokWhile() const {return type == TokenType.while_;}

    /// Conveniance function used by the parser.
    bool isTokAmp() const {return type == TokenType.amp;}

    /// Conveniance function used by the parser.
    bool isTokPlusPlus() const {return type == TokenType.plusPlus;}

    /// Conveniance function used by the parser.
    bool isTokMinusMinus() const {return type == TokenType.minusMinus;}
}

