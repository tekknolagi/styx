module yatol.lexer.types;

/// Enumerates the different token
enum TokenType : ubyte
{
    invalid,
    identifier,
    intLiteral,
    floatLiteral,
    hexLiteral,
    stringLiteral,
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
    return_,
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
    var,
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
    dotDot,
    ellipsis,
    // operators
    div,
    minus,
    mul,
    plus,
    minusMinus,
    plusPlus,
}

unittest
{
    static assert(tokenStringTable.length == TokenType.max + 1);
}

/**
 * Maps a $(D TokenType) to a string, between parens when the
 * token string is variable.
 */
static immutable string[TokenType.max + 1] tokenStringTable =
[
    "(invalid)",
    "(identifier)",
    "(integerLiteral)",
    "(floatLiteral)",
    "(hexLiteral)",
    "(stringLiteral)",
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
    "return",
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
    "var",
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
    "..",
    "...",
    // operators
    "/",
    "-",
    "*",
    "+",
    "--",
    "++",
];

unittest
{
    import std.traits : EnumMembers;
    foreach(m; EnumMembers!TokenType)
        static assert(m.tokenString != "");
}

/**
 * Maps a $(D TokenType) to a string, between parens when the
 * token string is variable.
 */
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
static immutable lastSymbol = TokenType.ellipsis;

/// The value of the first operator.
static immutable firstOperator = TokenType.div;
/// The value of the last operator.
static immutable lastOperator = TokenType.plus;

/// The value of the first number literal.
static immutable firstNumberLiteral = TokenType.intLiteral;
/// The value of the last number literal.
static immutable lastNumberLiteral = TokenType.hexLiteral;

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
        "unit", "", "virtual", "", "", "", "", "", "", "f32", "", "", "f64", "",
        "", "", "protection", "ureg", "", "", "", "", "", "function", "return",
        "", "struct", "u8", "u32", "", "", "u64", "u16", "", "", "class", "",
        "interface", "while", "", "continue", "", "", "", "", "", "import",
        "sreg", "", "", "var", "if", "", "", "break", "else", "", "s8", "s32",
        "", "", "s64", "s16", "static"
    ];

    static const ubyte[256] _coefficients =
    [
        166, 85, 251, 178, 59, 196, 222, 186, 69, 109, 241, 207, 155, 67, 65,
        15, 127, 213, 54, 66, 38, 212, 223, 169, 238, 254, 130, 68, 115, 155,
        14, 126, 103, 59, 247, 47, 2, 103, 245, 191, 3, 66, 80, 139, 205, 148,
        98, 206, 82, 252, 40, 163, 123, 120, 211, 92, 138, 254, 77, 249, 122,
        143, 51, 139, 219, 17, 124, 64, 143, 106, 39, 174, 161, 198, 37, 215,
        153, 198, 10, 47, 102, 75, 157, 150, 191, 167, 205, 120, 143, 52, 95,
        176, 8, 42, 34, 83, 103, 0, 50, 27, 159, 143, 126, 50, 192, 117, 154,
        54, 234, 102, 26, 164, 16, 154, 255, 175, 32, 209, 115, 248, 16, 224,
        254, 25, 90, 202, 78, 210, 244, 7, 13, 253, 21, 252, 241, 68, 214, 80,
        140, 115, 107, 55, 189, 104, 22, 49, 88, 249, 173, 18, 96, 9, 171, 23,
        89, 170, 238, 66, 32, 206, 196, 132, 113, 89, 122, 214, 83, 89, 174,
        113, 72, 152, 74, 204, 73, 219, 54, 200, 185, 42, 191, 3, 197, 175, 192,
        28, 79, 102, 224, 110, 230, 145, 89, 250, 95, 189, 189, 7, 137, 204, 76,
        113, 38, 40, 211, 145, 155, 90, 90, 18, 138, 168, 148, 154, 29, 89, 171,
        34, 59, 159, 65, 23, 190, 121, 174, 62, 255, 124, 99, 92, 158, 131, 150,
        48, 140, 169, 6, 35, 59, 142, 67, 202, 43, 201, 109, 128, 65, 224, 231,
        109, 170, 119, 113, 146, 110, 247
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

    ///
    this()(auto ref Tokens tokens)
    {
        _tokens = tokens;
    }

    ///
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

    ///
    ref const(Token) front()
    {
        import std.range: front;
        return front(_tokens);
    }

    ///
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

    /// Conveniance function used by the parser.
    bool isTokDotDot() const {return type == TokenType.dotDot;}

    /// Conveniance function used by the parser.
    bool isTokEllipsis() const {return type == TokenType.ellipsis;}

    /// Conveniance function used by the parser.
    bool isTokReturn() const {return type == TokenType.return_;}

    /// Conveniance function used by the parser.
    bool isTokBreak() const {return type == TokenType.break_;}

    /// Conveniance function used by the parser.
    bool isTokContinue() const {return type == TokenType.continue_;}

    /// Conveniance function used by the parser.
    bool isTokStringLiteral() const {return type == TokenType.stringLiteral;}

    /// Conveniance function used by the parser.
    bool isTokLineComment() const {return type == TokenType.lineComment;}

    /// Conveniance function used by the parser.
    bool isTokStarComment() const {return type == TokenType.starComment;}

    /// Conveniance function used by the parser.
    bool isTokVar() const {return type == TokenType.var;}

    /// Conveniance function used by the parser.
    bool isUnaryPrefix() const
    {
        return type == TokenType.plusPlus || type == TokenType.minusMinus ||
            type == TokenType.mul || type == TokenType.amp;
    }

    /// Conveniance function used by the parser.
    bool isUnarySuffix() const
    {
        return type == TokenType.plusPlus || type == TokenType.minusMinus;
    }

    /// Conveniance function used by the parser.
    bool isNumberLiteral() const
    {
        return firstNumberLiteral <= type && type <= lastNumberLiteral;
    }
}

