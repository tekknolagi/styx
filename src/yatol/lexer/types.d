module yatol.lexer.types;

/// Stores a position in the source code.
struct Position
{
    /// The line, 1-based.
    size_t line;
    /// The column, 1-based.
    size_t column;
}

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
    const_,
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
    dollar,
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
    "const",
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
    "$",
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
        "", "", "unit", "", "continue", "", "function", "f32", "", "s16", "",
        "static", "", "", "const", "", "u16", "if", "s32", "return", "class",
        "", "protection", "s8", "", "u32", "", "", "", "", "u8", "interface",
        "", "", "while", "sreg", "else", "var", "", "", "f64", "", "ureg", "",
        "", "", "", "", "", "", "", "s64", "virtual", "break", "", "", "", "",
        "u64", "struct", "", "", "import", ""
    ];

    static const ubyte[256] _coefficients =
    [
        87, 0, 60, 217, 28, 39, 73, 244, 7, 60, 180, 44, 254, 16, 112, 197, 195,
        203, 67, 7, 125, 144, 213, 49, 126, 57, 134, 125, 53, 96, 87, 172, 39,
        177, 105, 4, 116, 137, 241, 78, 40, 173, 41, 126, 196, 223, 162, 21,
        181, 118, 70, 177, 32, 65, 248, 156, 188, 238, 72, 244, 53, 96, 16, 167,
        177, 97, 231, 224, 25, 161, 217, 163, 210, 54, 196, 57, 233, 199, 147,
        66, 12, 207, 84, 155, 4, 186, 82, 223, 237, 245, 104, 68, 203, 189, 65,
        188, 177, 83, 23, 158, 162, 14, 80, 88, 37, 193, 239, 155, 173, 169,
        160, 54, 253, 245, 226, 27, 127, 226, 176, 65, 155, 103, 98, 95, 12,
        140, 242, 98, 235, 194, 82, 64, 110, 71, 169, 83, 107, 80, 99, 90, 69,
        182, 126, 42, 193, 198, 98, 165, 169, 232, 190, 25, 47, 249, 76, 64,
        120, 163, 10, 187, 204, 162, 249, 116, 219, 92, 206, 159, 91, 115, 252,
        71, 41, 128, 34, 95, 50, 177, 121, 79, 242, 17, 63, 128, 229, 108, 45,
        166, 182, 210, 46, 74, 20, 121, 133, 46, 203, 155, 24, 22, 137, 82, 127,
        15, 36, 223, 250, 122, 52, 207, 32, 17, 133, 177, 187, 99, 108, 36, 154,
        151, 73, 126, 93, 93, 79, 70, 183, 58, 193, 113, 204, 6, 180, 183, 205,
        209, 38, 252, 123, 124, 182, 167, 221, 246, 158, 104, 224, 145, 23, 41,
        173, 238, 210, 135, 170, 123
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
    Position _pos;
    bool _kwAsIdent;

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
        _type = type == TokenType.identifier ? text() in Keywords : type;
        if (_length > 1 && _start[0] == '$')
        {
            ++_start;
            --_length;
            --column;
            _kwAsIdent = true;
        }
        _pos = Position(line, column);
    }

    /// Returns: $(D true) if the identifier is a keyword prefixed with dollar.
    bool keywordAsIdentifier() {return _kwAsIdent;}

    /// Returns: The token type.
    TokenType type() const {return _type;}

    /// Returns: The token position.
    ref const(Position) position() const {return _pos;}

    /// Returns: The line, 1 based, where the token starts.
    size_t line() const {return _pos.line;}

    /// Returns: The column, 1 based, where the token starts.
    size_t column() const {return _pos.column;}

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
    bool isTokConst() const {return type == TokenType.const_;}

    /// Conveniance function used by the parser.
    bool isTokDollar() const {return type == TokenType.dollar;}

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

