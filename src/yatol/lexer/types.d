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
    //
    intLiteral,
    floatLiteral,
    hexLiteral,
    stringLiteral,
    //
    lineComment,
    starComment,
    // Keywords
    aka,
    auto_,
    break_,
    class_,
    const_,
    continue_,
    else_,
    enum_,
    foreach_,
    function_,
    if_,
    import_,
    in_,
    interface_,
    is_,
    null_,
    on,
    protection,
    return_,
    static_,
    struct_,
    switch_,
    unit,
    var,
    virtual,
    while_,
    // basic types
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
    mul,
    div,
    plus,
    minus,
    amp,
    pipe,
    // postfixes
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
    //
    "(integerLiteral)",
    "(floatLiteral)",
    "(hexLiteral)",
    "(stringLiteral)",
    //
    "(lineComment)",
    "(starComment)",
    // Keywords
    "aka",
    "auto",
    "break",
    "class",
    "const",
    "continue",
    "else",
    "enum",
    "foreach",
    "function",
    "if",
    "import",
    "in",
    "interface",
    "is",
    "null",
    "on",
    "protection",
    "return",
    "static",
    "struct",
    "switch",
    "unit",
    "var",
    "virtual",
    "while",
    // basic types
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
    "*",
    "/",
    "+",
    "-",
    "&",
    "|",
    //
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
static immutable ptrdiff_t firstKeyword = TokenType.aka;
/// The value of the last keyword.
static immutable ptrdiff_t lastKeyword = TokenType.while_;

/// The value of the first basic type.
static immutable ptrdiff_t firstBasicType = TokenType.f32;
/// The value of the last keyword.
static immutable ptrdiff_t lastBasicType = TokenType.ureg;

/// The value of the first symbol.
static immutable firstSymbol = TokenType.bang;
/// The value of the last symbol.
static immutable lastSymbol = TokenType.ellipsis;

/// The value of the first operator.
static immutable firstOperator = TokenType.mul;
/// The value of the last operator.
static immutable lastOperator = TokenType.pipe;

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
    ["", "function", "unit", "f32", "", "", "u32", "", "s8", "", "f64", "u16", "protection", "u64", "return", "null", "switch", "", "const", "foreach", "", "on", "", "interface", "var", "break", "", "", "auto", "while", "", "ureg", "", "is", "in", "", "", "", "s32", "", "u8", "", "", "s16", "enum", "s64", "", "import", "virtual", "aka", "", "", "", "", "static", "continue", "", "", "struct", "", "class", "else", "if", "sreg"];

    static const ubyte[256] _coefficients =
    [89, 193, 210, 37, 171, 229, 29, 211, 144, 176, 208, 85, 0, 148, 184, 227, 69, 6, 186, 95, 97, 250, 19, 197, 211, 3, 11, 230, 15, 146, 137, 249, 192, 234, 74, 43, 147, 179, 39, 203, 118, 208, 91, 201, 210, 40, 46, 254, 45, 254, 111, 103, 128, 86, 157, 125, 56, 86, 11, 47, 186, 113, 77, 62, 210, 46, 48, 65, 236, 154, 255, 66, 76, 26, 157, 49, 62, 57, 183, 137, 119, 68, 52, 232, 158, 158, 151, 202, 120, 44, 193, 135, 202, 48, 199, 21, 125, 248, 224, 125, 0, 35, 109, 111, 109, 209, 143, 193, 231, 200, 81, 68, 69, 120, 221, 80, 176, 176, 67, 21, 145, 0, 190, 34, 242, 174, 167, 237, 69, 136, 88, 129, 183, 158, 4, 232, 109, 201, 245, 109, 81, 77, 244, 50, 104, 192, 201, 240, 36, 125, 62, 187, 73, 110, 254, 57, 38, 173, 184, 134, 104, 236, 178, 32, 141, 71, 214, 77, 153, 151, 106, 26, 249, 129, 5, 147, 66, 246, 37, 210, 100, 179, 254, 254, 27, 152, 148, 19, 128, 147, 3, 54, 179, 95, 166, 233, 129, 157, 89, 17, 115, 202, 231, 229, 29, 139, 125, 106, 90, 215, 93, 54, 83, 199, 217, 169, 229, 62, 160, 89, 97, 196, 68, 225, 169, 164, 131, 189, 132, 168, 174, 103, 141, 55, 50, 223, 63, 224, 52, 134, 139, 199, 242, 167, 102, 187, 211, 100, 74, 7, 156, 50, 62, 219, 41, 131];

    static string generateFilledTable()
    {
        import std.algorithm: countUntil;
        import std.conv: to;
        string result = "static const TokenType[64] _filled = [";
        foreach(w; _words)
        {
            ptrdiff_t index = tokenStringTable[].countUntil(w);
            if (index >= firstKeyword && index <= lastBasicType)
                result ~= "TokenType." ~ to!string(cast(TokenType) index) ~ ", ";
            else
                result ~= "TokenType.invalid, ";
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
    static assert(TokenType.f32 > firstKeyword && TokenType.f32 < lastBasicType);
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
    bool isTokEnum() const {return type == TokenType.enum_;}

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
    bool isTokPipe() const {return type == TokenType.pipe;}

    /// Conveniance function used by the parser.
    bool isTokAka() const {return type == TokenType.aka;}

    /// Conveniance function used by the parser.
    bool isTokAuto() const {return type == TokenType.auto_;}

    /// Conveniance function used by the parser.
    bool isTokIs() const {return type == TokenType.is_;}

    /// Conveniance function used by the parser.
    bool isTokForeach() const {return type == TokenType.foreach_;}

    /// Conveniance function used by the parser.
    bool isTokSwitch() const {return type == TokenType.switch_;}

    /// Conveniance function used by the parser.
    bool isTokNull() const {return type == TokenType.null_;}

    /// Conveniance function used by the parser.
    bool isTokIn() const {return type == TokenType.in_;}

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

