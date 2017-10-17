/**
 * The yatol token type, as produced by the lexer.
 * Also contains a static hash map used to match the keywords.
 */
module yatol.token;

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
    eof,
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
    assert_,
    auto_,
    break_,
    class_,
    const_,
    continue_,
    else_,
    enum_,
    false_,
    finally_,
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
    throw_,
    true_,
    try_,
    union_,
    unit,
    var,
    version_,
    virtual,
    while_,
    // basic types
    bool_,
    f32,
    f64,
    s16,
    s32,
    s64,
    s8,
    sreg,
    super_,
    u16,
    u32,
    u64,
    u8,
    ureg,
    // symbols
    bang,
    qmark,
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
    at,
    dollar,
    dotDot,
    ellipsis,
    optAccess,
    // assign
    equal,
    mulEqual,
    divEqual,
    modEqual,
    plusEqual,
    minusEqual,
    ampEqual,
    pipeEqual,
    xorEqual,
    lshiftEqual,
    rshiftEqual,
    // relational
    equalEqual,
    notEqual,
    greater,
    greaterEqual,
    lesser,
    lesserEqual,
    // logical
    andAnd,
    orOr,
    // operators
    mul,
    div,
    mod,
    plus,
    minus,
    amp,
    pipe,
    tidle,
    lShift,
    rShift,
    xor,
    // postfixes
    minusMinus,
    plusPlus,
}

unittest
{
    static assert(tokenStringTable.length == TokenType.max + 1);
}

private static immutable string[TokenType.max + 1] tokenStringTable =
[
    "(invalid)",
    "(eof)",
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
    "assert",
    "auto",
    "break",
    "class",
    "const",
    "continue",
    "else",
    "enum",
    "false",
    "finally",
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
    "throw",
    "true",
    "try",
    "union",
    "unit",
    "var",
    "version",
    "virtual",
    "while",
    // basic types
    "bool",
    "f32",
    "f64",
    "s16",
    "s32",
    "s64",
    "s8",
    "sreg",
    "super",
    "u16",
    "u32",
    "u64",
    "u8",
    "ureg",
    // symbols
    "!",
    "?",
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
    "@",
    "$",
    "..",
    "...",
    "?.",
    // assign
    "=",
    "*=",
    "/=",
    "%=",
    "+=",
    "-=",
    "&=",
    "|=",
    "^=",
    "<<=",
    ">>=",
    // relational
    "==",
    "!=",
    ">",
    ">=",
    "<",
    "<=",
    // logical
    "&&",
    "||",
    // operators
    "*",
    "/",
    "%",
    "+",
    "-",
    "&",
    "|",
    "~",
    "<<",
    ">>",
    "^",
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
 * Maps a $(D TokenType) to a string. The text is between parens when the
 * token string is not constant, lie for a $(D TokenType.identifier).
 */
string tokenString(TokenType type)
{
    return tokenStringTable[type];
}

/// The $(D TokenType) of the first keyword.
static immutable TokenType firstKeyword = TokenType.aka;
/// The $(D TokenType) of the last keyword.
static immutable TokenType lastKeyword = TokenType.while_;

/// The $(D TokenType) of the first basic type.
static immutable TokenType firstBasicType = TokenType.bool_;
/// The $(D TokenType) of the last keyword.
static immutable TokenType lastBasicType = TokenType.ureg;

/// The $(D TokenType) of the first operator.
static immutable TokenType firstOperator = TokenType.equalEqual;
/// The $(D TokenType) of the last operator.
static immutable TokenType lastOperator = TokenType.xor;

/// The $(D TokenType) of the first number literal.
static immutable TokenType firstNumberLiteral = TokenType.intLiteral;
/// The $(D TokenType) of the last number literal.
static immutable TokenType lastNumberLiteral = TokenType.hexLiteral;

/// The $(D TokenType) of the first assignement operator.
static immutable TokenType firstAssignOperator = TokenType.equal;
/// The $(D TokenType) of the last assignement operator.
static immutable TokenType lastAssignOperator = TokenType.rshiftEqual;

/**
 * Hashset that allows to distinguish efficiently the identifiers
 * from the keywords.
 */
struct Keywords
{

private:

    /*
        rendered on 2017-Oct-18 00:08:03.6422228 by IsItThere.
         - PRNG seed: 6574
         - map length: 128
         - case sensitive: true
    */

    static const string[128] _words = ["", "u16", "if", "try", "is", "unit", "", "u64", "function", "break", "var", "u32", "", "", "sreg", "", "", "", "virtual", "on", "super", "", "", "static", "", "", "throw", "interface", "in", "import", "", "u8", "", "", "", "", "", "", "", "auto", "", "union", "", "", "", "else", "", "null", "", "", "", "", "", "", "", "class", "", "", "return", "ureg", "", "true", "", "", "while", "", "aka", "finally", "", "", "", "bool", "protection", "", "", "", "false", "version", "", "", "", "", "", "enum", "s16", "", "", "", "f64", "", "s64", "", "f32", "", "s32", "", "", "", "struct", "", "", "", "", "", "", "", "", "", "", "assert", "", "", "", "", "s8", "", "const", "", "", "", "", "continue", "", "foreach", "", "switch", "", ""];

    static const ubyte[256] _coefficients = [167, 179, 6, 175, 81, 245, 144, 165, 84, 247, 161, 101, 43, 244, 4, 247, 230, 157, 102, 35, 18, 102, 42, 174, 159, 2, 212, 156, 126, 225, 149, 55, 190, 190, 58, 25, 62, 112, 33, 163, 218, 26, 201, 99, 45, 174, 216, 208, 231, 102, 161, 112, 236, 107, 161, 3, 37, 139, 246, 163, 64, 126, 23, 24, 168, 231, 157, 54, 85, 118, 176, 57, 41, 120, 248, 43, 235, 50, 18, 114, 35, 9, 15, 108, 45, 86, 213, 109, 105, 188, 201, 117, 192, 161, 91, 39, 151, 16, 131, 37, 211, 60, 75, 109, 121, 55, 149, 162, 232, 184, 101, 174, 121, 79, 24, 77, 239, 250, 98, 236, 88, 124, 115, 125, 153, 105, 203, 107, 232, 80, 89, 253, 186, 91, 87, 253, 131, 59, 56, 244, 183, 53, 41, 105, 192, 192, 90, 208, 142, 198, 172, 55, 203, 200, 209, 91, 34, 236, 138, 182, 136, 54, 141, 45, 51, 18, 97, 252, 69, 122, 245, 24, 163, 73, 83, 8, 201, 15, 71, 183, 205, 69, 32, 22, 207, 51, 11, 245, 26, 145, 41, 225, 24, 172, 109, 119, 174, 50, 25, 181, 174, 14, 229, 206, 224, 75, 44, 205, 196, 189, 198, 21, 19, 115, 17, 226, 178, 85, 120, 178, 158, 185, 225, 252, 218, 196, 105, 235, 182, 116, 44, 26, 150, 236, 132, 40, 244, 43, 14, 85, 83, 16, 96, 201, 150, 51, 239, 186, 191, 218, 33, 15, 220, 70, 159, 32];

    static string generateFilledTable()
    {
        import std.algorithm: countUntil;
        import std.conv: to;
        string result = "static const TokenType[128] _filled = [";
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
        return result % 128;
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
    assert(Keywords.isKeyword("else"));
}

alias Tokens = Token[];
alias TokensList = Tokens[];


/**
 * Tokens iterator.
 *
 * Params:
 *      TokenTypes = A list of $(D TokenType) to skip.
 */
struct TokenRange(TokenTypes...)
{

private:

    Tokens _tokens;

public:

    /// Constructs the ieterator from an array of $(D Token).
    this()(auto ref Tokens tokens)
    {
        _tokens = tokens;
    }

    /// Advances the iterator.
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

    /// Returns: The current token.
    ref const(Token) front() const
    {
        import std.range: front;
        return front(_tokens);
    }

    /// Indicates wether no tokens are available.
    bool empty() const
    {
        import std.range: empty;
        return this.front().type == TokenType.eof;
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

    /* The source is maintained as a storage for the token text so escapes
    in string literals need to be processed from the constant storage*/
    const(char)[] postProcessStringLiteral() const
    {
        char[] result = new char[](_length);
        size_t i, j;
        while (i < _length)
        {
            if (_start[i] == '\\')
            {
                ++i;
                switch(_start[i])
                {
                case '0':
                    result[j] = '\0';
                    ++i; ++ j;
                    continue;
                case 'a':
                    result[j] = '\a';
                    ++i; ++ j;
                    continue;
                case 'b':
                    result[j] = '\b';
                    ++i; ++ j;
                    continue;
                case 'f':
                    result[j] = '\f';
                    ++i; ++ j;
                    continue;
                case 'n':
                    result[j] = '\n';
                    ++i; ++ j;
                    continue;
                case 'r':
                    result[j] = '\r';
                    ++i; ++ j;
                    continue;
                case 't':
                    result[j] = '\t';
                    ++i; ++ j;
                    continue;
                case 'v':
                    result[j] = '\v';
                    ++i; ++ j;
                    continue;
                default:
                }
            }
            result[j] = _start[i];
            ++i; ++j;
            continue;
        }
        return result[0..j];
    }

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
    const(char[]) text() const
    {
        if (_type != TokenType.stringLiteral)
            return _start[0.._length];
        else
            return postProcessStringLiteral();
    }

    /// Returns: The token text, never post processed.
    const(char[]) rawText() const
    {
        return _start[0.._length];
    }

    /// Conveniance function used by the parser.
    bool isTokOperator() const
    {
        return firstOperator <= type && type <= lastOperator ||
            type == TokenType.in_;
    }

    /// Conveniance function used by the parser.
    bool isTokBasicType() const {return firstBasicType <= type && type <= lastBasicType;}

    /// Conveniance function used by the parser.
    bool isTokKeyword() const {return firstKeyword <= type && type <= lastKeyword;}

    /// Conveniance function used by the parser.
    bool isTokStorageClass() const {return isTokVar || isTokConst;}

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
    bool isTokOptAccess() const {return type == TokenType.optAccess;}

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
    bool isTokDivEqual() const {return type == TokenType.divEqual;}

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
    bool isTokMulEqual() const {return type == TokenType.mulEqual;}

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
    bool isTokMinusEqual() const {return type == TokenType.minusEqual;}

    /// Conveniance function used by the parser.
    bool isTokPlus() const {return type == TokenType.plus;}

    /// Conveniance function used by the parser.
    bool isTokPlusEqual() const {return type == TokenType.plusEqual;}

    /// Conveniance function used by the parser.
    bool isTokLeftShift() const {return type == TokenType.lShift;}

    /// Conveniance function used by the parser.
    bool isTokLeftShiftEqual() const {return type == TokenType.lshiftEqual;}

    /// Conveniance function used by the parser.
    bool isTokRightShift() const {return type == TokenType.rShift;}

    /// Conveniance function used by the parser.
    bool isTokRightShiftEqual() const {return type == TokenType.rshiftEqual;}

    /// Conveniance function used by the parser.
    bool isTokXor() const {return type == TokenType.xor;}

    /// Conveniance function used by the parser.
    bool isTokXorEqual() const {return type == TokenType.xorEqual;}

    /// Conveniance function used by the parser.
    bool isTokAt() const {return type == TokenType.at;}

    /// Conveniance function used by the parser.
    bool isTokEqualEqual() const {return type == TokenType.equalEqual;}

    /// Conveniance function used by the parser.
    bool isTokNotEqual() const {return type == TokenType.notEqual;}

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
    bool isTokAndAnd() const {return type == TokenType.andAnd;}

    /// Conveniance function used by the parser.
    bool isTokOrOr() const {return type == TokenType.orOr;}

    /// Conveniance function used by the parser.
    bool isTokBang() const {return type == TokenType.bang;}

    /// Conveniance function used by the parser.
    bool isTokIf() const {return type == TokenType.if_;}

    /// Conveniance function used by the parser.
    bool isTokWhile() const {return type == TokenType.while_;}

    /// Conveniance function used by the parser.
    bool isTokTidle() const {return type == TokenType.tidle;}

    /// Conveniance function used by the parser.
    bool isTokAmp() const {return type == TokenType.amp;}

    /// Conveniance function used by the parser.
    bool isTokAmpEqual() const {return type == TokenType.ampEqual;}

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
    bool isTokPipeEqual() const {return type == TokenType.pipeEqual;}

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
    bool isTokElse() const {return type == TokenType.else_;}

    /// Conveniance function used by the parser.
    bool isTokTrue() const {return type == TokenType.true_;}

    /// Conveniance function used by the parser.
    bool isTokFalse() const {return type == TokenType.false_;}

    /// Conveniance function used by the parser.
    bool isTokSuper() const {return type == TokenType.super_;}

    /// Conveniance function used by the parser.
    bool isTokOn() const {return type == TokenType.on;}

    /// Conveniance function used by the parser.
    bool isTokMod() const {return type == TokenType.mod;}

    /// Conveniance function used by the parser.
    bool isTokModEqual() const {return type == TokenType.modEqual;}

    /// Conveniance function used by the parser.
    bool isTokQmark() const {return type == TokenType.qmark;}

    /// Conveniance function used by the parser.
    bool isTokVersion() const {return type == TokenType.version_;}

    /// Conveniance function used by the parser.
    bool isTokTry() const {return type == TokenType.try_;}

    /// Conveniance function used by the parser.
    bool isTokFinally() const {return type == TokenType.finally_;}

    /// Conveniance function used by the parser.
    bool isTokThrow() const {return type == TokenType.throw_;}

    /// Conveniance function used by the parser.
    bool isTokUnion() const {return type == TokenType.union_;}

    /// Conveniance function used by the parser.
    bool isTokAssert() const {return type == TokenType.assert_;}

    /// Conveniance function used by the parser.
    bool isTokUnaryPrefix() const
    {
        return type == TokenType.plusPlus || type == TokenType.minusMinus ||
            type == TokenType.mul || type == TokenType.amp ||
            type == TokenType.bang || type == TokenType.tidle ||
            type == TokenType.plus || type == TokenType.minus;
    }

    /// Conveniance function used by the parser.
    bool isTokUnarySuffix() const
    {
        return type == TokenType.plusPlus || type == TokenType.minusMinus;
    }

    /// Conveniance function used by the parser.
    bool isTokPostfixStarter() const
    {
        return type == TokenType.plusPlus || type == TokenType.minusMinus ||
            type == TokenType.leftSquare || type == TokenType.leftParen ||
            type == TokenType.colon || type == TokenType.dot || type == TokenType.optAccess;
    }

    /// Conveniance function used by the parser.
    bool isTokNumberLiteral() const
    {
        return firstNumberLiteral <= type && type <= lastNumberLiteral;
    }

    /// Conveniance function used by the parser.
    bool isTokLiteral() const
    {
        return isTokNumberLiteral || isTokStringLiteral;
    }

    /// Conveniance function used by the parser.
    bool isTokValueKeyword() const
    {
        return type == TokenType.true_ || type == TokenType.false_ ||
            type == TokenType.null_;
    }

    /// Conveniance function used by the parser.
    bool isTokAssignOperator() const
    {
        return firstAssignOperator <= type && type <= lastAssignOperator;
    }
}

/**
 * Formats an array of Token pointers as text.
 *
 * Params:
 *      toks = The array to format.
 *      reversed = Indicates wether the array has to be read from the front or from the back.
 *      glue = The text put between each token.
 */
const(char)[] tokenChainText(Token*[] toks, bool reversed = false, string glue = ".")
{
    import std.array: join;
    import std.range: retro;
    import std.algorithm.iteration: map;

    if (reversed)
        return toks.retro.map!(a => a.text).join(glue);
    else
        return toks.map!(a => a.text).join(glue);
}
///
unittest
{
    char[] s0 = "one".dup, s1 = "two".dup;
    Token t0 = Token(s0.ptr, s0.ptr + s0.length, 0, 0, TokenType.identifier);
    Token t1 = Token(s1.ptr, s1.ptr + s1.length, 0, 0, TokenType.identifier);
    Token*[] toks = [&t0, &t1];
    assert(toks.tokenChainText(false) == "one.two");
    assert(toks.tokenChainText(true) == "two.one");
    assert([&t0].tokenChainText(false) == "one");
}

unittest
{
    char[] s = "test".dup;
    Token t = Token(s.ptr, s.ptr + s.length, 0, 0, TokenType.stringLiteral);
    assert(t.text == s);
}

unittest
{
    char[] s = `\"test\"`.dup;
    Token t = Token(s.ptr, s.ptr + s.length, 0, 0, TokenType.stringLiteral);
    assert(t.text == `"test"`);
}

unittest
{
    char[] s = `\"1\n2\"`.dup;
    Token t = Token(s.ptr, s.ptr + s.length, 0, 0, TokenType.stringLiteral);
    assert(t.text == "\"1\n2\"");
}

unittest
{
    char[] s = `\"1\r\n2\"`.dup;
    Token t = Token(s.ptr, s.ptr + s.length, 0, 0, TokenType.stringLiteral);
    assert(t.text == "\"1\r\n2\"");
}

unittest
{
    char[] s = `\0\a\b\f\r\n\t\v`.dup;
    Token t = Token(s.ptr, s.ptr + s.length, 0, 0, TokenType.stringLiteral);
    assert(t.text == "\0\a\b\f\r\n\t\v");
}

