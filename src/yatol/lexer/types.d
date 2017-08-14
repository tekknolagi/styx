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

/// The $(D TokenType) of the first symbol.
static immutable TokenType firstSymbol = TokenType.bang;
/// The $(D TokenType) of the last symbol.
static immutable TokenType lastSymbol = TokenType.ellipsis;

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
        rendered on 2017-Aug-14 19:16:33.5916097 by IsItThere.
         - PRNG seed: 6574
         - map length: 128
         - case sensitive: true
    */

    static const string[128] _words = ["", "protection", "", "f64", "", "s8", "class", "", "finally", "", "try", "", "false", "if", "", "", "", "", "", "", "break", "u64", "", "", "", "", "", "", "", "", "static", "", "return", "", "", "", "", "", "in", "s64", "", "", "", "", "f32", "", "virtual", "", "aka", "is", "", "version", "", "", "", "", "continue", "", "", "", "", "", "u32", "", "", "", "", "var", "", "", "", "u16", "", "", "const", "true", "", "", "", "switch", "s32", "enum", "unit", "", "", "function", "import", "", "", "s16", "foreach", "", "on", "", "", "struct", "interface", "", "", "", "while", "", "", "", "ureg", "", "", "null", "", "", "", "auto", "", "super", "", "u8", "", "", "", "", "else", "", "sreg", "", "", "bool", "", "throw"];

    static const ubyte[256] _coefficients = [1, 51, 40, 67, 132, 162, 139, 224, 156, 25, 106, 185, 63, 242, 164, 108, 243, 222, 63, 40, 46, 150, 66, 186, 0, 173, 223, 187, 238, 71, 63, 26, 147, 102, 81, 239, 149, 25, 125, 91, 77, 43, 4, 167, 213, 128, 175, 18, 29, 61, 84, 225, 11, 104, 129, 35, 234, 254, 202, 114, 81, 117, 255, 211, 46, 229, 216, 211, 79, 242, 248, 20, 176, 85, 165, 177, 30, 6, 160, 39, 235, 69, 137, 254, 131, 6, 240, 6, 41, 165, 37, 54, 86, 62, 150, 144, 110, 247, 188, 48, 152, 90, 119, 64, 81, 22, 137, 66, 169, 94, 144, 76, 174, 16, 197, 155, 35, 9, 135, 122, 185, 34, 92, 187, 83, 164, 181, 158, 87, 54, 8, 246, 161, 243, 103, 103, 25, 105, 137, 219, 172, 79, 41, 121, 67, 231, 144, 50, 76, 171, 116, 239, 239, 239, 144, 131, 117, 179, 245, 15, 76, 6, 16, 86, 126, 176, 223, 138, 221, 152, 225, 2, 122, 118, 222, 50, 6, 248, 225, 139, 132, 113, 128, 158, 183, 190, 226, 75, 98, 56, 188, 17, 115, 121, 99, 228, 193, 200, 184, 170, 246, 135, 51, 120, 6, 165, 251, 222, 227, 165, 150, 202, 99, 86, 179, 100, 219, 196, 93, 15, 40, 167, 31, 174, 53, 168, 153, 166, 13, 185, 185, 109, 155, 173, 246, 233, 69, 35, 179, 196, 136, 149, 253, 70, 199, 237, 18, 240, 115, 106, 173, 8, 4, 11, 157, 1];

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
            switch (_start[i])
            {
            case '\\':
                ++i;
                if (_start[i] == 'r')
                {
                    result[j] = '\r';
                    ++i; ++ j;
                    continue;
                }
                else if (_start[i] == 'n')
                {
                    result[j] = '\n';
                    ++i; ++ j;
                    continue;
                }
                else goto default;
            default:
                result[j] = _start[i];
                ++i; ++j;
                continue;
            }
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
    bool isTokOperator() const {return firstOperator <= type && type <= lastOperator;}

    /// Conveniance function used by the parser.
    bool isTokBasicType() const {return firstBasicType <= type && type <= lastBasicType;}

    /// Conveniance function used by the parser.
    bool isTokKeyword() const {return firstKeyword <= type && type <= lastKeyword;}

    /// Conveniance function used by the parser.
    bool isTokSymbol() const {return firstSymbol <= type && type <= lastSymbol;}

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
    bool isTokUnaryPrefix() const
    {
        return type == TokenType.plusPlus || type == TokenType.minusMinus ||
            type == TokenType.mul || type == TokenType.amp || type == TokenType.bang;
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
            type == TokenType.colon || type == TokenType.dot || type == TokenType.qmark;
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

