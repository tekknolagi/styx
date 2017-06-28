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
    bool_,
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

/**
 * Maps a $(D TokenType) to a string, between parens when the
 * token string is variable.
 */
static immutable string[TokenType.max + 1] tokenStringTable =
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
    "bool",
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
static immutable ptrdiff_t firstBasicType = TokenType.bool_;
/// The value of the last keyword.
static immutable ptrdiff_t lastBasicType = TokenType.ureg;

/// The value of the first symbol.
static immutable firstSymbol = TokenType.bang;
/// The value of the last symbol.
static immutable lastSymbol = TokenType.ellipsis;

/// The value of the first operator.
static immutable firstOperator = TokenType.equalEqual;
/// The value of the last operator.
static immutable lastOperator = TokenType.xor;

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

    static const string[64] _words = ["u64", "s64", "struct", "else", "while", "f64", "continue", "ureg", "sreg", "return", "", "auto", "in", "null", "", "", "protection", "u32", "s32", "", "", "", "f32", "", "virtual", "", "aka", "", "enum", "", "bool", "foreach", "import", "", "function", "static", "", "u8", "s8", "break", "const", "class", "var", "interface", "", "", "switch", "", "", "", "", "u16", "s16", "", "", "", "is", "on", "", "unit", "if", "", "", ""];

    static const ubyte[256] _coefficients = [131, 108, 240, 186, 160, 241, 211, 48, 26, 126, 116, 112, 156, 225, 3, 163, 252, 133, 194, 129, 22, 27, 113, 59, 22, 7, 188, 248, 174, 186, 21, 51, 252, 203, 254, 130, 152, 16, 80, 187, 137, 223, 213, 42, 145, 149, 154, 141, 141, 222, 75, 156, 171, 31, 235, 186, 59, 180, 151, 245, 31, 100, 253, 88, 48, 108, 144, 226, 74, 77, 100, 235, 124, 52, 124, 236, 158, 27, 209, 215, 252, 74, 25, 60, 65, 146, 54, 16, 146, 246, 72, 4, 167, 200, 138, 152, 93, 226, 248, 255, 0, 19, 239, 230, 94, 141, 246, 214, 242, 32, 63, 250, 80, 51, 36, 43, 69, 170, 100, 212, 245, 93, 85, 69, 84, 141, 189, 240, 210, 78, 13, 53, 106, 107, 49, 116, 249, 125, 92, 3, 115, 214, 125, 166, 202, 130, 170, 65, 41, 15, 50, 65, 43, 95, 35, 40, 155, 95, 107, 47, 217, 181, 184, 42, 162, 66, 98, 148, 203, 136, 39, 51, 188, 14, 37, 118, 52, 235, 12, 223, 120, 138, 124, 185, 96, 184, 251, 70, 46, 78, 175, 157, 200, 86, 18, 91, 204, 6, 199, 84, 119, 150, 2, 14, 95, 132, 145, 81, 168, 31, 206, 139, 131, 134, 222, 236, 233, 112, 254, 204, 237, 206, 135, 72, 152, 128, 221, 172, 38, 196, 174, 53, 38, 36, 130, 111, 125, 17, 15, 138, 98, 169, 171, 133, 157, 47, 221, 240, 210, 44, 201, 123, 1, 255, 103, 255];

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
    assert(Keywords.isKeyword("else"));
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
    ref const(Token) front() const
    {
        import std.range: front;
        return front(_tokens);
    }

    ///
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
    bool isTokOperator() const {return firstOperator <= type && type <= lastOperator;}

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
    bool isTokLeftShift() const {return type == TokenType.lShift;}

    /// Conveniance function used by the parser.
    bool isTokRightShift() const {return type == TokenType.rShift;}

    /// Conveniance function used by the parser.
    bool isTokXor() const {return type == TokenType.xor;}

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
    bool isTokElse() const {return type == TokenType.else_;}

    /// Conveniance function used by the parser.
    bool isUnaryPrefix() const
    {
        return type == TokenType.plusPlus || type == TokenType.minusMinus ||
            type == TokenType.mul || type == TokenType.amp || type == TokenType.bang;
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

