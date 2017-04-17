module yatol.parser;

//TODO-cparser: check when advance() reaches the EOF.
//TODO-cparser: parseFunctionHeader, handle static.

import
    core.stdc.stdlib;
import
    std.stdio, std.format;
import
    yatol.lexer.types, yatol.lexer, yatol.parser.ast;

/// The parser
struct Parser
{

private:

    Lexer* _lexer;
    Token* _current;
    ptrdiff_t _declarationLevels;

    alias Range = TokenRange!(TokenType.lineComment, TokenType.starComment,
        TokenType.invalid);
    Range _range;

    void warning(const(char[]) message)
    {
        assert(_current);
        writefln("%s(%d,%d): warning, %s", _lexer.filename, _current.line,
            _current.column, message);
        stdout.flush;
    }

    void parseError(const(char[]) message)
    {
        assert(_current);
        writefln("%s(%d,%d): error, %s", _lexer.filename, _current.line,
            _current.column, message);
        stdout.flush;
    }

    void expected(TokenType expected)
    {
        static immutable string specifierDiff = "expected `%s` instead of `%s`";
        static immutable string specifierSame = "expected supplemental `%s`";
        if (current.type != expected)
            parseError(specifierDiff.format(tokenString(expected), _current.text));
        else
            parseError(specifierSame.format(tokenString(expected)));
    }

    void unexpected()
    {
        static immutable string specifier = "unexpected `%s`";
        parseError(specifier.format(_current.text));
    }

    bool advance()
    {
        bool result;
        if (!_range.empty())
        {
            _current = cast(Token*) &_range.front();
            _range.popFront();
            result = true;
        }
        return result;
    }

    pragma(inline, true)
    Token* current() {return _current;}

    pragma(inline, true)
    Token* lookup(size_t count)
    {
        return _current + count;
    }

private:

    /**
     * Parses a UnitDeclaration.
     *
     * Params:
     *      virtual = Indicates if the previous token is of type ($D TokenType.virtual).
     * Returns:
     *      $(D true) on success, $(D false) otherwise.
     */
    UnitAstNode parseUnit()
    {
        Token*[] toks;
        if (!current.isTokUnit)
        {
            expected(TokenType.unit);
            return null;
        }
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        toks ~= current;
        while (true)
        {
            advance();
            if (current.isTokSemicolon)
            {
                UnitAstNode result = new UnitAstNode;
                result.unitDeclaration = toks;
                if (parseDeclarations(result.declarations))
                {
                    --_declarationLevels;
                    return result;
                }
                else
                {
                    if (_declarationLevels > 1)
                        parseError("unclosed scope(s), class(es) or struct(s)");
                    return null;
                }
            }
            else
            {
                if (!current.isTokDot)
                {
                    expected(TokenType.dot);
                    return null;
                }
                else
                {
                    advance();
                    if (!current.isTokIdentifier)
                    {
                        expected(TokenType.identifier);
                        return null;
                    }
                    toks ~= current;
                }
            }
        }
    }

    /**
     * Parses an IdentifierChain.
     *
     * Returns:
     *      On success an array of $(D Token*) otherwise $(D null).
     */
    Token*[] parseIdentifierChain()
    {
        Token*[] result;
        while (true)
        {
            if (!current.isTokIdentifier)
            {
                expected(TokenType.identifier);
                return null;
            }
            result ~= current();
            advance();
            if (!current.isTokDot)
                return result;
            advance();
        }
    }

    /**
     * Parses consecutives TypeModifier.
     *
     * Returns:
     *      On success a $(D TypeModifierAstNode) otherwise $(D null).
     */
    TypeModifierAstNode parseTypeModifier()
    {
        TypeModifierAstNode result = new TypeModifierAstNode;
        if (current.isTokMul)
        {
            ++result.count;
            result.kind = ModifierKind.pointer;
            while (true)
            {
                advance();
                if (!current.isTokMul)
                {
                    if (current.isTokLeftSquare)
                        result.modifier = parseTypeModifier;
                    break;
                }
                ++result.count;
            }
        }
        else if (current.isTokLeftSquare)
        {
            ++result.count;
            result.kind = ModifierKind.array;
            while (true)
            {
                advance();
                if (!current.isTokRightSquare)
                {
                    expected(TokenType.rightSquare);
                    return null;
                }
                advance();
                if (!current.isTokLeftSquare)
                {
                    if (current.isTokMul)
                        result.modifier = parseTypeModifier;
                    break;
                }
                ++result.count;
            }
        }
        else if (current.isTokRightSquare)
        {
            unexpected;
            return null;
        }
        return result;
    }

    /**
     * Parses a Type.
     *
     * Returns:
     *      On success a $(D TypeAstNode) otherwise $(D null).
     */
    TypeAstNode parseType()
    {
        TypeAstNode result = new TypeAstNode;
        if (current.isTokBasicType)
        {
            result.basicOrQualifiedType ~= current();
            advance();
        }
        else if (current.isTokFunction || current.isTokStatic)
        {
            result.functionType = parseFunctionType();
            if (!result.functionType)
                parseError("invalid function type");
        }
        else
        {
            result.basicOrQualifiedType = parseIdentifierChain();
        }
        if (result.basicOrQualifiedType.length || result.functionType)
        {
            if (current.isTokMul || current.isTokLeftSquare)
            {
                if (TypeModifierAstNode mod = parseTypeModifier())
                    result.modifier = mod;
                else
                    return null;
            }
            return result;
        }
        else return null;
    }

    /**
     * Parses a TypedVariableList.
     *
     * Returns:
     *      On success a $(D TypedVariableListAstNode) otherwise $(D null).
     */
    TypedVariableListAstNode parseTypedVariableList()
    {
        TypedVariableListAstNode result;
        TypeAstNode type = parseType();
        if (!type && current.isTokIdentifier)
        {
            parseError("Type expected before parameter list");
            return null;
        }
        if (current.isTokIdentifier)
        {
            result = new TypedVariableListAstNode;
            while (true)
            {
                if (!current.isTokIdentifier)
                {
                    expected(TokenType.identifier);
                    return null;
                }
                result.variableList ~= current;
                advance();
                if (!current.isTokComma)
                    break;
                advance();
            }
        }
        result.type = type;
        return result;
    }

    /**
     * Parses a ClassDeclaration.
     *
     * Returns:
     *      On success a $(D ClassDeclarationAstNode) otherwise $(D null).
     */
    ClassDeclarationAstNode parseClassDeclaration()
    {
        if (!current.isTokClass)
        {
            expected(TokenType.class_);
            return null;
        }
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        ClassDeclarationAstNode result = new ClassDeclarationAstNode;
        result.name = current();
        advance();
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            destroy(result);
            return null;
        }
        parseDeclarations(result.declarations);
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
            destroy(result);
            return null;
        }
        return result;
    }

    /**
     * Parses a StructDeclaration.
     *
     * Returns:
     *      On success a $(D StructDeclarationAstNode) otherwise $(D null).
     */
    StructDeclarationAstNode parseStructDeclaration()
    {
        if (!current.isTokStruct)
        {
            expected(TokenType.struct_);
            return null;
        }
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        StructDeclarationAstNode result = new StructDeclarationAstNode;
        result.name = current();
        advance();
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            destroy(result);
            return null;
        }
        parseDeclarations(result.declarations);
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
            destroy(result);
            return null;
        }
        return result;
    }

    /**
     * Parses an ImportDeclaration.
     *
     * Returns:
     *      On success a $(D ImportDeclarationAstNode) otherwise $(D null).
     */
    ImportDeclarationAstNode parseImportDeclaration()
    {
        if (!current.isTokImport)
        {
            expected(TokenType.import_);
            return null;
        }
        advance();
        Token* priority;
        if (current.isTokLeftParen)
        {
            advance();
            priority = current();
            // literal
            advance();
            if (!current.isTokRightParen)
            {
                expected(TokenType.rightParen);
                return null;
            }
            advance();
        }
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        ImportDeclarationAstNode result = new ImportDeclarationAstNode;
        while (true)
        {
            Token*[] imp = parseIdentifierChain();
            if (!imp.length)
            {
                parseError("expected an identifier chain to identify an import");
                return null;
            }
            result.importList ~= imp;
            if (current.isTokSemicolon)
            {
                break;
            }
            else if (!current.isTokComma)
            {
                expected(TokenType.colon);
                return null;
            }
            advance();
        }
        return result;
    }

    /**
     * Parses an ScopeDeclaration.
     *
     * Returns:
     *      On success a $(D ScopeDeclarationAstNode) otherwise $(D null).
     */
    ScopeDeclarationAstNode parseScopeDeclaration()
    {
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            return null;
        }
        ScopeDeclarationAstNode result = new ScopeDeclarationAstNode;
        parseDeclarations(result.declarations);
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
            destroy(result);
            return null;
        }
        return result;
    }

    /**
     * Parses a FunctionType.
     *
     * Returns:
     *      On success a $(D FunctionTypeAstNode) otherwise $(D null).
     */
    FunctionTypeAstNode parseFunctionType()
    {
        const bool isStatic = current.isTokStatic;
        if (isStatic)
        {
            advance();
        }
        if (!current.isTokFunction)
        {
            expected(TokenType.function_);
            return null;
        }
        advance();
        if (!current.isTokMul)
        {
            expected(TokenType.mul);
            return null;
        }
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        FunctionTypeAstNode result = new FunctionTypeAstNode;
        result.isStatic = isStatic;
        if (!current.isTokRightParen) while (true)
        {
            if (TypedVariableListAstNode tvl = parseTypedVariableList())
            {
                result.parameters ~= tvl;
            if (!current.isTokSemicolon)
                break;
            }
            advance();
        }
        if (!current.isTokRightParen)
        {
            expected(TokenType.rightParen);
            return null;
        }
        advance();
        if (current.isTokColon)
        {
            advance();
            if (TypeAstNode returnType = parseType())
                result.returnType = returnType;
            else
            {
                parseError("expected function return type after colon");
                return null;
            }
        }
        return result;
    }

    /**
     * Parses a FunctionHeader.
     *
     * Returns:
     *      On success a $(D FunctionHeaderAstNode) otherwise $(D null).
     */
    FunctionHeaderAstNode parseFunctionHeader()
    {

        const bool isStatic = current.isTokStatic;
        if (isStatic)
        {
            advance();
        }
        if (!current.isTokFunction)
        {
            expected(TokenType.function_);
            return null;
        }
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        FunctionHeaderAstNode result = new FunctionHeaderAstNode;
        result.isStatic = isStatic;
        result.name = current();
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        if (!current.isTokRightParen) while (true)
        {
            if (TypedVariableListAstNode tvl = parseTypedVariableList())
            {
                result.parameters ~= tvl;
            if (!current.isTokSemicolon)
                break;
            }
            advance();
        }
        if (!current.isTokRightParen)
        {
            expected(TokenType.rightParen);
            return null;
        }
        advance();
        if (current.isTokColon)
        {
            advance();
            if (TypeAstNode returnType = parseType())
                result.returnType = returnType;
            else
            {
                parseError("expected function return type after colon");
                return null;
            }
        }
        return result;
    }

    /**
     * Parses a FunctionDeclaration.
     *
     * Returns:
     *      On success a $(D FunctionDeclarationAstNode) otherwise $(D null).
     */
    FunctionDeclarationAstNode parseFunctionDeclaration()
    {
        FunctionHeaderAstNode header = parseFunctionHeader();
        if (!header)
            return null;
        FunctionDeclarationAstNode result = new FunctionDeclarationAstNode;
        result.header = header;
        if (!current.isTokLeftCurly && !current.isTokSemicolon)
        {
            expected(TokenType.leftCurly);
            parseError("expected `;` or `{` as function body");
            return null;
        }
        result.firstBodyToken = current();
        if (current.isTokLeftCurly)
        {
            parseDeclarations(result.declarations);
            if (!current.isTokRightCurly)
            {
                expected(TokenType.rightCurly);
                destroy(result);
                return null;
            }
        }
        return result;
    }

    /**
     * Parses a ProtectionAttribute.
     *
     * Returns:
     *      A $(D ProtectionDeclarationAstNode) on success, $(D null) otherwise.
     */
    ProtectionDeclarationAstNode parseProtectionDeclaration()
    {
        if (!current.isTokProtection)
        {
            expected(TokenType.protection);
            return null;
        }
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        ProtectionDeclarationAstNode result = new ProtectionDeclarationAstNode;
        result.protection = current();
        advance();
        if (!current.isTokRightParen)
        {
            expected(TokenType.rightParen);
            destroy(result);
            return null;
        }
        return result;
    }

    /**
     * Parses contiguous declarations.
     *
     * Params:
     *      declarations = The array of $(D DeclarationAstNode) where the
     *      declarations are stored.
     *
     * Returns: $(D true) on success, $(D false= otherwise.
     */
    bool parseDeclarations(ref DeclarationAstNode[] declarations)
    {
        const ptrdiff_t oldDeclLvl = _declarationLevels;
        ++_declarationLevels;
        with(TokenType) while (advance()) switch(current.type)
        {
        case virtual:
        {
            if (_declarationLevels > 1)
            {
                unexpected();
                parseError("virtual units can only be declared after the main unit declarations");
                return false;
            }
            else
            {
                return true;
            }
        }
        case class_:
        {
            if (ClassDeclarationAstNode c = parseClassDeclaration())
            {
                DeclarationAstNode decl = new DeclarationAstNode;
                decl.classDeclaration = c;
                declarations ~= decl;
                break;
            }
            else return false;
        }
        case struct_:
        {
            if (StructDeclarationAstNode s = parseStructDeclaration())
            {
                DeclarationAstNode decl = new DeclarationAstNode;
                decl.structDeclaration = s;
                declarations ~= decl;
                break;
            }
            else return false;
        }
        case static_:
            if (lookup(1).isTokFunction)
            {
                goto case function_;
            }
            else
            {
                unexpected();
                return false;
            }
        case function_:
        {
            if (FunctionDeclarationAstNode f = parseFunctionDeclaration())
            {
                DeclarationAstNode decl = new DeclarationAstNode;
                decl.functionDeclaration = f;
                declarations ~= decl;
                break;
            }
            else return false;
        }
        case import_:
        {
            if (ImportDeclarationAstNode i = parseImportDeclaration())
            {
                DeclarationAstNode decl = new DeclarationAstNode;
                decl.importDeclaration = i;
                declarations ~= decl;
                break;
            }
            else return false;
        }
        case protection:
        {
            if (ProtectionDeclarationAstNode p = parseProtectionDeclaration())
            {
                DeclarationAstNode decl = new DeclarationAstNode;
                decl.protectionOverwrite = p;
                declarations ~= decl;
                break;
            }
            else return false;
        }
        case leftCurly:
        {
            if (ScopeDeclarationAstNode s = parseScopeDeclaration())
            {
                DeclarationAstNode decl = new DeclarationAstNode;
                decl.scopeDeclaration = s;
                declarations ~= decl;
                break;
            }
            else return false;
        }
        case rightCurly:
        {
            --_declarationLevels;
            if (_declarationLevels <= 0)
            {
                unexpected();
                return false;
            }
            else if (oldDeclLvl != _declarationLevels)
            {
                //parseError("unclosed scope(s), class(es) or struct(s)");
                return false;
            }
            else return true;
        }
        default:
            unexpected();
            return false;
        }
        return true;
    }

public:

    ///
    @disable this();

    /**
     * Constructs the parse with the lexer that contains the tokens to parse.
     */
    this(Lexer* lexer)
    {
        if (!lexer)
        {
            stderr.writeln("INTERNAL ERROR: attempt to create a parser without lexer");
            exit(1);
        }
        _lexer = lexer;
        _range = Range(lexer.tokens);
        advance();
    }

    /**
     * Main parser function. The function trie to parse from the main unit to
     * the last virtual unit (if any).
     *
     * Returns: An $(D UnitContainerAstNode) on success, $(D null) otherwise.
     */
    UnitContainerAstNode parse()
    {
        if (!current)
            return null;
        if (!current.isTokUnit)
        {
            expected(TokenType.unit);
            return null;
        }
        else
        {
            UnitContainerAstNode result = new UnitContainerAstNode;
            result.mainUnit = parseUnit();
            if (!result.mainUnit)
            {
                return null;
            }
            while (current.isTokVirtual)
            {
                advance();
                result.virtualUnits ~= parseUnit;
                if (!result.virtualUnits[$-1])
                    return null;
            }
            if (!_range.empty)
            {
                unexpected;
                return null;
            }
            return result;
        }
    }

    /**
     * For testing prurpose, tries to parse a particular AST node.
     *
     * Params:
     *      T = The AstNode type to parse.
     *      a = The parameters for the sub parser. Usually nothing, an
     *          array of statements or an array of declarations.
     * Returns:
     *      A $(D T) instance or $(D true) on success, $(D null) or $(D false)
     *      otherwise.
     */
    T parseCustomNode(T, A...)(auto ref A a)
    {
        import std.string: endsWith;
        static assert(T.stringof.endsWith("AstNode"), "expected an AstNode");
        enum fun = T.stringof[0..T.stringof.length - "AstNode".length];
        mixin( "return parse" ~ fun ~ "(a[0..$]);");
    }
}

unittest
{
    enum line = __LINE__;
    enum source = `
    s8**[]`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE__, line + 1, 1);
    lx.lex;
    Parser prs = Parser(&lx);
    TypeAstNode tan = prs.parseCustomNode!TypeAstNode;
    assert(tan);
}

unittest
{
    enum line = __LINE__;
    enum source = `
    ][]`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE__, line + 1, 1);
    lx.lex;
    Parser prs = Parser(&lx);
    TypeModifierAstNode tman = prs.parseCustomNode!TypeModifierAstNode;
    assert(!tman);
}

unittest
{
    enum line = __LINE__;
    enum source = `
    :s8**[]`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE__, line + 1, 1);
    lx.lex;
    Parser prs = Parser(&lx);
    TypeAstNode tan = prs.parseCustomNode!TypeAstNode;
    assert(!tan);
}

unittest
{
    import yatol.parser.debug_visitor;
    enum line = __LINE__;
    enum source = `
    unit a;
    protection(private)
        function ant(s8 p1,p2; s16 p3,p4): s8***[];
        function bee(Rat p1,p2; a.Cow p3,p4): s8* {}
    protection(public)
        struct Cat {}
        struct Rat {}
    protection(public)
        class Bat {}
        class Cow { class Fox {} }
    virtual unit b;
    function bee(s32[] p1): function*(): static function*(): s8[]
    {
        // function that returns a function that returns an s8 array
    }
    protection(public) import a.b, c.d;
    virtual unit c;
    struct Owl
    {
        function of1(): s32;
        function of2(): u32;
    }
`;

    Lexer lx;
    lx.setSourceFromText(source, __FILE__, line + 1, 1);
    lx.lex;

    Parser pr = Parser(&lx);
    DebugVisitor dv = new DebugVisitor(pr.parse);
    dv.printText();
}

