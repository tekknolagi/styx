module yatol.parser;

//TODO-cparser: check when advance() reaches the EOF.
//TODO-cparser: parseFunctionHeader, handle static.

import
    core.stdc.stdlib;
import
    std.stdio, std.format;
import
    yatol.lexer.types, yatol.lexer, yatol.parser.ast;

struct Parser
{

private:

    Lexer* _lexer;
    Token* _current;
    UnitContainerAstNode _unitContainer;
    ProtectionAttributeAstNode[] _protStack;
    char[] _firstProtection = "public:".dup;

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

    Token* current() {return _current;}

    Token* lookup(size_t count)
    {
        return _current + count;
    }

    ProtectionAttributeAstNode pushProtectionStack(Token* identifier)
    {
        assert(identifier);
        ProtectionAttributeAstNode result = new ProtectionAttributeAstNode;
        result.protection = identifier;
        _protStack ~= result;
        return result;
    }

    ProtectionAttributeAstNode pushProtectionStackWithCurrent()
    {
        ProtectionAttributeAstNode result = new ProtectionAttributeAstNode;
        assert(currentProtection.protection);
        result.protection = currentProtection.protection;
        _protStack ~= result;
        return result;
    }

    ProtectionAttributeAstNode overwriteProtectionStack(Token* identifier)
    {
        assert(identifier);
        ProtectionAttributeAstNode result = new ProtectionAttributeAstNode;
        result.protection = identifier;
        _protStack[$-1] = result;
        return result;
    }

    void popProtectionStack()
    {
        if (_protStack.length == 1)
            unexpected();
        else
            _protStack.length -= 1;
    }

    ProtectionAttributeAstNode currentProtection()
    {
        ProtectionAttributeAstNode result = _protStack[$-1];
        assert(result);
        assert(result.protection);
        return result;
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
    bool parseUnitDeclaration(bool virtual)
    {
        Token*[] toks;
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return false;
        }
        toks ~= current;
        while (true)
        {
            advance();
            if (current.isTokSemicolon)
            {
                UnitAstNode node = new UnitAstNode;
                node.unitDeclaration = toks.dup;
                if (!virtual)
                    _unitContainer.mainUnit = node;
                else
                    _unitContainer.virtualUnits ~= node;
                return parseDeclarations(node.declarations);
            }
            else
            {
                if (!current.isTokDot)
                {
                    expected(TokenType.dot);
                    return false;
                }
                else
                {
                    advance();
                    if (!current.isTokIdentifier)
                    {
                        expected(TokenType.identifier);
                        return false;
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
        Token*[] identifiers;
        if (current.isTokBasicType)
        {
            identifiers ~= current();
            advance();
        }
        else identifiers = parseIdentifierChain();
        if (identifiers.length)
        {
            ptrdiff_t indr;
            TypeAstNode result = new TypeAstNode;
            result.type = identifiers;
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
        pushProtectionStackWithCurrent();
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
        pushProtectionStackWithCurrent();
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
     * Parses a FunctionHeader.
     *
     * Returns:
     *      On success a $(D FunctionHeaderAstNode) otherwise $(D null).
     */
    FunctionHeaderAstNode parseFunctionHeader()
    {
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        FunctionHeaderAstNode result = new FunctionHeaderAstNode;
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
        if (!current.isTokColon)
        {
            expected(TokenType.colon);
            return null;
        }
        advance();
        if (TypeAstNode returnType = parseType())
            result.returnType = returnType;
        else
        {
            parseError("expected function return type after colon");
            return null;
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
            pushProtectionStackWithCurrent();
            parseDeclarations(result.declarations);
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
        ProtectionAttributeAstNode prot;
        with(TokenType) while (advance()) switch(current.type)
        {
        case leftCurly:
        {
            ScopeAstNode sc = new ScopeAstNode;
            DeclarationAstNode decl = new DeclarationAstNode;
            decl.scopeDeclaration = sc;
            if (!prot)
            {
                prot = new ProtectionAttributeAstNode;
                prot.protection = currentProtection.protection;
            }
            sc.protectionAttribute = pushProtectionStack(prot.protection);
            prot = null;
            declarations ~= new DeclarationAstNode;
            declarations[$-1].scopeDeclaration = sc;
            parseDeclarations(sc.declarations);
            if (_protStack.length > 1 && current.type != TokenType.rightCurly)
                expected(TokenType.rightCurly);
            break;
        }
        case rightCurly:
            popProtectionStack();
            return true;
        case virtual:
            advance();
            if (current.isTokUnit)
            {
                return parseUnitDeclaration(true);
            }
            else
            {
                unexpected();
                return false;
            }
        case class_:
            advance();
            ClassDeclarationAstNode classDecl = parseClassDeclaration();
            if (classDecl)
            {
                if (prot)
                {
                    classDecl.protectionAttribute = prot;
                    prot = null;
                }
                DeclarationAstNode decl = new DeclarationAstNode;
                decl.classDeclaration = classDecl;
                declarations ~= decl;
            }
            else return false;
            break;
        case struct_:
            advance();
            StructDeclarationAstNode structDecl = parseStructDeclaration();
            if (structDecl)
            {
                if (prot)
                {
                    structDecl.protectionAttribute = prot;
                    prot = null;
                }
                DeclarationAstNode decl = new DeclarationAstNode;
                decl.structDeclaration = structDecl;
                declarations ~= decl;
                break;
            }
            else return false;
        case function_:
            advance();
            FunctionDeclarationAstNode functionDecl = parseFunctionDeclaration();
            if (functionDecl)
            {
                if (prot)
                {
                    functionDecl.header.protectionAttribute = prot;
                    prot = null;
                }
                DeclarationAstNode decl = new DeclarationAstNode;
                decl.functionDeclaration = functionDecl;
                declarations ~= decl;
                break;
            }
            else return false;
        case protection:
        {
            if (Token* id = parseProtectionAttribute())
            {
                if (prot)
                {
                    warning("previous protection attribute is not used");
                    destroy(prot);
                    prot = null;
                }
                if (lookup(1).isTokColon)
                {
                    advance();
                    ProtectionAttributeAstNode pa = overwriteProtectionStack(id);
                    DeclarationAstNode decl = new DeclarationAstNode;
                    decl.protectionOverwrite = new ProtectionOverwriteAstNode;
                    decl.protectionOverwrite.protectionAttribute = pa;
                    declarations ~= decl;
                    break;
                }
                else
                {
                    prot = new ProtectionAttributeAstNode;
                    prot.protection = id;
                    break;
                }
            }
            else return false;
        }
        default:
            unexpected();
            return false;
        }
        return true;
    }

    /**
     * Parses a ProtectionAttribute.
     *
     * Returns:
     *      On success a pointer to the token that indicates the protection,
     *      $(D null) otherwise.
     */
    Token* parseProtectionAttribute()
    {
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
        Token* result = current;
        advance();
        if (!current.isTokRightParen)
        {
            expected(TokenType.rightParen);
            return null;
        }
        return result;
    }

public:

    ///
    @disable this();

    /**
     * Constructs the parse with the lexer that contains the tokens to parse.
     */
    this()(Lexer* lexer)
    {
        if (!lexer)
        {
            stderr.writeln("INTERNAL ERROR: attempt to create a parser without lexer");
            exit(1);
        }
        _lexer = lexer;
        _range = Range(lexer.tokens);
        _unitContainer = new UnitContainerAstNode;
        pushProtectionStack(new Token(_firstProtection.ptr, _firstProtection.ptr + 6,
            1, 1, TokenType.identifier));
        advance();
    }

    /**
     * Main parser function. The function trie to parse from the main unit to
     * the last virtual unit (if any).
     *
     * Returns: An $(D UnitContainerAstNode) on suceess, $(D null) otherwise.
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
            if (parseUnitDeclaration(false))
                return _unitContainer;
        }
        if (_protStack.length > 1)
        {
            expected(TokenType.rightCurly);
            parseError("there are %s unclosed scope(s), struct(s) or class(es)"
                .format(_protStack.length-1));
        }
        return null;
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
    function ant(s8 p1,p2; s16 p3,p4): s8***[] {}
    function bee(Rat p1,p2; a.Cow p3,p4): s8* {}
    protection(private):
    protection(public)
    {
        protection(public) struct Cat {}
    }
    struct Rat {}
    protection(public):
    class Bat {}
    class Cow { class Fox{  } }
    virtual unit b;
    function bee(s32[] p1): s32 {}
    `;

    Lexer lx;
    lx.setSourceFromText(source, __FILE__, line + 1, 1);
    lx.lex;

    Parser pr = Parser(&lx);
    DebugVisitor dv = new DebugVisitor(pr.parse);
    dv.printText();
}

