module yatol.parser;

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

    alias Range = TokenRange!(TokenType.lineComment, TokenType.invalid);
    Range _range;

    void warning(const(char[]) message)
    {
        assert(_current);
        writefln("%s(%d,%d): warning, %s", _lexer.filename, _current.line,
            _current.column, message);
    }

    void parseError(const(char[]) message)
    {
        assert(_current);
        writefln("%s(%d,%d): error, %s", _lexer.filename, _current.line,
            _current.column, message);
    }

    void expected(TokenType expected)
    {
        static immutable string specifier = "expected `%s` instead of `%s`";
        parseError(specifier.format(tokenString(expected), _current.text));
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

    Token* lookup(size_t count = 1)
    {
        return _current + count;
    }

    ProtectionAttributeAstNode pushProtectionStack(Token* identifier)
    {
        ProtectionAttributeAstNode result = new ProtectionAttributeAstNode(identifier);
        _protStack ~= result;
        return result;
    }

    ProtectionAttributeAstNode pushProtectionStackWithCurrent()
    {
        ProtectionAttributeAstNode result =
            new ProtectionAttributeAstNode(currentProtection.protection);
        _protStack ~= result;
        return result;
    }

    ProtectionAttributeAstNode overwriteProtectionStack(Token* identifier)
    {
        ProtectionAttributeAstNode result = new ProtectionAttributeAstNode(identifier);
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
        return _protStack[$-1];
    }

private:

    /**
     * Parses a UnitDeclaration. ($D virtual) indicates if the previous token
     * was of type ($D TokenType.virtual)
     */
    void parseUnitDeclaration(bool virtual)
    {
        Token*[] toks;
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return;
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
                parseDeclarations(node.declarations);
                return;
            }
            else
            {
                if (!current.isTokDot)
                {
                    expected(TokenType.dot);
                    return;
                }
                else
                {
                    advance();
                    if (!current.isTokIdentifier)
                    {
                        expected(TokenType.identifier);
                        return;
                    }
                    toks ~= current;
                }
            }
        }
    }

    /// Parses a ClassDeclaration.
    ClassDeclarationAstNode parseClassDeclaration()
    {
        ClassDeclarationAstNode result = new ClassDeclarationAstNode;
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

    /// Parses a StructDeclaration.
    StructDeclarationAstNode parseStructDeclaration()
    {
        StructDeclarationAstNode result = new StructDeclarationAstNode;
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
     * Parses contiguous declarations.
     */
    void parseDeclarations(ref DeclarationAstNode[] declarations)
    {
        ProtectionAttributeAstNode prot;
        with(TokenType) L0: while (advance()) switch(current.type)
        {
        L1:
        case leftCurly:
        {
            ScopeAstNode sc = new ScopeAstNode;
            DeclarationAstNode decl = new DeclarationAstNode;
            decl.scopeDeclaration = sc;
            if (!prot)
                prot = new ProtectionAttributeAstNode(currentProtection.protection);
            sc.protection = pushProtectionStack(prot.protection);
            destroy(prot);
            prot = null;
            declarations ~= sc;
            advance();
            parseDeclarations(sc.declarations);
            if (_protStack.length > 1 && current.type != TokenType.rightCurly)
                expected(TokenType.rightCurly);
            break;
        }
        case rightCurly:
            popProtectionStack();
            break L0;
        case virtual:
            parseUnitDeclaration(true);
            break;
        case class_:
            advance();
            ClassDeclarationAstNode classDecl = parseClassDeclaration;
            if (prot)
            {
                classDecl.protection = prot;
                destroy(prot);
                prot = null;
            }
            if (classDecl)
            {
                DeclarationAstNode decl = new DeclarationAstNode;
                decl.classDeclaration = classDecl;
                declarations ~= decl;
            }
            break;
        case struct_:
            advance();
            StructDeclarationAstNode structDecl = parseStructDeclaration;
            if (prot)
            {
                structDecl.protection = prot;
                destroy(prot);
                prot = null;
            }
            if (structDecl)
            {
                DeclarationAstNode decl = new DeclarationAstNode;
                decl.structDeclaration = structDecl;
                declarations ~= decl;
            }
            break;
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
                    decl.protectionOverwrite.protection = pa;
                    declarations ~= decl;
                    break;
                }
                else
                {
                    prot = new ProtectionAttributeAstNode(id);
                }
            }
            break;
        }
        default:
            break;
        }
    }

    /**
     * Parses a ProtectionAttribute.
     * Returns: The identifier that specifies the protection.
     */
    Token* parseProtectionAttribute()
    {
        Token* result;
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return result;
        }
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return result;
        }
        else result = current;
        advance();
        if (!current.isTokRightParen)
        {
            expected(TokenType.rightParen);
            return result;
        }
        return result;
    }

public:

    @disable this();

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
        pushProtectionStack(new Token(_firstProtection.ptr, _firstProtection.ptr + 5,
            1, 1, TokenType.identifier));
        advance();
    }

    void parseMainUnit()
    {
        if (!current)
            return;
        if (!current.isTokUnit)
        {
            expected(TokenType.unit);
            return;
        }
        else parseUnitDeclaration(false);
        if (_protStack.length > 1)
        {
            parseError("there are %s unclosed scope(s), struct(s) or class(es)"
                .format(_protStack.length-1));
        }
    }

    UnitContainerAstNode unitContainer()
    {
        return _unitContainer;
    }
}

unittest
{
    import yatol.parser.debug_visitor;
    enum line = __LINE__;
    enum source = `
    unit a;
    protection(private):
    protection(public)
    {
        protection(public) struct Cat {}
    }
    struct Rat {
    protection(public):
    class Bat {};
    class Cow {}`;

    Lexer lx;
    lx.setSourceFromText(source, line + 1, 4);
    lx.lex;

    Parser pr = Parser(&lx);
    pr.parseMainUnit;

    DebugVisitor dv = new DebugVisitor(pr.unitContainer);
    dv.printText();
}

