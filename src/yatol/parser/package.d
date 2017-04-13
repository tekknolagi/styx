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

    bool advance()
    {
        bool result;
        if (!_range.empty())
        {
            _current = cast(Token*) _range.front();
            _range.popFront();
            result = true;
        }
        return result;
    }

    Token* current() {return _current;}

    Token* lookup(size_t count = 1) {return _current + count;}

    void expected(TokenType expected)
    {
        static immutable string specifier = "expected `%s` instead of `%s`";
        parseError(specifier.format(tokenString(expected), tokenString(_current.type)));
    }

    void unexpected()
    {
        static immutable string specifier = "unexpected `%s`";
        parseError(specifier.format(tokenString(_current.type)));
    }

private:

    /**
     * Parses a UnitDeclaration. ($D virtual) indicates if the previous token
     * was of type ($D TokenType.virtual)
     */
    void parseUnitDeclaration(bool virtual)
    {
        const(Token*)[] toks;
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

    /// Parses a ClassDeclaration
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
        parseDeclarations(result.declarations);
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
            destroy(result);
            return null;
        }
        return result;
    }

    /// Parses a structDeclaration
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
        case virtual:
            parseUnitDeclaration(true);
            break;
        case class_:
            advance();
            parseClassDeclaration;
            break;
        case struct_:
            advance();
            parseStructDeclaration;
            break;
        case protection:
            if (Token* id = parseProtectionAttribute())
            {
                if (prot)
                {
                    warning("previous protection attribute is not used");
                    destroy(prot);
                    prot = null;
                }
                advance();
                if (current.isTokColon)
                {
                    declarations ~= new DeclarationAstNode;
                    declarations[$-1].protectionOverwrite = new ProtectionOverwriteAstNode(id);
                    break;
                }
                else
                {
                    prot = new ProtectionAttributeAstNode(id);
                    goto L1;
                }
            }
            break L0;
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
            stderr.writeln("INTERNAL ERROR: attempt to create a parser without a lexer");
            exit(1);
        }
        _lexer = lexer;
        _range = Range(lexer.tokens);
        _unitContainer = new UnitContainerAstNode;
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
    }

    UnitContainerAstNode unitContainer()
    {
        return _unitContainer;
    }
}

