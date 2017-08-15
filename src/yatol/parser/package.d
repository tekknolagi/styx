#!runnable: -g -gs
/**
 * YATOL parser.
 *
 * to maintain unittest coverage: 100% - 1LOC
 **/
module yatol.parser;

import
    core.stdc.stdlib;
import
    std.stdio, std.format, std.algorithm;
import
    yatol.lexer.types, yatol.lexer, yatol.parser.ast;

/// The parser
struct Parser
{

private:

    Lexer* _lexer;
    Token* _current;
    ptrdiff_t _declarationLevels;
    size_t _errorCount;
    UnitContainerAstNode _uc;

    alias Range = TokenRange!(TokenType.lineComment, TokenType.starComment);
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
        ++_errorCount;
        writefln("%s(%d,%d): error, %s", _lexer.filename, _current.line,
            _current.column, message);
        stdout.flush;
    }

    void expected(TokenType expected, string loc = __FUNCTION__, int line = __LINE__)
    {
        writeln(loc, " ", line);
        ++_errorCount;
        static immutable string specifierDiff = "expected `%s` instead of `%s`";
        static immutable string specifierSame = "expected supplemental `%s`";
        if (current.type != expected)
            parseError(specifierDiff.format(tokenString(expected), _current.text));
        else
            parseError(specifierSame.format(tokenString(expected)));
    }

    void unexpected(string loc = __FUNCTION__, int line = __LINE__)
    {
        writeln(loc, " ", line);
        ++_errorCount;
        static immutable string specifier = "unexpected `%s`";
        parseError(specifier.format(_current.text));
    }

    void advance()
    {
        _current = cast(Token*) &_range.front();
        _range.popFront();
    }

    pragma(inline, true)
    Token* current() {return _current;}

    pragma(inline, true)
    Token* lookupNext()
    {
        return _current + 1;
    }

    /**
     * Parses a UnitDeclaration.
     *
     * Params:
     *      virtual = Indicates if the previous token is of type $(D TokenType.virtual).
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
                result.position = current.position;
                result.unitDeclaration = toks;
                advance();
                if (parseDeclarations(result.declarations))
                {
                    --_declarationLevels;
                    if (_declarationLevels < 0)
                    {
                        unexpected();
                        return null;
                    }
                    else return result;
                }
                else
                {
                    if (_declarationLevels > 1)
                        parseError("unclosed scope(s), class(es), struct(s) or interface(s)");
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
     * Parses an IdentifierChains.
     *
     * Returns:
     *      On success an array of $(D Token*[]) otherwise $(D null).
     */
    IdentifierChainsAstNode parseIdentifierChains()
    {
        Token*[][] chains;
        while (true)
        {
            if (!current.isTokIdentifier)
            {
                expected(TokenType.identifier);
                return null;
            }
            Token*[] chain = parseIdentifierChain();
            if (chain.length)
                chains ~= chain;
            else
                return null;
            if (current.isTokComma)
                advance();
            else
                break;
        }
        IdentifierChainsAstNode result = new IdentifierChainsAstNode;
        result.chains = chains;
        return result;
    }

    InitializerAstNode parseInitializer()
    {
        InitializerAstNode result = new InitializerAstNode;
        result.position = current.position();
        if (current.isTokLeftSquare)
        {
            advance();
            while (true)
            {
                if (current.isTokRightSquare)
                {
                    break;
                }
                else if (InitializerAstNode i = parseInitializer())
                {
                    result.arrayInitializerElements ~= i;
                }
                else
                {
                    parseError("invalid array initializer");
                    return null;
                }
                if (current.isTokComma)
                {
                    advance();
                }
            }
            advance();
            return result;
        }
        else if (ExpressionAstNode e = parseExpression(null))
        {
            result.singleInitializer = e;
            return result;
        }
        else
        {
            parseError("invalid initializer");
            return null;
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
        result.position = current.position();
        TypeModifierAstNode lastMd = result;
        result.position = current.position;
        if (!current.isTokMul && !current.isTokLeftSquare)
        {
            return null;
        }
        while ((current.isTokMul || current.isTokLeftSquare) && !current.isTokRightParen)
        {
            if (lastMd.kind != ModifierKind.none)
            {
                lastMd.modifier = new TypeModifierAstNode;
                lastMd = lastMd.modifier;
            }
            if (current.isTokMul)
            {
                lastMd.kind = ModifierKind.pointer;
                advance();
            }
            else if (current.isTokLeftSquare)
            {
                advance();
                if (current.isTokRightSquare)
                {
                    lastMd.kind = ModifierKind.arrayDynDim;
                    advance();
                }
                else if (ExpressionAstNode e = parseExpression(null))
                {
                    lastMd.kind = ModifierKind.arrayStatDim;
                    lastMd.staticDimension = e;
                    assert (current.isTokRightSquare);
                    advance();
                }
                else
                {
                    parseError("expected either an expression that gives a dimension or `]`");
                    return null;
                }
            }
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
        result.position = current.position;
        bool isBetweenParens;
        if (current.isTokAuto)
        {
            result.basicOrQualifiedType ~= current();
            advance();
            return result;
        }
        else if (current.isTokLeftParen)
        {
            isBetweenParens = true;
            advance();
        }
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
        if (current.isTokRightParen)
        {
            if (!isBetweenParens)
            {
                parseError("invalid type delimiter, missing left paren");
                return null;
            }
            else advance();
        }
        else if (isBetweenParens && !current.isTokRightParen)
        {
            parseError("invalid type delimiter, missing right paren");
            return null;
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
     * Parses a FunctionParameterGroup.
     *
     * Returns:
     *      On success a $(D FunctionParameterGroupAstNode) otherwise $(D null).
     */
    FunctionParameterGroupAstNode parseFunctionParameterGroup()
    {
        FunctionParameterGroupAstNode result = new FunctionParameterGroupAstNode;
        result.position = current.position();
        while (true)
        {
            if (current.isTokConst)
            {
                if (result.isConst)
                {
                    warning("redundant storage class, `const` is already specified");
                }
                result.isConst = true;
                advance();
            }
            else if (current.isTokVar)
            {
                if (result.isVar)
                {
                    warning("redundant storage class, `var` is already specified");
                }
                result.isVar = true;
                advance();
            }
            else break;
        }
        TypeAstNode type = parseType();
        if (!type && (current.isTokIdentifier || current.isTokComma))
        {
            parseError("Type expected before parameter list");
            return null;
        }
        if (current.isTokIdentifier)
        {
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
        else
        {
            expected(TokenType.identifier);
            return null;
        }
        result.type = type;
        return result;
    }

    /**
     * Parses an InterfaceDeclaration.
     *
     * Returns:
     *      On success a $(D InterfaceDeclarationAstNode) otherwise $(D null).
     */
    InterfaceDeclarationAstNode parseInterfaceDeclaration()
    {
        assert(current.isTokInterface);
        InterfaceDeclarationAstNode result = new InterfaceDeclarationAstNode;
        result.position = current.position;
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        result.name = current();
        advance();
        if (current.isTokColon)
        {
            advance();
            if (IdentifierChainsAstNode ic = parseIdentifierChains())
            {
                result.inheritanceList = ic;
            }
            else
            {
                parseError("invalid inheritance list");
                return null;
            }
        }
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            return null;
        }
        advance();
        parseDeclarations(result.declarations);
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
            return null;
        }
        advance();
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
        assert(current.isTokClass);
        ClassDeclarationAstNode result = new ClassDeclarationAstNode;
        result.position = current.position;
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        result.name = current();
        advance();
        if (current.isTokColon)
        {
            advance();
            if (IdentifierChainsAstNode ic = parseIdentifierChains())
            {
                result.inheritanceList = ic;
            }
            else
            {
                parseError("invalid inheritance list");
                return null;
            }
        }
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            return null;
        }
        advance();
        parseDeclarations(result.declarations);
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
            return null;
        }
        advance();
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
        assert(current.isTokStruct);
        StructDeclarationAstNode result = new StructDeclarationAstNode;
        result.position = current.position;
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        result.name = current();
        advance();
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            return null;
        }
        advance();
        parseDeclarations(result.declarations);
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
            return null;
        }
        advance();
        return result;
    }

    /**
     * Parses an OnMatchExpression.
     *
     * Returns:
     *      On success a $(D OnMatchExpressionAstNode) otherwise $(D null).
     */
    SingleOrRangeExpressionAstNode parseSingleOrRangeExpression()
    {
        SingleOrRangeExpressionAstNode result = new SingleOrRangeExpressionAstNode;
        result.position = current.position();
        if (ExpressionAstNode e1 = parseExpression(null))
        {
            result.singleOrLeftExpression = e1;
            if (current.isTokDotDot)
            {
                advance();
                if (ExpressionAstNode e2 = parseExpression(null))
                    result.rightExpression = e2;
                else return null;
            }
            return result;
        }
        else return null;
    }

    /**
     * Parses an OnMatchStatement.
     *
     * Returns:
     *      On success a $(D OnMatchStatementAstNode) otherwise $(D null).
     */
    OnMatchStatementAstNode parseOnMatchStatementAstNode()
    {
        assert(current.isTokOn);
        OnMatchStatementAstNode result = new OnMatchStatementAstNode;
        result.position = current.position();
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        while (true)
        {
            if (SingleOrRangeExpressionAstNode e = parseSingleOrRangeExpression)
            {
                result.onMatchExpressions ~= e;
                if (current.isTokRightParen)
                {
                    advance();
                    break;
                }
                else if (current.isTokComma)
                {
                    advance();
                    continue;
                }
                else
                {
                    unexpected();
                    return null;
                }
            }
            else
            {
                parseError("invalid on match expression");
                return null;
            }
        }
        if (SingleStatementOrBlockAstNode ssob = parseSingleStatementOrBlock())
        {
            result.singleStatementOrBlock = ssob;
            return result;
        }
        else
        {
            parseError("invalid on match statement");
            return null;
        }
    }

    /**
     * Parses an SwitchStatement.
     *
     * Returns:
     *      On success a $(D SwitchStatementAstNode) otherwise $(D null).
     */
    SwitchStatementAstNode parseSwitchStatement()
    {
        assert(current.isTokSwitch);
        SwitchStatementAstNode result = new SwitchStatementAstNode;
        result.position = current.position();
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        if (ExpressionAstNode e = parseExpression(null))
        {
            result.expression = e;
        }
        else
        {
            return null;
        }
        assert(current.isTokRightParen);
        advance();
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            return null;
        }
        advance();
        if (!current.isTokOn)
        {
            expected(TokenType.on);
            return null;
        }
        while(true)
        {
            if (OnMatchStatementAstNode oms = parseOnMatchStatementAstNode())
            {
                result.onMatchStatements ~= oms;
            }
            else return null;
            if (current.isTokOn)
            {
                continue;
            }
            else if (current.isTokElse || current.isTokRightCurly)
            {
                break;
            }
            else
            {
                unexpected();
                return null;
            }
        }
        if (current.isTokElse)
        {
            advance();
            if (SingleStatementOrBlockAstNode ssob = parseSingleStatementOrBlock())
            {
                result.elseStatement = ssob;
            }
            else return null;
        }
        assert(current.isTokRightCurly);
        advance();
        return result;
    }

    /**
     * Parses an ThrowStatement.
     *
     * Returns:
     *      On success a $(D ThrowStatementAstNode) otherwise $(D null).
     */
    ThrowStatementAstNode parseThrowStatement()
    {
        assert(current.isTokThrow);
        ThrowStatementAstNode result = new ThrowStatementAstNode;
        result.position = current.position;
        advance();
        if (UnaryExpressionAstNode ue = parseUnaryExpression())
        {
            if (!current.isTokSemicolon)
            {
                expected(TokenType.semiColon);
                return null;
            }
            result.unary = ue;
            advance();
            return result;
        }
        else return null;
    }

    /**
     * Parses a TryStatement.
     *
     * Returns:
     *      On success a $(D TryStatementAstNode) otherwise $(D null).
     */
    TryOnFinallyStatementAstNode parseTryOnFinallyStatement()
    {
        assert(current.isTokTry);
        TryOnFinallyStatementAstNode result = new TryOnFinallyStatementAstNode;
        result.position = current.position;
        advance();
        if (SingleStatementOrBlockAstNode ssob = parseSingleStatementOrBlock())
        {
            result.triedStatementOrBlock = ssob;
        }
        else return null;
        while (current.isTokOn)
        {
            if (OnExceptionStatementAstNode oes = parseOnExceptionStatemtent())
            {
                result.exceptionStatements ~= oes;
            }
            else
            {
                parseError("invalid on exception statement");
                return null;
            }
        }
        if (current.isTokFinally)
        {
            advance();
            if (SingleStatementOrBlockAstNode ssob = parseSingleStatementOrBlock())
            {
                result.finalStatementOrBlock = ssob;
            }
            else return null;
        }
        return result;
    }

    /**
     * Parses an OnExceptionStatemtent.
     *
     * Returns:
     *      On success a $(D OnExceptionStatemtentAstNode) otherwise $(D null).
     */
    OnExceptionStatementAstNode parseOnExceptionStatemtent()
    {
        assert(current.isTokOn);
        OnExceptionStatementAstNode result = new OnExceptionStatementAstNode;
        result.position = current.position();
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        if (current.isTokRightParen)
        {
            parseError("expected at least one on exception instance");
            return null;
        }
        while (true)
        {
            if (OnExceptionInstanceAstNode oei = parseOnExceptionInstance())
            {
                result.exceptionsInstances ~= oei;
            }
            else
            {
                parseError("invalid exception instance");
                return null;
            }
            if (current.isTokComma)
            {
                advance();
                continue;
            }
            else if (current.isTokRightParen)
            {
                advance();
                break;
            }
            else
            {
                unexpected();
                return null;
            }
        }
        if (SingleStatementOrBlockAstNode ssob = parseSingleStatementOrBlock())
        {
            result.exceptionsStatementorBlock = ssob;
            return result;
        }
        else
        {
            parseError("invalid on exception statement");
            return null;
        }
    }

    /**
     * Parses an OnExceptionInstance.
     *
     * Returns:
     *      On success a $(D OnExceptionInstanceAstNode) otherwise $(D null).
     */
    OnExceptionInstanceAstNode parseOnExceptionInstance()
    {
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        OnExceptionInstanceAstNode result = new OnExceptionInstanceAstNode;
        result.position = current.position();
        if (TypeAstNode t = parseType())
        {
            result.exceptionType = t;
        }
        else
        {
            parseError("invalid on exception type");
            return null;
        }
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        result.identifier = current();
        advance();
        return result;
    }

    /**
     * Parses an EnumMember.
     *
     * Returns:
     *      On success a $(D EnumMemberAstNode) otherwise $(D null).
     */
    EnumMemberAstNode parseEnumMember()
    {
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        EnumMemberAstNode result = new EnumMemberAstNode;
        result.identifier = current();
        result.position = current.position();
        advance();
        if (current.isTokComma || current.isTokRightCurly)
        {
            return result;
        }
        else if (current.isTokEqual)
        {
            advance();
            if (ExpressionAstNode e = parseExpression(null))
            {
                result.value = e;
                if (current.isTokComma || current.isTokRightCurly)
                {
                    return result;
                }
                else
                {
                    parseError("expected `,` or `}` after declaring an enum member");
                    return null;
                }
            }
            else
            {
                parseError("invalid enum member value");
                return null;
            }
        }
        else
        {
            unexpected();
            return null;
        }
    }

    /**
     * Parses an EnumDeclaration
     *
     * Returns:
     *      On success a $(D EnumDeclarationAstNode) otherwise $(D null).
     */
    EnumDeclarationAstNode parseEnumDeclaration()
    {
        assert(current.isTokEnum);
        EnumDeclarationAstNode result = new EnumDeclarationAstNode;
        result.position = current.position;
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        result.name = current();
        advance();
        if (current.isTokColon)
        {
            advance();
            if (TypeAstNode t = parseType())
            {
                result.type = t;
            }
            else
            {
                parseError("invalid enum type");
                return null;
            }
        }
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            return null;
        }
        advance();
        while (true)
        {
            if (EnumMemberAstNode ei = parseEnumMember())
            {
                result.members ~= ei;
                if (current.isTokComma)
                {
                    advance();
                    continue;
                }
            }
            if (current.isTokRightCurly)
            {
                if (result.members.length)
                {
                    advance();
                    return result;
                }
                else
                {
                    parseError("no enum members");
                    return null;
                }
            }
            else
            {
                parseError("invalid enum member");
                return null;
            }
        }
    }

    /**
     * Parses an ImportDeclaration.
     *
     * Returns:
     *      On success a $(D ImportDeclarationAstNode) otherwise $(D null).
     */
    ImportDeclarationAstNode parseImportDeclaration()
    {
        assert(current.isTokImport);
        ImportDeclarationAstNode result = new ImportDeclarationAstNode;
        result.position = current.position;
        advance();
        if (current.isTokLeftParen)
        {
            advance();
            if (current.isTokIntegerLiteral || current.isTokHexLiteral)
            {
                result.priority = current();
                advance();
            }
            else
            {
                parseError("int or hex literal expected to set the import priority");
                return null;
            }
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
                advance();
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
        result.position = current.position;
        result.isStatic = isStatic;
        if (!current.isTokRightParen) while (true)
        {
            if (FunctionParameterGroupAstNode fpg = parseFunctionParameterGroup())
            {
                result.parameters ~= fpg;
                if (!current.isTokSemicolon)
                    break;
                else
                    advance();
            }
            else
            {
                break;
            }
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
     * Parses an AtAttribute.
     *
     * Returns:
     *      On success a $(D AtAttributeAstNode) otherwise $(D null).
     */
    AtAttributeAstNode parseAtAttribute()
    {
        assert(current.isTokAt);
        AtAttributeAstNode result = new AtAttributeAstNode;
        result.position = current.position();
        advance();
        if (!current.isTokKeyword && !current.isTokIdentifier)
        {
            parseError("expected a keyword or an identifier after `@`");
            return null;
        }
        result.identifierOrKeyword = current();
        advance();
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
        FunctionHeaderAstNode result = new FunctionHeaderAstNode;
        result.position = current.position();
        while (current.isTokAt)
        {
            if (AtAttributeAstNode aa = parseAtAttribute())
                result.attributes ~= aa;
            else return null;
        }
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
        result.position = current.position;
        result.isStatic = isStatic;
        result.name = current();
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        while (!current.isTokRightParen)
        {
            if (FunctionParameterGroupAstNode fpg = parseFunctionParameterGroup())
            {
                result.parameters ~= fpg;
                if (!current.isTokSemicolon)
                    break;
                advance();
            }
            else break;
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
        result.position = header.position;
        result.header = header;
        if (!current.isTokLeftCurly && !current.isTokSemicolon)
        {
            parseError("expected `;` or `{` to skip or start the function body");
            return null;
        }
        result.firstBodyToken = current();
        if (current.isTokLeftCurly)
        {
            advance();
            if (!parseDeclarationsOrStatements(result.declarationsOrStatements))
            {
                parseError("invalid declarations or statements");
                return null;
            }
            if (!current.isTokRightCurly)
            {
                expected(TokenType.rightCurly);
                return null;
            }
        }
        advance();
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
        assert(current.isTokProtection);
        ProtectionDeclarationAstNode result = new ProtectionDeclarationAstNode;
        result.position = current.position;
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
        result.protection = current();
        advance();
        if (!current.isTokRightParen)
        {
            expected(TokenType.rightParen);
            return null;
        }
        advance();
        return result;
    }

    /**
     * Parses a VariableDeclarationItem.
     *
     * Returns:
     *      A $(D VariableDeclarationItemAstNode) on success, $(D null) otherwise.
     */
    VariableDeclarationItemAstNode parseVariableDeclarationItem()
    {
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        VariableDeclarationItemAstNode result = new VariableDeclarationItemAstNode;
        result.position = current.position;
        result.name = current();
        advance();
        if (current.isTokEqual)
        {
            advance();
            if (InitializerAstNode i = parseInitializer())
            {
                result.initializer = i;
            }
            else return null;
        }
        /*if (!current.isTokComma && !current.isTokSemicolon)
        {
            parseError("expected colon or semicolon");
            return null;
        }
        else*/
        return result;
    }

    /**
     * Parses a VariableDeclaration.
     *
     * Returns:
     *      A $(D VariableDeclarationAstNode) on success, $(D null) otherwise.
     */
    VariableDeclarationAstNode parseVariableDeclaration()
    {
        bool isStatic;
        bool isConst;
        VariableDeclarationAstNode result = new VariableDeclarationAstNode;
        result.position = current.position();
        if (current.isTokConst)
        {
            isConst = true;
            advance();
        }
        else if (current.isTokVar)
        {
            advance();
        }
        else
        {
            expected(TokenType.var);
            return null;
        }
        if (current.isTokStatic)
        {
            isStatic = true;
            advance();
        }
        if (TypeAstNode t = parseType())
        {
            result.type = t;
            result.isStatic = isStatic;
            result.isConst = isConst;
            result.position = current.position;
            while (true)
            {
                if (VariableDeclarationItemAstNode vdi = parseVariableDeclarationItem())
                {
                    result.list ~= vdi;
                    if (current.isTokComma)
                    {
                        advance();
                        continue;
                    }
                    else if (current.isTokSemicolon)
                    {
                        advance();
                        return result;
                    }
                    else
                    {
                        parseError("expected colon or semicolon");
                        return null;
                    }
                }
                else
                {
                    parseError("invalid variable declaration");
                    return null;
                }
            }
        }
        else return null;
    }

    /**
     * Parses an AkaDeclaration.
     *
     * Returns:
     *      A $(D AkaDeclarationAstNode) on success, $(D null) otherwise.
     */
    AkaDeclarationAstNode parseAkaDeclaration()
    {
        assert(current.isTokIs);
        AkaDeclarationAstNode result = new AkaDeclarationAstNode;
        result.position = current.position;
        advance();
        if (TypeAstNode t = parseType())
        {
            if (!current.isTokAka)
            {
                expected(TokenType.aka);
                return null;
            }
            advance();
            if (!current.isTokIdentifier)
            {
                expected(TokenType.identifier);
                return null;
            }
            result.type = t;
            result.name = current();
            advance();
            if (!current.isTokSemicolon)
            {
                expected(TokenType.semiColon);
                return null;
            }
            advance();
            return result;
        }
        else return null;
    }

    /**
     * Parses a DeclarationOrStatement.
     *
     * Returns:
     *      A $(D DeclarationOrStatementAstNode) on success, $(D null) otherwise.
     */
    DeclarationOrStatementAstNode parseDeclarationOrStatement()
    {
        if (DeclarationAstNode d = parseDeclaration())
        {
            DeclarationOrStatementAstNode result = new DeclarationOrStatementAstNode;
            result.declaration = d;
            return result;
        }
        else if (StatementAstNode s = parseStatement())
        {
            DeclarationOrStatementAstNode result = new DeclarationOrStatementAstNode;
            result.statement  = s;
            return result;
        }
        else
        {
            return null;
        }
    }

    /**
     * Parses contiguous DeclarationsOrStatement.
     *
     * Params:
     *      declsOrStatements: The array filled with one or more
     *      DeclarationsOrStatement
     * Returns:
     *      $(D true) on success, $(D false) otherwise.
     */
    bool parseDeclarationsOrStatements(ref DeclarationOrStatementAstNode[] declsOrStatements)
    {
        const ptrdiff_t oldDeclLvl = _declarationLevels;
        ++_declarationLevels;

        while (true)
        {
            with (TokenType) switch (current.type)
            {
            case virtual, eof:
                return true; // virtual unit
            case rightCurly:
                --_declarationLevels;
                assert (oldDeclLvl == _declarationLevels);
                return true;
            default:
                if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
                {
                    declsOrStatements ~= dos;
                    continue;
                }
                else
                {
                    unexpected();
                    return false;
                }
            }
        }
    }

    /**
     * Parses contiguous Declaration.
     *
     * Params:
     *      declarations: The array filled with the declarations.
     * Returns:
     *      $(D true) on success, $(D false) otherwise.
     */
    bool parseDeclarations(ref DeclarationAstNode[] declarations)
    {
        const ptrdiff_t oldDeclLvl = _declarationLevels;
        ++_declarationLevels;
        while (true)
        {
            with (TokenType) switch (current.type)
            {
            case virtual, eof:
                return true; // virtual unit
            case rightCurly:
                --_declarationLevels;
                assert(oldDeclLvl == _declarationLevels);
                return true;
            default:
                if (DeclarationAstNode d = parseDeclaration())
                {
                    declarations ~= d;
                    continue;
                }
                else
                {
                    unexpected();
                    return false;
                }
            }
        }
    }

    /**
     * Parses CallParameters.
     *
     * Returns: a $(D CallParametersAstNode) on success, $(D null) otherwise.
     */
    CallParametersAstNode parseCallParameters()
    {
        assert(current.isTokLeftParen);
        CallParametersAstNode result = new CallParametersAstNode;
        result.position = current.position;
        advance();
        if (current.isTokRightParen)
        {
            advance();
            return result;
        }
        while (true)
        {
            if (ExpressionAstNode ex = parseExpression(null))
            {
                result.parameters ~= ex;
                with(TokenType) switch (current.type)
                {
                case rightParen:
                    advance();
                    return result;
                case comma:
                    advance();
                    continue;
                default:
                    unexpected();
                    return null;
                }
            }
            else return null;
        }
    }

    /**
     * Parses a PrimaryExpression.
     *
     * Returns: a $(D PrimaryExpressionAstNode) on success, $(D null) otherwise.
     */
    PrimaryExpressionAstNode parsePrimaryExpression()
    {
        if (!current.isTokSuper && !current.isTokValueKeyword &&
            !current.isTokIdentifier && !current.isTokLiteral &&
            !current.isTokLeftParen && !current.isTokLeftSquare)
        {
            unexpected();
            return null;
        }
        PrimaryExpressionAstNode result = new PrimaryExpressionAstNode;
        result.position = current.position;
        if (current.isTokLeftSquare)
        {
            if (InitializerAstNode i = parseInitializer())
            {
                result.arrayLiteral = i;
                return result;
            }
            else return null;
        }
        else if (!current.isTokLeftParen)
        {
            result.identifierOrKeywordOrLiteral = current;
            advance();
            return result;
        }
        else
        {
            advance();
            if (ExpressionAstNode e = parseExpression(null))
            {
                if (!current.isTokRightParen)
                {
                    expected(TokenType.rightParen);
                    return null;
                }
                else
                {
                    result.parenExpression = e;
                    advance();
                    return result;
                }
            }
            else return null;
        }
    }

    /**
     * Parses a PostfixExpression.
     *
     * Returns: a $(D PostfixExpressionAstNode) on success, $(D null) otherwise.
     */
    PostfixExpressionAstNode parsePostfixExpression()
    {
        assert(current.isTokPostfixStarter);
        PostfixExpressionAstNode result = new PostfixExpressionAstNode;
        result.position = current.position;
        if (current.isTokLeftSquare)
        {
            advance();
            ExpressionAstNode e = parseExpression(null);
            if (!e || current.isTokRightSquare)
            {
                result.indexExpression = new IndexExpressionAstNode;
                if (e)
                    result.indexExpression.index = e;
                advance();
                return result;
            }
            else if (current.isTokDotDot)
            {
                advance();
                if (ExpressionAstNode r = parseExpression(null))
                    if (current.isTokRightSquare)
                {
                    SliceExpressionAstNode se = new SliceExpressionAstNode;
                    se.left = e;
                    se.right = r;
                    result.sliceExpression = se;
                    advance();
                    return result;
                }
            }
            parseError("invalid index or range expression");
            return null;
        }
        else if (current.isTokUnarySuffix)
        {
            result.plusplusOrMinusMinus = current();
            advance();
            return result;
        }
        else if (current.isTokColon)
        {
            advance();
            if (TypeAstNode t = parseType())
            {
                result.castToType = t;
                return result;
            }
            else
            {
                parseError("invalid cast type");
                return null;
            }
        }
        else if (current.isTokLeftParen)
        {
            if (CallParametersAstNode cp = parseCallParameters())
            {
                result.callParameters = cp;
                return result;
            }
            else
            {
                parseError("invalid call parameters");
                return null;
            }
        }
        else
        {
            assert(current.isTokOptAccess || current.isTokDot);
            result.dotOrOptAccess = current;
            advance();
            if (PrimaryExpressionAstNode pe = parsePrimaryExpression())
            {
                result.primary = pe;
                return result;
            }
            else
            {
                parseError("invalid primary expression");
                return null;
            }
        }
    }

    /**
     * Parses an UnaryExpression.
     *
     * Returns: a $(D UnaryExpressionAstNode) on success, $(D null) otherwise.
     */
    UnaryExpressionAstNode parseUnaryExpression()
    {
        UnaryExpressionAstNode result = new UnaryExpressionAstNode;
        result.position = current.position;
        if (current.isTokUnaryPrefix)
        {
            result.prefix = current();
            advance();
            if (UnaryExpressionAstNode u = parseUnaryExpression())
            {
                result.unary = u;
                return result;
            }
            else return null;
        }
        else if (PrimaryExpressionAstNode pe = parsePrimaryExpression())
        {
            result.primary = pe;
            while (current.isTokPostfixStarter)
            {
                if (PostfixExpressionAstNode pfe = parsePostfixExpression)
                {
                    result.postfixes ~= pfe;
                }
                else return null;
            }
            return result;
        }
        else return null;
    }

    /**
     * Parses an AssignExpression.
     *
     * Returns: a $(D AssignExpressionAstNode) on success, $(D null) otherwise.
     */
    AssignExpressionAstNode parseAssignExpression()
    {
        if (ExpressionAstNode e = parseExpression(null))
        {
            AssignExpressionAstNode result = new AssignExpressionAstNode;
            result.left = e;
            e.position = current.position;
            if (current.isTokSemicolon)
            {
                return result;
            }
            else if (current.isTokAssignOperator)
            {
                result.operator = current();
                advance();
                if (AssignExpressionAstNode ae = parseAssignExpression())
                {
                    result.right = ae;
                    return result;
                }
                else
                {
                    parseError("expected an expression following `=`");
                    return null;
                }
            }
            else
            {
                unexpected();
                return null;
            }
        }
        return null;
    }

    /**
     * Parses an Expression.
     *
     * Returns: a $(D ExpressionAstNode) on success, $(D null) otherwise.
     */
    ExpressionAstNode parseExpression(ExpressionAstNode exp)
    {
        with(TokenType)
        if (exp)
        {
            if (!current.isTokOperator)
            {
                unexpected();
                return null;
            }
            Token* op = current();
            advance();
            if (ExpressionAstNode r = parseExpression(null))
            {
                ExpressionAstNode result = new ExpressionAstNode;
                BinaryExpressionAstNode be = new BinaryExpressionAstNode;

                be.position = current.position;
                be.left = exp;
                be.operator = op;
                be.right = r;
                result.binaryExpression = be;

                if (r.binaryExpression && r.binaryExpression.operator.type > op.type)
                {
                    /*
                            a           e1L
                              *         e1O
                                b + c   e1R

                            a * b       e1L
                                  +     e1O
                                    c   e1R
                    */
                    writeln("swap: ", r.binaryExpression.operator.text, " ", op.text);

                    ExpressionAstNode old_L = be.left;
                    ExpressionAstNode old_R = be.right;
                    Token* old_O = be.operator;

                    ExpressionAstNode old_RL = be.right.binaryExpression.left;
                    ExpressionAstNode old_RR = be.right.binaryExpression.right;
                    Token* old_RO = be.right.binaryExpression.operator;

                    be.left = old_R;
                    be.left.binaryExpression.left = old_L;
                    be.left.binaryExpression.operator = old_O;
                    be.left.binaryExpression.right = old_RL;
                    be.operator = old_RO;
                    be.right = old_RR;
                }
                if (current.type.among(semiColon, rightCurly, rightParen, rightSquare, comma, dotDot, equal) ||
                    current.isTokAssignOperator)
                {
                    return result;
                }
            }
            else
            {
                parseError("invalid binary expression RHS");
                return null;
            }
        }
        else if (current.isTokUnaryPrefix || current.isTokIdentifier ||
            current.isTokLiteral || current.isTokLeftParen || current.isTokLeftSquare ||
            current.isTokSuper || current.isTokValueKeyword)
        {
            if (UnaryExpressionAstNode u = parseUnaryExpression())
            {
                ExpressionAstNode result = new ExpressionAstNode;
                result.unaryExpression = u;
                u.position = current.position;
                if (current.type.among(semiColon, rightCurly, rightParen, rightSquare, comma, dotDot, equal) ||
                    current.isTokAssignOperator)
                {
                    return result;
                }
                else // binary with this unary as LHS
                {
                    result = parseExpression(result);
                    return result;
                }
            }
        }
        parseError("invalid binary or unary expression");
        return null;
    }

    /**
     * Parses an ExpressionStatement.
     *
     * Returns: a $(D ExpressionStatementAstNode) on success, $(D null) otherwise.
     */
    ExpressionStatementAstNode parseExpressionStatement()
    {
        if (AssignExpressionAstNode ae = parseAssignExpression())
        {
            ExpressionStatementAstNode result = new ExpressionStatementAstNode;
            result.assignExpression = ae;
            assert(current.isTokSemicolon);
            advance();
            return result;
        }
        else return null;
    }

    /**
     * Parses a WhileStatement.
     *
     * Returns: a $(D WhileStatementAstNode) on success, $(D null) otherwise.
     */
    WhileStatementAstNode parseWhileStatement()
    {
        assert(current.isTokWhile);
        WhileStatementAstNode result = new WhileStatementAstNode;
        result.position = current.position();
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        if (ExpressionAstNode c = parseExpression(null))
        {
            result.condition = c;
        }
        else
        {
            parseError("invalid while condition");
            return null;
        }
        if (!current.isTokRightParen)
        {
            expected(TokenType.rightParen);
            return null;
        }
        advance();
        if (SingleStatementOrBlockAstNode ssob = parseSingleStatementOrBlock())
        {
            result.singleStatementOrBlock = ssob;
            return result;
        }
        else
        {
            parseError("invalid single statement or block");
            return null;
        }
    }


    /**
     * Parses a ForeachStatement.
     *
     * Returns: a $(D ForeachStatementAstNode) on success, $(D null) otherwise.
     */
    ForeachStatementAstNode parseForeachStatement()
    {
        assert(current.isTokForeach);
        ForeachStatementAstNode result = new ForeachStatementAstNode;
        result.position = current.position();
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        if (VariableDeclarationAstNode vd = parseVariableDeclaration)
        {
            result.variable = vd;
        }
        else
        {
            parseError("invalid foreach variable");
            return null;
        }
        //note: parseVariableDeclaration should not eat the semicolon.
        /*if (!current.isTokSemicolon)
        {
            expected(TokenType.semiColon);
            return null;
        }
        advance();*/
        if (SingleOrRangeExpressionAstNode sre = parseSingleOrRangeExpression())
        {
            result.singleOrRangeExpression = sre;
        }
        else
        {
            parseError("invalid foreach enumarable");
            return null;
        }
        if (!current.isTokRightParen)
        {
            expected(TokenType.rightParen);
            return null;
        }
        advance();
        if (SingleStatementOrBlockAstNode ssob = parseSingleStatementOrBlock())
        {
            result.singleStatementOrBlock = ssob;
            return result;
        }
        else
        {
            parseError("invalid single statement or block");
            return null;
        }
    }

    /**
     * Parses an IfConditionVariableAstNode.
     *
     * Returns: an $(D IfConditionVariableAstNode) on success, $(D null) otherwise.
     */
    IfConditionVariableAstNode parseIfConditionVariableAstNode()
    {
        assert(current.isTokConst || current.isTokVar);
        IfConditionVariableAstNode result = new IfConditionVariableAstNode;
        result.position = current.position();
        result.isConst = current.isTokConst;
        advance();
        if (TypeAstNode t = parseType())
        {
            result.type = t;
        }
        else
        {
            parseError("invalid variable type");
            return null;
        }
        if (VariableDeclarationItemAstNode vdi = parseVariableDeclarationItem())
        {
            result.variable = vdi;
            if (!current.isTokRightParen)
            {
                expected(TokenType.rightParen);
                return null;
            }
            return result;
        }
        else return null;
    }

    /**
     * Parses an IfElseStatement.
     *
     * Returns: an $(D IfElseStatementAstNode) on success, $(D null) otherwise.
     */
    IfElseStatementAstNode parseIfElseStatement()
    {
        assert(current.isTokIf);
        IfElseStatementAstNode result = new IfElseStatementAstNode;
        result.position = current.position();
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        if (current.isTokStorageClass)
        {
            if (IfConditionVariableAstNode icv = parseIfConditionVariableAstNode())
                result.ifVariable = icv;
        }
        else if (ExpressionAstNode c = parseExpression(null))
        {
            result.condition = c;
        }
        if (!result.condition && !result.ifVariable)
        {
            parseError("invalid if condition");
            return null;
        }
        if (!current.isTokRightParen)
        {
            expected(TokenType.rightParen);
            return null;
        }
        advance();
        if (SingleStatementOrBlockAstNode ssob = parseSingleStatementOrBlock())
        {
            result.trueStatementOrBlock = ssob;
        }
        else
        {
            parseError("invalid true single statement or block");
            return null;
        }
        if (current.isTokElse)
        {
            advance();
            if (SingleStatementOrBlockAstNode ssob = parseSingleStatementOrBlock())
            {
                result.falseStatementOrBlock = ssob;
            }
            else
            {
                parseError("invalid false single statement or block");
                return null;
            }
        }
        return result;
    }

    /**
     * Parses a ReturnStatement.
     *
     * Returns: a $(D ReturnStatementAstNode) on success, $(D null) otherwise.
     */
    ReturnStatementAstNode parseReturnStatement()
    {
        assert(current.isTokReturn);
        ReturnStatementAstNode result = new ReturnStatementAstNode;
        result.position = current.position;
        advance();
        if (!current.isTokSemicolon)
            if (AssignExpressionAstNode ae = parseAssignExpression())
        {
            result.expression = ae;
        }
        if (!current.isTokSemicolon)
        {
            expected(TokenType.semiColon);
            return null;
        }
        else
        {
            advance();
            return result;
        }
    }

    /**
     * Parses a ContinueStatement.
     *
     * Returns: a $(D ContinueStatementAstNode) on success, $(D null) otherwise.
     */
    ContinueStatementAstNode parseContinueStatement()
    {
        assert(current.isTokContinue);
        ContinueStatementAstNode result = new ContinueStatementAstNode;
        result.position = current.position;
        advance();
        if (!current.isTokSemicolon)
            if (AssignExpressionAstNode ae = parseAssignExpression())
        {
            result.expression = ae;
        }
        if (!current.isTokSemicolon)
        {
            expected(TokenType.semiColon);
            return null;
        }
        else
        {
            advance();
            return result;
        }
    }

    /**
     * Parses a BreakStatement.
     *
     * Returns: a $(D BreakStatementAstNode) on success, $(D null) otherwise.
     */
    BreakStatementAstNode parseBreakStatement()
    {
        assert(current.isTokBreak);
        BreakStatementAstNode result = new BreakStatementAstNode;
        result.position = current.position;
        advance();
        if (current.isTokLeftParen)
        {
            advance();
            if (!current.isTokIdentifier)
            {
                parseError("expected an identifier as break label");
                return null;
            }
            result.label = current();
            advance();
            if (!current.isTokRightParen)
            {
                expected(TokenType.rightParen);
                return null;
            }
            advance();
        }
        if (!current.isTokSemicolon)
            if (AssignExpressionAstNode ae = parseAssignExpression())
        {
            result.expression = ae;
        }
        if (!current.isTokSemicolon)
        {
            expected(TokenType.semiColon);
            return null;
        }
        else
        {
            advance();
            return result;
        }
    }

    /**
     * Parses a SingleStatementOrBlock.
     *
     * Returns: a $(D SingleStatementOrBlockAstNode) on success, $(D null) otherwise.
     */
    SingleStatementOrBlockAstNode parseSingleStatementOrBlock()
    {
        SingleStatementOrBlockAstNode result = new SingleStatementOrBlockAstNode;
        result.position = current.position();
        if (current.isTokLeftCurly)
        {
            advance();
            BlockStatementAstNode bs = new BlockStatementAstNode;
            parseDeclarationsOrStatements(bs.declarationsOrStatements);
            if (!current.isTokRightCurly)
            {
                expected(TokenType.rightCurly);
                return null;
            }
            advance();
            result.block = bs;
            return result;
        }
        else
        {
            if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
            {
                result.singleStatement = dos;
                return result;
            }
            else
            {
                parseError("invalid single statement");
                return null;
            }
        }
    }

    /**
     * Parses a VersionBlockDeclaration.
     *
     * Returns: a $(D VersionBlockDeclarationAstNode) on success, $(D null) otherwise.
     */
    VersionBlockDeclarationAstNode parseVersionBlockDeclaration()
    {
        assert(current.isTokVersion);
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        VersionBlockDeclarationAstNode result = new VersionBlockDeclarationAstNode;
        result.position = current.position;
        if (VersionParenExpressionAstNode vpe = parseVersionParenExpression())
        {
            result.versionExpression = vpe;
        }
        else
        {
            parseError("invalid version paren expression");
            return null;
        }
        if (SingleStatementOrBlockAstNode ssob = parseSingleStatementOrBlock())
        {
            result.trueDeclarationOrBlock = ssob;
        }
        else
        {
            parseError("invalid true single statement or block");
            return null;
        }
        if (current.isTokElse)
        {
            advance();
            if (SingleStatementOrBlockAstNode ssob = parseSingleStatementOrBlock())
            {
                result.falseDeclarationOrBlock = ssob;
            }
            else
            {
                parseError("invalid false single statement or block");
                return null;
            }
        }
        return result;
    }

    /**
     * Parses a VersionParenExpression.
     *
     * Returns: a $(D VersionParenExpressionAstNode) on success, $(D null) otherwise.
     */
    VersionParenExpressionAstNode parseVersionParenExpression()
    {
        assert(current.isTokLeftParen);
        advance();
        if (current.isTokRightParen)
        {
            parseError("empty version expression");
            return null;
        }
        if (VersionOrExpressionAstNode voe = parseVersionOrExpression())
        {
            VersionParenExpressionAstNode result = new VersionParenExpressionAstNode;
            result.expression = voe;
            advance();
            return result;
        }
        else return null;
    }

    /**
     * Parses a VersionOrExpression.
     *
     * Returns: a $(D VersionOrExpressionAstNode) on success, $(D null) otherwise.
     */
    VersionOrExpressionAstNode parseVersionOrExpression()
    {
        if (VersionAndExpressionAstNode vae = parseVersionAndExpression())
        {
            VersionOrExpressionAstNode result = new VersionOrExpressionAstNode;
            result.leftExpression = vae;
            if (current.isTokPipe)
            {
                advance();
                if (VersionOrExpressionAstNode voe = parseVersionOrExpression())
                {
                    result.rightExpression = voe;
                    return result;
                }
                else return null;
            }
            else return result;
        }
        else return null;
    }

    /**
     * Parses a VersionAndExpression.
     *
     * Returns: a $(D VersionAndExpressionAstNode) on success, $(D null) otherwise.
     */
    VersionAndExpressionAstNode parseVersionAndExpression()
    {
        if (VersionPrimaryExpressionAstNode vpe = parseVersionPrimaryExpression())
        {
            VersionAndExpressionAstNode result = new VersionAndExpressionAstNode;
            result.leftExpression = vpe;
            if (current.isTokAmp)
            {
                advance();
                if (VersionAndExpressionAstNode vae = parseVersionAndExpression())
                {
                    result.rightExpression = vae;
                    return result;
                }
                else return null;
            }
            else return result;
        }
        else return null;
    }

    /**
     * Parses a VersionPrimaryExpression.
     *
     * Returns: a $(D VersionPrimaryExpressionAstNode) on success, $(D null) otherwise.
     */
    VersionPrimaryExpressionAstNode parseVersionPrimaryExpression()
    {
        if (current.isTokIdentifier)
        {
            VersionPrimaryExpressionAstNode result = new VersionPrimaryExpressionAstNode;
            result.identifier = current;
            advance();
            if (current.isTokRightParen || current.isTokPipe || current.isTokAmp)
            {
                return result;
            }
            else
            {
                unexpected();
                return null;
            }
        }
        else if (current.isTokLeftParen)
        {
            if (VersionParenExpressionAstNode vpe = parseVersionParenExpression())
            {
                VersionPrimaryExpressionAstNode result = new VersionPrimaryExpressionAstNode;
                result.parenExpression = vpe;
                return result;
            }
            else return null;
        }
        else
        {
            parseError("expected an identifier or `(`");
            return null;
        }
    }

    /**
     * Parses a Statement.
     *
     * Returns: a $(D StatementAstNode) on success, $(D null) otherwise.
     */
    StatementAstNode parseStatement()
    {
        with(TokenType) switch(current.type)
        {
        case semiColon:
        {
            StatementAstNode result = new StatementAstNode;
            result.emptyStatement = new EmptyStatementAstNode;
            result.emptyStatement.position = current.position;
            advance();
            return result;
        }
        case return_:
        {
            if (ReturnStatementAstNode rs = parseReturnStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.returnStatement = rs;
                return result;
            }
            else
            {
                parseError("invalid return statement");
                return null;
            }
        }
        case break_:
        {
            if (BreakStatementAstNode bs = parseBreakStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.breakStatement = bs;
                return result;
            }
            else
            {
                parseError("invalid break statement");
                return null;
            }
        }
        case continue_:
        {
            if (ContinueStatementAstNode cs = parseContinueStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.continueStatement = cs;
                return result;
            }
            else
            {
                parseError("invalid continue statement");
                return null;
            }
        }
        case if_:
        {
            if (IfElseStatementAstNode ies = parseIfElseStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.ifElseStatement = ies;
                return result;
            }
            else
            {
                parseError("invalid if-else statement");
                return null;
            }
        }
        case while_:
        {
            if (WhileStatementAstNode ws = parseWhileStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.whileStatement = ws;
                return result;
            }
            else
            {
                parseError("invalid while statement");
                return null;
            }
        }
        case foreach_:
        {
            if (ForeachStatementAstNode fs = parseForeachStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.foreachStatement = fs;
                return result;
            }
            else
            {
                parseError("invalid foreach statement");
                return null;
            }
        }
        case switch_:
        {
            if (SwitchStatementAstNode ss = parseSwitchStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.switchStatement = ss;
                return result;
            }
            else
            {
                parseError("invalid switch statement");
                return null;
            }
        }
        case try_:
        {
            if (TryOnFinallyStatementAstNode tofs = parseTryOnFinallyStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.tryOnFinallyStatement = tofs;
                return result;
            }
            else
            {
                parseError("invalid try statement");
                return null;
            }
        }
        case throw_:
        {
            if (ThrowStatementAstNode ts = parseThrowStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.throwStatement = ts;
                return result;
            }
            else
            {
                parseError("invalid throw statement");
                return null;
            }
        }
        case leftCurly:
        {
            advance();
            BlockStatementAstNode b = new BlockStatementAstNode;
            if (parseDeclarationsOrStatements(b.declarationsOrStatements))
            {
                StatementAstNode result = new StatementAstNode;
                result.block = b;
                advance();
                return result;
            }
            else return null;
        }
        default:
        {
            if (ExpressionStatementAstNode es = parseExpressionStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.expression = es;
                return result;
            }
            else
            {
                return null;
            }
        }
        }
    }

    /**
     * Parses a Declaration.
     *
     * Returns: a $(D DeclarationAstNode) on success, $(D null) otherwise.
     */
    DeclarationAstNode parseDeclaration()
    {
        with(TokenType) switch(current.type)
        {
        case enum_:
        {
            if (EnumDeclarationAstNode decl = parseEnumDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.enumDeclaration = decl;
                return result;
            }
            else return null;
        }
        case interface_:
        {
            if (InterfaceDeclarationAstNode decl = parseInterfaceDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.interfaceDeclaration = decl;
                return result;
            }
            else return null;
        }
        case class_:
        {
            if (ClassDeclarationAstNode decl = parseClassDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.classDeclaration = decl;
                return result;
            }
            else return null;
        }
        case struct_:
        {
            if (StructDeclarationAstNode decl = parseStructDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.structDeclaration = decl;
                return result;
            }
            else return null;
        }
        case function_:
        {
            if (FunctionDeclarationAstNode decl = parseFunctionDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.functionDeclaration = decl;
                return result;
            }
            else return null;
        }
        case import_:
        {
            if (ImportDeclarationAstNode decl = parseImportDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.importDeclaration = decl;
                return result;
            }
            else return null;
        }
        case protection:
        {
            if (ProtectionDeclarationAstNode decl = parseProtectionDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.protectionOverwrite = decl;
                return result;
            }
            else return null;
        }
        case var, const_:
        {
            if (VariableDeclarationAstNode vd = parseVariableDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.variableDeclaration = vd;
                return result;
            }
            else return null;
        }
        case is_:
        {
            if (AkaDeclarationAstNode ad = parseAkaDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.akaDeclaration = ad;
                return result;
            }
            else return null;
        }
        case version_:
        {
            if (VersionBlockDeclarationAstNode decl = parseVersionBlockDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.versionBlockDeclaration = decl;
                return result;
            }
            else return null;
        }
        case at:
        {
            goto case function_;
        }
        case static_:
        {
            if (lookupNext.isTokFunction)
            {
                goto case function_;
            }
            else
            {
                return null;
            }
        }
        default:
            return null;
        }
    }

public:

    ///
    @disable this();

    /**
     * Constructs the parse with the lexer that contains the tokens to parse.
     */
    this(Lexer* lexer)
    {
        if (lexer)
        {
            _lexer = lexer;
            _range = Range(lexer.tokens);
            advance();
        }
    }

    /**
     * Main parser function. The function tries to parse from the main unit to
     * the last virtual unit (if any).
     *
     * Returns: An $(D UnitContainerAstNode) on success, $(D null) otherwise.
     */
    UnitContainerAstNode parse()
    {
        assert(current); // at least EOF token
        _uc = new UnitContainerAstNode;
        _uc.mainUnit = parseUnit();
        if (!_uc.mainUnit)
        {
            return null;
        }
        while (current.isTokVirtual)
        {
            advance();
            _uc.virtualUnits ~= parseUnit;
            if (!_uc.virtualUnits[$-1])
                return null;
        }
        /*if (!_range.empty)
        {
            unexpected;
            return null;
        }*/
        if (_errorCount)
            return null;
        else
            return _uc;
    }

    /// Returns: The AST for the unit being parsed.
    UnitContainerAstNode unitContainer()
    {
        return _uc;
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
        mixin("return parse" ~ fun ~ "(a[0..$]);");
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
    const TypeAstNode tan = prs.parseCustomNode!TypeAstNode();
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
    const TypeModifierAstNode tman = prs.parseCustomNode!TypeModifierAstNode();
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
    const TypeAstNode tan = prs.parseCustomNode!TypeAstNode();
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
    protection(public) import(10) a.b, c.d;
    virtual unit c;
    struct Owl
    {
        function of1(): s32;
        function of2(): u32
        {
            import c.v.b;
            function local(): c.v.b.FooBar {}
        }
    }
    interface Pig
    {
        function pig1(): s32;
        function pig2(): s64;
    }
    import (10101) a.b, c.d.r;
    static function exp(const var u64 value): s32
    {
        a = b + c;
        ++a;
        a++;
        a = b + c;
        ++--++a.a.a;
        (++a);
        call(a++,--b);
        a = b(c++);
        a = b + a(&c(*p.m));
        a = d:u32 + c;
        a = b[c+d:u8];
        a = call(param)[call(param)];
        a = b[c..d];
        a = b[c].d[e].f[g];
        (b[c].d[e]) = a;
        a = a * u;
        return;
        return b + c;
        break (Label1) a.call();
        break a.call();
        break;
        continue a.call();
        continue;
        a = b + 8;
        {
            foo.bar.baz();
        }
        var static s8 a = 8, b = 7;
        var s8[][] a;
        var MyInt mi = 8;
        var s8[2][4] b;
        var auto a = 8:s64;

        function a(s64 param): auto
        {}

        is function*(s64 p): s64 aka Prototype;

        const auto a = (b[0].b[1].b[2])(8);

        enum A : s8
        {
            a,
            b = 8,
            c,
            d = call()
        }

        class Foo : Bar.Bar , Baz{}

        function withIf()
        {
            if (!a)
            {
                call(a);
            }
        }
    }
`;
    Lexer lx;
    lx.setSourceFromText(source, __FILE__, line + 1, 1);
    lx.lex;

    Parser pr = Parser(&lx);
    DebugVisitor dv = new DebugVisitor();
    if (UnitContainerAstNode uc = pr.parse())
    {
        dv.visit(uc);
        //import std.process;
        //if ("CI" !in environment)
            dv.printText();
    }
}

/**
 * Tests valid code.
 *
 * Used to test that the input code can be parsed.
 *
 * Params:
 *     code = The source code.
 */
void assertParse(const(char)[] code, bool printAST = false,
    string file = __FILE_FULL_PATH__, size_t line = __LINE__)
{
    import core.exception: AssertError;

    Lexer lx;
    lx.setSourceFromText(code, file, line, 1);
    lx.lex;
    Parser pr = Parser(&lx);
    if (pr.parse() is null)
    {
        throw new AssertError("code not parsed but should be", file, line);
    }
    else if (printAST)
    {
        import yatol.parser.debug_visitor;
        DebugVisitor dv = new DebugVisitor;
        dv.visit(pr.unitContainer);
        dv.printText();
    }
}
///
unittest
{
    assertParse(q{
        unit a;
        function bar()
        {
            a++;
        }
    });
}

/**
 * Tests invalid code.
 *
 * Used to test that the input code cant be parsed.
 *
 * Params:
 *     code = The source code.
 */
void assertNotParse(const(char)[] code, string file = __FILE_FULL_PATH__,
    size_t line = __LINE__)
{
    import core.exception: AssertError;

    Lexer lx;
    lx.setSourceFromText(code, file, line, 1);
    lx.lex;
    Parser pr = Parser(&lx);
    if (pr.parse() !is null)
    {
        throw new AssertError("code parsed but should not be", file, line);
    }
}
///
unittest
{
    assertNotParse(q{
        unit a;
        function bar()
        {
            a ?= 0;
        }
    });
}

unittest
{
    import core.exception: AssertError;
    import std.exception: assertThrown;
    assertThrown!AssertError(assertParse(q{unit}));
    assertThrown!AssertError(assertNotParse(q{unit a;}));
}

unittest
{
    assertNotParse(q{
        unit a;
        function bar()
        {
            #;
        }
    });
}

unittest
{
    assertNotParse(`
        unit a;
        function foo()
        {
            a++;}
        }
    `);
}

unittest
{
    assertParse(q{
        unit a;
    });
}

unittest
{
    assertParse(q{
        unit a;
        virtual unit b;
        virtual unit c;
    });
}

unittest
{
    assertNotParse(q{
        unit
    });
}

unittest
{
    assertNotParse(q{
        unit ;
    });
}

unittest
{
    assertNotParse(q{
        unit a;
        unit b;
    });
}

unittest
{
    assertNotParse(q{
        unit a;
        virtual unit b;
        unit c;
    });
}

unittest
{
    assertParse(q{
        unit a;
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        virtual unit b;
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        virtual unit c;
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
    });
}

unittest
{
    assertParse(q{
        unit a;
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
        struct Foo {struct Foo {class Foo {struct Foo {interface Foo {}}}}}
    });
}

unittest
{
    assertNotParse(q{
        unit a;
        struct Foo struct Foo {class Foo {struct Foo {interface Foo {}}}}
    });
}

unittest
{
    assertParse(q{
        unit a.c.d.e.f.g.h.i.j.k.l.m.n.o.p.q.r.s.t.u.v.w.x.y.z;
    });
}

unittest
{
    assertParse(q{
        unit a.c;
        protection(private)
        protection(public)
        virtual unit v;
        protection(private)
        protection(public)
    });
}

unittest
{
    assertNotParse(q{
        unit struct.class.function;
    });
}

unittest
{
    assertParse(q{
        unit a;
        function bar()
        {
            a = b;
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        function bar()
        {
            a = b + c(d);
            d(a) = d(j);
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        function bar()
        {
            a = array[table[b + c(d)]];
        }
    });
}

unittest
{
    assertNotParse(q{
        unit a;
        function bar()
        {
            a = array[table[[b + c(d)]];
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        function bar()
        {
            a = array[call(param)];
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        function bar()
        {
            a = call(param)[call(param)];
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        function bar()
        {
            a = array[a..b];
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        function bar()
        {
            a = array[a][b][c];
        }
    });
}

unittest
{
    assertNotParse(q{
        unit a;
        function bar()
        {
            a = array[a..];
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        function bar()
        {
            a[a[a].a[a]].a[a] = b;
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        function bar(const u32 a,b,c; u64 d);
        function bar(u32 a,b,c; var u64 d){}
        function bar(u32 a,b,c; u64 d): u64;
        function bar(u32 a,b,c; u64 d): u64 {}
        function bar(function*(u32 a) callback): function*();
        var function *(const u32 a,b,c; u64 d) a;
        var function *(u32 a,b,c; var u64 d) a;
        var function *(u32 a,b,c; u64 d): u64 a;
        var function *(u32 a,b,c; u64 d): u64 a;
        var function *(function*(u32 a) callback): function*() a;
    });
}

unittest
{
    assertNotParse(q{
        unit a;
        function bar()
        {
            return
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        function bar()
        {
            return;
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        function bar(): s8
        {
            return a[a[a]];
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        function bar(): s8
        {
            return a[a] + c;
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        function bar(): s8
        {
            return a[a].a[a].b[0][1][2]++:u32:u64;
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        var s8 a = 8, b = 7, c;
        var SomeType[][] d, e, f;
        var static s32 a;
    });
}

unittest
{
    assertNotParse(q{
        unit a;
        s8 a = 8;
    });
}

unittest
{
    assertParse(q{
        unit a;
        var auto a = b[0..8];
    });
}

unittest
{
    assertParse(q{
        unit a;
        const auto a = (b[0].b[1].b[2])(8);
    });
}

unittest
{
    assertParse(q{
        unit a;
        function foo()
        {
            instances[a].instances[b] = 8;
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        enum A {a}
    });
}

unittest
{
    assertNotParse(q{
        unit a;
        enum A {a, b = 8, c = call(), d,}
    });
}

unittest
{
    assertNotParse(q{
        unit a;
        enum A {a,b = }
    });
}

unittest
{
    assertParse(q{
        unit a;
        class Foo : Foo, Bar.Bar {}
    });
}

unittest
{
    assertNotParse(q{
        unit a;
        class Foo : Foo, Bar.Bar, {}
    });
}

unittest
{
    assertNotParse(q{
        a;
    });
}

unittest
{
    assertNotParse(q{
        unit a-a;
    });
}

unittest
{
    assertParse(q{
        unit a;
        function main()
        {
            {
                call();
            }
        }
    });
}

unittest
{
    assertNotParse(q{
        unit a.+;
    });
}

unittest
{
    assertParse(q{
        unit a;
        function foo()
        {
            {
                statement = good;
            }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            {
                wrong statement
            }
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        var auto a = b;
    });
    assertNotParse(q{
        unit a;
        var auto a = b+;
    });
}

unittest
{
    assertNotParse(q{
        unit a;
        var auto a =+;
    });
}

unittest
{
    assertParse(q{
        unit a;
        function foo()
        {
            a = a[a..a];
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            a = a[a..a..a];
        }
    });
}

unittest
{
    assertParse(q{
        unit a;
        function foo()
        {
            a = b * c + d;
            a = b + c * d;
        }
    }, true);
}

unittest
{
    assertParse(q{
        unit a;
        function foo()
        {
            a += b;
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            *a + 8 += b;
        }
    });
}

unittest // cover error cases for: postfix exp and call params
{
    assertNotParse(q{
        unit a;
        function foo()
        {
            a(a a
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            a@
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            a:=
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            a(exp]
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            a = b?.c?.d;
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            a = b??.c?.d;
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            a = .b..c?.d;
        }
    });
}

unittest // cover error cases for: paren expression
{
    assertNotParse(q{
        unit a;
        function foo()
        {
            (a+a;
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            (]
        }
    });
}

unittest // cover error cases for: unary and dot expr
{
    assertNotParse(q{
        unit a;
        function foo()
        {
            ++++;
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            ++a.a.]
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            (a).]
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            (a).....(a);
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            a = "stringLiteral";
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            call("stringLiteral");
        }
    });
}

unittest // function and function type decl
{
    assertNotParse(q{
        unit a;
        function foo(;
    });
    assertNotParse(q{
        unit a;
        function foo;
    });
    assertNotParse(q{
        unit a;
        function foo;
    });
    assertNotParse(q{
        unit a;
        function foo(;
    });
    assertNotParse(q{
        unit a;
        function foo():;
    });
    assertNotParse(q{
        unit a;
        function ():;
    });
    assertNotParse(q{
        unit a;
        function a(): a k
    });
    assertNotParse("
        unit a;
        function a(){;
    ");
    assertNotParse(q{
        unit a;
        var function* foo(;
    });
    assertNotParse(q{
        unit a;
        var function* foo;
    });
    assertNotParse(q{
        unit a;
        var function* foo;
    });
    assertNotParse(q{
        unit a;
        var function foo;
    });
    assertNotParse(q{
        unit a;
        var function *(;
    });
    assertNotParse(q{
        unit a;
        var function *():;
    });
    assertNotParse(q{
        unit a;
        function foo(): static
    });
    assertParse(q{
        unit a;
        function foo(const const s32 a);
    });
    assertParse(q{
        unit a;
        function foo(var var s32 a);
    });
    assertParse(q{
        unit a;
        function foo(var const var const s32 a);
    });
    assertNotParse(q{
        unit a;
        function foo(var const var s32 a,,);
    });
    assertParse(q{
        unit a;
        function foo(var const var s32 a,b; const C c; const D d;);
    });
    assertParse(q{
        unit a;
        function foo(var const var s32 a,b; const C c; const D d);
    });
    assertParse(q{
        unit a;
        @virtual function foo(){}
    });
    assertParse(q{
        unit a;
        @abstract @virtual function foo();
    });
    assertNotParse(q{
        unit a;
        @abstract,virtual function foo();
    });
    assertNotParse(q{
        unit a;
        @abstract,virtual foo();
    });
    assertNotParse(q{
        unit a;
        @++,virtual foo();
    });
}

unittest // cover error cases for: protection declaration
{
    assertNotParse(q{
        unit a;
        protection;
    });
    assertNotParse(q{
        unit a;
        protection(private;a
    });
    assertNotParse(q{
        unit a;
        protection(p
    });
    assertNotParse(q{
        unit a;
        protection m
    });
    assertNotParse(q{
        unit a;
        protection()
    });
}

unittest // cover error cases for: variable declaration
{
    assertNotParse(q{
        unit a;
        var a var a;
    });
    assertNotParse(q{
        unit a;
        var a = var ;
    });
    assertNotParse(q{
        unit a;
        var T a = var 0;
    });
    assertNotParse(q{
        unit a;
        var T a = 0 var
    });
    assertNotParse(q{
        unit a;
        var T a]
    });
    assertNotParse(q{
        unit a;
        var T ++;
    });
    assertNotParse(q{
        unit a;
        var const T ++;
    });
}

unittest // cover error cases for: continue break return statements
{
    assertNotParse(q{
        unit a;
        function foo()
        {
            break
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            break a
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            break(
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            break( a
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            continue
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            continue )
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            continue a
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            return
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            return a
        }
    });
}

unittest // cover error cases for: import declaration
{
    assertNotParse(q{
        unit a;
        import;
    });
    assertNotParse(q{
        unit a;
        import a.;
    });
    assertNotParse(q{
        unit a;
        import("tant") a;
    });
    assertNotParse(q{
        unit a;
        import() a;
    });
    assertNotParse(q{
        unit a;
        import(0) a
    });
    assertNotParse(q{
        unit a;
        import(a) a;
    });
    assertNotParse(q{
        unit a;
        import(0 a;
    });
}

unittest // cover error cases for: interface, struct & class
{
    assertNotParse(q{
        unit a;
        class {}
    });
    assertNotParse(q{
        unit a;
        struct {}
    });
    assertNotParse(q{
        unit a;
        interface {}
    });
    assertNotParse("
        unit a;
        class A}
    ");
    assertNotParse("
        unit a;
        struct A}
    ");
    assertNotParse("
        unit a;
        interface A}
    ");
    assertNotParse(q{
        unit a;
        class A : {}
    });
    assertNotParse(q{
        unit a;
        class A : B.C, {}
    });
    assertNotParse(q{
        unit a;
        interface A : B.C, {}
    });
    assertNotParse(q{
        unit a;
        class A : B.C, D. {}
    });
    assertNotParse(q{
        unit a;
        interface A : B.C, D. {}
    });
    assertNotParse(q{
        unit a;
        struct A : {}
    });
    assertNotParse(q{
        unit a;
        interface A : {}
    });
    assertNotParse(q{
        unit a;
        class A : A A{}
    });
    assertNotParse(q{
        unit a;
        interface A : A A{}
    });
    assertNotParse("
        unit a;
        class A{ var s8 a;
    ");
    assertNotParse("
        unit a;
        struct A{ var s8 a;
    ");
    assertNotParse("
        unit a;
        interface A{ var s8 a;
    ");
}

unittest // cover error cases for: enum
{
    assertNotParse(q{
        unit a;
        enum A : {}
    });
    assertNotParse("
        unit a;
        enum A a {
    ");
    assertNotParse(q{
        unit a;
        enum A {a a}
    });
    assertNotParse(q{
        unit a;
        enum  {a}
    });
    assertNotParse(q{
        unit a;
        enum  {a,++}
    });
    assertNotParse(q{
        unit a;
        enum  A {}
    });
    assertNotParse(q{
        unit a;
        enum  A {a,}
    });
    assertNotParse(q{
        unit a;
        enum  A {a = 4; 3 2 1 0}
    });
}

unittest // cover error cases for: aka
{
    assertNotParse(q{
        unit a;
        is;
    });
    assertNotParse(q{
        unit a;
        is a;
    });
    assertNotParse(q{
        unit a;
        is a aka
    });
    assertNotParse(q{
        unit a;
        is a aka other
    });
}

unittest // super
{
    assertParse(q{
        unit a;
        class A : B { var super superInstance;}
    });
    assertNotParse(q{
        unit a;
        class A : B { super superInstance;}
    });
    assertParse(q{
        unit a;
        class A : B { function foo() {super.a();}}
    });
    assertParse(q{
        unit a;
        class A : B { function foo() {var auto b = super.a;}}
    });
    assertParse(q{
        unit a;
        class A : B { function foo() {super;}}
    });
    assertNotParse(q{
        unit a;
        class A : B { function foo() {super a;}}
    });
    assertNotParse(q{
        unit a;
        class A : B { function foo() {super .;}}
    });
    assertParse(q{
        unit a;
        class A : B { function foo() : super {return this:super;}}
    });
}

unittest // while
{
    assertParse(q{
        unit a;
        function foo()
        {
            while (true) {}
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            while (a.b == true) {}
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            while (true)
                while (true)
                    {++a;}
        }
    });
    assertNotParse("
        unit a;
        function foo()
        {
            while (a) { a a a
        }
    ");
    assertNotParse(q{
        unit a;
        function foo()
        {
            while (a) unit
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            while ( {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            while () {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            while (true {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            while true {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            while (true; {}
        }
    });
}

unittest // foreach
{
    assertParse(q{
        unit a;
        function foo()
        {
            foreach(const s8 a; b) {}
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            foreach(const s8 a; 0..10) {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            foreach(const s8 a; 0..) {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            foreach(const s8 a; ..) {}
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            foreach(const auto a; b) {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            foreach(a; b) {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            foreach(; b) {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            foreach(a;a a) {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            foreach a; b {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            foreach(const auto a; b) a a
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            foreach(const auto a; b) ;
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            foreach(var auto a; b]) {}
        }
    });
    assertNotParse(`
        unit a;
        function foo()
        {
            foreach(var auto a; b]) {a]
        }
    `);
    assertNotParse(q{
        unit a;
        function foo()
        {
            foreach(auto a; b] {}
        }
    });
    assertNotParse(`
        unit a;
        function foo()
        {
            foreach(auto a; b) { a++; a a a
        }
    `);
    assertNotParse(`
        unit a;
        function foo()
        {
            foreach(const auto a; b) {a a;++
        }
    `);
}

unittest // misc. coverage for errors
{
    assertNotParse(q{
        unit a;
        var auto a = 8
        virtual unit b;
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            a = 8
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            a =;
        }
    });
    assertNotParse(q{
        unit a;
        A[0 b a;
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            a =
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            a = 8: ;
        }
    });
    assertNotParse(q{
        unit a;
        enum A : s8[8:;
    });
    assertNotParse(q{
        unit a;
        function foo(s8[8 a);
    });
    assertNotParse(q{});
    assertNotParse(q{
        unit a;
        static identifier;
    });
    assertNotParse(q{
        unit a;
        struct Foo
        {
            unit a;
        }
    });
    assertNotParse(q{
        unit a;
        struct Foo
        {
            virtual unit a;
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            if (true) {} else a a;
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            if (true) a a else a a;
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            if (true) {}
            else if (true) {}
            else {}
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            if (const auto a = 0) {}
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            if (const s8 a = call()) {}
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            if (var A.B a = call() && other()) {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            if (var 0) {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            if (const s8 a = 0; {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            if (const s8 0 = 0) {}
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            a = b % c;
        }
    });
}

unittest // if else
{
    assertParse(q{
        unit a;
        function foo()
        {
            if (a)
                call(b);
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            if (a)
            {
                call(b);
            }
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            if (a)
            {
                call(b);
                call(b);
            }
            else
            {
                how(c);
                how(c);
            }
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            if (a)
                call(b);
            else
                can(c);
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            if (a)
                ;
            else
                ;
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            if (a)
            else
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            if (a) {
            else }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            if a) {
            else }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            if (a {
            else }
        }
    });
    assertNotParse("
        unit a;
        function foo()
        {
            if (a {
            else a a a
        }
    ");
    assertNotParse("
        unit a;
        function foo()
        {
            if (a {}
            else {
        }
    ");
    assertNotParse("
        unit a;
        function foo()
        {
            if (a {}
            else a a a {
        }
    ");
    assertParse(q{
        unit a;
        function foo()
        {
            a = false;
            a = true;
            a = null;
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            if (true; {}
        }
    });
    assertNotParse(`
        unit a;
        function foo()
        {
            if (true) {}
            else {a a;++
        }
    `);
}

unittest // switch
{
    assertParse(q{
        unit a;
        function foo()
        {
            switch(a)
            {
                on (0,1) a++;
                else a--;
            }
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            switch(call())
            {
                on (0,1) a++;
                on (1,2) a++;
            }
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            switch(a)
            {
                on (0..2) a++;
                else a--;
            }
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            switch(a)
            {
                on (call1()..call2()) a++;
                else a--;
            }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch(a)
            {
                on (call1()..++) a++;
                else a--;
            }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch(call())
            {
                (0,1) a++;
            }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch(call())
            {
                0,1) a++;
            }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch(call())
            {
                (0,1) a++ + +
            }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch(call())
            {
                on (0,1) a++;
                a++;
            }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch(call())
            {
                on 0,1) a++;
                a++;
            }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch(call())
            {
                on (0;+;
            }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch(call())
            {
                on (,
            }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch(call())
            {
                on (0,1) a++
            }
        }
    });
    assertNotParse("
        unit a;
        function foo()
        {
            switch(call())
            {
                on (0,1) a++;

        }
    ");
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch(::
            {
                on (0,1) a++
            }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch(a
            {
                on (0,1) a++
            }
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch(a)
                on (0,1) a++
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch a
                on (0,1) a++
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            switch(a)
            {
                on (0,1) a++;
                else :;
            }
        }
    });
    assertNotParse("
        unit a;
        function foo()
        {
            switch(a)
            {
                on (0,1) a++;
                else {}

        }
    ");
}

unittest // initializer
{
    assertParse(q{
        unit a;
        const auto a = 0:s8;
    });
    assertNotParse(q{
        unit a;
        const auto a 0:s8;
    });
    assertParse(q{
        unit a;
        const auto a = [];
    });
    assertParse(q{
        unit a;
        const auto a = [0];
    });
    assertParse(q{
        unit a;
        const auto a = [0,1];
    });
    assertParse(q{
        unit a;
        const auto a = [[0,1]];
    });
    assertParse(q{
        unit a;
        const auto a = [[0,1],[2,3]];
    });
    assertNotParse(q{
        unit a;
        const auto a = [0,1],[2,3]];
    });
    assertNotParse(q{
        unit a;
        const auto a = [0,1][2,3]];
    });
    assertNotParse(q{
        unit a;
        const auto a = 0,1],[2,3]];
    });
    assertNotParse(q{
        unit a;
        const auto a = [;
    });
    assertNotParse(q{
        unit a;
        const auto a = ;
    });
    assertParse(q{
        unit a;
        function foo()
        {
            var s8[] a;
            a = [0,1];
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            var s8[] a;
            a = [const
        }
    });
}

unittest // version
{
    assertParse(q{
        unit a;
        version(a) const s8 b;
    });
    assertNotParse(q{
        unit a;
        version a) const s8 b;
    });
    assertNotParse(q{
        unit a;
        version (a const s8 b;
    });
    assertParse(q{
        unit a;
        version(a) const s8 b; else const s16 b;
    });
    assertParse(q{
        unit a;
        version(a | b) const s8 c;
    });
    assertNotParse(q{
        unit a;
        version(a |) const s8 c;
    });
    assertParse(q{
        unit a;
        version(a & b) const s8 c;
    });
    assertNotParse(q{
        unit a;
        version(a &) const s8 c;
    });
    assertNotParse(q{
        unit a;
        version(&) const s8 c;
    });
    assertNotParse(q{
        unit a;
        version const s8 c;
    });
    assertParse(q{
        unit a;
        version(a & b | c) const s8 d;
    });
    assertParse(q{
        unit a;
        version(a | b & c) const s8 d;
    });
    assertParse(q{
        unit a;
        version((a | b) & c) const s8 d;
    });
    assertParse(q{
        unit a;
        version((a | b) & (c)) const s8 d;
    });
    assertParse(q{
        unit a;
        version((a | b) & (c | d)) const s8 e;
    });
    assertParse(q{
        unit a;
        version(a) {const s8 b;} else {const s16 b;}
    });
    assertNotParse(q{
        unit a;
        version() {const s8 b;}
    });
    assertNotParse(q{
        unit a;
        version(a & ;) {const s8 b;}
    });
    assertNotParse(q{
        unit a;
        version(a & ()) {const s8 b;}
    });
    assertNotParse(q{
        unit a;
        version(a) const
    });
    assertNotParse(q{
        unit a;
        version(a) {const s8 b;} else const
    });
}

unittest // try
{
    assertParse(q{
        unit a;
        function foo()
        {
            try something();
            on(Error e) doThat();
            finally doThis();
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            try something();
            finally doThis();
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            try something();
            on(Error e, Oops o){doThat();doThat();}
            finally doThis();
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            try {something();}
            on(Error e, Oops o){doThat();doThat();}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            try finally doThis();
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            try something();
            on ();
            finally doThis();
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            try something();
            on (E,K);
            finally doThis();
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            try const
            on (E,K);
            finally doThis();
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            try this();
            on (E e) const
            finally doThis();
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            try thjis();
            on (E e) doThat();
            finally const
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            try this();
            on E e) doThat();
            finally doThis();
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            try this();
            on (E e doThat();
            finally doThis();
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            try this();
            on (const e) doThat();
            finally doThis();
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            try thise();
            on (E.const e) doThat();
            finally doThis();
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            try doThis();
            on (E e) handleE();
            on (F f) handleF();
            finally doThis();
        }
    });
}

unittest // throw
{
    assertParse(q{
        unit a;
        function foo()
        {
            throw Exception.create("boom");
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            throw Exception.create("boom")
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            throw = Exception.create("boom")
        }
    });
}

unittest // issue #1 ambiguous type modifiers when return is a func
{
    assertParse(q{
        unit a;
        const (s8)[] a;
    });
    assertNotParse(q{
        unit a;
        const s8)[] a;
    });
    assertNotParse(q{
        unit a;
        const (s8[] a;
    });
    assertNotParse(q{
        unit a;
        const (s8[]) a;
    });
    assertNotParse(q{
        unit a;
        const (auto) a;
    });
    assertParse(q{
        unit a;
        const (function*():s8[])[] a;
    });
    assertParse(q{
        unit a;
    var function*():(function*():s8[])[] a;
    });
}

