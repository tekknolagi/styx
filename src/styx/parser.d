#!dmd -g -gs
/**
 * Styx's parser.
 */
module styx.parser;

import
    core.stdc.stdlib;
import
    std.stdio, std.format, std.algorithm;
import
    styx.token, styx.lexer, styx.ast, styx.session;

/// The parser
struct Parser
{

private:

    Lexer* _lexer;
    Token* _current;
    ptrdiff_t _declarationLevels;
    size_t _errorCount;
    UnitAstNode _u;

    alias Range = TokenRange!(TokenType.lineComment, TokenType.starComment);
    Range _range;

    void warning(const(char[]) message)
    {
        assert(_current);
        session.warn(_lexer.filename, _current.position, message);
    }

    void parseError(const(char[]) message)
    {
        assert(_current);
        ++_errorCount;
        session.error(_lexer.filename,_current.position, message);
    }

    void expected(TokenType expected, string loc = __FUNCTION__, int line = __LINE__)
    {
        ++_errorCount;
        static immutable string specifierDiff = "expected `%s` instead of `%s`";
        //static immutable string specifierSame = "expected supplemental `%s`";
        assert(current.type != expected);
        parseError(specifierDiff.format(tokenString(expected), _current.text));
        //else
        //    parseError(specifierSame.format(tokenString(expected)));
    }

    void unexpected(string loc = __FUNCTION__, int line = __LINE__)
    {
        version(show_error_origin)
            session.info("error originated from %s at line %d", loc, line);
        ++_errorCount;
        static immutable string specifier = "unexpected `%s`";
        parseError(specifier.format(_current.text));
    }

    pragma(inline, true)
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
     * Parses an AssertStatement.
     *
     * Returns:
     *      $(D AssertStatementAstNode) on success, $(D null) otherwise.
     */
    AssertStatementAstNode parseAssertStatement()
    {
        assert(current.isTokAssert);
        AssertStatementAstNode result = new AssertStatementAstNode;
        result.startPos = current.position;
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        if (ExpressionAstNode e = parseExpression(null, TokenType.rightParen))
        {
            result.expression = e;
            if (!current.isTokRightParen)
            {
                expected(TokenType.rightParen);
                return null;
            }
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
     * Parses a UnitDeclaration.
     *
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
                result.startPos = current.position;
                result.identifiers = toks;
                advance();
                if (DeclarationsAstNode d = parseDeclarations())
                {
                    result.declarations = d;
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
    IdentifierChainAstNode parseIdentifierChain()
    {
        assert(current.isTokIdentifier);
        IdentifierChainAstNode result = new IdentifierChainAstNode;
        result.startPos = current.position();
        while (true)
        {
            result.chain ~= current();
            advance();
            if (current.isTokDot)
            {
                advance();
                if (!current.isTokIdentifier)
                {
                    expected(TokenType.identifier);
                    return null;
                }
                continue;
            }
            else return result;
        }
    }

    InitializerAstNode parseInitializer()
    {
        InitializerAstNode result = new InitializerAstNode;
        result.startPos = current.position();
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
        else if (ExpressionAstNode e = parseExpression(null, TokenType.semiColon))
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
     * Parses a TypeModifier.
     *
     * Returns:
     *      On success a $(D TypeModifierAstNode) otherwise $(D null).
     */
    TypeModifierAstNode parseTypeModifier()
    {
        TypeModifierAstNode result = new TypeModifierAstNode;
        result.startPos = current.position();
        TypeModifierAstNode lastMd = result;
        result.startPos = current.position;
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
                else if (ExpressionAstNode e = parseExpression(null, TokenType.rightSquare))
                {
                    lastMd.kind = ModifierKind.arrayStatDim;
                    lastMd.staticDimension = e;
                    if (!current.isTokRightSquare)
                    {
                        expected(TokenType.rightSquare);
                        return null;
                    }
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
     * Parses a TemplateParameters.
     *
     * Returns:
     *      On success a $(D TemplateParametersAstNode) otherwise $(D null).
     */
    TemplateParametersAstNode parseTemplateParameters()
    {
        assert(current.isTokLesser);
        TemplateParametersAstNode result = new TemplateParametersAstNode;
        result.startPos = current.position;
        advance();
        while (current.isTokIdentifier)
        {
            result.parameters ~= current;
            advance();
            if (current.isTokComma)
            {
                advance();
                continue;
            }
            else break;
        }
        if (!current.isTokGreater)
        {
            expected(TokenType.greater);
            return null;
        }
        else
        {
            advance();
            return result;
        }
    }

    /**
     * Parses a TemplateInstance.
     *
     * Returns:
     *      On success a $(D TemplateInstanceAstNode) otherwise $(D null).
     */
    TemplateInstanceAstNode parseTemplateInstance()
    {
        assert(current.isTokLesser);
        TemplateInstanceAstNode result = new TemplateInstanceAstNode;
        result.startPos = current.position;
        advance();
        while (true)
        {
            if (TypeAstNode t = parseType())
                result.types ~= t;
            else
                parseError("invalid template parameter specialization");
            if (current.isTokComma)
            {
                advance();
                continue;
            }
            else break;
        }
        if (!current.isTokGreater)
        {
            expected(TokenType.greater);
            return null;
        }
        else
        {
            advance();
            return result;
        }
    }

    /**
     * Parses the unambiguous form of a Type.
     *
     * Returns:
     *      On success a $(D TypeAstNode) otherwise $(D null).
     */
    TypeAstNode parseUnambiguousType()
    {
        assert(current.isTokLeftParen);
        advance();
        TypeAstNode result = parseType();
        if (!current.isTokRightParen)
        {
            expected(TokenType.rightParen);
            return null;
        }
        advance();
        if (current.isTokMul || current.isTokLeftSquare)
        {
            if (result.modifier)
            {
                parseError("type modifiers already specified before right paren");
                return null;
            }
            if (TypeModifierAstNode mod = parseTypeModifier())
            {
                result.modifier = mod;
            }
            else return null;
        }
        return result;
    }

    /**
     * Parses a TypeIdentifierPart.
     *
     * Returns:
     *      On success a $(D TypeIdentifierPartAstNode) otherwise $(D null).
     */
    TypeIdentifierPartAstNode parseTypeIdentifierPart()
    {
        assert(current.isTokIdentifier);
        TypeIdentifierPartAstNode result = new TypeIdentifierPartAstNode;
        result.startPos = current.position();
        result.identifier = current();
        advance();
        if (current.isTokLesser)
        {
            if (TemplateInstanceAstNode ti = parseTemplateInstance())
            {
                result.templateInstance = ti;
            }
            else return null;
        }
        if (current.isTokDot && lookupNext.isTokIdentifier)
        {
            advance();
            if (TypeIdentifierPartAstNode tip = parseTypeIdentifierPart())
            {
                result.nextPart = tip;
            }
            else return null;
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
        TypeAstNode result;
        if (current.isTokAuto)
        {
            result = new TypeAstNode;
            result.startPos = current.position;
            result.autoOrBasicType = current();
            advance();
            return result;
        }
        if (current.isTokLeftParen)
        {
            return parseUnambiguousType();
        }

        result = new TypeAstNode;
        result.startPos = current.position;
        if (current.isTokBasicType)
        {
            result.autoOrBasicType = current();
            advance();
        }
        else if (current.isTokFunction || current.isTokStatic)
        {
            result.functionType = parseFunctionDeclaration(true);
            if (!result.functionType)
            {
                parseError("invalid function type");
                return null;
            }
        }
        else
        {
            if (!current.isTokIdentifier)
            {
                expected(TokenType.identifier);
                return null;
            }
            else if (TypeIdentifierPartAstNode tip = parseTypeIdentifierPart())
            {
                result.typeIdentifierPart = tip;
            }
            else
            {
                parseError("invalid type identifier parts");
                return null;
            }
        }
        if (current.isTokMul || current.isTokLeftSquare)
        {
            if (TypeModifierAstNode mod = parseTypeModifier())
            {
                result.modifier = mod;
            }
            else return null;
        }
        return result;
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
        result.startPos = current.position();
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
        result.startPos = current.position;
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        result.name = current();
        advance();
        if (current.isTokLesser)
        {
            if (TemplateParametersAstNode tp = parseTemplateParameters())
            {
                result.templateParameters = tp;
            }
            else
            {
                parseError("invalid template parameters");
                return null;
            }
        }
        if (current.isTokColon)
        {
            advance();
            if (!current.isTokIdentifier)
            {
                expected(TokenType.identifier);
                return null;
            }
            while (true)
            {
                if (IdentifierChainAstNode ic = parseIdentifierChain())
                {
                    result.inheritanceList ~= ic;
                }
                else return null;
                if (current.isTokComma)
                {
                    advance();
                    if (!current.isTokIdentifier)
                    {
                        expected(TokenType.identifier);
                        return null;
                    }
                    else continue;
                }
                else break;
            }
        }
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            return null;
        }
        advance();
        if (DeclarationsAstNode d = parseDeclarations())
        {
            result.declarations = d;
        }
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
            return null;
        }
        result.stopPos = current.position;
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
        result.startPos = current.position;
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        result.name = current();
        advance();
        if (current.isTokLesser)
        {
            if (TemplateParametersAstNode tp = parseTemplateParameters())
            {
                result.templateParameters = tp;
            }
            else
            {
                parseError("invalid template parameters");
                return null;
            }
        }
        if (current.isTokColon)
        {
            advance();
            if (!current.isTokIdentifier)
            {
                expected(TokenType.identifier);
                return null;
            }
            while (true)
            {
                if (IdentifierChainAstNode ic = parseIdentifierChain())
                {
                    result.inheritanceList ~= ic;
                }
                else return null;
                if (current.isTokComma)
                {
                    advance();
                    if (!current.isTokIdentifier)
                    {
                        expected(TokenType.identifier);
                        return null;
                    }
                    else continue;
                }
                else break;
            }
        }
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            return null;
        }
        advance();
        if (DeclarationsAstNode d = parseDeclarations())
        {
            result.declarations = d;
        }
        else return null;
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
            return null;
        }
        result.stopPos = current.position;
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
        result.startPos = current.position;
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        result.name = current();
        advance();
        if (current.isTokLesser)
        {
            if (TemplateParametersAstNode tp = parseTemplateParameters())
            {
                result.templateParameters = tp;
            }
            else
            {
                parseError("invalid template parameters");
                return null;
            }
        }
        if (current.isTokColon)
        {
            advance();
            if (!current.isTokIdentifier)
            {
                expected(TokenType.identifier);
                return null;
            }
            while (true)
            {
                if (IdentifierChainAstNode ic = parseIdentifierChain())
                {
                    result.duckTypeList ~= ic;
                }
                else return null;
                if (current.isTokComma)
                {
                    advance();
                    if (!current.isTokIdentifier)
                    {
                        expected(TokenType.identifier);
                        return null;
                    }
                    else continue;
                }
                else break;
            }
        }
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            return null;
        }
        advance();
        if (DeclarationsAstNode d = parseDeclarations())
        {
            result.declarations = d;
        }
        else return null;
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
            return null;
        }
        result.stopPos = current.position;
        advance();
        return result;
    }

    /**
     * Parses an UnionDeclaration.
     *
     * Returns:
     *      On success a $(D StructDeclarationAstNode) otherwise $(D null).
     */
    UnionDeclarationAstNode parseUnionDeclaration()
    {
        assert(current.isTokUnion);
        UnionDeclarationAstNode result = new UnionDeclarationAstNode;
        result.startPos = current.position;
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        result.name = current();
        advance();
        if (current.isTokLesser)
        {
            if (TemplateParametersAstNode tp = parseTemplateParameters())
            {
                result.templateParameters = tp;
            }
            else
            {
                parseError("invalid template parameters");
                return null;
            }
        }
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            return null;
        }
        advance();
        if (DeclarationsAstNode d = parseDeclarations())
        {
            result.declarations = d;
        }
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
            return null;
        }
        result.stopPos = current.position;
        advance();
        return result;
    }

    /**
     * Parses an TemplateDeclaration.
     *
     * Returns:
     *      On success a $(D TemplateDeclarationAstNode) otherwise $(D null).
     */
    TemplateDeclarationAstNode parseTemplateDeclaration()
    {
        assert(current.isTokTemplate);
        TemplateDeclarationAstNode result = new TemplateDeclarationAstNode;
        result.startPos = current.position;
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        result.name = current();
        advance();
        if (!current.isTokLesser)
        {
            expected(TokenType.lesser);
            return null;
        }
        if (TemplateParametersAstNode tp = parseTemplateParameters())
        {
            result.templateParameters = tp;
        }
        else
        {
            parseError("invalid template parameters");
            return null;
        }
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            return null;
        }
        advance();
        if (DeclarationsAstNode d = parseDeclarations())
        {
            result.declarations = d;
        }
        else return null;
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
        result.startPos = current.position();
        if (ExpressionAstNode e1 = parseExpression(null))
        {
            result.singleOrLeftExpression = e1;
            if (current.isTokDotDot)
            {
                advance();
                if (ExpressionAstNode e2 = parseExpression(null, TokenType.rightSquare))
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
        result.startPos = current.position();
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
        if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
        {
            result.declarationOrStatement = dos;
            return result;
        }
        else
        {
            parseError("invalid on match declaration or statement");
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
        result.startPos = current.position();
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
            if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
            {
                result.elseStatement = dos;
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
        result.startPos = current.position;
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
        result.startPos = current.position;
        advance();
        if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
        {
            result.triedDeclarationOrStatement = dos;
        }
        else return null;
        while (current.isTokOn)
        {
            if (OnExceptionStatementAstNode oes = parseOnExceptionStatemtent())
            {
                result.exceptionDeclarationsOrStatements ~= oes;
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
            if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
            {
                result.finalDeclarationOrStatement = dos;
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
        result.startPos = current.position();
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
        if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
        {
            result.exceptionsDeclarationOrStatement = dos;
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
        result.startPos = current.position();
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
     * Parses a LabelStatement
     *
     * Returns:
     *      On success a $(D parseLabelStatement) otherwise $(D null).
     */
    LabelDeclarationstAstNode parseLabelDeclaration()
    {
        assert(current.isTokLabel);
        LabelDeclarationstAstNode result = new LabelDeclarationstAstNode;
        result.startPos = current.position;
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        result.identifier = current;
        advance();
        if (!current.isTokSemicolon)
        {
            expected(TokenType.semiColon);
            return null;
        }
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
        result.name = current();
        result.startPos = current.position();
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
        result.startPos = current.position;
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
                    result.stopPos = current.position;
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
        result.startPos = current.position;
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
            if (IdentifierChainAstNode ic = parseIdentifierChain())
            {
                result.importList ~= ic;
            }
            else return null;
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
     * Parses an AtAttribute.
     *
     * Returns:
     *      On success a $(D AtAttributeAstNode) otherwise $(D null).
     */
    AtAttributeAstNode parseAtAttribute()
    {
        assert(current.isTokAt);
        AtAttributeAstNode result = new AtAttributeAstNode;
        result.startPos = current.position();
        advance();
        if (!current.isTokKeyword && !current.isTokIdentifier)
        {
            parseError("expected a keyword or an identifier after `@`");
            return null;
        }
        result.identifierOrKeyword = current();
        advance();
        if (current.isTokLeftParen)
        {
            advance();
            while (true)
            {
                if (PrimaryExpressionAstNode pe = parsePrimaryExpression())
                {
                    result.parameters ~= pe;
                    if (current.isTokRightParen)
                    {
                        advance();
                        break;
                    }
                    else if (current.isTokComma)
                    {
                        advance();
                    }
                    else
                    {
                        unexpected();
                        return null;
                    }
                }
                else return null;
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
    FunctionDeclarationAstNode parseFunctionDeclaration(bool asType)
    {
        FunctionDeclarationAstNode result = new FunctionDeclarationAstNode;
        result.startPos = current.position;
        if (current.isTokStatic)
        {
            result.isStatic = true;
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
            if (!asType)
            {
                expected(TokenType.identifier);
                return null;
            }
        }
        else
        {
            result.name = current();
            advance();
        }
        if (current.isTokLesser)
        {
            if (TemplateParametersAstNode tp = parseTemplateParameters())
            {
                result.templateParameters = tp;
            }
            else
            {
                parseError("invalid template parameters");
                return null;
            }
        }
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
        if (asType)
        {
            return result;
        }
        if (!current.isTokLeftCurly && !current.isTokSemicolon)
        {
            parseError("expected `;` or `{` to skip or start the function body");
            return null;
        }
        result.firstBodyToken = current();
        if (current.isTokLeftCurly)
        {
            advance();
            if (DeclarationsOrStatementsAstNode dos = parseDeclarationsOrStatements())
            {
                result.declarationsOrStatements = dos;
            }
            else
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
        result.startPos = current.position;
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
        result.startPos = current.position;
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
        VariableDeclarationAstNode result = new VariableDeclarationAstNode;
        result.startPos = current.position();
        bool isStatic;
        if (current.isTokStatic)
        {
            isStatic = true;
            advance();
        }
        if (current.isTokStorageClass)
        {
            result.storageClass = current;
            advance();
        }
        else
        {
            parseError("expected a storage class");
            return null;
        }
        if (TypeAstNode t = parseType())
        {
            result.type = t;
            result.isStatic = isStatic;
            result.startPos = current.position;
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
        assert(current.isTokAka);
        AkaDeclarationAstNode result = new AkaDeclarationAstNode;
        result.startPos = current.position;
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        result.name = current();
        advance();
        if (current.isTokEqual)
        {
            advance();
            if (TypeAstNode t = parseType())
            {
                result.type = t;
            }
            else return null;
        }
        else
        {
            expected(TokenType.equal);
            return null;
        }
        if (!current.isTokSemicolon)
        {
            expected(TokenType.semiColon);
            return null;
        }
        advance();
        return result;
    }

    /**
     * Parses a DeclarationOrStatementAstNode.
     *
     * Returns:
     *      A $(D DeclarationOrStatementAstNode) on success, $(D null) otherwise.
     */
    DeclarationOrStatementAstNode parseDeclarationOrStatement()
    {
        if (!current.isTokVersion)
        {
            if (DeclarationAstNode d = parseDeclaration())
            {
                DeclarationOrStatementAstNode result = new DeclarationOrStatementAstNode;
                result.startPos = current.position;
                result.declaration = d;
                return result;
            }
        }
        if (StatementAstNode s = parseStatement())
        {
            DeclarationOrStatementAstNode result = new DeclarationOrStatementAstNode;
            result.startPos = current.position;
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
     * Returns:
     *      A $(D DeclarationsOrStatementsAstNode) on success, $(D null) otherwise.
     */
    DeclarationsOrStatementsAstNode parseDeclarationsOrStatements()
    {
        DeclarationsOrStatementsAstNode result = new DeclarationsOrStatementsAstNode;
        result.startPos = current.position;
        const ptrdiff_t oldDeclLvl = _declarationLevels;
        ++_declarationLevels;

        while (true)
        {
            with (TokenType) switch (current.type)
            {
            case eof:
                return result;
            case rightCurly:
                --_declarationLevels;
                assert (oldDeclLvl == _declarationLevels);
                return result;
            default:
                if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
                {
                    result.items ~= dos;
                    continue;
                }
                else
                {
                    unexpected();
                    return null;
                }
            }
        }
    }

    /**
     * Parses contiguous declarations.
     *
     * Returns: a $(D DeclarationsAstNode) on success, $(D null) otherwise.
     */
    DeclarationsAstNode parseDeclarations()
    {
        DeclarationsAstNode result = new DeclarationsAstNode;
        result.startPos = current.position;
        const ptrdiff_t oldDeclLvl = _declarationLevels;
        ++_declarationLevels;
        while (true)
        {
            with (TokenType) switch (current.type)
            {
            case eof:
                return result;
            case rightCurly:
                --_declarationLevels;
                assert(oldDeclLvl == _declarationLevels);
                return result;
            default:
                if (DeclarationAstNode d = parseDeclaration())
                {
                    result.items ~= d;
                    continue;
                }
                else
                {
                    unexpected();
                    return null;
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
        result.startPos = current.position;
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
     * Parses an EchoParameter.
     *
     * Returns: a $(D EchoParameterAstNode) on success, $(D null) otherwise.
     */
    EchoParameterAstNode parseEchoParameter()
    {
        EchoParameterAstNode result = new EchoParameterAstNode;
        result.startPos = current.position();
        if (current.isTokKeyword)
        {
            result.keyword = current.type;
            advance();
        }
        else if (current.isTokLeftCurly)
        {
            advance();
            if (ExpressionAstNode e = parseExpression(null, TokenType.rightCurly))
            {
                result.expression = e;
                advance();
                if (!current.isTokRightCurly)
                {
                    expected(TokenType.rightCurly);
                    return null;
                }
                advance();
            }
        }
        else if (TypeAstNode t = parseType())
        {
            result.type = t;
        }
        else return null;
        return result;
    }

    /**
     * Parses a CompilerEcho.
     *
     * Returns: a $(D CompilerEchoAstNode) on success, $(D null) otherwise.
     */
    CompilerEchoAstNode parseCompilerEcho()
    {
        assert(current.isTokEcho);
        CompilerEchoAstNode result = new CompilerEchoAstNode;
        result.startPos = current.position;
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
        result.command = current();
        advance();
        if (current.isTokRightParen)
        {
            advance();
            return result;
        }
        if (!current.isTokComma)
        {
            expected(TokenType.comma);
            return null;
        }
        advance();
        while (true)
        {
            if (EchoParameterAstNode ep = parseEchoParameter())
            {
                result.parameters ~= ep;
            }
            else
            {
                parseError("invalid echo parameter");
                return null;
            }
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
            !current.isTokLeftParen && !current.isTokLeftSquare &&
            !current.isTokEcho)
        {
            unexpected();
            return null;
        }
        PrimaryExpressionAstNode result = new PrimaryExpressionAstNode;
        result.startPos = current.position;
        if (current.isTokLeftSquare)
        {
            if (InitializerAstNode i = parseInitializer())
            {
                result.arrayLiteral = i;
                return result;
            }
            else return null;
        }
        else if (current.isTokEcho)
        {
            if (CompilerEchoAstNode cc = parseCompilerEcho())
            {
                result.compilerEcho = cc;
            }
            else
            {
                parseError("invalid compiler echo");
                return null;
            }
            return result;
        }
        else if (!current.isTokLeftParen)
        {
            result.identifierOrKeywordOrLiteral = current;
            advance();
            if (current.isTokLesser)
            {
                if (TemplateInstanceAstNode ti = parseTemplateInstance())
                {
                    result.templateInstance = ti;
                }
                else
                {
                    parseError("invalid template specialization");
                    return null;
                }
            }
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
        result.startPos = current.position;
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
        result.startPos = current.position;
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
            result.startPos = current.position;
            result.left = e;
            e.startPos = current.position;
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
     * Params:
     *      exp = The left hand side of a binary expression.
     *      ending = Specifies an alternative ending, e.g left curly bracket for an
     *          IfCondition.
     *
     * Returns: a $(D ExpressionAstNode) on success, $(D null) otherwise.
     */
    ExpressionAstNode parseExpression(ExpressionAstNode exp, TokenType ending = TokenType.invalid)
    {
        ExpressionAstNode result = new ExpressionAstNode;
        result.startPos = current.position;
        with(TokenType)
        if (exp)
        {
            if (!current.isTokBinaryOperator)
            {
                unexpected();
                return null;
            }
            Token* op = current();
            advance();
            if (ExpressionAstNode r = parseExpression(null))
            {
                BinaryExpressionAstNode be = new BinaryExpressionAstNode;

                be.startPos = current.position;
                be.left = exp;
                be.operator = op;
                be.right = r;
                result.binaryExpression = be;

                if (r.binaryExpression && (r.binaryExpression.operator.type > op.type || op.isTokIn))
                {
                    /*
                            a           e1L
                              *         e1O
                                b + c   e1R

                            a * b       e1L
                                  +     e1O
                                    c   e1R
                    */

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
            current.isTokSuper || current.isTokValueKeyword || current.isTokEcho)
        {
            if (UnaryExpressionAstNode u = parseUnaryExpression())
            {
                result.unaryExpression = u;
                if (ending != TokenType.invalid && current.type == ending)
                {
                    return result;
                }
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
            result.startPos = current.position;
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
        result.startPos = current.position();
        advance();
        const bool needRightParen = current.isTokLeftParen;
        if (needRightParen)
            advance();
        if (ExpressionAstNode c = parseExpression(null, TokenType.leftCurly))
        {
            result.condition = c;
        }
        else
        {
            parseError("invalid while condition");
            return null;
        }
        if (needRightParen)
        {
            if (!current.isTokRightParen)
            {
                expected(TokenType.rightParen);
                return null;
            }
            else advance();
        }
        if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
        {
            result.declarationOrStatement = dos;
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
        result.startPos = current.position();
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();

        while (true)
        {
            if (current.isTokStorageClass)
            {
                if (ForeachVariableDeclarationAstNode fvd = parseForeachVariableDeclaration())
                {
                    result.variables ~= fvd;
                }
                else
                {
                    parseError("invalid foreach variable");
                    return null;
                }
                if (current.isTokSemicolon)
                {
                    advance();
                    break;
                }
                else if (current.isTokComma)
                {
                    advance();
                    if (!current.isTokStorageClass)
                    {
                        unexpected();
                        return null;
                    }
                    else continue;
                }
                else
                {
                    unexpected();
                    return null;
                }
            }
            else
            {
                unexpected();
                return null;
            }
        }
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
        if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
        {
            result.declarationOrStatement = dos;
            return result;
        }
        else
        {
            parseError("invalid single statement or block");
            return null;
        }
    }

    /**
     * Parses a ForeachVariableDeclaration.
     *
     * Returns: a $(D ForeachVariableDeclarationAstNode) on success, $(D null) otherwise.
     */
    ForeachVariableDeclarationAstNode parseForeachVariableDeclaration()
    {
        assert(current.isTokStorageClass);
        ForeachVariableDeclarationAstNode result = new ForeachVariableDeclarationAstNode;
        result.startPos = current.position;
        result.isConst = !current.isTokVar;
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
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        result.identifier = current;
        advance();
        return result;
    }

    /**
     * Parses an IfConditionVariableAstNode.
     *
     * Returns: an $(D IfConditionVariableAstNode) on success, $(D null) otherwise.
     */
    IfConditionVariableAstNode parseIfConditionVariableAstNode()
    {
        assert(current.isTokStorageClass);
        IfConditionVariableAstNode result = new IfConditionVariableAstNode;
        result.startPos = current.position();
        result.isConst = !current.isTokVar;
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
        result.startPos = current.position();
        advance();
        const bool needRightParen = current.isTokLeftParen;
        if (needRightParen)
        {
            advance();
        }
        if (current.isTokStorageClass)
        {
            if (IfConditionVariableAstNode icv = parseIfConditionVariableAstNode())
                result.ifVariable = icv;
        }
        else if (ExpressionAstNode c = parseExpression(null, TokenType.leftCurly))
        {
            result.condition = c;
        }
        if (!result.condition && !result.ifVariable)
        {
            parseError("invalid if condition");
            return null;
        }
        if (needRightParen)
        {
            if (!current.isTokRightParen)
            {
                expected(TokenType.rightParen);
                return null;
            }
            else advance();
        }
        if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
        {
            result.trueDeclarationOrStatement = dos;
        }
        else
        {
            parseError("invalid true single statement or block");
            return null;
        }
        if (current.isTokElse)
        {
            advance();
            if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
            {
                result.falseDeclarationOrStatement = dos;
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
        result.startPos = current.position;
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
        result.startPos = current.position;
        advance();
        if (current.isTokLeftParen && lookupNext.isTokAt)
        {
            advance();
            advance();
            if (!current.isTokIdentifier)
            {
                parseError("expected an identifier as continue label");
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
     * Parses a BreakStatement.
     *
     * Returns: a $(D BreakStatementAstNode) on success, $(D null) otherwise.
     */
    BreakStatementAstNode parseBreakStatement()
    {
        assert(current.isTokBreak);
        BreakStatementAstNode result = new BreakStatementAstNode;
        result.startPos = current.position;
        advance();
        if (current.isTokLeftParen && lookupNext.isTokAt)
        {
            advance();
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
     * Parses a GotoStatement.
     *
     * Returns: a $(D GotoStatementAstNode) on success, $(D null) otherwise.
     */
    GotoStatementAstNode parseGotoStatement()
    {
        assert(current.isTokGoto);
        GotoStatementAstNode result = new GotoStatementAstNode;
        result.startPos = current.position;
        advance();
        if (current.isTokLeftParen && lookupNext.isTokAt)
        {
            advance();
            advance();
            if (!current.isTokIdentifier)
            {
                parseError("expected an identifier as goto label");
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
        else
        {
            expected(TokenType.goto_);
            return null;
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
        result.startPos = current.position;
        if (VersionParenExpressionAstNode vpe = parseVersionParenExpression())
        {
            result.versionExpression = vpe;
        }
        else
        {
            parseError("invalid version paren expression");
            return null;
        }
        if (current.isTokLeftCurly)
        {
            advance();
            if (DeclarationsAstNode ds = parseDeclarations())
            {
                result.trueDeclarations = ds;
                advance();
            }
            else return null;
        }
        else
        {
            if (DeclarationAstNode d = parseDeclaration())
            {
                result.trueDeclarations = new DeclarationsAstNode;
                result.trueDeclarations.items ~= d;
            }
            else return null;
        }
        if (current.isTokElse)
        {
            advance();
            if (current.isTokLeftCurly)
            {
                advance();
                if (DeclarationsAstNode ds = parseDeclarations())
                {
                    result.falseDeclarations = ds;
                    advance();
                }
                else return null;
            }
            else
            {
                if (DeclarationAstNode d = parseDeclaration())
                {
                    result.falseDeclarations = new DeclarationsAstNode;
                    result.falseDeclarations.items ~= d;
                }
                else return null;
            }
        }
        return result;
    }

    /**
     * Parses a VersionBlockStatement.
     *
     * Returns: a $(D VersionBlockStatementAstNode) on success, $(D null) otherwise.
     */
    VersionBlockStatementAstNode parseVersionBlockStatement()
    {
        assert(current.isTokVersion);
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        VersionBlockStatementAstNode result = new VersionBlockStatementAstNode;
        result.startPos = current.position;
        if (VersionParenExpressionAstNode vpe = parseVersionParenExpression())
        {
            result.versionExpression = vpe;
        }
        else
        {
            parseError("invalid version paren expression");
            return null;
        }
        if (current.isTokLeftCurly)
        {
            advance();
            if (DeclarationsOrStatementsAstNode doss = parseDeclarationsOrStatements())
            {
                result.trueDeclarationsOrStatements = doss;
                advance();
            }
            else return null;
        }
        else
        {
            if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
            {
                result.trueDeclarationsOrStatements = new DeclarationsOrStatementsAstNode;
                result.trueDeclarationsOrStatements.items ~= dos;
            }
            else return null;
        }
        if (current.isTokElse)
        {
            advance();
            if (current.isTokLeftCurly)
            {
                advance();
                if (DeclarationsOrStatementsAstNode doss = parseDeclarationsOrStatements())
                {
                    result.falseDeclarationsOrStatements = doss;
                    advance();
                }
                else return null;
            }
            else
            {
                if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
                {
                    result.falseDeclarationsOrStatements = new DeclarationsOrStatementsAstNode;
                    result.falseDeclarationsOrStatements.items ~= dos;
                }
                else return null;
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
            result.startPos = current.position;
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
            result.startPos = current.position;
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
            result.startPos = current.position;
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
        VersionPrimaryExpressionAstNode result = new VersionPrimaryExpressionAstNode;
        result.startPos = current.position;
        if (current.isTokBang)
        {
            result.not = current;
            advance();
        }
        if (current.isTokIdentifier)
        {
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
                result.parenExpression = vpe;
                return result;
            }
            else return null;
        }
        else
        {
            parseError("expected identifier or `(`");
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
            warning("empty statement");
            StatementAstNode result = new StatementAstNode;
            result.startPos = current.position;
            result.statementKind = StatementKind.skEmpty;
            result.statement.emptyStatement = new EmptyStatementAstNode;
            result.statement.emptyStatement.startPos = current.position;
            advance();
            return result;
        }
        case return_:
        {
            if (ReturnStatementAstNode rs = parseReturnStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.startPos = rs.startPos;
                result.statementKind = StatementKind.skReturn;
                result.statement.returnStatement = rs;
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
                result.startPos = bs.startPos;
                result.statementKind = StatementKind.skBreak;
                result.statement.breakStatement = bs;
                return result;
            }
            else
            {
                parseError("invalid break statement");
                return null;
            }
        }
        case goto_:
        {
            if (GotoStatementAstNode gs = parseGotoStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.startPos = gs.startPos;
                result.statementKind = StatementKind.skGoto;
                result.statement.gotoStatement = gs;
                return result;
            }
            else
            {
                parseError("invalid goto statement");
                return null;
            }
        }
        case continue_:
        {
            if (ContinueStatementAstNode cs = parseContinueStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.startPos = cs.startPos;
                result.statementKind = StatementKind.skContinue;
                result.statement.continueStatement = cs;
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
                result.startPos = ies.startPos;
                result.statementKind = StatementKind.skIfElse;
                result.statement.ifElseStatement = ies;
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
                result.startPos = ws.startPos;
                result.statementKind = StatementKind.skWhile;
                result.statement.whileStatement = ws;
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
                result.startPos = fs.startPos;
                result.statementKind = StatementKind.skForeach;
                result.statement.foreachStatement = fs;
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
                result.startPos = ss.startPos;
                result.statementKind = StatementKind.skSwitch;
                result.statement.switchStatement = ss;
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
                result.startPos = tofs.startPos;
                result.statementKind = StatementKind.skTryOnFinally;
                result.statement.tryOnFinallyStatement = tofs;
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
                result.startPos = ts.startPos;
                result.statementKind = StatementKind.skThrow;
                result.statement.throwStatement = ts;
                return result;
            }
            else
            {
                parseError("invalid throw statement");
                return null;
            }
        }
        case version_:
        {
            if (VersionBlockStatementAstNode vbs = parseVersionBlockStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.startPos = vbs.startPos;
                result.statementKind = StatementKind.skVersion;
                result.statement.versionBlockStatement = vbs;
                return result;
            }
            else
            {
                parseError("invalid version block statement");
                return null;
            }
        }
        case assert_:
        {
            if (AssertStatementAstNode as = parseAssertStatement())
            {
                StatementAstNode result = new StatementAstNode;
                result.startPos = as.startPos;
                result.statementKind = StatementKind.skAssert;
                result.statement.assertStatement = as;
                return result;
            }
            else
            {
                parseError("invalid assert statement");
                return null;
            }
        }
        case leftCurly:
        {
            advance();
            BlockStatementAstNode b = new BlockStatementAstNode;
            b.startPos = current.position;
            if (DeclarationsOrStatementsAstNode dos = parseDeclarationsOrStatements())
            {
                b.stopPos = current.position;
                StatementAstNode result = new StatementAstNode;
                result.startPos = dos.startPos;
                result.statementKind = StatementKind.skBlock;
                result.statement.block = b;
                result.statement.block.declarationsOrStatements = dos;
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
                result.startPos = es.startPos;
                result.statementKind = StatementKind.skExpression;
                result.statement.expression = es;
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
        AtAttributeAstNode[] attribs;
        while (current.isTokAt)
        {
            if (AtAttributeAstNode a = parseAtAttribute())
                attribs ~= a;
            else return null;
        }

        with(TokenType) switch(current.type)
        {
        case label:
        {
            if (LabelDeclarationstAstNode ld = parseLabelDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.startPos = ld.startPos;
                result.declarationKind = DeclarationKind.dkLabel;
                result.declaration.labelDeclaration = ld;
                return result;
            }
            else
            {
                parseError("invalid label");
                return null;
            }
        }
        case enum_:
        {
            if (EnumDeclarationAstNode decl = parseEnumDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.startPos = decl.startPos;
                result.declarationKind = DeclarationKind.dkEnum;
                result.declaration.enumDeclaration = decl;
                result.declaration.enumDeclaration.atAttributes = attribs;
                return result;
            }
            else return null;
        }
        case interface_:
        {
            if (InterfaceDeclarationAstNode decl = parseInterfaceDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.startPos = decl.startPos;
                result.declarationKind = DeclarationKind.dkInterface;
                result.declaration.interfaceDeclaration = decl;
                result.declaration.interfaceDeclaration.atAttributes = attribs;
                return result;
            }
            else return null;
        }
        case class_:
        {
            if (ClassDeclarationAstNode decl = parseClassDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.startPos = decl.startPos;
                result.declarationKind = DeclarationKind.dkClass;
                result.declaration.classDeclaration = decl;
                result.declaration.classDeclaration.atAttributes = attribs;
                return result;
            }
            else return null;
        }
        case struct_:
        {
            if (StructDeclarationAstNode decl = parseStructDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.startPos = decl.startPos;
                result.declarationKind = DeclarationKind.dkStruct;
                result.declaration.structDeclaration = decl;
                result.declaration.structDeclaration.atAttributes = attribs;
                return result;
            }
            else return null;
        }
        case union_:
        {
            if (UnionDeclarationAstNode decl = parseUnionDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.startPos = decl.startPos;
                result.declarationKind = DeclarationKind.dkUnion;
                result.declaration.unionDeclaration = decl;
                result.declaration.unionDeclaration.atAttributes = attribs;
                return result;
            }
            else return null;
        }
        case function_:
        {
            if (FunctionDeclarationAstNode decl = parseFunctionDeclaration(false))
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.startPos = decl.startPos;
                result.declarationKind = DeclarationKind.dkFunction;
                result.declaration.functionDeclaration = decl;
                result.declaration.functionDeclaration.atAttributes = attribs;
                return result;
            }
            else return null;
        }
        case import_:
        {
            if (ImportDeclarationAstNode decl = parseImportDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.startPos = decl.startPos;
                result.declarationKind = DeclarationKind.dkImport;
                result.declaration.importDeclaration = decl;
                result.declaration.importDeclaration.atAttributes = attribs;
                return result;
            }
            else return null;
        }
        case protection:
        {
            if (ProtectionDeclarationAstNode decl = parseProtectionDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.startPos = decl.startPos;
                result.declarationKind = DeclarationKind.dkProtection;
                result.declaration.protectionOverwrite = decl;
                return result;
            }
            else return null;
        }
        case var, const_, init:
        {
            if (VariableDeclarationAstNode vd = parseVariableDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.startPos = vd.startPos;
                result.declarationKind = DeclarationKind.dkVariable;
                result.declaration.variableDeclaration = vd;
                result.declaration.variableDeclaration.atAttributes = attribs;
                return result;
            }
            else return null;
        }
        case aka:
        {
            if (AkaDeclarationAstNode ad = parseAkaDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.startPos = ad.startPos;
                result.declarationKind = DeclarationKind.dkAka;
                result.declaration.akaDeclaration = ad;
                result.declaration.akaDeclaration.atAttributes = attribs;
                return result;
            }
            else return null;
        }
        case version_:
        {
            if (VersionBlockDeclarationAstNode vb = parseVersionBlockDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.startPos = vb.startPos;
                result.declarationKind = DeclarationKind.dkVersion;
                result.declaration.versionBlockDeclaration = vb;
                return result;
            }
            else return null;
        }
        case template_:
        {
            if (TemplateDeclarationAstNode td = parseTemplateDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.startPos = td.startPos;
                result.declarationKind = DeclarationKind.dkTemplate;
                result.declaration.templateDeclaration = td;
                result.declaration.templateDeclaration.atAttributes = attribs;
                return result;
            }
            else return null;
        }
        case static_:
        {
            if (lookupNext.isTokFunction)
            {
                goto case function_;
            }
            else
            {
                goto case var;
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
     * Constructs the parser with the lexer that contains the tokens to parse.
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
     * Main parser function.
     *
     * Returns: An $(D UniAstNode) on success, $(D null) otherwise.
     */
    UnitAstNode parse()
    {
        assert(current); // at least EOF token
        _u = parseUnit();
        if (_errorCount)
            return null;
        else
            return _u;
    }

    /// Returns: The AST for the unit being parsed.
    UnitAstNode unit()
    {
        return _u;
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
    import styx.ast_printer;
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

    function bee(s32[] p1): function _(): static function _(): s8[]
    {
        // function that returns a function that returns an s8 array
    }
    protection(public) import(10) a.b, c.d;

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
        break (@Label1) a.call();
        break a.call();
        break;
        continue a.call();
        continue;
        a = b + 8;
        {
            foo.bar.baz();
        }
        static var s8 a = 8, b = 7;
        var s8[][] a;
        var MyInt mi = 8;
        var s8[2][4] b;
        var auto a = 8:s64;

        function a(s64 param): auto
        {}

        aka Prototype = function _(s64 p): s64;

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
    AstPrinter ap = new AstPrinter();
    UnitAstNode u = pr.parse();
    assert(u);
    if (u)
    {
        ap.visit(u);
        import std.process;
        if ("CI" !in environment)
            writeln(ap.text);
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
    AstVisitor cov = new AstVisitor;
    if (pr.parse() is null)
    {
        throw new AssertError("code not parsed but should be", file, line);
    }
    else
    {
        cov.visit(pr.unit);
        if (printAST)
        {
            import styx.ast_printer;
            AstPrinter ap = new AstPrinter;
            ap.visit(pr.unit);
            writeln(ap.text);
        }
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
        function bar(function _(u32 a) callback): function _();
        var function _(const u32 a,b,c; u64 d) a;
        var function _(u32 a,b,c; var u64 d) a;
        var function _(u32 a,b,c; u64 d): u64 a;
        var function _(u32 a,b,c; u64 d): u64 a;
        var function _(function _(u32 a) callback): function _() a;
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
        static var s32 a;
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
            a = b in c + d;
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
        var function _foo(;
    });
    assertNotParse(q{
        unit a;
        var function  foo;
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
        var function _(;
    });
    assertNotParse(q{
        unit a;
        var function _():;
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

unittest // attributes
{
    assertParse(q{
        unit a;
        @operator("in") function foo();
    });
    assertParse(q{
        unit a;
        @metadata(42,13) function foo();
    });
    assertNotParse(q{
        unit a;
        @metadata(42,) function foo();
    });
    assertNotParse(q{
        unit a;
        @metadata(42,+ function foo();
    });
    assertNotParse(q{
        unit a;
        @metadata(42# function foo();
    });
    assertNotParse(q{
        unit a;
        @metadata() function foo();
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

unittest // cover error cases for: continue break return goto statements
{
    assertParse(q{
        unit a;
        function foo()
        {
            continue (@L0);
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            continue (L0;
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            continue ();
        }
    });
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
            break(a
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            break(@)
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            continue(@)
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            continue(@lbl
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            break(@lbl
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            break(@lbl) (afterParenExpression);
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            break (afterParenExpression);
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
    assertParse(q{
        unit a;
        function foo()
        {
            goto (@L0);
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            goto (@L0) afterThat;
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            goto;
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            goto (@L0;
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            goto ();
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            goto (@+);
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            goto (@L0)
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

unittest // cover error cases for: interface, struct, class, union
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
        union {}
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
    assertNotParse("
        unit a;
        union A{ var s8 a;
    ");
    assertNotParse(q{
        unit a;
        union A : {}
    });
    assertParse(q{
        unit a;
        union A {}
    });
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

unittest // aka & type
{
    assertParse(q{
        unit a;
        aka MyU64 = u64;
    });
    assertParse(q{
        unit a;
        aka Abc = a.B.C;
    });
    assertNotParse(q{
        unit a;
        aka;
    });
    assertNotParse(q{
        unit a;
        aka a;
    });
    assertNotParse(q{
        unit a;
        aka a.a;
    });
    assertNotParse(q{
        unit a;
        aka a =
    });
    assertNotParse(q{
        unit a;
        aka a = other
    });
    assertNotParse(q{
        unit a;
        aka a = Foo<T>.Bar<+;
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
    assertParse(q{
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
            foreach(const s8 a, var s32 i; b) {}
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
    assertNotParse(q{
        unit a;
        function foo()
        {
            foreach(const a; b) {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            foreach(const const a; b) {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            foreach(const auto a ? b) {}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            foreach(const auto a,; b) {}
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
    assertParse(q{
        unit a;
        const auto a = ~1;
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
    assertParse(q{
        unit a;
        function foo()
        {
            if true {doThat();}
        }
    });
    assertNotParse(q{
        unit a;
        function foo()
        {
            if true doThat();
        }
    });
    assertParse(q{
        unit a;
        function foo()
        {
            if true;
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

unittest // version block declaration
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
    assertNotParse(q{
        unit a;
        version(a) {const;}
    });
    assertNotParse(q{
        unit a;
        version(a) {const A a;} else {const}
    });
}

unittest // version block statement
{
    assertParse(q{
        unit a;
        function foo(){version(a) ++b;}
    });
    assertParse(q{
        unit a;
        function foo(){version(!a) ++b;}
    });
    assertParse(q{
        unit a;
        function foo(){version(a & !b) ++b;}
    });
    assertNotParse(q{
        unit a;
        function foo(){version a) const s8 b;}
    });
    assertNotParse(q{
        unit a;
        function foo(){version (a const s8 b;}
    });
    assertParse(q{
        unit a;
        function foo(){version(a) const s8 b; else const s16 b;}
    });
    assertParse(q{
        unit a;
        function foo(){version(a | b) const s8 c;}
    });
    assertNotParse(q{
        unit a;
        function foo(){version(a |) const s8 c;}
    });
    assertParse(q{
        unit a;
        function foo(){version(a & b) const s8 c;}
    });
    assertNotParse(q{
        unit a;
        function foo(){version(a &) const s8 c;}
    });
    assertNotParse(q{
        unit a;
        function foo(){version(&) const s8 c;}
    });
    assertNotParse(q{
        unit a;
        function foo(){version const s8 c;}
    });
    assertParse(q{
        unit a;
        function foo(){version(a & b | c) const s8 d;}
    });
    assertParse(q{
        unit a;
        function foo(){version(a | b & c) const s8 d;}
    });
    assertParse(q{
        unit a;
        function foo(){version((a | b) & c) const s8 d;}
    });
    assertParse(q{
        unit a;
        function foo(){version((a | b) & (c)) const s8 d;}
    });
    assertParse(q{
        unit a;
        function foo(){version((a | b) & (c | d)) const s8 e;}
    });
    assertParse(q{
        unit a;
        function foo(){version(a) {const s8 b;} else {const s16 b;}}
    });
    assertNotParse(q{
        unit a;
        function foo(){version() {const s8 b;}}
    });
    assertNotParse(q{
        unit a;
        function foo(){version(a & ;) {const s8 b;}}
    });
    assertNotParse(q{
        unit a;
        function foo(){version(a & ()) {const s8 b;}}
    });
    assertNotParse(q{
        unit a;
        function foo(){version(a) const}
    });
    assertNotParse(q{
        unit a;
        function foo(){version(a) {const s8 b;} else const}
    });
    assertNotParse(q{
        unit a;
        function foo(){version(a) {const}}
    });
    assertNotParse(q{
        unit a;
        function foo(){version(a) {const A a;} else {const}}
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
    assertNotParse(q{
        unit a;
        function foo()
        {
            try something();
            on(Error<+ e, Oops o){doThat();doThat();}
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
        const (s8)[ a;
    });
    assertParse(q{
        unit a;
        const (s8[]) a;
    });
    assertParse(q{
        unit a;
        const (auto) a;
    });
    assertParse(q{
        unit a;
        const (function _():s8[])[] a;
    });
    assertParse(q{
        unit a;
    var function _():(function _():s8[])[] a;
    });
    assertNotParse(q{
        unit a;
    var function ():(s8[])[] a;
    });
    assertParse(q{
        unit a;
    var function ():(s8[]) a;
    });
}

unittest // assert
{
    assertParse(q{
        unit a;
        function foo() { assert(true == true); }
    });
    assertNotParse(q{
        unit a;
        function foo() { assert true == true); }
    });
    assertNotParse(q{
        unit a;
        function foo() { assert(true true); }
    });
    assertNotParse(q{
        unit a;
        function foo() { assert(true; }
    });
    assertNotParse(q{
        unit a;
        function foo() { assert(true) }
    });
}

unittest // label
{
    assertParse(q{
        unit a;
        function foo() { label L0; }
    });
    assertNotParse(q{
        unit a;
        function foo() { label; }
    });
    assertNotParse(q{
        unit a;
        function foo() { label L0 }
    });
}

unittest // struct as duck type
{
    assertParse(q{
        unit a;
        struct Typed : A.B, C.D
        {}
    });
    assertNotParse(q{
        unit a;
        struct Typed : A.B,
        {}
    });
    assertNotParse(q{
        unit a;
        struct Typed : A.+,
        {}
    });
}

unittest // template params / specialization
{
    assertParse(q{
        unit a;
        struct Foo<>{}
    });
    assertParse(q{
        unit a;
        struct Foo<T0>{}
    });
    assertParse(q{
        unit a;
        struct Foo<T0, T1>{}
    });
    assertParse(q{
        unit a;
        class Foo<T0, T1>{}
    });
    assertParse(q{
        unit a;
        interface Foo<T0, T1>{}
    });
    assertParse(q{
        unit a;
        union Foo<T0, T1>{}
    });
    assertNotParse(q{
        unit a;
        union Foo<T0, T1{}
    });
    assertNotParse(q{
        unit a;
        struct Foo<T0, T1{}
    });
    assertNotParse(q{
        unit a;
        class Foo<T0, T1{}
    });
    assertNotParse(q{
        unit a;
        interface Foo<T0, T1{}
    });
    assertNotParse(q{
        unit a;
        union Foo<, T1{}
    });
    assertParse(q{
        unit a;
        function foo<T>(var T t){}
    });
    assertNotParse(q{
        unit a;
        function foo<T(var T t){}
    });
    assertParse(q{
        unit a;
        function foo<T>(var T t)
        {
            foo<s8>(0);
        }
    });
    assertNotParse(q{
        unit a;
        function foo<T>(var T t)
        {
            foo<s8(0);
        }
    });
    assertParse(q{
        unit a;
        var Foo<s8> a;
    });
    assertNotParse(q{
        unit a;
        var Foo<:> a;
    });
    assertParse(q{
        unit a;
        var Foo<s8, s32> a;
    });
}

unittest // template
{
    assertParse(q{
        unit a;
        template Foo<>{}
    });
    assertParse(q{
        unit a;
        template Foo<A, B> {}
    });
    assertParse(q{
        unit a;
        template Foo<A, B> { const A a; const B b;}
    });
    assertNotParse(q{
        unit a;
        template <A B> { const A a;}
    });
    assertNotParse(q{
        unit a;
        template Foo<A B> { const A a;}
    });
    assertNotParse(q{
        unit a;
        template Foo<A> { A a;}
    });
    assertNotParse(q{
        unit a;
        template Foo A B> { const A a;}
    });
    assertNotParse(q{
        unit a;
        template Foo<A { const A a;}
    });
    assertNotParse("
        unit a;
        template Foo<A>  const A a;}
    ");
    assertNotParse("
        unit a;
        template Foo<A> { const A a;
    ");
}

unittest // echo
{
    assertParse(q{
        unit a;
        function Foo<>(){ if (echo(verb)){} }
    });
    assertParse(q{
        unit a;
        function Foo<T>(){ if (echo(verb, T, s8[])){} }
    });
    assertNotParse(q{
        unit a;
        function Foo<>(){ if (echo }
    });
    assertNotParse(q{
        unit a;
        function Foo<>(){ if (echo( }
    });
    assertNotParse(q{
        unit a;
        function Foo<>(){ if (echo(a^ }
    });
    assertNotParse(q{
        unit a;
        function Foo<>(){ if (echo() }
    });
    assertNotParse(q{
        unit a;
        function Foo<>(){ if (echo(verb,) }
    });
    assertNotParse(q{
        unit a;
        function Foo<>(){ if (echo(verb,p0]) }
    });
    assertNotParse(`
        unit a;
        function Foo<>(){ if (echo(verb,{p0;) }
    `);
    assertParse(q{
        unit a;
        function Foo<>(){ if (echo(verb, {p0+8;})){} }
    });
    assertParse(q{
        unit a;
        function Foo<T>(){ if (echo(isType, T, class)){} }
    });
}

