#!runnable: -g -gs
/**
 * YATOL parser.
 *
 * to maintain unittest coverage: 99%
 **/
module yatol.parser;

//TODO-cparser todo: check when advance() reaches the EOF.

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

    NumberLiteralAstNode parseNumberLiteral()
    {
        if (!current.isTokIntegerLiteral && !current.isTokHexLiteral
            && !current.isTokFloatLiteral)
        {
            unexpected();
            return null;
        }
        NumberLiteralAstNode result = new NumberLiteralAstNode;
        result.position = current.position;
        result.literal = current();
        advance();
        if (current.isTokColon)
        {
            advance();
            if (!current.isTokBasicType)
            {
                parseError("expected a basic type as number literal suffix");
                return null;
            }
            result.literalType = current;
            advance();
        }
        return result;
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
        TypeModifierAstNode lastMd = result;
        result.position = current.position;
        if (!current.isTokMul && !current.isTokLeftSquare)
        {
            return null;
        }
        while (current.isTokMul || current.isTokLeftSquare)
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
        if (current.isTokBasicType)
        {
            result.basicOrQualifiedType ~= current();
            advance();
        }
        else if (current.isTokAuto)
        {
            result.basicOrQualifiedType ~= current();
            advance();
            return result;
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
     * Parses a FunctionParameterGroup.
     *
     * Returns:
     *      On success a $(D FunctionParameterGroupAstNode) otherwise $(D null).
     */
    FunctionParameterGroupAstNode parseFunctionParameterGroup()
    {
        FunctionParameterGroupAstNode result = new FunctionParameterGroupAstNode;
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
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        InterfaceDeclarationAstNode result = new InterfaceDeclarationAstNode;
        result.position = current.position;
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
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        ClassDeclarationAstNode result = new ClassDeclarationAstNode;
        result.position = current.position;
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
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        StructDeclarationAstNode result = new StructDeclarationAstNode;
        result.position = current.position;
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
    OnMatchExpressionAstNode parseOnMatchExpression()
    {
        OnMatchExpressionAstNode result = new OnMatchExpressionAstNode;
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
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        OnMatchStatementAstNode result = new OnMatchStatementAstNode;
        while (true)
        {
            if (OnMatchExpressionAstNode e = parseOnMatchExpression)
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
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        SwitchStatementAstNode result = new SwitchStatementAstNode;
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
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        EnumDeclarationAstNode result = new EnumDeclarationAstNode;
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
            result.priority = parseNumberLiteral();
            if (!result.priority)
            {
                parseError("number literal expected to set the import priority");
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
        assert(current.isTokFunction);
        advance();
        if (!current.isTokIdentifier)
        {
            expected(TokenType.identifier);
            return null;
        }
        FunctionHeaderAstNode result = new FunctionHeaderAstNode;
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
        result.position = current.position;
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
        result.position = current.position;
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
            if (ExpressionAstNode e = parseExpression(null))
            {
                result.initiliazer = e;
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
            VariableDeclarationAstNode result = new VariableDeclarationAstNode;
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
            AkaDeclarationAstNode result = new AkaDeclarationAstNode;
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
            if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
            {
                declsOrStatements ~= dos;
            }
            else
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
            if (DeclarationAstNode d = parseDeclaration())
            {
                declarations ~= d;
            }
            else
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
                    unexpected();
                    return false;
                }
            }
        }
    }

    /**
     * Parses a ParenExpression.
     *
     * Returns: a $(D ParenExpressionAstNode) on success, $(D null) otherwise.
     */
    ParenExpressionAstNode parseParenExpression()
    {
        advance();
        if (ExpressionAstNode ex = parseExpression(null))
        {
            ParenExpressionAstNode result = new ParenExpressionAstNode;
            result.position = current.position;
            result.expression = ex;
            if (!current.isTokRightParen)
            {
                expected(TokenType.rightParen);
                return null;
            }
            else
            {
                advance();
                return result;
            }
        }
        else return null;
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
     * Parses a PostfixExpression.
     *
     * Returns: a $(D PostfixExpressionAstNode) on success, $(D null) otherwise.
     */
    PostfixExpressionAstNode parsePostfixExpression()
    {
        assert(current.isTokLeftSquare || current.isTokDotDot||
            current.isUnarySuffix || current.isTokColon || current.isTokLeftParen);

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
                    RangeExpressionAstNode re = new RangeExpressionAstNode;
                    re.left = e;
                    re.right = r;
                    result.rangeExpression = re;
                    advance();
                    return result;
                }
            }
            parseError("invalid index or range expression");
            return null;
        }
        else if (current.isUnarySuffix)
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
        else /*if (current.isTokLeftParen)*/
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
        //else return null;
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
        if (current.isUnaryPrefix)
        {
            result.prefix = current();
            advance();
            if (current.isUnaryPrefix)
            {
                if (UnaryExpressionAstNode u = parseUnaryExpression())
                {
                    result.unary = u;
                    return result;
                }
                else return null;
            }
        }
        if (current.isTokSuper)
        {
            result.super_ = current();
            advance();
            if (current.isTokDot)
            {
                advance();
                if (!current.isTokIdentifier)
                {
                    expected(TokenType.identifier);
                    return null;
                }
                else
                {
                    if (Token*[] idc = parseIdentifierChain())
                        result.identifierChain = idc;
                    else
                        return null;
                }
            }
            else if (!current.isTokSemicolon)
            {
                parseError("expected semicolon or dotted identifier after `super`");
                return null;
            }
        }
        else if (current.isTokIdentifier)
        {
            if (Token*[] idc = parseIdentifierChain())
                result.identifierChain = idc;
            else
                return null;
        }
        else if (current.isNumberLiteral)
        {
            if (NumberLiteralAstNode nl = parseNumberLiteral())
            {
                result.numberLitteral = nl;
            }
            else
            {
                return null;
            }
        }
        else if (current.isTokLeftParen)
        {
            if (ParenExpressionAstNode pe = parseParenExpression)
                result.parenExpression = pe;
            else
                return null;
        }
        else if (current.isTokValueKeyword)
        {
            result.valueKeyword = current();
            advance();
        }
        else
        {
            parseError("expected identifier, literal, paren or value keyword");
            return null;
        }
        with(TokenType) while (current.type.among(colon, plusPlus, minusMinus, leftSquare, leftParen))
        {
            if (PostfixExpressionAstNode pe = parsePostfixExpression)
            {
                result.postfixes ~= pe;
            }
            else return null;
        }
        return result;
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
            else if (current.isTokEqual)
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
     * Parses a DotExpression.
     *
     * Returns: a $(D DotExpressionAstNode) on success, $(D null) otherwise.
     */
    DotExpressionAstNode parseDotExpression()
    {
        assert(current.isTokDot);
        advance();
        if (ExpressionAstNode e = parseExpression(null))
        {
            DotExpressionAstNode result = new DotExpressionAstNode;
            e.position = current.position;
            result.right = e;
            return result;
        }
        return null;
    }

    /**
     * Sets an expression as the LHS of a DotExpression.
     *
     * Params:
     *      exp = The current expression.
     * Returns:
     *      On success a n$(D ExpressionAstNode) with $(D dotExpression) assigned,
     *          $(D null) otherwise.
     */
    ExpressionAstNode dotifyExpression(ExpressionAstNode exp)
    {
        if (exp) if (DotExpressionAstNode de = parseDotExpression())
        {
            ExpressionAstNode result = new ExpressionAstNode;
            result.dotExpression = de;
            de.left = exp;
            return result;
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
        with(TokenType) if (current.isTokOperator
            && !(current.isTokAmp && exp is null) // &(unary)
            && !(current.isTokMul && exp is null) // *(unary)
        )
        {
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

                if (current.type.among(semiColon, rightCurly, rightParen, rightSquare, comma, dotDot, equal))
                {
                    return result;
                }
                assert(false);
                /*else if (current.isTokDot)
                {
                    assert(false);
                    return dotifyExpression(result);
                }
                else
                {
                    assert(false);
                    result = parseExpression(result);
                    return result;
                }*/
            }
        }

        with(TokenType) if (current.isUnaryPrefix || current.isTokIdentifier ||
            current.isNumberLiteral || current.isTokLeftParen ||
            current.isTokSuper || current.isTokValueKeyword)
        {
            if (exp && (exp.unaryExpression))
            {
                return null;
            }
            if (UnaryExpressionAstNode u = parseUnaryExpression)
            {
                ExpressionAstNode result = new ExpressionAstNode;
                result.unaryExpression = u;
                u.position = current.position;
                if (current.type.among(semiColon, rightCurly, rightParen, rightSquare, comma, dotDot, equal))
                {
                    return result;
                }
                else if (current.isTokDot)
                {
                    return dotifyExpression(result);
                }
                else
                {
                    result = parseExpression(result);
                    return result;
                }
            }
        }

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
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        WhileStatementAstNode result = new WhileStatementAstNode;
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
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        ForeachStatementAstNode result = new ForeachStatementAstNode;
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
        if (ExpressionAstNode e = parseExpression(null))
        {
            result.enumerable = e;
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
        advance();
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        IfElseStatementAstNode result = new IfElseStatementAstNode;
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
            SingleStatementOrBlockAstNode result = new SingleStatementOrBlockAstNode;
            result.block = bs;
            return result;
        }
        else
        {
            if (DeclarationOrStatementAstNode dos = parseDeclarationOrStatement())
            {
                SingleStatementOrBlockAstNode result = new SingleStatementOrBlockAstNode;
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
     * Parses a Statement.
     *
     * Returns: a $(D StatementAstNode) on success, $(D null) otherwise.
     */
    StatementAstNode parseStatement()
    {
        with(TokenType) switch(current.type)
        {
        case eof:
            return null;
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
        case virtual:
        {
            if (_declarationLevels > 1)
            {
                unexpected();
                parseError("virtual units can only be declared after the main unit declarations");
            }
            return null;
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
    const TypeAstNode tan = prs.parseCustomNode!TypeAstNode;
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
    const TypeModifierAstNode tman = prs.parseCustomNode!TypeModifierAstNode;
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
    const TypeAstNode tan = prs.parseCustomNode!TypeAstNode;
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
    Lexer lx;
    lx.setSourceFromText(code, file, line, 1);
    lx.lex;
    Parser pr = Parser(&lx);
    assert(pr.parse() !is null, "code not parsed but should be");
    if (printAST)
    {
        import yatol.parser.debug_visitor;
        DebugVisitor dv = new DebugVisitor;
        dv.visit(pr.unitContainer);
        dv.printText();
    }
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
    Lexer lx;
    lx.setSourceFromText(code, file, line, 1);
    lx.lex;
    Parser pr = Parser(&lx);
    assert(pr.parse() is null, "code parsed but should not be");
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
            ++a;
        }
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
}

unittest // cover error cases for: function and function type decl
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
    assertNotParse(q{
        unit a;
        class A : B { function foo() {super.a.0;}}
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
        function foo()
        {
            a = 8
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
}

unittest
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

