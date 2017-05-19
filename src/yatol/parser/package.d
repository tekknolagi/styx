#!runnable: -g -gs
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

    void expected(TokenType expected, string loc = __FUNCTION__)
    {
        ++_errorCount;
        static immutable string specifierDiff = "expected `%s` instead of `%s`";
        static immutable string specifierSame = "expected supplemental `%s`";
        if (current.type != expected)
            parseError(specifierDiff.format(tokenString(expected), _current.text));
        else
            parseError(specifierSame.format(tokenString(expected)));
    }

    void unexpected(string loc = __FUNCTION__)
    {
        writeln(loc);
        ++_errorCount;
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

    NumberLiteralAstNode parseNumberLiteral()
    {
        if (!current.isTokIntegerLiteral && !current.isTokHexLiteral
            && !current.isTokFloatLiteral)
        {
            unexpected();
            return null;
        }
        NumberLiteralAstNode result = new NumberLiteralAstNode;
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
        else
        {
            expected(TokenType.identifier);
            return null;
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
    InterfaceDeclarationAstNode parseInterfaceDeclaration()
    {
        if (!current.isTokInterface)
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
        InterfaceDeclarationAstNode result = new InterfaceDeclarationAstNode;
        result.name = current();
        advance();
        if (!current.isTokLeftCurly)
        {
            expected(TokenType.leftCurly);
            return null;
        }
        parseDeclarations(result.declarations);
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
            return null;
        }
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
            return null;
        }
        parseDeclarations(result.declarations);
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
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
            return null;
        }
        parseDeclarations(result.declarations);
        if (!current.isTokRightCurly)
        {
            expected(TokenType.rightCurly);
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
        ImportDeclarationAstNode result = new ImportDeclarationAstNode;
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
        while (!current.isTokRightParen)
        {
            TypedVariableListAstNode tvl = parseTypedVariableList();
            if (tvl)
            {
                result.parameters ~= tvl;
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
        result.header = header;
        if (!current.isTokLeftCurly && !current.isTokSemicolon)
        {
            parseError("expected `;` or `{` to skip or start the function body");
            return null;
        }
        result.firstBodyToken = current();
        if (current.isTokLeftCurly)
        {
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
            return null;
        }
        return result;
    }

    /**
     * Parses contiguous declarations or statements, a function body.
     *
     * Params:
     *      declsOrStatements: The array filled with the declarations or
     *      statements.
     * Returns:
     *      $(D true) on success, $(D false) otherwise.
     */
    bool parseDeclarationsOrStatements(ref DeclarationOrStatementAstNode[] declsOrStatements)
    {
        const ptrdiff_t oldDeclLvl = _declarationLevels;
        ++_declarationLevels;

        while (advance())
        {
            if (DeclarationAstNode d = parseDeclaration())
            {
                DeclarationOrStatementAstNode dos = new DeclarationOrStatementAstNode;
                dos.declaration = d;
                declsOrStatements ~= dos;
            }
            else if (StatementAstNode s = parseStatement())
            {
                // advance() bug in parseExpression(): one possible fix,
                // if cast(ErrorStatement) s ...
                DeclarationOrStatementAstNode dos = new DeclarationOrStatementAstNode;
                dos.statement  = s;
                declsOrStatements ~= dos;
            }
            else
            {
                // advance() bug in parseExpression(): this branch maybe reached
                // because "null" returned by parseStatement() can say either "error"
                // or "no statement found".
                with (TokenType) switch (current.type)
                {
                case virtual:
                    unexpected();
                    return false;
                case rightCurly:
                    --_declarationLevels;
                    if (_declarationLevels <= 0)
                    {
                        unexpected();
                        return false;
                    }
                    else if (oldDeclLvl != _declarationLevels)
                    {
                        return false;
                    }
                    else return true;
                default:
                    unexpected();
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Parses contiguous declarations.
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
        while (advance())
        {
            if (DeclarationAstNode d = parseDeclaration())
            {
                declarations ~= d;
            }
            else
            {
                with (TokenType) switch (current.type)
                {
                case virtual:
                    return true; // virtual unit
                case rightCurly:
                    --_declarationLevels;
                    if (_declarationLevels <= 0)
                    {
                        unexpected();
                        return false;
                    }
                    else if (oldDeclLvl != _declarationLevels)
                    {
                        return false;
                    }
                    else return true;
                default:
                    //unexpected();
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * Parses an paren expression
     *
     * Returns: a $(D ParenExpressionAstNode) on success, $(D null) otherwise.
     */
    ParenExpressionAstNode parseParenExpression()
    {
        advance();
        if (ExpressionAstNode ex = parseExpression(null))
        {
            ParenExpressionAstNode result = new ParenExpressionAstNode;
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
     * Parses call parameters.
     *
     * Returns: a $(D CallParametersAstNode) on success, $(D null) otherwise.
     */
    CallParametersAstNode parseCallParameters()
    {
        if (!current.isTokLeftParen)
        {
            expected(TokenType.leftParen);
            return null;
        }
        advance();
        CallParametersAstNode result = new CallParametersAstNode;
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
     * Parses an unary expression.
     *
     * Returns: a $(D UnaryExpressionAstNode) on success, $(D null) otherwise.
     */
    UnaryExpressionAstNode parseUnaryExpression()
    {
        UnaryExpressionAstNode result = new UnaryExpressionAstNode;
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
        if (current.isTokIdentifier)
        {
            if (Token*[] idc = parseIdentifierChain())
                result.identifierChain = idc;
            else return null;
            if (current.isTokLeftParen)
            {
                if (CallParametersAstNode cp = parseCallParameters)
                {
                    result.callParameters = cp;
                }
                else return null;
            }
        }
        // + else if current is literal...

        if (!result.identifierChain.length && !result.numberLitteral)
        {
            parseError("expected identifier or literal");
            return null;
        }

        if (current.isUnarySuffix)
        {
             result.suffix = current();
             advance();
             if (current.isTokPlusPlus || current.isTokMinusMinus)
             {
                unexpected();
                return null;
            }
        }
        return result;
    }

    /**
     * Parses an expression.
     *
     * Returns: a $(D ExpressionAstNode) on success, $(D null) otherwise.
     */
    ExpressionAstNode parseExpression(ExpressionAstNode exp)
    {
        with(TokenType) if (current.type.among(plusPlus, minusMinus, amp, mul, identifier))
        {
            if (exp && (exp.unaryExpression || exp.castExpression))
            {
                return null;
            }
            if (UnaryExpressionAstNode u = parseUnaryExpression)
            {
                ExpressionAstNode result = new ExpressionAstNode;
                result.unaryExpression = u;
                if (current.type.among(semiColon, rightCurly, rightParen, rightSquare, comma, ellipsis))
                {
                    return result;
                }
                else
                {
                    result = parseExpression(result);
                    return result;
                }
            }
        }

        with(TokenType) if (current.isTokEqual)
        {
            advance();
            if (ExpressionAstNode r = parseExpression(null))
            {
                ExpressionAstNode result = new ExpressionAstNode;
                AssignExpressionAstNode ae = new AssignExpressionAstNode;
                ae.left = exp;
                ae.right = r;
                result.assignExpression = ae;
                if (current.type.among(semiColon, rightCurly, rightParen, rightSquare, comma, ellipsis))
                {
                    return result;
                }
                else
                {
                    result = parseExpression(result);
                    return result;
                }
            }
        }

        with(TokenType) if (firstOperator <= current.type && current.type <= lastOperator)
        {
            Token* op = current();
            advance();
            if (ExpressionAstNode r = parseExpression(null))
            {
                ExpressionAstNode result = new ExpressionAstNode;
                BinaryExpressionAstNode be = new BinaryExpressionAstNode;
                be.left = exp;
                be.operator = op;
                be.right = r;
                result.binaryExpression = be;
                if (current.type.among(semiColon, rightCurly, rightParen, rightSquare, comma, ellipsis))
                {
                    return result;
                }
                else
                {
                    result = parseExpression(result);
                    return result;
                }
            }
        }

        with(TokenType) if (current.isTokLeftSquare)
        {
            advance();
            if (ExpressionAstNode i = parseExpression(null))
            {
                ExpressionAstNode result = new ExpressionAstNode;
                if (current.isTokRightSquare)
                {
                    IndexExpressionAstNode ie = new IndexExpressionAstNode;
                    ie.indexed = exp;
                    ie.index = i;
                    result.indexExpression = ie;
                    advance();
                    return result;
                }
                else if (current.isTokEllipsis)
                {
                    advance();
                    if (ExpressionAstNode r = parseExpression(null))
                        if (current.isTokRightSquare)
                    {
                        RangeExpressionAstNode re = new RangeExpressionAstNode;
                        re.indexed = exp;
                        re.left = i;
                        re.right = r;
                        result.rangeExpression = re;
                        advance();
                        return result;
                    }
                    if (!current.isTokRightSquare)
                        expected(TokenType.rightSquare);
                    // else other error ?
                    return null;
                }
                else
                {
                    parseError("expected either `]` or `..` to define an index or a range");
                    return null;
                }
            }
        }

        with(TokenType) if (current.isTokLeftParen)
        {
            if (ParenExpressionAstNode p = parseParenExpression)
            {
                ExpressionAstNode result = new ExpressionAstNode;
                result.parenExpression = p;
                if (current.type.among(semiColon, rightCurly, rightParen, rightSquare, comma, ellipsis))
                {
                    return result;
                }
                else
                {
                    result = parseExpression(result);
                    return result;
                }
            }
        }

        with(TokenType) if (current.isTokColon)
        {
            if (!exp)
            {
                parseError("no left expression to cast");
                return null;
            }
            advance();
            if (TypeAstNode t = parseType)
            {
                ExpressionAstNode result = new ExpressionAstNode;
                CastExpressionAstNode ce = new CastExpressionAstNode;
                ce.expression = exp;
                ce.type = t;
                result.castExpression = ce;
                if (current.type.among(semiColon, rightCurly, rightParen, rightSquare, comma, ellipsis))
                {
                    return result;
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
     * Parses an expression statement
     *
     * Returns: a $(D ExpressionStatementAstNode) on success, $(D null) otherwise.
     */
    ExpressionStatementAstNode parseExpressionStatement()
    {
        if (ExpressionAstNode ex = parseExpression(null))
        {
            ExpressionStatementAstNode result = new ExpressionStatementAstNode;
            result.expression = ex;
            if (!current.isTokSemicolon)
            {
                expected(TokenType.semiColon);
                return null;
            }
            else return result;
        }
        else return null;
    }

    /**
     * Parses a statement.
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
            return result;
        }
        /*
        other cases are for statements starting witha keyword
        */
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
     * Parses a declaration.
     *
     * Returns: a $(D DeclarationAstNode) on success, $(D null) otherwise.
     */
    DeclarationAstNode parseDeclaration()
    {
        with(TokenType) switch(current.type)
        {
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
        case leftCurly:
        {
            if (ScopeDeclarationAstNode decl = parseScopeDeclaration())
            {
                DeclarationAstNode result = new DeclarationAstNode;
                result.scopeDeclaration = decl;
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
        case static_:
        {
            if (lookup(1).isTokFunction)
            {
                goto case function_;
            }
            else
            {
                unexpected();
                return null;
            }
        }
        case identifier:
        {
            if (lookup(1).isTokIdentifier) // if parseType...
            {
                // TODO-cparser todo: parseVariableDeclaration
                return null;
            }
            else return null; // ident chain of unary
        }
        case u8, u16, u32, u64, s8, s16, s32, s64, ureg, sreg, f32, f64:
        {
            if (lookup(1).isTokIdentifier) // if parseType...
            {
                // TODO-cparser todo: parseVariableDeclaration
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
     * Main parser function. The function tries to parse from the main unit to
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
            if (!_range.empty)
            {
                unexpected;
                return null;
            }
            if (_errorCount)
                return null;
            else
                return _uc;
        }
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

version(none) unittest
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

version(none) unittest
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

version(none) unittest
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
    protection(public) import(10) a.b, c.d;
    virtual unit c;
    struct Owl
    {
        function of1(): s32;
        function of2(): u32
        {
            import c.v.b;
            function local(): c.v.b.FooBar {;;}
        }
    }
    interface Pig
    {
        function pig1(): s32;
        function pig2(): s64;
    }
    import (10101) a.b, c.d.r;
    static function exp()
    {
        a++;
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
        import std.process;
        if ("CI" !in environment)
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
void assertParse(const(char)[] code, string file = __FILE_FULL_PATH__,
    size_t line = __LINE__)
{
    Lexer lx;
    lx.setSourceFromText(code, file, line, 1);
    lx.lex;
    Parser pr = Parser(&lx);
    assert(pr.parse() !is null, "code not parsed but should be");
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

version(all):

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

version(all) unittest
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
        function bar(u32 a,b,c; u64 d);
        function bar(u32 a,b,c; u64 d){}
        function bar(u32 a,b,c; u64 d): u64;
        function bar(u32 a,b,c; u64 d): u64 {}
        function bar(function*(u32 a) callback): function*();
    });
}

