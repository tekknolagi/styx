/**
 * Contains an AstVisitor that formats the AST of a UnitContainer.
 */
module yatol.ast_formatter;

import
    std.array, std.algorithm.iteration, std.stdio;
import
    yatol.ast, yatol.token;


/**
 * An $(D AstVisitor) that formats the nodes.
 *
 * Comments, as already absent from the AST, are not included.
 */
class AstFormatter : AstVisitor
{
    alias visit = AstVisitor.visit;

private:

    Appender!(char[]) _source;
    char[1024] _indentText = ' ';
    size_t _indentLevel;
    bool _formattingFunctionReturn;

    void growIndentLevel()
    {
        _indentLevel += (_indentLevel == 1024) ? 0 : 4;
    }

    void shrinkIndentLevel()
    {
        _indentLevel += _indentLevel ? -4 : 0;
    }

    void indent()
    {
        _source ~= _indentText[0.._indentLevel];
    }

    void semicolonAndNewLine()
    {
        _source ~= ";\n";
    }

    void space()
    {
        _source ~= " ";
    }

    void visitPossiblyIndented(DeclarationOrStatementAstNode node)
    {
        if (node)
        {
            bool nested;
            if (node.declaration ||
                node.statement.statementKind != StatementKind.skBlock)
            {
                nested = true;
                growIndentLevel();
            }
            visit(node);
            if (nested)
                shrinkIndentLevel();
        }
    }

public:

    /// Returns: the formatted ast.
    char[] formattedAst() {return _source.data;}

    override void visit(AkaDeclarationAstNode node)
    {
        indent();
        node.visitAtAttributes(this);
        _source ~= "aka ";
        if (node.name)
            _source ~= node.name.text;
        if (node.type)
        {
            _source ~= " = ";
            visit(node.type);
        }
        semicolonAndNewLine();
    }

    override void visit(AssertStatementAstNode node)
    {
        indent();
        _source ~= "assert(";
        node.accept(this);
        _source ~= ");\n";
    }

    override void visit(AssignExpressionAstNode node)
    {
        if (node.left)
            visit(node.left);
        if (node.operator)
        {
            space();
            _source ~= node.operator.text;
            space();
        }
        if (node.right)
            visit(node.right);
    }

    override void visit(AtAttributeAstNode node)
    {
        _source ~= "@";
        _source ~= node.identifierOrKeyword.text;
        space();
    }

    override void visit(BinaryExpressionAstNode node)
    {
        _source ~= "(";
        visit(node.left);
        space();
        _source ~= node.operator.text;
        space();
        visit(node.right);
        _source ~= ")";
    }

    override void visit(BlockStatementAstNode node)
    {
        indent();
        _source ~= "{\n";
        growIndentLevel();
        node.accept(this);
        shrinkIndentLevel();
        indent();
        _source ~= "}\n";
    }

    override void visit(BreakStatementAstNode node)
    {
        indent();
        _source ~= "break";
        if (node.label)
        {
            _source ~= "(@";
            _source ~= node.label.text;
            _source ~= ")";
        }
        if (node.expression)
        {
            space();
            visit(node.expression);
        }
        semicolonAndNewLine();
    }

    override void visit(CallParametersAstNode node)
    {
        _source ~= "(";
        foreach(i, p; node.parameters)
        {
            visit(p);
            if (i != node.parameters.length - 1)
                _source ~= ", ";
        }
        _source ~= ")";
    }

    override void visit(ClassDeclarationAstNode node)
    {
        indent();
        node.visitAtAttributes(this);
        _source ~= "class ";
        _source ~= node.name.text;
        if (node.templateParameters)
            visit(node.templateParameters);
        if (node.inheritanceList.length)
        {
            _source ~= ": ";
            foreach(i, c; node.inheritanceList)
            {
                visit(c);
                if (i != node.inheritanceList.length - 1)
                    _source ~= ", ";
            }
        }
        _source ~= "\n";
        indent();
        _source ~= "{\n";
        growIndentLevel();
        if (node.declarations)
            visit(node.declarations);
        shrinkIndentLevel();
        indent();
        _source ~= "}\n";
    }

    override void visit(ContinueStatementAstNode node)
    {
        indent();
        _source ~= "continue";
        if (node.label)
        {
            _source ~= "(@";
            _source ~= node.label.text;
            _source ~= ")";
        }
        if (node.expression)
        {
            space();
            visit(node.expression);
        }
        semicolonAndNewLine();
    }

    override void visit(DeclarationAstNode node)
    {
        if (node.declarationKind == DeclarationKind.dkVariable)
            indent();
        node.accept(this);
        if (node.declarationKind == DeclarationKind.dkVariable)
            semicolonAndNewLine();
    }

    override void visit(EnumDeclarationAstNode node)
    {
        indent();
        node.visitAtAttributes(this);
        _source ~= "enum ";
        _source ~= node.name.text;
        if (node.type)
        {
            _source ~= ": ";
            visit(node.type);
        }
        _source ~= "\n";
        indent();
        _source ~= "{\n";
        growIndentLevel();
        foreach(i, m; node.members)
        {
            indent();
            visit(m);
            if (i != node.members.length - 1)
                _source ~= ",\n";
            else
                _source ~= "\n";
        }
        shrinkIndentLevel();
        indent();
        _source ~= "}\n";
    }

    override void visit(EnumMemberAstNode node)
    {
        if (node.identifier)
            _source ~= node.identifier.text;
        if (node.value)
        {
            _source ~= " = ";
            visit(node.value);
        }
    }

    override void visit(ExpressionStatementAstNode node)
    {
        indent();
        node.accept(this);
        _source ~= ";\n";
    }

    override void visit(ForeachStatementAstNode node)
    {
        indent();
        _source ~= "foreach(";
        foreach(i, v; node.variables)
        {
            visit(v);
            if (i != node.variables.length - 1)
                _source ~= ", ";
        }
        _source ~= "; ";
        if (node.singleOrRangeExpression)
            visit(node.singleOrRangeExpression);
        _source ~= ")\n";
        visitPossiblyIndented(node.declarationOrStatement);
    }

    override void visit(ForeachVariableDeclarationAstNode node)
    {
        if (node.isConst)
            _source ~= "const ";
        else
            _source ~= "var ";
        node.accept(this);
        space();
        if (node.identifier)
            _source ~= node.identifier.text;
    }

    override void visit(FunctionDeclarationAstNode node)
    {
        indent();
        node.visitAtAttributes(this);
        if (node.isStatic)
            _source ~= "static function ";
        else
            _source ~= "function ";
        if (node.name)
            _source ~= node.name.text;
        if (node.templateParameters)
            visit(node.templateParameters);
        _source ~= "(";
        foreach(i, p; node.parameters)
        {
            visit(p);
            if (i != node.parameters.length - 1)
                _source ~= "; ";
        }
        _source ~= ")";
        if (node.returnType)
        {
            const bool old = _formattingFunctionReturn;
            _formattingFunctionReturn = true;
            _source ~= ":";
            visit(node.returnType);
            _formattingFunctionReturn = old;
        }
        if (node.firstBodyToken)
        {
            if (node.firstBodyToken.text == ";")
                _source ~= ";\n";
            else
            {
                _source ~= "\n";
                indent();
                _source ~= "{\n";
                growIndentLevel();
                if (node.declarationsOrStatements)
                    visit(node.declarationsOrStatements);
                shrinkIndentLevel();
                indent();
                _source ~= "}\n";
            }
        }
    }

    override void visit(FunctionParameterGroupAstNode node)
    {
        if (node.isConst)
            _source ~= "const ";
        if (node.isVar)
            _source ~= "var ";
        if (node.type)
            visit(node.type);
        space();
        _source ~= node.variableList.tokenChainText(false, ", ");
    }

    override void visit(IdentifierChainAstNode node)
    {
        _source ~= node.chain.tokenChainText;
    }


    override void visit(IfConditionVariableAstNode node)
    {
        if (node.isConst)
            _source ~= "const ";
        else
            _source ~= "var ";
        if (node.type)
            visit(node.type);
        space();
        if (node.variable)
            visit(node.variable);
    }

    override void visit(IfElseStatementAstNode node)
    {
        indent();
        _source ~= "if (";
        if (node.condition)
            visit(node.condition);
        else if (node.ifVariable)
            visit(node.ifVariable);
        _source ~= ")\n";
        visitPossiblyIndented(node.trueDeclarationOrStatement);
        if (node.falseDeclarationOrStatement)
        {
            indent();
            _source ~= "else\n";
            visitPossiblyIndented(node.falseDeclarationOrStatement);
        }
    }

    override void visit(ImportDeclarationAstNode node)
    {
        indent();
        node.visitAtAttributes(this);
        if (node.priority)
        {
            _source ~= "import(";
            _source ~= node.priority.text;
            _source ~= ") ";
        }
        else
        {
            _source ~= "import ";
        }
        foreach(i, p; node.importList)
        {
            visit(p);
            if (i != node.importList.length - 1)
                _source ~= ", ";
        }
        semicolonAndNewLine();
    }

    override void visit(IndexExpressionAstNode node)
    {
        _source ~= "[";
        if (node.index)
            visit(node.index);
        _source ~= "]";
    }

    override void visit(InitializerAstNode node)
    {
        if (node.singleInitializer)
            visit(node.singleInitializer);
        else
        {
            _source ~= "[";
            foreach(i, e; node.arrayInitializerElements)
            {
                visit(e);
                if (i != node.arrayInitializerElements.length - 1)
                    _source ~= ", ";
            }
            _source ~= "]";
        }
    }

    override void visit(InterfaceDeclarationAstNode node)
    {
        indent();
        node.visitAtAttributes(this);
        _source ~= "interface ";
        _source ~= node.name.text;
        if (node.templateParameters)
            visit(node.templateParameters);
        if (node.inheritanceList.length)
        {
            _source ~= ": ";
            foreach(i, c; node.inheritanceList)
            {
                visit(c);
                if (i != node.inheritanceList.length - 1)
                    _source ~= ", ";
            }
        }
        _source ~= "\n";
        indent();
        _source ~= "{\n";
        growIndentLevel();
        if (node.declarations)
            visit(node.declarations);
        shrinkIndentLevel();
        indent();
        _source ~= "}\n";
    }

    override void visit(LabelStatementAstNode node)
    {
        indent();
        _source ~= "label ";
        if (node.identifier)
            _source ~= node.identifier.text;
        semicolonAndNewLine();
    }

    override void visit(OnExceptionInstanceAstNode node)
    {
        if (node.exceptionType)
            visit(node.exceptionType);
        space();
        if (node.identifier)
            _source ~= node.identifier.text;
    }

    override void visit(OnExceptionStatementAstNode node)
    {
        indent();
        _source ~= "on(";
        foreach(i, e; node.exceptionsInstances)
        {
            visit(e);
            if (i != node.exceptionsInstances.length - 1)
                _source ~= ", ";
        }
        _source ~= ")\n";
        visitPossiblyIndented(node.exceptionsDeclarationOrStatement);
    }

    override void visit(OnMatchStatementAstNode node)
    {
        indent();
        _source ~= "on(";
        node.onMatchExpressions.each!(a => visit(a));
        _source ~= ")\n";
        visitPossiblyIndented(node.declarationOrStatement);
    }

    override void visit(PostfixExpressionAstNode node)
    {
        if (node.plusplusOrMinusMinus)
            _source ~= node.plusplusOrMinusMinus.text;
        else if (node.dotOrOptAccess)
            _source ~= node.dotOrOptAccess.text;
        else if (node.castToType)
            _source ~= ":";

        node.accept(this);
    }

    override void visit(PrimaryExpressionAstNode node)
    {
        if (node.identifierOrKeywordOrLiteral)
            _source ~= node.identifierOrKeywordOrLiteral.text;
        if (node.templateInstance)
            visit(node.templateInstance);
        else if (node.arrayLiteral)
            visit(node.arrayLiteral);
        else if (node.parenExpression)
        {
            _source ~= "(";
            visit(node.parenExpression);
            _source ~= ")";
        }
    }

    override void visit(ProtectionDeclarationAstNode node)
    {
        indent();
        _source ~= "protection(";
        if (node.protection)
            _source ~= node.protection.text;
        _source ~= ")\n";
    }

    override void visit(ReturnStatementAstNode node)
    {
        indent();
        _source ~= "return";
        if (node.expression)
        {
            space();
            visit(node.expression);
        }
        semicolonAndNewLine;
    }

    override void visit(SingleOrRangeExpressionAstNode node)
    {
        if (node.singleOrLeftExpression)
            visit(node.singleOrLeftExpression);
        if (node.rightExpression)
        {
            _source ~= " .. ";
            visit(node.rightExpression);
        }
    }

    override void visit(SliceExpressionAstNode node)
    {
        _source ~= "[";
        if (node.left)
            visit(node.left);
        _source ~= " .. ";
        if (node.right)
            visit(node.right);
        _source ~= "]";
    }

    override void visit(StructDeclarationAstNode node)
    {
        indent();
        node.visitAtAttributes(this);
        _source ~= "struct ";
        _source ~= node.name.text;
        if (node.templateParameters)
            visit(node.templateParameters);
        _source ~= "\n";
        indent();
        _source ~= "{\n";
        growIndentLevel();
        if (node.declarations)
            visit(node.declarations);
        shrinkIndentLevel();
        indent();
        _source ~= "}\n";
    }

    override void visit(SwitchStatementAstNode node)
    {
        indent();
        _source ~= "switch(";
        if (node.expression)
            visit(node.expression);
        _source ~= ")\n";
        indent();
        _source ~= "{\n";
        node.onMatchStatements.each!(a => visit(a));
        if (node.elseStatement)
        {
            indent();
            _source ~= "else\n";
            visitPossiblyIndented(node.elseStatement);
        }
        indent();
        _source ~= "}\n";
    }

    override void visit(TemplateParametersAstNode node)
    {
        _source ~= "<";
        foreach(i, p; node.parameters)
        {
            _source ~= p.text;
            if (i != node.parameters.length - 1)
                _source ~= ", ";
        }
        _source ~= ">";
    }

    override void visit(TemplateDeclarationAstNode node)
    {
        indent();
        node.visitAtAttributes(this);
        _source ~= "template ";
        _source ~= node.name.text;
        if (node.templateParameters)
            visit(node.templateParameters);
        _source ~= "\n";
        indent();
        _source ~= "{\n";
        growIndentLevel();
        if (node.declarations)
            visit(node.declarations);
        shrinkIndentLevel();
        indent();
        _source ~= "}\n";
    }

    override void visit(TemplateInstanceAstNode node)
    {
        _source ~= "<";
        foreach(i, t; node.types)
        {
            visit(t);
            if (i != node.types.length - 1)
                _source ~= ", ";
        }
        _source ~= ">";
    }

    override void visit(ThrowStatementAstNode node)
    {
        indent();
        _source ~= "throw ";
        node.accept(this);
        semicolonAndNewLine();
    }

    override void visit(TryOnFinallyStatementAstNode node)
    {
        indent();
        _source ~= "try\n";
        visitPossiblyIndented(node.triedDeclarationOrStatement);
        node.exceptionDeclarationsOrStatements.each!(a => visit(a));
        if (node.finalDeclarationOrStatement)
        {
            indent();
            _source ~= "finally\n";
            visitPossiblyIndented(node.finalDeclarationOrStatement);
        }
    }

    override void visit(TypeAstNode node)
    {
        if (_formattingFunctionReturn)
            _source ~= "(";
        if (node.autoOrBasicType)
            _source ~= node.autoOrBasicType.text;
        else if (node.qualifiedType)
            visit(node.qualifiedType);
        else if (node.functionType)
            visit(node.functionType);
        if (node.templateInstance)
            visit(node.templateInstance);
        if (_formattingFunctionReturn)
            _source ~= ")";
        if (node.modifier)
            visit(node.modifier);
    }

    override void visit(TypeModifierAstNode node)
    {
        with(ModifierKind) switch (node.kind)
        {
            case arrayDynDim:
                _source ~= "[]";
                goto default;
            case arrayStatDim:
                _source ~= "[";
                if (node.staticDimension)
                    visit(node.staticDimension);
                _source ~= "]";
                goto default;
            case pointer:
                _source ~= "*";
                goto default;
            default:
        }
        if (node.modifier)
            visit(node.modifier);
    }

    override void visit(UnaryExpressionAstNode node)
    {
        if (node.prefix)
            _source ~= node.prefix.text;

        node.accept(this);
    }

    override void visit(UnionDeclarationAstNode node)
    {
        indent();
        node.visitAtAttributes(this);
        _source ~= "union ";
        _source ~= node.name.text;
        if (node.templateParameters)
            visit(node.templateParameters);
        _source ~= "\n";
        indent();
        _source ~= "{\n";
        growIndentLevel();
        if (node.declarations)
            visit(node.declarations);
        shrinkIndentLevel();
        indent();
        _source ~= "}\n";
    }

    override void visit(UnitAstNode node)
    {
        _source ~= "unit " ~ node.identifiers.tokenChainText;
        semicolonAndNewLine();
        node.accept(this);
    }

    override void visit(VariableDeclarationAstNode node)
    {
        node.visitAtAttributes(this);
        if (node.isStatic)
            _source ~= "static ";
        if (node.storageClass)
            _source ~= node.storageClass.text ~ " ";
        if (node.type)
            visit(node.type);
        space();
        foreach(i, v; node.list)
        {
            visit(v);
            if (i != node.list.length - 1)
                _source ~= ", ";
        }
    }

    override void visit(VariableDeclarationItemAstNode node)
    {
        if (node.name)
            _source ~= node.name.text;
        if (node.initializer)
        {
            _source ~= " = ";
            visit(node.initializer);
        }
    }

    override void visit(VersionBlockDeclarationAstNode node)
    {
        indent();
        _source ~= "version";
        visit(node.versionExpression);
        _source ~= "\n";
        indent();
        _source ~= "{\n";
        growIndentLevel();
        if (node.trueDeclarations)
            visit(node.trueDeclarations);
        shrinkIndentLevel();
        indent();
        _source ~= "}\n";
        if (node.falseDeclarations)
        {
            indent();
            _source ~= "else\n";
            indent();
            _source ~= "{\n";
            growIndentLevel();
            visit(node.falseDeclarations);
            shrinkIndentLevel();
            indent();
            _source ~= "}\n";
        }
    }

    override void visit(VersionBlockStatementAstNode node)
    {
        indent();
        _source ~= "version";
        visit(node.versionExpression);
        _source ~= "\n";
        indent();
        _source ~= "{\n";
        growIndentLevel();
        if (node.trueDeclarationsOrStatements)
            visit(node.trueDeclarationsOrStatements);
        shrinkIndentLevel();
        indent();
        _source ~= "}\n";
        if (node.falseDeclarationsOrStatements)
        {
            indent();
            _source ~= "else\n";
            indent();
            _source ~= "{\n";
            growIndentLevel();
            visit(node.falseDeclarationsOrStatements);
            shrinkIndentLevel();
            indent();
            _source ~= "}\n";
        }
    }

    override void visit(VersionAndExpressionAstNode node)
    {
        if (node.rightExpression && node.leftExpression)
        {
            visit(node.leftExpression);
            _source ~= " & ";
            visit(node.rightExpression);
        }
        else if (node.leftExpression)
        {
            visit(node.leftExpression);
        }
    }

    override void visit(VersionOrExpressionAstNode node)
    {
        if (node.rightExpression && node.leftExpression)
        {
            visit(node.leftExpression);
            _source ~= " | ";
            visit(node.rightExpression);
        }
        else if (node.leftExpression)
        {
            visit(node.leftExpression);
        }
    }

    override void visit(VersionParenExpressionAstNode node)
    {
        _source ~= "(";
        if (node.expression)
            visit(node.expression);
        _source ~= ")";
    }

    override void visit(VersionPrimaryExpressionAstNode node)
    {
        if (node.not)
            _source ~= "!";
        if (node.identifier)
            _source ~= node.identifier.text;
        else
            node.accept(this);
    }

    override void visit(WhileStatementAstNode node)
    {
        indent();
        _source ~= "while (";
        if (node.condition)
            visit(node.condition);
        _source ~= ")\n";
        visitPossiblyIndented(node.declarationOrStatement);
    }
}

/**
 * Asserts that some code can be parsed and will be formatted in a specific way.
 *
 * Params:
 *      code = The source code.
 *      expected = The source code as it should be formatted.
 */
void test(const(char)[] code, const(char)[] expected,
    string file = __FILE_FULL_PATH__, size_t line = __LINE__)
{
    import core.exception: AssertError;
    import std.string;
    import yatol.utils;

    if (lexAndParse(expected, file, line) is null)
    {
        throw new AssertError("the expected code is invalid", file, line);
    }

    UnitContainerAstNode uc = lexAndParse(code, file, line);

    if (uc is null)
    {
        throw new AssertError("the code to test is invalid", file, line);
    }
    AstFormatter af = new AstFormatter();

    af.visit(uc);

    if (af.formattedAst.stripRight != expected.stripRight)
    {
        writeln(af.formattedAst);
        throw new AssertError("the code is not formatted as expected", file, line);
    }
}
///
unittest
{
    string c = "unit      a   ;";
    string e = "unit a;";
    test(c, e);
}

unittest
{
    import std.exception, core.exception;
    assertThrown!AssertError(test(" ?? bbzz", "woops"));
    assertThrown!AssertError(test("unit a ;", "woops"));
    assertThrown!AssertError(test("woops", "unit a;"));
    assertThrown!AssertError(test("unit a ;", "unit a   ;"));
}

unittest
{
    string c = "unit a./*grrrr*/b;";
    string e = "unit a.b;";
    test(c, e);
}

unittest
{
    string c = "unit a; class A { class B {} }";
    string e =
"unit a;
class A
{
    class B
    {
    }
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; class A:B,c.D {}";
    string e =
"unit a;
class A: B, c.D
{
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; interface A:B,c.D {}";
    string e =
"unit a;
interface A: B, c.D
{
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; struct A {}";
    string e =
"unit a;
struct A
{
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; union A {}";
    string e =
"unit a;
union A
{
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; union A < T > {}";
    string e =
"unit a;
union A<T>
{
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; union A < T0 , T1 > {}";
    string e =
"unit a;
union A<T0, T1>
{
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; struct A < T > {}";
    string e =
"unit a;
struct A<T>
{
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; class A < T > {}";
    string e =
"unit a;
class A<T>
{
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; interface A < T > {}";
    string e =
"unit a;
interface A<T>
{
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; enum A:s8 {a=0,b=1}";
    string e =
"unit a;
enum A: s8
{
    a = 0,
    b = 1
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo();";
    string e =
"unit a;
function foo();
";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo<  >();";
    string e =
"unit a;
function foo<>();
";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo<T>(var T t){foo< s8 >(0);}";
    string e =
"unit a;
function foo<T>(var T t)
{
    foo<s8>(0);
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; var Foo<T0,T1> foo;";
    string e =
"unit a;
var Foo<T0, T1> foo;
";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){-- a ++ ;}";
    string e =
"unit a;
function foo()
{
    --a++;
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){a += b*c+8+d[e]+f[0..2]*g./* c */h;}";
    string e =
"unit a;
function foo()
{
    a += ((b * c) + (8 + (d[e] + (f[0 .. 2] * g.h))));
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){a = [0,1] + (a[1]); }";
    string e =
"unit a;
function foo()
{
    a = ([0, 1] + (a[1]));
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){a = b * c + d; }";
    string e =
"unit a;
function foo()
{
    a = ((b * c) + d);
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){a = b + c * d - 8; }";
    string e =
"unit a;
function foo()
{
    a = ((b + (c * d)) - 8);
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){a = b in c + 8; }";
    string e =
"unit a;
function foo()
{
    a = ((b in c) + 8);
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){call(p0,p1 );}";
    string e =
"unit a;
function foo()
{
    call(p0, p1);
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(): s8 {return 0: s8;}";
    string e =
"unit a;
function foo():(s8)
{
    return 0:s8;
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){if (a) {a++;} else {a--;}}";
    string e =
"unit a;
function foo()
{
    if (a)
    {
        a++;
    }
    else
    {
        a--;
    }
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){if (a) a++; else a--;}";
    string e =
"unit a;
function foo()
{
    if (a)
        a++;
    else
        a--;
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){if(const auto a = call()) {a++;}}";
    string e =
"unit a;
function foo()
{
    if (const auto a = call())
    {
        a++;
    }
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){if(var auto a = call()) {a++;}}";
    string e =
"unit a;
function foo()
{
    if (var auto a = call())
    {
        a++;
    }
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){while(true){a++;}}";
    string e =
"unit a;
function foo()
{
    while (true)
    {
        a++;
    }
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){while(true)a++;}";
    string e =
"unit a;
function foo()
{
    while (true)
        a++;
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){foreach(const auto i;I){if (0)
    {break(@ here ) afterCall();}}}";
    string e =
"unit a;
function foo()
{
    foreach(const auto i; I)
    {
        if (0)
        {
            break(@here) afterCall();
        }
    }
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){foreach(const auto i, var auto b; I){if (0)
    {break(@ here ) afterCall();}}}";
    string e =
"unit a;
function foo()
{
    foreach(const auto i, var auto b; I)
    {
        if (0)
        {
            break(@here) afterCall();
        }
    }
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){foreach(const auto i;I)if (0)break(@ here) afterCall();}";
    string e =
"unit a;
function foo()
{
    foreach(const auto i; I)
        if (0)
            break(@here) afterCall();
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){foreach(const auto i;I)if (0)continue(@here) afterCall();}";
    string e =
"unit a;
function foo()
{
    foreach(const auto i; I)
        if (0)
            continue(@here) afterCall();
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){foreach(const auto i; 0 .. 1){ callThat();}}";
    string e =
"unit a;
function foo()
{
    foreach(const auto i; 0 .. 1)
    {
        callThat();
    }
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){foreach(const auto i;I){if (0)
    {continue afterCall();} andDontDoThis();}}";
    string e =
"unit a;
function foo()
{
    foreach(const auto i; I)
    {
        if (0)
        {
            continue afterCall();
        }
        andDontDoThis();
    }
}";
    test(c, e);
}

unittest
{
    string c = "unit a; @A@B function foo(const s32 a, b):  s64;";
    string e =
"unit a;
@A @B function foo(const s32 a, b):(s64);
";
    test(c, e);
}

unittest
{
    string c = "unit a; static  function foo(const s32 a;  var  s32 b);";
    string e =
"unit a;
static function foo(const s32 a; var s32 b);
";
    test(c, e);
}

unittest
{
    string c = "unit a; aka funcPtr = static function _ (const s32 a, b):  s64[ ]   ;";
    string e =
"unit a;
aka funcPtr = static function _(const s32 a, b):(s64)[];
";
    test(c, e);
}

unittest
{
    string c = "unit a; aka funcPtr =  static function _ (const s32 a; var s8 b);";
    string e =
"unit a;
aka funcPtr = static function _(const s32 a; var s8 b);
";
    test(c, e);
}

unittest
{
    string c = "unit a; aka funcPtr = function _ (const s32 a, b):  s64[ ];";
    string e =
"unit a;
aka funcPtr = function _(const s32 a, b):(s64)[];
";
    test(c, e);
}

unittest
{
    string c = "unit a; aka FuncT = function _():(function _() : s8*)[] ;";
    string e =
"unit a;
aka FuncT = function _():(function _():(s8)*)[];
";
    test(c, e);
}

unittest
{
    string c = "unit a; aka FuncT = function< T >()  : T [ ] ;";
    string e =
"unit a;
aka FuncT = function <T>():(T)[];
";
    test(c, e);
}

unittest
{
    string c = "unit a; const s8[2] a = [0:s8,1:s8], b=[2:s8,3:s8];";
    string e =
"unit a;
const s8[2] a = [0:s8, 1:s8], b = [2:s8, 3:s8];
";
    test(c, e);
}

unittest
{
    string c = "unit a; var s8* ** a  ,  b, c;";
    string e =
"unit a;
var s8*** a, b, c;
";
    test(c, e);
}

unittest
{
    string c = "unit a; class A {static const s8 a;}";
    string e =
"unit a;
class A
{
    static const s8 a;
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; class A {static const s8 a;static const s8 b;}";
    string e =
"unit a;
class A
{
    static const s8 a;
    static const s8 b;
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; version(a){const s8 b;}else{const s8 c;}";
    string e =
"unit a;
version(a)
{
    const s8 b;
}
else
{
    const s8 c;
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; version(!  a){}";
    string e =
"unit a;
version(!a)
{
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; version(!(a)){}";
    string e =
"unit a;
version(!(a))
{
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; version(a &   b){const s8 b;}else{const s8 c;}";
    string e =
"unit a;
version(a & b)
{
    const s8 b;
}
else
{
    const s8 c;
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; version((a &   b)){const s8 b;}else{const s8 c;}";
    string e =
"unit a;
version((a & b))
{
    const s8 b;
}
else
{
    const s8 c;
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; version(  a |   b){const s8 b;}else{const s8 c;}";
    string e =
"unit a;
version(a | b)
{
    const s8 b;
}
else
{
    const s8 c;
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){version(a){const s8 b;}else{const s8 c;}}";
    string e =
"unit a;
function foo()
{
    version(a)
    {
        const s8 b;
    }
    else
    {
        const s8 c;
    }
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; import(1) system,classes,io.streams; import other;";
    string e =
"unit a;
import(1) system, classes, io.streams;
import other;
";
    test(c, e);
}

unittest
{
    string c = "unit a; protection(strict)";
    string e =
"unit a;
protection(strict)";
    test(c, e);
}

unittest
{
    string c ="unit a;function foo(){
    switch(e)
    {
    on(0) doThat();
    on(1) doThis();
    else doThisAndThat();
    }
}";
    string e =
"unit a;
function foo()
{
    switch(e)
    {
    on(0)
        doThat();
    on(1)
        doThis();
    else
        doThisAndThat();
    }
}
";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){throw Error.construct(text);}";
    string e =
"unit a;
function foo()
{
    throw Error.construct(text);
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){try doThis();
        on(Error1 e1, Error2 e2) doThat; finally cleanup();}";
    string e =
"unit a;
function foo()
{
    try
        doThis();
    on(Error1 e1, Error2 e2)
        doThat;
    finally
        cleanup();
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){assert(true & true);}";
    string e =
"unit a;
function foo()
{
    assert((true & true));
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){label L0  ;  }";
    string e =
"unit a;
function foo()
{
    label L0;
}";
    test(c, e);
}

unittest
{
    string c = "unit a; @a function foo() {@b@c init auto d = 0;}";
    string e =
"unit a;
@a function foo()
{
    @b @c init auto d = 0;
}";
    test(c, e);
}

unittest
{
    string c = "unit a; @foo struct Foo{@bar class Bar{@baz interface Baz{}}}";
    string e =
"unit a;
@foo struct Foo
{
    @bar class Bar
    {
        @baz interface Baz
        {
        }
    }
}";
    test(c, e);
}

unittest
{
    string c = "unit a; @a template Foo<> {}";
    string e =
"unit a;
@a template Foo<>
{
}";
    test(c, e);
}

unittest
{
    string c = "unit a; @a template Foo<T0 , T1> {}";
    string e =
"unit a;
@a template Foo<T0, T1>
{
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){ if (a + b > c + d){} }";
    string e =
"unit a;
function foo()
{
    if (((a + b) > (c + d)))
    {
    }
}";
    test(c, e);
}

unittest
{
    string c = "unit a; function foo(){ if (a && b > c){} }";
    string e =
"unit a;
function foo()
{
    if ((a && (b > c)))
    {
    }
}";
    test(c, e);
}

