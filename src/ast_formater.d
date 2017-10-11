module yatol.ast_formater;

import
    std.array, std.algorithm.iteration, std.stdio;
import
    yatol.ast, yatol.token;


/**
 * An AST visitor that formats the nodes.
 *
 * Comments, since already abscent from the AST, are not included.
 */
class AstFormater : AstVisitor
{
    alias visit = AstVisitor.visit;

private:

    Appender!(char[]) _source;
    char[1024] _indentText = ' ';
    size_t _indentLevel;

    void growIndentLevel()
    {
        _indentLevel += (_indentLevel == 1024) ? 0 : 4;
    }

    void shrinkIndentLevel()
    {
        _indentLevel -= (_indentLevel == 0) ? 0 : 4;
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

public:

    /// Returns: the formatted ast.
    char[] formattedAst() {return _source.data;}

    override void visit(AkaDeclarationAstNode node)
    {
        indent();
        _source ~= "is ";
        node.accept(this);
        _source ~= "aka ";
        _source ~= node.name.text;
        semicolonAndNewLine();
    }

    override void visit(AtAttributeAstNode node)
    {
        indent();
        _source ~= "@";
        _source ~= node.identifierOrKeyword.text;
        space();
    }

    override void visit(BinaryExpressionAstNode node)
    {
        visit(node.left);
        space();
        _source ~= node.operator.text;
        space();
        visit(node.right);
    }

    override void visit(BlockStatementAstNode node)
    {
        indent();
        _source ~= "{\n";
        growIndentLevel();
        node.accept(this);
        shrinkIndentLevel();
        _source ~= "}\n";
    }

    override void visit(BreakStatementAstNode node)
    {
        _source ~= "break";
        if (node.label)
        {
            _source ~= "(";
            _source ~= node.label.text;
            _source ~= ")";
        }
        if (node.expression)
        {
            space();
            visit(node.expression);
        }
        _source ~= ";\n";
    }

    override void visit(CallParametersAstNode node)
    {
        foreach(i, p; node.parameters)
        {
            visit(p);
            if (i != node.parameters.length - 1)
                _source ~= " ,";
        }
    }

    override void visit(ClassDeclarationAstNode node)
    {
        indent();
        _source ~= "class ";
        _source ~= node.name.text;
        if (node.inheritanceList.length)
        {
            _source ~= ": ";
            foreach(i, c; node.inheritanceList)
            {
                visit(c);
                if (i != node.inheritanceList.length - 1)
                    _source ~= " ,";
            }
        }
        _source ~= "\n";
        indent();
        _source ~= "{\n";
        growIndentLevel();
        node.declarations.each!(a => visit(a));
        shrinkIndentLevel();
        indent();
        _source ~= "}\n";
    }

    override void visit(ContinueStatementAstNode node)
    {
        _source ~= "continue";
        if (node.expression)
        {
            space();
            visit(node.expression);
        }
        _source ~= ";\n";
    }

    override void visit(EnumDeclarationAstNode node)
    {
        indent();
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
        if (node.variable)
            visit(node.variable);
        _source ~= "; ";
        if (node.singleOrRangeExpression)
            visit(node.singleOrRangeExpression);
        _source ~= ")\n";
        if (node.declarationOrStatement)
            visit(node.declarationOrStatement);
    }

    override void visit(FunctionDeclarationAstNode node)
    {
        indent();
        _source ~= "function ";
        if (node.header)
            visit(node.header);
        if (node.firstBodyToken.text == ";")
            _source ~= ";\n";
        else
        {
            _source ~= "\n";
            indent();
            _source ~= "{\n";
            growIndentLevel();
            node.declarationsOrStatements.each!(a => visit(a));
            shrinkIndentLevel();
            indent();
            _source ~= "}\n";
        }
    }

    override void visit(FunctionHeaderAstNode node)
    {
        if (node.name)
            _source ~= node.name.text;
        _source ~= "(";
        foreach(i, p; node.parameters)
        {
            visit(p);
            if (i != node.parameters.length - 1)
                _source ~= " ;";
        }
        _source ~= ")";
        if (node.returnType)
        {
            _source ~= ": ";
            visit(node.returnType);
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
        _source ~= node.variableList.tokenChainText(false, ", ");
    }

    override void visit(FunctionTypeAstNode node)
    {
        assert(0, "TODO " ~ __PRETTY_FUNCTION__);
    }

    override void visit(IdentifierChainAstNode node)
    {
        _source ~= node.chain.tokenChainText;
    }

    override void visit(UnitAstNode node)
    {
        _source ~= "unit " ~ node.identifiers.tokenChainText;
        semicolonAndNewLine();
        node.accept(this);
    }
}

/**
 * Asserts that some code can be parsed and will be formatted in a secific way.
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

    UnitContainerAstNode uc = lexAndParse(code, file, line);

    if (uc is null)
    {
        throw new AssertError("the code to test is invalid", file, line);
    }
    AstFormater af = new AstFormater();

    af.visit(uc);

    if (af._source.data.stripRight != expected.stripRight)
    {
        writeln(af._source.data);
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
    string c = "unit a; function foo();";
    string e =
"unit a;
function foo();
";
    test(c, e);
}


