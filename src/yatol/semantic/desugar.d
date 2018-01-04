module yatol.semantic.desugar;

import
    std.stdio;
import
    yatol.ast;

/**
 * This $(D AstVisitor) is used to rewrite syntactic shortcuts into their
 * matching construct.
 */
class DesugarVisitor: AstVisitor
{
    alias visit = AstVisitor.visit;

    private BlockStatementAstNode _inBlock;

    ///
    this(){}

    ///
    this(UnitContainerAstNode uc)
    {
        visit(uc);
    }

    override void visit(DeclarationAstNode node)
    {
        node.accept(this);
        with(DeclarationKind) switch(node.declarationKind)
        {
        case dkClass:
            if (node.declaration.classDeclaration.templateParameters)
                toTemplate(node, node.declaration.classDeclaration, dkClass);
            break;
        case dkFunction:
            if (node.declaration.functionDeclaration.templateParameters)
                toTemplate(node, node.declaration.functionDeclaration, dkFunction);
            break;
        case dkInterface:
            if (node.declaration.interfaceDeclaration.templateParameters)
                toTemplate(node, node.declaration.interfaceDeclaration, dkInterface);
            break;
        case dkStruct:
            if (node.declaration.structDeclaration.templateParameters)
                toTemplate(node, node.declaration.structDeclaration, dkStruct);
            break;
        case dkUnion:
            if (node.declaration.unionDeclaration.templateParameters)
                toTemplate(node, node.declaration.unionDeclaration, dkUnion);
            break;
        default:
            break;

        }
    }

    private void toTemplate(N)(DeclarationAstNode decl, N node, DeclarationKind oldKind)
    {
        DeclarationAstNode.Declaration old = decl.declaration;

        TemplateDeclarationAstNode td = new TemplateDeclarationAstNode;
        td.declarations = new DeclarationsAstNode;
        td.name = node.name;
        td.position = node.position;
        td.templateParameters = node.templateParameters;
        td.declarations.items = [new DeclarationAstNode];
        td.declarations.items[0].declarationKind = oldKind;
        td.declarations.items[0].declaration = old;

        decl.declaration.templateDeclaration = td;
        decl.declarationKind = DeclarationKind.dkTemplate;

        node.templateParameters = null;
    }

    //NOTE: do the same for ForeachStatement and WhileStatement ?
    override void visit(IfElseStatementAstNode node)
    {
        BlockStatementAstNode saved = _inBlock;

        if (node.condition)
            visit(node.condition);
        else if (node.ifVariable)
            visit(node.ifVariable);
        if (node.trueDeclarationOrStatement)
        {
            if (node.trueDeclarationOrStatement.statement &&
                node.trueDeclarationOrStatement.statement.statementKind == StatementKind.skBlock)
            {
                _inBlock = node.trueDeclarationOrStatement.statement.statement.block;
            }
            visit(node.trueDeclarationOrStatement);
        }
        if (node.falseDeclarationOrStatement)
        {
            if (node.falseDeclarationOrStatement.statement &&
                node.falseDeclarationOrStatement.statement.statementKind == StatementKind.skBlock)
            {
                _inBlock = node.falseDeclarationOrStatement.statement.statement.block;
            }
            visit(node.falseDeclarationOrStatement);
        }

        _inBlock = saved;
    }

    override void visit(DeclarationOrStatementAstNode node)
    {
        FlowControlBaseNode fc;
        BlockStatementAstNode b;
        bool isBreak;

        node.accept(this);

        if (node.statement &&
            (node.statement.statementKind == StatementKind.skBreak ||
             node.statement.statementKind == StatementKind.skContinue))
        {
            fc = node.statement.statement.breakStatement;
            isBreak = node.statement.statementKind == StatementKind.skBreak;
        }
        if (fc && fc.expression)
        {
            if (_inBlock is null)
            {
                b = new BlockStatementAstNode;
                b.position = fc.position;
                node.statement.statementKind = StatementKind.skBlock;
            }
            else b = _inBlock;
            node.statement.statement.block = b;
            b.declarationsOrStatements = new DeclarationsOrStatementsAstNode;
            DeclarationOrStatementAstNode dos1 = new DeclarationOrStatementAstNode;
            DeclarationOrStatementAstNode dos2 = new DeclarationOrStatementAstNode;
            b.declarationsOrStatements.items = [dos1, dos2];
            // expression
            StatementAstNode s1 = new StatementAstNode;
            s1.statementKind = StatementKind.skExpression;
            s1.statement.expression = new ExpressionStatementAstNode;
            s1.statement.expression.position = fc.position;
            s1.statement.expression.assignExpression = fc.expression;
            fc.expression = null;
            dos1.statement = s1;
            // break / continue
            StatementAstNode s2 = new StatementAstNode;
            s2.position = fc.position;
            if (isBreak)
            {
                s2.statementKind = StatementKind.skBreak;
                s2.statement.breakStatement = cast(BreakStatementAstNode) fc;
            }
            else
            {
                s2.statementKind = StatementKind.skContinue;
                s2.statement.continueStatement = cast(ContinueStatementAstNode) fc;
            }
            dos2.statement = s2;
        }
    }
}

/**
 * Asserts that some code can be parsed, desugared and finally formatted
 * in a specific way.
 *
 * Params:
 *      code = The source code.
 *      expected = The source code as it should be formatted after desugarization.
 */
void assertDesugaredAs(const(char)[] code, const(char)[] expected,
    string file = __FILE_FULL_PATH__, size_t line = __LINE__)
{
    import core.exception: AssertError;
    import std.string, std.stdio;
    import yatol.utils, yatol.ast_formatter;

    UnitContainerAstNode uc = lexAndParse(code, file, line);

    if (uc is null)
    {
        throw new AssertError("the code to test is invalid", file, line);
    }
    DesugarVisitor dv = new DesugarVisitor;
    AstFormatter af = new AstFormatter();

    dv.visit(uc);
    af.visit(uc);

    if (af.formattedAst.stripRight != expected.stripRight)
    {
        writeln(af.formattedAst);
        throw new AssertError("the code is not formatted as expected", file, line);
    }
}

unittest
{
    import std.exception, core.exception;
    assertThrown!AssertError(assertDesugaredAs(" ?? bbzz", "woops"));
    assertThrown!AssertError(assertDesugaredAs("unit a ;", "woops"));
}

unittest
{
    assertDesugaredAs("unit a; class Foo<T0, T1> {}",
"unit a;
template Foo<T0, T1>
{
    class Foo
    {
    }
}");
}

unittest
{
    assertDesugaredAs("unit a; function Foo<T0, T1>() {}",
"unit a;
template Foo<T0, T1>
{
    function Foo()
    {
    }
}");
}

unittest
{
    assertDesugaredAs("unit a; interface Foo<T0, T1> {}",
"unit a;
template Foo<T0, T1>
{
    interface Foo
    {
    }
}");
}

unittest
{
    assertDesugaredAs("unit a; struct Foo<T0, T1> {}",
"unit a;
template Foo<T0, T1>
{
    struct Foo
    {
    }
}");
}

unittest
{
    assertDesugaredAs("unit a; union Foo<T0, T1> {}",
"unit a;
template Foo<T0, T1>
{
    union Foo
    {
    }
}");
}

unittest
{
    assertDesugaredAs("unit a; protection(public)",
"unit a;
protection(public)");
}

unittest
{
    assertDesugaredAs(
"unit a;
function foo()
{
    if (true)
        break afterCall();
}",
"unit a;
function foo()
{
    if (true)
    {
        afterCall();
        break;
    }
}");
}

unittest
{
    assertDesugaredAs(
"unit a;
function foo()
{
    if (true)
        continue afterCall();
}",
"unit a;
function foo()
{
    if (true)
    {
        afterCall();
        continue;
    }
}");
}

unittest
{
    assertDesugaredAs(
"unit a;
function foo()
{
    if (true) {
        continue afterCall();
    } else {
        break afterCall();
    }
}",
"unit a;
function foo()
{
    if (true)
    {
        afterCall();
        continue;
    }
    else
    {
        afterCall();
        break;
    }
}");
}

unittest
{
    assertDesugaredAs(
"unit a;
function foo()
{
    if (const auto a = true) {
        continue afterCall();
    }
}",
"unit a;
function foo()
{
    if (const auto a = true)
    {
        afterCall();
        continue;
    }
}");
}

