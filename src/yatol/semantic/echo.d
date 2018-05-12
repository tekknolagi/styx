module yatol.semantic.echo;

import
    std.stdio, std.conv, std.exception;
import
    yatol.token, yatol.lexer, yatol.ast, yatol.session;

/**
 * Visitor that transforms CompilerEchoes in other expressions.
 */
class EchoSemantic: AstVisitor
{
    alias visit = AstVisitor.visit;

private:

    Lexer* _lx;

    void evaluate(PrimaryExpressionAstNode node)
    {
        switch(node.compilerEcho.command.text)
        {
        case "semver":
            if (node.compilerEcho.parameters.length)
                session.error(_lx.filename, node.position,
                    "no parameter expected for `version`");

            node.identifierOrKeywordOrLiteral = &versionToken;
            node.compilerEcho = null;
            break;

        case "line":
            if (node.compilerEcho.parameters.length)
                session.error(_lx.filename, node.position,
                    "no parameter expected for `line`");

            node.identifierOrKeywordOrLiteral = Token.intLiteral(0, 0, to!(char[])(node.position.line));
            node.compilerEcho = null;
            break;

        default:
            session.error(_lx.filename, node.position,
                "invalid echo command `%s`", node.compilerEcho.command.text);
        }
    }

public:

    /// Creates an instance and start to visit from node.
    this(UnitAstNode node, Lexer* lexer)
    {
        assert(node);
        assert(lexer);
        _lx = lexer;
        visit(node);
    }

    override void visit(UnaryExpressionAstNode node)
    {
        if (node.primary && node.primary.compilerEcho)
            evaluate(node.primary);
        node.accept(this);
    }
}

/**
 * Asserts that some code can be parsed, and that the compiler echoes are formatted
 * in a specific way.
 *
 * Params:
 *      code = The source code.
 *      expected = The source code as it should be formatted after desugarization.
 */
void assertEchoedAs(const(char)[] code, const(char)[] expected,
    string file = __FILE_FULL_PATH__, size_t line = __LINE__)
{
    import std.string, core.exception;
    import yatol.utils, yatol.ast_formatter, yatol.parser;

    Lexer lx;
    lx.setSourceFromText(code, file, line);
    lx.lex();
    UnitAstNode u = Parser(&lx).parse();


    if (u is null)
    {
        throw new AssertError("the code to test is invalid", file, line);
    }
    EchoSemantic es = new EchoSemantic(u, &lx);
    AstFormatter af = new AstFormatter();

    es.visit(u);
    af.visit(u);

    if (af.formattedAst.stripRight != expected.stripRight)
    {
        writeln(af.formattedAst);
        throw new AssertError("the code is not formatted as expected", file, line);
    }
}

unittest
{
    import core.exception: AssertError;
    import std.exception: assertThrown;
    assertThrown!(AssertError)(assertEchoedAs("bzzz","grrr"));
    assertThrown!(AssertError)(assertEchoedAs("unit a;","grrr"));
}

unittest
{
    string ln = to!string(__LINE__ + 1);
    assertEchoedAs("unit a; function foo(){const usize a = echo(line); }",
"unit a;
function foo()
{
    const usize a = " ~ ln ~ ";
}");
}

unittest
{
    assertEchoedAs("unit a; function foo(){const char[] a = echo(semver); }",
"unit a;
function foo()
{
    const char[] a = " ~ versionToken.text ~ ";
}");
}

