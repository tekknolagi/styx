module styx.semantic.echo;

import
    std.stdio, std.conv, std.exception, std.algorithm;
import
    styx.token, styx.lexer, styx.ast, styx.session;

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
    with(node.compilerEcho)
        switch(command.text)
        {
        case "semver":
            if (parameters.length)
                session.error(_lx.filename, node.startPos,
                    "no parameter expected for `version`");

            node.identifierOrKeywordOrLiteral = &versionToken;
            node.compilerEcho = null;
            break;

        case "line":
            if (parameters.length)
                session.error(_lx.filename, node.startPos,
                    "no parameter expected for `line`");

            node.identifierOrKeywordOrLiteral = Token.intLiteral(0, 0, to!(char[])(node.startPos.line));
            node.compilerEcho = null;
            break;

        case "is":
            if (parameters.length != 2)
            {
                session.error(_lx.filename, node.startPos,
                    "2 parameters expected for `is`, and not %d", parameters.length);
                break;
            }
            if (!parameters[0].type)
            {
                session.error(_lx.filename, node.startPos,
                    "Type expected as first `is` parameter");
                break;
            }
            if (!parameters[0].type.symbol)
            {
                session.error(_lx.filename, node.startPos,
                    "Type represented by first `is` parameter is not solved");
                break;
            }
            if (parameters[1].type && !parameters[1].type.symbol)
            {
                session.error(_lx.filename, node.startPos,
                    "Type represented by second `is` parameter is not solved");
                break;
            }
            with (TokenType) if (!parameters[1].type &&
                !parameters[1].keyword.among(class_, interface_, struct_))
            {
                session.error(_lx.filename, node.startPos,
                    "Type, `class` `interface`, `struct` or `union` expected as second `is` parameter");
                break;
            }
            if (parameters[0].type && parameters[1].type)
            {
                node.identifierOrKeywordOrLiteral =
                    parameters[0].type.symbol == parameters[1].type.symbol ? &trueToken : &falseToken;
            }
            else
            {
                switch (parameters[1].keyword)
                {
                case TokenType.class_:
                    node.identifierOrKeywordOrLiteral =
                    cast(ClassDeclarationAstNode) parameters[0].type.symbol.astNode ? &trueToken : &falseToken;
                    break;
                case TokenType.interface_:
                    node.identifierOrKeywordOrLiteral =
                    cast(InterfaceDeclarationAstNode) parameters[0].type.symbol.astNode ? &trueToken : &falseToken;
                    break;
                case TokenType.struct_:
                    node.identifierOrKeywordOrLiteral =
                    cast(StructDeclarationAstNode) parameters[0].type.symbol.astNode ? &trueToken : &falseToken;
                    break;
                case TokenType.union_:
                    node.identifierOrKeywordOrLiteral =
                    cast(UnionDeclarationAstNode) parameters[0].type.symbol.astNode ? &trueToken : &falseToken;
                    break;
                default: assert(false);
                }
            }
            node.compilerEcho = null;
            break;

        default:
            session.error(_lx.filename, node.startPos,
                "invalid echo command `%s`", command.text);
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
    import styx.utils, styx.ast_formatter, styx.parser;

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


