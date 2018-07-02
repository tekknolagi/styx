/**
 * Utilities and miscellaneous sub routines.
 */
module styx.utils;

import
    std.stdio;
import
    styx.lexer, styx.parser, styx.ast, styx.symbol;

/**
 * Lexes and parses some code. Usually used for the embedded compiler tests.
 *
 * Params:
 *      code = The source code to process.
 * Returns:
 *      The AST for the code passed as parameter.
 */
UnitAstNode lexAndParse(const(char)[] code, string file = __FILE_FULL_PATH__,
    size_t line = __LINE__)
{
    Lexer lx;
    lx.setSourceFromText(code, file, line);
    lx.lex();
    return Parser(&lx).parse();
}
///
unittest
{
    UnitAstNode u = lexAndParse("unit a; const s8 a = 42:s8;");
    assert(u !is null);
}

private class DeclarationFinder: AstVisitor
{
private:

    const(char)[][] _fqn;
    size_t _index;
    AstNode _retv;

    void visitImpl(Node)(Node node)
    {
        foreach (member; __traits(allMembers, Node))
            static if (member == "name")
        {
            if (_index < _fqn.length && node.name && node.name.text == _fqn[_index])
            {
                ++_index;
                if (_index == _fqn.length)
                {
                    _retv = node;
                    break;
                }
                else node.accept(this);
            }
        }
        node.accept(this);
    }

public:

    alias visit = AstVisitor.visit;

    this(AstNode node, const(char)[][] fqn)
    {
        _fqn = fqn;
        if (fqn.length)
            visit(node);
    }

    mixin(genVisitMethods("visitImpl(node);"));
}

/**
 * Finds a declaration in an AST.
 *
 * Params:
 *      T = The expected AstNode derived class.
 *      node = An AstNode.
 *      fqn = The declaration fully qualified name.
 *
 * Returns:
 *      The $(D AstNode) on success, otherwise $(D null).
 */
T findDeclaration(T = AstNode, Node : AstNode)(Node node, const(char)[] fqn)
{
    import std.algorithm.iteration: splitter;
    import std.array: array;
    return findDeclaration!(T, Node)(node, fqn.splitter('.').array);
}
///
unittest
{
    UnitAstNode u = lexAndParse("unit a; class Foo {var s8 a;}");
    assert(findDeclaration(u, "Foo.a") !is null);
    assert(findDeclaration!VariableDeclarationItemAstNode(u, "Foo.a") !is null);
    assert(findDeclaration!StructDeclarationAstNode(u, "Foo.Bar") is null);
}

/// ditto
T findDeclaration(T = AstNode, Node : AstNode)(Node node, const(char)[][] fqn)
{
    auto df = new DeclarationFinder(node, fqn);
    return cast(T) df._retv;
}
///
unittest
{
    UnitAstNode u = lexAndParse("unit a; class Foo{struct Bar{ function fun(); }}");
    assert(findDeclaration(u, ["Foo", "Bar", "fun"]) !is null);
    assert(findDeclaration!FunctionDeclarationAstNode(u, ["Foo", "Bar", "fun"]) !is null);
}

