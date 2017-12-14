module yatol.semantic.desugar;

import
    yatol.ast;

/**
 * This $(D AstVisitor) is used to rewrite syntactic shortcuts into their
 * matching construct.
 */
class DesugarVisitor: AstVisitor
{
    alias visit = AstVisitor.visit;

    this(){}

    this(UnitContainerAstNode uc)
    {
        visit(uc);
    }

    override void visit(DeclarationAstNode node)
    {
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
        td.templateParameters = node.templateParameters;
        td.declarations.items = [new DeclarationAstNode];
        td.declarations.items[0].declarationKind = oldKind;
        td.declarations.items[0].declaration = old;

        decl.declaration.templateDeclaration = td;
        decl.declarationKind = DeclarationKind.dkTemplate;

        node.templateParameters = null;
    }
}

/**
 * Asserts that some code can be parsed, desigared and finally formatted
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

