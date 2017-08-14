/**
 * Semantic processing for the VersionBlockDeclaration.
 */
module yatol.semantic.versions;

import
    yatol.parser.ast;

// TODO: create the list of predefined versions + vhevk for conflicts in the driver.
// TODO-csemantic: plug VersionEvaluatorVisitor to unit semantic.

/**
 * This $(D AstVisitor) is used to determine which branch of a
 * VersionBlockDeclaration is valid. After the visit, the nodes of the
 * unreachable branches are supressed.
 */
final class VersionEvaluatorVisitor: AstVisitor
{
    alias visit = AstVisitor.visit;

private:

    string[] _userVersions;
    bool[string] _predefinedVersions;

    bool isPredefinedVersion(const(char)[] value)
    {
        return (value in _predefinedVersions) !is null;
    }

    bool isPredefinedVersionSet(const(char)[] value)
    {
        return *(value in _predefinedVersions);
    }

    enum VersionOperator
    {
        and,
        or
    }

    bool[] _boolStack;
    VersionOperator[] _operatorStack;

    bool evaluate()
    {
        bool result = _boolStack[0];

        foreach(i, b; _boolStack[1..$])
        {
            if (_operatorStack[i] == VersionOperator.and)
                result &= b;
            else
                result |= b;
        }

        _boolStack.length = 0;
        _operatorStack.length = 0;

        return result;
    }

public:

    /// Constructs an instance with a list of user defined version.
    this(string[] userVersions)
    {
        _userVersions = userVersions;
    }

    override void visit(VersionPrimaryExpressionAstNode node)
    {
        if (node.identifier)
        {
            if (isPredefinedVersion(node.identifier.text))
            {
                node.isDefined = isPredefinedVersionSet(node.identifier.text);
                _boolStack ~= node.isDefined;
            }
            else
            {
                import std.algorithm.searching: canFind;
                node.isDefined = canFind(_userVersions, node.identifier.text);
                _boolStack ~= node.isDefined;
            }
        }
        else super.visit(node);
    }

    override void visit(VersionBlockDeclarationAstNode node)
    {
        node.accept(this);
        node.isTrue = evaluate();

        if (node.isTrue && node.falseDeclarationOrBlock)
            destroy(node.falseDeclarationOrBlock);
        else if (!node.isTrue && node.trueDeclarationOrBlock)
            destroy(node.trueDeclarationOrBlock);
    }

    override void visit(VersionOrExpressionAstNode node)
    {
        node.accept(this);
        if (node.leftExpression && node.rightExpression)
            _operatorStack ~= VersionOperator.or;
    }

    override void visit(VersionAndExpressionAstNode node)
    {
        node.accept(this);
        if (node.leftExpression && node.rightExpression)
            _operatorStack ~= VersionOperator.and;
    }
}

/**
 * Asserts that the first VersionBlockDeclarationAstNode in the AST evaluates to true.
 *
 * Params:
 *      code = The source code. The version must be declared right after the unit.
 *      userVersions = The custom versions, as set with "--versions" in the compiler.
 */
void assertFirstVersionIsTrue(const(char)[] code, string[] userVersions,
    string file = __FILE_FULL_PATH__, size_t line = __LINE__)
{
    import core.exception: AssertError;
    import yatol.lexer, yatol.parser;

    Lexer lx;
    lx.setSourceFromText(code, file, line, 1);
    lx.lex;
    Parser pr = Parser(&lx);
    UnitContainerAstNode uc = pr.parse();
    if (uc is null || !uc.mainUnit || !uc.mainUnit.declarations.length ||
        !uc.mainUnit.declarations[0].versionBlockDeclaration)
    {
        throw new AssertError("the code to test is invalid", file, line);
    }
    VersionEvaluatorVisitor vev = new VersionEvaluatorVisitor(userVersions);

    vev.visit(uc);
    const VersionBlockDeclarationAstNode vb = uc.mainUnit.declarations[0].versionBlockDeclaration;

    if (!vb.isTrue)
    {
        throw new AssertError("the version evaluates to false instead of true",
            file, vb.position.line);
    }
}

/**
 * Asserts that the first VersionBlockDeclarationAstNode in the AST
 * does not evaluate to true.
 *
 * Params:
 *      code = The source code. The version must be declared right after the unit.
 *      userVersions = The custom versions, as set with "--versions" in the compiler.
 */
void assertFirstVersionIsFalse(const(char)[] code, string[] userVersions,
    string file = __FILE_FULL_PATH__, size_t line = __LINE__)
{
    import core.exception: AssertError;
    import yatol.lexer, yatol.parser;

    Lexer lx;
    lx.setSourceFromText(code, file, line, 1);
    lx.lex;
    Parser pr = Parser(&lx);
    UnitContainerAstNode uc = pr.parse();
    if (uc is null || !uc.mainUnit || !uc.mainUnit.declarations.length ||
        !uc.mainUnit.declarations[0].versionBlockDeclaration)
    {
        throw new AssertError("the code to test is invalid", file, line);
    }
    VersionEvaluatorVisitor vev = new VersionEvaluatorVisitor(userVersions);

    vev.visit(uc);
    const VersionBlockDeclarationAstNode vb = uc.mainUnit.declarations[0].versionBlockDeclaration;

    if (vb.isTrue)
    {
        throw new AssertError("the version evaluates to true instead of false",
            file, vb.position.line);
    }
}
///
unittest
{
    assertFirstVersionIsTrue(q{
        unit a;
        version(a) const s32 b;
    }, ["a"]);
    assertFirstVersionIsFalse(q{
        unit a;
        version(a & b) const s32 b;
    }, ["a"]);
}

unittest // single versions
{
    assertFirstVersionIsTrue(q{
        unit a;
        version(a) const s32 b;
    }, ["a"]);
    assertFirstVersionIsFalse(q{
        unit a;
        version(a) const s32 b;
    }, ["b"]);
}

unittest // simple "and" versions
{
    assertFirstVersionIsTrue(q{
        unit a;
        version(a & b) const s32 b;
    }, ["a", "b"]);
    assertFirstVersionIsFalse(q{
        unit a;
        version(a & b) const s32 b;
    }, ["a"]);
    assertFirstVersionIsTrue(q{
        unit a;
        version(a & b & c) const s32 b;
    }, ["a", "b", "c"]);
    assertFirstVersionIsTrue(q{
        unit a;
        version(a & (b & c)) const s32 b;
    }, ["a", "b", "c"]);
}

unittest // simple "or" versions
{
    assertFirstVersionIsTrue(q{
        unit a;
        version(a | b) const s32 b;
    }, ["a", "b"]);
    assertFirstVersionIsTrue(q{
        unit a;
        version(a | b) const s32 b;
    }, ["b"]);
    assertFirstVersionIsTrue(q{
        unit a;
        version(a | b) const s32 b;
    }, ["a"]);
    assertFirstVersionIsFalse(q{
        unit a;
        version(a | b) const s32 b;
    }, ["c"]);
    assertFirstVersionIsTrue(q{
        unit a;
        version(a | b | c) const s32 b;
    }, ["c"]);
    assertFirstVersionIsTrue(q{
        unit a;
        version(a | (b | c)) const s32 b;
    }, ["b"]);
}

unittest // more complex cases involving precedence
{
    assertFirstVersionIsTrue(q{
        unit a;
        version(a | b & c) const s32 b;
    }, ["b", "c"]);
    assertFirstVersionIsFalse(q{
        unit a;
        version(a | b & c) const s32 b;
    }, ["b"]);
    assertFirstVersionIsFalse(q{
        unit a;
        version((a | b) & c) const s32 b;
    }, ["b"]);
}

