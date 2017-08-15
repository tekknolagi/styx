/**
 * Semantic processing for the VersionBlockDeclaration and VersionBlockStatement
 *
 * to maintain unittest coverage: 96%
 */
module yatol.semantic.versions;

import
    std.algorithm.iteration : each;
import
    yatol.parser.ast;

// TODO: create the list of predefined versions + check for conflicts in the driver.

/**
 * This $(D AstVisitor) is used to determine which branch of a
 * VersionBlockDeclaration or of a VersionBlockStatement is valid.
 * After the visit, the nodes of the unreachable branches are supressed.
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

    /// Constructs an instance with an AST and a list of user defined version.
    this(UnitContainerAstNode uc, string[] userVersions)
    {
        _userVersions = userVersions;
        visit(uc);
    }

    /// $(D version) semantic should never fail.
    bool success(){return true;}

    override void visit(VersionBlockDeclarationAstNode node)
    {
        visit(node.versionExpression);
        node.isTrue = evaluate();

        if (node.isTrue)
        {
            if (node.falseDeclarations)
            {
                destroy(node.falseDeclarations);
                node.falseDeclarations = null;
            }
            node.trueDeclarations.each!(a => visit(a));
        }
        else
        {
            if (node.trueDeclarations)
            {
                destroy(node.trueDeclarations);
                node.trueDeclarations = null;
            }
            node.falseDeclarations.each!(a => visit(a));
        }
    }

    override void visit(VersionBlockStatementAstNode node)
    {
        visit(node.versionExpression);
        node.isTrue = evaluate();

        if (node.isTrue)
        {
            if (node.falseDeclarationsOrStatements)
            {
                destroy(node.falseDeclarationsOrStatements);
                node.falseDeclarationsOrStatements = null;
            }
            node.trueDeclarationsOrStatements.each!(a => visit(a));
        }
        else
        {
            if (node.trueDeclarationsOrStatements)
            {
                destroy(node.trueDeclarationsOrStatements);
                node.trueDeclarationsOrStatements = null;
            }
            node.falseDeclarationsOrStatements.each!(a => visit(a));
        }
    }

    override void visit(VersionParenExpressionAstNode node)
    {
        const bool[] savedBoolStack = _boolStack.dup;
        const VersionOperator[] savedOperatorStack = _operatorStack.dup;
        _boolStack.length = 0;
        _operatorStack.length = 0;

        node.accept(this);

        _boolStack = savedBoolStack.dup ~ evaluate;
        _operatorStack = savedOperatorStack.dup;
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

    override void visit(VersionPrimaryExpressionAstNode node)
    {
        node.accept(this);
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
    import yatol.utils;

    UnitContainerAstNode uc = lexAndParse(code, file, line);

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
    import yatol.utils;

    UnitContainerAstNode uc = lexAndParse(code, file, line);

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

unittest
{
    import std.exception: assertThrown;
    import core.exception: AssertError;
    assertThrown!AssertError(assertFirstVersionIsTrue(q{unit}, ["a"]));
    assertThrown!AssertError(assertFirstVersionIsFalse(q{unit}, ["a"]));
    assertThrown!AssertError(assertFirstVersionIsTrue(q{unit a;version(b) const s32 b;}, ["a"]));
    assertThrown!AssertError(assertFirstVersionIsFalse(q{unit a;version(a) const s32 b;}, ["a"]));
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
    assertFirstVersionIsTrue(q{
        unit a;
        version(a & b) const s32 b; else const s32 c;
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
    assertFirstVersionIsTrue(q{
        unit a;
        version((a | b) & c) const s32 b;
    }, ["b", "c"]);
    assertFirstVersionIsTrue(q{
        unit a;
        version((a | b) & (c | d)) const s32 b;
    }, ["a", "c"]);
    assertFirstVersionIsFalse(q{
        unit a;
        version((a | b) & (c | d)) const s32 b;
    }, ["a", "e"]);
}

unittest // version statements
{
    assertFirstVersionIsTrue(q{
        unit a;
        version(a) function foo()
        {
            version(b)
            {
                const s32 c;
            }
            else const s32 d;
        }
    }, ["a","b"]);
    assertFirstVersionIsTrue(q{
        unit a;
        version(a) function foo()
        {
            version(b)
            {
                const s32 c;
            }
            else const s32 d;
        }
    }, ["a"]);
}

