/**
 * Semantic processing for the VersionBlockDeclaration and VersionBlockStatement.
 */
module styx.semantic.versions;

import
    std.algorithm.iteration : each;
import
    styx.ast, styx.lexer, styx.session;

// TODO: create the list of predefined versions + check for conflicts in the driver.

/**
 * This $(D AstVisitor) is used to determine which branch of a
 * VersionBlockDeclaration or of a VersionBlockStatement is valid.
 * After the visit, the unreachable branches are supressed.
 */
final class VersionEvaluatorVisitor: AstVisitor
{
    alias visit = AstVisitor.visit;

private:

    bool _supressVersion;
    string[] _userVersions;
    static immutable bool[string] _predefinedVersions;

    static bool isPredefinedVersion(const(char)[] value)
    {
        return (value in _predefinedVersions) !is null;
    }

    static bool isPredefinedVersionSet(const(char)[] value)
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

    void pullVersionDependantStuff(DeclarationsOrStatementsAstNode doss)
    {
        if (doss) while (true)
        {
            import std.algorithm.searching : countUntil;
            ptrdiff_t p = doss.items.countUntil!(a => a.statement !is null
                && a.statement.statementKind == StatementKind.skVersion);
            if (p != -1)
            {
                VersionBlockStatementAstNode v = doss.items[p].statement.statement.versionBlockStatement;
                DeclarationOrStatementAstNode[] m = v.trueDeclarationsOrStatements
                    ? v.trueDeclarationsOrStatements.items : v.falseDeclarationsOrStatements
                    ? v.falseDeclarationsOrStatements.items : null;
                DeclarationOrStatementAstNode[] r;
                if (p + 1 < doss.items.length)
                    r = doss.items[p + 1 .. $].dup;
                doss.items = doss.items[0..p] ~ m ~ r;
            }
            else break;
        }
    }

    void pullVersionDependantStuff(DeclarationsAstNode ds)
    {
        if (ds) while (true)
        {
            import std.algorithm.searching : countUntil;
            ptrdiff_t p = ds.items.countUntil!(a => a.declarationKind == DeclarationKind.dkVersion);
            if (p != -1)
            {
                VersionBlockDeclarationAstNode v = ds.items[p].declaration.versionBlockDeclaration;
                DeclarationAstNode[] m = v.trueDeclarations
                    ? v.trueDeclarations.items : v.falseDeclarations
                    ? v.falseDeclarations.items : null;
                DeclarationAstNode[] r;
                if (p + 1 < ds.items.length)
                    r = ds.items[p + 1 .. $].dup;
                ds.items = ds.items[0..p] ~ m ~ r;
            }
            else break;
        }
    }

public:

    static this()
    {
        version(unittest)
        {
            _predefinedVersions["valid_OS"] = true;
            _predefinedVersions["invalid_OS"] = false;
        }
        _predefinedVersions["all"] = true;
        _predefinedVersions["none"] = false;
    }

    /// Constructs an instance with a list of user defined version.
    this(string[] userVersions, bool supressVersion = false)
    {
        _userVersions = userVersions;
        _supressVersion = supressVersion;
    }

    /// Constructs an instance with an AST and a list of user defined version.
    this(UnitAstNode u, string[] userVersions, bool supressVersion = false)
    {
        _userVersions = userVersions;
        _supressVersion = supressVersion;
        visit(u);
    }

    override void visit(VersionBlockDeclarationAstNode node)
    {
        visit(node.versionExpression);
        node.isTrue = evaluate();

        DeclarationsAstNode valids;

        if (node.isTrue)
        {
            valids = node.trueDeclarations;
            assert(valids !is null);
            destroy(node.falseDeclarations);
            node.falseDeclarations = null;
        }
        else
        {
            valids = node.falseDeclarations;
            destroy(node.trueDeclarations);
            node.trueDeclarations = null;
        }

        if (valids)
        {
            if (_supressVersion)
                pullVersionDependantStuff(valids);
            visit(valids);
        }
    }

    override void visit(BlockStatementAstNode node)
    {
        node.accept(this);
        if (_supressVersion)
            pullVersionDependantStuff(node.declarationsOrStatements);
    }

    override void visit(FunctionDeclarationAstNode node)
    {
        node.accept(this);
        if (_supressVersion)
            pullVersionDependantStuff(node.declarationsOrStatements);
    }

    override void visit(ClassDeclarationAstNode node)
    {
        node.accept(this);
        if (_supressVersion)
            pullVersionDependantStuff(node.declarations);
    }

    override void visit(InterfaceDeclarationAstNode node)
    {
        node.accept(this);
        if (_supressVersion)
            pullVersionDependantStuff(node.declarations);
    }

    override void visit(StructDeclarationAstNode node)
    {
        node.accept(this);
        if (_supressVersion)
            pullVersionDependantStuff(node.declarations);
    }

    override void visit(UnionDeclarationAstNode node)
    {
        node.accept(this);
        if (_supressVersion)
            pullVersionDependantStuff(node.declarations);
    }

    override void visit(UnitAstNode node)
    {
        node.accept(this);
        if (_supressVersion)
            pullVersionDependantStuff(node.declarations);
    }

    override void visit(VersionBlockStatementAstNode node)
    {
        visit(node.versionExpression);
        node.isTrue = evaluate();

        DeclarationsOrStatementsAstNode valids;

        if (node.isTrue)
        {
            valids = node.trueDeclarationsOrStatements;
            destroy(node.falseDeclarationsOrStatements);
            node.falseDeclarationsOrStatements = null;
        }
        else
        {
            valids = node.falseDeclarationsOrStatements;
            destroy(node.trueDeclarationsOrStatements);
            node.trueDeclarationsOrStatements = null;
        }

        if (valids)
        {
            if (_supressVersion)
                pullVersionDependantStuff(valids);
            visit(valids);
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
        if (node.not)
        {
            _boolStack[$-1] = !_boolStack[$-1];
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
void assertFirstVersionIsTrue(const(char)[] code, string[] userVersions = [],
    string file = __FILE_FULL_PATH__, size_t line = __LINE__)
{
    import core.exception: AssertError;
    import styx.utils;

    UnitAstNode u = lexAndParse(code, file, line);

    if (u is null || !u.declarations ||
        !u.declarations.items.length ||
        !u.declarations.items[0].declaration.versionBlockDeclaration)
    {
        throw new AssertError("the code to test is invalid", file, line);
    }
    VersionEvaluatorVisitor vev = new VersionEvaluatorVisitor(userVersions);

    vev.visit(u);
    const VersionBlockDeclarationAstNode vb = u.declarations.items[0]
        .declaration.versionBlockDeclaration;

    if (!vb.isTrue)
    {
        throw new AssertError("the version evaluates to false instead of true",
            file, vb.startPos.line);
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
void assertFirstVersionIsFalse(const(char)[] code, string[] userVersions = [],
    string file = __FILE_FULL_PATH__, size_t line = __LINE__)
{
    import core.exception: AssertError;
    import styx.utils;

    UnitAstNode u = lexAndParse(code, file, line);

    if (u is null || !u.declarations ||
        !u.declarations.items.length ||
        !u.declarations.items[0].declaration.versionBlockDeclaration)
    {
        throw new AssertError("the code to test is invalid", file, line);
    }
    new VersionEvaluatorVisitor(u, userVersions);

    const VersionBlockDeclarationAstNode vb = u.declarations.items[0]
        .declaration.versionBlockDeclaration;

    if (vb.isTrue)
    {
        throw new AssertError("the version evaluates to true instead of false",
            file, vb.startPos.line);
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

unittest // predefined versions
{
    assertFirstVersionIsTrue(q{
        unit a;
        version(valid_OS) const s32 b;
    });
    assertFirstVersionIsFalse(q{
        unit a;
        version(invalid_OS) const s32 b;
    });
}

unittest // not
{
    assertFirstVersionIsTrue(q{
        unit a;
        version(!a) const s32 b;
    }, ["b"]);
    assertFirstVersionIsFalse(q{
        unit a;
        version(!a) const s32 b;
    }, ["a"]);
    assertFirstVersionIsTrue(q{
        unit a;
        version(!(a)) const s32 b;
    }, ["b"]);
    assertFirstVersionIsFalse(q{
        unit a;
        version(!(a)) const s32 b;
    }, ["a"]);
    assertFirstVersionIsTrue(q{
        unit a;
        version(!(a & b)) const s32 b;
    }, ["a", "c"]);
    assertFirstVersionIsFalse(q{
        unit a;
        version(a & !b) const s32 b;
    }, ["a", "b"]);
    assertFirstVersionIsFalse(q{
        unit a;
        version(!a & !b) const s32 b;
    }, ["a", "b"]);
    assertFirstVersionIsTrue(q{
        unit a;
        version(!a | b) const s32 b;
    }, ["a", "b"]);
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
    assertFirstVersionIsFalse(q{
        unit a;
        version(none)
        {
            import importable.x;
        }
    }, []);
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

unittest
{
    import styx.utils;
    import std.stdio;

    class Tester : AstVisitor
    {
        alias visit = AstVisitor.visit;

        string source;
        string[] versions;
        size_t count;

        void test(){}

        this()
        {
            UnitAstNode u = lexAndParse(source);
            VersionEvaluatorVisitor v = new VersionEvaluatorVisitor(versions, true);
            v.visit(u);
            visit(u);
            test();
            version(none)
            {
                import styx.ast_formatter;
                AstFormatter af = new AstFormatter;
                af.visit(u);
                writeln(af.formattedAst);
            }
        }
    }

    class Test1 : Tester
    {
        alias visit = AstVisitor.visit;

        override void visit(FunctionDeclarationAstNode node)
        {
            node.accept(this);
            count = node.declarationsOrStatements.items.length;
        }

        override void test(){assert(count == 5);}

        this()
        {
            versions = ["a"];
            source = `unit a;
            function foo()
            {
                const int a1;
                version (a) {const int a2; const int a3; const int a4;}
                else {const int a5;}
                version (a) const int a6;
            }`;
            super();
        }
    }

    class Test2 : Tester
    {
        alias visit = AstVisitor.visit;

        override void visit(UnitAstNode node)
        {
            node.accept(this);
            count = node.declarations.items.length;
        }

        override void test(){assert(count == 5);}

        this()
        {
            versions = ["a", "b"];
            source = `unit a;
            const int a1;
            version (a)
            {const int a2; const int a3; const int a4;}
            else
            {const int a5;}
            version (b) const int a6;`;
            super();
        }
    }

    class Test3 : Tester
    {
        alias visit = AstVisitor.visit;

        override void visit(UnitAstNode node)
        {
            node.accept(this);
            count = node.declarations.items.length;
        }

        override void test(){assert(count == 4);}

        this()
        {
            versions = ["a"];
            source = `unit a;
            const int b1;
            version (a)
                version (a)
                    version (a)
                        {const int b2; const int b3; const int b4;}`;
            super();
        }
    }

    class Test4 : Tester
    {
        alias visit = AstVisitor.visit;

        override void visit(StructDeclarationAstNode node)
        {
            node.accept(this);
            count = node.declarations.items.length;
        }

        override void test(){assert(count == 4);}

        this()
        {
            versions = ["a"];
            source = `unit a;
            struct Foo
            {
            const int b1;
            version (a)
                version (a)
                    version (a)
                        {const int b2; const int b3; const int b4;}
            }`;
            super();
        }
    }

    class Test5 : Tester
    {
        alias visit = AstVisitor.visit;

        override void visit(ClassDeclarationAstNode node)
        {
            node.accept(this);
            count = node.declarations.items.length;
        }

        override void test(){assert(count == 4);}

        this()
        {
            versions = ["a"];
            source = `unit a;
            class Foo
            {
            const int b1;
            version (a)
                version (a)
                    version (a)
                        {const int b2; const int b3; const int b4;}
            }`;
            super();
        }
    }

    class Test6 : Tester
    {
        alias visit = AstVisitor.visit;

        override void visit(UnionDeclarationAstNode node)
        {
            node.accept(this);
            count = node.declarations.items.length;
        }

        override void test(){assert(count == 4);}

        this()
        {
            versions = ["a"];
            source = `unit a;
            union Foo
            {
            const int b1;
            version (a)
                version (a)
                    version (a)
                        {const int b2; const int b3; const int b4;}
            }`;
            super();
        }
    }

    class Test7 : Tester
    {
        alias visit = AstVisitor.visit;

        override void visit(InterfaceDeclarationAstNode node)
        {
            node.accept(this);
            count = node.declarations.items.length;
        }

        override void test(){assert(count == 4);}

        this()
        {
            versions = ["a"];
            source = `unit a;
            interface Foo
            {
            const int b1;
            version (a)
                version (a)
                    version (a)
                        {const int b2; const int b3; const int b4;}
            }`;
            super();
        }
    }

    class Test8 : Tester
    {
        alias visit = AstVisitor.visit;

        override void visit(BlockStatementAstNode node)
        {
            node.accept(this);
            count = node.declarationsOrStatements.items.length;
        }

        override void test(){assert(count == 5);}

        this()
        {
            versions = ["a"];
            source = `unit a;
            function foo()
            {
                {
                    const int a1;
                    version (a) {const int a2; const int a3; const int a4;}
                    else {const int a5;}
                    version (a) const int a6;
                }
            }`;
            super();
        }
    }

    const Test1 c1 = new Test1;
    const Test2 c2 = new Test2;
    const Test3 c3 = new Test3;
    const Test4 c4 = new Test4;
    const Test5 c5 = new Test5;
    const Test6 c6 = new Test6;
    const Test7 c7 = new Test7;
    const Test8 c8 = new Test8;
}

