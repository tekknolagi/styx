module yatol.semantic.node_protection;

import
    std.stdio;
import
    yatol.token, yatol.lexer, yatol.ast, yatol.session;

/**
 * An AST visitor that sets the protection of each node.
 */
final class NodeProtectionVisitor: AstVisitor
{

    alias visit = AstVisitor.visit;

private:

    Lexer* _lx;

    enum Protection
    {
        public_,
        protected_,
        private_,
        strict,
    }

    Protection _protection = Protection.public_;
    Protection _oldProtection = Protection.public_;

    void setFields(Node)(Node node)
    {
        with(Protection) final switch (_protection)
        {
        case public_:
            node.isPublic = true;
            break;
        case protected_:
            node.isProtected = true;
            break;
        case private_:
            node.isPrivate = true;
            break;
        case strict:
            node.isStrict = true;
            break;
        }
    }

    void pushProtection(Protection value)
    {
        _oldProtection = _protection;
        _protection = value;
    }

    void overwriteProtection(Protection value)
    {
        _protection = value;
    }

    void popProtection()
    {
        _protection = _oldProtection;
    }

public:

    /// Creates an instance and start to visit from node.
    this(UnitAstNode node, Lexer* lexer)
    {
        assert(node);
        assert(lexer);
        _lx = lexer;
        pushProtection(Protection.public_);
        visit(node);
    }

    override void visit(ProtectionDeclarationAstNode node)
    {
        with (Protection) switch (node.protection.text())
        {
            case "public":
                overwriteProtection(public_);
                break;
            case "protected":
                overwriteProtection(protected_);
                break;
            case "private":
                overwriteProtection(private_);
                break;
            case "strict":
                overwriteProtection(strict);
                break;
            default:
                session.error(_lx.filename, node.protection.line, node.protection.column,
                    "`%s` is not a valid protection, expected `public, `protected`, `private` or `strict`",
                    node.protection.text);
        }
    }

    override void visit(ClassDeclarationAstNode node)
    {
        setFields(node);
        pushProtection(Protection.public_);
        node.accept(this);
        popProtection;
    }

    override void visit(InterfaceDeclarationAstNode node)
    {
        setFields(node);
        pushProtection(Protection.public_);
        node.accept(this);
        popProtection;
    }

    override void visit(StructDeclarationAstNode node)
    {
        setFields(node);
        pushProtection(Protection.public_);
        node.accept(this);
        popProtection;
    }

    override void visit(UnionDeclarationAstNode node)
    {
        setFields(node);
        pushProtection(Protection.public_);
        node.accept(this);
        popProtection;
    }

    override void visit(EnumDeclarationAstNode node)
    {
        setFields(node);
        node.accept(this);
    }

    override void visit(AkaDeclarationAstNode node)
    {
        setFields(node);
        node.accept(this);
    }

    override void visit(ImportDeclarationAstNode node)
    {
        node.accept(this);
        setFields(node);
    }

    override void visit(VariableDeclarationItemAstNode node)
    {
        node.accept(this);
        setFields(node);
    }

    override void visit(FunctionDeclarationAstNode node)
    {
        node.accept(this);
        setFields(node);
    }

    override void visit(TemplateDeclarationAstNode node)
    {
        setFields(node);
        pushProtection(Protection.public_);
        node.accept(this);
        popProtection;
    }
}

unittest
{
    class ProtectionChecker: AstVisitor
    {
        alias visit = AstVisitor.visit;

        void check(N)(N node)
        {
            import std.algorithm;
            if (node.name.text.startsWith("public"))
                assert(node.isPublic, node.name.text);
            else if (node.name.text.startsWith("private"))
                assert(node.isPrivate);
            else if (node.name.text.startsWith("protected"))
                assert(node.isProtected);
        }

        override void visit(VariableDeclarationItemAstNode node)
        {
            node.accept(this);
            check(node);
        }
    }

    enum line = __LINE__ + 1;
    enum string source = q{
        unit a;

        const s8 public1 = 8;
        const s8 public2 = 16;

        struct Foo
        {
        protection(private)

            var s8 private_member_1, private_member_2;
            aka a = b;

            interface Bar{}

        protection(public)

            var s8 public_member_1, public_member_2;

            union U{}
        }

        class Bar
        {
        protection(private)

            var s8 private_member_1, private_member_2;

        protection(protected)

            var s8 protected_member_1, protected_member_2;

            import a.b;

            enum E {e}

        protection(public)

            function public1();
        }

        protection(strict) template Baz<T>
        {
            protection(strict)
                var T strict_member1;
        }

        protection(invalid)

            function public1();
    };

    import yatol.lexer, yatol.parser, yatol.utils;

    Lexer lx;
    lx.setSourceFromText(source, __FILE_FULL_PATH__, line);
    lx.lex();
    UnitAstNode u = Parser(&lx).parse();
    const old = session.errorsCount;
    NodeProtectionVisitor np = new NodeProtectionVisitor(u, &lx);
    ProtectionChecker pc = new ProtectionChecker;
    pc.visit(u);

    assert(findDeclaration(u, "Foo.private_member_1").isPrivate);
    assert(findDeclaration(u, "Foo.private_member_2").isPrivate);
    assert(findDeclaration(u, "Foo.public_member_1").isPublic);
    assert(findDeclaration(u, "Foo.public_member_2").isPublic);
    assert(findDeclaration(u, "Bar.protected_member_1").isProtected);
    assert(findDeclaration(u, "Bar.protected_member_2").isProtected);
    assert(findDeclaration(u, "Baz.strict_member1").isStrict);

    assert(session.errorsCount == old + 1);
}

