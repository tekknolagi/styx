module yatol.semantic.node_protection;

import
    std.stdio;
import
    yatol.lexer.types, yatol.lexer, yatol.parser.ast;

/**
 * An AST visitor that set the protection of each node.
 */
final class NodeProtectionVisitor: AstVisitor
{

    alias visit = AstVisitor.visit;

private:

    Lexer* _lx;
    bool _success = true;

    enum Protection
    {
        public_,
        private_,
        protected_
    }

    Protection[] _protection = [Protection.public_];

    void setFields(Node)(Node node)
    {
        with(Protection) final switch (_protection[$-1])
        {
        case public_:
            node.isPublic = true;
            break;
        case private_:
            node.isPrivate = true;
            break;
        case protected_:
            node.isProtected = true;
            break;
        }
    }

    void pushProtection(Protection value)
    {
        _protection ~= value;
    }

    void overwriteProtection(Protection value)
    {
        _protection[$-1] = value;
    }

    void popProtection()
    {
        _protection.length -= 1;
    }

public:

    /// Creates an instance and start to visit from node.
    this(UnitContainerAstNode node, Lexer* lexer)
    {
        assert(node);
        assert(lexer);
        _lx = lexer;
        pushProtection(Protection.public_);
        visit(node);
    }

    /// Returns: $(D true) if the protections have been set correctly.
    bool success() {return _success;}

    override void visit(DeclarationAstNode node)
    {
        node.accept(this);
    }

    override void visit(ProtectionDeclarationAstNode node)
    {
        with (Protection) switch (node.protection.text())
        {
            case "public":
                pushProtection(public_);
                break;
            case "private":
                pushProtection(private_);
                break;
            case "protected":
                pushProtection(protected_);
                break;
            default:
                writefln("%s(%d,%d): error, `%s` is not a valid protection, " ~
                    "expected `public, `private` or `protected`",
                    _lx.filename, node.protection.line, node.protection.column,
                    node.protection.text);
                _success = false;
        }
    }

    override void visit(ClassDeclarationAstNode node)
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

    override void visit(ImportDeclarationAstNode node)
    {
        setFields(node);
    }
}

