module yatol.semantic.node_protection;

import
    yatol.lexer.types, yatol.parser.ast;

/**
 * An AST visitor that set the protection of each node.
 */
class NodeProtectionVisitor: AstVisitor
{

    alias visit = AstVisitor.visit;

private:

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
    this(UnitContainerAstNode node)
    {
        assert(node);
        pushProtection(Protection.public_);
        if (node)
            visit(node);
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
        node.accept(this);
    }
}

