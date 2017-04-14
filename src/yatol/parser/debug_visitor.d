module yatol.parser.debug_visitor;

import
    std.stdio, std.traits;
import
    yatol.lexer.types, yatol.parser.ast;


class DebugVisitor: AstVisitor
{

    alias visit = AstVisitor.visit;

private:

    ptrdiff_t _indentLevel;

    void visitImpl(Node)(Node node)
    {
        if (!node.isGrammatic)
        {
            node.accept(this);
        }
        else
        {
            _text ~= _indentText ~ node.classinfo.name ~ "\n";
            const bool hasChildren = !node.isTerminal;
            if (hasChildren)
            {
                _text ~= _indentText ~ "{\n";
                indent();
            }
            node.accept(this);
            if (hasChildren)
            {
                outdent();
                _text ~= _indentText ~ "}\n";
            }
        }
    }

    void indent()
    {
        ++_indentLevel;
        _indentText.length = _indentLevel * 4;
        _indentText[] = ' ';
    }

    void outdent()
    {
        --_indentLevel;
        _indentText.length = _indentLevel * 4;
    }

    invariant{assert(_indentLevel > -1);}
    char[] _text;
    char[] _indentText;

public:

    /// Creates an instance and start to visit from node.
    this(UnitContainerAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    void printText()
    {
        writeln(_text);
    }

    override void visit(ClassDeclarationAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(ImportDeclarationAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(DeclarationAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(LiteralAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(ProtectionAttributeAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(ProtectionOverwriteAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(StructDeclarationAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(UnitAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(UnitContainerAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(ScopeAstNode node)
    {
        assert(node);
        visitImpl(node);
    }
}

