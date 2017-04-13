module yatol.parser.debug_visitor;

import
    std.stdio;
import
    yatol.lexer.types, yatol.parser.ast;

class DebugVisitor: AstVisitor
{

    alias visit = AstVisitor.visit;

private:

    ptrdiff_t _indentLevel;


    void indentLevel(ptrdiff_t value)
    {
        _indentLevel = value;
        _indentText.length = value * 4;
        _indentText[] = ' ';
    }

    ptrdiff_t indentLevel(){return _indentLevel;}

    invariant{assert(_indentLevel > -1);}
    char[] _text;
    char[] _indentText;

public:

    /// Creates an instance and start to visit from node.
    this(UnitContainerAstNode node)
    {
        assert(node);
        visit(node);
    }

    void printText()
    {
        writeln(_text);
    }

    override void visit(ClassDeclarationAstNode node)
    {
        _text ~= _indentText ~ node.classinfo.name ~ "\n";
        indentLevel(indentLevel+1);
        node.accept(this);
        indentLevel(indentLevel-1);
    }

    override void visit(ImportDeclarationAstNode node)
    {
        _text ~= _indentText ~ node.classinfo.name ~ "\n";
        node.accept(this);
    }

    override void visit(DeclarationAstNode node)
    {
        node.DeclarationAstNode.accept(this);
    }

    override void visit(LiteralAstNode node)
    {
        _text ~= _indentText ~ node.classinfo.name ~ "\n";
        node.accept(this);
    }

    override void visit(ProtectionAttributeAstNode node)
    {
        _text ~= _indentText ~ node.classinfo.name ~ " ";
        node.accept(this);
    }

    override void visit(ProtectionOverwriteAstNode node)
    {
        _text ~= _indentText ~ node.classinfo.name ~ "\n";
        indentLevel(indentLevel+1);
        node.accept(this);
        indentLevel(indentLevel-1);
    }

    override void visit(StructDeclarationAstNode node)
    {
        _text ~= _indentText ~ node.classinfo.name ~ "\n";
        indentLevel(indentLevel+1);
        node.accept(this);
        indentLevel(indentLevel-1);
    }

    override void visit(UnitAstNode node)
    {
        _text ~= _indentText ~ node.classinfo.name ~ "\n";
        indentLevel(indentLevel+1);
        node.accept(this);
        indentLevel(indentLevel-1);
    }

    override void visit(UnitContainerAstNode node)
    {
        _text ~= _indentText ~ node.classinfo.name ~ "\n";
        indentLevel(indentLevel+1);
        node.accept(this);
        indentLevel(indentLevel-1);
    }
}
