module yatol.parser.debug_visitor;

import
    std.stdio, std.traits, std.format, std.algorithm;
import
    yatol.lexer.types, yatol.lexer, yatol.parser.ast;


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
            _text ~= _indentText ~ Node.stringof ~ "\n";
            const bool hasChildren = !node.isTerminal;
            if (hasChildren)
            {
                _text ~= _indentText ~ "{\n";
                indent();
            }
            static immutable specifier1 = "%s- %s : `%s` \n";
            static immutable specifier2 = "%s- %s[%d] : `%s` \n";
            static immutable specifier3 = "%s- %s : `%d` \n";
            foreach (member; __traits(allMembers, Node))
            {
                static if (member.among("__monitor", "__ctor", "__vtbl", "Monitor"))
                    continue;
                else static if (is(typeof((){auto a = __traits(getMember, node, member);}))
                    && !hasUDA!(__traits(getMember, Node, member), Semantic))
                {
                    alias NT = typeof(__traits(getMember, node, member));
                    static if (is(NT == Token*))
                    {
                        _text ~= specifier1.format(_indentText, member,
                            __traits(getMember, node, member).text());
                    }
                    else static if (is(NT == Token*[]))
                        foreach(i, t; __traits(getMember, node, member))
                    {
                        _text ~= specifier2.format(_indentText, member, i, t.text);
                    }
                    else static if (is(NT == Token*[][]))
                        foreach(i, t; __traits(getMember, node, member))
                    {
                        _text ~= specifier2.format(_indentText, member, i, t.tokenPointerArrayText);
                    }
                    else static if (is(NT == enum) || isSomeString!NT || is(NT == bool))
                    {
                        _text ~= specifier1.format(_indentText, member,
                            __traits(getMember, node, member));
                    }
                    else static if (isIntegral!NT)
                    {
                        _text ~= specifier3.format(_indentText, member,
                            __traits(getMember, node, member));
                    }
                }
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
        if (_indentLevel < 0)
            _indentLevel = 0;
        _indentText.length = _indentLevel * 4;
    }

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

    override void visit(ProtectionDeclarationAstNode node)
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

    override void visit(ScopeDeclarationAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(TypeAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(TypeModifierAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(TypedVariableListAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(FunctionHeaderAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(FunctionDeclarationAstNode node)
    {
        assert(node);
        visitImpl(node);
    }

    override void visit(FunctionTypeAstNode node)
    {
        assert(node);
        visitImpl(node);
    }
}

