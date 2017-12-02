module yatol.symbol;

import
    std.stdio;
import
    yatol.ast, yatol.token, yatol.session;

/// Enumerates the possible symbol kinds.
enum SymbolKind
{
    aka,
    array,
    builtin,
    class_,
    enum_,
    interface_,
    import_,
    function_,
    functionPtr_,
    struct_,
    template_,
    union_,
    unit,
    variable,
}

final class Symbol
{
    Token* name;
    SymbolKind kind;
    Symbol type;
    Symbol parent;
    Symbol[] children;

    this(Token* name, Symbol parent, SymbolKind kind, Symbol type = null)
    {
        this.name = name;
        this.parent = parent;
        this.kind = kind;
        this.type = type;
        if (parent)
        {
            parent.children ~= this;
        }
    }

    Symbol find(Token* name, SymbolKind kind)
    {
        Symbol result;
        foreach(c; children)
            if (c.kind == kind && c.name && c.name.text == name.text)
        {
            result = c;
            break;
        }
        return result;
    }
}

Symbol root;
Symbol s8, s16, s32, s64, sreg;
Symbol u8, u16, u32, u64, ureg;
Symbol f32, f64;
Symbol bool_;

static this()
{
    root = new Symbol(null, null, SymbolKind.builtin);

    bool_ = new Symbol(null, root, SymbolKind.builtin);
    s8  = new Symbol(null, root, SymbolKind.builtin);
    s16 = new Symbol(null, root, SymbolKind.builtin);
    s32 = new Symbol(null, root, SymbolKind.builtin);
    s64 = new Symbol(null, root, SymbolKind.builtin);
    u8  = new Symbol(null, root, SymbolKind.builtin);
    u16 = new Symbol(null, root, SymbolKind.builtin);
    u32 = new Symbol(null, root, SymbolKind.builtin);
    u64 = new Symbol(null, root, SymbolKind.builtin);
    f32 = new Symbol(null, root, SymbolKind.builtin);
    f64 = new Symbol(null, root, SymbolKind.builtin);

    if (session.regSize == 64)
    {
        sreg = s64;
        ureg = u64;
    }
    else if (session.regSize == 32)
    {
        sreg = s32;
        ureg = u32;
    }
    else assert(0);
}

unittest
{
    assert(u8 != u16);
    assert(s8 != u8);
    assert(s16 != u16);
    assert(s16 == s16);
}

final class AstSymbolizerA: AstVisitor
{

    alias visit = AstVisitor.visit;

private:

    Symbol _parent;

    void visitNamedNode(N : AstNode)(N node, SymbolKind kind)
    {
        Symbol old = _parent;
        _parent = new Symbol(node.name, _parent, kind);
        node.symbol = _parent;
        node.accept(this);
        _parent = old;
    }

public:

    this(UnitContainerAstNode uc)
    {
        _parent = root;
        visit(uc);
    }

    override void visit(AkaDeclarationAstNode node)
    {
        if (node.type && node.type.symbol)
        {
            //Symbol aka = new Symbol(node.name, _parent, SymbolKind.aka);
            //aka.type = cast(Symbol) node.type.symbol;
            //aka.registerAsQualified();
        }
    }

    override void visit(ClassDeclarationAstNode node)
    {
        visitNamedNode(node, SymbolKind.class_);
    }

    override void visit(EnumDeclarationAstNode node)
    {
        visitNamedNode(node, SymbolKind.enum_);
    }

    override void visit(InterfaceDeclarationAstNode node)
    {
        visitNamedNode(node, SymbolKind.interface_);
    }

    override void visit(StructDeclarationAstNode node)
    {
        visitNamedNode(node, SymbolKind.struct_);
    }

    override void visit(TemplateDeclarationAstNode node)
    {
        visitNamedNode(node, SymbolKind.template_);
    }

    override void visit(UnionDeclarationAstNode node)
    {
        visitNamedNode(node, SymbolKind.union_);
    }

    override void visit(UnitAstNode node)
    {
        _parent = root;
        foreach(i; 0..node.identifiers.length)
        {
            if (Symbol c = _parent.find(node.identifiers[i], SymbolKind.unit))
            {
                _parent = c;
            }
            else
            {
                _parent = new Symbol(node.identifiers[i], _parent, SymbolKind.unit);
            }
        }
        node.symbol = _parent;
        node.accept(this);
        _parent = root;
    }
}

