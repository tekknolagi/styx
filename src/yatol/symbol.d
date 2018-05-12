module yatol.symbol;

import
    std.stdio;
import
    yatol.ast, yatol.token, yatol.session, yatol.utils;

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
    /// To be solved
    partial,
    struct_,
    template_,
    union_,
    unit,
    /// Also for enum members (considered as constant) and function parameters
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

    /**
     * Remove all the non system symbols.
     */
    void clear()
    {
        if (this is root)
        {
            import std.algorithm.searching: countUntil;
            ptrdiff_t i = children.countUntil!(a => a.kind == SymbolKind.unit);
            if (i != -1)
            {
                children = children[0..i];
            }
        }
        else children.length = 0;
    }

    /**
     * Finds a direct children.
     *
     * Params:
     *     name = The symbol name, either as a string or as a $(D Token*).
     *     kind = The symbol kind.
     *
     * Returns: On success the symbol, $(D null) otherwise.
     */
    Symbol find(Name)(Name name, SymbolKind kind)
    {
        static if (is(Name == Token*)) auto n = name.text;
        else alias n = name;

        Symbol result;
        foreach(c; children)
            if (c.kind == kind && c.name && c.name.text == n)
        {
            result = c;
            break;
        }
        return result;
    }

    /**
     * Finds a direct children.
     *
     * Params:
     *     name = The symbol name, either as a string or as a $(D Token*).
     *
     * Returns: On success the symbol, $(D null) otherwise.
     */
    Symbol find(Name)(Name name)
    {
        static if (is(Name == Token*)) auto n = name.text;
        else alias n = name;

        Symbol result;
        foreach(c; children)
            if (c.name && c.name.text == n)
        {
            result = c;
            break;
        }
        return result;
    }

    /**
     * Finds a qualified children.
     *
     * Params:
     *     name = The symbol name, either as a string or as $(D Token*[]).
     *     kind = The symbol kind.
     *
     * Returns: On success the symbol, $(D null) otherwise.
     */
    Symbol findQualified(QName)(QName qname, SymbolKind[] kinds)
    {
        static if (is(Name == Token*[]))
        {
            import std.algorithm.iteration: map;

            assert(kinds.length == qname.length);
            auto n = qname.map!(a => a.text);
        }
        else
        {
            import std.algorithm.iteration: splitter;
            import std.range: walkLength;

            auto n = qname.splitter(".");
            assert(kinds.length == n.save.walkLength);
        }

        import std.range: zip;

        Symbol result = this;
        foreach(s; zip(n, kinds))
        {

            if (Symbol c = result.find(s[0], s[1]))
            {
                result = c;
            }
            else
            {
                result = null;
                break;
            }
        }
        return result;
    }
}

Symbol root;
Symbol s8, s16, s32, s64, ssize;
Symbol u8, u16, u32, u64, usize;
Symbol f32, f64;
Symbol bool_;

static bool initialized;

void initialize()
{
    if (initialized)
        return;
    initialized = true;

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
        ssize = s64;
        usize = u64;
    }
    else if (session.regSize == 32)
    {
        ssize = s32;
        usize = u32;
    }
    else assert(0);
}

/**
 * This AST visitor creates symbols, among a single unit for types and variables.
 */
final class AstSymbolizerA: AstVisitor
{

    alias visit = AstVisitor.visit;

private:

    Symbol _parent;

    void visitNamedNode(N : AstNode)(N node, SymbolKind kind)
    {
        Symbol old = _parent;
        _parent = new Symbol(node.name, old, kind);
        node.symbol = _parent;
        node.accept(this);
        _parent = old;
    }

public:

    this(UnitAstNode u)
    {
        version(unittest)
            initialize();
        _parent = root;
        visit(u);
    }

    override void visit(AkaDeclarationAstNode node)
    {
        visitNamedNode(node, SymbolKind.aka);
    }

    override void visit(ClassDeclarationAstNode node)
    {
        visitNamedNode(node, SymbolKind.class_);
    }

    override void visit(EnumDeclarationAstNode node)
    {
        visitNamedNode(node, SymbolKind.enum_);
    }

    override void visit(EnumMemberAstNode node)
    {
        visitNamedNode(node, SymbolKind.variable);
    }

    override void visit(FunctionDeclarationAstNode node)
    {
        visitNamedNode(node, SymbolKind.function_);
    }

    override void visit(FunctionParameterGroupAstNode node)
    {
        Symbol old = _parent;
        foreach(tk; node.variableList)
        {
            _parent = new Symbol(tk, old, SymbolKind.variable);
            //note: ParamGroup is a bit inconsistent since an AST node is suposed to be assoc with a single symbol
            //node.symbol = _parent;
            node.accept(this);
        }
        _parent = old;
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

    override void visit(TemplateParametersAstNode node)
    {
        Symbol old = _parent;
        foreach(tk; node.parameters)
        {
            _parent = new Symbol(tk, old, SymbolKind.partial);
            node.accept(this);
        }
        _parent = old;
    }

    override void visit(TypeAstNode node)
    {
        //note: wait for https://github.com/BBasile/yatol/issues/28
        node.accept(this);
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
                if (i == 0 && _parent is root && node.identifiers.length == 1)
                {
                    session.error(__FILE_FULL_PATH__, Position(__LINE__, 0), "INTERNAL");
                }
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

    override void visit(VariableDeclarationItemAstNode node)
    {
        visitNamedNode(node, SymbolKind.variable);
    }
}

void runAstSymbolizerAForTest(string source, int line = __LINE__)
{
    if (root)
        root.clear;
    size_t old = session.errorsCount;
    UnitAstNode u = lexAndParse(source, __FILE_FULL_PATH__, line);
    assert(u !is null && session.errorsCount == old, "the code to test is invalid");
    new AstSymbolizerA(u);
}

unittest
{
    enum s = q{  unit u; function func(){var auto v = 0;}  };
    runAstSymbolizerAForTest(s);
    with(SymbolKind) assert(root.findQualified("u.func.v", [unit, function_, variable]));
}

unittest
{
    enum s = q{  unit u; function func<T,Z>(){}  };
    runAstSymbolizerAForTest(s);
    with(SymbolKind) assert(root.findQualified("u.func.T", [unit, function_, partial]));
    with(SymbolKind) assert(root.findQualified("u.func.Z", [unit, function_, partial]));
}

unittest
{
    enum s = q{  unit u; struct Foo{class Bar{interface Baz{}}}  };
    runAstSymbolizerAForTest(s);
    with(SymbolKind) assert(root.findQualified("u.Foo.Bar.Baz", [unit, struct_, class_, interface_]));
}

unittest
{
    enum s = q{  unit u; template Foo<>{union Bar{enum Baz{b}}}  };
    runAstSymbolizerAForTest(s);
    with(SymbolKind) assert(root.findQualified("u.Foo.Bar.Baz.b", [unit, template_, union_, enum_, variable]));
}

unittest
{
    enum s = q{  unit u; aka z = s8;  };
    runAstSymbolizerAForTest(s);
    with(SymbolKind) assert(root.findQualified("u.z", [unit, aka]));
}

unittest
{
    enum s = q{  unit u; aka z = s8;  };
    runAstSymbolizerAForTest(s);
    with(SymbolKind) assert(!root.findQualified("u.s8", [unit, aka]));
}

unittest
{
    enum s = q{  unit u; function foo(const s8 a,b){}  };
    runAstSymbolizerAForTest(s);
    with(SymbolKind) assert(root.findQualified("u.foo.a", [unit, function_, variable]));
    with(SymbolKind) assert(root.findQualified("u.foo.b", [unit, function_, variable]));
}

unittest
{
    enum s = q{  unit u; aka z = s8;  };
    runAstSymbolizerAForTest(s);
    Symbol sym = root.findQualified("u.z", [SymbolKind.unit, SymbolKind.aka]);
    assert(sym);
    sym.clear;
    assert(sym.children.length == 0);
}
