module styx.symbol;

import
    std.stdio;
import
    styx.ast, styx.token, styx.session, styx.utils;

/// Enumerates the possible kinds first class kinds of first class symbols
enum SymbolKind
{
    invalid_,

    aka,
    builtin,
    import_,
    function_,
    /// To be solved
    partial,
    unit,
    template_,
    /// Also for enum members (considered as constant) and function parameters
    variable,

    bool_,

    u8,
    u16,
    u32,
    u64,
    s8,
    s16,
    s32,
    s64,

    f32,
    f64,

    functionProto_,

    enum_,

    class_,
    interface_,
    struct_,
    union_,
}

class Symbol
{
    Token* name;
    SymbolKind kind;
    Symbol parent;
    Symbol[] children;

    this(Token* name, Symbol parent, SymbolKind kind)
    {
        this.name = name;
        this.parent = parent;
        this.kind = kind;
        if (parent)
        {
            parent.children ~= this;
        }
    }

    /**
     * Remove all the non system symbols.
     */
    final void clear()
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

private Symbol _root;

/**
 * Returns: The root symbol. It Contains all the units and also the default
 * internal symbols, such as the one used representing the basic types.
 */
Symbol root()
{
    if (_root is null)
        initialize();
    return _root;
}

class Type: Symbol
{
    alias typeKind = super.kind;
    Type[] baseTypes;

    this(Token* name, Symbol parent, SymbolKind kind)
    {
        super(name, parent, kind);
    }

    /// Returns: $(D true) if this type is numeric.
    final bool isNumeric(){return SymbolKind.u8 <= typeKind && typeKind <= SymbolKind.f64;}

    /// Returns: $(D true) if this type is integral.
    final bool isIntegral(){return SymbolKind.u8 <= typeKind && typeKind <= SymbolKind.s64;}

    /// Returns: $(D true) if this type is a floating point type.
    final bool isFloatingPoint(){return SymbolKind.f32 <= typeKind && typeKind <= SymbolKind.f64;}
}

Type u8, u16, u32, u64, usize;
Type s8, s16, s32, s64, ssize;
Type f32, f64;
Type bool_;

void initialize()
{
    if (_root !is null)
        return;

    _root = new Symbol(null, null, SymbolKind.builtin);

    bool_ = new Type(null, root, SymbolKind.bool_);
    u8  = new Type(null, root, SymbolKind.u8);
    u16 = new Type(null, root, SymbolKind.u16);
    u32 = new Type(null, root, SymbolKind.u32);
    u64 = new Type(null, root, SymbolKind.u64);
    s8  = new Type(null, root, SymbolKind.s8);
    s16 = new Type(null, root, SymbolKind.s16);
    s32 = new Type(null, root, SymbolKind.s32);
    s64 = new Type(null, root, SymbolKind.s64);
    f32 = new Type(null, root, SymbolKind.f32);
    f64 = new Type(null, root, SymbolKind.f64);

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
    bool _inType;

    void addNamedSymbol(N : AstNode)(N node, SymbolKind kind)
    {
        Symbol old = _parent;
        _parent = new Symbol(node.name, old, kind);
        node.symbol = _parent;
        node.accept(this);
        _parent = old;
    }

    void addNamedType(N : AstNode)(N node, SymbolKind kind)
    {
        Symbol old = _parent;
        _parent = new Type(node.name, old, kind);
        node.symbol = _parent;
        node.accept(this);
        _parent = old;
    }

public:

    this(UnitAstNode u)
    {
        initialize();
        _parent = root;
        visit(u);
    }

    override void visit(AkaDeclarationAstNode node)
    {
        addNamedSymbol(node, SymbolKind.aka);
    }

    override void visit(ClassDeclarationAstNode node)
    {
        addNamedType(node, SymbolKind.class_);
    }

    override void visit(EnumDeclarationAstNode node)
    {
        addNamedType(node, SymbolKind.enum_);
    }

    override void visit(EnumMemberAstNode node)
    {
        addNamedSymbol(node, SymbolKind.variable);
    }

    override void visit(FunctionDeclarationAstNode node)
    {
        addNamedSymbol(node, SymbolKind.function_);
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
        addNamedType(node, SymbolKind.interface_);
    }

    override void visit(StructDeclarationAstNode node)
    {
        addNamedType(node, SymbolKind.struct_);
    }

    override void visit(TemplateDeclarationAstNode node)
    {
        addNamedSymbol(node, SymbolKind.template_);
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
        Symbol old = _parent;
        const bool oldinType = _inType;
        _inType = true;

        if (node.autoOrBasicType)
        {
            if (node.autoOrBasicType.isTokAuto)
            {
                _parent = new Symbol(node.autoOrBasicType, old, SymbolKind.partial);
                node.symbol = _parent;
            }
            else
            {
                switch(node.autoOrBasicType.type)
                {
                    case TokenType.bool_: node.symbol = bool_; break;

                    case TokenType.u8: node.symbol  = u8; break;
                    case TokenType.u16: node.symbol = u16; break;
                    case TokenType.u32: node.symbol = u32; break;
                    case TokenType.u64: node.symbol = u64; break;

                    case TokenType.s8: node.symbol  = s8; break;
                    case TokenType.s16: node.symbol = s16; break;
                    case TokenType.s32: node.symbol = s32; break;
                    case TokenType.s64: node.symbol = s64; break;

                    case TokenType.f32: node.symbol = f32; break;
                    case TokenType.f64: node.symbol = f64; break;

                    default: assert(0);
                }
                if (Type t = cast(Type) old)
                    if (node.symbol)
                {
                    t.baseTypes ~= cast(Type) node.symbol;
                }
            }
        }

        _inType = oldinType;
        _parent = old;
    }

    override void visit(UnionDeclarationAstNode node)
    {
        addNamedType(node, SymbolKind.union_);
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
        addNamedSymbol(node, SymbolKind.variable);
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
    assert(root());
    assert(u8.isIntegral);
    assert(!f32.isIntegral);
    assert(f32.isFloatingPoint);
    assert(f64.isNumeric);
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
    enum s = q{  unit u; enum E:bool {a,b}  };
    runAstSymbolizerAForTest(s);
    with(SymbolKind)
    {
        Symbol sy = root.findQualified("u.E", [unit, enum_]);
        assert(sy);
        assert(sy.children.length == 2);
        assert((cast(Type)sy).baseTypes[0].kind == SymbolKind.bool_);
        assert(sy.children[0].kind == SymbolKind.variable && sy.children[0].name.text == "a");
        assert(sy.children[1].kind == SymbolKind.variable && sy.children[1].name.text == "b");
    }
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
