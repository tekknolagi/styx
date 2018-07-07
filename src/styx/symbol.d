module styx.symbol;

import
    std.stdio, std.algorithm.iteration, std.array, std.traits;
import
    styx.token, styx.session, styx.ast;

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
    /// But parameters are children of the following kind
    parameterGroup,

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
    AstNode astNode;


    ///
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

    ///
    static Symbol newInternal(Symbol parent, SymbolKind kind)
    {
        return new Symbol(null, parent, kind);
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
        static if (is(QName == Token*[]))
        {
            assert(kinds.length == qname.length);
            auto n = qname.map!(a => a.text);
        }
        else static if (isSomeString!QName)
        {
            import std.range: walkLength;

            auto n = qname.splitter(".");
            assert(kinds.length == n.save.walkLength);
        }
        else static if (is(QName : string[]))
        {
            alias n = qname;
        }
        else static assert(0);

        import std.range: zip;

        Symbol result = this;
        foreach(s, t; zip(n, kinds))
        {

            if (Symbol c = result.find(s, t))
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

__gshared Type u8, u16, u32, u64, usize;
__gshared Type s8, s16, s32, s64, ssize;
__gshared Type f32, f64;
__gshared Type bool_;

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
 *
 */
class Scope
{

    Symbol[] symbols;
    Scope parent;
    Position startPos;
    Position stopPos;

    /**
     * Appends a symbol to the scope.
     */
    void insertBack(Symbol value)
    {
        symbols ~= value;
    }

    /**
     * Returns: A child scope which inherits currently known symbols.
     */
    Scope push(Position startPos, Position stopPos)
    {
        Scope result = new Scope;
        result.parent = this;
        result.symbols = symbols.dup;
        result.startPos = startPos;
        result.stopPos = stopPos;
        return result;
    }

    /**
     * Returns: A copy of the scope.
     */
    Scope advance()
    {
        Scope result = new Scope;
        result.parent = parent;
        result.symbols = symbols.dup;
        result.startPos = startPos;
        result.stopPos = stopPos;
        return result;
    }

    /**
     * Returns: The parent scope.
     */
    Scope pop()
    {
        return parent.advance();
    }

    /**
     * Finds unqualified symbols known in this scope.
     *
     * Params:
     *     name = The symbol name, either as a string or as a $(D Token*).
     *
     * Returns: On success a symbol and its overload, $(D null) otherwise.
     */
    // TODO-cFundamentals: find qualified symbol in scope
    // TODO-cFundamentals: test Scope.find with imported units
    Symbol[] find(Name)(Name name)
    {
        static if (is(Name == Token*)) auto n = name.text;
        else alias n = name;

        // this unit ...
        Symbol[] results = symbols.filter!(a => a.name.text == n).array;

        // imported units
        results ~= symbols.filter!(a => a.kind == SymbolKind.import_)
                          .map!(a => a.astNode.scope_)
                          .map!(a => a.find(name))
                          .joiner
                          .array;
        return results;
    }
}

