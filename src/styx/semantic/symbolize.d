module styx.semantic.symbolize;

import
    std.stdio;
import
    styx.token, styx.lexer, styx.parser,
    styx.ast, styx.symbol, styx.utils, styx.session;

/**
 * This AST visitor creates the symbols and their scope, even partial.
 */
final class AstSymbolizerA: AstVisitor
{

    alias visit = AstVisitor.visit;

private:

    Symbol  _currUnt;
    Symbol  _currSmb;
    Symbol  _crrScop;
    bool    _inType;
    Lexer*  _lexer;

    void checkSameSymbolName(N : AstNode)(N node, SymbolKind kind)
    {
        if (kind == SymbolKind.function_)
            return;

        Symbol[] s = _currSmb.find(node.name);
        if (s.length && s[0].parent is _currSmb)
        {
            session.error(_lexer.filename, node.startPos,
                "symbol `%s` already declared line %d",
                node.name.text, s[0].astNode.startPos.line);
        }
    }

    void addNamedSymbol(N : AstNode)(N node, SymbolKind kind)
    {
        Symbol oldSmb = _currSmb;
        checkSameSymbolName(node, kind);
        _currSmb = new Symbol(node.name, oldSmb, kind);
        _currSmb.astNode = node;
        _currSmb.unit = _currUnt;
        node.symbol = _currSmb;
        node.accept(this);
        _currSmb = oldSmb;
    }

    void addNamedType(N : AstNode)(N node, SymbolKind kind)
    {
        Symbol oldSmb = _currSmb;
        checkSameSymbolName(node, kind);
        _currSmb = new Type(node.name, oldSmb, kind);
        _currSmb.astNode = node;
        _currSmb.unit = _currUnt;
        node.symbol = _currSmb;
        node.accept(this);
        _currSmb = oldSmb;
    }

public:

    this(UnitAstNode u, Lexer* lexer)
    {
        initialize();
        _lexer = lexer;
        _currSmb = root;
        visit(u);
    }

    override void visit(AkaDeclarationAstNode node)
    {
        addNamedSymbol(node, SymbolKind.aka);
    }

    override void visit(BlockStatementAstNode node)
    {
        Symbol old = _currSmb;
        _currSmb = Symbol.newInternal(_currSmb, SymbolKind.unamed);
        node.accept(this);
        _currSmb = old;
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
        node.accept(this);
        foreach(tk; node.variableList)
        {
            Symbol p = new Symbol(tk, _currSmb, SymbolKind.variable);
        }
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
        Symbol old = _currSmb;
        node.accept(this);
        foreach(tk; node.parameters)
        {
            Symbol p = new Symbol(tk, old, SymbolKind.partial);
        }
        _currSmb = old;
    }

    override void visit(TypeAstNode node)
    {
        Symbol old = _currSmb;
        const bool oldinType = _inType;
        _inType = true;

        if (node.autoOrBasicType)
        {
            if (node.autoOrBasicType.isTokAuto)
            {
                _currSmb = new Symbol(node.autoOrBasicType, old, SymbolKind.partial);
                node.symbol = _currSmb;
            }
            else
            {
                switch(node.autoOrBasicType.type)
                {
                    case TokenType.bool_: node.symbol = bool_; break;

                    case TokenType.u8:  node.symbol  = u8; break;
                    case TokenType.u16: node.symbol = u16; break;
                    case TokenType.u32: node.symbol = u32; break;
                    case TokenType.u64: node.symbol = u64; break;

                    case TokenType.s8:  node.symbol  = s8; break;
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
        _currSmb = old;
    }

    override void visit(UnionDeclarationAstNode node)
    {
        addNamedType(node, SymbolKind.union_);
    }

    override void visit(UnitAstNode node)
    {
        _currSmb = root;
        foreach(i; 0..node.identifiers.length)
        {
            Symbol[] c = _currSmb.find(node.identifiers[i], SymbolKind.unit);
            if (c.length == 1)
            {
                if (i == node.identifiers.length-1 &&
                    _currSmb is root && node.identifiers.length == 1)
                {
                    session.error(_lexer.filename, node.startPos,
                        "a unit named `%s` is already parsed", tokenChainText(node.identifiers));
                }
                _currSmb = c[0];
            }
            else
            {
                _currSmb = new Symbol(node.identifiers[i], _currSmb, SymbolKind.unit);
                _currUnt = _currSmb;
            }
        }
        node.symbol = _currSmb;
        node.accept(this);
    }

    override void visit(VariableDeclarationItemAstNode node)
    {
        addNamedSymbol(node, SymbolKind.variable);
    }
}

UnitAstNode runAstSymbolizerAForTest(string source, int line = __LINE__)
{
    if (root)
        root.clear;
    size_t old = session.errorsCount;

    Lexer lx;
    lx.setSourceFromText(source, "stdin", line);
    lx.lex();
    UnitAstNode u = Parser(&lx).parse();
    assert(u !is null && session.errorsCount == old, "the code to test is invalid");
    new AstSymbolizerA(u, &lx);
    return u;
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
    enum s = q{
        unit u;
        aka s8_alt = s8;
        aka u8_alt = u8;
        aka s16_alt = s16;
        aka u16_alt = u16;
        aka s32_alt = s32;
        aka u32_alt = u32;
        aka s64_alt = s64;
        aka u64_alt = u64;
        aka f32_alt = f32;
        aka f64_alt = f64;
        aka bool_alt = bool;
    };
    runAstSymbolizerAForTest(s);
    assert((cast(AkaDeclarationAstNode) root
        .findQualified("u.s8_alt", [SymbolKind.unit, SymbolKind.aka])[0].astNode).type.symbol is s8);
    assert((cast(AkaDeclarationAstNode) root
        .findQualified("u.u8_alt", [SymbolKind.unit, SymbolKind.aka])[0].astNode).type.symbol is u8);
    assert((cast(AkaDeclarationAstNode) root
        .findQualified("u.s16_alt", [SymbolKind.unit, SymbolKind.aka])[0].astNode).type.symbol is s16);
    assert((cast(AkaDeclarationAstNode) root
        .findQualified("u.u16_alt", [SymbolKind.unit, SymbolKind.aka])[0].astNode).type.symbol is u16);
    assert((cast(AkaDeclarationAstNode) root
        .findQualified("u.s32_alt", [SymbolKind.unit, SymbolKind.aka])[0].astNode).type.symbol is s32);
    assert((cast(AkaDeclarationAstNode) root
        .findQualified("u.u32_alt", [SymbolKind.unit, SymbolKind.aka])[0].astNode).type.symbol is u32);
    assert((cast(AkaDeclarationAstNode) root
        .findQualified("u.s64_alt", [SymbolKind.unit, SymbolKind.aka])[0].astNode).type.symbol is s64);
    assert((cast(AkaDeclarationAstNode) root
        .findQualified("u.u64_alt", [SymbolKind.unit, SymbolKind.aka])[0].astNode).type.symbol is u64);
    assert((cast(AkaDeclarationAstNode) root
        .findQualified("u.f32_alt", [SymbolKind.unit, SymbolKind.aka])[0].astNode).type.symbol is f32);
    assert((cast(AkaDeclarationAstNode) root
        .findQualified("u.f64_alt", [SymbolKind.unit, SymbolKind.aka])[0].astNode).type.symbol is f64);
    assert((cast(AkaDeclarationAstNode) root
        .findQualified("u.bool_alt", [SymbolKind.unit, SymbolKind.aka])[0].astNode).type.symbol is bool_);
}

unittest
{
    enum s = q{  unit u; function func(){var auto v = 0;}  };
    runAstSymbolizerAForTest(s);
    with(SymbolKind) assert(root.findQualified("u.func.v", [unit, function_, variable]));
    with(SymbolKind) assert(findFullyQualified("u.func.v", [unit, function_, variable]));
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
    with(SymbolKind) assert(root.findQualified("u.z", [unit, aka]).length);
}

unittest
{
    enum s = q{  unit u; aka z = s8;  };
    runAstSymbolizerAForTest(s);
    with(SymbolKind) assert(!root.findQualified("u.s8", [unit, aka]).length);
}

unittest
{
    enum s = q{  unit u; function foo(const s8 a,b){}  };
    runAstSymbolizerAForTest(s);
    with(SymbolKind) assert(root.findQualified("u.foo.a", [unit, function_, variable]).length);
    with(SymbolKind) assert(root.findQualified("u.foo.b", [unit, function_, variable]).length);
}

unittest
{
    enum s = q{  unit u; enum E:bool {a,b}  };
    runAstSymbolizerAForTest(s);
    with(SymbolKind)
    {
        Symbol sy = root.findQualified("u.E", [unit, enum_])[0];
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
    Symbol sym = root.findQualified("u.z", [SymbolKind.unit, SymbolKind.aka])[0];
    assert(sym);
    sym.clear;
    assert(sym.children.length == 0);
}

unittest
{
    enum s = q{  unit u; aka z = s8; const u8 z;  };
    auto e = session.errorsCount;
    runAstSymbolizerAForTest(s);
    assert(session.errorsCount == e + 1);
}

unittest
{
    enum s = q{  unit u; function foo(var s32 p0); function foo(); };
    auto e = session.errorsCount;
    runAstSymbolizerAForTest(s);
    assert(session.errorsCount == e);
}

unittest
{
    enum s = q{  unit u; function foo(){ {const int a;} {const int a;}} };
    auto e = session.errorsCount;
    runAstSymbolizerAForTest(s);
    assert(session.errorsCount == e);
}

unittest
{
    enum s = q{  unit u; class Foo{ struct Foo{} }  };
    auto e = session.errorsCount;
    runAstSymbolizerAForTest(s);
    assert(session.errorsCount == e);
}

