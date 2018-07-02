module styx.semantic.symbolize;

import
    std.stdio;
import
    styx.token,
    styx.ast, styx.symbol, styx.utils, styx.session;

/**
 * This AST visitor creates the symbols and their scope, even partial.
 */
final class AstSymbolizerA: AstVisitor
{

    alias visit = AstVisitor.visit;

private:

    Symbol _currSmb;
    Scope  _currScp;
    bool _inType;

    void addNamedSymbolInScope(N : AstNode)(N node, SymbolKind kind)
    {
        Symbol oldSmb = _currSmb;

        node.scope_ = _currScp;
        _currSmb = new Symbol(node.name, oldSmb, kind);
        node.symbol = _currSmb;
        _currScp = _currScp.advance();
        _currScp.insertBack(_currSmb);
        node.accept(this);
        _currSmb = oldSmb;
    }

    void addNamedSymbolInNewScope(N : AstNode)(N node, SymbolKind kind)
    {
        Symbol oldSmb = _currSmb;

        node.scope_ = _currScp;
        _currSmb = new Symbol(node.name, oldSmb, kind);
        node.symbol = _currSmb;
        _currScp = _currScp = _currScp.push(node.startPos, node.stopPos);
        _currScp.insertBack(_currSmb);
        node.accept(this);
        _currSmb = oldSmb;
        _currScp = _currScp.pop();
    }

    void addNamedType(N : AstNode)(N node, SymbolKind kind)
    {
        Symbol oldSmb = _currSmb;

        node.scope_ = _currScp;
        _currSmb = new Type(node.name, oldSmb, kind);
        node.symbol = _currSmb;
        _currScp = _currScp = _currScp.push(node.startPos, node.stopPos);
        _currScp.insertBack(_currSmb);
        node.accept(this);
        _currSmb = oldSmb;
        _currScp = _currScp.pop();
    }

public:

    this(UnitAstNode u)
    {
        initialize();
        _currSmb = root;
        visit(u);
    }

    override void visit(AkaDeclarationAstNode node)
    {
        addNamedSymbolInScope(node, SymbolKind.aka);
    }

    override void visit(BlockStatementAstNode node)
    {
        node.accept(this);
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
        addNamedSymbolInScope(node, SymbolKind.variable);
    }

    override void visit(FunctionDeclarationAstNode node)
    {

        addNamedSymbolInNewScope(node, SymbolKind.function_);
    }

    override void visit(FunctionParameterGroupAstNode node)
    {
        node.accept(this);
        foreach(tk; node.variableList)
        {
            new Symbol(tk, _currSmb, SymbolKind.variable);
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
        addNamedSymbolInNewScope(node, SymbolKind.template_);
    }

    override void visit(TemplateParametersAstNode node)
    {
        Symbol old = _currSmb;
        foreach(tk; node.parameters)
        {
            _currSmb = new Symbol(tk, old, SymbolKind.partial);
            node.accept(this);
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
        _currScp = new Scope;
        _currScp.startPos = node.startPos;
        _currScp.stopPos  = Position(int.max, 0);

        _currSmb = root;
        foreach(i; 0..node.identifiers.length)
        {
            if (Symbol c = _currSmb.find(node.identifiers[i], SymbolKind.unit))
            {
                if (i == 0 && _currSmb is root && node.identifiers.length == 1)
                {
                    session.error(__FILE_FULL_PATH__, Position(__LINE__, 0), "INTERNAL");
                }
                _currSmb = c;
            }
            else
            {
                _currSmb = new Symbol(node.identifiers[i], _currSmb, SymbolKind.unit);
            }
        }
        node.symbol = _currSmb;
        node.accept(this);
        _currSmb = root;
    }

    override void visit(VariableDeclarationItemAstNode node)
    {
        addNamedSymbolInScope(node, SymbolKind.variable);
    }
}

UnitAstNode runAstSymbolizerAForTest(string source, int line = __LINE__)
{
    if (root)
        root.clear;
    size_t old = session.errorsCount;
    UnitAstNode u = lexAndParse(source, __FILE_FULL_PATH__, line);
    assert(u !is null && session.errorsCount == old, "the code to test is invalid");
    new AstSymbolizerA(u);
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

unittest
{
    enum s = q{
        unit u;
        function f1()
        {
            const int a;
            function f2();
            const int b;
            function f3();
        }
    };
    UnitAstNode u = runAstSymbolizerAForTest(s);

    {
        AstNode node = u.findDeclaration("f1");
        assert(node);
        assert(node.scope_);
        assert(node.scope_.symbols.length == 0);
    }
    {
        AstNode node = u.findDeclaration("f1.a");
        assert(node);
        assert(node.scope_);
        assert(node.scope_.symbols.length == 1);
        with(SymbolKind) assert(node.scope_.symbols[0] is
            root.findQualified("u.f1", [unit, function_]));
    }
    {
        AstNode node = u.findDeclaration("f1.f2");
        assert(node);
        assert(node.scope_);
        assert(node.scope_.symbols.length == 2);
        with(SymbolKind) assert(node.scope_.symbols[0] is
            root.findQualified("u.f1", [unit, function_]));
        with(SymbolKind) assert(node.scope_.symbols[1] is
            root.findQualified("u.f1.a", [unit, function_, variable]));
    }
}

