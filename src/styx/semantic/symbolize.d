module styx.semantic.symbolize;

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

    Symbol _parent;
    Scope _currentScope;
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
        _currentScope.symbols ~= node.symbol;
        node.accept(this);
    }

    override void visit(BlockStatementAstNode node)
    {
        _currentScope = _currentScope.push(node.position, node.end);
        node.scope_ = _currentScope;
        node.accept(this);
        _currentScope = _currentScope.pop();
    }

    override void visit(ClassDeclarationAstNode node)
    {
        _currentScope = _currentScope.push(node.position, node.end);
        node.scope_ = _currentScope;
        addNamedType(node, SymbolKind.class_);
        _currentScope = _currentScope.pop();
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
        _currentScope = _currentScope.push(node.position, node.end);
        node.scope_ = _currentScope;
        addNamedSymbol(node, SymbolKind.function_);
        _currentScope = _currentScope.pop();
    }

    override void visit(FunctionParameterGroupAstNode node)
    {
        node.accept(this);
        foreach(tk; node.variableList)
        {
            new Symbol(tk, _parent, SymbolKind.variable);
        }
    }

    override void visit(InterfaceDeclarationAstNode node)
    {
        _currentScope = _currentScope.push(node.position, node.end);
        node.scope_ = _currentScope;
        addNamedType(node, SymbolKind.interface_);
        _currentScope = _currentScope.pop();
    }

    override void visit(StructDeclarationAstNode node)
    {
        _currentScope = _currentScope.push(node.position, node.end);
        node.scope_ = _currentScope;
        addNamedType(node, SymbolKind.struct_);
        _currentScope = _currentScope.pop();
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
        _currentScope = _currentScope.push(node.position, node.end);
        node.scope_ = _currentScope;
        addNamedType(node, SymbolKind.union_);
        _currentScope = _currentScope.pop();
    }

    override void visit(UnitAstNode node)
    {
        _currentScope = new Scope;
        _currentScope.position = node.position;
        _currentScope.end = Position(int.max, 0);

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
        _currentScope.symbols ~= node.symbol;
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



