module ygrammar;

import pegged.grammar, pegged.tohtml;

mixin(grammar(`
Yatol:

################################################################################
# Unit

    UnitContainer < MainUnit VirtualUnits?

    MainUnit < UnitDeclaration Declarations?
    VirtualUnits < VirtualUnit*
    VirtualUnit < Virtual UnitDeclaration Declarations?

################################################################################
# Declarations

    UnitDeclaration < Unit IdentifierChain Semicolon

    Declarations    < Declaration+
    Declaration     < ImportDeclaration
                    / ProtectionDeclaration
                    / VariableDeclaration
                    / ClassDeclaration
                    / StructDeclaration
                    / EnumDeclaration
                    / FunctionDeclaration
                    / InterfaceDeclaration
                    / BlockStatement
                    / AkaDeclaration

    ProtectionDeclaration   < Prot LeftParen Identifier RightParen
    StructDeclaration       < Struct Identifier LeftCurly Declarations? RightCurly
    ClassDeclaration        < Class Identifier LeftCurly Declarations? RightCurly
    InterfaceDeclaration    < Interface Identifier LeftCurly Declarations? RightCurly

    VariableDeclaration     < ParameterStorageClass Static? Type VariableDeclarationList Semicolon

    VariableDeclarationList < VariableDeclarationItem (Comma VariableDeclarationItem)*

    VariableDeclarationItem < Identifier Initializer?

    EnumDeclaration < Enum Identifier Cast? LeftCurly EnumMember (Comma EnumMember)* RightCurly

    EnumMember    < Identifier BasicTypeInitializer?

    #The IdentifierChain in Type may represent a symbol
    AkaDeclaration  < Is Type Aka Identifier

################################################################################
# Imports declaration

    ImportDeclaration < Import ImportPriority? ImportList Semicolon
    ImportList <- IdentifierChain (Comma IdentifierChain)*
    ImportPriority <- LeftParen IntLiteral RightParen

################################################################################
# Function declaration

    FunctionDeclaration < FunctionHeader FunctionBody

    FunctionHeader < Attributes? Static? Function Identifier LeftParen FunctionParameters? RightParen Cast?

    FunctionBody < LeftCurly DeclarationOrStatements? RightCurly
                 / Semicolon

    FunctionPointerType < Attributes? Static? Function Mul LeftParen FunctionParameters? RightParen Cast?

    FunctionParameters < TypedVariableList (Semicolon TypedVariableList)

################################################################################
# Attribute

    Attributes < Attribute*
    Attribute <- At Identifier / At Keyword

################################################################################
# ParameterStorageClass

    ParameterStorageClass < Var / Const

################################################################################
# Initializer

    Initializer < BasicTypeInitializer
                / ArrayDimInitializer

    BasicTypeInitializer < Equal Expression
    ArrayDimInitializer < Equal LeftSquare ArrayInitializerElements? RightSquare

    ArrayInitializerElements < Expression (Comma UnaryExpression)*

################################################################################
# DeclarationOrStatement

    DeclarationOrStatementsBlock < DeclarationOrStatement
                                 / BlockStatement

    DeclarationOrStatements < DeclarationOrStatement+

    DeclarationOrStatement  < Declaration
                            / Statement

################################################################################
# Statements

    Statement   < EmptyStatment
                / ReturnStatement
                / ExpressionStatement
                / IfElseStatement
                / WhileStatement
                / ContinueStatement
                / BreakStatement
                / BlockStatement
#               / SwitchStatement
#               / OnStatement
#               / TryStatement
#               / OnTriedStatement
#               / FinallyStatement

    EmptyStatment < Semicolon

    ExpressionStatement < AssignExpression Semicolon

    IfElseStatement < If IfCondition DeclarationOrStatementsBlock (Else DeclarationOrStatementsBlock)?

    WhileStatement < While LeftParen IfCondition RightParen DeclarationOrStatementsBlock

    ReturnStatement < Return AssignExpression? Semicolon

    ContinueStatement < Continue AssignExpression? Semicolon

    BreakStatement < Break AtLabel? AssignExpression? Semicolon

    BlockStatement < LeftCurly DeclarationOrStatementsBlock RightCurly

    AtLabel < LeftParen Identifier RightParen

    IfCondition < LeftParen RelationalExpressions RightParen
                / LeftParen ConditionalIdentifierChain RightParen
                / LeftParen VariableDeclaration RightParen
                #/ RelationalExpression Then
                #/ IdentifierChain Then

    RelationalExpressions < RelationalExpression (RelOperator RelationalExpression)*

################################################################################
# Composites expressions

    AssignExpression    < Expression Equal AssignExpression
                        / Expression

    Expression  < BinaryExpression
                / DotExpression
                / OptionalExpression
                / UnaryExpression
                / RelationalExpression

    BinaryExpression < Expression Operator Expression

    RelationalExpression    < Expression RelOperator Expression
                            / ConditionalIdentifierChain RelOperator Expression

    DotExpression < Expression Dot Expression

    OptionalExpression  < Expression Qmark

################################################################################
# Postfixable single expression

    ParenExpression < UnaryPrefix? LeftParen Expression RightParen PostfixExpression*

    CallParameters < LeftParen Expression? (Comma Expression)* RightParen

    UnaryExpression < UnaryPrefix? IdentifierChain PostfixExpression*
                    / NumberLiteral UnarySuffix?
                    / UnaryPrefix? UnaryExpression
                    / UnaryPrefix? ParenExpression

################################################################################
# PostfixExpression

    PostfixExpression   < PlusPlus
                        / MinusMinus
                        / IndexExpression
                        / RangeExpression
                        / CallParameters
                        / Cast

    IndexExpression < LeftSquare Expression RightSquare

    RangeExpression < LeftSquare Expression DotDot Expression RightSquare

################################################################################
# Cast

    Cast <- Colon Type

################################################################################
# Type

    TypedVariableList < ParameterStorageClass* Type IdentifierList

    Type    < TypeIdentifier TypeModifiers?
            / Auto

    TypeIdentifier  < BasicType
                    / IdentifierChain
                    / FunctionPointerType

    TypeModifiers < TypeModifier TypeModifiers?

    TypeModifier < LeftRightSquares / Mul / StaticArrayDim

    StaticArrayDim < LeftSquare Expression RightSquare

################################################################################
# List, chain, etc

    ConditionalIdentifierChain < Identifier (ConditionalIdentifierChainKnot Identifier)*

    ConditionalIdentifierChainKnot < Dot / OptAccess

    IdentifierChainList < IdentifierChain (Comma IdentifierChain)*

    IdentifierChain < Identifier (Dot Identifier)*

    IdentifierList  < Identifier (Comma Identifier)*

    LeftRightSquares < LeftSquare RightSquare

################################################################################
# Comments, should be considered as part of to the spaces allowed with " < "

    Comment <~ LineComment / StarComment

    LineComment < : "//" (!Eol .)* :Eol
    StarComment < : "/*" (!"*/" .)* :"*/"

################################################################################
# Identifier and literals

    NumberLiteral   < IntLiteral
                    / HexLiteral
                    / FloatLiteral

    HexLiteral  <~ HexPrefix HexDigits+ HexLiteralSuffix?
    HexLiteralSuffix <- Colon BasicType

    FloatLiteral  <~ Minus? Num+ Dot Num+ FloatLiteralSuffix?
    FloatLiteralSuffix <- Colon BasicFloatType

    IntLiteral  <~ Minus? Num+ IntLiteralSuffix?
    IntLiteralSuffix <- Colon BasicIntegerType

    Eol <- "\r\n" / '\n'

    Identifier  <~ !Keyword (Alpha) (AlphaNum)*
                /  DollarKw

    DollarKw <~Dollar Keyword

    Operator < Mul / Div / Plus / Minus

    RelOperator < EqualEqual
                / NotEqual
                / Lesser
                / Greater
                / GreaterEqual
                / LesserEqual

    UnaryPrefix < Mul / PlusPlus / MinusMinus / Amp

    UnarySuffix < PlusPlus / MinusMinus

    OptAccess   <~ Qmark Dot
    Ellipsis    <~ Dot Dot Dot
    DotDot      <~ Dot Dot
    EqualEqual  <~ Equal Equal
    NotEqual    <~ Bang Equal
    Lesser      <- '<'
    Greater     <- '>'
    GreaterEqual<~ Greater Equal
    LesserEqual <~ Lesser Equal

    PlusPlus    <~ Plus Plus
    MinusMinus  <~ Minus Minus

    HexDigits   <- (HexAlpha|Num)
    AlphaNum    <- (Alpha|Num)
    Num         <- [0-9]
    HexAlpha    <- [a-fA-F]
    Alpha       <- [a-zA-Z_]

    Div         <- '/'
    Mul         <- '*'
    Dot         <- '.'
    Semicolon   <- ';'
    Colon       <- ':'
    Comma       <- ','
    LeftParen   <- '('
    RightParen  <- ')'
    LeftSquare  <- '['
    RightSquare <- ']'
    LeftCurly   <- '{'
    RightCurly  <- '}'
    Minus       <- '-'
    Plus        <- '+'
    Equal       <- '='
    At          <- '@'
    Bang        <- '!'
    Amp         <- '&'
    Qmark       <- '?'
    Dollar      <- '$'

    HexPrefix   <- "0x" / "0X"

################################################################################
# Keywords

    Keyword < BasicType
            / Unit
            / Prot
            / Else
            / If
            / Unit
            / Prot
            / Else
            / If
            / Import
            / Interface
            / Virtual
            / Struct
            / Class
            / Function
            / Static
            / Return
            / Break
            / Continue
            / While
            / Var
            / Const
            / Aka
            / Auto
            / Is
            / Foreach
            / Switch
            / Null
            / On


    BasicType  < BasicFloatType
                / BasicIntegerType

    BasicFloatType  < F64
                    / F32

    BasicIntegerType    < S64
                        / S32
                        / S16
                        / S8
                        / U64
                        / U32
                        / U16
                        / U8
                        / UREG
                        / SREG

    Unit    <- "unit"
    Prot    <- "protection"
    Else    <- "else"
    If      <- "if"
    Import  <- "import"
    Interface <- "interface"
    Virtual <- "virtual"
    Struct  <- "struct"
    Class   <- "class"
    Function<- "function"
    Static  <- "static"
    Return  <- "return"
    Break   <- "break"
    Continue<- "continue"
    While   <- "while"
    Var     <- "var"
    Const   <- "const"
    Aka     <- "aka"
    Auto    <- "auto"
    Is      <- "is"
    Foreach <- "foreach"
    On      <- "on"
    Switch  <- "switch"
    Null    <- "null"
    Enum    <- "enum"

    SREG    <- "sreg"
    UREG    <- "ureg"
    F64     <- "f64"
    F32     <- "f32"
    S64     <- "s64"
    S32     <- "s32"
    S16     <- "s16"
    S8      <- "s8"
    U64     <- "u64"
    U32     <- "u32"
    U16     <- "u16"
    U8      <- "u8"
`));

enum overview =q{

function foo(): s8;
static function foo(): s8;
static function*(): s8 freeFuncPtr;
function*(): s8 memberFuncPtr;

};

enum source1 = `
    unit a.$function;
    import(0:s8) r.d, s.d,t;
    import(1) s1, s256yy;
    struct Foo{}
    var s8*[]*[] q,h; var sreg j;
    var Foo[][] foo;
    virtual unit c;
    protection(private)
    protection(public) struct Foo { var sreg a,b,c; }
    virtual unit d;
    @const @inline function bar()
    {
        a;
        a = b;
        a = b + c;
        ++a;
        --a;
        a = a++;
        a = a[0][1];
        a = *derefer;
        a = b:ToType;
        a = b:ToType + c:ToType;;
        if (a == 0) {call(a);}

        var s8 a = 8;

        if (a == 0) {call(a);}
        else {call(1);}
        a.b(8);
        a.b(8, (c + d) * 8);
        a = call()++;
        b = ((1 + a) / (1 - a)) + (a * b);
        b = ((1:s32 + a * 2 * c++) / (1 - a:ToType)) + (a * b);
        b = b(b(b(8)));
        ++a = b + c;
        a = ++++b;
        a = b = c + d;
        a = b[c];
        a = b[c..d];

        if (a[8]?.b?.c == 8)
            callThis();
        else
            callThat();

        instances[a].instances[b] = 8;
        a = b[c].d[e].f[g];

        (a + b)++;

        a = (b[c](param0, param1 + stuff):u32):u64;

        a = b[c](param0).b[c](param0);

        var auto a = 8;

        is function*() aka FuncPtr;

        const auto a = (b[0].b[1].b[2])(8);

    }

    #ok, the peg is used to have a "formal" grammar
    # now let's write this by hand
    enum A
    {
        a = 0,
        b,
        c = 2
    }
`;

// use "on" instead of "case" ?
auto s =
q{

    switch (value)
    {
        on 0,8,9: return "flûte";
        on 10..20: return "nay";
        else return "yah";
    }
};

// done
auto f =
q{
    L1:
    foreach(t; ts)
    {
        if (t.call())
        {
            t.otherCall();
            break L1;
        }
    }

    // break after expression;
    L1:
    foreach(t; ts)
    {
        if (t.call())
            break(L1) t.otherCall();
    }
};


auto c =
q{
    class Foo
    {
        var s32 field;
        @virtual void foo();
        @abstract void foo();
        @constructor @virtual Foo construct(s32 param)
        {
            /// auto generated alloc
            this.field = param;
            return this;
        }
        @destructor @virtual void destruct()
        {

        }
    }

    void foo()
    {
        Foo f = Foo.construct();
        f.destruct();
    }
};

// reuse "on" instead of "catch" ?
auto h =
q{
    try
    {
    }
    on(Exception e)
    {
    }

};

// use pipe operator to make unions/algebraic ?
auto u =
q{
    struct SomeUnion
    {
        s8 a | s32 b | SomeType c;
    }
};

// done
auto a =
q{
    is void* aka Ptr ;
};

// function composition ?
auto k =
q{
    function foo1(s8 p): s8;
    function foo2(s8 p): s8;
    is foo1 + foo2 aka f1 ;
    is foo1(foo2) aka f2 ;
};


unittest
{
    const ParseTree tree = Yatol(source1);
    pegged.tohtml.toHTML!(Expand.ifNotMatch, "Literal", "Chain", "List", "Expression")(tree,
        "/home/basile/ya-tree.html");
    assert(tree.successful);
}
