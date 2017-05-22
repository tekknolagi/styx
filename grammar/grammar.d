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

    # Statements should go with Declarations

    Declarations    < Declaration+
    Declaration     < ImportDeclaration
                    / ProtectionDeclaration
                    / VariableDeclaration
                    / ClassDeclaration
                    / StructDeclaration
                    / FunctionDeclaration
                    / ScopeDeclaration
                    / InterfaceDeclaration

    ProtectionDeclaration   < Prot LeftParen Identifier RightParen
    StructDeclaration       < Struct Identifier LeftCurly Declarations? RightCurly
    ClassDeclaration        < Class Identifier LeftCurly Declarations? RightCurly
    InterfaceDeclaration    < Interface Identifier LeftCurly Declarations? RightCurly
    ScopeDeclaration        < LeftCurly Declarations RightCurly

    VariableDeclaration     < Static? Type VariableDeclarationList Semicolon

    VariableDeclarationList < VariableDeclarationItem (Comma VariableDeclarationItem)*

    VariableDeclarationItem < Identifier Initializer?

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
    Attribute <- At Identifier

################################################################################
# ParameterStorageClass



################################################################################
# Initializer

    Initializer < BasicTypeInitializer
                / ArrayDimInitializer

    BasicTypeInitializer < Equal UnaryExpression
    ArrayDimInitializer < Equal LeftSquare ArrayInitializerElements? RightSquare

    ArrayInitializerElements < UnaryExpression (Comma UnaryExpression)*

################################################################################
# DeclarationOrStatement

    DeclarationOrStatementsBlock < DeclarationOrStatement
                                 / LeftCurly DeclarationOrStatements RightCurly

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
#               / SwitchStatement
#               / CaseStatement

    EmptyStatment < Semicolon

    ExpressionStatement < AssignExpression Semicolon

    IfElseStatement < If IfCondition DeclarationOrStatementsBlock (Else DeclarationOrStatementsBlock)?

    WhileStatement < While LeftParen IfCondition RightParen DeclarationOrStatementsBlock

    ReturnStatement < Return AssignExpression? Semicolon

    ContinueStatement < Continue AssignExpression? Semicolon

    BreakStatement < Break AtLabel? AssignExpression? Semicolon

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

    #NOT ANYMORE ? BUG: Expression LeftSquare, parse only if space seprated.

    BinaryExpression < Expression Operator Expression

    CallParameters < Expression (Comma Expression)*

    RelationalExpression    < Expression RelOperator Expression
                            / ConditionalIdentifierChain RelOperator Expression

    DotExpression < Expression Dot Expression

    OptionalExpression  < Expression Qmark

################################################################################
# Postfixable single expression

    ParenExpression < UnaryPrefix? LeftParen Expression RightParen PostfixExpression*

    UnaryExpression < UnaryPrefix? IdentifierChain CallExpression? PostfixExpression*
                    / NumberLiteral UnarySuffix?
                    / UnaryPrefix? UnaryExpression
                    / UnaryPrefix? ParenExpression

################################################################################
# PostfixExpression

    PostfixExpression   < PlusPlus
                        / MinusMinus
                        / IndexExpression
                        / RangeExpression
                        / callParameters
                        / Cast

    IndexExpression < LeftSquare Expression RightSquare

    RangeExpression < LeftSquare Expression Ellipsis Expression RightSquare

################################################################################
# Cast

    Cast <- Colon Type

################################################################################
# Type

    TypedVariableList < Type IdentifierList

    Type < TypeIdentifier TypeModifiers?
    TypeIdentifier  <  BasicType
                    /  IdentifierChain
                    /  FunctionPointerType

    TypeModifiers < TypeModifier TypeModifiers?

    TypeModifier < LeftRightSquares / Mul

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
# Identifier and numbers

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
    Ellipsis    <~ Dot Dot
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
    unit a.b;
    import(0:s8) r.d, s.d,t;
    import(1) s1, s256yy;
    struct Foo{}
    s8*[]*[] q,h; sreg j;
    Foo[][] foo;
    virtual unit c;
    protection(private)
    protection(public) struct Foo { sreg a,b,c; }
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

        s8 a = 8;

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

        return;
        break a.call();
    }
`;

/*
        while(call())
        {
            if (a)
                break a.call();
        }
        ++(a);
    }
*/

auto s =
q{

    switch (value)
    {
        case(0,8,9) return "flûte";
        case(10..20) return "nay";
        else return "yah";
    }
};

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



unittest
{
    const ParseTree tree = Yatol(source1);
    pegged.tohtml.toHTML!(Expand.ifNotMatch, "Literal", "Chain", "List", "Expression")(tree,
        "/home/basile/ya-tree.html");
    assert(tree.successful);
}
