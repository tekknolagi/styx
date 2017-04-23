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
                / ExpressionStatement
                / IfElseStatement
#               / SwitchStatement
#               / CaseStatement
#               / ContinueStatement
#               / BreakStatement
#               / ReturnStatement

    EmptyStatment < Semicolon

    ExpressionStatement < PrimaryExpression Semicolon

    IfElseStatement < If LeftParen ConditionExpression RightParen DeclarationOrStatementsBlock (Else DeclarationOrStatementsBlock)?

################################################################################
# Expressions

    PrimaryExpression   < AssignExpression
                        / BinaryExpression
                        / ParenExpression
                        / CastExpression
                        / UnaryExpression
                        / ConditionExpression
                        / PolishExpression

    ParenExpression < LeftParen PrimaryExpression RightParen

    AssignExpression < PrimaryExpression Equal PrimaryExpression

    BinaryExpression < PrimaryExpression Operator PrimaryExpression

    CastExpression   < PrimaryExpression Cast

    UnaryExpression < UnaryPrefix? IdentifierChain CallExpression? UnarySuffix?
                    / NumberLiteral UnarySuffix?

    CallExpression < LeftParen CallParameters? RightParen

    CallParameters < PrimaryExpression (Comma PrimaryExpression)*

    ConditionExpression < PrimaryExpression CmpOperator PrimaryExpression



    PolishExpression < "@PN" LeftParen PolishPrimaryExpression* RightParen

    PolishPrimaryExpression < Operator PolishOperand+

    PolishOperand   < NumberLiteral
                    / IdentifierChain

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

    CmpOperator < EqualEqual
                / NotEqual
                / Lesser
                / Greater
                / GreaterEqual
                / LesserEqual

    UnaryPrefix < Mul / PlusPlus / MinusMinus / Amp

    UnarySuffix < PlusPlus / MinusMinus

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

    HexPrefix   <- "0x" / "0X"

################################################################################
# Keywords

    Keyword <  Unit
            / Prot
            / Import
            / BasicType

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
        a = 8;
        a = unary;
        a = a + a;
        ++a;
        --a;
        a = a++;
        a = *derefer;
        a = b:ToType;
        a = b:ToType + b:ToType;;
        s8 a = 8;
        b = @PN(+ 8 2 * 8 + 1 1 1 1);
        if (a == 0) {call(a);}
        else {call(1);}
        a.b(8);
        a.b(8, (c + d) * 8);
        a = call()++;
        b = ((1 + a) / (1 - a)) + (a * b);
    }
    s16 signed1 = 42, signed2 = 355;
`;

auto s =
q{

    switch (value)
    {
        case(0,8,9) return "flûte";
        case(10..20) return "nay";
        else return "yah";
    }
};

unittest
{
    const ParseTree tree = Yatol(source1);
    pegged.tohtml.toHTML!(Expand.ifNotMatch, "Literal", "Chain", "List", "Expression")(tree,
        "/home/basile/ya-tree.html");
    assert(tree.successful);
}
