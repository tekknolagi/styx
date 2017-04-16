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
                    / ProtectionOverwrite
                    / VariableDeclaration
                    / ClassDeclaration
                    / StructDeclaration
                    / FunctionDeclaration
                    / Scope

    VariableDeclaration < ProtectionAttribute? Static? TypedVariableList Semicolon
    StructDeclaration   < ProtectionAttribute? Struct Identifier LeftCurly Declarations? RightCurly
    ClassDeclaration    < ProtectionAttribute? Class Identifier LeftCurly Declarations? RightCurly

################################################################################
# Imports declaration

    ImportDeclaration < Import ImportPriority? ImportList Semicolon
    ImportList <- IdentifierChain (Comma IdentifierChain)*
    ImportPriority <- LeftParen IntLiteral RightParen

################################################################################
# Function declaration

    FunctionDeclaration < FunctionHeader FunctionBody

    FunctionHeader < ProtectionAttribute? Static? Function Identifier LeftParen FunctionParameters? RightParen Cast?

    FunctionBody < LeftCurly Declarations RightCurly
                 / Semicolon

    FunctionPointerType < Static? Function Mul Identifier LeftParen FunctionParameters? RightParen Cast?

    FunctionParameters < TypedVariableList (Semicolon TypedVariableList)

################################################################################
# Initializer

################################################################################
# Statements
#
#    Statements  < (Statement SemiColon)+
#    Statement   < ExpressionStatement
#                / IfElseStatement
#
################################################################################
# Expressions
#
#    ExpressionStatement < Expressions*
#
#    Expressions <
#        CallExpression
#        BinaryExpression
#
################################################################################
# Cast

    Cast <- Colon Type

################################################################################
# Type

    TypedVariableList < Type IdentifierList

    Type < Static? TypeIdentifier TypeModifiers? Mul*
    TypeIdentifier  <  BasicType
                    /  IdentifierChain
                    /  FunctionPointerType

    TypeModifiers < TypeModifier TypeModifiers?

    TypeModifier < LeftRightSquares / Mul

################################################################################
# Scope

    Scope < ProtectionAttribute? LeftCurly Declarations RightCurly

################################################################################
# Protection

    ProtectionOverwrite < ProtectionAttribute Colon
    ProtectionAttribute < Prot LeftParen Identifier RightParen

################################################################################
# List, chain, etc

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

    HexLiteral  <- HexPrefix HexDigits+ HexLiteralSuffix?
    HexLiteralSuffix <- Colon BasicType

    FloatLiteral  <- Num+ Dot Num+ FloatLiteralSuffix?
    FloatLiteralSuffix <- Colon BasicFloatType

    IntLiteral  <- Num+ IntLiteralSuffix?
    IntLiteralSuffix <- Colon BasicIntegerType

    Eol <- "\r\n" / '\n'

    Identifier  <~ !Keyword (Alpha) (AlphaNum)*

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

    HexPrefix   <- "0x"

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
    Import  <- "import"
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
    import(0:s8) r.d,s.d,t;
    import(1) s1,s256yy;
    s8*[]*[] q,h; sreg j;
    Foo[][  ] foo;
    virtual unit c;
    protection(private):
    protection(public) struct Foo { sreg a,b,c; }
    virtual unit d;
    function foo(s8 a,b;): Foo;
    function bar(){};
    /*sfsdfsdf °0°0°033&
    */
`;

unittest
{
    const ParseTree tree = Yatol(source1);
    pegged.tohtml.toHTML(tree, "/home/basile/ya-tree.html");
    assert(tree.successful);
}
