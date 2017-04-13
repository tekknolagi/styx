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
                    / ProtectionOverwrite
                    / VariableDeclaration
                    / ClassDeclaration
                    / StructDeclaration
                    / FunctionDeclaration

    VariableDeclaration < ProtectionAttribute? Type IdentifierList Semicolon
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

    FunctionParameters < VariableDeclaration+

    FunctionHeader < Type Identifier LeftParen FunctionParameters? RightParen

    FunctionBody < LeftCurly RightCurly   # Statements in between
                 / Semicolon

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

    Type < TypeIdentifier TypeModifier?
    TypeIdentifier  <  BasicType
                    /  Identifier

    TypeModifier < ArrayDimensions

    ArrayDimensions < LeftRightSquares+

################################################################################
# Scope

    Scope < ProtectionAttribute? LeftCurly Declarations RightCurly

################################################################################
# ProtectionAttribute

    ProtectionOverwrite < ProtectionAttribute Colon
    ProtectionAttribute < Prot LeftParen Identifier RightParen

################################################################################
# List, chain, etc

    IdentifierChain < Identifier (Dot Identifier)*

    IdentifierList  < Identifier (Comma Identifier)*

    LeftRightSquares < LeftSquare RightSquare

################################################################################
# Identifier and numbers

    HexLiteral  <- HexPrefix HexDigits+ HexLiteralSuffix?
    HexLiteralSuffix <- Colon BasicType

    FloatLiteral  <- Num+ Dot Num+ IntLiteralSuffix?
    FloatLiteralSuffix <- Colon BasicFloatType

    IntLiteral  <- Num+ IntLiteralSuffix?
    IntLiteralSuffix <- Colon BasicIntegerType


    Identifier  <~ !Keyword (Alpha) (AlphaNum)*

    HexDigits   <- (HexAlpha|Num)
    AlphaNum    <- (Alpha|Num)
    Num         <- [0-9]
    HexAlpha    <- [a-fA-F]
    Alpha       <- [a-zA-Z_]

    Div         <- '/'
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

enum source1 = `
    unit a.b;
    import(0:s8) r.d,s.d,t;
    import(1) s1,s256yy;
    s8 q,h; sreg j;
    Foo[][  ] foo;
    virtual unit c;
    protection(private):
    protection(public) struct Foo { sreg a,b,c; }
    virtual unit d;
    s8 foo(s8 a,b; f32 c;){}
    Foo foo(s8 a,b;);
`;

unittest
{
    const ParseTree tree = Yatol(source1);
    pegged.tohtml.toHTML(tree, "/home/basile/ya-tree.html");
    assert(tree.successful);
}
