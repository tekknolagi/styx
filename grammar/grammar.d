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

    BasicTypeInitializer < Equal InitializerElement
    ArrayDimInitializer < Equal LeftSquare ArrayInitializerElements? RightSquare

    ArrayInitializerElements < InitializerElement (Comma InitializerElement)*

    InitializerElement  < NumberLiteral
                     #   / StructInit...

################################################################################
# DeclarationOrStatement

DeclarationOrStatements < DeclarationOrStatement+

DeclarationOrStatement  < Declaration
                        / Statement

################################################################################
# Statements

Statement   < EmptyStatment
#            / ExpressionStatement
#            / IfElseStatement
#            / SwitchStatement
#            / CaseStatement
#            / ContinueStatement
#            / BreakStatement
#            / ReturnStatement

EmptyStatment < Semicolon

################################################################################
# Expressions
#
#    ExpressionStatement < Expressions*
#
#    Expressions <
#        / AssignExpression
#        / CallExpression
#        / UnaryExpression
#        / BinaryExpression
#
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
    Equal       <- '='
    At          <- '@'

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
    @const @inline function bar(){}
    s8 signed1 = 42, signed2 = 355;
`;

unittest
{
    const ParseTree tree = Yatol(source1);
    pegged.tohtml.toHTML(tree, "/home/basile/ya-tree.html");
    assert(tree.successful);
}
