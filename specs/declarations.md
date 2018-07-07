# Declarations

## Unit declaration

### Grammar

    UnitDeclaration ::= "unit" IdentifierChain ";"

### Semantic

All source files must begin with a _UnitDeclaration_ (optionally preceeded by a she bang line).
The _IdentifierChain_ must follow a path of the system.
For example if `/a/b/c.sx` is passed to the compiler the declaration `unit a.b.c;` is expected.

## Imports

### Grammar

    ImportDeclaration ::= "import" [ImportPriority] ImportList ";"
    ImportList        ::= IdentifierChain ("," IdentifierChain)*
    ImportPriority    ::= "(" IntLiteral ")"

### Semantic

An _ImportDeclaration_ specifies a unit whose content is used to resolve unsolved symbols.
Local declarations are considered first so that using a fully qualified name might be nececessary to distinguish a local declaration of an imported declaration.

The _IdentifierChain_ must match to source file.
The last list element is the file name, the firsts are directories.
If the last list element doesn't match to a file name but to a directory then all sources found in the matching directory are imported.

The _ImportPriority_ is not implemented and has no effect on the program.

## Protection

### Grammar

    ProtectionDeclaration ::= "protection" "(" Identifier ")"

### Semantic

A _ProtectionDeclaration_ defines the visibility of the declarations that follow.
_ProtectionDeclaration_ don't nest, which means that a _ProtectionDeclaration_ overwrites the previous protection.
Until the first _ProtectionDeclaration_, everything is public.

Valid _Identifier_ are

#### `strict`

Strict declarations, when located in an aggregate are only visible from the aggregate.
Otherwise strict declarations have the same semantic as private ones.

#### `private`

Private declarations are only visible from the unit where they are declared.

#### `protected`

Protected declarations, when located in an aggregate are only visible from the aggregate or from its derived types.
Otherwise protected declarations have the same semantic as private ones.

#### `public`

Public declarations are always visible.

#### Example

    unit u;

    var s32 public_var;

    protection(private)

    var s32 private_var;

    struct StrictStruct
    {
    protection(strict)
        var s32 strict_var;
    protection(private)
        var s32 private_var;
    }

    function main(): s32
    {
        var StrictStruct ss;
        ss.strict_var = 42; // error
        ss.private_var = 42; // ok
    }

## Attributes

### Grammar

    AtAttributes          ::= AtAttribute*
    AtAttribute           ::= "@" (Identifier | Keyword) [AtAttributeParameters]
    AtAttributeParameters ::= "(" PrimaryExpression ("," PrimaryExpression)* ")"

### Semantic

_AtAttributes_ allow to attach metadata to declarations, which can be retrieved at runtime.
They are also used by the compiler to detect function with a special semantic such as `@unittest`, `@operator` or `@constructor` functions.
In the later case, _Identifier_ is a reserved word, which cant be used for user metadata.

Reserved _Identifier_ in the context of this rule are

- constructor
- destructor
- deprecated
- extern
- unittest
- abstract
- virtual
- operator
- final
- inline
- set
- get
- ducktype

Keywords used in _AtAttribute_ are

- const

Meta data can be retrieved using the following _CompilerEcho_

- _hasAttribute_
- _getAttributeParameter_

### Example

    @mdt("Metadata") function foo()
    {
        assert(echo(hasAttribute, foo, mdt));
        // write "Metadata" to the standard output.
        const auto meta_parameter_index = 0;
        printf("%s", echo(getAttributeParameter, foo, mdt, meta_parameter_index));
    }

For semantic of reserved attributes see the semantic section of the declaration they are used for.

## Enumerations

### Grammar

    EnumDeclaration ::= "enum" Identifier [Cast] "{" EnumMember ("," EnumMember)* "}"
    EnumMember      ::= Identifier ["=" Initializer]

### Semantic

An EnumDeclaration allows to declare the equivalent of several const variables in a named group.
Each members have the same type, by default `u32` when the _Cast_ is not present.
When the Initializer is not specified then the member value is set to its rank,
starting from the last member which is initialized.
The _Type_ specified in the _Cast_ must be integral and a value can only be used once.

    enum EnumA: u8
    {
         a0     // 0
        ,a1     // 1
        ,a2 = 8 // 8
        ,a3     // 9
    }

Members can be used without the _Identifier_ used in the _EnumDeclaration_.

    const EnumA a = a0 | a2;

Members coerce to the enumeraton _Type_

    const u8 a = EnumA.a0;

Conflicts in the scope are checked (TODO) and it's not possible to declare
something with the same identifier.

    function foo()
    {
        enum EnumB: u8 { b0 }
        const s32 b0 = 16; // error
    }

