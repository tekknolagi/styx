# Expressions

## Compiler echo

### Grammar

    CompilerEcho  ::= "echo" "(" Identifier ("," EchoParameter)* ")"
    EchoParameter ::= "{" Expression ";" "}"
                    | Type
                    | Keyword

### Semantic

Compiler echoes are the reflection system of the language.
The first parameter is a command followed by optional parameters.
Number of parameters and the constant value returned depend on the command.

The echo expression is completly replaced by a constant during compilation and
_Expression_ using the result are simplified accordingly,
for example by eleminating unreachable branches in _IfElseExpression_.

#### The `line` echo

    const s32 line = echo(line);

Evaluates to the line, as a `u32`, in the source file, 0-based, of the echo.
No parameter is expected.

#### The `semver` echo

    assert(echo(version) != "v99.99.99");

Evaluates to the version of the compiler, as a `s8[]`, representing a SemVer.
No parameter is expected.

#### The `is` echo

Evaluates if the first parameter, a _Type_, is of same type as the second.
The second parameter must be a _Type_ or one of the keyword used to declare aggregates,
i.e "class", "interface", "struct" or "union".

    function foo<T>()
    {
        const bool T_is_a_class = echo(is, T, class);
        const bool T_is_s32 = echo(is, T, s32);
    }

## Cast

### Grammar

    Cast ::= ":" Type

### Semantic

#### In declarations

When used in a declaration, a _Cast_ indicates a type.
This syntax is part of _EnumDeclaration_ and _FunctionDeclaration_

#### Dynamic cast

When used on the right of an expression evaluating to a class or to an interface the cast is dynamic.
If the class or the interface implements the _Type_ then its matching instance is returned, otherwise null is returned.

    class Bar {}
    class Baz {}
    class Foo : Bar { @constructor function construct(){} }
    Foo foo = Foo.construct();
    Bar bar = foo:Bar;
    assert(bar != null);
    assert(foo:Baz == null);

#### Reinterpret cast

Otherwise _Cast_ reinterprets the left expression as _Type_.

    struct Foo {const s32 a = 42;}
    Foo foo;
    assert(foo:s32 == 42);

To reinterpret cast class and interface instances they can be cast as pointer first.

    class Bar { @constructor function construct(){} const s32 a;}
    class Baz { @constructor function construct(){} const s32 b;}
    var Bar bar = Foo.construct();
    var Baz baz = bar:Baz; // dynamic cast
    assert(baz == null);
    // cast the reference to bar as a pointer and this pointer as a reference to a Baz
    baz = bar:s32*:Baz;
    assert(baz.b == 42);
