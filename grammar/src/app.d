module app;

void main()
{
    version (gen)
    {
        import pegged.grammar, pegged.peg;

        asModule("yatol", "src/yatol", import("peg.txt"));
    }
    version (test)
    {
        import pegged.tohtml, std.path, std.file, std.process;

        // generated by running the "generate" config
        import yatol;

        string fname = __FILE_FULL_PATH__.dirName ~ "/../ya-tree.html";
        auto tree = Yatol(source1);
        toHTML!(Expand.ifNotMatch, "Literal", "Chain", "List", "Expression")(tree, fname);
        if (fname.exists)
            browse(fname);
    }
}

// sample source
enum source1 = "#!yatol --ast
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
    function bar()
    {
        a;
        a++;
        a = b;
        a = b + c;
        a.b = c.d;
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
        super.call(a);
        super.call.call(a);
        parent?.call(a);
        a = b = c + d;
        a = b[c];
        a = b[c..d];
        if (a[8]?.b?.c == 8)
            callThis();
        else
            callThat();
        instances[a].instances[b] = 8;
        a = instances[a].instances[b];
        a = b[c].d[e];
        (a + b)++;
        a = (b[c](param0, param1 + stuff):u32):u64;
        a = b[c](param0).b[c](param0);
        var auto a = 8;
        aka FuncPtr = (function fun())*;
        const auto a = (b[0].b[1].b[2])(8);
        if (const s8 a = call())
            do();
        switch(a)
        {
            on (0,1) doThis();
            on (2,3) doThat();
            on (4,6) {doThisAndThat();}
            else
            {
                a++++;
            }
        }

        var function _():s8 a;
        var function _():(function _():s8[])[] arrayOfFuncReturnArrayOfS8;

        const auto a = [];
        const auto a = [1,2];
        const auto a = [[1,2]];
        const auto a = [[1,2],[1,2]];

        version(a) const int j = 8;
        version(a & b) const int k = 8;
        version((a | b) & c) const int m = 8;
        version(a) { const int v = 6; } else { const int v = 7; }
        version(a) const int v = 8; else const int v = 9;

        version(!a) const int j = 8;

        version(a1 & a2 | a3) const int a1anda2_or3 = 8;
        version(a1 | a2 & a3) const int a1or_a2anda3 = 8;
        version(a1 | a2 & a3 | a4) const int a1or_a2anda3_ora4 = 8;
        version(a1 | a2 & (a3 | a4)) const int a1or_a2and_a3ora4 = 8;

        version(a1) a = 8;

        a += b;
        a *= b = c;

        try
            call();
        on(FileError fe, OptionError oe)
            cleanup1();
        on(StreamError se)
            cleanup2();
        finally
            alwaysDoThis();

        try
        {
            try
                call();
            on(FileError fe, OptionError oe)
                cleanup1();
            on(StreamError se)
                cleanup2();
            finally
                alwaysDoThis();
        }
        finally
        {
            return 0;
        }

        var s8[] b;
        b = [0:s8,1:s8];

        const bool b = elem in container;

        throw Exception.create(\"oops\");
        throw Exception.create(`oops`);

        assert(true == true);

        continue(@L0) afterThat();
        label L0;

        foreach(const auto a, const auto b; stuff)
            call(a,b);
    }

    enum A
    {
        a = 0,
        b,
        c = 2
    }

    class Foo: Bar.bar, Baz{}

    union U
    {
        var A a; var  B b; var  C c;
    }

    @foo struct Foo
    {
        @bar class Bar
        {
            @baz interface Baz
            {
            }
        }
    }

    class A<T0, T1> {}

    var A<int, Bar> a;

    function foo<T>(var T t)
    {
        foo<int>(0);
    }

    template Foo<T0,T1>
    {
        const T0 t0;
        const T1 t1;
    }

    struct Last {}
";

