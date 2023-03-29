// { dg-prune-output "Warning: struct HasNonConstToHash has method toHash" }
// { dg-prune-output "HasNonConstToHash.toHash defined here:" }
void main()
{
    issue19562();
    issue15111();
    issues16654And16764();
    issue18918();
    issue18925();
    issue19005();
    issue19204();
    issue19262();
    issue19282();
    issue19332(); // Support might be removed in the future!
    issue19568();
    issue19582();
    issue20034();
    issue21642();
    issue22024();
    issue22076();
    testTypeInfoArrayGetHash1();
    testTypeInfoArrayGetHash2();
    pr2243();
}

/// Check hashOf an array of void pointers or delegates is @safe.
void issue19562() @nogc nothrow pure @safe
{
    void*[10] val;
    size_t h = hashOf(val[]);

    alias D = void delegate();
    D[10] ds;
    h = hashOf(ds[]);
}

/// hashOf was failing for structs that had an `alias this` to a dynamic array.
void issue15111()
{
    void testAlias(T)()
    {
        static struct Foo
        {
            T t;
            alias t this;
        }
        Foo foo;
        static assert(is(typeof(hashOf(foo))));
    }
    // was fixed
    testAlias!(int[]);
    testAlias!(int*);
    // was not affected
    testAlias!int;
    testAlias!(void delegate());
    testAlias!(string[string]);
    testAlias!(int[8]);
}

void issues16654And16764()
{
    auto a = [1];
    auto b = a.dup;
    assert(hashOf(a) == hashOf(b));
}

/// Check hashOf dynamic array of scalars is usable in @safe code.
void issue18918() nothrow pure @safe
{
    const _ = (() @nogc => hashOf("abc"))();

    static struct S { string array; }
    auto s1 = S("abc");
    auto s2 = S(s1.array.idup);
    assert(hashOf(s1) == hashOf(s2));
    enum e = hashOf(S("abc"));
    assert(hashOf(s1) == e);
}

/// Check hashOf struct of scalar fields is usable in @safe code.
void issue18925() @nogc nothrow pure @safe
{

    static struct S { int a; int b; }
    auto h = hashOf(S.init);
}

void issue19005() @nogc nothrow pure @safe
{
    enum Month : ubyte
    {
        jan = 1
    }
    static struct Date
    {
        short _year;
        Month _month;
        ubyte _day;
    }
    Date date;
    auto hash = date.hashOf;
}

/// Accept SIMD vectors.
void issue19204() @nogc nothrow pure @safe
{
    version (D_SIMD)
    {
        static import simd = core.simd;
        static if (is(simd.int4)) // __traits(isArithmetic)
        {{
            enum simd.int4 val = [1,2,3,4];
            enum ctfeHash = hashOf(val);
            simd.int4 rtVal = val;
            auto rtHash = hashOf(rtVal);
            assert(ctfeHash == rtHash);
        }}
        static if (is(simd.void16)) // non __traits(isArithmetic)
        {{
            auto h = hashOf(simd.void16.init);
        }}
        static if (is(simd.float4)) // __traits(isArithmetic) and __traits(isFloating)
        {{
            enum simd.float4 val = [1.1f, 2.2f, 3.3f, 4.4f];
            enum ctfeHash = hashOf(val);
            simd.float4 rtVal = val;
            auto rtHash = hashOf(rtVal);
            assert(ctfeHash == rtHash);
        }}
    }
}

/// hashOf associative array should infer nothrow
void issue19262() nothrow
{
    int[int] aa;
    auto h = hashOf(aa);
    h = hashOf(aa, h);
}

extern(C++) class Issue19282CppClass {}

/// test that hashOf doesn't crash for non-null C++ objects.
void issue19282()
{
    Issue19282CppClass c = new Issue19282CppClass();
    size_t h = hashOf(c);
    h = hashOf(c, h);
}

/// Ensure hashOf works for const struct that has non-const toHash & has all
/// fields bitwise-hashable. (Support might be removed in the future!)
void issue19332()
{
    static struct HasNonConstToHash
    {
        int a;
        size_t toHash() { return a; }
    }
    const HasNonConstToHash val;
    size_t h = hashOf(val);
    h = hashOf!(const HasNonConstToHash)(val); // Ensure doesn't match more than one overload.
}

/// hashOf should not unnecessarily call a struct's fields' postblits & dtors in CTFE
void issue19568()
{
    static struct S1
    {
        @disable this(this);

        ~this() @nogc nothrow
        {
            import core.stdc.stdio;
            if (mptr) puts("impure");
        }

        size_t[2] pad;
        void* mptr;
    }

    static struct S2
    {
        @disable this(this);

        ~this() @nogc nothrow
        {
            import core.stdc.stdio;
            if (fd != -1) puts("impure");
        }

        int fd = -1;
        S1 s1;
    }

    static struct S3
    {
        private S2 s2;
    }

    S3 s3;
    size_t h = ((ref S3 s3) pure => hashOf(s3))(s3);
}

/// Check core.internal.convert.toUbyte in CTFE for arrays works with
/// reference type elements and doesn't call postblits/dtors.
void issue19582()
{
    import core.internal.convert : toUbyte;
    final static class C : Object {}
    enum b1 = (() @nogc nothrow pure @safe { C[10] o; return toUbyte(o[])[0]; })();

    static struct S
    {
        int x;
        @disable this(this);
        ~this() @nogc nothrow
        {
            import core.stdc.stdio : puts;
            if (x) puts("impure");
        }
    }
    enum b2 = () {
            return ((const S[] a) @nogc nothrow pure @safe => toUbyte(a))(new S[10]);
        }();
}

/// Check core.internal.hash.hashOf works with enums of non-scalar values
void issue20034()
{
    enum E
    {
        a = "foo"
    }
    // should compile
    assert(hashOf(E.a, 1));
}

/// [REG 2.084] hashOf will fail to compile for some structs/unions that recursively contain shared enums
void issue21642() @safe nothrow pure
{
    enum C : char { _ = 1, }
    union U { C c; void[0] _; }
    shared union V { U u; }
    cast(void) hashOf(V.init);
    // Also test the underlying reason the above was failing.
    import core.internal.convert : toUbyte;
    shared C c;
    assert(toUbyte(c) == [ubyte(1)]);
}

/// Accept enum type whose ultimate base type is a SIMD vector.
void issue22024() @nogc nothrow pure @safe
{
    static if (is(__vector(float[2])))
    {
        enum E2 : __vector(float[2]) { a = __vector(float[2]).init, }
        enum F2 : E2 { a = E2.init, }
        assert(hashOf(E2.init) == hashOf(F2.init));
        assert(hashOf(E2.init, 1) == hashOf(F2.init, 1));
    }
    static if (is(__vector(float[4])))
    {
        enum E4 : __vector(float[4]) { a = __vector(float[4]).init, }
        enum F4 : E4 { a = E4.init, }
        assert(hashOf(E4.init) == hashOf(F4.init));
        assert(hashOf(E4.init, 1) == hashOf(F4.init, 1));
    }
}

/// hashOf(S) can segfault if S.toHash is forwarded via `alias this` to a
/// receiver which may be null.
void issue22076()
{
    static struct S0 { Object a; alias a this; }

    static struct S1
    {
        S0 a;
        inout(S0)* b() inout return nothrow { return &a; }
        alias b this;
    }

    static struct S2
    {
        S0 a;
        S1 b;
    }

    extern(C++) static class C0
    {
        int foo() { return 0; } // Need at least one function in vtable.
        S0 a; alias a this; // { dg-warning "is deprecated" }
    }

    extern(C++) static class C1
    {
        S1 a;
        inout(S1)* b() inout nothrow { return &a; }
        alias b this;       // { dg-warning "is deprecated" }
    }

    cast(void) hashOf(S0.init);
    cast(void) hashOf(S0.init, 0);
    cast(void) hashOf(S1.init);
    cast(void) hashOf(S1.init, 0);
    cast(void) hashOf(S2.init);
    cast(void) hashOf(S2.init, 0);
    auto c0 = new C0();
    cast(void) hashOf(c0);
    cast(void) hashOf(c0, 0);
    auto c1 = new C1();
    cast(void) hashOf(c1);
    cast(void) hashOf(c1, 0);
}

/// Tests ensure TypeInfo_Array.getHash uses element hash functions instead
/// of hashing array data.
void testTypeInfoArrayGetHash1()
{
    class C
    {
        int i;
        this(in int i) { this.i = i; }
        override hash_t toHash() { return 0; }
    }
    C[] a1 = [new C(11)], a2 = [new C(12)];
    assert(typeid(C[]).getHash(&a1) == typeid(C[]).getHash(&a2));
}

/// ditto
void testTypeInfoArrayGetHash2()
{
    struct S
    {
        int i;
        hash_t toHash() const @safe nothrow { return 0; }
    }
    S[] a1 = [S(11)], a2 = [S(12)];
    assert(typeid(S[]).getHash(&a1) == typeid(S[]).getHash(&a2));
}

/++
Use the new `core.internal.hash.hashOf` in all `TypeInfo.getHash` instead of
the `old rt.util.hash.hashOf`. Also make `typeid(T).getHash(&val)` get the
same result as `hashOf(val)`.
+/
void pr2243()
{
    static struct Foo
    {
        int a = 99;
        float b = 4.0;
        size_t toHash() const pure @safe nothrow
        {
            return a;
        }
    }

    static struct Bar
    {
        char c = 'x';
        int a = 99;
        float b = 4.0;
        void* d = null;
    }

    static struct Boom
    {
        char c = 'M';
        int* a = null;
    }

    static struct Plain
    {
        int a = 1;
        int b = 2;
    }

    interface IBoo
    {
        void boo();
    }

    static class Boo: IBoo
    {
        override void boo()
        {
        }

        override size_t toHash()
        {
            return 1;
        }
    }

    static struct Goo
    {
        size_t toHash() pure @safe nothrow
        {
            return 1;
        }
    }

    enum Gun: long
    {
        A = 99,
        B = 17
    }

    enum double dexpr = 3.14;
    enum float fexpr = 2.71;
    enum wstring wsexpr = "abcdef"w;
    enum string csexpr = "abcdef";
    enum int iexpr = 7;
    enum long lexpr = 42;
    enum int[2][3] saexpr = [[1, 2], [3, 4], [5, 6]];
    enum int[] daexpr = [7,8,9];
    enum Foo thsexpr = Foo();
    enum Bar vsexpr = Bar();
    enum int[int] aaexpr = [99:2, 12:6, 45:4];
    enum Gun eexpr = Gun.A;
    enum Foo[] staexpr = [Foo(), Foo(), Foo()];
    enum Bar[] vsaexpr = [Bar(), Bar(), Bar()];
    enum realexpr = 7.88;
    enum nullexpr = null;
    enum plstr = Plain();
    enum plarrstr = [Plain(), Plain(), Plain()];
    //No CTFE:
    Boom rstructexpr = Boom();
    Boom[] rstrarrexpr = [Boom(), Boom(), Boom()];
    int delegate() dgexpr  = (){return 78;};
    void* ptrexpr = &dgexpr;


    //CTFE hashes
    enum h1 = dexpr.hashOf();
    enum h2 = fexpr.hashOf();
    enum h3 = wsexpr.hashOf();
    enum h4 = csexpr.hashOf();
    enum h5 = iexpr.hashOf();
    enum h6 = lexpr.hashOf();
    enum h7 = saexpr.hashOf();
    enum h8 = daexpr.hashOf();
    enum h9 = thsexpr.hashOf();
    enum h10 = vsexpr.hashOf();
    enum h11 = aaexpr.hashOf();
    enum h12 = eexpr.hashOf();
    enum h14 = hashOf(new Boo);
    enum h15 = staexpr.hashOf();
    enum h16 = hashOf([new Boo, new Boo, new Boo]);
    enum h17 = hashOf([cast(IBoo)new Boo, cast(IBoo)new Boo, cast(IBoo)new Boo]);
    enum h18 = hashOf(cast(IBoo)new Boo);
    enum h19 = vsaexpr.hashOf();
    enum h20 = hashOf(cast(Foo[3])staexpr);

    //BUG: cannot cast [Boo(), Boo(), Boo()][0] to object.Object at compile time
    auto h21 = hashOf(cast(Boo[3])[new Boo, new Boo, new Boo]);
    auto h22 = hashOf(cast(IBoo[3])[cast(IBoo)new Boo, cast(IBoo)new Boo, cast(IBoo)new Boo]);
    enum h23 = hashOf(cast(Bar[3])vsaexpr);

    //NO CTFE (Compute, but don't check correctness):
    auto h24 = rstructexpr.hashOf();
    auto h25 = rstrarrexpr.hashOf();
    auto h26 = dgexpr.hashOf();
    auto h27 = ptrexpr.hashOf();

    enum h28 = realexpr.hashOf();
    enum h30 = nullexpr.hashOf();
    enum h31 = plstr.hashOf();
    enum h32 = plarrstr.hashOf();
    enum h33 = hashOf(cast(Plain[3])plarrstr);

    auto v1 = dexpr;
    auto v2 = fexpr;
    auto v3 = wsexpr;
    auto v4 = csexpr;
    auto v5 = iexpr;
    auto v6 = lexpr;
    auto v7 = saexpr;
    auto v8 = daexpr;
    auto v9 = thsexpr;
    auto v10 = vsexpr;
    auto v11 = aaexpr;
    auto v12 = eexpr;
    auto v14 = new Boo;
    auto v15 = staexpr;
    auto v16 = [new Boo, new Boo, new Boo];
    auto v17 = [cast(IBoo)new Boo, cast(IBoo)new Boo, cast(IBoo)new Boo];
    auto v18 = cast(IBoo)new Boo;
    auto v19 = vsaexpr;
    auto v20 = cast(Foo[3])staexpr;
    auto v21 = cast(Boo[3])[new Boo, new Boo, new Boo];
    auto v22 = cast(IBoo[3])[cast(IBoo)new Boo, cast(IBoo)new Boo, cast(IBoo)new Boo];
    auto v23 = cast(Bar[3])vsaexpr;
    auto v30 = null;
    auto v31 = plstr;
    auto v32 = plarrstr;
    auto v33 = cast(Plain[3])plarrstr;

    //NO CTFE:
    auto v24 = rstructexpr;
    auto v25 = rstrarrexpr;
    auto v26 = dgexpr;
    auto v27 = ptrexpr;
    auto v28 = realexpr;

    //runtime hashes
    auto rth1 = hashOf(v1);
    auto rth2 = hashOf(v2);
    auto rth3 = hashOf(v3);
    auto rth4 = hashOf(v4);
    auto rth5 = hashOf(v5);
    auto rth6 = hashOf(v6);
    auto rth7 = hashOf(v7);
    auto rth8 = hashOf(v8);
    auto rth9 = hashOf(v9);
    auto rth10 = hashOf(v10);
    auto rth11 = hashOf(v11);
    auto rth12 = hashOf(v12);
    auto rth14 = hashOf(v14);
    auto rth15 = hashOf(v15);
    auto rth16 = hashOf(v16);
    auto rth17 = hashOf(v17);
    auto rth18 = hashOf(v18);
    auto rth19 = hashOf(v19);
    auto rth20 = hashOf(v20);
    auto rth21 = hashOf(v21);
    auto rth22 = hashOf(v22);
    auto rth23 = hashOf(v23);
    auto rth30 = hashOf(v30);
    //NO CTFE:
    auto rth24 = hashOf(v24);
    auto rth25 = hashOf(v25);
    auto rth26 = hashOf(v26);
    auto rth27 = hashOf(v27);
    auto rth28 = hashOf(v28);

    auto rth31 = hashOf(v31);
    auto rth32 = hashOf(v32);
    auto rth33 = hashOf(v33);

    assert(h1 == rth1);
    assert(h2 == rth2);
    assert(h3 == rth3);
    assert(h4 == rth4);
    assert(h5 == rth5);
    assert(h6 == rth6);
    assert(h7 == rth7);
    assert(h8 == rth8);
    assert(h9 == rth9);
    assert(h10 == rth10);
    assert(h11 == rth11);
    assert(h12 == rth12);
    assert(h14 == rth14);
    assert(h15 == rth15);
    assert(h16 == rth16);
    assert(h17 == rth17);
    assert(h18 == rth18);
    assert(h19 == rth19);
    assert(h20 == rth20);
    assert(h21 == rth21);
    assert(h22 == rth22);
    assert(h23 == rth23);
    /*assert(h24 == rth24);
    assert(h25 == rth25);
    assert(h26 == rth26);
    assert(h27 == rth27);
    assert(h28 == rth28);*/
    assert(h30 == rth30);
    assert(h31 == rth31);
    assert(h32 == rth32);
    assert(h33 == rth33);

    // https://issues.dlang.org/show_bug.cgi?id=18932
    assert(hashOf(null, 0) != hashOf(null, 123456789));

    static size_t tiHashOf(T)(T var)
    {
        return typeid(T).getHash(&var);
    }

    auto tih1 = tiHashOf(v1);
    auto tih2 = tiHashOf(v2);
    auto tih3 = tiHashOf(v3);
    auto tih4 = tiHashOf(v4);
    auto tih5 = tiHashOf(v5);
    auto tih6 = tiHashOf(v6);
    auto tih7 = tiHashOf(v7);
    auto tih8 = tiHashOf(v8);
    auto tih9 = tiHashOf(v9);
    auto tih10 = tiHashOf(v10);
    auto tih11 = tiHashOf(v11);
    auto tih12 = tiHashOf(v12);
    auto tih14 = tiHashOf(v14);
    auto tih15 = tiHashOf(v15);
    auto tih16 = tiHashOf(v16);
    auto tih17 = tiHashOf(v17);
    auto tih18 = tiHashOf(v18);
    auto tih19 = tiHashOf(v19);
    auto tih20 = tiHashOf(v20);
    auto tih21 = tiHashOf(v21);
    auto tih22 = tiHashOf(v22);
    auto tih23 = tiHashOf(v23);
    auto tih24 = tiHashOf(v24);
    auto tih25 = tiHashOf(v25);
    auto tih26 = tiHashOf(v26);
    auto tih27 = tiHashOf(v27);
    auto tih28 = tiHashOf(v28);
    auto tih30 = tiHashOf(v30);
    auto tih31 = tiHashOf(v31);
    auto tih32 = tiHashOf(v32);
    auto tih33 = tiHashOf(v33);

    assert(tih1 == rth1);
    assert(tih2 == rth2);
    assert(tih3 == rth3);
    assert(tih4 == rth4);
    assert(tih5 == rth5);
    assert(tih6 == rth6);
    assert(tih7 == rth7);
    assert(tih8 == rth8);
    assert(tih9 == rth9);
    //assert(tih10 == rth10); // need compiler-generated __xtoHash changes
    assert(tih11 == rth11);
    assert(tih12 == rth12);
    assert(tih14 == rth14);
    assert(tih15 == rth15);
    assert(tih16 == rth16);
    assert(tih17 == rth17);
    assert(tih18 == rth18);
    //assert(tih19 == rth19); // need compiler-generated __xtoHash changes
    assert(tih20 == rth20);
    assert(tih21 == rth21);
    assert(tih22 == rth22);
    //assert(tih23 == rth23); // need compiler-generated __xtoHash changes
    //assert(tih24 == rth24);
    //assert(tih25 == rth25);
    assert(tih26 == rth26);
    assert(tih27 == rth27);
    assert(tih28 == rth28);
    assert(tih30 == rth30);
    assert(tih31 == rth31);
    assert(tih32 == rth32);
    assert(tih33 == rth33);
}
