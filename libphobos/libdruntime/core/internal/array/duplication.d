/**
The `.dup` and `.idup` properties for Associative Arrays and Dynamic Arrays

Copyright: Copyright Digital Mars 2000 - 2022.
License: Distributed under the $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
    (See accompanying file LICENSE)
Source: $(DRUNTIMESRC core/internal/_array/_duplication.d)
*/
module core.internal.array.duplication;

private extern (C) void[] _d_newarrayU(const scope TypeInfo ti, size_t length) pure nothrow;

U[] _dup(T, U)(scope T[] a) pure nothrow @trusted if (__traits(isPOD, T))
{
    if (__ctfe)
        return _dupCtfe!(T, U)(a);

    version (D_BetterC)
    {
        return _dupCtfe!(T, U)(a);
    }
    else
    {
        import core.stdc.string : memcpy;
        auto arr = _d_newarrayU(typeid(T[]), a.length);
        memcpy(arr.ptr, cast(const(void)*) a.ptr, T.sizeof * a.length);
        return *cast(U[]*) &arr;
    }
}

U[] _dupCtfe(T, U)(scope T[] a)
{
    static if (is(T : void))
        assert(0, "Cannot dup a void[] array at compile time.");
    else
    {
        U[] res;
        foreach (ref e; a)
            res ~= e;
        return res;
    }
}

U[] _dup(T, U)(T[] a) if (!__traits(isPOD, T))
{
    // note: copyEmplace is `@system` inside a `@trusted` block, so the __ctfe branch
    // has the extra duty to infer _dup `@system` when the copy-constructor is `@system`.
    if (__ctfe)
        return _dupCtfe!(T, U)(a);

    version (D_BetterC)
    {
        return _dupCtfe!(T, U)(a);
    }
    else
    {
        import core.lifetime: copyEmplace;
        U[] res = () @trusted {
            auto arr = cast(U*) _d_newarrayU(typeid(T[]), a.length);
            size_t i;
            scope (failure)
            {
                import core.internal.lifetime: emplaceInitializer;
                // Initialize all remaining elements to not destruct garbage
                foreach (j; i .. a.length)
                    emplaceInitializer(cast() arr[j]);
            }
            for (; i < a.length; i++)
            {
                copyEmplace(a.ptr[i], arr[i]);
            }
            return cast(U[])(arr[0..a.length]);
        } ();

        return res;
    }
}

// https://issues.dlang.org/show_bug.cgi?id=22107
@safe unittest
{
    static int i;
    @safe struct S
    {
        this(this) { i++; }
    }

    void fun(scope S[] values...) @safe
    {
        values.dup;
    }
}

@safe unittest
{
    static struct S1 { int* p; }
    static struct S2 { @disable this(); }
    static struct S3 { @disable this(this); }

    int dg1() pure nothrow @safe
    {
        {
           char[] m;
           string i;
           m = m.dup;
           i = i.idup;
           m = i.dup;
           i = m.idup;
        }
        {
           S1[] m;
           immutable(S1)[] i;
           m = m.dup;
           i = i.idup;
           static assert(!is(typeof(m.idup)));
           static assert(!is(typeof(i.dup)));
        }
        {
            S3[] m;
            immutable(S3)[] i;
            static assert(!is(typeof(m.dup)));
            static assert(!is(typeof(i.idup)));
        }
        {
            shared(S1)[] m;
            m = m.dup;
            static assert(!is(typeof(m.idup)));
        }
        {
            int[] a = (inout(int)) { inout(const(int))[] a; return a.dup; }(0);
        }
        return 1;
    }

    int dg2() pure nothrow @safe
    {
        {
           S2[] m = [S2.init, S2.init];
           immutable(S2)[] i = [S2.init, S2.init];
           m = m.dup;
           m = i.dup;
           i = m.idup;
           i = i.idup;
        }
        return 2;
    }

    enum a = dg1();
    enum b = dg2();
    assert(dg1() == a);
    assert(dg2() == b);
}

@system unittest
{
    static struct Sunpure { this(this) @safe nothrow {} }
    static struct Sthrow { this(this) @safe pure {} }
    static struct Sunsafe { this(this) @system pure nothrow {} }
    static struct Snocopy { @disable this(this); }

    [].dup!Sunpure;
    [].dup!Sthrow;
    cast(void) [].dup!Sunsafe;
    static assert(!__traits(compiles, () pure    { [].dup!Sunpure; }));
    static assert(!__traits(compiles, () nothrow { [].dup!Sthrow; }));
    static assert(!__traits(compiles, () @safe   { [].dup!Sunsafe; }));
    static assert(!__traits(compiles, ()         { [].dup!Snocopy; }));

    [].idup!Sunpure;
    [].idup!Sthrow;
    [].idup!Sunsafe;
    static assert(!__traits(compiles, () pure    { [].idup!Sunpure; }));
    static assert(!__traits(compiles, () nothrow { [].idup!Sthrow; }));
    static assert(!__traits(compiles, () @safe   { [].idup!Sunsafe; }));
    static assert(!__traits(compiles, ()         { [].idup!Snocopy; }));
}

@safe unittest
{
    // test that the copy-constructor is called with .dup
    static struct ArrElem
    {
        int a;
        this(int a)
        {
            this.a = a;
        }
        this(ref const ArrElem)
        {
            a = 2;
        }
        this(ref ArrElem) immutable
        {
            a = 3;
        }
    }

    auto arr = [ArrElem(1), ArrElem(1)];

    ArrElem[] b = arr.dup;
    assert(b[0].a == 2 && b[1].a == 2);

    immutable ArrElem[] c = arr.idup;
    assert(c[0].a == 3 && c[1].a == 3);
}

@system unittest
{
    static struct Sunpure { this(ref const typeof(this)) @safe nothrow {} }
    static struct Sthrow { this(ref const typeof(this)) @safe pure {} }
    static struct Sunsafe { this(ref const typeof(this)) @system pure nothrow {} }
    [].dup!Sunpure;
    [].dup!Sthrow;
    cast(void) [].dup!Sunsafe;
    static assert(!__traits(compiles, () pure    { [].dup!Sunpure; }));
    static assert(!__traits(compiles, () nothrow { [].dup!Sthrow; }));
    static assert(!__traits(compiles, () @safe   { [].dup!Sunsafe; }));

    // for idup to work on structs that have copy constructors, it is necessary
    // that the struct defines a copy constructor that creates immutable objects
    static struct ISunpure { this(ref const typeof(this)) immutable @safe nothrow {} }
    static struct ISthrow { this(ref const typeof(this)) immutable @safe pure {} }
    static struct ISunsafe { this(ref const typeof(this)) immutable @system pure nothrow {} }
    [].idup!ISunpure;
    [].idup!ISthrow;
    [].idup!ISunsafe;
    static assert(!__traits(compiles, () pure    { [].idup!ISunpure; }));
    static assert(!__traits(compiles, () nothrow { [].idup!ISthrow; }));
    static assert(!__traits(compiles, () @safe   { [].idup!ISunsafe; }));
}

@safe unittest
{
    static int*[] pureFoo() pure { return null; }
    { char[] s; immutable x = s.dup; }
    { immutable x = (cast(int*[])null).dup; }
    { immutable x = pureFoo(); }
    { immutable x = pureFoo().dup; }
}

@safe unittest
{
    auto a = [1, 2, 3];
    auto b = a.dup;
    debug(SENTINEL) {} else
        assert(b.capacity >= 3);
}

@system unittest
{
    // Bugzilla 12580
    void[] m = [0];
    shared(void)[] s = [cast(shared)1];
    immutable(void)[] i = [cast(immutable)2];

    s = s.dup;
    static assert(is(typeof(s.dup) == shared(void)[]));

    m = i.dup;
    i = m.dup;
    i = i.idup;
    i = m.idup;
    i = s.idup;
    i = s.dup;
    static assert(!__traits(compiles, m = s.dup));
}

@safe unittest
{
    // Bugzilla 13809
    static struct S
    {
        this(this) {}
        ~this() {}
    }

    S[] arr;
    auto a = arr.dup;
}

@system unittest
{
    // Bugzilla 16504
    static struct S
    {
        __gshared int* gp;
        int* p;
        // postblit and hence .dup could escape
        this(this) { gp = p; }
    }

    int p;
    scope S[1] arr = [S(&p)];
    auto a = arr.dup; // dup does escape
}

// https://issues.dlang.org/show_bug.cgi?id=21983
// dup/idup destroys partially constructed arrays on failure
@safe unittest
{
    static struct SImpl(bool postblit)
    {
        int num;
        long l = 0xDEADBEEF;

        static if (postblit)
        {
            this(this)
            {
                if (this.num == 3)
                    throw new Exception("");
            }
        }
        else
        {
            this(scope ref const SImpl other)
            {
                if (other.num == 3)
                    throw new Exception("");

                this.num = other.num;
                this.l = other.l;
            }
        }

        ~this() @trusted
        {
            if (l != 0xDEADBEEF)
            {
                import core.stdc.stdio;
                printf("Unexpected value: %lld\n", l);
                fflush(stdout);
                assert(false);
            }
        }
    }

    alias Postblit = SImpl!true;
    alias Copy = SImpl!false;

    static int test(S)()
    {
        S[4] arr = [ S(1), S(2), S(3), S(4) ];
        try
        {
            arr.dup();
            assert(false);
        }
        catch (Exception)
        {
            return 1;
        }
    }

    static assert(test!Postblit());
    assert(test!Postblit());

    static assert(test!Copy());
    assert(test!Copy());
}
