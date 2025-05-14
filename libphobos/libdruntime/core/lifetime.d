module core.lifetime;

import core.internal.attributes : betterC;

// emplace
/**
Given a pointer `chunk` to uninitialized memory (but already typed
as `T`), constructs an object of non-`class` type `T` at that
address. If `T` is a class, initializes the class reference to null.
Returns: A pointer to the newly constructed object (which is the same
as `chunk`).
 */
T* emplace(T)(T* chunk) @safe pure nothrow
{
    import core.internal.lifetime : emplaceRef;

    emplaceRef!T(*chunk);
    return chunk;
}

///
@betterC
@system unittest
{
    static struct S
    {
        int i = 42;
    }
    S[2] s2 = void;
    emplace(&s2);
    assert(s2[0].i == 42 && s2[1].i == 42);
}

///
@system unittest
{
    interface I {}
    class K : I {}

    K k = void;
    emplace(&k);
    assert(k is null);

    I i = void;
    emplace(&i);
    assert(i is null);
}

/**
Given a pointer `chunk` to uninitialized memory (but already typed
as a non-class type `T`), constructs an object of type `T` at
that address from arguments `args`. If `T` is a class, initializes
the class reference to `args[0]`.
This function can be `@trusted` if the corresponding constructor of
`T` is `@safe`.
Returns: A pointer to the newly constructed object (which is the same
as `chunk`).
 */
T* emplace(T, Args...)(T* chunk, auto ref Args args)
    if (is(T == struct) || Args.length == 1)
{
    import core.internal.lifetime : emplaceRef;

    emplaceRef!T(*chunk, forward!args);
    return chunk;
}

///
@betterC
@system unittest
{
    int a;
    int b = 42;
    assert(*emplace!int(&a, b) == 42);
}

@betterC
@system unittest
{
    shared int i;
    emplace(&i, 42);
    assert(i == 42);
}

/**
Given a raw memory area `chunk` (but already typed as a class type `T`),
constructs an object of `class` type `T` at that address. The constructor
is passed the arguments `Args`.
If `T` is an inner class whose `outer` field can be used to access an instance
of the enclosing class, then `Args` must not be empty, and the first member of it
must be a valid initializer for that `outer` field. Correct initialization of
this field is essential to access members of the outer class inside `T` methods.
Note:
This function is `@safe` if the corresponding constructor of `T` is `@safe`.
Returns: The newly constructed object.
 */
T emplace(T, Args...)(T chunk, auto ref Args args)
    if (is(T == class))
{
    import core.internal.traits : isInnerClass;

    static assert(!__traits(isAbstractClass, T), T.stringof ~
        " is abstract and it can't be emplaced");

    // Initialize the object in its pre-ctor state
    const initializer = __traits(initSymbol, T);
    () @trusted { (cast(void*) chunk)[0 .. initializer.length] = cast(void[]) initializer[]; }();

    static if (isInnerClass!T)
    {
        static assert(Args.length > 0,
            "Initializing an inner class requires a pointer to the outer class");
        static assert(is(Args[0] : typeof(T.outer)),
            "The first argument must be a pointer to the outer class");

        chunk.outer = args[0];
        alias args1 = args[1..$];
    }
    else alias args1 = args;

    // Call the ctor if any
    static if (is(typeof(chunk.__ctor(forward!args1))))
    {
        // T defines a genuine constructor accepting args
        // Go the classic route: write .init first, then call ctor
        chunk.__ctor(forward!args1);
    }
    else
    {
        static assert(args1.length == 0 && !is(typeof(&T.__ctor)),
            "Don't know how to initialize an object of type "
            ~ T.stringof ~ " with arguments " ~ typeof(args1).stringof);
    }
    return chunk;
}

///
@safe unittest
{
    () @safe {
        class SafeClass
        {
            int x;
            @safe this(int x) { this.x = x; }
        }

        auto buf = new void[__traits(classInstanceSize, SafeClass)];
        auto support = (() @trusted => cast(SafeClass)(buf.ptr))();
        auto safeClass = emplace!SafeClass(support, 5);
        assert(safeClass.x == 5);

        class UnsafeClass
        {
            int x;
            @system this(int x) { this.x = x; }
        }

        auto buf2 = new void[__traits(classInstanceSize, UnsafeClass)];
        auto support2 = (() @trusted => cast(UnsafeClass)(buf2.ptr))();
        static assert(!__traits(compiles, emplace!UnsafeClass(support2, 5)));
        static assert(!__traits(compiles, emplace!UnsafeClass(buf2, 5)));
    }();
}

@safe unittest
{
    class Outer
    {
        int i = 3;
        class Inner
        {
            @safe auto getI() { return i; }
        }
    }
    auto outerBuf = new void[__traits(classInstanceSize, Outer)];
    auto outerSupport = (() @trusted => cast(Outer)(outerBuf.ptr))();

    auto innerBuf = new void[__traits(classInstanceSize, Outer.Inner)];
    auto innerSupport = (() @trusted => cast(Outer.Inner)(innerBuf.ptr))();

    auto inner = innerSupport.emplace!(Outer.Inner)(outerSupport.emplace!Outer);
    assert(inner.getI == 3);
}

/**
Given a raw memory area `chunk`, constructs an object of `class` type `T` at
that address. The constructor is passed the arguments `Args`.
If `T` is an inner class whose `outer` field can be used to access an instance
of the enclosing class, then `Args` must not be empty, and the first member of it
must be a valid initializer for that `outer` field. Correct initialization of
this field is essential to access members of the outer class inside `T` methods.
Preconditions:
`chunk` must be at least as large as `T` needs and should have an alignment
multiple of `T`'s alignment. (The size of a `class` instance is obtained by using
$(D __traits(classInstanceSize, T))).
Note:
This function can be `@trusted` if the corresponding constructor of `T` is `@safe`.
Returns: The newly constructed object.
 */
T emplace(T, Args...)(void[] chunk, auto ref Args args)
    if (is(T == class))
{
    enum classSize = __traits(classInstanceSize, T);
    assert(chunk.length >= classSize, "chunk size too small.");

    enum alignment = __traits(classInstanceAlignment, T);
    assert((cast(size_t) chunk.ptr) % alignment == 0, "chunk is not aligned.");

    return emplace!T(cast(T)(chunk.ptr), forward!args);
}

///
@system unittest
{
    static class C
    {
        int i;
        this(int i){this.i = i;}
    }
    auto buf = new void[__traits(classInstanceSize, C)];
    auto c = emplace!C(buf, 5);
    assert(c.i == 5);
}

///
@betterC
@nogc pure nothrow @system unittest
{
    // works with -betterC too:

    static extern (C++) class C
    {
        @nogc pure nothrow @safe:
        int i = 3;
        this(int i)
        {
            assert(this.i == 3);
            this.i = i;
        }
        int virtualGetI() { return i; }
    }

    align(__traits(classInstanceAlignment, C)) byte[__traits(classInstanceSize, C)] buffer;
    C c = emplace!C(buffer[], 42);
    assert(c.virtualGetI() == 42);
}

@system unittest
{
    class Outer
    {
        int i = 3;
        class Inner
        {
            auto getI() { return i; }
        }
    }
    auto outerBuf = new void[__traits(classInstanceSize, Outer)];
    auto innerBuf = new void[__traits(classInstanceSize, Outer.Inner)];
    auto inner = innerBuf.emplace!(Outer.Inner)(outerBuf.emplace!Outer);
    assert(inner.getI == 3);
}

@nogc pure nothrow @safe unittest
{
    static class __conv_EmplaceTestClass
    {
        @nogc @safe pure nothrow:
        int i = 3;
        this(int i)
        {
            assert(this.i == 3);
            this.i = 10 + i;
        }
        this(ref int i)
        {
            assert(this.i == 3);
            this.i = 20 + i;
        }
        this(int i, ref int j)
        {
            assert(this.i == 3 && i == 5 && j == 6);
            this.i = i;
            ++j;
        }
    }

    int var = 6;
    align(__traits(classInstanceAlignment, __conv_EmplaceTestClass))
        ubyte[__traits(classInstanceSize, __conv_EmplaceTestClass)] buf;
    auto support = (() @trusted => cast(__conv_EmplaceTestClass)(buf.ptr))();

    auto fromRval = emplace!__conv_EmplaceTestClass(support, 1);
    assert(fromRval.i == 11);

    auto fromLval = emplace!__conv_EmplaceTestClass(support, var);
    assert(fromLval.i == 26);

    auto k = emplace!__conv_EmplaceTestClass(support, 5, var);
    assert(k.i == 5);
    assert(var == 7);
}

/**
Given a raw memory area `chunk`, constructs an object of non-$(D
class) type `T` at that address. The constructor is passed the
arguments `args`, if any.
Preconditions:
`chunk` must be at least as large
as `T` needs and should have an alignment multiple of `T`'s
alignment.
Note:
This function can be `@trusted` if the corresponding constructor of
`T` is `@safe`.
Returns: A pointer to the newly constructed object.
 */
T* emplace(T, Args...)(void[] chunk, auto ref Args args)
    if (!is(T == class))
{
    import core.internal.traits : Unqual;
    import core.internal.lifetime : emplaceRef;

    assert(chunk.length >= T.sizeof, "chunk size too small.");
    assert((cast(size_t) chunk.ptr) % T.alignof == 0, "emplace: Chunk is not aligned.");

    emplaceRef!(T, Unqual!T)(*cast(Unqual!T*) chunk.ptr, forward!args);
    return cast(T*) chunk.ptr;
}

///
@betterC
@system unittest
{
    struct S
    {
        int a, b;
    }
    void[S.sizeof] buf = void;
    S s;
    s.a = 42;
    s.b = 43;
    auto s1 = emplace!S(buf, s);
    assert(s1.a == 42 && s1.b == 43);
}

// Bulk of emplace unittests starts here

@betterC
@system unittest /* unions */
{
    static union U
    {
        string a;
        int b;
        struct
        {
            long c;
            int[] d;
        }
    }
    U u1 = void;
    U u2 = { "hello" };
    emplace(&u1, u2);
    assert(u1.a == "hello");
}

@system unittest // https://issues.dlang.org/show_bug.cgi?id=15772
{
    abstract class Foo {}
    class Bar: Foo {}
    void[] memory;
    // test in emplaceInitializer
    static assert(!is(typeof(emplace!Foo(cast(Foo*) memory.ptr))));
    static assert( is(typeof(emplace!Bar(cast(Bar*) memory.ptr))));
    // test in the emplace overload that takes void[]
    static assert(!is(typeof(emplace!Foo(memory))));
    static assert( is(typeof(emplace!Bar(memory))));
}

@betterC
@system unittest
{
    struct S { @disable this(); }
    S s = void;
    static assert(!__traits(compiles, emplace(&s)));
    emplace(&s, S.init);
}

@betterC
@system unittest
{
    struct S1
    {}

    struct S2
    {
        void opAssign(S2);
    }

    S1 s1 = void;
    S2 s2 = void;
    S1[2] as1 = void;
    S2[2] as2 = void;
    emplace(&s1);
    emplace(&s2);
    emplace(&as1);
    emplace(&as2);
}

@system unittest
{
    static struct S1
    {
        this(this) @disable;
    }
    static struct S2
    {
        this() @disable;
    }
    S1[2] ss1 = void;
    S2[2] ss2 = void;
    emplace(&ss1);
    static assert(!__traits(compiles, emplace(&ss2)));
    S1 s1 = S1.init;
    S2 s2 = S2.init;
    static assert(!__traits(compiles, emplace(&ss1, s1)));
    emplace(&ss2, s2);
}

@system unittest
{
    struct S
    {
        immutable int i;
    }
    S s = void;
    S[2] ss1 = void;
    S[2] ss2 = void;
    emplace(&s, 5);
    assert(s.i == 5);
    emplace(&ss1, s);
    assert(ss1[0].i == 5 && ss1[1].i == 5);
    emplace(&ss2, ss1);
    assert(ss2 == ss1);
}

//Start testing emplace-args here

@system unittest
{
    interface I {}
    class K : I {}

    K k = null, k2 = new K;
    assert(k !is k2);
    emplace!K(&k, k2);
    assert(k is k2);

    I i = null;
    assert(i !is k);
    emplace!I(&i, k);
    assert(i is k);
}

@system unittest
{
    static struct S
    {
        int i = 5;
        void opAssign(S){assert(0);}
    }
    S[2] sa = void;
    S[2] sb;
    emplace(&sa, sb);
    assert(sa[0].i == 5 && sa[1].i == 5);
}

//Start testing emplace-struct here

// Test constructor branch
@betterC
@system unittest
{
    struct S
    {
        double x = 5, y = 6;
        this(int a, int b)
        {
            assert(x == 5 && y == 6);
            x = a;
            y = b;
        }
    }

    void[S.sizeof] s1 = void;
    auto s2 = S(42, 43);
    assert(*emplace!S(cast(S*) s1.ptr, s2) == s2);
    assert(*emplace!S(cast(S*) s1, 44, 45) == S(44, 45));
}

@system unittest
{
    static struct __conv_EmplaceTest
    {
        int i = 3;
        this(int i)
        {
            assert(this.i == 3 && i == 5);
            this.i = i;
        }
        this(int i, ref int j)
        {
            assert(i == 5 && j == 6);
            this.i = i;
            ++j;
        }

    @disable:
        this();
        this(this);
        void opAssign();
    }

    __conv_EmplaceTest k = void;
    emplace(&k, 5);
    assert(k.i == 5);

    int var = 6;
    __conv_EmplaceTest x = void;
    emplace(&x, 5, var);
    assert(x.i == 5);
    assert(var == 7);

    var = 6;
    auto z = emplace!__conv_EmplaceTest(new void[__conv_EmplaceTest.sizeof], 5, var);
    assert(z.i == 5);
    assert(var == 7);
}

// Test matching fields branch
@betterC
@system unittest
{
    struct S { uint n; }
    S s;
    emplace!S(&s, 2U);
    assert(s.n == 2);
}

@betterC
@safe unittest
{
    struct S { int a, b; this(int){} }
    S s;
    static assert(!__traits(compiles, emplace!S(&s, 2, 3)));
}

@betterC
@system unittest
{
    struct S { int a, b = 7; }
    S s1 = void, s2 = void;

    emplace!S(&s1, 2);
    assert(s1.a == 2 && s1.b == 7);

    emplace!S(&s2, 2, 3);
    assert(s2.a == 2 && s2.b == 3);
}

//opAssign
@betterC
@system unittest
{
    static struct S
    {
        int i = 5;
        void opAssign(int){assert(0);}
        void opAssign(S){assert(0);}
    }
    S sa1 = void;
    S sa2 = void;
    S sb1 = S(1);
    emplace(&sa1, sb1);
    emplace(&sa2, 2);
    assert(sa1.i == 1);
    assert(sa2.i == 2);
}

//postblit precedence
@betterC
@system unittest
{
    //Works, but breaks in "-w -O" because of @@@9332@@@.
    //Uncomment test when 9332 is fixed.
    static struct S
    {
        int i;

        this(S other){assert(false);}
        this(int i){this.i = i;}
        this(this){}
    }
    S a = void;
    assert(is(typeof({S b = a;})));    //Postblit
    assert(is(typeof({S b = S(a);}))); //Constructor
    auto b = S(5);
    emplace(&a, b);
    assert(a.i == 5);

    static struct S2
    {
        int* p;
        this(const S2){}
    }
    static assert(!is(immutable S2 : S2));
    S2 s2 = void;
    immutable is2 = (immutable S2).init;
    emplace(&s2, is2);
}

//nested structs and postblit
@system unittest
{
    static struct S
    {
        int* p;
        this(int i){p = [i].ptr;}
        this(this)
        {
            if (p)
                p = [*p].ptr;
        }
    }
    static struct SS
    {
        S s;
        void opAssign(const SS)
        {
            assert(0);
        }
    }
    SS ssa = void;
    SS ssb = SS(S(5));
    emplace(&ssa, ssb);
    assert(*ssa.s.p == 5);
    assert(ssa.s.p != ssb.s.p);
}

//disabled postblit
@betterC
@system unittest
{
    static struct S1
    {
        int i;
        @disable this(this);
    }
    S1 s1 = void;
    emplace(&s1, 1);
    assert(s1.i == 1);
    static assert(!__traits(compiles, emplace(&s1, s1))); // copy disabled
    static assert(__traits(compiles, emplace(&s1, move(s1)))); // move not affected

    static struct S2
    {
        int i;
        @disable this(this);
        this(ref S2){}
    }
    S2 s2 = void;
    //static assert(!__traits(compiles, emplace(&s2, 1)));
    emplace(&s2, S2.init);

    static struct SS1
    {
        S1 s;
    }
    SS1 ss1 = void;
    emplace(&ss1);
    static assert(!__traits(compiles, emplace(&ss1, ss1))); // copying disabled
    static assert(__traits(compiles, emplace(&ss1, move(ss1)))); // move unaffected

    static struct SS2
    {
        S2 s;
    }
    SS2 ss2 = void;
    emplace(&ss2);
    static assert(!__traits(compiles, emplace(&ss2, ss2))); // copying disabled
    static assert(__traits(compiles, emplace(&ss2, SS2.init))); // move is OK


    // SS1 sss1 = s1;      //This doesn't compile
    // SS1 sss1 = SS1(s1); //This doesn't compile
    // So emplace shouldn't compile either
    static assert(!__traits(compiles, emplace(&sss1, s1)));
    static assert(!__traits(compiles, emplace(&sss2, s2)));
}

//Imutability
@betterC
@system unittest
{
    //Castable immutability
    {
        static struct S1
        {
            int i;
        }
        static assert(is( immutable(S1) : S1));
        S1 sa = void;
        auto sb = immutable(S1)(5);
        emplace(&sa, sb);
        assert(sa.i == 5);
    }
    //Un-castable immutability
    {
        static struct S2
        {
            int* p;
        }
        static assert(!is(immutable(S2) : S2));
        S2 sa = void;
        auto sb = immutable(S2)(null);
        assert(!__traits(compiles, emplace(&sa, sb)));
    }
}

@betterC
@system unittest
{
    static struct S
    {
        immutable int i;
        immutable(int)* j;
    }
    S s = void;
    emplace(&s, 1, null);
    emplace(&s, 2, &s.i);
    assert(s is S(2, &s.i));
}

//Context pointer
@system unittest
{
    int i = 0;
    {
        struct S1
        {
            void foo(){++i;}
        }
        S1 sa = void;
        S1 sb;
        emplace(&sa, sb);
        sa.foo();
        assert(i == 1);
    }
    {
        struct S2
        {
            void foo(){++i;}
            this(this){}
        }
        S2 sa = void;
        S2 sb;
        emplace(&sa, sb);
        sa.foo();
        assert(i == 2);
    }
}

//Alias this
@betterC
@system unittest
{
    static struct S
    {
        int i;
    }
    //By Ref
    {
        static struct SS1
        {
            int j;
            S s;
            alias s this;
        }
        S s = void;
        SS1 ss = SS1(1, S(2));
        emplace(&s, ss);
        assert(s.i == 2);
    }
    //By Value
    {
        static struct SS2
        {
            int j;
            S s;
            S foo() @property{return s;}
            alias foo this;
        }
        S s = void;
        SS2 ss = SS2(1, S(2));
        emplace(&s, ss);
        assert(s.i == 2);
    }
}

version (CoreUnittest)
{
    //Ambiguity
    private struct __std_conv_S
    {
        int i;
        this(__std_conv_SS ss)         {assert(0);}
        static opCall(__std_conv_SS ss)
        {
            __std_conv_S s; s.i = ss.j;
            return s;
        }
    }
    private struct __std_conv_SS
    {
        int j;
        __std_conv_S s;
        ref __std_conv_S foo() return @property {s.i = j; return s;}
        alias foo this;
    }
}

@system unittest
{
    static assert(is(__std_conv_SS : __std_conv_S));
    __std_conv_S s = void;
    __std_conv_SS ss = __std_conv_SS(1);

    __std_conv_S sTest1 = ss; //this calls "SS alias this" (and not "S.this(SS)")
    emplace(&s, ss); //"alias this" should take precedence in emplace over "opCall"
    assert(s.i == 1);
}

//Nested classes
@system unittest
{
    class A{}
    static struct S
    {
        A a;
    }
    S s1 = void;
    S s2 = S(new A);
    emplace(&s1, s2);
    assert(s1.a is s2.a);
}

//safety & nothrow & CTFE
@betterC
@system unittest
{
    //emplace should be safe for anything with no elaborate opassign
    static struct S1
    {
        int i;
    }
    static struct S2
    {
        int i;
        this(int j)@safe nothrow{i = j;}
    }

    int i;
    S1 s1 = void;
    S2 s2 = void;

    auto pi = &i;
    auto ps1 = &s1;
    auto ps2 = &s2;

    void foo() @safe nothrow
    {
        emplace(pi);
        emplace(pi, 5);
        emplace(ps1);
        emplace(ps1, 5);
        emplace(ps1, S1.init);
        emplace(ps2);
        emplace(ps2, 5);
        emplace(ps2, S2.init);
    }
    foo();

    T bar(T)() @property
    {
        T t/+ = void+/; //CTFE void illegal
        emplace(&t, 5);
        return t;
    }
    // CTFE
    enum a = bar!int;
    static assert(a == 5);
    enum b = bar!S1;
    static assert(b.i == 5);
    enum c = bar!S2;
    static assert(c.i == 5);
    // runtime
    auto aa = bar!int;
    assert(aa == 5);
    auto bb = bar!S1;
    assert(bb.i == 5);
    auto cc = bar!S2;
    assert(cc.i == 5);
}

@betterC
@system unittest
{
    struct S
    {
        int[2] get(){return [1, 2];}
        alias get this;
    }
    struct SS
    {
        int[2] ii;
    }
    struct ISS
    {
        int[2] ii;
    }
    S s;
    SS ss = void;
    ISS iss = void;
    emplace(&ss, s);
    emplace(&iss, s);
    assert(ss.ii == [1, 2]);
    assert(iss.ii == [1, 2]);
}

//disable opAssign
@betterC
@system unittest
{
    static struct S
    {
        @disable void opAssign(S);
    }
    S s;
    emplace(&s, S.init);
}

//opCall
@betterC
@system unittest
{
    int i;
    //Without constructor
    {
        static struct S1
        {
            int i;
            static S1 opCall(int*){assert(0);}
        }
        S1 s = void;
        static assert(!__traits(compiles, emplace(&s,  1)));
    }
    //With constructor
    {
        static struct S2
        {
            int i = 0;
            static S2 opCall(int*){assert(0);}
            static S2 opCall(int){assert(0);}
            this(int i){this.i = i;}
        }
        S2 s = void;
        emplace(&s,  1);
        assert(s.i == 1);
    }
    //With postblit ambiguity
    {
        static struct S3
        {
            int i = 0;
            static S3 opCall(ref S3){assert(0);}
        }
        S3 s = void;
        emplace(&s, S3.init);
    }
}

//static arrays
@system unittest
{
    static struct S
    {
        int[2] ii;
    }
    static struct IS
    {
        immutable int[2] ii;
    }
    int[2] ii;
    S  s   = void;
    IS ims = void;
    ubyte ub = 2;
    emplace(&s, ub);
    emplace(&s, ii);
    emplace(&ims, ub);
    emplace(&ims, ii);
    uint[2] uu;
    static assert(!__traits(compiles, {S ss = S(uu);}));
    static assert(!__traits(compiles, emplace(&s, uu)));
}

@system unittest
{
    int[2]  sii;
    int[2]  sii2;
    uint[2] uii;
    uint[2] uii2;
    emplace(&sii, 1);
    emplace(&sii, 1U);
    emplace(&uii, 1);
    emplace(&uii, 1U);
    emplace(&sii, sii2);
    //emplace(&sii, uii2); //Sorry, this implementation doesn't know how to...
    //emplace(&uii, sii2); //Sorry, this implementation doesn't know how to...
    emplace(&uii, uii2);
    emplace(&sii, sii2[]);
    //emplace(&sii, uii2[]); //Sorry, this implementation doesn't know how to...
    //emplace(&uii, sii2[]); //Sorry, this implementation doesn't know how to...
    emplace(&uii, uii2[]);
}

@system unittest
{
    bool allowDestruction = false;
    struct S
    {
        int i;
        this(this){}
        ~this(){assert(allowDestruction);}
    }
    S s = S(1);
    S[2] ss1 = void;
    S[2] ss2 = void;
    S[2] ss3 = void;
    emplace(&ss1, s);
    emplace(&ss2, ss1);
    emplace(&ss3, ss2[]);
    assert(ss1[1] == s);
    assert(ss2[1] == s);
    assert(ss3[1] == s);
    allowDestruction = true;
}

@system unittest
{
    //Checks postblit, construction, and context pointer
    int count = 0;
    struct S
    {
        this(this)
        {
            ++count;
        }
        ~this()
        {
            --count;
        }
    }

    S s;
    {
        S[4] ss = void;
        emplace(&ss, s);
        assert(count == 4);
    }
    assert(count == 0);
}

@system unittest
{
    struct S
    {
        int i;
    }
    S s;
    S[2][2][2] sss = void;
    emplace(&sss, s);
}

@system unittest //Constness
{
    import core.internal.lifetime : emplaceRef;

    int a = void;
    emplaceRef!(const int)(a, 5);

    immutable i = 5;
    const(int)* p = void;
    emplaceRef!(const int*)(p, &i);

    struct S
    {
        int* p;
    }
    alias IS = immutable(S);
    S s = void;
    emplaceRef!IS(s, IS());
    S[2] ss = void;
    emplaceRef!(IS[2])(ss, IS());

    IS[2] iss = IS.init;
    emplaceRef!(IS[2])(ss, iss);
    emplaceRef!(IS[2])(ss, iss[]);
}

@betterC
pure nothrow @safe @nogc unittest
{
    import core.internal.lifetime : emplaceRef;

    int i;
    emplaceRef(i);
    emplaceRef!int(i);
    emplaceRef(i, 5);
    emplaceRef!int(i, 5);
}

// Test attribute propagation for UDTs
pure nothrow @safe /* @nogc */ unittest
{
    import core.internal.lifetime : emplaceRef;

    static struct Safe
    {
        this(this) pure nothrow @safe @nogc {}
    }

    Safe safe = void;
    emplaceRef(safe, Safe());

    Safe[1] safeArr = [Safe()];
    Safe[1] uninitializedSafeArr = void;
    emplaceRef(uninitializedSafeArr, safe);
    emplaceRef(uninitializedSafeArr, safeArr);

    static struct Unsafe
    {
        this(this) @system {}
    }

    Unsafe unsafe = void;
    static assert(!__traits(compiles, emplaceRef(unsafe, unsafe)));

    Unsafe[1] unsafeArr = [Unsafe()];
    Unsafe[1] uninitializedUnsafeArr = void;
    static assert(!__traits(compiles, emplaceRef(uninitializedUnsafeArr, unsafe)));
    static assert(!__traits(compiles, emplaceRef(uninitializedUnsafeArr, unsafeArr)));
}

@betterC
@system unittest
{
    // Issue 15313
    static struct Node
    {
        int payload;
        Node* next;
        uint refs;
    }

    import core.stdc.stdlib : malloc;
    void[] buf = malloc(Node.sizeof)[0 .. Node.sizeof];

    const Node* n = emplace!(const Node)(buf, 42, null, 10);
    assert(n.payload == 42);
    assert(n.next == null);
    assert(n.refs == 10);
}

@system unittest
{
    class A
    {
        int x = 5;
        int y = 42;
        this(int z)
        {
            assert(x == 5 && y == 42);
            x = y = z;
        }
    }
    void[] buf;

    static align(__traits(classInstanceAlignment, A)) byte[__traits(classInstanceSize, A)] sbuf;
    buf = sbuf[];
    auto a = emplace!A(buf, 55);
    assert(a.x == 55 && a.y == 55);

    // emplace in bigger buffer
    buf = new byte[](__traits(classInstanceSize, A) + 10);
    a = emplace!A(buf, 55);
    assert(a.x == 55 && a.y == 55);

    // need ctor args
    static assert(!is(typeof(emplace!A(buf))));
}

//constructor arguments forwarding
@betterC
@system unittest
{
    static struct S
    {
        this()(auto ref long arg)
        {
            // assert that arg is an lvalue
            static assert(__traits(isRef, arg));
        }
        this()(auto ref double arg)
            // assert that arg is an rvalue
        {
            static assert(!__traits(isRef, arg));
        }
    }
    S obj = void;
    long i;
    emplace(&obj, i);   // lvalue
    emplace(&obj, 0.0); // rvalue
}
// Bulk of emplace unittests ends here

/**
 * Emplaces a copy of the specified source value into uninitialized memory,
 * i.e., simulates `T target = source` copy-construction for cases where the
 * target memory is already allocated and to be initialized with a copy.
 *
 * Params:
 *   source = value to be copied into target
 *   target = uninitialized value to be initialized with a copy of source
 */
void copyEmplace(S, T)(ref S source, ref T target) @system
    if (is(immutable S == immutable T))
{
    import core.internal.traits : BaseElemOf, hasElaborateCopyConstructor, Unconst, Unqual;

    // cannot have the following as simple template constraint due to nested-struct special case...
    static if (!__traits(compiles, (ref S src) { T tgt = src; }))
    {
        alias B = BaseElemOf!T;
        enum isNestedStruct = is(B == struct) && __traits(isNested, B);
        static assert(isNestedStruct, "cannot copy-construct " ~ T.stringof ~ " from " ~ S.stringof);
    }

    void blit()
    {
        import core.stdc.string : memcpy;
        memcpy(cast(Unqual!(T)*) &target, cast(Unqual!(T)*) &source, T.sizeof);
    }

    static if (is(T == struct))
    {
        static if (__traits(hasPostblit, T))
        {
            blit();
            (cast() target).__xpostblit();
        }
        else static if (__traits(hasCopyConstructor, T))
        {
            // https://issues.dlang.org/show_bug.cgi?id=22766
            import core.internal.lifetime : emplaceInitializer;
            emplaceInitializer(*(cast(Unqual!T*)&target));
            static if (__traits(isNested, T))
            {
                 // copy context pointer
                *(cast(void**) &target.tupleof[$-1]) = cast(void*) source.tupleof[$-1];
            }
            target.__ctor(source); // invoke copy ctor
        }
        else
        {
            blit(); // no opAssign
        }
    }
    else static if (is(T == E[n], E, size_t n))
    {
        static if (hasElaborateCopyConstructor!E)
        {
            size_t i;
            try
            {
                for (i = 0; i < n; i++)
                    copyEmplace(source[i], target[i]);
            }
            catch (Exception e)
            {
                // destroy, in reverse order, what we've constructed so far
                while (i--)
                    destroy(*cast(Unconst!(E)*) &target[i]);
                throw e;
            }
        }
        else // trivial copy
        {
            blit(); // all elements at once
        }
    }
    else
    {
        *cast(Unconst!(T)*) &target = *cast(Unconst!(T)*) &source;
    }
}

///
@betterC
@system pure nothrow @nogc unittest
{
    int source = 123;
    int target = void;
    copyEmplace(source, target);
    assert(target == 123);
}

///
@betterC
@system pure nothrow @nogc unittest
{
    immutable int[1][1] source = [ [123] ];
    immutable int[1][1] target = void;
    copyEmplace(source, target);
    assert(target[0][0] == 123);
}

///
@betterC
@system pure nothrow @nogc unittest
{
    struct S
    {
        int x;
        void opAssign(const scope ref S rhs) @safe pure nothrow @nogc
        {
            assert(0);
        }
    }

    S source = S(42);
    S target = void;
    copyEmplace(source, target);
    assert(target.x == 42);
}

// preserve shared-ness
@system pure nothrow unittest
{
    auto s = new Object();
    auto ss = new shared Object();

    Object t;
    shared Object st;

    copyEmplace(s, t);
    assert(t is s);

    copyEmplace(ss, st);
    assert(st is ss);

    static assert(!__traits(compiles, copyEmplace(s, st)));
    static assert(!__traits(compiles, copyEmplace(ss, t)));
}

// https://issues.dlang.org/show_bug.cgi?id=22766
@system pure nothrow @nogc unittest
{
    static struct S
    {
        @disable this();
        this(int) @safe pure nothrow @nogc{}
        this(ref const(S) other) @safe pure nothrow @nogc {}
    }

    S s1 = S(1);
    S s2 = void;
    copyEmplace(s1, s2);
    assert(s2 == S(1));
}

version (DigitalMars) version (X86) version (Posix) version = DMD_X86_Posix;

// don't violate immutability for reference types
@system pure nothrow unittest
{
    auto s = new Object();
    auto si = new immutable Object();

    Object t;
    immutable Object ti;

    copyEmplace(s, t);
    assert(t is s);

    copyEmplace(si, ti);
    version (DMD_X86_Posix) { /* wrongly fails without -O */ } else
        assert(ti is si);

    static assert(!__traits(compiles, copyEmplace(s, ti)));
    static assert(!__traits(compiles, copyEmplace(si, t)));
}

version (CoreUnittest)
{
    private void testCopyEmplace(S, T)(const scope T* expected = null)
    {
        S source;
        T target = void;
        copyEmplace(source, target);
        if (expected)
            assert(target == *expected);
        else
        {
            T expectedCopy = source;
            assert(target == expectedCopy);
        }
    }
}

// postblit
@system pure nothrow @nogc unittest
{
    static struct S
    {
        @safe pure nothrow @nogc:
        int x = 42;
        this(this) { x += 10; }
    }

    testCopyEmplace!(S, S)();
    testCopyEmplace!(immutable S, S)();
    testCopyEmplace!(S, immutable S)();
    testCopyEmplace!(immutable S, immutable S)();

    testCopyEmplace!(S[1], S[1])();
    testCopyEmplace!(immutable S[1], S[1])();

    // copying to an immutable static array works, but `T expected = source`
    // wrongly ignores the postblit: https://issues.dlang.org/show_bug.cgi?id=8950
    immutable S[1] expectedImmutable = [S(52)];
    testCopyEmplace!(S[1], immutable S[1])(&expectedImmutable);
    testCopyEmplace!(immutable S[1], immutable S[1])(&expectedImmutable);
}

// copy constructors
@system pure nothrow @nogc unittest
{
    static struct S
    {
        @safe pure nothrow @nogc:
        int x = 42;
        this(int x) { this.x = x; }
        this(const scope ref S rhs) { x = rhs.x + 10; }
        this(const scope ref S rhs) immutable { x = rhs.x + 20; }
    }

    testCopyEmplace!(S, S)();
    testCopyEmplace!(immutable S, S)();
    testCopyEmplace!(S, immutable S)();
    testCopyEmplace!(immutable S, immutable S)();

    // static arrays work, but `T expected = source` wrongly ignores copy ctors
    // https://issues.dlang.org/show_bug.cgi?id=20365
    S[1] expectedMutable = [S(52)];
    immutable S[1] expectedImmutable = [immutable S(62)];
    testCopyEmplace!(S[1], S[1])(&expectedMutable);
    testCopyEmplace!(immutable S[1], S[1])(&expectedMutable);
    testCopyEmplace!(S[1], immutable S[1])(&expectedImmutable);
    testCopyEmplace!(immutable S[1], immutable S[1])(&expectedImmutable);
}

// copy constructor in nested struct
@system pure nothrow unittest
{
    int copies;
    struct S
    {
        @safe pure nothrow @nogc:
        size_t x = 42;
        this(size_t x) { this.x = x; }
        this(const scope ref S rhs)
        {
            assert(x == 42); // T.init
            x = rhs.x;
            ++copies;
        }
    }

    {
        copies = 0;
        S source = S(123);
        immutable S target = void;
        copyEmplace(source, target);
        assert(target is source);
        assert(copies == 1);
    }

    {
        copies = 0;
        immutable S[1] source = [immutable S(456)];
        S[1] target = void;
        copyEmplace(source, target);
        assert(target[0] is source[0]);
        assert(copies == 1);
    }
}

// destruction of partially copied static array
@system unittest
{
    static struct S
    {
        __gshared int[] deletions;
        int x;
        this(this) { if (x == 5) throw new Exception(""); }
        ~this() { deletions ~= x; }
    }

    alias T = immutable S[3][2];
    T source = [ [S(1), S(2), S(3)], [S(4), S(5), S(6)] ];
    T target = void;
    try
    {
        copyEmplace(source, target);
        assert(0);
    }
    catch (Exception)
    {
        static immutable expectedDeletions = [ 4, 3, 2, 1 ];
        version (DigitalMars)
        {
            assert(S.deletions == expectedDeletions ||
                   S.deletions == [ 4 ]); // FIXME: happens with -O
        }
        else
            assert(S.deletions == expectedDeletions);
    }
}

/**
Forwards function arguments while keeping `out`, `ref`, and `lazy` on
the parameters.

Params:
    args = a parameter list or an $(REF AliasSeq,std,meta).
Returns:
    An `AliasSeq` of `args` with `out`, `ref`, and `lazy` saved.
*/
template forward(args...)
{
    import core.internal.traits : AliasSeq;

    template fwd(alias arg)
    {
        // by ref || lazy || const/immutable
        static if (__traits(isRef,  arg) ||
                   __traits(isOut,  arg) ||
                   __traits(isLazy, arg) ||
                   !is(typeof(move(arg))))
            alias fwd = arg;
        // (r)value
        else
            @property auto fwd()
            {
                version (DigitalMars) { /* @@BUG 23890@@ */ } else pragma(inline, true);
                return move(arg);
            }
    }

    alias Result = AliasSeq!();
    static foreach (arg; args)
        Result = AliasSeq!(Result, fwd!arg);
    static if (Result.length == 1)
        alias forward = Result[0];
    else
        alias forward = Result;
}

///
@safe unittest
{
    class C
    {
        static int foo(int n) { return 1; }
        static int foo(ref int n) { return 2; }
    }

    // with forward
    int bar()(auto ref int x) { return C.foo(forward!x); }

    // without forward
    int baz()(auto ref int x) { return C.foo(x); }

    int i;
    assert(bar(1) == 1);
    assert(bar(i) == 2);

    assert(baz(1) == 2);
    assert(baz(i) == 2);
}

///
@safe unittest
{
    void foo(int n, ref string s) { s = null; foreach (i; 0 .. n) s ~= "Hello"; }

    // forwards all arguments which are bound to parameter tuple
    void bar(Args...)(auto ref Args args) { return foo(forward!args); }

    // forwards all arguments with swapping order
    void baz(Args...)(auto ref Args args) { return foo(forward!args[$/2..$], forward!args[0..$/2]); }

    string s;
    bar(1, s);
    assert(s == "Hello");
    baz(s, 2);
    assert(s == "HelloHello");
}

@safe unittest
{
    auto foo(TL...)(auto ref TL args)
    {
        string result = "";
        foreach (i, _; args)
        {
            //pragma(msg, "[",i,"] ", __traits(isRef, args[i]) ? "L" : "R");
            result ~= __traits(isRef, args[i]) ? "L" : "R";
        }
        return result;
    }

    string bar(TL...)(auto ref TL args)
    {
        return foo(forward!args);
    }
    string baz(TL...)(auto ref TL args)
    {
        int x;
        return foo(forward!args[3], forward!args[2], 1, forward!args[1], forward!args[0], x);
    }

    struct S {}
    S makeS(){ return S(); }
    int n;
    string s;
    assert(bar(S(), makeS(), n, s) == "RRLL");
    assert(baz(S(), makeS(), n, s) == "LLRRRL");
}

@betterC
@safe unittest
{
    ref int foo(return ref int a) { return a; }
    ref int bar(Args)(auto ref Args args)
    {
        return foo(forward!args);
    }
    static assert(!__traits(compiles, { auto x1 = bar(3); })); // case of NG
    int value = 3;
    auto x2 = bar(value); // case of OK
}

///
@betterC
@safe unittest
{
    struct X {
        int i;
        this(this)
        {
            ++i;
        }
    }

    struct Y
    {
        private X x_;
        this()(auto ref X x)
        {
            x_ = forward!x;
        }
    }

    struct Z
    {
        private const X x_;
        this()(auto ref X x)
        {
            x_ = forward!x;
        }
        this()(auto ref const X x)
        {
            x_ = forward!x;
        }
    }

    X x;
    const X cx;
    auto constX = (){ const X x; return x; };
    static assert(__traits(compiles, { Y y = x; }));
    static assert(__traits(compiles, { Y y = X(); }));
    static assert(!__traits(compiles, { Y y = cx; }));
    static assert(!__traits(compiles, { Y y = constX(); }));
    static assert(__traits(compiles, { Z z = x; }));
    static assert(__traits(compiles, { Z z = X(); }));
    static assert(__traits(compiles, { Z z = cx; }));
    static assert(__traits(compiles, { Z z = constX(); }));


    Y y1 = x;
    // ref lvalue, copy
    assert(y1.x_.i == 1);
    Y y2 = X();
    // rvalue, move
    assert(y2.x_.i == 0);

    Z z1 = x;
    // ref lvalue, copy
    assert(z1.x_.i == 1);
    Z z2 = X();
    // rvalue, move
    assert(z2.x_.i == 0);
    Z z3 = cx;
    // ref const lvalue, copy
    assert(z3.x_.i == 1);
    Z z4 = constX();
    // const rvalue, copy
    assert(z4.x_.i == 1);
}

// lazy -> lazy
@betterC
@safe unittest
{
    int foo1(lazy int i) { return i; }
    int foo2(A)(auto ref A i) { return foo1(forward!i); }
    int foo3(lazy int i) { return foo2(i); }

    int numCalls = 0;
    assert(foo3({ ++numCalls; return 42; }()) == 42);
    assert(numCalls == 1);
}

// lazy -> non-lazy
@betterC
@safe unittest
{
    int foo1(int a, int b) { return a + b; }
    int foo2(A...)(auto ref A args) { return foo1(forward!args); }
    int foo3(int a, lazy int b) { return foo2(a, b); }

    int numCalls;
    assert(foo3(11, { ++numCalls; return 31; }()) == 42);
    assert(numCalls == 1);
}

// non-lazy -> lazy
@betterC
@safe unittest
{
    int foo1(int a, lazy int b) { return a + b; }
    int foo2(A...)(auto ref A args) { return foo1(forward!args); }
    int foo3(int a, int b) { return foo2(a, b); }

    assert(foo3(11, 31) == 42);
}

// out
@betterC
@safe unittest
{
    void foo1(int a, out int b) { b = a; }
    void foo2(A...)(auto ref A args) { foo1(forward!args); }
    void foo3(int a, out int b) { foo2(a, b); }

    int b;
    foo3(42, b);
    assert(b == 42);
}

// move
/**
Moves `source` into `target`, via a destructive copy when necessary.

If `T` is a struct with a destructor or postblit defined, source is reset
to its `.init` value after it is moved into target, otherwise it is
left unchanged.

Preconditions:
If source has internal pointers that point to itself and doesn't define
opPostMove, it cannot be moved, and will trigger an assertion failure.

Params:
    source = Data to copy.
    target = Where to copy into. The destructor, if any, is invoked before the
        copy is performed.
*/
void move(T)(ref T source, ref T target)
{
    moveImpl(target, source);
}

/// For non-struct types, `move` just performs `target = source`:
@safe unittest
{
    Object obj1 = new Object;
    Object obj2 = obj1;
    Object obj3;

    move(obj2, obj3);
    assert(obj3 is obj1);
    // obj2 unchanged
    assert(obj2 is obj1);
}

///
pure nothrow @safe @nogc unittest
{
    // Structs without destructors are simply copied
    struct S1
    {
        int a = 1;
        int b = 2;
    }
    S1 s11 = { 10, 11 };
    S1 s12;

    move(s11, s12);

    assert(s12 == S1(10, 11));
    assert(s11 == s12);

    // But structs with destructors or postblits are reset to their .init value
    // after copying to the target.
    struct S2
    {
        int a = 1;
        int b = 2;

        ~this() pure nothrow @safe @nogc { }
    }
    S2 s21 = { 3, 4 };
    S2 s22;

    move(s21, s22);

    assert(s21 == S2(1, 2));
    assert(s22 == S2(3, 4));
}

@safe unittest
{
    import core.internal.traits;

    assertCTFEable!((){
        Object obj1 = new Object;
        Object obj2 = obj1;
        Object obj3;
        move(obj2, obj3);
        assert(obj3 is obj1);

        static struct S1 { int a = 1, b = 2; }
        S1 s11 = { 10, 11 };
        S1 s12;
        move(s11, s12);
        assert(s11.a == 10 && s11.b == 11 && s12.a == 10 && s12.b == 11);

        static struct S2 { int a = 1; int * b; }
        S2 s21 = { 10, null };
        s21.b = new int;
        S2 s22;
        move(s21, s22);
        assert(s21 == s22);
    });
    // Issue 5661 test(1)
    static struct S3
    {
        static struct X { int n = 0; ~this(){n = 0;} }
        X x;
    }
    static assert(hasElaborateDestructor!S3);
    S3 s31, s32;
    s31.x.n = 1;
    move(s31, s32);
    assert(s31.x.n == 0);
    assert(s32.x.n == 1);

    // Issue 5661 test(2)
    static struct S4
    {
        static struct X { int n = 0; this(this){n = 0;} }
        X x;
    }
    static assert(hasElaborateCopyConstructor!S4);
    S4 s41, s42;
    s41.x.n = 1;
    move(s41, s42);
    assert(s41.x.n == 0);
    assert(s42.x.n == 1);

    // Issue 13990 test
    class S5;

    S5 s51;
    S5 s52 = s51;
    S5 s53;
    move(s52, s53);
    assert(s53 is s51);
}

/// Ditto
T move(T)(return scope ref T source)
{
    return moveImpl(source);
}

/// Non-copyable structs can still be moved:
pure nothrow @safe @nogc unittest
{
    struct S
    {
        int a = 1;
        @disable this(this);
        ~this() pure nothrow @safe @nogc {}
    }
    S s1;
    s1.a = 2;
    S s2 = move(s1);
    assert(s1.a == 1);
    assert(s2.a == 2);
}

// https://issues.dlang.org/show_bug.cgi?id=20869
// `move` should propagate the attributes of `opPostMove`
@system unittest
{
    static struct S
    {
        void opPostMove(const ref S old) nothrow @system
        {
            __gshared int i;
            new int(i++); // Force @gc impure @system
        }
    }

    alias T = void function() @system nothrow;
    static assert(is(typeof({ S s; move(s); }) == T));
    static assert(is(typeof({ S s; move(s, s); }) == T));
}

private void moveImpl(T)(scope ref T target, return scope ref T source)
{
    import core.internal.traits : hasElaborateDestructor;

    static if (is(T == struct))
    {
        //  Unsafe when compiling without -preview=dip1000
        if ((() @trusted => &source == &target)()) return;
        // Destroy target before overwriting it
        static if (hasElaborateDestructor!T) target.__xdtor();
    }
    // move and emplace source into target
    moveEmplaceImpl(target, source);
}

private T moveImpl(T)(return scope ref T source)
{
    // Properly infer safety from moveEmplaceImpl as the implementation below
    // might void-initialize pointers in result and hence needs to be @trusted
    if (false) moveEmplaceImpl(source, source);

    return trustedMoveImpl(source);
}

private T trustedMoveImpl(T)(return scope ref T source) @trusted
{
    T result = void;
    moveEmplaceImpl(result, source);
    return result;
}

@safe unittest
{
    import core.internal.traits;

    assertCTFEable!((){
        Object obj1 = new Object;
        Object obj2 = obj1;
        Object obj3 = move(obj2);
        assert(obj3 is obj1);

        static struct S1 { int a = 1, b = 2; }
        S1 s11 = { 10, 11 };
        S1 s12 = move(s11);
        assert(s11.a == 10 && s11.b == 11 && s12.a == 10 && s12.b == 11);

        static struct S2 { int a = 1; int * b; }
        S2 s21 = { 10, null };
        s21.b = new int;
        S2 s22 = move(s21);
        assert(s21 == s22);
    });

    // Issue 5661 test(1)
    static struct S3
    {
        static struct X { int n = 0; ~this(){n = 0;} }
        X x;
    }
    static assert(hasElaborateDestructor!S3);
    S3 s31;
    s31.x.n = 1;
    S3 s32 = move(s31);
    assert(s31.x.n == 0);
    assert(s32.x.n == 1);

    // Issue 5661 test(2)
    static struct S4
    {
        static struct X { int n = 0; this(this){n = 0;} }
        X x;
    }
    static assert(hasElaborateCopyConstructor!S4);
    S4 s41;
    s41.x.n = 1;
    S4 s42 = move(s41);
    assert(s41.x.n == 0);
    assert(s42.x.n == 1);

    // Issue 13990 test
    class S5;

    S5 s51;
    S5 s52 = s51;
    S5 s53;
    s53 = move(s52);
    assert(s53 is s51);
}

@betterC
@system unittest
{
    static struct S { int n = 0; ~this() @system { n = 0; } }
    S a, b;
    static assert(!__traits(compiles, () @safe { move(a, b); }));
    static assert(!__traits(compiles, () @safe { move(a); }));
    a.n = 1;
    () @trusted { move(a, b); }();
    assert(a.n == 0);
    a.n = 1;
    () @trusted { move(a); }();
    assert(a.n == 0);
}
/+ this can't be tested in druntime, tests are still run in phobos
@safe unittest//Issue 6217
{
    import std.algorithm.iteration : map;
    auto x = map!"a"([1,2,3]);
    x = move(x);
}
+/
@betterC
@safe unittest// Issue 8055
{
    static struct S
    {
        int x;
        ~this()
        {
            assert(x == 0);
        }
    }
    S foo(S s)
    {
        return move(s);
    }
    S a;
    a.x = 0;
    auto b = foo(a);
    assert(b.x == 0);
}

@system unittest// Issue 8057
{
    int n = 10;
    struct S
    {
        int x;
        ~this()
        {
            // Access to enclosing scope
            assert(n == 10);
        }
    }
    S foo(S s)
    {
        // Move nested struct
        return move(s);
    }
    S a;
    a.x = 1;
    auto b = foo(a);
    assert(b.x == 1);

    // Regression 8171
    static struct Array(T)
    {
        // nested struct has no member
        struct Payload
        {
            ~this() {}
        }
    }
    Array!int.Payload x = void;
    move(x);
    move(x, x);
}

private enum bool hasContextPointers(T) = {
    static if (__traits(isStaticArray, T))
    {
        return hasContextPointers!(typeof(T.init[0]));
    }
    else static if (is(T == struct))
    {
        import core.internal.traits : anySatisfy;
        return __traits(isNested, T) || anySatisfy!(hasContextPointers, typeof(T.tupleof));
    }
    else return false;
} ();

@safe @nogc nothrow pure unittest
{
    static assert(!hasContextPointers!int);
    static assert(!hasContextPointers!(void*));

    static struct S {}
    static assert(!hasContextPointers!S);
    static assert(!hasContextPointers!(S[1]));

    struct Nested
    {
        void foo() {}
    }

    static assert(hasContextPointers!Nested);
    static assert(hasContextPointers!(Nested[1]));

    static struct OneLevel
    {
        int before;
        Nested n;
        int after;
    }

    static assert(hasContextPointers!OneLevel);
    static assert(hasContextPointers!(OneLevel[1]));

    static struct TwoLevels
    {
        int before;
        OneLevel o;
        int after;
    }

    static assert(hasContextPointers!TwoLevels);
    static assert(hasContextPointers!(TwoLevels[1]));

    union U
    {
        Nested n;
    }

    // unions can have false positives, so this query ignores them
    static assert(!hasContextPointers!U);
}

// target must be first-parameter, because in void-functions DMD + dip1000 allows it to take the place of a return-scope
private void moveEmplaceImpl(T)(scope ref T target, return scope ref T source)
{
    // TODO: this assert pulls in half of phobos. we need to work out an alternative assert strategy.
//    static if (!is(T == class) && hasAliasing!T) if (!__ctfe)
//    {
//        import std.exception : doesPointTo;
//        assert(!doesPointTo(source, source) && !hasElaborateMove!T),
//              "Cannot move object with internal pointer unless `opPostMove` is defined.");
//    }

    import core.internal.traits : hasElaborateAssign, isAssignable, hasElaborateMove,
                                  hasElaborateDestructor, hasElaborateCopyConstructor;
    static if (is(T == struct))
    {

        //  Unsafe when compiling without -preview=dip1000
        assert((() @trusted => &source !is &target)(), "source and target must not be identical");

        static if (hasElaborateAssign!T || !isAssignable!T)
        {
            import core.stdc.string : memcpy;
            () @trusted { memcpy(&target, &source, T.sizeof); }();
        }
        else
            target = source;

        static if (hasElaborateMove!T)
            __move_post_blt(target, source);

        // If the source defines a destructor or a postblit hook, we must obliterate the
        // object in order to avoid double freeing and undue aliasing
        static if (hasElaborateDestructor!T || hasElaborateCopyConstructor!T)
        {
            // If there are members that are nested structs, we must take care
            // not to erase any context pointers, so we might have to recurse
            static if (__traits(isZeroInit, T))
                wipe(source);
            else
                wipe(source, ref () @trusted { return *cast(immutable(T)*) __traits(initSymbol, T).ptr; } ());
        }
    }
    else static if (__traits(isStaticArray, T))
    {
        static if (T.length)
        {
            static if (!hasElaborateMove!T &&
                       !hasElaborateDestructor!T &&
                       !hasElaborateCopyConstructor!T)
            {
                // Single blit if no special per-instance handling is required
                () @trusted
                {
                    assert(source.ptr !is target.ptr, "source and target must not be identical");
                    *cast(ubyte[T.sizeof]*) &target = *cast(ubyte[T.sizeof]*) &source;
                } ();
            }
            else
            {
                for (size_t i = 0; i < source.length; ++i)
                    moveEmplaceImpl(target[i], source[i]);
            }
        }
    }
    else
    {
        // Primitive data (including pointers and arrays) or class -
        // assignment works great
        target = source;
    }
}

/**
 * Similar to $(LREF move) but assumes `target` is uninitialized. This
 * is more efficient because `source` can be blitted over `target`
 * without destroying or initializing it first.
 *
 * Params:
 *   source = value to be moved into target
 *   target = uninitialized value to be filled by source
 */
void moveEmplace(T)(ref T source, ref T target) @system
{
    moveEmplaceImpl(target, source);
}

///
@betterC
pure nothrow @nogc @system unittest
{
    static struct Foo
    {
    pure nothrow @nogc:
        this(int* ptr) { _ptr = ptr; }
        ~this() { if (_ptr) ++*_ptr; }
        int* _ptr;
    }

    int val;
    Foo foo1 = void; // uninitialized
    auto foo2 = Foo(&val); // initialized
    assert(foo2._ptr is &val);

    // Using `move(foo2, foo1)` would have an undefined effect because it would destroy
    // the uninitialized foo1.
    // moveEmplace directly overwrites foo1 without destroying or initializing it first.
    moveEmplace(foo2, foo1);
    assert(foo1._ptr is &val);
    assert(foo2._ptr is null);
    assert(val == 0);
}

@betterC
pure nothrow @nogc @system unittest
{
    static struct Foo
    {
    pure nothrow @nogc:
        this(int* ptr) { _ptr = ptr; }
        ~this() { if (_ptr) ++*_ptr; }
        int* _ptr;
    }

    int val;
    {
        Foo[1] foo1 = void; // uninitialized
        Foo[1] foo2 = [Foo(&val)];// initialized
        assert(foo2[0]._ptr is &val);

        // Using `move(foo2, foo1)` would have an undefined effect because it would destroy
        // the uninitialized foo1.
        // moveEmplace directly overwrites foo1 without destroying or initializing it first.
        moveEmplace(foo2, foo1);
        assert(foo1[0]._ptr is &val);
        assert(foo2[0]._ptr is null);
        assert(val == 0);
    }
    assert(val == 1);
}

// https://issues.dlang.org/show_bug.cgi?id=18913
@safe unittest
{
    static struct NoCopy
    {
        int payload;
        ~this() { }
        @disable this(this);
    }

    static void f(NoCopy[2]) { }

    NoCopy[2] ncarray = [ NoCopy(1), NoCopy(2) ];

    static assert(!__traits(compiles, f(ncarray)));
    f(move(ncarray));
}

//debug = PRINTF;

debug(PRINTF)
{
    import core.stdc.stdio : printf;
}

/// Implementation of `_d_delstruct` and `_d_delstructTrace`
template _d_delstructImpl(T)
{
    private void _d_delstructImpure(ref T p)
    {
        debug(PRINTF) printf("_d_delstruct(%p)\n", p);

        import core.memory : GC;

        destroy(*p);
        GC.free(p);
        p = null;
    }

    /**
     * This is called for a delete statement where the value being deleted is a
     * pointer to a struct with a destructor but doesn't have an overloaded
     * `delete` operator.
     *
     * Params:
     *   p = pointer to the value to be deleted
     *
     * Bugs:
     *   This function template was ported from a much older runtime hook that
     *   bypassed safety, purity, and throwabilty checks. To prevent breaking
     *   existing code, this function template is temporarily declared
     *   `@trusted` until the implementation can be brought up to modern D
     *   expectations.
     */
    void _d_delstruct(ref T p) @trusted @nogc pure nothrow
    {
        if (p)
        {
            alias Type = void function(ref T P) @nogc pure nothrow;
            (cast(Type) &_d_delstructImpure)(p);
        }
    }

    version (D_ProfileGC)
    {
        import core.internal.array.utils : _d_HookTraceImpl;

        private enum errorMessage = "Cannot delete struct if compiling without support for runtime type information!";

        /**
         * TraceGC wrapper around $(REF _d_delstruct, core,lifetime,_d_delstructImpl).
         *
         * Bugs:
         *   This function template was ported from a much older runtime hook that
         *   bypassed safety, purity, and throwabilty checks. To prevent breaking
         *   existing code, this function template is temporarily declared
         *   `@trusted` until the implementation can be brought up to modern D
         *   expectations.
         */
        alias _d_delstructTrace = _d_HookTraceImpl!(T, _d_delstruct, errorMessage);
    }
}

@system pure nothrow unittest
{
    int dtors = 0;
    struct S { ~this() nothrow { ++dtors; } }

    S *s = new S();
    _d_delstructImpl!(typeof(s))._d_delstruct(s);

    assert(s == null);
    assert(dtors == 1);
}

@system pure unittest
{
    int innerDtors = 0;
    int outerDtors = 0;

    struct Inner { ~this() { ++innerDtors; } }
    struct Outer
    {
        Inner *i1;
        Inner *i2;

        this(int x)
        {
            i1 = new Inner();
            i2 = new Inner();
        }

        ~this()
        {
            ++outerDtors;

            _d_delstructImpl!(typeof(i1))._d_delstruct(i1);
            assert(i1 == null);

           _d_delstructImpl!(typeof(i2))._d_delstruct(i2);
            assert(i2 == null);
        }
    }

    Outer *o = new Outer(0);
    _d_delstructImpl!(typeof(o))._d_delstruct(o);

    assert(o == null);
    assert(innerDtors == 2);
    assert(outerDtors == 1);
}

// https://issues.dlang.org/show_bug.cgi?id=25552
pure nothrow @system unittest
{
    int i;
    struct Nested
    {
    pure nothrow @nogc:
        char[1] arr; // char.init is not 0
        ~this() { ++i; }
    }

    {
        Nested[1] dst = void;
        Nested[1] src = [Nested(['a'])];

        moveEmplace(src, dst);
        assert(i == 0);
        assert(dst[0].arr == ['a']);
        assert(src[0].arr == [char.init]);
        assert(dst[0].tupleof[$-1] is src[0].tupleof[$-1]);
    }
    assert(i == 2);
}

// https://issues.dlang.org/show_bug.cgi?id=25552
@safe unittest
{
    int i;
    struct Nested
    {
        ~this() { ++i; }
    }

    static struct NotNested
    {
        Nested n;
    }

    static struct Deep
    {
        NotNested nn;
    }

    static struct Deeper
    {
        NotNested[1] nn;
    }

    static assert(__traits(isZeroInit, Nested));
    static assert(__traits(isZeroInit, NotNested));
    static assert(__traits(isZeroInit, Deep));
    static assert(__traits(isZeroInit, Deeper));

    {
        auto a = NotNested(Nested());
        assert(a.n.tupleof[$-1]);
        auto b = move(a);
        assert(b.n.tupleof[$-1]);
        assert(a.n.tupleof[$-1] is b.n.tupleof[$-1]);

        auto c = Deep(NotNested(Nested()));
        auto d = move(c);
        assert(d.nn.n.tupleof[$-1]);
        assert(c.nn.n.tupleof[$-1] is d.nn.n.tupleof[$-1]);

        auto e = Deeper([NotNested(Nested())]);
        auto f = move(e);
        assert(f.nn[0].n.tupleof[$-1]);
        assert(e.nn[0].n.tupleof[$-1] is f.nn[0].n.tupleof[$-1]);
    }
    assert(i == 6);
}

// https://issues.dlang.org/show_bug.cgi?id=25552
@safe unittest
{
    int i;
    struct Nested
    {
        align(32) // better still find context pointer correctly!
        int[3] stuff = [0, 1, 2];
        ~this() { ++i; }
    }

    static struct NoAssign
    {
        int value;
        @disable void opAssign(typeof(this));
    }

    static struct NotNested
    {
        int before = 42;
        align(Nested.alignof * 4) // better still find context pointer correctly!
        Nested n;
        auto after = NoAssign(43);
    }

    static struct Deep
    {
        NotNested nn;
    }

    static struct Deeper
    {
        NotNested[1] nn;
    }

    static assert(!__traits(isZeroInit, Nested));
    static assert(!__traits(isZeroInit, NotNested));
    static assert(!__traits(isZeroInit, Deep));
    static assert(!__traits(isZeroInit, Deeper));

    {
        auto a = NotNested(1, Nested([3, 4, 5]), NoAssign(2));
        auto b = move(a);
        assert(b.n.tupleof[$-1]);
        assert(a.n.tupleof[$-1] is b.n.tupleof[$-1]);
        assert(a.n.stuff == [0, 1, 2]);
        assert(a.before == 42);
        assert(a.after == NoAssign(43));

        auto c = Deep(NotNested(1, Nested([3, 4, 5]), NoAssign(2)));
        auto d = move(c);
        assert(d.nn.n.tupleof[$-1]);
        assert(c.nn.n.tupleof[$-1] is d.nn.n.tupleof[$-1]);
        assert(c.nn.n.stuff == [0, 1, 2]);
        assert(c.nn.before == 42);
        assert(c.nn.after == NoAssign(43));

        auto e = Deeper([NotNested(1, Nested([3, 4, 5]), NoAssign(2))]);
        auto f = move(e);
        assert(f.nn[0].n.tupleof[$-1]);
        assert(e.nn[0].n.tupleof[$-1] is f.nn[0].n.tupleof[$-1]);
        assert(e.nn[0].n.stuff == [0, 1, 2]);
        assert(e.nn[0].before == 42);
        assert(e.nn[0].after == NoAssign(43));
    }
    assert(i == 6);
}

// wipes source after moving
pragma(inline, true)
private void wipe(T, Init...)(return scope ref T source, ref const scope Init initializer) @trusted
if (!Init.length ||
    ((Init.length == 1) && (is(immutable T == immutable Init[0]))))
{
    static if (__traits(isStaticArray, T) && hasContextPointers!T)
    {
        for (auto i = 0; i < T.length; i++)
            static if (Init.length)
                wipe(source[i], initializer[0][i]);
            else
                wipe(source[i]);
    }
    else static if (is(T == struct) && hasContextPointers!T)
    {
        import core.internal.traits : anySatisfy;
        static if (anySatisfy!(hasContextPointers, typeof(T.tupleof)))
        {
            static foreach (i; 0 .. T.tupleof.length - __traits(isNested, T))
                static if (Init.length)
                    wipe(source.tupleof[i], initializer[0].tupleof[i]);
                else
                    wipe(source.tupleof[i]);
        }
        else
        {
            static if (__traits(isNested, T))
                enum sz = T.tupleof[$-1].offsetof;
            else
                enum sz = T.sizeof;

            static if (Init.length)
                *cast(ubyte[sz]*) &source = *cast(ubyte[sz]*) &initializer[0];
            else
                *cast(ubyte[sz]*) &source = 0;
        }
    }
    else
    {
        import core.internal.traits : hasElaborateAssign, isAssignable;
        static if (Init.length)
        {
            static if (hasElaborateAssign!T || !isAssignable!T)
                *cast(ubyte[T.sizeof]*) &source = *cast(ubyte[T.sizeof]*) &initializer[0];
            else
                source = *cast(T*) &initializer[0];
        }
        else
        {
            *cast(ubyte[T.sizeof]*) &source = 0;
        }
    }
}

/**
 * Allocate an exception of type `T` from the exception pool.
 * `T` must be `Throwable` or derived from it and cannot be a COM or C++ class.
 *
 * Note:
 *  This function does not call the constructor of `T` because that would require
 *  `forward!args`, which causes errors with -dip1008. This inconvenience will be
 *  removed once -dip1008 works as intended.
 *
 * Returns:
 *   allocated instance of type `T`
 */
T _d_newThrowable(T)() @trusted
    if (is(T : Throwable) && __traits(getLinkage, T) == "D")
{
    debug(PRINTF) printf("_d_newThrowable(%s)\n", cast(char*) T.stringof);

    import core.memory : pureMalloc;
    auto init = __traits(initSymbol, T);
    void* p = pureMalloc(init.length);
    if (!p)
    {
        import core.exception : onOutOfMemoryError;
        onOutOfMemoryError();
    }

    debug(PRINTF) printf(" p = %p\n", p);

    // initialize it
    p[0 .. init.length] = cast(void[]) init[];

    import core.internal.traits : hasIndirections;
    if (hasIndirections!T)
    {
        // Inform the GC about the pointers in the object instance
        import core.memory : GC;
        GC.addRange(p, init.length);
    }

    debug(PRINTF) printf("initialization done\n");

    (cast(Throwable) p).refcount() = 1;

    return cast(T) p;
}

@system unittest
{
    class E : Exception
    {
        this(string msg = "", Throwable nextInChain = null)
        {
            super(msg, nextInChain);
        }
    }

    Throwable exc = _d_newThrowable!Exception();
    Throwable e = _d_newThrowable!E();

    assert(exc.refcount() == 1);
    assert(e.refcount() == 1);
}

/**
 * Create a new class instance.
 * Allocates memory and sets fields to their initial value, but does not call a
 * constructor.
 * ---
 * new C() // _d_newclass!(C)()
 * ---
 * Returns: newly created object
 */
T _d_newclassT(T)() @trusted
if (is(T == class))
{
    import core.internal.traits : hasIndirections;
    import core.exception : onOutOfMemoryError;
    import core.memory : pureMalloc;
    import core.memory : GC;

    alias BlkAttr = GC.BlkAttr;

    auto init = __traits(initSymbol, T);
    void* p;

    static if (__traits(isCOMClass, T))
    {
        // If this is a COM class we allocate it using malloc.
        // This allows the reference counting to outlive the reference known about by the GC.

        p = pureMalloc(init.length);
        if (!p)
            onOutOfMemoryError();
    }
    else
    {
        BlkAttr attr = BlkAttr.NONE;

        /* `extern(C++)`` classes don't have a classinfo pointer in their vtable,
         * so the GC can't finalize them.
         */
        static if (__traits(hasMember, T, "__dtor") && __traits(getLinkage, T) != "C++")
            attr |= BlkAttr.FINALIZE;
        static if (!hasIndirections!T)
            attr |= BlkAttr.NO_SCAN;

        p = GC.malloc(init.length, attr, typeid(T));
        debug(PRINTF) printf(" p = %p\n", p);
    }

    debug(PRINTF)
    {
        printf("p = %p\n", p);
        printf("init.ptr = %p, len = %llu\n", init.ptr, cast(ulong)init.length);
        printf("vptr = %p\n", *cast(void**) init);
        printf("vtbl[0] = %p\n", (*cast(void***) init)[0]);
        printf("vtbl[1] = %p\n", (*cast(void***) init)[1]);
        printf("init[0] = %x\n", (cast(uint*) init)[0]);
        printf("init[1] = %x\n", (cast(uint*) init)[1]);
        printf("init[2] = %x\n", (cast(uint*) init)[2]);
        printf("init[3] = %x\n", (cast(uint*) init)[3]);
        printf("init[4] = %x\n", (cast(uint*) init)[4]);
    }

    // initialize it
    p[0 .. init.length] = cast(void[]) init[];

    debug(PRINTF) printf("initialization done\n");
    return cast(T) p;
}

/**
 * TraceGC wrapper around $(REF _d_newclassT, core,lifetime).
 */
T _d_newclassTTrace(T)(string file = __FILE__, int line = __LINE__, string funcname = __FUNCTION__) @trusted
{
    version (D_TypeInfo)
    {
        import core.internal.array.utils : TraceHook, gcStatsPure, accumulatePure;
        mixin(TraceHook!(T.stringof, "_d_newclassT"));

        return _d_newclassT!T();
    }
    else
        assert(0, "Cannot create new class if compiling without support for runtime type information!");
}

/**
 * Allocate an initialized non-array item.
 *
 * This is an optimization to avoid things needed for arrays like the __arrayPad(size).
 * Used to allocate struct instances on the heap.
 *
 * ---
 * struct Sz {int x = 0;}
 * struct Si {int x = 3;}
 *
 * void main()
 * {
 *     new Sz(); // uses zero-initialization
 *     new Si(); // uses Si.init
 * }
 * ---
 *
 * Returns:
 *     newly allocated item
 */
T* _d_newitemT(T)() @trusted
{
    import core.internal.lifetime : emplaceInitializer;
    import core.internal.traits : hasIndirections;
    import core.memory : GC;

    auto flags = !hasIndirections!T ? GC.BlkAttr.NO_SCAN : GC.BlkAttr.NONE;
    immutable itemSize = T.sizeof;
    if (TypeInfoSize!T)
        flags |= GC.BlkAttr.FINALIZE;

    auto p = GC.malloc(itemSize, flags, typeid(T));

    emplaceInitializer(*(cast(T*) p));

    return cast(T*) p;
}

// Test allocation
@safe unittest
{
    class C { }
    C c = _d_newclassT!C();

    assert(c !is null);
}

// Test initializers
@safe unittest
{
    {
        class C { int x, y; }
        C c = _d_newclassT!C();

        assert(c.x == 0);
        assert(c.y == 0);
    }
    {
        class C { int x = 2, y = 3; }
        C c = _d_newclassT!C();

        assert(c.x == 2);
        assert(c.y == 3);
    }
}

// Test allocation
@safe unittest
{
    struct S { }
    S* s = _d_newitemT!S();

    assert(s !is null);
}

// Test initializers
@safe unittest
{
    {
        // zero-initialization
        struct S { int x, y; }
        S* s = _d_newitemT!S();

        assert(s.x == 0);
        assert(s.y == 0);
    }
    {
        // S.init
        struct S { int x = 2, y = 3; }
        S* s = _d_newitemT!S();

        assert(s.x == 2);
        assert(s.y == 3);
    }
}

// Test GC attributes
version (CoreUnittest)
{
    struct S1
    {
        int x = 5;
    }
    struct S2
    {
        int x;
        this(int x) { this.x = x; }
    }
    struct S3
    {
        int[4] x;
        this(int x) { this.x[] = x; }
    }
    struct S4
    {
        int *x;
    }

}
@system unittest
{
    import core.memory : GC;

    auto s1 = new S1;
    assert(s1.x == 5);
    assert(GC.getAttr(s1) == GC.BlkAttr.NO_SCAN);

    auto s2 = new S2(3);
    assert(s2.x == 3);
    assert(GC.getAttr(s2) == GC.BlkAttr.NO_SCAN);

    auto s3 = new S3(1);
    assert(s3.x == [1, 1, 1, 1]);
    assert(GC.getAttr(s3) == GC.BlkAttr.NO_SCAN);
    debug(SENTINEL) {} else
        assert(GC.sizeOf(s3) == 16);

    auto s4 = new S4;
    assert(s4.x == null);
    assert(GC.getAttr(s4) == 0);
}

// Test struct finalizers exception handling
debug(SENTINEL) {} else
@system unittest
{
    import core.memory : GC;

    bool test(E)()
    {
        import core.exception;
        static struct S1
        {
            E exc;
            ~this() { throw exc; }
        }

        bool caught = false;
        S1* s = new S1(new E("test onFinalizeError"));
        try
        {
            GC.runFinalizers((cast(char*)(typeid(S1).xdtor))[0 .. 1]);
        }
        catch (FinalizeError err)
        {
            caught = true;
        }
        catch (E)
        {
        }
        GC.free(s);
        return caught;
    }

    assert(test!Exception);
    import core.exception : InvalidMemoryOperationError;
    assert(!test!InvalidMemoryOperationError);
}

version (D_ProfileGC)
{
    /**
    * TraceGC wrapper around $(REF _d_newitemT, core,lifetime).
    */
    T* _d_newitemTTrace(T)(string file = __FILE__, int line = __LINE__, string funcname = __FUNCTION__) @trusted
    {
        version (D_TypeInfo)
        {
            static if (is(T == struct))
            {
                // prime the TypeInfo name, we don't want that affecting the allocated bytes
                // Issue https://github.com/dlang/dmd/issues/20832
                static string typeName(TypeInfo_Struct ti) nothrow @trusted => ti.name;
                auto tnPure = cast(string function(TypeInfo_Struct ti) nothrow pure @trusted)&typeName;
                cast(void)tnPure(typeid(T));
            }

            import core.internal.array.utils : TraceHook, gcStatsPure, accumulatePure;
            mixin(TraceHook!(T.stringof, "_d_newitemT"));

            return _d_newitemT!T();
        }
        else
            assert(0, "Cannot create new `struct` if compiling without support for runtime type information!");
    }
}

template TypeInfoSize(T)
{
    import core.internal.traits : hasElaborateDestructor;
    enum TypeInfoSize = (is (T == struct) && hasElaborateDestructor!T) ? size_t.sizeof : 0;
}
