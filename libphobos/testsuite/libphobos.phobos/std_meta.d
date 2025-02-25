@safe unittest
{
    import std.meta;

    import std.meta;
    alias TL = AliasSeq!(int, double);

    int foo(TL td)  // same as int foo(int, double);
    {
        return td[0] + cast(int) td[1];
    }
}

@safe unittest
{
    import std.meta;

    alias TL = AliasSeq!(int, double);

    alias Types = AliasSeq!(TL, char);
    static assert(is(Types == AliasSeq!(int, double, char)));
}

@safe unittest
{
    import std.meta;

    // Creates a compile-time sequence of function call expressions
    // that each call `func` with the next variadic template argument
    template Map(alias func, args...)
    {
        auto ref lazyItem() {return func(args[0]);}

        static if (args.length == 1)
        {
            alias Map = lazyItem;
        }
        else
        {
            // recurse
            alias Map = AliasSeq!(lazyItem, Map!(func, args[1 .. $]));
        }
    }

    static void test(int a, int b)
    {
        assert(a == 4);
        assert(b == 16);
    }

    static int a = 2;
    static int b = 4;

    test(Map!(i => i ^^ 2, a, b));
    assert(a == 2);
    assert(b == 4);

    test(Map!((ref i) => i *= i, a, b));
    assert(a == 4);
    assert(b == 16);

    static void testRef(ref int a, ref int b)
    {
        assert(a++ == 16);
        assert(b++ == 256);
    }

    testRef(Map!(function ref(ref i) => i *= i, a, b));
    assert(a == 17);
    assert(b == 257);
}

@safe unittest
{
    import std.meta;

    // Without Alias this would fail if Args[0] was e.g. a value and
    // some logic would be needed to detect when to use enum instead
    alias Head(Args...) = Alias!(Args[0]);
    alias Tail(Args...) = Args[1 .. $];

    alias Blah = AliasSeq!(3, int, "hello");
    static assert(Head!Blah == 3);
    static assert(is(Head!(Tail!Blah) == int));
    static assert((Tail!Blah)[1] == "hello");
}

@safe unittest
{
    import std.meta;

    alias a = Alias!(123);
    static assert(a == 123);

    enum abc = 1;
    alias b = Alias!(abc);
    static assert(b == 1);

    alias c = Alias!(3 + 4);
    static assert(c == 7);

    alias concat = (s0, s1) => s0 ~ s1;
    alias d = Alias!(concat("Hello", " World!"));
    static assert(d == "Hello World!");

    alias e = Alias!(int);
    static assert(is(e == int));

    alias f = Alias!(AliasSeq!(int));
    static assert(!is(typeof(f[0]))); //not an AliasSeq
    static assert(is(f == int));

    auto g = 6;
    alias h = Alias!g;
    ++h;
    assert(g == 7);
}

@safe unittest
{
    import std.meta;

    import std.stdio;

    void foo()
    {
        writefln("The index of long is %s",
                 staticIndexOf!(long, AliasSeq!(int, long, double)));
        // prints: The index of long is 1
    }
}

@safe unittest
{
    import std.meta;

    alias Types = AliasSeq!(int, long, double, char);
    alias TL = Erase!(long, Types);
    static assert(is(TL == AliasSeq!(int, double, char)));
}

@safe unittest
{
    import std.meta;

    alias Types = AliasSeq!(int, long, long, int);
    static assert(is(EraseAll!(long, Types) == AliasSeq!(int, int)));
}

@safe unittest
{
    import std.meta;

    alias Types = AliasSeq!(int, long, long, int, float);

    alias TL = NoDuplicates!(Types);
    static assert(is(TL == AliasSeq!(int, long, float)));
}

@safe unittest
{
    import std.meta;

    alias Types = AliasSeq!(int, long, long, int, float);

    alias TL = Replace!(long, char, Types);
    static assert(is(TL == AliasSeq!(int, char, long, int, float)));
}

@safe unittest
{
    import std.meta;

    alias Types = AliasSeq!(int, long, long, int, float);

    alias TL = ReplaceAll!(long, char, Types);
    static assert(is(TL == AliasSeq!(int, char, char, int, float)));
}

@safe unittest
{
    import std.meta;

    alias Types = AliasSeq!(int, long, long, int, float, byte, ubyte, short, ushort, uint);

    alias TL = Reverse!(Types);
    static assert(is(TL == AliasSeq!(uint, ushort, short, ubyte, byte, float, int, long, long, int)));
}

@safe unittest
{
    import std.meta;

    class A { }
    class B : A { }
    class C : B { }
    alias Types = AliasSeq!(A, C, B);

    MostDerived!(Object, Types) x;  // x is declared as type C
    static assert(is(typeof(x) == C));
}

@safe unittest
{
    import std.meta;

    class A { }
    class B : A { }
    class C : B { }
    alias Types = AliasSeq!(A, C, B);

    alias TL = DerivedToFront!(Types);
    static assert(is(TL == AliasSeq!(C, B, A)));

    alias TL2 = DerivedToFront!(A, A, A, B, B, B, C, C, C);
    static assert(is(TL2 == AliasSeq!(C, C, C, B, B, B, A, A, A)));
}

@safe unittest
{
    import std.meta;

    import std.traits : Unqual;
    alias TL = staticMap!(Unqual, int, const int, immutable int, uint, ubyte, byte, short, ushort);
    static assert(is(TL == AliasSeq!(int, int, int, uint, ubyte, byte, short, ushort)));
}

@safe unittest
{
    import std.meta;

    import std.traits : isIntegral;

    static assert(!allSatisfy!(isIntegral, int, double));
    static assert( allSatisfy!(isIntegral, int, long));
}

@safe unittest
{
    import std.meta;

    import std.traits : isIntegral;

    static assert(!anySatisfy!(isIntegral, string, double));
    static assert( anySatisfy!(isIntegral, int, double));
}

@safe unittest
{
    import std.meta;

    import std.traits : isNarrowString, isUnsigned;

    alias Types1 = AliasSeq!(string, wstring, dchar[], char[], dstring, int);
    alias TL1 = Filter!(isNarrowString, Types1);
    static assert(is(TL1 == AliasSeq!(string, wstring, char[])));

    alias Types2 = AliasSeq!(int, byte, ubyte, dstring, dchar, uint, ulong);
    alias TL2 = Filter!(isUnsigned, Types2);
    static assert(is(TL2 == AliasSeq!(ubyte, uint, ulong)));
}

@safe unittest
{
    import std.meta;

    import std.traits : isPointer;

    alias isNoPointer = templateNot!isPointer;
    static assert(!isNoPointer!(int*));
    static assert(allSatisfy!(isNoPointer, string, char, float));
}

@safe unittest
{
    import std.meta;

    import std.traits : isNumeric, isUnsigned;

    alias storesNegativeNumbers = templateAnd!(isNumeric, templateNot!isUnsigned);
    static assert(storesNegativeNumbers!int);
    static assert(!storesNegativeNumbers!string && !storesNegativeNumbers!uint);

    // An empty sequence of predicates always yields true.
    alias alwaysTrue = templateAnd!();
    static assert(alwaysTrue!int);
}

@safe unittest
{
    import std.meta;

    import std.traits : isPointer, isUnsigned;

    alias isPtrOrUnsigned = templateOr!(isPointer, isUnsigned);
    static assert( isPtrOrUnsigned!uint &&  isPtrOrUnsigned!(short*));
    static assert(!isPtrOrUnsigned!int  && !isPtrOrUnsigned!(string));

    // An empty sequence of predicates never yields true.
    alias alwaysFalse = templateOr!();
    static assert(!alwaysFalse!int);
}

@safe unittest
{
    import std.meta;

    import std.algorithm.iteration : map;
    import std.algorithm.sorting : sort;
    import std.string : capitalize;

    struct S
    {
        int a;
        int c;
        int b;
    }

    alias capMembers = aliasSeqOf!([__traits(allMembers, S)].sort().map!capitalize());
    static assert(capMembers[0] == "A");
    static assert(capMembers[1] == "B");
    static assert(capMembers[2] == "C");
}

@safe unittest
{
    import std.meta;

    static immutable REF = [0, 1, 2, 3];
    foreach (I, V; aliasSeqOf!([0, 1, 2, 3]))
    {
        static assert(V == I);
        static assert(V == REF[I]);
    }
}

@safe unittest
{
    import std.meta;

    // enum bool isImplicitlyConvertible(From, To)
    import std.traits : isImplicitlyConvertible;

    static assert(allSatisfy!(
        ApplyLeft!(isImplicitlyConvertible, ubyte),
        short, ushort, int, uint, long, ulong));

    static assert(is(Filter!(ApplyRight!(isImplicitlyConvertible, short),
        ubyte, string, short, float, int) == AliasSeq!(ubyte, short)));
}

@safe unittest
{
    import std.meta;

    import std.traits : hasMember, ifTestable;

    struct T1
    {
        bool foo;
    }

    struct T2
    {
        struct Test
        {
            bool opCast(T : bool)() { return true; }
        }

        Test foo;
    }

    static assert(allSatisfy!(ApplyRight!(hasMember, "foo"), T1, T2));
    static assert(allSatisfy!(ApplyRight!(ifTestable, a => a.foo), T1, T2));
}

@safe unittest
{
    import std.meta;

    import std.traits : Largest;

    alias Types = AliasSeq!(byte, short, int, long);

    static assert(is(staticMap!(ApplyLeft!(Largest, short), Types) ==
                AliasSeq!(short, short, int, long)));
    static assert(is(staticMap!(ApplyLeft!(Largest, int), Types) ==
                AliasSeq!(int, int, int, long)));
}

@safe unittest
{
    import std.meta;

    import std.traits : FunctionAttribute, SetFunctionAttributes;

    static void foo() @system;
    static int bar(int) @system;

    alias SafeFunctions = AliasSeq!(
        void function() @safe,
        int function(int) @safe);

    static assert(is(staticMap!(ApplyRight!(
        SetFunctionAttributes, "D", FunctionAttribute.safe),
        typeof(&foo), typeof(&bar)) == SafeFunctions));
}

@safe unittest
{
    import std.meta;

    alias ImInt0 = Repeat!(0, int);
    static assert(is(ImInt0 == AliasSeq!()));

    alias ImInt1 = Repeat!(1, immutable(int));
    static assert(is(ImInt1 == AliasSeq!(immutable(int))));

    alias Real3 = Repeat!(3, real);
    static assert(is(Real3 == AliasSeq!(real, real, real)));

    alias Real12 = Repeat!(4, Real3);
    static assert(is(Real12 == AliasSeq!(real, real, real, real, real, real,
        real, real, real, real, real, real)));

    alias Composite = AliasSeq!(uint, int);
    alias Composite2 = Repeat!(2, Composite);
    static assert(is(Composite2 == AliasSeq!(uint, int, uint, int)));

    alias ImInt10 = Repeat!(10, int);
    static assert(is(ImInt10 == AliasSeq!(int, int, int, int, int, int, int, int, int, int)));

    alias Big = Repeat!(1_000_000, int);
}

@safe unittest
{
    import std.meta;

    auto staticArray(T, size_t n)(Repeat!(n, T) elems)
    {
        T[n] a = [elems];
        return a;
    }

    auto a = staticArray!(long, 3)(3, 1, 4);
    assert(is(typeof(a) == long[3]));
    assert(a == [3, 1, 4]);
}

@safe unittest
{
    import std.meta;

    alias Nums = AliasSeq!(7, 2, 3, 23);
    enum Comp(int N1, int N2) = N1 < N2;
    static assert(AliasSeq!(2, 3, 7, 23) == staticSort!(Comp, Nums));
}

@safe unittest
{
    import std.meta;

    alias Types = AliasSeq!(uint, short, ubyte, long, ulong);
    enum Comp(T1, T2) = __traits(isUnsigned, T2) - __traits(isUnsigned, T1);
    static assert(is(AliasSeq!(uint, ubyte, ulong, short, long) == staticSort!(Comp,
        Types)));
}

@safe unittest
{
    import std.meta;

    enum Comp(int N1, int N2) = N1 < N2;
    static assert( staticIsSorted!(Comp, 2, 2));
    static assert( staticIsSorted!(Comp, 2, 3, 7, 23));
    static assert(!staticIsSorted!(Comp, 7, 2, 3, 23));
}

@safe unittest
{
    import std.meta;

    enum Comp(T1, T2) = __traits(isUnsigned, T2) - __traits(isUnsigned, T1);
    static assert( staticIsSorted!(Comp, uint, ubyte, ulong, short, long));
    static assert(!staticIsSorted!(Comp, uint, short, ubyte, long, ulong));
}

@safe unittest
{
    import std.meta;

    static assert(is(Stride!(1, short, int, long) == AliasSeq!(short, int, long)));
    static assert(is(Stride!(2, short, int, long) == AliasSeq!(short, long)));
    static assert(is(Stride!(-1, short, int, long) == AliasSeq!(long, int, short)));
    static assert(is(Stride!(-2, short, int, long) == AliasSeq!(long, short)));

    alias attribs = AliasSeq!(short, int, long, ushort, uint, ulong);
    static assert(is(Stride!(3, attribs) == AliasSeq!(short, ushort)));
    static assert(is(Stride!(3, attribs[1 .. $]) == AliasSeq!(int, uint)));
    static assert(is(Stride!(-3, attribs) == AliasSeq!(ulong, long)));
}

@safe unittest
{
    import std.meta;

    // ApplyRight combined with Instantiate can be used to apply various
    // templates to the same parameters.
    import std.string : leftJustify, center, rightJustify;
    alias functions = staticMap!(ApplyRight!(Instantiate, string),
                                 leftJustify, center, rightJustify);
    string result = "";
    static foreach (f; functions)
    {
        {
            auto x = &f; // not a template, but a function instantiation
            result ~= x("hello", 7);
            result ~= ";";
        }
    }

    assert(result == "hello  ; hello ;  hello;");
}

