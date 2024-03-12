/**
 * A few predefined implementations for primitive types and arrays thereof. Also a couple of helpers.
 *
 * Copyright: Copyright Kenji Hara 2014-.
 * License:   <a href="https://boost.org/LICENSE_1_0.txt">Boost License 1.0</a>.
 * Authors:   Kenji Hara
 * Source: $(DRUNTIMESRC rt/util/_typeinfo.d)
 */
module rt.util.typeinfo;
import rt.util.utility : d_cfloat, d_cdouble, d_creal, isComplex;
static import core.internal.hash;

// Three-way compare for integrals: negative if `lhs < rhs`, positive if `lhs > rhs`, 0 otherwise.
pragma(inline, true)
private int cmp3(T)(const T lhs, const T rhs)
if (__traits(isIntegral, T))
{
    static if (T.sizeof < int.sizeof)
        // Taking the difference will always fit in an int.
        return int(lhs) - int(rhs);
    else
        return (lhs > rhs) - (lhs < rhs);
}

// Three-way compare for real fp types. NaN is smaller than all valid numbers.
// Code is small and fast, see https://godbolt.org/z/fzb877
pragma(inline, true)
private int cmp3(T)(const T d1, const T d2)
if (is(T == float) || is(T == double) || is(T == real))
{
    if (d2 != d2)
        return d1 == d1; // 0 if both ar NaN, 1 if d1 is valid and d2 is NaN.
    // If d1 is NaN, both comparisons are false so we get -1, as needed.
    return (d1 > d2) - !(d1 >= d2);
}

// Three-way compare for complex types.
pragma(inline, true)
private int cmp3(T)(const T f1, const T f2)
if (isComplex!T)
{
    if (int result = cmp3(f1.re, f2.re))
        return result;
    return cmp3(f1.im, f2.im);
}

unittest
{
    assert(cmp3(short.max, short.min) > 0);
    assert(cmp3(42, 42) == 0);
    assert(cmp3(int.max, int.min) > 0);

    double x, y;
    assert(cmp3(x, y) == 0);
    assert(cmp3(y, x) == 0);
    x = 42;
    assert(cmp3(x, y) > 0);
    assert(cmp3(y, x) < 0);
    y = 43;
    assert(cmp3(x, y) < 0);
    assert(cmp3(y, x) > 0);
    y = 42;
    assert(cmp3(x, y) == 0);
    assert(cmp3(y, x) == 0);

    d_cdouble u, v;
    assert(cmp3(u, v) == 0);
    assert(cmp3(v, u) == 0);
    u = d_cdouble(42, 42);
    assert(cmp3(u, v) > 0);
    assert(cmp3(v, u) < 0);
    v = d_cdouble(43, 42);
    assert(cmp3(u, v) < 0);
    assert(cmp3(v, u) > 0);
    v = d_cdouble(42, 43);
    assert(cmp3(u, v) < 0);
    assert(cmp3(v, u) > 0);
    v = d_cdouble(42, 42);
    assert(cmp3(u, v) == 0);
    assert(cmp3(v, u) == 0);
}

// @@@DEPRECATED_2.105@@@
template Array(T)
if (isComplex!T)
{
  pure nothrow @safe:

    bool equals(T[] s1, T[] s2)
    {
        size_t len = s1.length;
        if (len != s2.length)
            return false;
        for (size_t u = 0; u < len; u++)
        {
            if (!Floating!T.equals(s1[u], s2[u]))
                return false;
        }
        return true;
    }

    int compare(T[] s1, T[] s2)
    {
        size_t len = s1.length;
        if (s2.length < len)
            len = s2.length;
        for (size_t u = 0; u < len; u++)
        {
            if (int c = Floating!T.compare(s1[u], s2[u]))
                return c;
        }
        return (s1.length > s2.length) - (s1.length < s2.length);
    }

    size_t hashOf(scope const T[] val)
    {
        size_t hash = 0;
        foreach (ref o; val)
        {
            hash = core.internal.hash.hashOf(Floating!T.hashOf(o), hash);
        }
        return hash;
    }
}

version (CoreUnittest)
{
    alias TypeTuple(T...) = T;
}
unittest
{
    // Bugzilla 13052

    static struct SX(F) { F f; }
    TypeInfo ti;

    // real types
    foreach (F; TypeTuple!(float, double, real))
    (){ // workaround #2396
        alias S = SX!F;
        F f1 = +0.0,
          f2 = -0.0;

        assert(f1  == f2);
        assert(f1 !is f2);
        ti = typeid(F);
        assert(ti.getHash(&f1) == ti.getHash(&f2));

        F[] a1 = [f1, f1, f1];
        F[] a2 = [f2, f2, f2];
        assert(a1  == a2);
        assert(a1 !is a2);
        ti = typeid(F[]);
        assert(ti.getHash(&a1) == ti.getHash(&a2));

        F[][] aa1 = [a1, a1, a1];
        F[][] aa2 = [a2, a2, a2];
        assert(aa1  == aa2);
        assert(aa1 !is aa2);
        ti = typeid(F[][]);
        assert(ti.getHash(&aa1) == ti.getHash(&aa2));

        S s1 = {f1},
          s2 = {f2};
        assert(s1  == s2);
        assert(s1 !is s2);
        ti = typeid(S);
        assert(ti.getHash(&s1) == ti.getHash(&s2));

        S[] da1 = [S(f1), S(f1), S(f1)],
            da2 = [S(f2), S(f2), S(f2)];
        assert(da1  == da2);
        assert(da1 !is da2);
        ti = typeid(S[]);
        assert(ti.getHash(&da1) == ti.getHash(&da2));

        S[3] sa1 = {f1},
             sa2 = {f2};
        assert(sa1  == sa2);
        assert(sa1[] !is sa2[]);
        ti = typeid(S[3]);
        assert(ti.getHash(&sa1) == ti.getHash(&sa2));
    }();
}

// Reduces to `T` if `cond` is `true` or `U` otherwise. Consider moving elsewhere if useful.
private template Select(bool cond, T, U)
{
    static if (cond) alias Select = T;
    else alias Select = U;
}

/*
TypeInfo information for built-in types.

A `Base` type may be specified, which must be a type with the same layout, alignment, hashing, and
equality comparison as type `T`. This saves on code size because parts of `Base` will be reused. Example:
`char` and `ubyte`. The implementation assumes `Base` and `T` hash the same, swap
the same, have the same ABI flags, and compare the same for equality. For ordering comparisons, we detect
during compilation whether they have different signedness and override appropriately. For initializer, we
detect if we need to override. The overriding initializer should be nonzero.
*/
private class TypeInfoGeneric(T, Base = T) : Select!(is(T == Base), TypeInfo, TypeInfoGeneric!Base)
if (T.sizeof == Base.sizeof && T.alignof == Base.alignof)
{
    const: nothrow: pure: @trusted:

    // Returns the type name.
    override string toString() const pure nothrow @safe { return T.stringof; }

    // `getHash` is the same for `Base` and `T`, introduce it just once.
    static if (is(T == Base))
        override size_t getHash(scope const void* p)
        {
            return hashOf(*cast(const T *)p);
        }

    // `equals` is the same for `Base` and `T`, introduce it just once.
    static if (is(T == Base))
        override bool equals(in void* p1, in void* p2)
        {
            return *cast(const T *)p1 == *cast(const T *)p2;
        }

    // `T` and `Base` may have different signedness, so this function is introduced conditionally.
    static if (is(T == Base) || (__traits(isIntegral, T) && T.max != Base.max))
        override int compare(in void* p1, in void* p2)
        {
            return cmp3(*cast(const T*) p1, *cast(const T*) p2);
        }

    static if (is(T == Base))
        override @property size_t tsize()
        {
            return T.sizeof;
        }

    static if (is(T == Base))
        override @property size_t talign()
        {
            return T.alignof;
        }

    // Override initializer only if necessary.
    static if (is(T == Base) || T.init != Base.init)
        override const(void)[] initializer()
        {
            static if (__traits(isZeroInit, T))
            {
                return (cast(void *)null)[0 .. T.sizeof];
            }
            else
            {
                static immutable T[1] c;
                return c;
            }
        }

    // `swap` is the same for `Base` and `T`, so introduce only once.
    static if (is(T == Base))
        override void swap(void *p1, void *p2)
        {
            auto t = *cast(T *) p1;
            *cast(T *)p1 = *cast(T *)p2;
            *cast(T *)p2 = t;
        }

    static if (is(T == Base) || RTInfo!T != RTInfo!Base)
        override @property immutable(void)* rtInfo()
        {
            return RTInfo!T;
        }

    static if (is(T == Base))
    {
        static if ((__traits(isFloating, T) && T.mant_dig != 64) ||
                   (isComplex!T && T.re.mant_dig != 64))
            // FP types except 80-bit X87 are passed in SIMD register.
            override @property uint flags() const { return 2; }
    }
}

unittest
{
    assert(typeid(int).toString == "int");

    with (typeid(double))
    {
        double a = 42, b = 43;
        assert(equals(&a, &a));
        assert(!equals(&a, &b));
        assert(compare(&a, &a) == 0);
        assert(compare(&a, &b) == -1);
        assert(compare(&b, &a) == 1);
    }

    with (typeid(short))
    {
        short c = 42, d = 43;
        assert(equals(&c, &c));
        assert(!equals(&c, &d));
        assert(compare(&c, &c) == 0);
        assert(compare(&c, &d) == -1);
        assert(compare(&d, &c) == 1);
        assert(initializer.ptr is null);
        assert(initializer.length == short.sizeof);
        swap(&d, &c);
        assert(c == 43 && d == 42);
    }
}

/*
TypeInfo information for arrays of built-in types.

A `Base` type may be specified, which must be a type with the same layout, alignment, hashing, and
equality comparison as type `T`. This saves on code size because parts of `Base` will be reused. Example:
`char` and `ubyte`. The implementation assumes `Base` and `T` hash the same, swap
the same, have the same ABI flags, and compare the same for equality. For ordering comparisons, we detect
during compilation whether they have different signedness and override appropriately. For initializer, we
detect if we need to override. The overriding initializer should be nonzero.
*/
private class TypeInfoArrayGeneric(T, Base = T) : Select!(is(T == Base), TypeInfo_Array, TypeInfoArrayGeneric!Base)
{
    static if (is(T == Base))
        override bool opEquals(const Object o) const @safe nothrow { return TypeInfo.opEquals(cast(const TypeInfo) o); }

    alias opEquals = typeof(super).opEquals;
    alias opEquals = TypeInfo.opEquals;

    override string toString() const { return (T[]).stringof; }

    static if (is(T == Base))
        override size_t getHash(scope const void* p) @trusted const
        {
            return hashOf(*cast(const T[]*) p);
        }

    static if (is(T == Base))
        override bool equals(in void* p1, in void* p2) const
        {
            // Just reuse the builtin.
            return *cast(const(T)[]*) p1 == *cast(const(T)[]*) p2;
        }

    static if (is(T == Base) || (__traits(isIntegral, T) && T.max != Base.max))
        override int compare(in void* p1, in void* p2) const
        {
            // Can't reuse __cmp in object.d because that handles NaN differently.
            // (Q: would it make sense to unify behaviors?)
            // return __cmp(*cast(const T[]*) p1, *cast(const T[]*) p2);
            auto lhs = *cast(const T[]*) p1;
            auto rhs = *cast(const T[]*) p2;
            size_t len = lhs.length;
            if (rhs.length < len)
                len = rhs.length;
            for (size_t u = 0; u < len; u++)
            {
                if (int result = cmp3(lhs.ptr[u], rhs.ptr[u]))
                    return result;
            }
            return cmp3(lhs.length, rhs.length);        }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout) typeid(T);
    }
}

unittest
{
    assert(typeid(int[]) == typeid(int[]));
    assert(typeid(int[]) != typeid(uint[]));
    assert(typeid(int[]).toString == "int[]");

    with (typeid(double[]))
    {
        double[] a = [ 1, 2, 3 ], b = [ 2, 3 ];
        assert(equals(&a, &a));
        assert(!equals(&a, &b));
        assert(compare(&a, &a) == 0);
        assert(compare(&a, &b) == -1);
        assert(compare(&b, &a) == 1);
    }
}

////////////////////////////////////////////////////////////////////////////////
// Predefined TypeInfos
////////////////////////////////////////////////////////////////////////////////

// void
class TypeInfo_v : TypeInfoGeneric!ubyte
{
    const: nothrow: pure: @trusted:

    override string toString() const pure nothrow @safe { return "void"; }

    override size_t getHash(scope const void* p)
    {
        assert(0);
    }

    override @property uint flags() nothrow pure
    {
        return 1;
    }
}

unittest
{
    assert(typeid(void).toString == "void");
    assert(typeid(void).flags == 1);
}

// All integrals.
class TypeInfo_h : TypeInfoGeneric!ubyte {}
class TypeInfo_b : TypeInfoGeneric!(bool, ubyte) {}
class TypeInfo_g : TypeInfoGeneric!(byte, ubyte) {}
class TypeInfo_a : TypeInfoGeneric!(char, ubyte) {}
class TypeInfo_t : TypeInfoGeneric!ushort {}
class TypeInfo_s : TypeInfoGeneric!(short, ushort) {}
class TypeInfo_u : TypeInfoGeneric!(wchar, ushort) {}
class TypeInfo_w : TypeInfoGeneric!(dchar, uint) {}
class TypeInfo_k : TypeInfoGeneric!uint {}
class TypeInfo_i : TypeInfoGeneric!(int, uint) {}
class TypeInfo_m : TypeInfoGeneric!ulong {}
class TypeInfo_l : TypeInfoGeneric!(long, ulong) {}
static if (is(cent)) class TypeInfo_zi : TypeInfoGeneric!cent {}
static if (is(ucent)) class TypeInfo_zk : TypeInfoGeneric!ucent {}

// All simple floating-point types.
class TypeInfo_f : TypeInfoGeneric!float {}
class TypeInfo_d : TypeInfoGeneric!double {}
class TypeInfo_e : TypeInfoGeneric!real {}

// All imaginary floating-point types.

// ifloat @@@DEPRECATED_2.105@@@
deprecated class TypeInfo_o : TypeInfoGeneric!float
{
    override string toString() const pure nothrow @safe { return "ifloat"; }
}

// idouble @@@DEPRECATED_2.105@@@
deprecated class TypeInfo_p : TypeInfoGeneric!double
{
    override string toString() const pure nothrow @safe { return "idouble"; }
}

// ireal @@@DEPRECATED_2.105@@@
deprecated class TypeInfo_j : TypeInfoGeneric!real
{
    override string toString() const pure nothrow @safe { return "ireal"; }
}

// All complex floating-point types.

// cfloat @@@DEPRECATED_2.105@@@
deprecated class TypeInfo_q : TypeInfoGeneric!d_cfloat
{
    override string toString() const pure nothrow @safe { return "cfloat"; }

    const: nothrow: pure: @trusted:
    static if (__traits(hasMember, TypeInfo, "argTypes"))
        override int argTypes(out TypeInfo arg1, out TypeInfo arg2)
        {
            arg1 = typeid(double);
            return 0;
        }
}

// cdouble @@@DEPRECATED_2.105@@@
deprecated class TypeInfo_r : TypeInfoGeneric!d_cdouble
{
    override string toString() const pure nothrow @safe { return "cdouble"; }

    const: nothrow: pure: @trusted:
    static if (__traits(hasMember, TypeInfo, "argTypes"))
        override int argTypes(out TypeInfo arg1, out TypeInfo arg2)
        {
            arg1 = typeid(double);
            arg2 = typeid(double);
            return 0;
        }
}

// creal @@@DEPRECATED_2.105@@@
deprecated class TypeInfo_c : TypeInfoGeneric!d_creal
{
    override string toString() const pure nothrow @safe { return "creal"; }

    const: nothrow: pure: @trusted:
    static if (__traits(hasMember, TypeInfo, "argTypes"))
        override int argTypes(out TypeInfo arg1, out TypeInfo arg2)
        {
            arg1 = typeid(real);
            arg2 = typeid(real);
            return 0;
        }
}

// Arrays of all integrals.
class TypeInfo_Ah : TypeInfoArrayGeneric!ubyte {}
class TypeInfo_Ab : TypeInfoArrayGeneric!(bool, ubyte) {}
class TypeInfo_Ag : TypeInfoArrayGeneric!(byte, ubyte) {}
class TypeInfo_Aa : TypeInfoArrayGeneric!(char, ubyte) {}
class TypeInfo_Axa : TypeInfoArrayGeneric!(const char) {}
class TypeInfo_Aya : TypeInfoArrayGeneric!(immutable char)
{
    // Must override this, otherwise "string" is returned.
    override string toString() const { return "immutable(char)[]"; }
}
class TypeInfo_At : TypeInfoArrayGeneric!ushort {}
class TypeInfo_As : TypeInfoArrayGeneric!(short, ushort) {}
class TypeInfo_Au : TypeInfoArrayGeneric!(wchar, ushort) {}
class TypeInfo_Ak : TypeInfoArrayGeneric!uint {}
class TypeInfo_Ai : TypeInfoArrayGeneric!(int, uint) {}
class TypeInfo_Aw : TypeInfoArrayGeneric!(dchar, uint) {}
class TypeInfo_Am : TypeInfoArrayGeneric!ulong {}
class TypeInfo_Al : TypeInfoArrayGeneric!(long, ulong) {}

version (CoreUnittest)
    private extern (C) void[] _adSort(void[] a, TypeInfo ti);

unittest
{
    assert(typeid(string).toString() == "immutable(char)[]");
    int[][] a = [[5,3,8,7], [2,5,3,8,7]];
    _adSort(*cast(void[]*)&a, typeid(a[0]));
    assert(a == [[2,5,3,8,7], [5,3,8,7]]);

    a = [[5,3,8,7], [5,3,8]];
    _adSort(*cast(void[]*)&a, typeid(a[0]));
    assert(a == [[5,3,8], [5,3,8,7]]);
}

unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=13073: original code uses int subtraction which is susceptible to
    // integer overflow, causing the following case to fail.
    int[] a = [int.max, int.max];
    int[] b = [int.min, int.min];
    assert(a > b);
    assert(b < a);
}

unittest
{
    // Original test case from https://issues.dlang.org/show_bug.cgi?id=13073
    uint x = 0x22_DF_FF_FF;
    uint y = 0xA2_DF_FF_FF;
    assert(!(x < y && y < x));
    uint[] a = [x];
    uint[] b = [y];
    assert(!(a < b && b < a)); // Original failing case
    uint[1] a1 = [x];
    uint[1] b1 = [y];
    assert(!(a1 < b1 && b1 < a1)); // Original failing case
}

// Arrays of all simple floating-point types.
class TypeInfo_Af : TypeInfoArrayGeneric!float {}
class TypeInfo_Ad : TypeInfoArrayGeneric!double {}
class TypeInfo_Ae : TypeInfoArrayGeneric!real {}

// Arrays of all imaginary floating-point types.

// ifloat @@@DEPRECATED_2.105@@@
deprecated class TypeInfo_Ao : TypeInfoArrayGeneric!float
{
    override string toString() const pure nothrow @safe { return "ifloat[]"; }
}

// idouble @@@DEPRECATED_2.105@@@
deprecated class TypeInfo_Ap : TypeInfoArrayGeneric!double
{
    override string toString() const pure nothrow @safe { return "idouble[]"; }
}

// ireal @@@DEPRECATED_2.105@@@
deprecated class TypeInfo_Aj : TypeInfoArrayGeneric!real
{
    override string toString() const pure nothrow @safe { return "ireal[]"; }
}

// Arrays of all complex floating-point types.

// cfloat @@@DEPRECATED_2.105@@@
deprecated class TypeInfo_Aq : TypeInfoArrayGeneric!d_cfloat
{
    override string toString() const pure nothrow @safe { return "cfloat[]"; }
}

// cdouble @@@DEPRECATED_2.105@@@
deprecated class TypeInfo_Ar : TypeInfoArrayGeneric!d_cdouble
{
    override string toString() const pure nothrow @safe { return "cdouble[]"; }
}

// creal @@@DEPRECATED_2.105@@@
deprecated class TypeInfo_Ac : TypeInfoArrayGeneric!d_creal
{
    override string toString() const pure nothrow @safe { return "creal[]"; }
}

// void[] is a bit different, behaves like ubyte[] for comparison purposes.
class TypeInfo_Av : TypeInfo_Ah
{
    override string toString() const { return "void[]"; }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout) typeid(void);
    }

    unittest
    {
        assert(typeid(void[]).toString == "void[]");
        assert(typeid(void[]).next == typeid(void));
    }
}

// all delegates
unittest
{
    assert(typeid(void delegate(int)).flags == 1);
}

// typeof(null)
class TypeInfo_n : TypeInfo
{
    const: pure: @nogc: nothrow: @safe:

    override string toString() { return "typeof(null)"; }

    override size_t getHash(scope const void*) { return 0; }

    override bool equals(in void*, in void*) { return true; }

    override int compare(in void*, in void*) { return 0; }

    override @property size_t tsize() { return typeof(null).sizeof; }

    override const(void)[] initializer() @trusted { return (cast(void *)null)[0 .. size_t.sizeof]; }

    override void swap(void*, void*) {}

    override @property immutable(void)* rtInfo() { return rtinfoNoPointers; }
}

unittest
{
    with (typeid(typeof(null)))
    {
        assert(toString == "typeof(null)");
        assert(getHash(null) == 0);
        assert(equals(null, null));
        assert(compare(null, null) == 0);
        assert(tsize == typeof(null).sizeof);
        assert(initializer.ptr is null);
        assert(initializer.length == typeof(null).sizeof);
        assert(rtInfo == rtinfoNoPointers);
    }
}

// Test typeinfo for classes.
unittest
{
    static class Bacon
    {
        int sizzle = 1;
        override int opCmp(Object rhs) const
        {
            if (auto rhsb = cast(Bacon) rhs)
                return (sizzle > rhsb.sizzle) - (sizzle < rhsb.sizzle);
            return 0;
        }
    }
    Object obj = new Bacon;
    Bacon obj2 = new Bacon;
    obj2.sizzle = 2;
    auto dummy = new Object;
    with (typeid(obj))
    {
        assert(toString[$ - 6 .. $] == ".Bacon");
        assert(getHash(&obj) != 0);
        assert(equals(&obj, &obj));
        assert(!equals(&obj, &obj2));
        assert(compare(&obj, &dummy) == 0);
        assert(compare(&obj, &obj) == 0);
        assert(compare(&obj, &obj2) == -1);
        assert(compare(&obj2, &obj) == 1);
        assert(tsize == Object.sizeof);
        assert(rtInfo == RTInfo!Bacon);
        assert(tsize == Object.sizeof);
        assert(initializer.ptr !is null);
        assert(initializer.length == __traits(classInstanceSize, Bacon));
        assert(flags == 1);
    }
}
