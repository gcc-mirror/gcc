/**
 * This module contains compiler support for comparing dynamic arrays
 *
 * Copyright: Copyright Digital Mars 2000 - 2019.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Source: $(DRUNTIMESRC core/internal/_array/_comparison.d)
 */

module core.internal.array.comparison;

int __cmp(T)(scope const T[] lhs, scope const T[] rhs) @trusted
    if (__traits(isScalar, T))
{
    // Compute U as the implementation type for T
    static if (is(T == ubyte) || is(T == void) || is(T == bool))
        alias U = char;
    else static if (is(T == wchar))
        alias U = ushort;
    else static if (is(T == dchar))
        alias U = uint;
    else static if (is(T == ifloat))
        alias U = float;
    else static if (is(T == idouble))
        alias U = double;
    else static if (is(T == ireal))
        alias U = real;
    else
        alias U = T;

    static if (is(U == char))
    {
        import core.internal.string : dstrcmp;
        return dstrcmp(cast(char[]) lhs, cast(char[]) rhs);
    }
    else static if (!is(U == T))
    {
        // Reuse another implementation
        return __cmp(cast(U[]) lhs, cast(U[]) rhs);
    }
    else
    {
        version (BigEndian)
        static if (__traits(isUnsigned, T) ? !is(T == __vector) : is(T : P*, P))
        {
            if (!__ctfe)
            {
                import core.stdc.string : memcmp;
                int c = memcmp(lhs.ptr, rhs.ptr, (lhs.length <= rhs.length ? lhs.length : rhs.length) * T.sizeof);
                if (c)
                    return c;
                static if (size_t.sizeof <= uint.sizeof && T.sizeof >= 2)
                    return cast(int) lhs.length - cast(int) rhs.length;
                else
                    return int(lhs.length > rhs.length) - int(lhs.length < rhs.length);
            }
        }

        immutable len = lhs.length <= rhs.length ? lhs.length : rhs.length;
        foreach (const u; 0 .. len)
        {
            auto a = lhs.ptr[u], b = rhs.ptr[u];
            static if (is(T : creal))
            {
                // Use rt.cmath2._Ccmp instead ?
                // Also: if NaN is present, numbers will appear equal.
                auto r = (a.re > b.re) - (a.re < b.re);
                if (!r) r = (a.im > b.im) - (a.im < b.im);
            }
            else
            {
                // This pattern for three-way comparison is better than conditional operators
                // See e.g. https://godbolt.org/z/3j4vh1
                const r = (a > b) - (a < b);
            }
            if (r) return r;
        }
        return (lhs.length > rhs.length) - (lhs.length < rhs.length);
    }
}

// This function is called by the compiler when dealing with array
// comparisons in the semantic analysis phase of CmpExp. The ordering
// comparison is lowered to a call to this template.
int __cmp(T1, T2)(T1[] s1, T2[] s2)
if (!__traits(isScalar, T1) && !__traits(isScalar, T2))
{
    import core.internal.traits : Unqual;
    alias U1 = Unqual!T1;
    alias U2 = Unqual!T2;

    static if (is(U1 == void) && is(U2 == void))
        static @trusted ref inout(ubyte) at(inout(void)[] r, size_t i) { return (cast(inout(ubyte)*) r.ptr)[i]; }
    else
        static @trusted ref R at(R)(R[] r, size_t i) { return r.ptr[i]; }

    // All unsigned byte-wide types = > dstrcmp
    immutable len = s1.length <= s2.length ? s1.length : s2.length;

    foreach (const u; 0 .. len)
    {
        static if (__traits(compiles, __cmp(at(s1, u), at(s2, u))))
        {
            auto c = __cmp(at(s1, u), at(s2, u));
            if (c != 0)
                return c;
        }
        else static if (__traits(compiles, at(s1, u).opCmp(at(s2, u))))
        {
            auto c = at(s1, u).opCmp(at(s2, u));
            if (c != 0)
                return c;
        }
        else static if (__traits(compiles, at(s1, u) < at(s2, u)))
        {
            if (int result = (at(s1, u) > at(s2, u)) - (at(s1, u) < at(s2, u)))
                return result;
        }
        else
        {
            // TODO: fix this legacy bad behavior, see
            // https://issues.dlang.org/show_bug.cgi?id=17244
            static assert(is(U1 == U2), "Internal error.");
            import core.stdc.string : memcmp;
            auto c = (() @trusted => memcmp(&at(s1, u), &at(s2, u), U1.sizeof))();
            if (c != 0)
                return c;
        }
    }
    return (s1.length > s2.length) - (s1.length < s2.length);
}

// integral types
@safe unittest
{
    void compareMinMax(T)()
    {
        T[2] a = [T.max, T.max];
        T[2] b = [T.min, T.min];

        assert(__cmp(a, b) > 0);
        assert(__cmp(b, a) < 0);
    }

    compareMinMax!int;
    compareMinMax!uint;
    compareMinMax!long;
    compareMinMax!ulong;
    compareMinMax!short;
    compareMinMax!ushort;
    compareMinMax!byte;
    compareMinMax!dchar;
    compareMinMax!wchar;
}

// char types (dstrcmp)
@safe unittest
{
    void compareMinMax(T)()
    {
        T[2] a = [T.max, T.max];
        T[2] b = [T.min, T.min];

        assert(__cmp(a, b) > 0);
        assert(__cmp(b, a) < 0);
    }

    compareMinMax!ubyte;
    compareMinMax!bool;
    compareMinMax!char;
    compareMinMax!(const char);

    string s1 = "aaaa";
    string s2 = "bbbb";
    assert(__cmp(s2, s1) > 0);
    assert(__cmp(s1, s2) < 0);
}

// fp types
@safe unittest
{
    void compareMinMax(T)()
    {
        T[2] a = [T.max, T.max];
        T[2] b = [T.min_normal, T.min_normal];
        T[2] c = [T.max, T.min_normal];
        T[1] d = [T.max];

        assert(__cmp(a, b) > 0);
        assert(__cmp(b, a) < 0);
        assert(__cmp(a, c) > 0);
        assert(__cmp(a, d) > 0);
        assert(__cmp(d, c) < 0);
        assert(__cmp(c, c) == 0);
    }

    compareMinMax!real;
    compareMinMax!float;
    compareMinMax!double;

    // qualifiers
    compareMinMax!(const real);
    compareMinMax!(immutable real);
}

// void[]
@safe unittest
{
    void[] a;
    const(void)[] b;

    (() @trusted
    {
        a = cast(void[]) "bb";
        b = cast(const(void)[]) "aa";
    })();

    assert(__cmp(a, b) > 0);
    assert(__cmp(b, a) < 0);
}

// arrays of arrays with mixed modifiers
@safe unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=17876
    bool less1(immutable size_t[][] a, size_t[][] b) { return a < b; }
    bool less2(const void[][] a, void[][] b) { return a < b; }
    bool less3(inout size_t[][] a, size_t[][] b) { return a < b; }

    immutable size_t[][] a = [[1, 2], [3, 4]];
    size_t[][] b = [[1, 2], [3, 5]];
    assert(less1(a, b));
    assert(less3(a, b));

    auto va = [cast(immutable void[])a[0], a[1]];
    auto vb = [cast(void[])b[0], b[1]];
    assert(less2(va, vb));
}
