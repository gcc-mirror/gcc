/**
 * This module contains compiler support determining equality of arrays.
 *
 * Copyright: Copyright Digital Mars 2000 - 2020.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Source: $(DRUNTIMESRC core/internal/_array/_equality.d)
 */

module core.internal.array.equality;

// The compiler lowers `lhs == rhs` to `__equals(lhs, rhs)` for
// * dynamic arrays,
// * (most) arrays of different (unqualified) element types, and
// * arrays of structs with custom opEquals.
bool __equals(T1, T2)(scope T1[] lhs, scope T2[] rhs) @trusted
{
    if (lhs.length != rhs.length)
        return false;

    if (lhs.length == 0)
        return true;

    alias PureType = bool function(scope T1[], scope T2[], size_t) @safe pure nothrow @nogc;

    return (cast(PureType)&isEqual!(T1,T2))(lhs, rhs, lhs.length);
}

pragma(inline, true)
bool __equals(T1, T2, size_t N)(scope ref T1[N] lhs, scope T2[] rhs) @trusted {
    return __equals(lhs[], rhs);
}

pragma(inline, true)
bool __equals(T1, T2, size_t N)(scope T1[] lhs, scope ref T2[N] rhs) @trusted {
    return __equals(lhs, rhs[]);
}

pragma(inline, true)
bool __equals(T1, T2, size_t N, size_t M)(scope ref T1[N] lhs, scope ref T2[M] rhs) @trusted {
    return __equals(lhs[], rhs[]);
}

/******************************
 * Helper function for __equals().
 * Outlined to enable __equals() to be inlined, as dmd cannot inline loops.
 */
private
bool isEqual(T1, T2)(scope T1[] lhs, scope T2[] rhs, size_t length)
{
    // Returns a reference to an array element, eliding bounds check and
    // casting void to ubyte.
    pragma(inline, true)
    static ref at(T)(scope T[] r, size_t i) @trusted
        // exclude opaque structs due to https://issues.dlang.org/show_bug.cgi?id=20959
        if (!(is(T == struct) && !is(typeof(T.sizeof))))
    {
        static if (is(T == void))
            return (cast(ubyte[]) r)[i];
        else
            return r[i];
    }

    foreach (const i; 0 .. length)
    {
        if (at(lhs, i) != at(rhs, i))
            return false;
    }
    return true;
}

@safe unittest
{
    assert(__equals([], []));
    assert(!__equals([1, 2], [1, 2, 3]));
}

@safe unittest
{
    auto a = "hello"c;

    assert(a != "hel");
    assert(a != "helloo");
    assert(a != "betty");
    assert(a == "hello");
    assert(a != "hxxxx");

    float[] fa = [float.nan];
    assert(fa != fa);
}

@safe unittest
{
    struct A
    {
        int a;
    }

    auto arr1 = [A(0), A(2)];
    auto arr2 = [A(0), A(1)];
    auto arr3 = [A(0), A(1)];

    assert(arr1 != arr2);
    assert(arr2 == arr3);
}

@safe unittest
{
    struct A
    {
        int a;
        int b;

        bool opEquals(const A other)
        {
            return this.a == other.b && this.b == other.a;
        }
    }

    auto arr1 = [A(1, 0), A(0, 1)];
    auto arr2 = [A(1, 0), A(0, 1)];
    auto arr3 = [A(0, 1), A(1, 0)];

    assert(arr1 != arr2);
    assert(arr2 == arr3);
}

// https://issues.dlang.org/show_bug.cgi?id=18252
@safe unittest
{
    string[int][] a1, a2;
    assert(__equals(a1, a2));
    assert(a1 == a2);
    a1 ~= [0: "zero"];
    a2 ~= [0: "zero"];
    assert(__equals(a1, a2));
    assert(a1 == a2);
    a2[0][1] = "one";
    assert(!__equals(a1, a2));
    assert(a1 != a2);
}

// https://issues.dlang.org/show_bug.cgi?id=21094
unittest
{
    static class C
    {
        int a;
    }
    static struct S
    {
        bool isValid;
        C fib;

        inout(C) get() pure @safe @nogc nothrow inout
        {
            return isValid ? fib : C.init;
        }
        T opCast(T : C)() const { return null; }

        alias get this;
    }

    auto foo(S[] lhs, S[] rhs)
    {
        return lhs == rhs;
    }
}
