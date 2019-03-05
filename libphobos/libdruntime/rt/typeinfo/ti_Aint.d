/**
 * TypeInfo support code.
 *
 * Copyright: Copyright Digital Mars 2004 - 2009.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright
 */

/*          Copyright Digital Mars 2004 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module rt.typeinfo.ti_Aint;

private import core.stdc.string;

extern (C) void[] _adSort(void[] a, TypeInfo ti);

// int[]

class TypeInfo_Ai : TypeInfo_Array
{
    override bool opEquals(Object o) { return TypeInfo.opEquals(o); }

    override string toString() const { return "int[]"; }

    override size_t getHash(scope const void* p) @trusted const
    {
        // Hash as if unsigned.
        const s = *cast(const uint[]*)p;
        return hashOf(s);
    }

    override bool equals(in void* p1, in void* p2) const
    {
        int[] s1 = *cast(int[]*)p1;
        int[] s2 = *cast(int[]*)p2;

        return s1.length == s2.length &&
               memcmp(cast(void *)s1, cast(void *)s2, s1.length * int.sizeof) == 0;
    }

    override int compare(in void* p1, in void* p2) const
    {
        int[] s1 = *cast(int[]*)p1;
        int[] s2 = *cast(int[]*)p2;
        size_t len = s1.length;

        if (s2.length < len)
            len = s2.length;
        for (size_t u = 0; u < len; u++)
        {
            if (s1[u] < s2[u])
                return -1;
            else if (s1[u] > s2[u])
                return 1;
        }
        if (s1.length < s2.length)
            return -1;
        else if (s1.length > s2.length)
            return 1;
        return 0;
    }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout)typeid(int);
    }
}

unittest
{
    int[][] a = [[5,3,8,7], [2,5,3,8,7]];
    _adSort(*cast(void[]*)&a, typeid(a[0]));
    assert(a == [[2,5,3,8,7], [5,3,8,7]]);

    a = [[5,3,8,7], [5,3,8]];
    _adSort(*cast(void[]*)&a, typeid(a[0]));
    assert(a == [[5,3,8], [5,3,8,7]]);
}

unittest
{
    // Issue 13073: original code uses int subtraction which is susceptible to
    // integer overflow, causing the following case to fail.
    int[] a = [int.max, int.max];
    int[] b = [int.min, int.min];
    assert(a > b);
    assert(b < a);
}

// uint[]

class TypeInfo_Ak : TypeInfo_Ai
{
    override string toString() const { return "uint[]"; }

    override int compare(in void* p1, in void* p2) const
    {
        uint[] s1 = *cast(uint[]*)p1;
        uint[] s2 = *cast(uint[]*)p2;
        size_t len = s1.length;

        if (s2.length < len)
            len = s2.length;
        for (size_t u = 0; u < len; u++)
        {
            if (s1[u] < s2[u])
                return -1;
            else if (s1[u] > s2[u])
                return 1;
        }
        if (s1.length < s2.length)
            return -1;
        else if (s1.length > s2.length)
            return 1;
        return 0;
    }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout)typeid(uint);
    }
}

unittest
{
    // Original test case from issue 13073
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

// dchar[]

class TypeInfo_Aw : TypeInfo_Ak
{
    override string toString() const { return "dchar[]"; }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout)typeid(dchar);
    }
}
