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
module rt.typeinfo.ti_Ashort;

private import core.stdc.string;

// short[]

class TypeInfo_As : TypeInfo_Array
{
    override bool opEquals(Object o) { return TypeInfo.opEquals(o); }

    override string toString() const { return "short[]"; }

    override size_t getHash(scope const void* p) @trusted const
    {
        // Hash as if unsigned.
        const s = *cast(const ushort[]*)p;
        return hashOf(s);
    }

    override bool equals(in void* p1, in void* p2) const
    {
        short[] s1 = *cast(short[]*)p1;
        short[] s2 = *cast(short[]*)p2;

        return s1.length == s2.length &&
               memcmp(cast(void *)s1, cast(void *)s2, s1.length * short.sizeof) == 0;
    }

    override int compare(in void* p1, in void* p2) const
    {
        short[] s1 = *cast(short[]*)p1;
        short[] s2 = *cast(short[]*)p2;
        size_t len = s1.length;

        if (s2.length < len)
            len = s2.length;
        for (size_t u = 0; u < len; u++)
        {
            int result = s1[u] - s2[u];
            if (result)
                return result;
        }
        if (s1.length < s2.length)
            return -1;
        else if (s1.length > s2.length)
            return 1;
        return 0;
    }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout)typeid(short);
    }
}


// ushort[]

class TypeInfo_At : TypeInfo_As
{
    override string toString() const { return "ushort[]"; }

    override int compare(in void* p1, in void* p2) const
    {
        ushort[] s1 = *cast(ushort[]*)p1;
        ushort[] s2 = *cast(ushort[]*)p2;
        size_t len = s1.length;

        if (s2.length < len)
            len = s2.length;
        for (size_t u = 0; u < len; u++)
        {
            int result = s1[u] - s2[u];
            if (result)
                return result;
        }
        if (s1.length < s2.length)
            return -1;
        else if (s1.length > s2.length)
            return 1;
        return 0;
    }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout)typeid(ushort);
    }
}

// wchar[]

class TypeInfo_Au : TypeInfo_At
{
    override string toString() const { return "wchar[]"; }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout)typeid(wchar);
    }
}
