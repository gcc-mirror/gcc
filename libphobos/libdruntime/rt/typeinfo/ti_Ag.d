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
module rt.typeinfo.ti_Ag;

private import core.stdc.string;
private import core.internal.string;

// byte[]

class TypeInfo_Ag : TypeInfo_Array
{
    override bool opEquals(Object o) { return TypeInfo.opEquals(o); }

    override string toString() const { return "byte[]"; }

    override size_t getHash(scope const void* p) @trusted const
    {
        const s = *cast(const void[]*)p;
        return hashOf(s);
    }

    override bool equals(in void* p1, in void* p2) const
    {
        byte[] s1 = *cast(byte[]*)p1;
        byte[] s2 = *cast(byte[]*)p2;

        return s1.length == s2.length &&
               memcmp(cast(byte *)s1, cast(byte *)s2, s1.length) == 0;
    }

    override int compare(in void* p1, in void* p2) const
    {
        byte[] s1 = *cast(byte[]*)p1;
        byte[] s2 = *cast(byte[]*)p2;
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
        return cast(inout)typeid(byte);
    }
}


// ubyte[]

class TypeInfo_Ah : TypeInfo_Ag
{
    override string toString() const { return "ubyte[]"; }

    override int compare(in void* p1, in void* p2) const
    {
        char[] s1 = *cast(char[]*)p1;
        char[] s2 = *cast(char[]*)p2;

        return dstrcmp(s1, s2);
    }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout)typeid(ubyte);
    }
}

// void[]

class TypeInfo_Av : TypeInfo_Ah
{
    override string toString() const { return "void[]"; }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout)typeid(void);
    }
}

// bool[]

class TypeInfo_Ab : TypeInfo_Ah
{
    override string toString() const { return "bool[]"; }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout)typeid(bool);
    }
}

// char[]

class TypeInfo_Aa : TypeInfo_Ah
{
    override string toString() const { return "char[]"; }

    override size_t getHash(scope const void* p) @trusted const
    {
        char[] s = *cast(char[]*)p;
        return hashOf(s);
    }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout)typeid(char);
    }
}

// string

class TypeInfo_Aya : TypeInfo_Aa
{
    override string toString() const { return "immutable(char)[]"; }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout)typeid(immutable(char));
    }
}

// const(char)[]

class TypeInfo_Axa : TypeInfo_Aa
{
    override string toString() const { return "const(char)[]"; }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout)typeid(const(char));
    }
}
