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
module rt.typeinfo.ti_long;

private import rt.util.hash;

// long

class TypeInfo_l : TypeInfo
{
    @trusted:
    const:
    pure:
    nothrow:

    override string toString() const pure nothrow @safe { return "long"; }

    override size_t getHash(in void* p)
    {
        return rt.util.hash.hashOf(p[0 .. long.sizeof], 0);
    }

    override bool equals(in void* p1, in void* p2)
    {
        return *cast(long *)p1 == *cast(long *)p2;
    }

    override int compare(in void* p1, in void* p2)
    {
        if (*cast(long *)p1 < *cast(long *)p2)
            return -1;
        else if (*cast(long *)p1 > *cast(long *)p2)
            return 1;
        return 0;
    }

    override @property size_t tsize() nothrow pure
    {
        return long.sizeof;
    }

    override const(void)[] initializer() const @trusted
    {
        return (cast(void *)null)[0 .. long.sizeof];
    }

    override void swap(void *p1, void *p2)
    {
        long t;

        t = *cast(long *)p1;
        *cast(long *)p1 = *cast(long *)p2;
        *cast(long *)p2 = t;
    }

    override @property size_t talign() nothrow pure
    {
        return long.alignof;
    }
}
