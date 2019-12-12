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
module rt.typeinfo.ti_uint;

// uint

class TypeInfo_k : TypeInfo
{
    @trusted:
    const:
    pure:
    nothrow:

    override string toString() const pure nothrow @safe { return "uint"; }

    override size_t getHash(scope const void* p)
    {
        return *cast(const uint *)p;
    }

    override bool equals(in void* p1, in void* p2)
    {
        return *cast(uint *)p1 == *cast(uint *)p2;
    }

    override int compare(in void* p1, in void* p2)
    {
        if (*cast(uint*) p1 < *cast(uint*) p2)
            return -1;
        else if (*cast(uint*) p1 > *cast(uint*) p2)
            return 1;
        return 0;
    }

    override @property size_t tsize() nothrow pure
    {
        return uint.sizeof;
    }

    override const(void)[] initializer() const @trusted
    {
        return (cast(void *)null)[0 .. uint.sizeof];
    }

    override void swap(void *p1, void *p2)
    {
        int t;

        t = *cast(uint *)p1;
        *cast(uint *)p1 = *cast(uint *)p2;
        *cast(uint *)p2 = t;
    }
}
