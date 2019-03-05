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
module rt.typeinfo.ti_ubyte;

// ubyte

class TypeInfo_h : TypeInfo
{
    @trusted:
    const:
    pure:
    nothrow:

    override string toString() const pure nothrow @safe { return "ubyte"; }

    override size_t getHash(scope const void* p)
    {
        return *cast(const ubyte *)p;
    }

    override bool equals(in void* p1, in void* p2)
    {
        return *cast(ubyte *)p1 == *cast(ubyte *)p2;
    }

    override int compare(in void* p1, in void* p2)
    {
        return *cast(ubyte *)p1 - *cast(ubyte *)p2;
    }

    override @property size_t tsize() nothrow pure
    {
        return ubyte.sizeof;
    }

    override const(void)[] initializer() const @trusted
    {
        return (cast(void *)null)[0 .. ubyte.sizeof];
    }

    override void swap(void *p1, void *p2)
    {
        ubyte t;

        t = *cast(ubyte *)p1;
        *cast(ubyte *)p1 = *cast(ubyte *)p2;
        *cast(ubyte *)p2 = t;
    }
}

class TypeInfo_b : TypeInfo_h
{
    @trusted:
    const:
    pure:
    nothrow:

    override string toString() const pure nothrow @safe { return "bool"; }
}
