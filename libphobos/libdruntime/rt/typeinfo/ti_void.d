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
module rt.typeinfo.ti_void;

// void

class TypeInfo_v : TypeInfo
{
    @trusted:
    const:
    pure:
    nothrow:

    override string toString() const pure nothrow @safe { return "void"; }

    override size_t getHash(scope const void* p)
    {
        assert(0);
    }

    override bool equals(in void* p1, in void* p2)
    {
        return *cast(byte *)p1 == *cast(byte *)p2;
    }

    override int compare(in void* p1, in void* p2)
    {
        return *cast(byte *)p1 - *cast(byte *)p2;
    }

    override @property size_t tsize() nothrow pure
    {
        return void.sizeof;
    }

    override const(void)[] initializer() const @trusted
    {
        return (cast(void *)null)[0 .. void.sizeof];
    }

    override void swap(void *p1, void *p2)
    {
        byte t;

        t = *cast(byte *)p1;
        *cast(byte *)p1 = *cast(byte *)p2;
        *cast(byte *)p2 = t;
    }

    override @property uint flags() nothrow pure
    {
        return 1;
    }
}
