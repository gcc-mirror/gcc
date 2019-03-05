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
module rt.typeinfo.ti_dchar;

// dchar

class TypeInfo_w : TypeInfo
{
    @trusted:
    const:
    pure:
    nothrow:

    override string toString() const pure nothrow @safe { return "dchar"; }

    override size_t getHash(scope const void* p)
    {
        return *cast(const dchar *)p;
    }

    override bool equals(in void* p1, in void* p2)
    {
        return *cast(dchar *)p1 == *cast(dchar *)p2;
    }

    override int compare(in void* p1, in void* p2)
    {
        return *cast(dchar *)p1 - *cast(dchar *)p2;
    }

    override @property size_t tsize() nothrow pure
    {
        return dchar.sizeof;
    }

    override void swap(void *p1, void *p2)
    {
        dchar t;

        t = *cast(dchar *)p1;
        *cast(dchar *)p1 = *cast(dchar *)p2;
        *cast(dchar *)p2 = t;
    }

    override const(void)[] initializer() const @trusted
    {
        static immutable dchar c;

        return (&c)[0 .. 1];
    }
}
