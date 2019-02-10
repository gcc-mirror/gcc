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
module rt.typeinfo.ti_short;

// short

class TypeInfo_s : TypeInfo
{
    @trusted:
    const:
    pure:
    nothrow:

    override string toString() const pure nothrow @safe { return "short"; }

    override size_t getHash(scope const void* p)
    {
        return *cast(const short *)p;
    }

    override bool equals(in void* p1, in void* p2)
    {
        return *cast(short *)p1 == *cast(short *)p2;
    }

    override int compare(in void* p1, in void* p2)
    {
        return *cast(short *)p1 - *cast(short *)p2;
    }

    override @property size_t tsize() nothrow pure
    {
        return short.sizeof;
    }

    override const(void)[] initializer() const @trusted
    {
        return (cast(void *)null)[0 .. short.sizeof];
    }

    override void swap(void *p1, void *p2)
    {
        short t;

        t = *cast(short *)p1;
        *cast(short *)p1 = *cast(short *)p2;
        *cast(short *)p2 = t;
    }
}
