/**
 * TypeInfo support code.
 *
 * Copyright: Copyright Digital Mars 2004 - 2015.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright
 */

/*          Copyright Digital Mars 2004 - 2015.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module rt.typeinfo.ti_cent;

private import rt.util.hash;

static if (is(cent)):

// cent

class TypeInfo_zi : TypeInfo
{
    @trusted:
    const:
    pure:
    nothrow:

    override string toString() const pure nothrow @safe { return "cent"; }

    override size_t getHash(in void* p)
    {
        return rt.util.hash.hashOf(p[0 .. cent.sizeof], 0);
    }

    override bool equals(in void* p1, in void* p2)
    {
        return *cast(cent *)p1 == *cast(cent *)p2;
    }

    override int compare(in void* p1, in void* p2)
    {
        if (*cast(cent *)p1 < *cast(cent *)p2)
            return -1;
        else if (*cast(cent *)p1 > *cast(cent *)p2)
            return 1;
        return 0;
    }

    override @property size_t tsize() nothrow pure
    {
        return cent.sizeof;
    }

    override const(void)[] initializer() const @trusted
    {
        return (cast(void *)null)[0 .. cent.sizeof];
    }

    override void swap(void *p1, void *p2)
    {
        cent t;

        t = *cast(cent *)p1;
        *cast(cent *)p1 = *cast(cent *)p2;
        *cast(cent *)p2 = t;
    }

    override @property size_t talign() nothrow pure
    {
        return cent.alignof;
    }
}
