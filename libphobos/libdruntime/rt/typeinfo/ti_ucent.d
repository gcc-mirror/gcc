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
module rt.typeinfo.ti_ucent;

static if (is(ucent)):

// ucent

class TypeInfo_zk : TypeInfo
{
    @trusted:
    const:
    pure:
    nothrow:

    override string toString() const pure nothrow @safe { return "ucent"; }

    override size_t getHash(scope const void* p)
    {
        return hashOf(*cast(const ucent*) p);
    }

    override bool equals(in void* p1, in void* p2)
    {
        return *cast(ucent *)p1 == *cast(ucent *)p2;
    }

    override int compare(in void* p1, in void* p2)
    {
        if (*cast(ucent *)p1 < *cast(ucent *)p2)
            return -1;
        else if (*cast(ucent *)p1 > *cast(ucent *)p2)
            return 1;
        return 0;
    }

    override @property size_t tsize() nothrow pure
    {
        return ucent.sizeof;
    }

    override const(void)[] initializer() const @trusted
    {
        return (cast(void *)null)[0 .. ucent.sizeof];
    }

    override void swap(void *p1, void *p2)
    {
        ucent t;

        t = *cast(ucent *)p1;
        *cast(ucent *)p1 = *cast(ucent *)p2;
        *cast(ucent *)p2 = t;
    }

    override @property size_t talign() nothrow pure
    {
        return ucent.alignof;
    }
}
