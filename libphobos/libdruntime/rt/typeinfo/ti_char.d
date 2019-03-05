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
module rt.typeinfo.ti_char;

// char

class TypeInfo_a : TypeInfo
{
    @trusted:
    const:
    pure:
    nothrow:

    override string toString() const pure nothrow @safe { return "char"; }

    override size_t getHash(scope const void* p)
    {
        return *cast(const char *)p;
    }

    override bool equals(in void* p1, in void* p2)
    {
        return *cast(char *)p1 == *cast(char *)p2;
    }

    override int compare(in void* p1, in void* p2)
    {
        return *cast(char *)p1 - *cast(char *)p2;
    }

    override @property size_t tsize() nothrow pure
    {
        return char.sizeof;
    }

    override void swap(void *p1, void *p2)
    {
        char t;

        t = *cast(char *)p1;
        *cast(char *)p1 = *cast(char *)p2;
        *cast(char *)p2 = t;
    }

    override const(void)[] initializer() const @trusted
    {
        static immutable char c;

        return (&c)[0 .. 1];
    }
}
