/**
 * TypeInfo support code.
 *
 * Copyright: Copyright Digital Mars 2016.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Kenji Hara
 */

/*          Copyright Digital Mars 2016.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module rt.typeinfo.ti_n;

// typeof(null)

class TypeInfo_n : TypeInfo
{
    override string toString() const @safe { return "typeof(null)"; }

    override size_t getHash(scope const void* p) const
    {
        return 0;
    }

    override bool equals(in void* p1, in void* p2) const @trusted
    {
        //return *cast(typeof(null)*)p1 is *cast(typeof(null)*)p2;
        return true;
    }

    override int compare(in void* p1, in void* p2) const @trusted
    {
        //if (*cast(int*) p1 < *cast(int*) p2)
        //    return -1;
        //else if (*cast(int*) p1 > *cast(int*) p2)
        //    return 1;
        return 0;
    }

    override @property size_t tsize() const
    {
        return typeof(null).sizeof;
    }

    override const(void)[] initializer() const @trusted
    {
        return (cast(void*)null)[0 .. typeof(null).sizeof];
    }

    override void swap(void *p1, void *p2) const @trusted
    {
        //auto t = *cast(typeof(null)*)p1;
        //*cast(typeof(null)*)p1 = *cast(typeof(null)*)p2;
        //*cast(typeof(null)*)p2 = t;
    }
}
