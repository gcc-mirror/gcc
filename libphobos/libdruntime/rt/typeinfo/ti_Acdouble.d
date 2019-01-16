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
module rt.typeinfo.ti_Acdouble;

private import rt.util.typeinfo;

// cdouble[]

class TypeInfo_Ar : TypeInfo_Array
{
    alias F = cdouble;

    override bool opEquals(Object o) { return TypeInfo.opEquals(o); }

    override string toString() const { return (F[]).stringof; }

    override size_t getHash(in void* p) @trusted const
    {
        return Array!F.hashOf(*cast(F[]*)p);
    }

    override bool equals(in void* p1, in void* p2) const
    {
        return Array!F.equals(*cast(F[]*)p1, *cast(F[]*)p2);
    }

    override int compare(in void* p1, in void* p2) const
    {
        return Array!F.compare(*cast(F[]*)p1, *cast(F[]*)p2);
    }

    override @property inout(TypeInfo) next() inout
    {
        return cast(inout)typeid(F);
    }
}
