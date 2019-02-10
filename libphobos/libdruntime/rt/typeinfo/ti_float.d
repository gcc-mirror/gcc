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
module rt.typeinfo.ti_float;

private import rt.util.typeinfo;

// float

class TypeInfo_f : TypeInfo
{
  pure:
  nothrow:
  @safe:

    alias F = float;

    override string toString() const { return F.stringof; }

    override size_t getHash(scope const void* p) const @trusted
    {
        return Floating!F.hashOf(*cast(F*)p);
    }

    override bool equals(in void* p1, in void* p2) const @trusted
    {
        return Floating!F.equals(*cast(F*)p1, *cast(F*)p2);
    }

    override int compare(in void* p1, in void* p2) const @trusted
    {
        return Floating!F.compare(*cast(F*)p1, *cast(F*)p2);
    }

    override @property size_t tsize() const
    {
        return F.sizeof;
    }

    override void swap(void *p1, void *p2) const @trusted
    {
        F t = *cast(F*)p1;
        *cast(F*)p1 = *cast(F*)p2;
        *cast(F*)p2 = t;
    }

    override const(void)[] initializer() const @trusted
    {
        static immutable F r;
        return (&r)[0 .. 1];
    }

    version (Windows)
    {
    }
    else version (X86_64)
    {
        // 2 means arg to function is passed in XMM registers
        override @property uint flags() const { return 2; }
    }
}
