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
module rt.typeinfo.ti_cdouble;

private import rt.util.typeinfo;

// cdouble

class TypeInfo_r : TypeInfo
{
  pure:
  nothrow:
  @safe:

    alias F = cdouble;

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

    override @property size_t talign() const
    {
        return F.alignof;
    }

    version (X86_64) override int argTypes(out TypeInfo arg1, out TypeInfo arg2)
    {
        arg1 = typeid(double);
        arg2 = typeid(double);
        return 0;
    }
}
