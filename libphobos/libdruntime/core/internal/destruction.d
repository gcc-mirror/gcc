/**
 This module contains implementations for destroying instances of types

  Copyright: Copyright Digital Mars 2000 - 2019.
  License: Distributed under the
       $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
     (See accompanying file LICENSE)
  Source: $(DRUNTIMESRC core/_internal/_destruction.d)
*/
module core.internal.destruction;

// compiler frontend lowers dynamic array deconstruction to this
void __ArrayDtor(T)(scope T[] a)
{
    foreach_reverse (ref T e; a)
        e.__xdtor();
}

public void destructRecurse(E, size_t n)(ref E[n] arr)
{
    import core.internal.traits : hasElaborateDestructor;

    static if (hasElaborateDestructor!E)
    {
        foreach_reverse (ref elem; arr)
            destructRecurse(elem);
    }
}

public void destructRecurse(S)(ref S s)
    if (is(S == struct))
{
    static if (__traits(hasMember, S, "__xdtor") &&
            // Bugzilla 14746: Check that it's the exact member of S.
            __traits(isSame, S, __traits(parent, s.__xdtor)))
        s.__xdtor();
}

// Test static struct
nothrow @safe @nogc unittest
{
    static int i = 0;
    static struct S { ~this() nothrow @safe @nogc { i = 42; } }
    S s;
    destructRecurse(s);
    assert(i == 42);
}
