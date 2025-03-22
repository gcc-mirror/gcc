/**
 * Implementation of invariant support routines.
 *
 * Copyright: Copyright Digital Mars 2007 - 2010.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright
 * Source: $(DRUNTIMESRC rt/_invariant_.d)
 */

module rt.invariant_;

/**
 *
 */
void _d_invariant(Object o)
{   ClassInfo c;

    //printf("__d_invariant(%p)\n", o);

    // BUG: needs to be filename/line of caller, not library routine
    assert(o !is null); // just do null check, not invariant check

    c = typeid(o);
    do
    {
        if (c.classInvariant)
        {
            (*c.classInvariant)(o);
        }
        c = c.base;
    } while (c);
}
