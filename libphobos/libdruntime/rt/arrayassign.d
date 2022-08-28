/**
 * Implementation of array assignment support routines.
 *
 *
 * Copyright: Copyright Digital Mars 2010 - 2016.
 * License:   Distributed under the
 *            $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 * Authors:   Walter Bright, Kenji Hara
 * Source:    $(DRUNTIMESRC rt/_arrayassign.d)
 */

module rt.arrayassign;

private
{
    import core.internal.util.array;
    import core.stdc.string;
    import core.stdc.stdlib;
    debug(PRINTF) import core.stdc.stdio;
}

/**
Set all elements of an array to a single value.

---
p[0 .. count] = value;
---

Takes into account postblits and destructors, for Plain Old Data elements,
`rt/memset.d` is used.

Params:
    p = pointer to start of array
    value = bytes of the element to set. Size is derived from `ti`.
    count = amount of array elements to set
    ti = type info of the array element type / `value`
Returns: `p`
*/
extern (C) void* _d_arraysetassign(void* p, void* value, int count, TypeInfo ti)
{
    void* pstart = p;

    auto element_size = ti.tsize;

    // Need a temporary buffer tmp[] big enough to hold one element
    immutable maxAllocaSize = 512;
    void *ptmp = (element_size > maxAllocaSize) ? malloc(element_size) : alloca(element_size);

    foreach (i; 0 .. count)
    {
        memcpy(ptmp, p, element_size);
        memcpy(p, value, element_size);
        ti.postblit(p);
        ti.destroy(ptmp);
        p += element_size;
    }
    if (element_size > maxAllocaSize)
        free(ptmp);
    return pstart;
}
