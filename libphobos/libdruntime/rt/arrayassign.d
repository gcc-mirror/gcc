/**
 * Implementation of array assignment support routines.
 *
 *
 * Copyright: Copyright Digital Mars 2010 - 2016.
 * License:   Distributed under the
 *            $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 * Authors:   Walter Bright, Kenji Hara
 * Source:    $(DRUNTIMESRC src/rt/_arrayassign.d)
 */

module rt.arrayassign;

private
{
    import rt.util.array;
    import core.stdc.string;
    import core.stdc.stdlib;
    debug(PRINTF) import core.stdc.stdio;
}

/**
 * Keep for backward binary compatibility. This function can be removed in the future.
 */
extern (C) void[] _d_arrayassign(TypeInfo ti, void[] from, void[] to)
{
    debug(PRINTF) printf("_d_arrayassign(from = %p,%d, to = %p,%d) size = %d\n", from.ptr, from.length, to.ptr, to.length, ti.tsize);

    immutable elementSize = ti.tsize;

    // Need a temporary buffer tmp[] big enough to hold one element
    void[16] buf = void;
    void* ptmp = (elementSize > buf.sizeof) ? malloc(elementSize) : buf.ptr;
    scope (exit)
    {
        if (ptmp != buf.ptr)
            free(ptmp);
    }
    return _d_arrayassign_l(ti, from, to, ptmp);
}

/**
 * Does array assignment (not construction) from another
 * lvalue array of the same element type.
 * Handles overlapping copies.
 * Input:
 *      ti      TypeInfo of the element type.
 *      dst     Points target memory. Its .length is equal to the element count, not byte length.
 *      src     Points source memory. Its .length is equal to the element count, not byte length.
 *      ptmp    Temporary memory for element swapping.
 */
extern (C) void[] _d_arrayassign_l(TypeInfo ti, void[] src, void[] dst, void* ptmp)
{
    debug(PRINTF) printf("_d_arrayassign_l(src = %p,%d, dst = %p,%d) size = %d\n", src.ptr, src.length, dst.ptr, dst.length, ti.tsize);

    immutable elementSize = ti.tsize;

    enforceRawArraysConformable("copy", elementSize, src, dst, true);

    if (src.ptr < dst.ptr && dst.ptr < src.ptr + elementSize * src.length)
    {
        // If dst is in the middle of src memory, use reverse order.
        for (auto i = dst.length; i--; )
        {
            void* pdst = dst.ptr + i * elementSize;
            void* psrc = src.ptr + i * elementSize;
            memcpy(ptmp, pdst, elementSize);
            memcpy(pdst, psrc, elementSize);
            ti.postblit(pdst);
            ti.destroy(ptmp);
        }
    }
    else
    {
        // Otherwise, use normal order.
        foreach (i; 0 .. dst.length)
        {
            void* pdst = dst.ptr + i * elementSize;
            void* psrc = src.ptr + i * elementSize;
            memcpy(ptmp, pdst, elementSize);
            memcpy(pdst, psrc, elementSize);
            ti.postblit(pdst);
            ti.destroy(ptmp);
        }
    }
    return dst;
}

unittest    // Bugzilla 14024
{
    string op;

    struct S
    {
        char x = 'x';
        this(this) { op ~= x-0x20; }    // upper case
        ~this()    { op ~= x; }         // lower case
    }

    S[4] mem;
    ref S[2] slice(int a, int b) { return mem[a .. b][0 .. 2]; }

    op = null;
    mem[0].x = 'a';
    mem[1].x = 'b';
    mem[2].x = 'x';
    mem[3].x = 'y';
    slice(0, 2) = slice(2, 4);  // [ab] = [xy]
    assert(op == "XaYb", op);

    op = null;
    mem[0].x = 'x';
    mem[1].x = 'y';
    mem[2].x = 'a';
    mem[3].x = 'b';
    slice(2, 4) = slice(0, 2);  // [ab] = [xy]
    assert(op == "XaYb", op);

    op = null;
    mem[0].x = 'a';
    mem[1].x = 'b';
    mem[2].x = 'c';
    slice(0, 2) = slice(1, 3);  // [ab] = [bc]
    assert(op == "BaCb", op);

    op = null;
    mem[0].x = 'x';
    mem[1].x = 'y';
    mem[2].x = 'z';
    slice(1, 3) = slice(0, 2);  // [yz] = [xy]
    assert(op == "YzXy", op);
}

/**
 * Does array assignment (not construction) from another
 * rvalue array of the same element type.
 * Input:
 *      ti      TypeInfo of the element type.
 *      dst     Points target memory. Its .length is equal to the element count, not byte length.
 *      src     Points source memory. Its .length is equal to the element count, not byte length.
 *              It is always allocated on stack and never overlapping with dst.
 *      ptmp    Temporary memory for element swapping.
 */
extern (C) void[] _d_arrayassign_r(TypeInfo ti, void[] src, void[] dst, void* ptmp)
{
    debug(PRINTF) printf("_d_arrayassign_r(src = %p,%d, dst = %p,%d) size = %d\n", src.ptr, src.length, dst.ptr, dst.length, ti.tsize);

    immutable elementSize = ti.tsize;

    enforceRawArraysConformable("copy", elementSize, src, dst, false);

    // Always use normal order, because we can assume that
    // the rvalue src has no overlapping with dst.
    foreach (i; 0 .. dst.length)
    {
        void* pdst = dst.ptr + i * elementSize;
        void* psrc = src.ptr + i * elementSize;
        memcpy(ptmp, pdst, elementSize);
        memcpy(pdst, psrc, elementSize);
        ti.destroy(ptmp);
    }
    return dst;
}

/**
 * Does array initialization (not assignment) from another
 * array of the same element type.
 * ti is the element type.
 */
extern (C) void[] _d_arrayctor(TypeInfo ti, void[] from, void[] to)
{
    debug(PRINTF) printf("_d_arrayctor(from = %p,%d, to = %p,%d) size = %d\n", from.ptr, from.length, to.ptr, to.length, ti.tsize);


    auto element_size = ti.tsize;

    enforceRawArraysConformable("initialization", element_size, from, to);

    size_t i;
    try
    {
        for (i = 0; i < to.length; i++)
        {
            // Copy construction is defined as bit copy followed by postblit.
            memcpy(to.ptr + i * element_size, from.ptr + i * element_size, element_size);
            ti.postblit(to.ptr + i * element_size);
        }
    }
    catch (Throwable o)
    {
        /* Destroy, in reverse order, what we've constructed so far
         */
        while (i--)
        {
            ti.destroy(to.ptr + i * element_size);
        }

        throw o;
    }
    return to;
}


/**
 * Do assignment to an array.
 *      p[0 .. count] = value;
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

/**
 * Do construction of an array.
 *      ti[count] p = value;
 */
extern (C) void* _d_arraysetctor(void* p, void* value, int count, TypeInfo ti)
{
    void* pstart = p;
    auto element_size = ti.tsize;

    try
    {
        foreach (i; 0 .. count)
        {
            // Copy construction is defined as bit copy followed by postblit.
            memcpy(p, value, element_size);
            ti.postblit(p);
            p += element_size;
        }
    }
    catch (Throwable o)
    {
        // Destroy, in reverse order, what we've constructed so far
        while (p > pstart)
        {
            p -= element_size;
            ti.destroy(p);
        }

        throw o;
    }
    return pstart;
}
