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

/*
 * Superseded array assignment hook. Does not take into account destructors:
 * https://issues.dlang.org/show_bug.cgi?id=13661
 * Kept for backward binary compatibility. This function can be removed in the future.
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
Does array assignment (not construction) from another array of the same
element type.

Handles overlapping copies.

The `_d_arrayassign_l` variant assumes the right hand side is an lvalue,
while `_d_arrayassign_r` assumes it's an rvalue, which means it doesn't have to call copy constructors.

Used for static array assignment with non-POD element types:
---
struct S
{
    ~this() {} // destructor, so not Plain Old Data
}

void main()
{
    S[3] arr;
    S[3] lvalue;

    arr = lvalue;
    // Generates:
    // S _tmp;
    // _d_arrayassign_l(typeid(S), (cast(void*) lvalue.ptr)[0..lvalue.length], (cast(void*) arr.ptr)[0..arr.length], &_tmp);

    S[3] getRvalue() {return lvalue;}
    arr = getRvalue();
    // Similar, but `_d_arrayassign_r`
}
---

Params:
    ti = `TypeInfo` of the array element type.
    dst = target memory. Its `.length` is equal to the element count, not byte length.
    src = source memory. Its `.length` is equal to the element count, not byte length.
    ptmp =  Temporary memory for element swapping, must have capacity of `ti.tsize` bytes.
Returns: `dst`
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

/// ditto
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
