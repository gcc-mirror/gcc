/**
 * This module contains all functions related to an object's lifetime:
 * allocation, resizing, deallocation, and finalization.
 *
 * Copyright: Copyright Digital Mars 2000 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Walter Bright, Sean Kelly, Steven Schveighoffer
 * Source: $(DRUNTIMESRC rt/_lifetime.d)
 */

module rt.lifetime;

import core.attribute : weak;
import core.memory;
import core.internal.gc.blockmeta : PAGESIZE;
debug(PRINTF) import core.stdc.stdio;
static import rt.tlsgc;

alias BlkInfo = GC.BlkInfo;
alias BlkAttr = GC.BlkAttr;

// for now, all GC array functions are not exposed via core.memory.
extern(C) {
    void[] gc_getArrayUsed(void *ptr, bool atomic) nothrow;
    bool gc_expandArrayUsed(void[] slice, size_t newUsed, bool atomic) nothrow;
    size_t gc_reserveArrayCapacity(void[] slice, size_t request, bool atomic) nothrow;
    bool gc_shrinkArrayUsed(void[] slice, size_t existingUsed, bool atomic) nothrow;
}

private
{
    alias bool function(Object) CollectHandler;
    __gshared CollectHandler collectHandler = null;

    extern (C) void _d_monitordelete(Object h, bool det);

}

// Now-removed symbol, kept around for ABI
// Some programs are dynamically linked, so best to err on the side of keeping symbols around for a while (especially extern(C) ones)
// https://github.com/dlang/druntime/pull/3361
deprecated extern (C) void lifetime_init()
{
}

/**
Allocate memory using the garbage collector

DMD uses this to allocate closures:
---
void f(byte[24] x)
{
    return () => x; // `x` is on stack, must be moved to heap to keep it alive
}
---

Params:
    sz = number of bytes to allocate

Returns: pointer to `sz` bytes of free, uninitialized memory, managed by the GC.
*/
extern (C) void* _d_allocmemory(size_t sz) @weak
{
    return GC.malloc(sz);
}

/**
Create a new class instance.

Allocates memory and sets fields to their initial value, but does not call a constructor.

---
new Object() // _d_newclass(typeid(Object))
---
Params:
    ci = `TypeInfo_Class` object, to provide instance size and initial bytes to copy

Returns: newly created object
*/
extern (C) Object _d_newclass(const ClassInfo ci) @weak
{
    import core.stdc.stdlib;
    import core.exception : onOutOfMemoryError;
    void* p;
    auto init = ci.initializer;

    debug(PRINTF) printf("_d_newclass(ci = %p, %s)\n", ci, cast(char *)ci.name);
    if (ci.m_flags & TypeInfo_Class.ClassFlags.isCOMclass)
    {   /* COM objects are not garbage collected, they are reference counted
         * using AddRef() and Release().  They get free'd by C's free()
         * function called by Release() when Release()'s reference count goes
         * to zero.
     */
        p = malloc(init.length);
        if (!p)
            onOutOfMemoryError();
    }
    else
    {
        // TODO: should this be + 1 to avoid having pointers to the next block?
        BlkAttr attr = BlkAttr.NONE;
        // extern(C++) classes don't have a classinfo pointer in their vtable so the GC can't finalize them
        if (ci.m_flags & TypeInfo_Class.ClassFlags.hasDtor
            && !(ci.m_flags & TypeInfo_Class.ClassFlags.isCPPclass))
            attr |= BlkAttr.FINALIZE;
        if (ci.m_flags & TypeInfo_Class.ClassFlags.noPointers)
            attr |= BlkAttr.NO_SCAN;
        p = GC.malloc(init.length, attr, ci);
        debug(PRINTF) printf(" p = %p\n", p);
    }

    debug(PRINTF)
    {
        printf("p = %p\n", p);
        printf("ci = %p, ci.init.ptr = %p, len = %llu\n", ci, init.ptr, cast(ulong)init.length);
        printf("vptr = %p\n", *cast(void**) init);
        printf("vtbl[0] = %p\n", (*cast(void***) init)[0]);
        printf("vtbl[1] = %p\n", (*cast(void***) init)[1]);
        printf("init[0] = %x\n", (cast(uint*) init)[0]);
        printf("init[1] = %x\n", (cast(uint*) init)[1]);
        printf("init[2] = %x\n", (cast(uint*) init)[2]);
        printf("init[3] = %x\n", (cast(uint*) init)[3]);
        printf("init[4] = %x\n", (cast(uint*) init)[4]);
    }

    // initialize it
    p[0 .. init.length] = cast(void[]) init[];

    debug(PRINTF) printf("initialization done\n");
    return cast(Object) p;
}


/**
 *
 */
extern (C) void _d_delinterface(void** p)
{
    if (*p)
    {
        Interface* pi = **cast(Interface ***)*p;
        Object     o  = cast(Object)(*p - pi.offset);

        _d_delclass(&o);
        *p = null;
    }
}


// used for deletion
private extern (D) alias void function (Object) fp_t;


/**
 *
 */
extern (C) void _d_delclass(Object* p) @weak
{
    if (*p)
    {
        debug(PRINTF) printf("_d_delclass(%p)\n", *p);

        ClassInfo **pc = cast(ClassInfo **)*p;
        if (*pc)
        {
            ClassInfo c = **pc;

            rt_finalize(cast(void*) *p);

            if (c.deallocator)
            {
                fp_t fp = cast(fp_t)c.deallocator;
                (*fp)(*p); // call deallocator
                *p = null;
                return;
            }
        }
        else
        {
            rt_finalize(cast(void*) *p);
        }
        GC.free(cast(void*) *p);
        *p = null;
    }
}

// strip const/immutable/shared/inout from type info
inout(TypeInfo) unqualify(return scope inout(TypeInfo) cti) pure nothrow @nogc
{
    TypeInfo ti = cast() cti;
    while (ti)
    {
        // avoid dynamic type casts
        auto tti = typeid(ti);
        if (tti is typeid(TypeInfo_Const))
            ti = (cast(TypeInfo_Const)cast(void*)ti).base;
        else if (tti is typeid(TypeInfo_Invariant))
            ti = (cast(TypeInfo_Invariant)cast(void*)ti).base;
        else if (tti is typeid(TypeInfo_Shared))
            ti = (cast(TypeInfo_Shared)cast(void*)ti).base;
        else if (tti is typeid(TypeInfo_Inout))
            ti = (cast(TypeInfo_Inout)cast(void*)ti).base;
        else
            break;
    }
    return ti;
}

private uint __typeAttrs(const scope TypeInfo ti, void *copyAttrsFrom = null) pure nothrow
{
    if (copyAttrsFrom)
    {
        // try to copy attrs from the given block
        auto info = GC.query(copyAttrsFrom);
        if (info.base)
            return info.attr;
    }
    uint attrs = !(ti.flags & 1) ? BlkAttr.NO_SCAN : 0;
    if (typeid(ti) is typeid(TypeInfo_Struct)) {
        auto sti = cast(TypeInfo_Struct)cast(void*)ti;
        if (sti.xdtor)
            attrs |= BlkAttr.STRUCTFINAL | BlkAttr.FINALIZE;
    }
    return attrs;
}

/**
Shrink the "allocated" length of an array to be the exact size of the array.

It doesn't matter what the current allocated length of the array is, the
user is telling the runtime that he knows what he is doing.

Params:
    ti = `TypeInfo` of array type
    arr = array to shrink. Its `.length` is element length, not byte length, despite `void` type
*/
extern(C) void _d_arrayshrinkfit(const TypeInfo ti, void[] arr) nothrow
{
    debug(PRINTF) printf("_d_arrayshrinkfit, elemsize = %zd, arr.ptr = %p arr.length = %zd\n", ti.next.tsize, arr.ptr, arr.length);
    auto tinext = unqualify(ti.next);
    auto size = tinext.tsize;                  // array element size
    auto reqsize = arr.length * size;
    auto isshared = typeid(ti) is typeid(TypeInfo_Shared);

    auto curArr = gc_getArrayUsed(arr.ptr, isshared);
    if (curArr.ptr is null)
        // not a valid GC pointer
        return;

    // align the array.
    auto offset = arr.ptr - curArr.ptr;
    auto cursize = curArr.length - offset;
    if (cursize <= reqsize)
        // invalid situation, or no change.
        return;

    // if the type has a destructor, destroy elements we are about to remove.
    if (typeid(tinext) is typeid(TypeInfo_Struct)) // avoid a complete dynamic type cast
    {
        auto sti = cast(TypeInfo_Struct)cast(void*)tinext;
        if (sti.xdtor)
        {
            try
            {
                finalize_array(arr.ptr + reqsize, cursize - reqsize, sti);
            }
            catch (Exception e)
            {
                import core.exception : onFinalizeError;
                onFinalizeError(sti, e);
            }
        }
    }

    gc_shrinkArrayUsed(arr.ptr[0 .. reqsize], cursize, isshared);
}

package bool hasPostblit(in TypeInfo ti) nothrow pure
{
    return (&ti.postblit).funcptr !is &TypeInfo.postblit;
}

void __doPostblit(void *ptr, size_t len, const TypeInfo ti)
{
    if (!hasPostblit(ti))
        return;

    if (auto tis = cast(TypeInfo_Struct)ti)
    {
        // this is a struct, check the xpostblit member
        auto pblit = tis.xpostblit;
        if (!pblit)
            // postblit not specified, no point in looping.
            return;

        // optimized for struct, call xpostblit directly for each element
        immutable size = ti.tsize;
        const eptr = ptr + len;
        for (;ptr < eptr;ptr += size)
            pblit(ptr);
    }
    else
    {
        // generic case, call the typeinfo's postblit function
        immutable size = ti.tsize;
        const eptr = ptr + len;
        for (;ptr < eptr;ptr += size)
            ti.postblit(ptr);
    }
}


/**
Set the array capacity.

If the array capacity isn't currently large enough
to hold the requested capacity (in number of elements), then the array is
resized/reallocated to the appropriate size.

Pass in a requested capacity of 0 to get the current capacity.

Params:
    ti = type info of element type
    newcapacity = requested new capacity
    p = pointer to array to set. Its `length` is left unchanged.

Returns: the number of elements that can actually be stored once the resizing is done
*/
extern(C) size_t _d_arraysetcapacity(const TypeInfo ti, size_t newcapacity, void[]* p) @weak
in
{
    assert(ti);
    assert(!(*p).length || (*p).ptr);
}
do
{
    import core.stdc.string;
    import core.exception : onOutOfMemoryError;

    auto isshared = typeid(ti) is typeid(TypeInfo_Shared);
    auto tinext = unqualify(ti.next);
    auto size = tinext.tsize;
    version (D_InlineAsm_X86)
    {
        size_t reqsize = void;

        asm
        {
            mov EAX, newcapacity;
            mul EAX, size;
            mov reqsize, EAX;
            jnc  Lcontinue;
        }
    }
    else version (D_InlineAsm_X86_64)
    {
        size_t reqsize = void;

        asm
        {
            mov RAX, newcapacity;
            mul RAX, size;
            mov reqsize, RAX;
            jnc  Lcontinue;
        }
    }
    else
    {
        import core.checkedint : mulu;

        bool overflow = false;
        size_t reqsize = mulu(size, newcapacity, overflow);
        if (!overflow)
            goto Lcontinue;
    }
Loverflow:
    onOutOfMemoryError();
    assert(0);
Lcontinue:

    // step 1, see if we can ensure the capacity is valid in-place
    auto datasize = (*p).length * size;
    auto curCapacity = gc_reserveArrayCapacity((*p).ptr[0 .. datasize], reqsize, isshared);
    if (curCapacity != 0)
        // in-place worked!
        return curCapacity / size;

    if (reqsize <= datasize)
        // requested size is less than array size, the current array satisfies
        // the request. But this is not an appendable GC array, so return 0.
        return 0;

    // step 2, if reserving in-place doesn't work, allocate a new array with at
    // least the requested allocated size.
    auto attrs = __typeAttrs(tinext, (*p).ptr) | BlkAttr.APPENDABLE;
    auto ptr = GC.malloc(reqsize, attrs, tinext);
    if (ptr is null)
        goto Loverflow;

    // copy the data over.
    // note that malloc will have initialized the data we did not request to 0.
    memcpy(ptr, (*p).ptr, datasize);

    // handle postblit
    __doPostblit(ptr, datasize, tinext);

    if (!(attrs & BlkAttr.NO_SCAN))
    {
        // need to memset the newly requested data, except for the data that
        // malloc returned that we didn't request.
        void *endptr = ptr + reqsize;
        void *begptr = ptr + datasize;

        // sanity check
        assert(endptr >= begptr);
        memset(begptr, 0, endptr - begptr);
    }

    *p = ptr[0 .. (*p).length];

    // set up the correct length. Note that we need to do this here, because
    // the GC malloc will automatically set the used size to what we requested.
    gc_shrinkArrayUsed(ptr[0 .. datasize], reqsize, isshared);

    curCapacity = gc_reserveArrayCapacity(ptr[0 .. datasize], 0, isshared);
    assert(curCapacity);
    return curCapacity / size;
}

/**
Allocate an array with the garbage collector.

Has three variants:
- `_d_newarrayU` leave elements uninitialized
- `_d_newarrayT` initializes to 0 (e.g `new int[]`)
- `_d_newarrayiT` initializes based on initializer retrieved from TypeInfo (e.g `new float[]`)

Params:
    ti = the type of the resulting array, (may also be the corresponding `array.ptr` type)
    length = `.length` of resulting array
Returns: newly allocated array
*/
extern (C) void[] _d_newarrayU(const scope TypeInfo ti, size_t length) pure nothrow @weak
{
    import core.exception : onOutOfMemoryError;

    auto tinext = unqualify(ti.next);
    auto size = tinext.tsize;

    debug(PRINTF) printf("_d_newarrayU(length = x%zx, size = %zd)\n", length, size);
    if (length == 0 || size == 0)
        return null;

    version (D_InlineAsm_X86)
    {
        asm pure nothrow @nogc
        {
            mov     EAX,size        ;
            mul     EAX,length      ;
            mov     size,EAX        ;
            jnc     Lcontinue       ;
        }
    }
    else version (D_InlineAsm_X86_64)
    {
        asm pure nothrow @nogc
        {
            mov     RAX,size        ;
            mul     RAX,length      ;
            mov     size,RAX        ;
            jnc     Lcontinue       ;
        }
    }
    else
    {
        import core.checkedint : mulu;

        bool overflow = false;
        size = mulu(size, length, overflow);
        if (!overflow)
            goto Lcontinue;
    }
Loverflow:
    onOutOfMemoryError();
    assert(0);
Lcontinue:

    auto ptr = GC.malloc(size, __typeAttrs(tinext) | BlkAttr.APPENDABLE, tinext);
    if (!ptr)
        goto Loverflow;
    debug(PRINTF) printf(" p = %p\n", ptr);
    return ptr[0 .. length];
}

/// ditto
extern (C) void[] _d_newarrayT(const TypeInfo ti, size_t length) pure nothrow @weak
{
    import core.stdc.string;

    void[] result = _d_newarrayU(ti, length);
    auto tinext = unqualify(ti.next);
    auto size = tinext.tsize;

    memset(result.ptr, 0, size * length);
    return result;
}

/// ditto
extern (C) void[] _d_newarrayiT(const TypeInfo ti, size_t length) pure nothrow @weak
{
    import core.internal.traits : AliasSeq;

    void[] result = _d_newarrayU(ti, length);
    auto tinext = unqualify(ti.next);
    auto size = tinext.tsize;

    auto init = tinext.initializer();

    switch (init.length)
    {
    foreach (T; AliasSeq!(ubyte, ushort, uint, ulong))
    {
    case T.sizeof:
        if (tinext.talign % T.alignof == 0)
        {
            (cast(T*)result.ptr)[0 .. size * length / T.sizeof] = *cast(T*)init.ptr;
            return result;
        }
        goto default;
    }

    default:
    {
        import core.stdc.string;
        immutable sz = init.length;
        for (size_t u = 0; u < size * length; u += sz)
            memcpy(result.ptr + u, init.ptr, sz);
        return result;
    }
    }
}

/**
Non-template version of $(REF _d_newitemT, core,lifetime) that does not perform
initialization. Needed for $(REF allocEntry, rt,aaA).

Params:
    _ti = `TypeInfo` of item to allocate
Returns:
    newly allocated item
*/
extern (C) void* _d_newitemU(scope const TypeInfo _ti) pure nothrow @weak
{
    auto ti = unqualify(_ti);
    auto flags = __typeAttrs(ti);

    return GC.malloc(ti.tsize, flags, ti);
}

/**
 *
 */
extern (C) void _d_delmemory(void* *p) @weak
{
    if (*p)
    {
        GC.free(*p);
        *p = null;
    }
}


/**
 *
 */
extern (C) void _d_callinterfacefinalizer(void *p) @weak
{
    if (p)
    {
        Interface *pi = **cast(Interface ***)p;
        Object o = cast(Object)(p - pi.offset);
        rt_finalize(cast(void*)o);
    }
}


/**
 *
 */
extern (C) void _d_callfinalizer(void* p) @weak
{
    rt_finalize( p );
}


/**
 *
 */
extern (C) void rt_setCollectHandler(CollectHandler h)
{
    collectHandler = h;
}


/**
 *
 */
extern (C) CollectHandler rt_getCollectHandler()
{
    return collectHandler;
}


/**
 *
 */
extern (C) int rt_hasFinalizerInSegment(void* p, size_t size, uint attr, scope const(void)[] segment) nothrow
{
    if (!p)
        return false;

    if (attr & BlkAttr.STRUCTFINAL)
    {
        import core.internal.gc.blockmeta;
        auto info = BlkInfo(
            base: p,
            size: size,
            attr: attr
        );

        auto ti = cast(TypeInfo_Struct)cast(void*)__getBlockFinalizerInfo(info);
        return cast(size_t)(cast(void*)ti.xdtor - segment.ptr) < segment.length;
    }

    // otherwise class
    auto ppv = cast(void**) p;
    if (!*ppv)
        return false;

    auto c = *cast(ClassInfo*)*ppv;
    do
    {
        auto pf = c.destructor;
        if (cast(size_t)(pf - segment.ptr) < segment.length) return true;
    }
    while ((c = c.base) !is null);

    return false;
}

debug (VALGRIND) import etc.valgrind.valgrind;

void finalize_array(void* p, size_t size, const TypeInfo_Struct si)
{
    // Due to the fact that the delete operator calls destructors
    // for arrays from the last element to the first, we maintain
    // compatibility here by doing the same.
    auto tsize = si.tsize;
    for (auto curP = p + size - tsize; curP >= p; curP -= tsize)
    {
        // call destructor
        si.destroy(curP);
    }
}

// called by the GC
void finalize_struct(void* p, TypeInfo_Struct ti) nothrow
{
    debug(PRINTF) printf("finalize_struct(p = %p)\n", p);

    try
    {
        ti.destroy(p); // call destructor
    }
    catch (Exception e)
    {
        import core.exception : onFinalizeError;
        onFinalizeError(ti, e);
    }
}

/**
 *
 */
extern (C) void rt_finalize2(void* p, bool det = true, bool resetMemory = true) nothrow
{
    debug(PRINTF) printf("rt_finalize2(p = %p)\n", p);

    auto ppv = cast(void**) p;
    if (!p || !*ppv)
        return;

    auto pc = cast(ClassInfo*) *ppv;
    try
    {
        if (det || collectHandler is null || collectHandler(cast(Object) p))
        {
            auto c = *pc;
            do
            {
                if (c.destructor)
                    (cast(fp_t) c.destructor)(cast(Object) p); // call destructor
            }
            while ((c = c.base) !is null);
        }

        if (ppv[1]) // if monitor is not null
            _d_monitordelete(cast(Object) p, det);

        if (resetMemory)
        {
            auto w = (*pc).initializer;
            p[0 .. w.length] = cast(void[]) w[];
        }
    }
    catch (Exception e)
    {
        import core.exception : onFinalizeError;
        onFinalizeError(*pc, e);
    }
    finally
    {
        *ppv = null; // zero vptr even if `resetMemory` is false
    }
}

/// Backwards compatibility
extern (C) void rt_finalize(void* p, bool det = true) nothrow
{
    rt_finalize2(p, det, true);
}

extern (C) void rt_finalizeFromGC(void* p, size_t size, uint attr) nothrow
{
    // to verify: reset memory necessary?
    if (!(attr & BlkAttr.STRUCTFINAL)) {
        rt_finalize2(p, false, false); // class
        return;
    }

    // get the struct typeinfo from the block, and the used size.
    import core.internal.gc.blockmeta;
    auto info = BlkInfo(
            base: p,
            size: size,
            attr: attr
    );

    auto si = cast(TypeInfo_Struct)cast(void*)__getBlockFinalizerInfo(info);

    if (attr & BlkAttr.APPENDABLE)
    {
        auto usedsize = __arrayAllocLength(info);
        auto arrptr = __arrayStart(info);
        try
        {
            finalize_array(arrptr, usedsize, si);
        }
        catch (Exception e)
        {
            import core.exception : onFinalizeError;
            onFinalizeError(si, e);
        }
    }
    else
        finalize_struct(p, si); // struct
}


/**
Resize a dynamic array by setting the `.length` property

Newly created elements are initialized to their default value.

Has two variants:
- `_d_arraysetlengthT` for arrays with elements that initialize to 0
- `_d_arraysetlengthiT` for non-zero initializers retrieved from `TypeInfo`

---
void main()
{
    int[] a = [1, 2];
    a.length = 3; // gets lowered to `_d_arraysetlengthT(typeid(int[]), 3, &a)`
}
---

Params:
    ti = `TypeInfo` of array
    newlength = new value for the array's `.length`
    p = pointer to array to update the `.length` of.
        While it's cast to `void[]`, its `.length` is still treated as element length.
Returns: `*p` after being updated
*/
extern (C) void[] _d_arraysetlengthT(const TypeInfo ti, size_t newlength, void[]* p) @weak
in
{
    assert(ti);
    assert(!(*p).length || (*p).ptr);
}
do
{
    import core.stdc.string;
    import core.exception : onOutOfMemoryError;

    debug(PRINTF)
    {
        //printf("_d_arraysetlengthT(p = %p, sizeelem = %d, newlength = %d)\n", p, sizeelem, newlength);
        if (p)
            printf("\tp.ptr = %p, p.length = %zd\n", (*p).ptr, (*p).length);
    }

    if (newlength <= (*p).length)
    {
        *p = (*p)[0 .. newlength];
        return *p;
    }
    auto tinext = unqualify(ti.next);
    size_t sizeelem = tinext.tsize;

    /* Calculate: newsize = newlength * sizeelem
     */
    bool overflow = false;
    version (D_InlineAsm_X86)
    {
        size_t newsize = void;

        asm pure nothrow @nogc
        {
            mov EAX, newlength;
            mul EAX, sizeelem;
            mov newsize, EAX;
            setc overflow;
        }
    }
    else version (D_InlineAsm_X86_64)
    {
        size_t newsize = void;

        asm pure nothrow @nogc
        {
            mov RAX, newlength;
            mul RAX, sizeelem;
            mov newsize, RAX;
            setc overflow;
        }
    }
    else
    {
        import core.checkedint : mulu;
        const size_t newsize = mulu(sizeelem, newlength, overflow);
    }
    if (overflow)
    {
        onOutOfMemoryError();
        assert(0);
    }

    debug(PRINTF) printf("newsize = %zx, newlength = %zx\n", newsize, newlength);

    if (!(*p).ptr)
    {
        assert((*p).length == 0);
        // pointer was null, need to allocate
        auto ptr = GC.malloc(newsize, __typeAttrs(tinext) | BlkAttr.APPENDABLE, tinext);
        if (ptr is null)
        {
            onOutOfMemoryError();
            assert(0);
        }
        memset(ptr, 0, newsize);
        *p = ptr[0 .. newlength];
        return *p;
    }

    const size_t size = (*p).length * sizeelem;
    const isshared = typeid(ti) is typeid(TypeInfo_Shared);

    /* Attempt to extend past the end of the existing array.
     * If not possible, allocate new space for entire array and copy.
     */
    void* newdata = (*p).ptr;
    if (!gc_expandArrayUsed(newdata[0 .. size], newsize, isshared))
    {
        newdata = GC.malloc(newsize, __typeAttrs(tinext, (*p).ptr) | BlkAttr.APPENDABLE, tinext);
        if (newdata is null)
        {
            onOutOfMemoryError();
            assert(0);
        }

        newdata[0 .. size] = (*p).ptr[0 .. size];

        // Do postblit processing, as we are making a copy.
        __doPostblit(newdata, size, tinext);
    }

    // Zero the unused portion of the newly allocated space
    memset(newdata + size, 0, newsize - size);

    *p = newdata[0 .. newlength];
    return *p;
}

/// ditto
extern (C) void[] _d_arraysetlengthiT(const TypeInfo ti, size_t newlength, void[]* p) @weak
in
{
    assert(!(*p).length || (*p).ptr);
}
do
{
    import core.stdc.string;
    import core.exception : onOutOfMemoryError;

    debug(PRINTF)
    {
        //printf("_d_arraysetlengthT(p = %p, sizeelem = %d, newlength = %d)\n", p, sizeelem, newlength);
        if (p)
            printf("\tp.ptr = %p, p.length = %zd\n", (*p).ptr, (*p).length);
    }

    if (newlength <= (*p).length)
    {
        *p = (*p)[0 .. newlength];
        return *p;
    }
    auto tinext = unqualify(ti.next);
    size_t sizeelem = tinext.tsize;

    /* Calculate: newsize = newlength * sizeelem
     */
    bool overflow = false;
    version (D_InlineAsm_X86)
    {
        size_t newsize = void;

        asm pure nothrow @nogc
        {
            mov EAX, newlength;
            mul EAX, sizeelem;
            mov newsize, EAX;
            setc overflow;
        }
    }
    else version (D_InlineAsm_X86_64)
    {
        size_t newsize = void;

        asm pure nothrow @nogc
        {
            mov RAX, newlength;
            mul RAX, sizeelem;
            mov newsize, RAX;
            setc overflow;
        }
    }
    else
    {
        import core.checkedint : mulu;
        const size_t newsize = mulu(sizeelem, newlength, overflow);
    }
    if (overflow)
    {
        onOutOfMemoryError();
        assert(0);
    }

    debug(PRINTF) printf("newsize = %zx, newlength = %zx\n", newsize, newlength);

    static void doInitialize(void *start, void *end, const void[] initializer)
    {
        if (initializer.length == 1)
        {
            memset(start, *(cast(ubyte*)initializer.ptr), end - start);
        }
        else
        {
            auto q = initializer.ptr;
            immutable initsize = initializer.length;
            for (; start < end; start += initsize)
            {
                memcpy(start, q, initsize);
            }
        }
    }

    if (!(*p).ptr)
    {
        assert((*p).length == 0);
        // pointer was null, need to allocate
        auto ptr = GC.malloc(newsize, __typeAttrs(tinext) | BlkAttr.APPENDABLE, tinext);
        if (ptr is null)
        {
            onOutOfMemoryError();
            assert(0);
        }
        doInitialize(ptr, ptr + newsize, tinext.initializer);
        *p = ptr[0 .. newlength];
        return *p;
    }

    const size_t size = (*p).length * sizeelem;
    const isshared = typeid(ti) is typeid(TypeInfo_Shared);

    /* Attempt to extend past the end of the existing array.
     * If not possible, allocate new space for entire array and copy.
     */
    void* newdata = (*p).ptr;
    if (!gc_expandArrayUsed(newdata[0 .. size], newsize, isshared))
    {
        newdata = GC.malloc(newsize, __typeAttrs(tinext, (*p).ptr) | BlkAttr.APPENDABLE, tinext);
        if (newdata is null)
        {
            onOutOfMemoryError();
            assert(0);
        }

        newdata[0 .. size] = (*p).ptr[0 .. size];

        // Do postblit processing, as we are making a copy.
        __doPostblit(newdata, size, tinext);
    }

    // Initialize the unused portion of the newly allocated space
    doInitialize(newdata + size, newdata + newsize, tinext.initializer);
    *p = newdata[0 .. newlength];
    return *p;
}


/**
Given an array of length `size` that needs to be expanded to `newlength`,
compute a new capacity.

Better version by Dave Fladebo, enhanced by Steven Schveighoffer:
This uses an inverse logorithmic algorithm to pre-allocate a bit more
space for larger arrays.
- The maximum "extra" space is about 80% of the requested space. This is for
PAGE size and smaller.
- As the arrays grow, the relative pre-allocated space shrinks.
- Perhaps most importantly, overall memory usage and stress on the GC
is decreased significantly for demanding environments.
- The algorithm is tuned to avoid any division at runtime.

Params:
    newlength = new `.length`
    elemsize = size of the element in the new array
Returns: new capacity for array
*/
size_t newCapacity(size_t newlength, size_t elemsize)
{
    size_t newcap = newlength * elemsize;

    /*
     * Max growth factor numerator is 234, so allow for multiplying by 256.
     * But also, the resulting size cannot be more than 2x, so prevent
     * growing if 2x would fill up the address space (for 32-bit)
     */
    enum largestAllowed = (ulong.max >> 8) & (size_t.max >> 1);
    if (!newcap || (newcap & ~largestAllowed))
        return newcap;

    /*
     * The calculation for "extra" space depends on the requested capacity.
     * We use an inverse logarithm of the new capacity to add an extra 15%
     * to 83% capacity. Note that normally we humans think in terms of
     * percent, but using 128 instead of 100 for the denominator means we
     * can avoid all division by simply bit-shifthing. Since there are only
     * 64 bits in a long, the bsr of a size_t is going to be 0 - 63. Using
     * a lookup table allows us to precalculate the multiplier based on the
     * inverse logarithm. The formula rougly is:
     *
     * newcap = request * (1.0 + min(0.83, 10.0 / (log(request) + 1)))
     */
    import core.bitop;
    static immutable multTable = (){
        assert(__ctfe);
        ulong[size_t.sizeof * 8] result;
        foreach (i; 0 .. result.length)
        {
            auto factor = 128 + 1280 / (i + 1);
            result[i] = factor > 234 ? 234 : factor;
        }
        return result;
    }();

    auto mult = multTable[bsr(newcap)];

    // if this were per cent, then the code would look like:
    // ((newlength * mult + 99) / 100) * elemsize
    newcap = cast(size_t)((newlength * mult + 127) >> 7) * elemsize;
    debug(PRINTF) printf("mult: %2.2f, alloc: %2.2f\n",mult/128.0,newcap / cast(double)elemsize);
    debug(PRINTF) printf("newcap = %zd, newlength = %zd, elemsize = %zd\n", newcap, newlength, elemsize);
    return newcap;
}


/**
Extend an array by n elements.

Caller must initialize those elements.

Params:
    ti = type info of array type (not element type)
    px = array to append to, cast to `byte[]` while keeping the same `.length`. Will be updated.
    n = number of elements to append
Returns: `px` after being appended to
*/
extern (C)
byte[] _d_arrayappendcTX(const TypeInfo ti, return scope ref byte[] px, size_t n) @weak
{
    import core.stdc.string;
    import core.exception : onOutOfMemoryError;
    // This is a cut&paste job from _d_arrayappendT(). Should be refactored.

    // Short circuit if no data is being appended.
    if (n == 0)
        return px;


    // only optimize array append where ti is not a shared type
    auto tinext = unqualify(ti.next);
    auto sizeelem = tinext.tsize;              // array element size
    auto isshared = typeid(ti) is typeid(TypeInfo_Shared);
    auto length = px.length;
    auto newlength = length + n;
    auto newsize = newlength * sizeelem;
    auto size = length * sizeelem;

    if (!gc_expandArrayUsed(px.ptr[0 .. size], newsize, isshared))
    {
        // could not set the size, we must reallocate.
        auto newcap = newCapacity(newlength, sizeelem);
        auto attrs = __typeAttrs(tinext, px.ptr) | BlkAttr.APPENDABLE;
        auto ptr = cast(byte*) GC.malloc(newcap, attrs, tinext);
        if (ptr is null)
        {
            onOutOfMemoryError();
            assert(0);
        }

        if (newsize != newcap)
        {
            // For small blocks that are always fully scanned, if we allocated more
            // capacity than was requested, we are responsible for zeroing that
            // memory.
            // TODO: should let the GC figure this out, as this property may
            // not always hold.
            if (!(attrs & BlkAttr.NO_SCAN) && newcap < PAGESIZE)
                memset(ptr + newsize, 0, newcap - newsize);

            gc_shrinkArrayUsed(ptr[0 .. newsize], newcap, isshared);
        }

        memcpy(ptr, px.ptr, size);

        // do potsblit processing.
        __doPostblit(ptr, size, tinext);

        px = ptr[0 .. newlength];
        return px;
    }

    // we were able to expand in place, just update the length
    px = px.ptr[0 .. newlength];
    return px;
}


/**
Append `dchar` to `char[]`, converting UTF-32 to UTF-8

---
void main()
{
    char[] s;
    s ~= 'α';
}
---

Params:
    x = array to append to cast to `byte[]`. Will be modified.
    c = `dchar` to append
Returns: updated `x` cast to `void[]`
*/
extern (C) void[] _d_arrayappendcd(ref byte[] x, dchar c) @weak
{
    // c could encode into from 1 to 4 characters
    char[4] buf = void;
    char[] appendthis; // passed to appendT
    if (c <= 0x7F)
    {
        buf.ptr[0] = cast(char)c;
        appendthis = buf[0..1];
    }
    else if (c <= 0x7FF)
    {
        buf.ptr[0] = cast(char)(0xC0 | (c >> 6));
        buf.ptr[1] = cast(char)(0x80 | (c & 0x3F));
        appendthis = buf[0..2];
    }
    else if (c <= 0xFFFF)
    {
        buf.ptr[0] = cast(char)(0xE0 | (c >> 12));
        buf.ptr[1] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        buf.ptr[2] = cast(char)(0x80 | (c & 0x3F));
        appendthis = buf[0..3];
    }
    else if (c <= 0x10FFFF)
    {
        buf.ptr[0] = cast(char)(0xF0 | (c >> 18));
        buf.ptr[1] = cast(char)(0x80 | ((c >> 12) & 0x3F));
        buf.ptr[2] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        buf.ptr[3] = cast(char)(0x80 | (c & 0x3F));
        appendthis = buf[0..4];
    }
    else
    {
        import core.exception : onUnicodeError;
        onUnicodeError("Invalid UTF-8 sequence", 0);      // invalid utf character
    }

    //
    // TODO: This always assumes the array type is shared, because we do not
    // get a typeinfo from the compiler.  Assuming shared is the safest option.
    // Once the compiler is fixed, the proper typeinfo should be forwarded.
    //

    // Hack because _d_arrayappendT takes `x` as a reference
    auto xx = cast(shared(char)[])x;
    object._d_arrayappendT(xx, cast(shared(char)[])appendthis);
    x = cast(byte[])xx;
    return x;
}

unittest
{
    import core.exception : UnicodeException;

    /* Using inline try {} catch {} blocks fails to catch the UnicodeException
     * thrown.
     * https://issues.dlang.org/show_bug.cgi?id=16799
     */
    static void assertThrown(T : Throwable = Exception, E)(lazy E expr, string msg)
    {
        try
            expr;
        catch (T e) {
            assert(e.msg == msg);
            return;
        }
    }

    static void f()
    {
        string ret;
        int i = -1;
        ret ~= i;
    }

    assertThrown!UnicodeException(f(), "Invalid UTF-8 sequence");
}


/**
Append `dchar` to `wchar[]`, converting UTF-32 to UTF-16

---
void main()
{
    dchar x;
    wchar[] s;
    s ~= 'α';
}
---

Params:
    x = array to append to cast to `byte[]`. Will be modified.
    c = `dchar` to append

Returns: updated `x` cast to `void[]`
*/
extern (C) void[] _d_arrayappendwd(ref byte[] x, dchar c) @weak
{
    // c could encode into from 1 to 2 w characters
    wchar[2] buf = void;
    wchar[] appendthis; // passed to appendT
    if (c <= 0xFFFF)
    {
        buf.ptr[0] = cast(wchar) c;
        appendthis = buf[0..1];
    }
    else
    {
        buf.ptr[0] = cast(wchar) ((((c - 0x10000) >> 10) & 0x3FF) + 0xD800);
        buf.ptr[1] = cast(wchar) (((c - 0x10000) & 0x3FF) + 0xDC00);
        appendthis = buf[0..2];
    }

    //
    // TODO: This always assumes the array type is shared, because we do not
    // get a typeinfo from the compiler.  Assuming shared is the safest option.
    // Once the compiler is fixed, the proper typeinfo should be forwarded.
    //

    auto xx = (cast(shared(wchar)*)x.ptr)[0 .. x.length];
    object._d_arrayappendT(xx, cast(shared(wchar)[])appendthis);
    x = (cast(byte*)xx.ptr)[0 .. xx.length];
    return x;
}

/**
Allocate an array literal

Rely on the caller to do the initialization of the array.

---
int[] getArr()
{
    return [10, 20];
    // auto res = cast(int*) _d_arrayliteralTX(typeid(int[]), 2);
    // res[0] = 10;
    // res[1] = 20;
    // return res[0..2];
}
---

Params:
    ti = `TypeInfo` of resulting array type
    length = `.length` of array literal

Returns: pointer to allocated array
*/
extern (C)
void* _d_arrayliteralTX(const TypeInfo ti, size_t length) @weak
{
    auto tinext = unqualify(ti.next);
    auto sizeelem = tinext.tsize;              // array element size
    void* result;

    debug(PRINTF) printf("_d_arrayliteralTX(sizeelem = %zd, length = %zd)\n", sizeelem, length);
    if (length == 0 || sizeelem == 0)
        return null;
    else
    {
        auto allocsize = length * sizeelem;
        return GC.malloc(allocsize, __typeAttrs(tinext) | BlkAttr.APPENDABLE, tinext);
    }
}


unittest
{
    int[] a;
    int[] b;
    int i;

    a = new int[3];
    a[0] = 1; a[1] = 2; a[2] = 3;
    b = a.dup;
    assert(b.length == 3);
    for (i = 0; i < 3; i++)
        assert(b[i] == i + 1);

    // test slice appending
    b = a[0..1];
    b ~= 4;
    for (i = 0; i < 3; i++)
        assert(a[i] == i + 1);

    // test reserving
    char[] arr = new char[4093];
    for (i = 0; i < arr.length; i++)
        arr[i] = cast(char)(i % 256);

    // note that these two commands used to cause corruption, which may not be
    // detected.
    arr.reserve(4094);
    auto arr2 = arr ~ "123";
    assert(arr2[0..arr.length] == arr);
    assert(arr2[arr.length..$] == "123");

    // test postblit on array concat, append, length, etc.
    static struct S
    {
        int x;
        int pad;
        this(this)
        {
            ++x;
        }
    }
    void testPostBlit(T)()
    {
        auto sarr = new T[1];
        debug(SENTINEL) {} else
            assert(sarr.capacity == 1);

        // length extend
        auto sarr2 = sarr;
        assert(sarr[0].x == 0);
        sarr2.length += 1;
        assert(sarr2[0].x == 1);
        assert(sarr[0].x == 0);

        // append
        T s;
        sarr2 = sarr;
        sarr2 ~= s;
        assert(sarr2[0].x == 1);
        assert(sarr2[1].x == 1);
        assert(sarr[0].x == 0);
        assert(s.x == 0);

        // concat
        sarr2 = sarr ~ sarr;
        assert(sarr2[0].x == 1);
        assert(sarr2[1].x == 1);
        assert(sarr[0].x == 0);

        // concat multiple (calls different method)
        sarr2 = sarr ~ sarr ~ sarr;
        assert(sarr2[0].x == 1);
        assert(sarr2[1].x == 1);
        assert(sarr2[2].x == 1);
        assert(sarr[0].x == 0);

        // reserve capacity
        sarr2 = sarr;
        sarr2.reserve(2);
        assert(sarr2[0].x == 1);
        assert(sarr[0].x == 0);
    }
    testPostBlit!(S)();
    testPostBlit!(const(S))();
}

unittest
{
    // Bugzilla 3454 - Inconsistent flag setting in GC.realloc()
    static void test(size_t multiplier)
    {
        auto p = GC.malloc(8 * multiplier, 0);
        assert(GC.getAttr(p) == 0);

        // no move, set attr
        p = GC.realloc(p, 8 * multiplier + 5, BlkAttr.NO_SCAN);
        assert(GC.getAttr(p) == BlkAttr.NO_SCAN);

        // shrink, copy attr
        p = GC.realloc(p, 2 * multiplier, 0);
        assert(GC.getAttr(p) == BlkAttr.NO_SCAN);

        // extend, copy attr
        p = GC.realloc(p, 8 * multiplier, 0);
        assert(GC.getAttr(p) == BlkAttr.NO_SCAN);
    }
    test(16);
    version (OnlyLowMemUnittests) {} else
    test(1024 * 1024);
}

unittest
{
    import core.exception;
    try
    {
        size_t x = size_t.max;
        byte[] big_buf = new byte[x];
    }
    catch (OutOfMemoryError)
    {
    }
}

unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=13854
    auto arr = new ubyte[PAGESIZE]; // ensure page size
    auto info1 = GC.query(arr.ptr);
    assert(info1.base !is arr.ptr); // offset is required for page size or larger

    auto arr2 = arr[0..1];
    assert(arr2.capacity == 0); // cannot append
    arr2 ~= 0; // add a byte
    assert(arr2.ptr !is arr.ptr); // reallocated
    auto info2 = GC.query(arr2.ptr);
    assert(info2.base is arr2.ptr); // no offset, the capacity is small.

    // do the same via setting length
    arr2 = arr[0..1];
    assert(arr2.capacity == 0);
    arr2.length += 1;
    assert(arr2.ptr !is arr.ptr); // reallocated
    info2 = GC.query(arr2.ptr);
    assert(info2.base is arr2.ptr); // no offset, the capacity is small.

    // do the same for char[] since we need a type with an initializer to test certain runtime functions
    auto carr = new char[PAGESIZE];
    info1 = GC.query(carr.ptr);
    assert(info1.base !is carr.ptr); // offset is required for page size or larger

    auto carr2 = carr[0..1];
    assert(carr2.capacity == 0); // cannot append
    carr2 ~= 0; // add a byte
    assert(carr2.ptr !is carr.ptr); // reallocated
    info2 = GC.query(carr2.ptr);
    assert(info2.base is carr2.ptr); // no offset, the capacity is small.

    // do the same via setting length
    carr2 = carr[0..1];
    assert(carr2.capacity == 0);
    carr2.length += 1;
    assert(carr2.ptr !is carr.ptr); // reallocated
    info2 = GC.query(carr2.ptr);
    assert(info2.base is carr2.ptr); // no offset, the capacity is small.
}

unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=13878
    auto arr = new ubyte[1];
    auto info = GC.query(arr.ptr);
    assert(info.attr & BlkAttr.NO_SCAN); // should be NO_SCAN
    arr ~= 0; // ensure array is inserted into cache
    debug(SENTINEL) {} else
        assert(arr.ptr is info.base);
    GC.clrAttr(arr.ptr, BlkAttr.NO_SCAN); // remove the attribute
    auto arr2 = arr[0..1];
    assert(arr2.capacity == 0); // cannot append
    arr2 ~= 0;
    assert(arr2.ptr !is arr.ptr);
    info = GC.query(arr2.ptr);
    assert(!(info.attr & BlkAttr.NO_SCAN)); // ensure attribute sticks

    // do the same via setting length
    arr = new ubyte[1];
    arr ~= 0; // ensure array is inserted into cache
    GC.clrAttr(arr.ptr, BlkAttr.NO_SCAN); // remove the attribute
    arr2 = arr[0..1];
    assert(arr2.capacity == 0);
    arr2.length += 1;
    assert(arr2.ptr !is arr.ptr); // reallocated
    info = GC.query(arr2.ptr);
    assert(!(info.attr & BlkAttr.NO_SCAN)); // ensure attribute sticks

    // do the same for char[] since we need a type with an initializer to test certain runtime functions
    auto carr = new char[1];
    info = GC.query(carr.ptr);
    assert(info.attr & BlkAttr.NO_SCAN); // should be NO_SCAN
    carr ~= 0; // ensure array is inserted into cache
    debug(SENTINEL) {} else
        assert(carr.ptr is info.base);
    GC.clrAttr(carr.ptr, BlkAttr.NO_SCAN); // remove the attribute
    auto carr2 = carr[0..1];
    assert(carr2.capacity == 0); // cannot append
    carr2 ~= 0;
    assert(carr2.ptr !is carr.ptr);
    info = GC.query(carr2.ptr);
    assert(!(info.attr & BlkAttr.NO_SCAN)); // ensure attribute sticks

    // do the same via setting length
    carr = new char[1];
    carr ~= 0; // ensure array is inserted into cache
    GC.clrAttr(carr.ptr, BlkAttr.NO_SCAN); // remove the attribute
    carr2 = carr[0..1];
    assert(carr2.capacity == 0);
    carr2.length += 1;
    assert(carr2.ptr !is carr.ptr); // reallocated
    info = GC.query(carr2.ptr);
    assert(!(info.attr & BlkAttr.NO_SCAN)); // ensure attribute sticks
}

// test struct finalizers
debug(SENTINEL) {} else
deprecated unittest
{
    __gshared int dtorCount;
    static struct S1
    {
        int x;

        ~this()
        {
            dtorCount++;
        }
    }

    dtorCount = 0;
    S1* s2 = new S1;
    GC.runFinalizers((cast(char*)(typeid(S1).xdtor))[0..1]);
    assert(dtorCount == 1);
    GC.free(s2);

    dtorCount = 0;
    const(S1)* s3 = new const(S1);
    GC.runFinalizers((cast(char*)(typeid(S1).xdtor))[0..1]);
    assert(dtorCount == 1);
    GC.free(cast(void*)s3);

    dtorCount = 0;
    shared(S1)* s4 = new shared(S1);
    GC.runFinalizers((cast(char*)(typeid(S1).xdtor))[0..1]);
    assert(dtorCount == 1);
    GC.free(cast(void*)s4);

    dtorCount = 0;
    const(S1)[] carr1 = new const(S1)[5];
    BlkInfo blkinf1 = GC.query(carr1.ptr);
    GC.runFinalizers((cast(char*)(typeid(S1).xdtor))[0..1]);
    assert(dtorCount == 5);
    GC.free(blkinf1.base);

    dtorCount = 0;
    S1[] arr2 = new S1[10];
    arr2.length = 6;
    arr2.assumeSafeAppend;
    assert(dtorCount == 4); // destructors run explicitely?

    dtorCount = 0;
    BlkInfo blkinf = GC.query(arr2.ptr);
    GC.runFinalizers((cast(char*)(typeid(S1).xdtor))[0..1]);
    assert(dtorCount == 6);
    GC.free(blkinf.base);

    // associative arrays
    import rt.aaA : entryDtor;
    // throw away all existing AA entries with dtor
    GC.runFinalizers((cast(char*)&entryDtor)[0..1]);

    S1[int] aa1;
    aa1[0] = S1(0);
    aa1[1] = S1(1);
    dtorCount = 0;
    aa1 = null;
    GC.runFinalizers((cast(char*)&entryDtor)[0..1]);
    assert(dtorCount == 2);

    int[S1] aa2;
    aa2[S1(0)] = 0;
    aa2[S1(1)] = 1;
    aa2[S1(2)] = 2;
    dtorCount = 0;
    aa2 = null;
    GC.runFinalizers((cast(char*)&entryDtor)[0..1]);
    assert(dtorCount == 3);

    S1[2][int] aa3;
    aa3[0] = [S1(0),S1(2)];
    aa3[1] = [S1(1),S1(3)];
    dtorCount = 0;
    aa3 = null;
    GC.runFinalizers((cast(char*)&entryDtor)[0..1]);
    assert(dtorCount == 4);
}

// test struct dtor handling not causing false pointers
unittest
{
    // for 64-bit, allocate a struct of size 40
    static struct S
    {
        size_t[4] data;
        S* ptr4;
    }
    auto p1 = new S;
    auto p2 = new S;
    p2.ptr4 = p1;

    // a struct with a dtor with size 32, but the dtor will cause
    //  allocation to be larger by a pointer
    static struct A
    {
        size_t[3] data;
        S* ptr3;

        ~this() {}
    }

    GC.free(p2);
    auto a = new A; // reuse same memory
    if (cast(void*)a is cast(void*)p2) // reusage not guaranteed
    {
        auto ptr = cast(S**)(a + 1);
        assert(*ptr != p1); // still same data as p2.ptr4?
    }

    // small array
    static struct SArr
    {
        void*[10] data;
    }
    auto arr1 = new SArr;
    arr1.data[] = p1;
    GC.free(arr1);

    // allocates 2*A.sizeof + (void*).sizeof (TypeInfo) + 1 (array length)
    auto arr2 = new A[2];
    if (cast(void*)arr1 is cast(void*)arr2.ptr) // reusage not guaranteed
    {
        auto ptr = cast(S**)(arr2.ptr + 2);
        assert(*ptr != p1); // still same data as p2.ptr4?
    }

    // large array
    static struct LArr
    {
        void*[1023] data;
    }
    auto larr1 = new LArr;
    larr1.data[] = p1;
    GC.free(larr1);

    auto larr2 = new S[255];
    import core.internal.gc.blockmeta : LARGEPREFIX;
    if (cast(void*)larr1 is cast(void*)larr2.ptr - LARGEPREFIX) // reusage not guaranteed
    {
        auto ptr = cast(S**)larr1;
        assert(ptr[0] != p1); // 16 bytes array header
        assert(ptr[1] != p1);
        version (D_LP64) {} else
        {
            assert(ptr[2] != p1);
            assert(ptr[3] != p1);
        }
    }
}

// test class finalizers exception handling
unittest
{
    bool test(E)()
    {
        import core.exception;
        static class C1
        {
            E exc;
            this(E exc) { this.exc = exc; }
            ~this() { throw exc; }
        }

        bool caught = false;
        C1 c = new C1(new E("test onFinalizeError"));
        try
        {
            GC.runFinalizers((cast(uint*)&C1.__dtor)[0..1]);
        }
        catch (FinalizeError err)
        {
            caught = true;
        }
        catch (E)
        {
        }
        GC.free(cast(void*)c);
        return caught;
    }

    assert( test!Exception);
    import core.exception : InvalidMemoryOperationError;
    assert(!test!InvalidMemoryOperationError);
}

// test bug 14126
unittest
{
    static struct S
    {
        S* thisptr;
        ~this() { assert(&this == thisptr); thisptr = null;}
    }

    S[] test14126 = new S[2048]; // make sure we allocate at least a PAGE
    foreach (ref s; test14126)
    {
        s.thisptr = &s;
    }
}
