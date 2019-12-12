/**
 * This module contains all functions related to an object's lifetime:
 * allocation, resizing, deallocation, and finalization.
 *
 * Copyright: Copyright Digital Mars 2000 - 2012.
 * License: Distributed under the
 *      $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
 *    (See accompanying file LICENSE)
 * Authors:   Walter Bright, Sean Kelly, Steven Schveighoffer
 * Source: $(DRUNTIMESRC src/rt/_lifetime.d)
 */

module rt.lifetime;

import core.stdc.stdlib;
import core.stdc.string;
import core.stdc.stdarg;
import core.bitop;
import core.memory;
debug(PRINTF) import core.stdc.stdio;
static import rt.tlsgc;

alias BlkInfo = GC.BlkInfo;
alias BlkAttr = GC.BlkAttr;
import core.exception : onOutOfMemoryError, onFinalizeError, onInvalidMemoryOperationError;

private
{
    alias bool function(Object) CollectHandler;
    __gshared CollectHandler collectHandler = null;

    extern (C) void _d_monitordelete(Object h, bool det);

    enum : size_t
    {
        PAGESIZE = 4096,
        BIGLENGTHMASK = ~(PAGESIZE - 1),
        SMALLPAD = 1,
        MEDPAD = ushort.sizeof,
        LARGEPREFIX = 16, // 16 bytes padding at the front of the array
        LARGEPAD = LARGEPREFIX + 1,
        MAXSMALLSIZE = 256-SMALLPAD,
        MAXMEDSIZE = (PAGESIZE / 2) - MEDPAD
    }
}

private immutable bool callStructDtorsDuringGC;

extern (C) void lifetime_init()
{
    // this is run before static ctors, so it is safe to modify immutables
    import rt.config;
    string s = rt_configOption("callStructDtorsDuringGC");
    if (s != null)
        cast() callStructDtorsDuringGC = s[0] == '1' || s[0] == 'y' || s[0] == 'Y';
    else
        cast() callStructDtorsDuringGC = true;
}

/**
 *
 */
extern (C) void* _d_allocmemory(size_t sz)
{
    return GC.malloc(sz);
}

/**
 *
 */
extern (C) Object _d_newclass(const ClassInfo ci)
{
    void* p;

    debug(PRINTF) printf("_d_newclass(ci = %p, %s)\n", ci, cast(char *)ci.name);
    if (ci.m_flags & TypeInfo_Class.ClassFlags.isCOMclass)
    {   /* COM objects are not garbage collected, they are reference counted
         * using AddRef() and Release().  They get free'd by C's free()
         * function called by Release() when Release()'s reference count goes
         * to zero.
     */
        p = malloc(ci.initializer.length);
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
        p = GC.malloc(ci.initializer.length, attr, ci);
        debug(PRINTF) printf(" p = %p\n", p);
    }

    debug(PRINTF)
    {
        printf("p = %p\n", p);
        printf("ci = %p, ci.init.ptr = %p, len = %llu\n", ci, ci.initializer.ptr, cast(ulong)ci.initializer.length);
        printf("vptr = %p\n", *cast(void**) ci.initializer);
        printf("vtbl[0] = %p\n", (*cast(void***) ci.initializer)[0]);
        printf("vtbl[1] = %p\n", (*cast(void***) ci.initializer)[1]);
        printf("init[0] = %x\n", (cast(uint*) ci.initializer)[0]);
        printf("init[1] = %x\n", (cast(uint*) ci.initializer)[1]);
        printf("init[2] = %x\n", (cast(uint*) ci.initializer)[2]);
        printf("init[3] = %x\n", (cast(uint*) ci.initializer)[3]);
        printf("init[4] = %x\n", (cast(uint*) ci.initializer)[4]);
    }

    // initialize it
    p[0 .. ci.initializer.length] = ci.initializer[];

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
extern (C) void _d_delclass(Object* p)
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

/**
 * This is called for a delete statement where the value
 * being deleted is a pointer to a struct with a destructor
 * but doesn't have an overloaded delete operator.
 */
extern (C) void _d_delstruct(void** p, TypeInfo_Struct inf)
{
    if (*p)
    {
        debug(PRINTF) printf("_d_delstruct(%p, %p)\n", *p, cast(void*)inf);

        inf.destroy(*p);
        GC.free(*p);
        *p = null;
    }
}

// strip const/immutable/shared/inout from type info
inout(TypeInfo) unqualify(inout(TypeInfo) cti) pure nothrow @nogc
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

// size used to store the TypeInfo at the end of an allocation for structs that have a destructor
size_t structTypeInfoSize(const TypeInfo ti) pure nothrow @nogc
{
    if (!callStructDtorsDuringGC)
        return 0;

    if (ti && typeid(ti) is typeid(TypeInfo_Struct)) // avoid a complete dynamic type cast
    {
        auto sti = cast(TypeInfo_Struct)cast(void*)ti;
        if (sti.xdtor)
            return size_t.sizeof;
    }
    return 0;
}

/** dummy class used to lock for shared array appending */
private class ArrayAllocLengthLock
{}


/**
  Set the allocated length of the array block.  This is called
  any time an array is appended to or its length is set.

  The allocated block looks like this for blocks < PAGESIZE:

  |elem0|elem1|elem2|...|elemN-1|emptyspace|N*elemsize|


  The size of the allocated length at the end depends on the block size:

  a block of 16 to 256 bytes has an 8-bit length.

  a block with 512 to pagesize/2 bytes has a 16-bit length.

  For blocks >= pagesize, the length is a size_t and is at the beginning of the
  block.  The reason we have to do this is because the block can extend into
  more pages, so we cannot trust the block length if it sits at the end of the
  block, because it might have just been extended.  If we can prove in the
  future that the block is unshared, we may be able to change this, but I'm not
  sure it's important.

  In order to do put the length at the front, we have to provide 16 bytes
  buffer space in case the block has to be aligned properly.  In x86, certain
  SSE instructions will only work if the data is 16-byte aligned.  In addition,
  we need the sentinel byte to prevent accidental pointers to the next block.
  Because of the extra overhead, we only do this for page size and above, where
  the overhead is minimal compared to the block size.

  So for those blocks, it looks like:

  |N*elemsize|padding|elem0|elem1|...|elemN-1|emptyspace|sentinelbyte|

  where elem0 starts 16 bytes after the first byte.
  */
bool __setArrayAllocLength(ref BlkInfo info, size_t newlength, bool isshared, const TypeInfo tinext, size_t oldlength = ~0) pure nothrow
{
    import core.atomic;

    size_t typeInfoSize = structTypeInfoSize(tinext);

    if (info.size <= 256)
    {
        import core.checkedint;

        bool overflow;
        auto newlength_padded = addu(newlength,
                                     addu(SMALLPAD, typeInfoSize, overflow),
                                     overflow);

        if (newlength_padded > info.size || overflow)
            // new size does not fit inside block
            return false;

        auto length = cast(ubyte *)(info.base + info.size - typeInfoSize - SMALLPAD);
        if (oldlength != ~0)
        {
            if (isshared)
            {
                return cas(cast(shared)length, cast(ubyte)oldlength, cast(ubyte)newlength);
            }
            else
            {
                if (*length == cast(ubyte)oldlength)
                    *length = cast(ubyte)newlength;
                else
                    return false;
            }
        }
        else
        {
            // setting the initial length, no cas needed
            *length = cast(ubyte)newlength;
        }
        if (typeInfoSize)
        {
            auto typeInfo = cast(TypeInfo*)(info.base + info.size - size_t.sizeof);
            *typeInfo = cast() tinext;
        }
    }
    else if (info.size < PAGESIZE)
    {
        if (newlength + MEDPAD + typeInfoSize > info.size)
            // new size does not fit inside block
            return false;
        auto length = cast(ushort *)(info.base + info.size - typeInfoSize - MEDPAD);
        if (oldlength != ~0)
        {
            if (isshared)
            {
                return cas(cast(shared)length, cast(ushort)oldlength, cast(ushort)newlength);
            }
            else
            {
                if (*length == oldlength)
                    *length = cast(ushort)newlength;
                else
                    return false;
            }
        }
        else
        {
            // setting the initial length, no cas needed
            *length = cast(ushort)newlength;
        }
        if (typeInfoSize)
        {
            auto typeInfo = cast(TypeInfo*)(info.base + info.size - size_t.sizeof);
            *typeInfo = cast() tinext;
        }
    }
    else
    {
        if (newlength + LARGEPAD > info.size)
            // new size does not fit inside block
            return false;
        auto length = cast(size_t *)(info.base);
        if (oldlength != ~0)
        {
            if (isshared)
            {
                return cas(cast(shared)length, cast(size_t)oldlength, cast(size_t)newlength);
            }
            else
            {
                if (*length == oldlength)
                    *length = newlength;
                else
                    return false;
            }
        }
        else
        {
            // setting the initial length, no cas needed
            *length = newlength;
        }
        if (typeInfoSize)
        {
            auto typeInfo = cast(TypeInfo*)(info.base + size_t.sizeof);
            *typeInfo = cast()tinext;
        }
    }
    return true; // resize succeeded
}

/**
  get the allocation size of the array for the given block (without padding or type info)
  */
size_t __arrayAllocLength(ref BlkInfo info, const TypeInfo tinext) pure nothrow
{
    if (info.size <= 256)
        return *cast(ubyte *)(info.base + info.size - structTypeInfoSize(tinext) - SMALLPAD);

    if (info.size < PAGESIZE)
        return *cast(ushort *)(info.base + info.size - structTypeInfoSize(tinext) - MEDPAD);

    return *cast(size_t *)(info.base);
}

/**
  get the start of the array for the given block
  */
void *__arrayStart(BlkInfo info) nothrow pure
{
    return info.base + ((info.size & BIGLENGTHMASK) ? LARGEPREFIX : 0);
}

/**
  get the padding required to allocate size bytes.  Note that the padding is
  NOT included in the passed in size.  Therefore, do NOT call this function
  with the size of an allocated block.
  */
size_t __arrayPad(size_t size, const TypeInfo tinext) nothrow pure @trusted
{
    return size > MAXMEDSIZE ? LARGEPAD : ((size > MAXSMALLSIZE ? MEDPAD : SMALLPAD) + structTypeInfoSize(tinext));
}

/**
  allocate an array memory block by applying the proper padding and
  assigning block attributes if not inherited from the existing block
  */
BlkInfo __arrayAlloc(size_t arrsize, const TypeInfo ti, const TypeInfo tinext) nothrow pure
{
    import core.checkedint;

    size_t typeInfoSize = structTypeInfoSize(tinext);
    size_t padsize = arrsize > MAXMEDSIZE ? LARGEPAD : ((arrsize > MAXSMALLSIZE ? MEDPAD : SMALLPAD) + typeInfoSize);

    bool overflow;
    auto padded_size = addu(arrsize, padsize, overflow);

    if (overflow)
        return BlkInfo();

    uint attr = (!(tinext.flags & 1) ? BlkAttr.NO_SCAN : 0) | BlkAttr.APPENDABLE;
    if (typeInfoSize)
        attr |= BlkAttr.STRUCTFINAL | BlkAttr.FINALIZE;
    return GC.qalloc(padded_size, attr, ti);
}

BlkInfo __arrayAlloc(size_t arrsize, ref BlkInfo info, const TypeInfo ti, const TypeInfo tinext)
{
    import core.checkedint;

    if (!info.base)
        return __arrayAlloc(arrsize, ti, tinext);

    bool overflow;
    auto padded_size = addu(arrsize, __arrayPad(arrsize, tinext), overflow);
    if (overflow)
    {
        return BlkInfo();
    }

    return GC.qalloc(padded_size, info.attr, ti);
}

/**
  cache for the lookup of the block info
  */
enum N_CACHE_BLOCKS=8;

// note this is TLS, so no need to sync.
BlkInfo *__blkcache_storage;

static if (N_CACHE_BLOCKS==1)
{
    version=single_cache;
}
else
{
    //version=simple_cache; // uncomment to test simple cache strategy
    //version=random_cache; // uncomment to test random cache strategy

    // ensure N_CACHE_BLOCKS is power of 2.
    static assert(!((N_CACHE_BLOCKS - 1) & N_CACHE_BLOCKS));

    version (random_cache)
    {
        int __nextRndNum = 0;
    }
    int __nextBlkIdx;
}

@property BlkInfo *__blkcache() nothrow
{
    if (!__blkcache_storage)
    {
        // allocate the block cache for the first time
        immutable size = BlkInfo.sizeof * N_CACHE_BLOCKS;
        __blkcache_storage = cast(BlkInfo *)malloc(size);
        memset(__blkcache_storage, 0, size);
    }
    return __blkcache_storage;
}

// called when thread is exiting.
static ~this()
{
    // free the blkcache
    if (__blkcache_storage)
    {
        free(__blkcache_storage);
        __blkcache_storage = null;
    }
}


// we expect this to be called with the lock in place
void processGCMarks(BlkInfo* cache, scope rt.tlsgc.IsMarkedDg isMarked) nothrow
{
    // called after the mark routine to eliminate block cache data when it
    // might be ready to sweep

    debug(PRINTF) printf("processing GC Marks, %x\n", cache);
    if (cache)
    {
        debug(PRINTF) foreach (i; 0 .. N_CACHE_BLOCKS)
        {
            printf("cache entry %d has base ptr %x\tsize %d\tflags %x\n", i, cache[i].base, cache[i].size, cache[i].attr);
        }
        auto cache_end = cache + N_CACHE_BLOCKS;
        for (;cache < cache_end; ++cache)
        {
            if (cache.base != null && !isMarked(cache.base))
            {
                debug(PRINTF) printf("clearing cache entry at %x\n", cache.base);
                cache.base = null; // clear that data.
            }
        }
    }
}

unittest
{
    // Bugzilla 10701 - segfault in GC
    ubyte[] result; result.length = 4096;
    GC.free(result.ptr);
    GC.collect();
}

/**
  Get the cached block info of an interior pointer.  Returns null if the
  interior pointer's block is not cached.

  NOTE: The base ptr in this struct can be cleared asynchronously by the GC,
        so any use of the returned BlkInfo should copy it and then check the
        base ptr of the copy before actually using it.

  TODO: Change this function so the caller doesn't have to be aware of this
        issue.  Either return by value and expect the caller to always check
        the base ptr as an indication of whether the struct is valid, or set
        the BlkInfo as a side-effect and return a bool to indicate success.
  */
BlkInfo *__getBlkInfo(void *interior) nothrow
{
    BlkInfo *ptr = __blkcache;
    version (single_cache)
    {
        if (ptr.base && ptr.base <= interior && (interior - ptr.base) < ptr.size)
            return ptr;
        return null; // not in cache.
    }
    else version (simple_cache)
    {
        foreach (i; 0..N_CACHE_BLOCKS)
        {
            if (ptr.base && ptr.base <= interior && (interior - ptr.base) < ptr.size)
                return ptr;
            ptr++;
        }
    }
    else
    {
        // try to do a smart lookup, using __nextBlkIdx as the "head"
        auto curi = ptr + __nextBlkIdx;
        for (auto i = curi; i >= ptr; --i)
        {
            if (i.base && i.base <= interior && cast(size_t)(interior - i.base) < i.size)
                return i;
        }

        for (auto i = ptr + N_CACHE_BLOCKS - 1; i > curi; --i)
        {
            if (i.base && i.base <= interior && cast(size_t)(interior - i.base) < i.size)
                return i;
        }
    }
    return null; // not in cache.
}

void __insertBlkInfoCache(BlkInfo bi, BlkInfo *curpos) nothrow
{
    version (single_cache)
    {
        *__blkcache = bi;
    }
    else
    {
        version (simple_cache)
        {
            if (curpos)
                *curpos = bi;
            else
            {
                // note, this is a super-simple algorithm that does not care about
                // most recently used.  It simply uses a round-robin technique to
                // cache block info.  This means that the ordering of the cache
                // doesn't mean anything.  Certain patterns of allocation may
                // render the cache near-useless.
                __blkcache[__nextBlkIdx] = bi;
                __nextBlkIdx = (__nextBlkIdx+1) & (N_CACHE_BLOCKS - 1);
            }
        }
        else version (random_cache)
        {
            // strategy: if the block currently is in the cache, move the
            // current block index to the a random element and evict that
            // element.
            auto cache = __blkcache;
            if (!curpos)
            {
                __nextBlkIdx = (__nextRndNum = 1664525 * __nextRndNum + 1013904223) & (N_CACHE_BLOCKS - 1);
                curpos = cache + __nextBlkIdx;
            }
            else
            {
                __nextBlkIdx = curpos - cache;
            }
            *curpos = bi;
        }
        else
        {
            //
            // strategy: If the block currently is in the cache, swap it with
            // the head element.  Otherwise, move the head element up by one,
            // and insert it there.
            //
            auto cache = __blkcache;
            if (!curpos)
            {
                __nextBlkIdx = (__nextBlkIdx+1) & (N_CACHE_BLOCKS - 1);
                curpos = cache + __nextBlkIdx;
            }
            else if (curpos !is cache + __nextBlkIdx)
            {
                *curpos = cache[__nextBlkIdx];
                curpos = cache + __nextBlkIdx;
            }
            *curpos = bi;
        }
    }
}

/**
 * Shrink the "allocated" length of an array to be the exact size of the array.
 * It doesn't matter what the current allocated length of the array is, the
 * user is telling the runtime that he knows what he is doing.
 */
extern(C) void _d_arrayshrinkfit(const TypeInfo ti, void[] arr) /+nothrow+/
{
    // note, we do not care about shared.  We are setting the length no matter
    // what, so no lock is required.
    debug(PRINTF) printf("_d_arrayshrinkfit, elemsize = %d, arr.ptr = x%x arr.length = %d\n", ti.next.tsize, arr.ptr, arr.length);
    auto tinext = unqualify(ti.next);
    auto size = tinext.tsize;                  // array element size
    auto cursize = arr.length * size;
    auto isshared = typeid(ti) is typeid(TypeInfo_Shared);
    auto bic = isshared ? null : __getBlkInfo(arr.ptr);
    auto info = bic ? *bic : GC.query(arr.ptr);
    if (info.base && (info.attr & BlkAttr.APPENDABLE))
    {
        auto newsize = (arr.ptr - __arrayStart(info)) + cursize;

        debug(PRINTF) printf("setting allocated size to %d\n", (arr.ptr - info.base) + cursize);

        // destroy structs that become unused memory when array size is shrinked
        if (typeid(tinext) is typeid(TypeInfo_Struct)) // avoid a complete dynamic type cast
        {
            auto sti = cast(TypeInfo_Struct)cast(void*)tinext;
            if (sti.xdtor)
            {
                auto oldsize = __arrayAllocLength(info, tinext);
                if (oldsize > cursize)
                    finalize_array(arr.ptr + cursize, oldsize - cursize, sti);
            }
        }
        // Note: Since we "assume" the append is safe, it means it is not shared.
        // Since it is not shared, we also know it won't throw (no lock).
        if (!__setArrayAllocLength(info, newsize, false, tinext))
            onInvalidMemoryOperationError();

        // cache the block if not already done.
        if (!isshared && !bic)
            __insertBlkInfoCache(info, null);
    }
}

package bool hasPostblit(in TypeInfo ti)
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
 * set the array capacity.  If the array capacity isn't currently large enough
 * to hold the requested capacity (in number of elements), then the array is
 * resized/reallocated to the appropriate size.  Pass in a requested capacity
 * of 0 to get the current capacity.  Returns the number of elements that can
 * actually be stored once the resizing is done.
 */
extern(C) size_t _d_arraysetcapacity(const TypeInfo ti, size_t newcapacity, void[]* p)
in
{
    assert(ti);
    assert(!(*p).length || (*p).ptr);
}
body
{
    // step 1, get the block
    auto isshared = typeid(ti) is typeid(TypeInfo_Shared);
    auto bic = isshared ? null : __getBlkInfo((*p).ptr);
    auto info = bic ? *bic : GC.query((*p).ptr);
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

    // step 2, get the actual "allocated" size.  If the allocated size does not
    // match what we expect, then we will need to reallocate anyways.

    // TODO: this probably isn't correct for shared arrays
    size_t curallocsize = void;
    size_t curcapacity = void;
    size_t offset = void;
    size_t arraypad = void;
    if (info.base && (info.attr & BlkAttr.APPENDABLE))
    {
        if (info.size <= 256)
        {
            arraypad = SMALLPAD + structTypeInfoSize(tinext);
            curallocsize = *(cast(ubyte *)(info.base + info.size - arraypad));
        }
        else if (info.size < PAGESIZE)
        {
            arraypad = MEDPAD + structTypeInfoSize(tinext);
            curallocsize = *(cast(ushort *)(info.base + info.size - arraypad));
        }
        else
        {
            curallocsize = *(cast(size_t *)(info.base));
            arraypad = LARGEPAD;
        }


        offset = (*p).ptr - __arrayStart(info);
        if (offset + (*p).length * size != curallocsize)
        {
            curcapacity = 0;
        }
        else
        {
            // figure out the current capacity of the block from the point
            // of view of the array.
            curcapacity = info.size - offset - arraypad;
        }
    }
    else
    {
        curallocsize = curcapacity = offset = 0;
    }
    debug(PRINTF) printf("_d_arraysetcapacity, p = x%d,%d, newcapacity=%d, info.size=%d, reqsize=%d, curallocsize=%d, curcapacity=%d, offset=%d\n", (*p).ptr, (*p).length, newcapacity, info.size, reqsize, curallocsize, curcapacity, offset);

    if (curcapacity >= reqsize)
    {
        // no problems, the current allocated size is large enough.
        return curcapacity / size;
    }

    // step 3, try to extend the array in place.
    if (info.size >= PAGESIZE && curcapacity != 0)
    {
        auto extendsize = reqsize + offset + LARGEPAD - info.size;
        auto u = GC.extend(info.base, extendsize, extendsize);
        if (u)
        {
            // extend worked, save the new current allocated size
            if (bic)
                bic.size = u; // update cache
            curcapacity = u - offset - LARGEPAD;
            return curcapacity / size;
        }
    }

    // step 4, if extending doesn't work, allocate a new array with at least the requested allocated size.
    auto datasize = (*p).length * size;
    // copy attributes from original block, or from the typeinfo if the
    // original block doesn't exist.
    info = __arrayAlloc(reqsize, info, ti, tinext);
    if (info.base is null)
        goto Loverflow;
    // copy the data over.
    // note that malloc will have initialized the data we did not request to 0.
    auto tgt = __arrayStart(info);
    memcpy(tgt, (*p).ptr, datasize);

    // handle postblit
    __doPostblit(tgt, datasize, tinext);

    if (!(info.attr & BlkAttr.NO_SCAN))
    {
        // need to memset the newly requested data, except for the data that
        // malloc returned that we didn't request.
        void *endptr = tgt + reqsize;
        void *begptr = tgt + datasize;

        // sanity check
        assert(endptr >= begptr);
        memset(begptr, 0, endptr - begptr);
    }

    // set up the correct length
    __setArrayAllocLength(info, datasize, isshared, tinext);
    if (!isshared)
        __insertBlkInfoCache(info, bic);

    *p = (cast(void*)tgt)[0 .. (*p).length];

    // determine the padding.  This has to be done manually because __arrayPad
    // assumes you are not counting the pad size, and info.size does include
    // the pad.
    if (info.size <= 256)
        arraypad = SMALLPAD + structTypeInfoSize(tinext);
    else if (info.size < PAGESIZE)
        arraypad = MEDPAD + structTypeInfoSize(tinext);
    else
        arraypad = LARGEPAD;

    curcapacity = info.size - arraypad;
    return curcapacity / size;
}

/**
 * Allocate a new uninitialized array of length elements.
 * ti is the type of the resulting array, or pointer to element.
 */
extern (C) void[] _d_newarrayU(const TypeInfo ti, size_t length) pure nothrow
{
    auto tinext = unqualify(ti.next);
    auto size = tinext.tsize;

    debug(PRINTF) printf("_d_newarrayU(length = x%x, size = %d)\n", length, size);
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

    auto info = __arrayAlloc(size, ti, tinext);
    if (!info.base)
        goto Loverflow;
    debug(PRINTF) printf(" p = %p\n", info.base);
    // update the length of the array
    auto arrstart = __arrayStart(info);
    auto isshared = typeid(ti) is typeid(TypeInfo_Shared);
    __setArrayAllocLength(info, size, isshared, tinext);
    return arrstart[0..length];
}

/**
 * Allocate a new array of length elements.
 * ti is the type of the resulting array, or pointer to element.
 * (For when the array is initialized to 0)
 */
extern (C) void[] _d_newarrayT(const TypeInfo ti, size_t length) pure nothrow
{
    void[] result = _d_newarrayU(ti, length);
    auto tinext = unqualify(ti.next);
    auto size = tinext.tsize;

    memset(result.ptr, 0, size * length);
    return result;
}

/**
 * For when the array has a non-zero initializer.
 */
extern (C) void[] _d_newarrayiT(const TypeInfo ti, size_t length) pure nothrow
{
    import core.internal.traits : TypeTuple;

    void[] result = _d_newarrayU(ti, length);
    auto tinext = unqualify(ti.next);
    auto size = tinext.tsize;

    auto init = tinext.initializer();

    switch (init.length)
    {
    foreach (T; TypeTuple!(ubyte, ushort, uint, ulong))
    {
    case T.sizeof:
        (cast(T*)result.ptr)[0 .. size * length / T.sizeof] = *cast(T*)init.ptr;
        return result;
    }

    default:
    {
        immutable sz = init.length;
        for (size_t u = 0; u < size * length; u += sz)
            memcpy(result.ptr + u, init.ptr, sz);
        return result;
    }
    }
}


/**
 *
 */
void[] _d_newarrayOpT(alias op)(const TypeInfo ti, size_t[] dims)
{
    debug(PRINTF) printf("_d_newarrayOpT(ndims = %d)\n", dims.length);
    if (dims.length == 0)
        return null;

    void[] foo(const TypeInfo ti, size_t[] dims)
    {
        auto tinext = unqualify(ti.next);
        auto dim = dims[0];

        debug(PRINTF) printf("foo(ti = %p, ti.next = %p, dim = %d, ndims = %d\n", ti, ti.next, dim, dims.length);
        if (dims.length == 1)
        {
            auto r = op(ti, dim);
            return *cast(void[]*)(&r);
        }

        auto allocsize = (void[]).sizeof * dim;
        auto info = __arrayAlloc(allocsize, ti, tinext);
        auto isshared = typeid(ti) is typeid(TypeInfo_Shared);
        __setArrayAllocLength(info, allocsize, isshared, tinext);
        auto p = __arrayStart(info)[0 .. dim];

        foreach (i; 0..dim)
        {
            (cast(void[]*)p.ptr)[i] = foo(tinext, dims[1..$]);
        }
        return p;
    }

    auto result = foo(ti, dims);
    debug(PRINTF) printf("result = %llx\n", result.ptr);

    return result;
}


/**
 *
 */
extern (C) void[] _d_newarraymTX(const TypeInfo ti, size_t[] dims)
{
    debug(PRINTF) printf("_d_newarraymT(dims.length = %d)\n", dims.length);

    if (dims.length == 0)
        return null;
    else
    {
        return _d_newarrayOpT!(_d_newarrayT)(ti, dims);
    }
}


/**
 *
 */
extern (C) void[] _d_newarraymiTX(const TypeInfo ti, size_t[] dims)
{
    debug(PRINTF) printf("_d_newarraymiT(dims.length = %d)\n", dims.length);

    if (dims.length == 0)
        return null;
    else
    {
        return _d_newarrayOpT!(_d_newarrayiT)(ti, dims);
    }
}

/**
 * Allocate an uninitialized non-array item.
 * This is an optimization to avoid things needed for arrays like the __arrayPad(size).
 */
extern (C) void* _d_newitemU(in TypeInfo _ti)
{
    auto ti = unqualify(_ti);
    auto flags = !(ti.flags & 1) ? BlkAttr.NO_SCAN : 0;
    immutable tiSize = structTypeInfoSize(ti);
    immutable size = ti.tsize + tiSize;
    if (tiSize)
        flags |= BlkAttr.STRUCTFINAL | BlkAttr.FINALIZE;

    auto blkInf = GC.qalloc(size, flags, ti);
    auto p = blkInf.base;

    if (tiSize)
        *cast(TypeInfo*)(p + blkInf.size - tiSize) = cast() ti;

    return p;
}

/// Same as above, zero initializes the item.
extern (C) void* _d_newitemT(in TypeInfo _ti)
{
    auto p = _d_newitemU(_ti);
    memset(p, 0, _ti.tsize);
    return p;
}

/// Same as above, for item with non-zero initializer.
extern (C) void* _d_newitemiT(in TypeInfo _ti)
{
    auto p = _d_newitemU(_ti);
    auto init = _ti.initializer();
    assert(init.length <= _ti.tsize);
    memcpy(p, init.ptr, init.length);
    return p;
}

/**
 *
 */
struct Array
{
    size_t length;
    byte*  data;
}


/**
 * This function has been replaced by _d_delarray_t
 */
extern (C) void _d_delarray(void[]* p)
{
    _d_delarray_t(p, null);
}

debug(PRINTF)
{
    extern(C) void printArrayCache()
    {
        auto ptr = __blkcache;
        printf("CACHE: \n");
        foreach (i; 0 .. N_CACHE_BLOCKS)
        {
            printf("  %d\taddr:% .8x\tsize:% .10d\tflags:% .8x\n", i, ptr[i].base, ptr[i].size, ptr[i].attr);
        }
    }
}

/**
 *
 */
extern (C) void _d_delarray_t(void[]* p, const TypeInfo_Struct ti)
{
    if (p)
    {
        auto bic = __getBlkInfo(p.ptr);
        auto info = bic ? *bic : GC.query(p.ptr);

        if (info.base && (info.attr & BlkAttr.APPENDABLE))
        {
            if (ti) // ti non-null only if ti is a struct with dtor
            {
                void* start = __arrayStart(info);
                size_t length = __arrayAllocLength(info, ti);
                finalize_array(start, length, ti);
            }

            // if p is in the cache, clear it there as well
            if (bic)
                bic.base = null;

            GC.free(info.base);
            *p = null;
        }
    }
}

unittest
{
    __gshared size_t countDtor = 0;
    struct S
    {
        int x;
        ~this() { countDtor++; }
    }
    // destroy large array with x.ptr not base address of allocation
    auto x = new S[10000];
    void* p = x.ptr;
    assert(GC.addrOf(p) != null);
    delete x;
    assert(GC.addrOf(p) == null);
    assert(countDtor == 10000);

    // destroy full array even if only slice passed
    auto y = new S[400];
    auto z = y[200 .. 300];
    p = z.ptr;
    assert(GC.addrOf(p) != null);
    delete z;
    assert(GC.addrOf(p) == null);
    assert(countDtor == 10000 + 400);
}

/**
 *
 */
extern (C) void _d_delmemory(void* *p)
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
extern (C) void _d_callinterfacefinalizer(void *p)
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
extern (C) void _d_callfinalizer(void* p)
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
extern (C) int rt_hasFinalizerInSegment(void* p, size_t size, uint attr, in void[] segment) nothrow
{
    if (attr & BlkAttr.STRUCTFINAL)
    {
        if (attr & BlkAttr.APPENDABLE)
            return hasArrayFinalizerInSegment(p, size, segment);
        return hasStructFinalizerInSegment(p, size, segment);
    }

    // otherwise class
    auto ppv = cast(void**) p;
    if (!p || !*ppv)
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

int hasStructFinalizerInSegment(void* p, size_t size, in void[] segment) nothrow
{
    if (!p)
        return false;

    auto ti = *cast(TypeInfo_Struct*)(p + size - size_t.sizeof);
    return cast(size_t)(cast(void*)ti.xdtor - segment.ptr) < segment.length;
}

int hasArrayFinalizerInSegment(void* p, size_t size, in void[] segment) nothrow
{
    if (!p)
        return false;

    TypeInfo_Struct si = void;
    if (size < PAGESIZE)
        si = *cast(TypeInfo_Struct*)(p + size - size_t.sizeof);
    else
        si = *cast(TypeInfo_Struct*)(p + size_t.sizeof);

    return cast(size_t)(cast(void*)si.xdtor - segment.ptr) < segment.length;
}

// called by the GC
void finalize_array2(void* p, size_t size) nothrow
{
    debug(PRINTF) printf("rt_finalize_array2(p = %p)\n", p);

    TypeInfo_Struct si = void;
    if (size <= 256)
    {
        si = *cast(TypeInfo_Struct*)(p + size - size_t.sizeof);
        size = *cast(ubyte*)(p + size - size_t.sizeof - SMALLPAD);
    }
    else if (size < PAGESIZE)
    {
        si = *cast(TypeInfo_Struct*)(p + size - size_t.sizeof);
        size = *cast(ushort*)(p + size - size_t.sizeof - MEDPAD);
    }
    else
    {
        si = *cast(TypeInfo_Struct*)(p + size_t.sizeof);
        size = *cast(size_t*)p;
        p += LARGEPREFIX;
    }

    try
    {
        finalize_array(p, size, si);
    }
    catch (Exception e)
    {
        onFinalizeError(si, e);
    }
}

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
void finalize_struct(void* p, size_t size) nothrow
{
    debug(PRINTF) printf("finalize_struct(p = %p)\n", p);

    auto ti = *cast(TypeInfo_Struct*)(p + size - size_t.sizeof);
    try
    {
        ti.destroy(p); // call destructor
    }
    catch (Exception e)
    {
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
            p[0 .. w.length] = w[];
        }
    }
    catch (Exception e)
    {
        onFinalizeError(*pc, e);
    }
    finally
    {
        *ppv = null; // zero vptr even if `resetMemory` is false
    }
}

extern (C) void rt_finalize(void* p, bool det = true)
{
    rt_finalize2(p, det, true);
}

extern (C) void rt_finalizeFromGC(void* p, size_t size, uint attr)
{
    // to verify: reset memory necessary?
    if (!(attr & BlkAttr.STRUCTFINAL))
        rt_finalize2(p, false, false); // class
    else if (attr & BlkAttr.APPENDABLE)
        finalize_array2(p, size); // array of structs
    else
        finalize_struct(p, size); // struct
}


/**
 * Resize dynamic arrays with 0 initializers.
 */
extern (C) void[] _d_arraysetlengthT(const TypeInfo ti, size_t newlength, void[]* p)
in
{
    assert(ti);
    assert(!(*p).length || (*p).ptr);
}
body
{
    debug(PRINTF)
    {
        //printf("_d_arraysetlengthT(p = %p, sizeelem = %d, newlength = %d)\n", p, sizeelem, newlength);
        if (p)
            printf("\tp.ptr = %p, p.length = %d\n", (*p).ptr, (*p).length);
    }

    void* newdata = void;
    if (newlength)
    {
        if (newlength <= (*p).length)
        {
            *p = (*p)[0 .. newlength];
            newdata = (*p).ptr;
            return newdata[0 .. newlength];
        }
        auto tinext = unqualify(ti.next);
        size_t sizeelem = tinext.tsize;
        version (D_InlineAsm_X86)
        {
            size_t newsize = void;

            asm pure nothrow @nogc
            {
                mov EAX, newlength;
                mul EAX, sizeelem;
                mov newsize, EAX;
                jc  Loverflow;
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
                jc  Loverflow;
            }
        }
        else
        {
            import core.checkedint : mulu;

            bool overflow = false;
            size_t newsize = mulu(sizeelem, newlength, overflow);
            if (overflow)
                goto Loverflow;
        }

        debug(PRINTF) printf("newsize = %x, newlength = %x\n", newsize, newlength);

        auto   isshared = typeid(ti) is typeid(TypeInfo_Shared);

        if ((*p).ptr)
        {
            newdata = (*p).ptr;
            if (newlength > (*p).length)
            {
                size_t size = (*p).length * sizeelem;
                auto   bic = isshared ? null : __getBlkInfo((*p).ptr);
                auto   info = bic ? *bic : GC.query((*p).ptr);
                if (info.base && (info.attr & BlkAttr.APPENDABLE))
                {
                    // calculate the extent of the array given the base.
                    size_t offset = (*p).ptr - __arrayStart(info);
                    if (info.size >= PAGESIZE)
                    {
                        // size of array is at the front of the block
                        if (!__setArrayAllocLength(info, newsize + offset, isshared, tinext, size + offset))
                        {
                            // check to see if it failed because there is not
                            // enough space
                            if (*(cast(size_t*)info.base) == size + offset)
                            {
                                // not enough space, try extending
                                auto extendsize = newsize + offset + LARGEPAD - info.size;
                                auto u = GC.extend(info.base, extendsize, extendsize);
                                if (u)
                                {
                                    // extend worked, now try setting the length
                                    // again.
                                    info.size = u;
                                    if (__setArrayAllocLength(info, newsize + offset, isshared, tinext, size + offset))
                                    {
                                        if (!isshared)
                                            __insertBlkInfoCache(info, bic);
                                        goto L1;
                                    }
                                }
                            }

                            // couldn't do it, reallocate
                            goto L2;
                        }
                        else if (!isshared && !bic)
                        {
                            // add this to the cache, it wasn't present previously.
                            __insertBlkInfoCache(info, null);
                        }
                    }
                    else if (!__setArrayAllocLength(info, newsize + offset, isshared, tinext, size + offset))
                    {
                        // could not resize in place
                        goto L2;
                    }
                    else if (!isshared && !bic)
                    {
                        // add this to the cache, it wasn't present previously.
                        __insertBlkInfoCache(info, null);
                    }
                }
                else
                {
                    if (info.base)
                    {
                L2:
                        if (bic)
                        {
                            // a chance that flags have changed since this was cached, we should fetch the most recent flags
                            info.attr = GC.getAttr(info.base) | BlkAttr.APPENDABLE;
                        }
                        info = __arrayAlloc(newsize, info, ti, tinext);
                    }
                    else
                    {
                        info = __arrayAlloc(newsize, ti, tinext);
                    }

                    if (info.base is null)
                        goto Loverflow;

                    __setArrayAllocLength(info, newsize, isshared, tinext);
                    if (!isshared)
                        __insertBlkInfoCache(info, bic);
                    newdata = cast(byte *)__arrayStart(info);
                    newdata[0 .. size] = (*p).ptr[0 .. size];

                    // do postblit processing
                    __doPostblit(newdata, size, tinext);
                }
             L1:
                memset(newdata + size, 0, newsize - size);
            }
        }
        else
        {
            // pointer was null, need to allocate
            auto info = __arrayAlloc(newsize, ti, tinext);
            if (info.base is null)
                goto Loverflow;
            __setArrayAllocLength(info, newsize, isshared, tinext);
            if (!isshared)
                __insertBlkInfoCache(info, null);
            newdata = cast(byte *)__arrayStart(info);
            memset(newdata, 0, newsize);
        }
    }
    else
    {
        newdata = (*p).ptr;
    }

    *p = newdata[0 .. newlength];
    return *p;

Loverflow:
    onOutOfMemoryError();
    assert(0);
}


/**
 * Resize arrays for non-zero initializers.
 *      p               pointer to array lvalue to be updated
 *      newlength       new .length property of array
 *      sizeelem        size of each element of array
 *      initsize        size of initializer
 *      ...             initializer
 */
extern (C) void[] _d_arraysetlengthiT(const TypeInfo ti, size_t newlength, void[]* p)
in
{
    assert(!(*p).length || (*p).ptr);
}
body
{
    void* newdata;
    auto tinext = unqualify(ti.next);
    auto sizeelem = tinext.tsize;
    auto initializer = tinext.initializer();
    auto initsize = initializer.length;

    assert(sizeelem);
    assert(initsize);
    assert(initsize <= sizeelem);
    assert((sizeelem / initsize) * initsize == sizeelem);

    debug(PRINTF)
    {
        printf("_d_arraysetlengthiT(p = %p, sizeelem = %d, newlength = %d, initsize = %d)\n", p, sizeelem, newlength, initsize);
        if (p)
            printf("\tp.data = %p, p.length = %d\n", (*p).ptr, (*p).length);
    }

    if (newlength)
    {
        version (D_InlineAsm_X86)
        {
            size_t newsize = void;

            asm
            {
                mov     EAX,newlength   ;
                mul     EAX,sizeelem    ;
                mov     newsize,EAX     ;
                jc      Loverflow       ;
            }
        }
        else version (D_InlineAsm_X86_64)
        {
            size_t newsize = void;

            asm
            {
                mov     RAX,newlength   ;
                mul     RAX,sizeelem    ;
                mov     newsize,RAX     ;
                jc      Loverflow       ;
            }
        }
        else
        {
            import core.checkedint : mulu;

            bool overflow = false;
            size_t newsize = mulu(sizeelem, newlength, overflow);
            if (overflow)
                goto Loverflow;
        }
        debug(PRINTF) printf("newsize = %x, newlength = %x\n", newsize, newlength);


        size_t size = (*p).length * sizeelem;
        auto isshared = typeid(ti) is typeid(TypeInfo_Shared);
        if ((*p).ptr)
        {
            newdata = (*p).ptr;
            if (newlength > (*p).length)
            {
                auto   bic = isshared ? null : __getBlkInfo((*p).ptr);
                auto   info = bic ? *bic : GC.query((*p).ptr);

                // calculate the extent of the array given the base.
                size_t offset = (*p).ptr - __arrayStart(info);
                if (info.base && (info.attr & BlkAttr.APPENDABLE))
                {
                    if (info.size >= PAGESIZE)
                    {
                        // size of array is at the front of the block
                        if (!__setArrayAllocLength(info, newsize + offset, isshared, tinext, size + offset))
                        {
                            // check to see if it failed because there is not
                            // enough space
                            if (*(cast(size_t*)info.base) == size + offset)
                            {
                                // not enough space, try extending
                                auto extendsize = newsize + offset + LARGEPAD - info.size;
                                auto u = GC.extend(info.base, extendsize, extendsize);
                                if (u)
                                {
                                    // extend worked, now try setting the length
                                    // again.
                                    info.size = u;
                                    if (__setArrayAllocLength(info, newsize + offset, isshared, tinext, size + offset))
                                    {
                                        if (!isshared)
                                            __insertBlkInfoCache(info, bic);
                                        goto L1;
                                    }
                                }
                            }

                            // couldn't do it, reallocate
                            goto L2;
                        }
                        else if (!isshared && !bic)
                        {
                            // add this to the cache, it wasn't present previously.
                            __insertBlkInfoCache(info, null);
                        }
                    }
                    else if (!__setArrayAllocLength(info, newsize + offset, isshared, tinext, size + offset))
                    {
                        // could not resize in place
                        goto L2;
                    }
                    else if (!isshared && !bic)
                    {
                        // add this to the cache, it wasn't present previously.
                        __insertBlkInfoCache(info, null);
                    }
                }
                else
                {
                    // not appendable or not part of the heap yet.
                    if (info.base)
                    {
                L2:
                        if (bic)
                        {
                            // a chance that flags have changed since this was cached, we should fetch the most recent flags
                            info.attr = GC.getAttr(info.base) | BlkAttr.APPENDABLE;
                        }
                        info = __arrayAlloc(newsize, info, ti, tinext);
                    }
                    else
                    {
                        info = __arrayAlloc(newsize, ti, tinext);
                    }
                    __setArrayAllocLength(info, newsize, isshared, tinext);
                    if (!isshared)
                        __insertBlkInfoCache(info, bic);
                    newdata = cast(byte *)__arrayStart(info);
                    newdata[0 .. size] = (*p).ptr[0 .. size];

                    // do postblit processing
                    __doPostblit(newdata, size, tinext);
                }
                L1: ;
            }
        }
        else
        {
            // length was zero, need to allocate
            auto info = __arrayAlloc(newsize, ti, tinext);
            __setArrayAllocLength(info, newsize, isshared, tinext);
            if (!isshared)
                __insertBlkInfoCache(info, null);
            newdata = cast(byte *)__arrayStart(info);
        }

        auto q = initializer.ptr; // pointer to initializer

        if (newsize > size)
        {
            if (initsize == 1)
            {
                debug(PRINTF) printf("newdata = %p, size = %d, newsize = %d, *q = %d\n", newdata, size, newsize, *cast(byte*)q);
                memset(newdata + size, *cast(byte*)q, newsize - size);
            }
            else
            {
                for (size_t u = size; u < newsize; u += initsize)
                {
                    memcpy(newdata + u, q, initsize);
                }
            }
        }
    }
    else
    {
        newdata = (*p).ptr;
    }

    *p = newdata[0 .. newlength];
    return *p;

Loverflow:
    onOutOfMemoryError();
    assert(0);
}


/**
 * Append y[] to array x[]
 */
extern (C) void[] _d_arrayappendT(const TypeInfo ti, ref byte[] x, byte[] y)
{
    auto length = x.length;
    auto tinext = unqualify(ti.next);
    auto sizeelem = tinext.tsize;              // array element size
    _d_arrayappendcTX(ti, x, y.length);
    memcpy(x.ptr + length * sizeelem, y.ptr, y.length * sizeelem);

    // do postblit
    __doPostblit(x.ptr + length * sizeelem, y.length * sizeelem, tinext);
    return x;
}


/**
 *
 */
size_t newCapacity(size_t newlength, size_t size)
{
    version (none)
    {
        size_t newcap = newlength * size;
    }
    else
    {
        /*
         * Better version by Dave Fladebo:
         * This uses an inverse logorithmic algorithm to pre-allocate a bit more
         * space for larger arrays.
         * - Arrays smaller than PAGESIZE bytes are left as-is, so for the most
         * common cases, memory allocation is 1 to 1. The small overhead added
         * doesn't affect small array perf. (it's virtually the same as
         * current).
         * - Larger arrays have some space pre-allocated.
         * - As the arrays grow, the relative pre-allocated space shrinks.
         * - The logorithmic algorithm allocates relatively more space for
         * mid-size arrays, making it very fast for medium arrays (for
         * mid-to-large arrays, this turns out to be quite a bit faster than the
         * equivalent realloc() code in C, on Linux at least. Small arrays are
         * just as fast as GCC).
         * - Perhaps most importantly, overall memory usage and stress on the GC
         * is decreased significantly for demanding environments.
         */
        size_t newcap = newlength * size;
        size_t newext = 0;

        if (newcap > PAGESIZE)
        {
            //double mult2 = 1.0 + (size / log10(pow(newcap * 2.0,2.0)));

            // redo above line using only integer math

            /*static int log2plus1(size_t c)
            {   int i;

                if (c == 0)
                    i = -1;
                else
                    for (i = 1; c >>= 1; i++)
                    {
                    }
                return i;
            }*/

            /* The following setting for mult sets how much bigger
             * the new size will be over what is actually needed.
             * 100 means the same size, more means proportionally more.
             * More means faster but more memory consumption.
             */
            //long mult = 100 + (1000L * size) / (6 * log2plus1(newcap));
            //long mult = 100 + (1000L * size) / log2plus1(newcap);
            long mult = 100 + (1000L) / (bsr(newcap) + 1);

            // testing shows 1.02 for large arrays is about the point of diminishing return
            //
            // Commented out because the multipler will never be < 102.  In order for it to be < 2,
            // then 1000L / (bsr(x) + 1) must be > 2.  The highest bsr(x) + 1
            // could be is 65 (64th bit set), and 1000L / 64 is much larger
            // than 2.  We need 500 bit integers for 101 to be achieved :)
            /*if (mult < 102)
                mult = 102;*/
            /*newext = cast(size_t)((newcap * mult) / 100);
            newext -= newext % size;*/
            // This version rounds up to the next element, and avoids using
            // mod.
            newext = cast(size_t)((newlength * mult + 99) / 100) * size;
            debug(PRINTF) printf("mult: %2.2f, alloc: %2.2f\n",mult/100.0,newext / cast(double)size);
        }
        newcap = newext > newcap ? newext : newcap;
        debug(PRINTF) printf("newcap = %d, newlength = %d, size = %d\n", newcap, newlength, size);
    }
    return newcap;
}


/**************************************
 * Extend an array by n elements.
 * Caller must initialize those elements.
 */
extern (C)
byte[] _d_arrayappendcTX(const TypeInfo ti, ref byte[] px, size_t n)
{
    // This is a cut&paste job from _d_arrayappendT(). Should be refactored.

    // only optimize array append where ti is not a shared type
    auto tinext = unqualify(ti.next);
    auto sizeelem = tinext.tsize;              // array element size
    auto isshared = typeid(ti) is typeid(TypeInfo_Shared);
    auto bic = isshared ? null : __getBlkInfo(px.ptr);
    auto info = bic ? *bic : GC.query(px.ptr);
    auto length = px.length;
    auto newlength = length + n;
    auto newsize = newlength * sizeelem;
    auto size = length * sizeelem;
    size_t newcap = void; // for scratch space

    // calculate the extent of the array given the base.
    size_t offset = cast(void*)px.ptr - __arrayStart(info);
    if (info.base && (info.attr & BlkAttr.APPENDABLE))
    {
        if (info.size >= PAGESIZE)
        {
            // size of array is at the front of the block
            if (!__setArrayAllocLength(info, newsize + offset, isshared, tinext, size + offset))
            {
                // check to see if it failed because there is not
                // enough space
                newcap = newCapacity(newlength, sizeelem);
                if (*(cast(size_t*)info.base) == size + offset)
                {
                    // not enough space, try extending
                    auto extendoffset = offset + LARGEPAD - info.size;
                    auto u = GC.extend(info.base, newsize + extendoffset, newcap + extendoffset);
                    if (u)
                    {
                        // extend worked, now try setting the length
                        // again.
                        info.size = u;
                        if (__setArrayAllocLength(info, newsize + offset, isshared, tinext, size + offset))
                        {
                            if (!isshared)
                                __insertBlkInfoCache(info, bic);
                            goto L1;
                        }
                    }
                }

                // couldn't do it, reallocate
                goto L2;
            }
            else if (!isshared && !bic)
            {
                __insertBlkInfoCache(info, null);
            }
        }
        else if (!__setArrayAllocLength(info, newsize + offset, isshared, tinext, size + offset))
        {
            // could not resize in place
            newcap = newCapacity(newlength, sizeelem);
            goto L2;
        }
        else if (!isshared && !bic)
        {
            __insertBlkInfoCache(info, null);
        }
    }
    else
    {
        // not appendable or is null
        newcap = newCapacity(newlength, sizeelem);
        if (info.base)
        {
    L2:
            if (bic)
            {
                // a chance that flags have changed since this was cached, we should fetch the most recent flags
                info.attr = GC.getAttr(info.base) | BlkAttr.APPENDABLE;
            }
            info = __arrayAlloc(newcap, info, ti, tinext);
        }
        else
        {
            info = __arrayAlloc(newcap, ti, tinext);
        }
        __setArrayAllocLength(info, newsize, isshared, tinext);
        if (!isshared)
            __insertBlkInfoCache(info, bic);
        auto newdata = cast(byte *)__arrayStart(info);
        memcpy(newdata, px.ptr, length * sizeelem);
        // do postblit processing
        __doPostblit(newdata, length * sizeelem, tinext);
        (cast(void **)(&px))[1] = newdata;
    }

  L1:
    *cast(size_t *)&px = newlength;
    return px;
}


/**
 * Append dchar to char[]
 */
extern (C) void[] _d_arrayappendcd(ref byte[] x, dchar c)
{
    // c could encode into from 1 to 4 characters
    char[4] buf = void;
    byte[] appendthis; // passed to appendT
    if (c <= 0x7F)
    {
        buf.ptr[0] = cast(char)c;
        appendthis = (cast(byte *)buf.ptr)[0..1];
    }
    else if (c <= 0x7FF)
    {
        buf.ptr[0] = cast(char)(0xC0 | (c >> 6));
        buf.ptr[1] = cast(char)(0x80 | (c & 0x3F));
        appendthis = (cast(byte *)buf.ptr)[0..2];
    }
    else if (c <= 0xFFFF)
    {
        buf.ptr[0] = cast(char)(0xE0 | (c >> 12));
        buf.ptr[1] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        buf.ptr[2] = cast(char)(0x80 | (c & 0x3F));
        appendthis = (cast(byte *)buf.ptr)[0..3];
    }
    else if (c <= 0x10FFFF)
    {
        buf.ptr[0] = cast(char)(0xF0 | (c >> 18));
        buf.ptr[1] = cast(char)(0x80 | ((c >> 12) & 0x3F));
        buf.ptr[2] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        buf.ptr[3] = cast(char)(0x80 | (c & 0x3F));
        appendthis = (cast(byte *)buf.ptr)[0..4];
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
    return _d_arrayappendT(typeid(shared char[]), x, appendthis);
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
 * Append dchar to wchar[]
 */
extern (C) void[] _d_arrayappendwd(ref byte[] x, dchar c)
{
    // c could encode into from 1 to 2 w characters
    wchar[2] buf = void;
    byte[] appendthis; // passed to appendT
    if (c <= 0xFFFF)
    {
        buf.ptr[0] = cast(wchar) c;
        // note that although we are passing only 1 byte here, appendT
        // interprets this as being an array of wchar, making the necessary
        // casts.
        appendthis = (cast(byte *)buf.ptr)[0..1];
    }
    else
    {
        buf.ptr[0] = cast(wchar) ((((c - 0x10000) >> 10) & 0x3FF) + 0xD800);
        buf.ptr[1] = cast(wchar) (((c - 0x10000) & 0x3FF) + 0xDC00);
        // ditto from above.
        appendthis = (cast(byte *)buf.ptr)[0..2];
    }

    //
    // TODO: This always assumes the array type is shared, because we do not
    // get a typeinfo from the compiler.  Assuming shared is the safest option.
    // Once the compiler is fixed, the proper typeinfo should be forwarded.
    //
    return _d_arrayappendT(typeid(shared wchar[]), x, appendthis);
}


/**
 *
 */
extern (C) byte[] _d_arraycatT(const TypeInfo ti, byte[] x, byte[] y)
out (result)
{
    auto tinext = unqualify(ti.next);
    auto sizeelem = tinext.tsize;              // array element size
    debug(PRINTF) printf("_d_arraycatT(%d,%p ~ %d,%p sizeelem = %d => %d,%p)\n", x.length, x.ptr, y.length, y.ptr, sizeelem, result.length, result.ptr);
    assert(result.length == x.length + y.length);

    // If a postblit is involved, the contents of result might rightly differ
    // from the bitwise concatenation of x and y.
    if (!hasPostblit(tinext))
    {
        for (size_t i = 0; i < x.length * sizeelem; i++)
            assert((cast(byte*)result)[i] == (cast(byte*)x)[i]);
        for (size_t i = 0; i < y.length * sizeelem; i++)
            assert((cast(byte*)result)[x.length * sizeelem + i] == (cast(byte*)y)[i]);
    }

    size_t cap = GC.sizeOf(result.ptr);
    assert(!cap || cap > result.length * sizeelem);
}
body
{
    version (none)
    {
        /* Cannot use this optimization because:
         *  char[] a, b;
         *  char c = 'a';
         *  b = a ~ c;
         *  c = 'b';
         * will change the contents of b.
         */
        if (!y.length)
            return x;
        if (!x.length)
            return y;
    }

    auto tinext = unqualify(ti.next);
    auto sizeelem = tinext.tsize;              // array element size
    debug(PRINTF) printf("_d_arraycatT(%d,%p ~ %d,%p sizeelem = %d)\n", x.length, x.ptr, y.length, y.ptr, sizeelem);
    size_t xlen = x.length * sizeelem;
    size_t ylen = y.length * sizeelem;
    size_t len  = xlen + ylen;

    if (!len)
        return null;

    auto info = __arrayAlloc(len, ti, tinext);
    byte* p = cast(byte*)__arrayStart(info);
    p[len] = 0; // guessing this is to optimize for null-terminated arrays?
    memcpy(p, x.ptr, xlen);
    memcpy(p + xlen, y.ptr, ylen);
    // do postblit processing
    __doPostblit(p, xlen + ylen, tinext);

    auto isshared = typeid(ti) is typeid(TypeInfo_Shared);
    __setArrayAllocLength(info, len, isshared, tinext);
    return p[0 .. x.length + y.length];
}


/**
 *
 */
extern (C) void[] _d_arraycatnTX(const TypeInfo ti, byte[][] arrs)
{
    size_t length;
    auto tinext = unqualify(ti.next);
    auto size = tinext.tsize;   // array element size

    foreach (b; arrs)
        length += b.length;

    if (!length)
        return null;

    auto allocsize = length * size;
    auto info = __arrayAlloc(allocsize, ti, tinext);
    auto isshared = typeid(ti) is typeid(TypeInfo_Shared);
    __setArrayAllocLength(info, allocsize, isshared, tinext);
    void *a = __arrayStart (info);

    size_t j = 0;
    foreach (b; arrs)
    {
        if (b.length)
        {
            memcpy(a + j, b.ptr, b.length * size);
            j += b.length * size;
        }
    }

    // do postblit processing
    __doPostblit(a, j, tinext);

    return a[0..length];
}


/**
 * Allocate the array, rely on the caller to do the initialization of the array.
 */
extern (C)
void* _d_arrayliteralTX(const TypeInfo ti, size_t length)
{
    auto tinext = unqualify(ti.next);
    auto sizeelem = tinext.tsize;              // array element size
    void* result;

    debug(PRINTF) printf("_d_arrayliteralTX(sizeelem = %d, length = %d)\n", sizeelem, length);
    if (length == 0 || sizeelem == 0)
        result = null;
    else
    {
        auto allocsize = length * sizeelem;
        auto info = __arrayAlloc(allocsize, ti, tinext);
        auto isshared = typeid(ti) is typeid(TypeInfo_Shared);
        __setArrayAllocLength(info, allocsize, isshared, tinext);
        result = __arrayStart(info);
    }
    return result;
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

// cannot define structs inside unit test block, or they become nested structs.
version (unittest)
{
    struct S1
    {
        int x = 5;
    }
    struct S2
    {
        int x;
        this(int x) {this.x = x;}
    }
    struct S3
    {
        int[4] x;
        this(int x)
        {this.x[] = x;}
    }
    struct S4
    {
        int *x;
    }

}

unittest
{
    auto s1 = new S1;
    assert(s1.x == 5);
    assert(GC.getAttr(s1) == BlkAttr.NO_SCAN);

    auto s2 = new S2(3);
    assert(s2.x == 3);
    assert(GC.getAttr(s2) == BlkAttr.NO_SCAN);

    auto s3 = new S3(1);
    assert(s3.x == [1,1,1,1]);
    assert(GC.getAttr(s3) == BlkAttr.NO_SCAN);
    debug(SENTINEL) {} else
        assert(GC.sizeOf(s3) == 16);

    auto s4 = new S4;
    assert(s4.x == null);
    assert(GC.getAttr(s4) == 0);
}

unittest
{
    // Bugzilla 3454 - Inconsistent flag setting in GC.realloc()
    static void test(size_t multiplier)
    {
        auto p = GC.malloc(8 * multiplier, BlkAttr.NO_SCAN);
        assert(GC.getAttr(p) == BlkAttr.NO_SCAN);
        p = GC.realloc(p, 2 * multiplier, BlkAttr.NO_SCAN);
        assert(GC.getAttr(p) == BlkAttr.NO_SCAN);
    }
    test(1);
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
    // bugzilla 13854
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
    // bugzilla 13878
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
unittest
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
    S1* s1 = new S1;
    delete s1;
    assert(dtorCount == 1);

    dtorCount = 0;
    S1[] arr1 = new S1[7];
    delete arr1;
    assert(dtorCount == 7);

    if (callStructDtorsDuringGC)
    {
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
    }

    dtorCount = 0;
    S1[] arr2 = new S1[10];
    arr2.length = 6;
    arr2.assumeSafeAppend;
    assert(dtorCount == 4); // destructors run explicitely?

    if (callStructDtorsDuringGC)
    {
        dtorCount = 0;
        BlkInfo blkinf = GC.query(arr2.ptr);
        GC.runFinalizers((cast(char*)(typeid(S1).xdtor))[0..1]);
        assert(dtorCount == 6);
        GC.free(blkinf.base);
    }

    // associative arrays
    import rt.aaA : entryDtor;
    // throw away all existing AA entries with dtor
    GC.runFinalizers((cast(char*)(&entryDtor))[0..1]);

    S1[int] aa1;
    aa1[0] = S1(0);
    aa1[1] = S1(1);
    if (callStructDtorsDuringGC)
    {
        dtorCount = 0;
        aa1 = null;
        GC.runFinalizers((cast(char*)(&entryDtor))[0..1]);
        assert(dtorCount == 2);
    }

    int[S1] aa2;
    aa2[S1(0)] = 0;
    aa2[S1(1)] = 1;
    aa2[S1(2)] = 2;
    if (callStructDtorsDuringGC)
    {
        dtorCount = 0;
        aa2 = null;
        GC.runFinalizers((cast(char*)(&entryDtor))[0..1]);
        assert(dtorCount == 3);
    }

    S1[2][int] aa3;
    aa3[0] = [S1(0),S1(2)];
    aa3[1] = [S1(1),S1(3)];
    if (callStructDtorsDuringGC)
    {
        dtorCount = 0;
        aa3 = null;
        GC.runFinalizers((cast(char*)(&entryDtor))[0..1]);
        assert(dtorCount == 4);
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

// test struct finalizers exception handling
debug(SENTINEL) {} else
unittest
{
    if (!callStructDtorsDuringGC)
        return;

    bool test(E)()
    {
        import core.exception;
        static struct S1
        {
            E exc;
            ~this() { throw exc; }
        }

        bool caught = false;
        S1* s = new S1(new E("test onFinalizeError"));
        try
        {
            GC.runFinalizers((cast(char*)(typeid(S1).xdtor))[0..1]);
        }
        catch (FinalizeError err)
        {
            caught = true;
        }
        catch (E)
        {
        }
        GC.free(s);
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
