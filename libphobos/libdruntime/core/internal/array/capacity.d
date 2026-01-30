/**
 This module contains support for controlling dynamic arrays' capacity and length

  Copyright: Copyright Digital Mars 2000 - 2019.
  License: Distributed under the
       $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
     (See accompanying file LICENSE)
  Source: $(DRUNTIMESRC core/internal/_array/_capacity.d)
*/
module core.internal.array.capacity;

debug (PRINTF) import core.stdc.stdio : printf;
debug (VALGRIND) import etc.valgrind.valgrind;

// for now, all GC array functions are not exposed via core.memory.
extern(C) {
    bool gc_expandArrayUsed(void[] slice, size_t newUsed, bool atomic) nothrow pure;
    size_t gc_reserveArrayCapacity(void[] slice, size_t request, bool atomic) nothrow pure;
    bool gc_shrinkArrayUsed(void[] slice, size_t existingUsed, bool atomic) nothrow pure;
    void[] gc_getArrayUsed(void *ptr, bool atomic) nothrow pure;
}

/**
Shrink the "allocated" length of an array to be the exact size of the array.

It doesn't matter what the current allocated length of the array is, the
user is telling the runtime that he knows what he is doing.

Params:
    T = the type of the elements in the array (this should be unqualified)
    arr = array to shrink. Its `.length` is element length, not byte length, despite `void` type
    isshared = true if the underlying data is shared
*/
void _d_arrayshrinkfit(Tarr: T[], T)(Tarr arr, bool isshared) @trusted
{
    import core.exception : onFinalizeError;
    import core.internal.traits: hasElaborateDestructor;

    debug(PRINTF) printf("_d_arrayshrinkfit, elemsize = %zd, arr.ptr = %p arr.length = %zd\n", T.sizeof, arr.ptr, arr.length);
    auto reqlen = arr.length;

    auto curArr = cast(Tarr)gc_getArrayUsed(arr.ptr, isshared);
    if (curArr.ptr is null)
        // not a valid GC pointer
        return;

    // align the array.
    auto offset = arr.ptr - curArr.ptr;
    auto curlen = curArr.length - offset;
    if (curlen <= reqlen)
        // invalid situation, or no change.
        return;

    // if the type has a destructor, destroy elements we are about to remove.
    static if(is(T == struct) && hasElaborateDestructor!T)
    {
        try
        {
            // Finalize the elements that are being removed

            // Due to the fact that the delete operator calls destructors
            // for arrays from the last element to the first, we maintain
            // compatibility here by doing the same.
            for (auto curP = arr.ptr + curlen - 1; curP >= arr.ptr + reqlen; curP--)
            {
                // call destructor
                curP.__xdtor();
            }
        }
        catch (Exception e)
        {
            onFinalizeError(typeid(T), e);
        }
    }

    gc_shrinkArrayUsed(arr[0 .. reqlen], curlen * T.sizeof, isshared);
}

/**
Set the array capacity.

If the array capacity isn't currently large enough
to hold the requested capacity (in number of elements), then the array is
resized/reallocated to the appropriate size.

Pass in a requested capacity of 0 to get the current capacity.

Params:
    T = the type of the elements in the array (this should be unqualified)
    newcapacity = requested new capacity
    p = pointer to array to set. Its `length` is left unchanged.
    isshared = true if the underlying data is shared

Returns: the number of elements that can actually be stored once the resizing is done
*/
size_t _d_arraysetcapacityPureNothrow(T)(size_t newcapacity, void[]* p, bool isshared) pure nothrow @trusted
do
{
    alias PureNothrowType = size_t function(size_t, void[]*, bool) pure nothrow @trusted;
    return (cast(PureNothrowType) &_d_arraysetcapacity!T)(newcapacity, p, isshared);
}

size_t _d_arraysetcapacity(T)(size_t newcapacity, void[]* p, bool isshared) @trusted
in
{
    assert(!(*p).length || (*p).ptr);
}
do
{
    import core.checkedint : mulu;
    import core.exception : onOutOfMemoryError;
    import core.stdc.string : memcpy, memset;
    import core.internal.array.utils: __typeAttrs;
    import core.internal.lifetime : __doPostblit;

    import core.memory : GC;

    alias BlkAttr = GC.BlkAttr;

    auto size = T.sizeof;

    bool overflow = false;
    const reqsize = mulu(size, newcapacity, overflow);
    if (overflow)
    {
        onOutOfMemoryError();
        assert(0);
    }

    // step 1, see if we can ensure the capacity is valid in-place
    auto datasize = (*p).length * size;
    auto curCapacity = gc_reserveArrayCapacity((*p).ptr[0 .. datasize], reqsize, isshared);
    if (curCapacity != 0) // in-place worked!
        return curCapacity / size;

    if (reqsize <= datasize) // requested size is less than array size, the current array satisfies
        // the request. But this is not an appendable GC array, so return 0.
        return 0;

    // step 2, if reserving in-place doesn't work, allocate a new array with at
    // least the requested allocated size.
    auto attrs = __typeAttrs!T((*p).ptr) | BlkAttr.APPENDABLE;

    // use this static enum to avoid recomputing TypeInfo for every call.
    static enum ti = typeid(T);
    auto ptr = GC.malloc(reqsize, attrs, ti);
    if (ptr is null)
    {
        onOutOfMemoryError();
        assert(0);
    }

    // copy the data over.
    // note that malloc will have initialized the data we did not request to 0.
    memcpy(ptr, (*p).ptr, datasize);

    // handle postblit
    __doPostblit!T(cast(T[])ptr[0 .. datasize]);

    if (!(attrs & BlkAttr.NO_SCAN))
    {
        // need to memset the newly requested data, except for the data that
        // malloc returned that we didn't request.
        void* endptr = ptr + reqsize;
        void* begptr = ptr + datasize;

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
Resize a dynamic array by setting its `.length` property.

Newly created elements are initialized based on their default value.
If the array's elements initialize to `0`, memory is zeroed out. Otherwise, elements are explicitly initialized.

This function handles memory allocation, expansion, and initialization while maintaining array integrity.

---
void main()
{
    int[] a = [1, 2];
    a.length = 3; // Gets lowered to `_d_arraysetlengthT!(int)(a, 3, false)`
}
---

Params:
    arr         = The array to resize.
    newlength   = The new value for the array's `.length`.

Returns:
    The resized array with updated length and properly initialized elements.

Throws:
    OutOfMemoryError if allocation fails.
*/
size_t _d_arraysetlengthT(Tarr : T[], T)(return ref scope Tarr arr, size_t newlength) @trusted
{
    import core.internal.traits : Unqual;

    // Check if the type is shared
    enum isShared = is(T == shared);

    // Unqualify the type to remove `const`, `immutable`, `shared`, etc.
    alias UnqT = Unqual!T;

    // Cast the array to the unqualified type
    auto unqual_arr = cast(UnqT[]) arr;

    // Call the implementation with the unqualified array and sharedness flag
    size_t result = _d_arraysetlengthT_(unqual_arr, newlength, isShared);

    arr = cast(Tarr) unqual_arr;
    // Return the result
    return result;
}

private size_t _d_arraysetlengthT_(Tarr : T[], T)(return ref scope Tarr arr, size_t newlength, bool isShared) @trusted
{
    import core.checkedint : mulu;
    import core.exception : onFinalizeError, onOutOfMemoryError;
    import core.stdc.string : memcpy, memset;
    import core.internal.traits : hasElaborateCopyConstructor, Unqual;
    import core.lifetime : emplace;
    import core.memory;
    import core.internal.lifetime : __doPostblit;

    alias BlkAttr = GC.BlkAttr;
    alias UnqT = Unqual!T;

    debug(PRINTF)
    {
        printf("_d_arraysetlengthT(arr.ptr = %p, arr.length = %zd, newlength = %zd)\n",
            arr.ptr, arr.length, newlength);
    }

    // If the new length is less than or equal to the current length, just truncate the array
    if (newlength <= arr.length)
    {
        arr = arr[0 .. newlength];
        return newlength;
    }

    enum sizeelem = T.sizeof;
    enum hasPostblit = __traits(hasMember, T, "__postblit");
    enum hasEnabledPostblit = hasPostblit && !__traits(isDisabled, T.__postblit);

    bool overflow = false;
    const newsize = mulu(sizeelem, newlength, overflow);
    if (overflow)
    {
        onOutOfMemoryError();
        assert(0);
    }

    debug(PRINTF) printf("newsize = %zx\n", newsize);

    uint gcAttrs = BlkAttr.APPENDABLE;
    static if (is(T == struct) && __traits(hasMember, T, "xdtor"))
    {
        gcAttrs |= BlkAttr.FINALIZE;
    }

    if (!arr.ptr)
    {
        assert(arr.length == 0);
        void* ptr = GC.malloc(newsize, gcAttrs);
        if (!ptr)
        {
            onOutOfMemoryError();
            assert(0);
        }

        static if (__traits(isZeroInit, T))
        {
            memset(ptr, 0, newsize);
        }
        else static if (hasElaborateCopyConstructor!T && !hasPostblit)
        {
            foreach (i; 0 .. newlength)
                emplace(cast(UnqT*) ptr + i, UnqT.init); // safe default construction
        }
        else
        {
            auto temp = UnqT.init;
            foreach (i; 0 .. newlength)
                memcpy(cast(UnqT*) ptr + i, cast(const void*)&temp, T.sizeof);

            static if (hasEnabledPostblit)
                __doPostblit!T((cast(T*) ptr)[0 .. newlength]);
        }

        arr = (cast(T*) ptr)[0 .. newlength];
        return newlength;
    }

    size_t oldsize = arr.length * sizeelem;

    auto newdata = cast(void*) arr.ptr;

    if (!gc_expandArrayUsed(newdata[0 .. oldsize], newsize, isShared))
    {
        newdata = GC.malloc(newsize, gcAttrs);
        if (!newdata)
        {
            onOutOfMemoryError();
            assert(0);
        }
        static if (hasElaborateCopyConstructor!T && !hasPostblit)
        {
            // Use emplace for types with copy constructors but not postblit
            foreach (i; 0 .. arr.length)
                emplace(cast(UnqT*)newdata + i, arr[i]); // safe copy
        }
        else
        {
            memcpy(newdata, cast(const(void)*)arr.ptr, oldsize);

            // Postblit handling for types with postblit, but ensure it compiles
            static if (hasEnabledPostblit)
                __doPostblit!T((cast(T*) (cast(ubyte*)newdata))[0 .. arr.length]);
        }
    }

    // Handle initialization based on whether the type requires zero-init
    static if (__traits(isZeroInit, T))
        memset(cast(void*) (cast(ubyte*)newdata + oldsize), 0, newsize - oldsize);
    else static if (hasElaborateCopyConstructor!T && !hasPostblit)
    {
        foreach (i; 0 .. newlength - arr.length)
            emplace(cast(UnqT*) (cast(ubyte*)newdata + oldsize) + i, UnqT.init);
    }
    else
    {
        auto temp = UnqT.init;
        foreach (i; 0 .. newlength - arr.length)
            memcpy(cast(UnqT*) (cast(ubyte*)newdata + oldsize) + i, cast(const void*)&temp, T.sizeof);

        static if (hasEnabledPostblit)
            __doPostblit!T((cast(T*) (cast(ubyte*)newdata + oldsize))[0 .. newlength - arr.length]);
    }

    arr = (cast(T*) newdata)[0 .. newlength];
    return newlength;
}

version (D_ProfileGC)
{
    enum errorMessage = "Cannot resize arrays";
    import core.internal.array.utils : _d_HookTraceImpl;

    // Function wrapper around the hook, so it’s callable
    size_t _d_arraysetlengthTTrace(Tarr : T[], T)(
        return ref scope Tarr arr,
        size_t newlength,
        string file = __FILE__,
        int line = __LINE__,
        string func = __FUNCTION__
    ) @trusted
    {
        alias Hook = _d_HookTraceImpl!(Tarr, _d_arraysetlengthT!Tarr, errorMessage);
        return Hook(arr, newlength, file, line, func);
    }
}

// @safe unittest remains intact
@safe unittest
{
    struct S
    {
        float f = 1.0;
    }

    int[] arr;
    _d_arraysetlengthT!(typeof(arr))(arr, 16);
    assert(arr.length == 16);
    foreach (int i; arr)
        assert(i == int.init);

    shared S[] arr2;
    _d_arraysetlengthT!(typeof(arr2))(arr2, 16);
    assert(arr2.length == 16);
    foreach (s; arr2)
        assert(s == S.init);
}
