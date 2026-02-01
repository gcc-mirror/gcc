/**
 This module contains support for controlling dynamic arrays' appending

  Copyright: Copyright Digital Mars 2000 - 2019.
  License: Distributed under the
       $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
     (See accompanying file LICENSE)
  Source: $(DRUNTIMESRC core/_internal/_array/_appending.d)
*/
module core.internal.array.appending;

private extern (C)
{
    bool gc_expandArrayUsed(void[] slice, size_t newUsed, bool atomic) pure nothrow;
    bool gc_shrinkArrayUsed(void[] slice, size_t existingUsed, bool atomic) pure nothrow;
}

private enum isCopyingNothrow(T) = __traits(compiles, (ref T rhs) nothrow { T lhs = rhs; });

/**
 * Extend an array `px` by `n` elements.
 * Caller must initialize those elements.
 * Params:
 *  px = the array that will be extended, taken as a reference
 *  n = how many new elements to extend it with
 * Returns:
 *  The new value of `px`
 * Bugs:
 *  This function template was ported from a much older runtime hook that bypassed safety,
 *  purity, and throwabilty checks. To prevent breaking existing code, this function template
 *  is temporarily declared `@trusted` until the implementation can be brought up to modern D expectations.
 */
ref Tarr _d_arrayappendcTX(Tarr : T[], T)(return ref scope Tarr px, size_t n) @trusted
{
    import core.internal.traits: Unqual;

    alias Unqual_T = Unqual!T;
    alias Unqual_Tarr = Unqual_T[];
    enum isshared = is(T == shared);
    auto unqual_px = cast(Unqual_Tarr) px;

    // Ignoring additional attributes allows reusing the same generated code
    px = cast(Tarr)_d_arrayappendcTX_(unqual_px, n, isshared);
    return px;
}

private ref Tarr _d_arrayappendcTX_(Tarr : T[], T)(return ref scope Tarr px, size_t n, bool isshared) @trusted
{
    version (DigitalMars) pragma(inline, false);
    version (D_TypeInfo)
    {
        // Short circuit if no data is being appended.
        if (n == 0)
            return px;

        import core.stdc.string : memcpy, memset;
        import core.internal.lifetime : __doPostblit;
        import core.internal.array.utils: __arrayAlloc, newCapacity, __typeAttrs;
        import core.internal.gc.blockmeta : PAGESIZE;
        import core.exception: onOutOfMemoryError;
        import core.memory: GC;

        alias BlkAttr = GC.BlkAttr;

        enum sizeelem = T.sizeof;
        auto length = px.length;
        auto newlength = length + n;
        auto newsize = newlength * sizeelem;
        auto size = length * sizeelem;

        if (!gc_expandArrayUsed(px, newsize, isshared))
        {
            // could not set the size, we must reallocate.
            auto newcap = newCapacity(newlength, sizeelem);
            auto attrs = __typeAttrs!T(cast(void*)px.ptr) | BlkAttr.APPENDABLE;

            T* ptr = cast(T*)GC.malloc(newcap, attrs, typeid(T));
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
                    memset(ptr + newlength, 0, newcap - newsize);

                gc_shrinkArrayUsed(ptr[0 .. newlength], newcap, isshared);
            }

            memcpy(ptr, px.ptr, size);

            // do potsblit processing.
            __doPostblit!T(ptr[0 .. length]);

            px = ptr[0 .. newlength];
            return px;
        }

        // we were able to expand in place, just update the length
        px = px.ptr[0 .. newlength];
        return px;
    }
    else
        assert(0, "Cannot append to array if compiling without support for runtime type information!");
}

version (D_ProfileGC)
{
    /**
     * TraceGC wrapper around _d_arrayappendcTX.
     */
    ref Tarr _d_arrayappendcTXTrace(Tarr : T[], T)(return ref scope Tarr px, size_t n,
        string file = __FILE__, int line = __LINE__, string funcname = __FUNCTION__) @trusted
    {
        version (D_TypeInfo)
        {
            import core.internal.array.utils: TraceHook, gcStatsPure, accumulatePure;
            mixin(TraceHook!("Tarr", "_d_arrayappendcTX"));

            return _d_arrayappendcTX(px, n);
        }
        else
            static assert(0, "Cannot append to array if compiling without support for runtime type information!");
    }
}

/// Implementation of `_d_arrayappendT`
ref Tarr _d_arrayappendT(Tarr : T[], T)(return ref scope Tarr x, scope Tarr y) @trusted
{
    version (DigitalMars) pragma(inline, false);

    import core.stdc.string : memcpy;
    import core.internal.traits : Unqual;

    auto length = x.length;

    _d_arrayappendcTX(x, y.length);

    // Only call `copyEmplace` if `T` has a copy ctor and no postblit.
    static if (__traits(hasCopyConstructor, T))
    {
        import core.lifetime : copyEmplace;

        size_t i;
        try
        {
            for (i = 0; i < y.length; ++i)
                copyEmplace(y[i], x[length + i]);
        }
        catch (Exception o)
        {
            /* Destroy, in reverse order, what we've constructed so far
            */
            while (i--)
            {
                auto elem = cast(Unqual!T*) &x[length + i];
                destroy(*elem);
            }

            throw o;
        }
    }
    else
    {
        if (y.length)
        {
            // blit all elements at once
            memcpy(cast(void*)&x[length], cast(void*)&y[0], y.length * T.sizeof);

            // call postblits if they exist
            static if (__traits(hasPostblit, T))
            {
                import core.internal.lifetime : __doPostblit;
                size_t i = 0;
                try __doPostblit(x[length .. $], i);
                catch (Exception o)
                {
                    // Destroy, in reverse order, what we've constructed so far
                    while (i--)
                    {
                        auto elem = cast(Unqual!T*) &x[length + i];
                        destroy(*elem);
                    }

                    throw o;
                }
            }}
    }

    return x;
}

version (D_ProfileGC)
{
    /**
     * TraceGC wrapper around $(REF _d_arrayappendT, core,internal,array,appending).
     */
    ref Tarr _d_arrayappendTTrace(Tarr : T[], T)(return ref scope Tarr x, scope Tarr y, string file = __FILE__, int line = __LINE__, string funcname = __FUNCTION__) @trusted
    {
        version (D_TypeInfo)
        {
            import core.internal.array.utils: TraceHook, gcStatsPure, accumulatePure;
            mixin(TraceHook!("Tarr", "_d_arrayappendT"));

            return _d_arrayappendT(x, y);
        }
        else
            static assert(0, "Cannot append to array if compiling without support for runtime type information!");
    }
}

@safe unittest
{
    double[] arr1;
    foreach (i; 0 .. 4)
        _d_arrayappendT(arr1, [cast(double)i]);
    assert(arr1 == [0.0, 1.0, 2.0, 3.0]);
}

@safe unittest
{
    int blitted;
    struct Item
    {
        this(this)
        {
            blitted++;
        }
    }

    Item[] arr1 = [Item(), Item()];
    Item[] arr2 = [Item(), Item()];
    Item[] arr1_org = [Item(), Item()];
    arr1_org ~= arr2;
    _d_arrayappendT(arr1, arr2);

    // postblit should have triggered on at least the items in arr2
    assert(blitted >= arr2.length);
}

@safe nothrow unittest
{
    int blitted;
    struct Item
    {
        this(this) nothrow
        {
            blitted++;
        }
    }

    Item[][] arr1 = [[Item()]];
    Item[][] arr2 = [[Item()]];

    _d_arrayappendT(arr1, arr2);

    // no postblit should have happened because arr{1,2} contain dynamic arrays
    assert(blitted == 0);
}

@safe nothrow unittest
{
    int copied;
    struct Item
    {
        this(const scope ref Item) nothrow
        {
            copied++;
        }
    }

    Item[1][] arr1 = [[Item()]];
    Item[1][] arr2 = [[Item()]];

    _d_arrayappendT(arr1, arr2);
    // copy constructor should have been invoked because arr{1,2} contain static arrays
    assert(copied >= arr2.length);
}

@safe nothrow unittest
{
    string str;
    _d_arrayappendT(str, "a");
    _d_arrayappendT(str, "b");
    _d_arrayappendT(str, "c");
    assert(str == "abc");
}

@safe nothrow unittest
{
    static class FailedPostblitException : Exception { this() nothrow @safe { super(null); } }
    static size_t inner_postblit_cnt = 0;
    static size_t inner_dtor_cnt = 0;
    static size_t outer_postblit_cnt = 0;
    static size_t outer_dtor_cnt = 0;
    static struct Inner
    {
        char id;

        @safe:
        this(this)
        {
            ++inner_postblit_cnt;
            if (id == '2')
                throw new FailedPostblitException();
        }

        ~this() nothrow
        {
            ++inner_dtor_cnt;
        }
    }

    static struct Outer
    {
        Inner inner1, inner2, inner3;

        nothrow @safe:
        this(char first, char second, char third)
        {
            inner1 = Inner(first);
            inner2 = Inner(second);
            inner3 = Inner(third);
        }

        this(this)
        {
            ++outer_postblit_cnt;
        }

        ~this()
        {
            ++outer_dtor_cnt;
        }
    }

    Outer[3] arr = [Outer('1', '1', '1'), Outer('1', '2', '3'), Outer('3', '3', '3')];

    try {
        Outer[] arrApp;
        arrApp ~= arr;
    }
    catch (FailedPostblitException) {}
    catch (Exception) assert(false);

    assert(inner_postblit_cnt == 5);
    assert(inner_dtor_cnt == 4);
    assert(outer_postblit_cnt == 1);
    assert(outer_dtor_cnt == 1);
}
