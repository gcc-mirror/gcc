/**
 This module contains support for controlling dynamic arrays' appending

  Copyright: Copyright Digital Mars 2000 - 2019.
  License: Distributed under the
       $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
     (See accompanying file LICENSE)
  Source: $(DRUNTIMESRC core/_internal/_array/_appending.d)
*/
module core.internal.array.appending;

/// See $(REF _d_arrayappendcTX, rt,lifetime,_d_arrayappendcTX)
private extern (C) byte[] _d_arrayappendcTX(const TypeInfo ti, ref return scope byte[] px, size_t n) @trusted pure nothrow;

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
 *  is temporarily declared `@trusted pure` until the implementation can be brought up to modern D expectations.
 */
ref Tarr _d_arrayappendcTX(Tarr : T[], T)(return ref scope Tarr px, size_t n) @trusted
{
    // needed for CTFE: https://github.com/dlang/druntime/pull/3870#issuecomment-1178800718
    version (DigitalMars) pragma(inline, false);
    version (D_TypeInfo)
    {
        auto ti = typeid(Tarr);

        // _d_arrayappendcTX takes the `px` as a ref byte[], but its length
        // should still be the original length
        auto pxx = (cast(byte*)px.ptr)[0 .. px.length];
        ._d_arrayappendcTX(ti, pxx, n);
        px = (cast(T*)pxx.ptr)[0 .. pxx.length];

        return px;
    }
    else
        assert(0, "Cannot append to array if compiling without support for runtime type information!");
}

version (D_ProfileGC)
{
    /**
     * TraceGC wrapper around $(REF _d_arrayappendT, core,internal,array,appending).
     */
    ref Tarr _d_arrayappendcTXTrace(Tarr : T[], T)(return ref scope Tarr px, size_t n, string file = __FILE__, int line = __LINE__, string funcname = __FUNCTION__) @trusted
    {
        version (D_TypeInfo)
        {
            import core.internal.array.utils: TraceHook, gcStatsPure, accumulatePure;
            mixin(TraceHook!(Tarr.stringof, "_d_arrayappendcTX"));

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
    import core.internal.traits : hasElaborateCopyConstructor, Unqual;

    enum hasPostblit = __traits(hasPostblit, T);
    auto length = x.length;

    _d_arrayappendcTX(x, y.length);

    // Only call `copyEmplace` if `T` has a copy ctor and no postblit.
    static if (hasElaborateCopyConstructor!T && !hasPostblit)
    {
        import core.lifetime : copyEmplace;

        foreach (i, ref elem; y)
            copyEmplace(elem, x[length + i]);
    }
    else
    {
        if (y.length)
        {
            // blit all elements at once
            auto xptr = cast(Unqual!T *)&x[length];
            immutable size = T.sizeof;

            memcpy(xptr, cast(Unqual!T *)&y[0], y.length * size);

            // call postblits if they exist
            static if (hasPostblit)
            {
                auto eptr = xptr + y.length;
                for (auto ptr = xptr; ptr < eptr; ptr++)
                    ptr.__xpostblit();
            }
        }
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
            mixin(TraceHook!(Tarr.stringof, "_d_arrayappendT"));

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
