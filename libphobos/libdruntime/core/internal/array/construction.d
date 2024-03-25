/**
 This module contains compiler support for constructing dynamic arrays

  Copyright: Copyright Digital Mars 2000 - 2019.
  License: Distributed under the
       $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
     (See accompanying file LICENSE)
  Source: $(DRUNTIMESRC core/internal/_array/_construction.d)
*/
module core.internal.array.construction;

import core.internal.traits : Unqual;

debug(PRINTF)
{
    import core.stdc.stdio;
}

/**
 * Does array initialization (not assignment) from another array of the same element type.
 * Params:
 *  to = what array to initialize
 *  from = what data the array should be initialized with
 *  makeWeaklyPure = unused; its purpose is to prevent the function from becoming
 *      strongly pure and risk being optimised out
 * Returns:
 *  The created and initialized array `to`
 * Bugs:
 *  This function template was ported from a much older runtime hook that bypassed safety,
 *  purity, and throwabilty checks. To prevent breaking existing code, this function template
 *  is temporarily declared `@trusted` until the implementation can be brought up to modern D expectations.
 *
 *  The third parameter is never used, but is necessary in order for the
 *  function be treated as weakly pure, instead of strongly pure.
 *  This is needed because constructions such as the one below can be ignored by
 *  the compiler if `_d_arrayctor` is believed to be pure, because purity would
 *  mean the call to `_d_arrayctor` has no effects (no side effects and the
 *  return value is ignored), despite it actually modifying the contents of `a`.
 *      const S[2] b;
 *      const S[2] a = b;  // this would get lowered to _d_arrayctor(a, b)
 */
Tarr _d_arrayctor(Tarr : T[], T)(return scope Tarr to, scope Tarr from, char* makeWeaklyPure = null) @trusted
{
    version (DigitalMars) pragma(inline, false);
    import core.internal.traits : hasElaborateCopyConstructor;
    import core.lifetime : copyEmplace;
    import core.stdc.string : memcpy;
    import core.stdc.stdint : uintptr_t;
    debug(PRINTF) import core.stdc.stdio : printf;

    debug(PRINTF) printf("_d_arrayctor(from = %p,%d) size = %d\n", from.ptr, from.length, T.sizeof);

    void[] vFrom = (cast(void*) from.ptr)[0..from.length];
    void[] vTo = (cast(void*) to.ptr)[0..to.length];

    // Force `enforceRawArraysConformable` to remain weakly `pure`
    void enforceRawArraysConformable(const char[] action, const size_t elementSize,
        const void[] a1, const void[] a2) @trusted
    {
        import core.internal.util.array : enforceRawArraysConformableNogc;

        alias Type = void function(const char[] action, const size_t elementSize,
            const void[] a1, const void[] a2, in bool allowOverlap = false) @nogc pure nothrow;
        (cast(Type)&enforceRawArraysConformableNogc)(action, elementSize, a1, a2, false);
    }

    enforceRawArraysConformable("initialization", T.sizeof, vFrom, vTo);

    static if (hasElaborateCopyConstructor!T)
    {
        size_t i;
        try
        {
            for (i = 0; i < to.length; i++)
                copyEmplace(from[i], to[i]);
        }
        catch (Exception o)
        {
            /* Destroy, in reverse order, what we've constructed so far
            */
            while (i--)
            {
                auto elem = cast(Unqual!T*) &to[i];
                destroy(*elem);
            }

            throw o;
        }
    }
    else
    {
        // blit all elements at once
        memcpy(cast(void*) to.ptr, from.ptr, to.length * T.sizeof);
    }

    return to;
}

// postblit
@safe unittest
{
    int counter;
    struct S
    {
        int val;
        this(this) { counter++; }
    }

    S[4] arr1;
    S[4] arr2 = [S(0), S(1), S(2), S(3)];
    _d_arrayctor(arr1[], arr2[]);

    assert(counter == 4);
    assert(arr1 == arr2);
}

// copy constructor
@safe unittest
{
    int counter;
    struct S
    {
        int val;
        this(int val) { this.val = val; }
        this(const scope ref S rhs)
        {
            val = rhs.val;
            counter++;
        }
    }

    S[4] arr1;
    S[4] arr2 = [S(0), S(1), S(2), S(3)];
    _d_arrayctor(arr1[], arr2[]);

    assert(counter == 4);
    assert(arr1 == arr2);
}

@safe nothrow unittest
{
    // Test that throwing works
    int counter;
    bool didThrow;

    struct Throw
    {
        int val;
        this(this)
        {
            counter++;
            if (counter == 2)
                throw new Exception("");
        }
    }
    try
    {
        Throw[4] a;
        Throw[4] b = [Throw(1), Throw(2), Throw(3), Throw(4)];
        _d_arrayctor(a[], b[]);
    }
    catch (Exception)
    {
        didThrow = true;
    }
    assert(didThrow);
    assert(counter == 2);


    // Test that `nothrow` works
    didThrow = false;
    counter = 0;
    struct NoThrow
    {
        int val;
        this(this)
        {
            counter++;
        }
    }
    try
    {
        NoThrow[4] a;
        NoThrow[4] b = [NoThrow(1), NoThrow(2), NoThrow(3), NoThrow(4)];
        _d_arrayctor(a[], b[]);
    }
    catch (Exception)
    {
        didThrow = false;
    }
    assert(!didThrow);
    assert(counter == 4);
}

/**
 * Do construction of an array.
 *      ti[count] p = value;
 * Params:
 *  p = what array to initialize
 *  value = what data to construct the array with
 * Bugs:
 *  This function template was ported from a much older runtime hook that bypassed safety,
 *  purity, and throwabilty checks. To prevent breaking existing code, this function template
 *  is temporarily declared `@trusted` until the implementation can be brought up to modern D expectations.
 */
void _d_arraysetctor(Tarr : T[], T)(scope Tarr p, scope ref T value) @trusted
{
    version (DigitalMars) pragma(inline, false);
    import core.lifetime : copyEmplace;

    size_t i;
    try
    {
        for (i = 0; i < p.length; i++)
            copyEmplace(value, p[i]);
    }
    catch (Exception o)
    {
        // Destroy, in reverse order, what we've constructed so far
        while (i--)
        {
            auto elem = cast(Unqual!T*)&p[i];
            destroy(*elem);
        }

        throw o;
    }
}

// postblit
@safe unittest
{
    int counter;
    struct S
    {
        int val;
        this(this)
        {
            counter++;
        }
    }

    S[4] arr;
    S s = S(1234);
    _d_arraysetctor(arr[], s);
    assert(counter == arr.length);
    assert(arr == [S(1234), S(1234), S(1234), S(1234)]);
}

// copy constructor
@safe unittest
{
    int counter;
    struct S
    {
        int val;
        this(int val) { this.val = val; }
        this(const scope ref S rhs)
        {
            val = rhs.val;
            counter++;
        }
    }

    S[4] arr;
    S s = S(1234);
    _d_arraysetctor(arr[], s);
    assert(counter == arr.length);
    assert(arr == [S(1234), S(1234), S(1234), S(1234)]);
}

@safe nothrow unittest
{
    // Test that throwing works
    int counter;
    bool didThrow;
    struct Throw
    {
        int val;
        this(this)
        {
            counter++;
            if (counter == 2)
                throw new Exception("Oh no.");
        }
    }
    try
    {
        Throw[4] a;
        Throw[4] b = [Throw(1), Throw(2), Throw(3), Throw(4)];
        _d_arrayctor(a[], b[]);
    }
    catch (Exception)
    {
        didThrow = true;
    }
    assert(didThrow);
    assert(counter == 2);


    // Test that `nothrow` works
    didThrow = false;
    counter = 0;
    struct NoThrow
    {
        int val;
        this(this)
        {
            counter++;
        }
    }
    try
    {
        NoThrow[4] a;
        NoThrow b = NoThrow(1);
        _d_arraysetctor(a[], b);
        foreach (ref e; a)
            assert(e == NoThrow(1));
    }
    catch (Exception)
    {
        didThrow = false;
    }
    assert(!didThrow);
    assert(counter == 4);
}

/**
 * Allocate an array with the garbage collector. Also initalize elements if
 * their type has an initializer. Otherwise, not zero-initialize the array.
 *
 * Has three variants:
 *      `_d_newarrayU` leaves elements uninitialized
 *      `_d_newarrayT` initializes to 0 or based on initializer
 *
 * Params:
 *      length = `.length` of resulting array
 *
 * Returns:
 *      newly allocated array
 */
T[] _d_newarrayU(T)(size_t length, bool isShared=false) pure nothrow @nogc @trusted
{
    alias PureType = T[] function(size_t length, bool isShared) pure nothrow @nogc @trusted;
    return (cast(PureType) &_d_newarrayUImpl!T)(length, isShared);
}

T[] _d_newarrayUImpl(T)(size_t length, bool isShared=false) @trusted
{
    import core.exception : onOutOfMemoryError;
    import core.internal.array.utils : __arrayStart, __setArrayAllocLength, __arrayAlloc;

    size_t elemSize = T.sizeof;
    size_t arraySize;

    debug(PRINTF) printf("_d_newarrayU(length = x%zu, size = %zu)\n", length, elemSize);
    if (length == 0 || elemSize == 0)
        return null;

    version (D_InlineAsm_X86)
    {
        asm pure nothrow @nogc
        {
            mov     EAX, elemSize       ;
            mul     EAX, length         ;
            mov     arraySize, EAX      ;
            jnc     Lcontinue           ;
        }
    }
    else version (D_InlineAsm_X86_64)
    {
        asm pure nothrow @nogc
        {
            mov     RAX, elemSize       ;
            mul     RAX, length         ;
            mov     arraySize, RAX      ;
            jnc     Lcontinue           ;
        }
    }
    else
    {
        import core.checkedint : mulu;

        bool overflow = false;
        arraySize = mulu(elemSize, length, overflow);
        if (!overflow)
            goto Lcontinue;
    }

Loverflow:
    onOutOfMemoryError();
    assert(0);

Lcontinue:
    auto info = __arrayAlloc!T(arraySize);
    if (!info.base)
        goto Loverflow;
    debug(PRINTF) printf("p = %p\n", info.base);

    auto arrstart = __arrayStart(info);

    __setArrayAllocLength!T(info, arraySize, isShared);

    return (cast(T*) arrstart)[0 .. length];
}

/// ditto
T[] _d_newarrayT(T)(size_t length, bool isShared=false) @trusted
{
    T[] result = _d_newarrayU!T(length, isShared);

    static if (__traits(isZeroInit, T))
    {
        import core.stdc.string : memset;
        memset(result.ptr, 0, length * T.sizeof);
    }
    else
    {
        import core.internal.lifetime : emplaceInitializer;
        foreach (ref elem; result)
            emplaceInitializer(elem);
    }

    return result;
}

unittest
{
    {
        // zero-initialization
        struct S { int x, y; }
        S[] s = _d_newarrayT!S(10);

        assert(s !is null);
        assert(s.length == 10);
        foreach (ref elem; s)
        {
            assert(elem.x == 0);
            assert(elem.y == 0);
        }
    }
    {
        // S.init
        struct S { int x = 2, y = 3; }
        S[] s = _d_newarrayT!S(10);

        assert(s.length == 10);
        foreach (ref elem; s)
        {
            assert(elem.x == 2);
            assert(elem.y == 3);
        }
    }
}

unittest
{
    int pblits;

    struct S
    {
        this(this) { pblits++; }
    }

    S[] s = _d_newarrayT!S(2);

    assert(s.length == 2);
    assert(pblits == 0);
}

version (D_ProfileGC)
{
    /**
    * TraceGC wrapper around $(REF _d_newitemT, core,lifetime).
    */
    T[] _d_newarrayTTrace(T)(string file, int line, string funcname, size_t length, bool isShared) @trusted
    {
        version (D_TypeInfo)
        {
            import core.internal.array.utils : TraceHook, gcStatsPure, accumulatePure;
            mixin(TraceHook!(T.stringof, "_d_newarrayT"));

            return _d_newarrayT!T(length, isShared);
        }
        else
            assert(0, "Cannot create new array if compiling without support for runtime type information!");
    }
}

/**
 * Create a new multi-dimensional array. Also initalize elements if their type has an initializer.
 * Otherwise, not zero-initialize the array.
 *
 * ---
 * void main()
 * {
 *     S[][] s = new S[][](2, 3)
 *
 *     // lowering:
 *     S[] s = _d_newarraymTX!(S[][], S)([2, 3]);
 * }
 * ---
 *
 * Params:
 *    dims = array length values for each dimension
 *    isShared = whether the array should be shared
 *
 * Returns:
 *    newly allocated array
 */
Tarr _d_newarraymTX(Tarr : U[], T, U)(size_t[] dims, bool isShared=false) @trusted
{
    debug(PRINTF) printf("_d_newarraymTX(dims.length = %d)\n", dims.length);

    if (dims.length == 0)
        return null;

    alias UnqT = Unqual!(T);

    void[] __allocateInnerArray(size_t[] dims)
    {
        import core.internal.array.utils : __arrayStart, __setArrayAllocLength, __arrayAlloc;

        auto dim = dims[0];

        debug(PRINTF) printf("__allocateInnerArray(ti = %p, ti.next = %p, dim = %d, ndims = %d\n", ti, ti.next, dim, dims.length);
        if (dims.length == 1)
        {
            auto r = _d_newarrayT!UnqT(dim, isShared);
            return *cast(void[]*)(&r);
        }

        auto allocSize = (void[]).sizeof * dim;
        auto info = __arrayAlloc!UnqT(allocSize);
        __setArrayAllocLength!UnqT(info, allocSize, isShared);
        auto p = __arrayStart(info)[0 .. dim];

        foreach (i; 0..dim)
        {
            (cast(void[]*)p.ptr)[i] = __allocateInnerArray(dims[1..$]);
        }
        return p;
    }

    auto result = __allocateInnerArray(dims);
    debug(PRINTF) printf("result = %llx\n", result.ptr);

    return (cast(U*) result.ptr)[0 .. dims[0]];
}

unittest
{
    int[][] a = _d_newarraymTX!(int[][], int)([2, 3]);

    assert(a.length == 2);
    for (size_t i = 0; i < a.length; i++)
    {
        assert(a[i].length == 3);
        for (size_t j = 0; j < a[i].length; j++)
            assert(a[i][j] == 0);
    }
}

unittest
{
    struct S { int x = 1; }

    S[][] a = _d_newarraymTX!(S[][], S)([2, 3]);

    assert(a.length == 2);
    for (size_t i = 0; i < a.length; i++)
    {
        assert(a[i].length == 3);
        for (size_t j = 0; j < a[i].length; j++)
            assert(a[i][j].x == 1);
    }
}

version (D_ProfileGC)
{
    /**
    * TraceGC wrapper around $(REF _d_newarraymT, core,internal,array,construction).
    */
    Tarr _d_newarraymTXTrace(Tarr : U[], T, U)(string file, int line, string funcname, size_t[] dims, bool isShared=false) @trusted
    {
        version (D_TypeInfo)
        {
            import core.internal.array.utils : TraceHook, gcStatsPure, accumulatePure;
            mixin(TraceHook!(T.stringof, "_d_newarraymTX"));

            return _d_newarraymTX!(Tarr, T)(dims, isShared);
        }
        else
            assert(0, "Cannot create new multi-dimensional array if compiling without support for runtime type information!");
    }
}
