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

/**
 * Does array initialization (not assignment) from another array of the same element type.
 * Params:
 *  from = what data the array should be initialized with
 * Returns:
 *  The created and initialized array `to`
 * Bugs:
 *  This function template was ported from a much older runtime hook that bypassed safety,
 *  purity, and throwabilty checks. To prevent breaking existing code, this function template
 *  is temporarily declared `@trusted` until the implementation can be brought up to modern D expectations.
 */
Tarr1 _d_arrayctor(Tarr1 : T1[], Tarr2 : T2[], T1, T2)(scope Tarr2 from) @trusted
    if (is(Unqual!T1 == Unqual!T2))
{
    pragma(inline, false);
    import core.internal.traits : hasElaborateCopyConstructor;
    import core.lifetime : copyEmplace;
    import core.stdc.string : memcpy;
    import core.stdc.stdint : uintptr_t;
    debug(PRINTF) import core.stdc.stdio : printf;

    debug(PRINTF) printf("_d_arrayctor(to = %p,%d, from = %p,%d) size = %d\n", from.ptr, from.length, to.ptr, to.length, T1.tsize);

    Tarr1 to = void;

    void[] vFrom = (cast(void*)from.ptr)[0..from.length];
    void[] vTo = (cast(void*)to.ptr)[0..to.length];

    // Force `enforceRawArraysConformable` to be `pure`
    void enforceRawArraysConformable(const char[] action, const size_t elementSize,
        const void[] a1, const void[] a2) @trusted
    {
        import core.internal.util.array : enforceRawArraysConformableNogc;

        alias Type = void function(const char[] action, const size_t elementSize,
            const void[] a1, const void[] a2, in bool allowOverlap = false) @nogc pure nothrow;
        (cast(Type)&enforceRawArraysConformableNogc)(action, elementSize, a1, a2, false);
    }

    enforceRawArraysConformable("initialization", T1.sizeof, vFrom, vTo);

    static if (hasElaborateCopyConstructor!T1)
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
                auto elem = cast(Unqual!T1*)&to[i];
                destroy(*elem);
            }

            throw o;
        }
    }
    else
    {
        // blit all elements at once
        memcpy(cast(void*) to.ptr, from.ptr, to.length * T1.sizeof);
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
    arr1 = _d_arrayctor!(typeof(arr1))(arr2[]);

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
    arr1 = _d_arrayctor!(typeof(arr1))(arr2[]);

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
        a = _d_arrayctor!(typeof(a))(b[]);
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
        a = _d_arrayctor!(typeof(a))(b[]);
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
    pragma(inline, false);
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
        a = _d_arrayctor!(typeof(a))(b[]);
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
