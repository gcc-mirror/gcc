/**
 This module contains compiler support for constructing dynamic arrays

  Copyright: Copyright Digital Mars 2000 - 2019.
  License: Distributed under the
       $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost Software License 1.0).
     (See accompanying file LICENSE)
  Source: $(DRUNTIMESRC core/internal/_array/_construction.d)
*/
module core.internal.array.construction;

/**
 * Does array initialization (not assignment) from another array of the same element type.
 * Params:
 *  to = what array to initialize
 *  from = what data the array should be initialized with
 * Returns:
 *  The constructed `to`
 * Bugs:
 *  This function template was ported from a much older runtime hook that bypassed safety,
 *  purity, and throwabilty checks. To prevent breaking existing code, this function template
 *  is temporarily declared `@trusted` until the implementation can be brought up to modern D expectations.
 */
Tarr _d_arrayctor(Tarr : T[], T)(return scope Tarr to, scope Tarr from) @trusted
{
    pragma(inline, false);
    import core.internal.traits : hasElaborateCopyConstructor, Unqual;
    import core.lifetime : copyEmplace;
    import core.stdc.string : memcpy;
    debug(PRINTF) import core.stdc.stdio;

    // Force `enforceRawArraysConformable` to be `pure`
    void enforceRawArraysConformable(const char[] action, const size_t elementSize, const void[] a1, const void[] a2, in bool allowOverlap = false) @trusted
    {
        import core.internal.util.array : enforceRawArraysConformable;

        alias Type = void function(const char[] action, const size_t elementSize, const void[] a1, const void[] a2, in bool allowOverlap = false) pure nothrow;
        (cast(Type)&enforceRawArraysConformable)(action, elementSize, a1, a2, allowOverlap);
    }

    debug(PRINTF) printf("_d_arrayctor(to = %p,%d, from = %p,%d) size = %d\n", from.ptr, from.length, to.ptr, to.length, T.tsize);

    auto element_size = T.sizeof;

    void[] vFrom = (cast(void*)from.ptr)[0..from.length];
    void[] vTo = (cast(void*)to.ptr)[0..to.length];
    enforceRawArraysConformable("initialization", element_size, vFrom, vTo, false);

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
                auto elem = cast(Unqual!T*)&to[i];
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
    pragma(inline, false);
    import core.internal.traits : Unqual;
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
