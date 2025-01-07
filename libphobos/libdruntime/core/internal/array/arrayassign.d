module core.internal.array.arrayassign;

// Force `enforceRawArraysConformable` to remain `pure` `@nogc`
private void enforceRawArraysConformable(const char[] action, const size_t elementSize,
    const void[] a1, const void[] a2, const bool allowOverlap) @trusted @nogc pure nothrow
{
    import core.internal.util.array : enforceRawArraysConformable;

    alias Type = void function(const char[] action, const size_t elementSize,
        const void[] a1, const void[] a2, in bool allowOverlap = false) @nogc pure nothrow;
    (cast(Type)&enforceRawArraysConformable)(action, elementSize, a1, a2, allowOverlap);
}

private template CopyElem(string CopyAction)
{
    const char[] CopyElem = "{\n" ~ q{
            memcpy(&tmp, cast(void*) &dst, elemSize);
            } ~ CopyAction ~ q{
            auto elem = cast(Unqual!T*) &tmp;
            destroy(*elem);
        } ~ "}\n";
}

private template CopyArray(bool CanOverlap, string CopyAction)
{
    const char[] CopyArray = CanOverlap ? q{
        if (vFrom.ptr < vTo.ptr && vTo.ptr < vFrom.ptr + elemSize * vFrom.length)
            foreach_reverse (i, ref dst; to)
            } ~ CopyElem!(CopyAction) ~ q{
        else
            foreach (i, ref dst; to)
            } ~ CopyElem!(CopyAction)
        : q{
            foreach (i, ref dst; to)
            } ~ CopyElem!(CopyAction);
}

private template ArrayAssign(string CopyLogic, string AllowOverLap)
{
    const char[] ArrayAssign = q{
        import core.internal.traits : hasElaborateCopyConstructor, Unqual;
        import core.lifetime : copyEmplace;
        import core.stdc.string : memcpy;

        void[] vFrom = (cast(void*) from.ptr)[0 .. from.length];
        void[] vTo = (cast(void*) to.ptr)[0 .. to.length];
        enum elemSize = T.sizeof;

        enforceRawArraysConformable("copy", elemSize, vFrom, vTo, } ~ AllowOverLap ~ q{);

        void[elemSize] tmp = void;

        } ~ CopyLogic ~ q{

        return to;
    };
}

/**
 * Does array assignment (not construction) from another array of the same
 * element type. Handles overlapping copies. Assumes the right hand side is an
 * lvalue,
 *
 * Used for static array assignment with non-POD element types:
 * ---
 * struct S
 * {
 *     ~this() {} // destructor, so not Plain Old Data
 * }
 *
 * void main()
 * {
 *   S[3] arr;
 *   S[3] lvalue;
 *
 *   arr = lvalue;
 *   // Generates:
 *   // _d_arrayassign_l(arr[], lvalue[]), arr;
 * }
 * ---
 *
 * Params:
 *     to = destination array
 *     from = source array
 * Returns:
 *     `to`
 */
Tarr _d_arrayassign_l(Tarr : T[], T)(return scope Tarr to, scope Tarr from) @trusted
{
    mixin(ArrayAssign!(q{
        static if (hasElaborateCopyConstructor!T)
            } ~ CopyArray!(true, "copyEmplace(from[i], dst);") ~ q{
        else
            } ~ CopyArray!(true, "memcpy(cast(void*) &dst, cast(void*) &from[i], elemSize);"),
        "true"));
}

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
    _d_arrayassign_l(arr1[], arr2[]);

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
    _d_arrayassign_l(arr1[], arr2[]);

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
        _d_arrayassign_l(a[], b[]);
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
        _d_arrayassign_l(a[], b[]);
    }
    catch (Exception)
    {
        didThrow = false;
    }
    assert(!didThrow);
    assert(counter == 4);
}

/**
 * Does array assignment (not construction) from another array of the same
 * element type. Does not support overlapping copies. Assumes the right hand
 * side is an rvalue,
 *
 * Used for static array assignment with non-POD element types:
 * ---
 * struct S
 * {
 *     ~this() {} // destructor, so not Plain Old Data
 * }
 *
 * void main()
 * {
 *   S[3] arr;
 *   S[3] getRvalue() {return lvalue;}
 *
 *   arr = getRvalue();
 *   // Generates:
 *   // (__appendtmp = getRvalue), _d_arrayassign_l(arr[], __appendtmp), arr;
 * }
 * ---
 *
 * Params:
 *     to = destination array
 *     from = source array
 * Returns:
 *     `to`
 */
Tarr _d_arrayassign_r(Tarr : T[], T)(return scope Tarr to, scope Tarr from) @trusted
{
    mixin(ArrayAssign!(
        CopyArray!(false, "memcpy(cast(void*) &dst, cast(void*) &from[i], elemSize);"),
        "false"));
}

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
    _d_arrayassign_r(arr1[], arr2[]);

    assert(counter == 0);
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
    _d_arrayassign_r(arr1[], arr2[]);

    assert(counter == 0);
    assert(arr1 == arr2);
}

@safe nothrow unittest
{
    // Test that `nothrow` works
    bool didThrow = false;
    int counter = 0;
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
        _d_arrayassign_r(a[], b[]);
    }
    catch (Exception)
    {
        didThrow = false;
    }
    assert(!didThrow);
    assert(counter == 0);
}

/**
 * Sets all elements of an array to a single value. Takes into account postblits,
 * copy constructors and destructors. For Plain Old Data elements,`rt/memset.d`
 * is used.
 *
 * ---
 * struct S
 * {
 *     ~this() {} // destructor, so not Plain Old Data
 * }
 *
 * void main()
 * {
 *   S[3] arr;
 *   S value;
 *
 *   arr = value;
 *   // Generates:
 *   // _d_arraysetassign(arr[], value), arr;
 * }
 * ---
 *
 * Params:
 *     to = destination array
 *     value = the element to set
 * Returns:
 *     `to`
 */
Tarr _d_arraysetassign(Tarr : T[], T)(return scope Tarr to, scope ref T value) @trusted
{
    import core.internal.traits : Unqual;
    import core.lifetime : copyEmplace;
    import core.stdc.string : memcpy;

    enum elemSize = T.sizeof;
    void[elemSize] tmp = void;

    foreach (ref dst; to)
    {
        memcpy(&tmp, cast(void*) &dst, elemSize);
        // Use `memcpy` if `T` has a `@disable`d postblit.
        static if (__traits(isCopyable, T))
            copyEmplace(value, dst);
        else
            memcpy(cast(void*) &dst, cast(void*) &value, elemSize);
        auto elem = cast(Unqual!T*) &tmp;
        destroy(*elem);
    }

    return to;
}

// postblit and destructor
@safe unittest
{
    string ops;
    struct S
    {
        int val;
        this(this) { ops ~= "="; }
        ~this() { ops ~= "~"; }
    }

    S[4] arr;
    S s = S(1234);
    _d_arraysetassign(arr[], s);
    assert(ops == "=~=~=~=~");
    assert(arr == [S(1234), S(1234), S(1234), S(1234)]);
}

// copy constructor
@safe unittest
{
    string ops;
    struct S
    {
        int val;
        this(const scope ref S rhs)
        {
            val = rhs.val;
            ops ~= "=";
        }
        ~this() { ops ~= "~"; }
    }

    S[4] arr;
    S s = S(1234);
    _d_arraysetassign(arr[], s);
    assert(ops == "=~=~=~=~");
    assert(arr == [S(1234), S(1234), S(1234), S(1234)]);
}

// disabled copy constructor
@safe unittest
{
    static struct S
    {
        int val;
        @disable this(ref S);
    }
    S[1] arr;
    S s = S(1234);
    _d_arraysetassign(arr[], s);
    assert(arr[0].val == 1234);
}

// throwing and `nothrow`
@safe nothrow unittest
{
    // Test that throwing works
    bool didThrow;
    int counter;
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
        Throw b = Throw(1);
        _d_arraysetassign(a[], b);
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
        this(this) { counter++; }
    }

    try
    {
        NoThrow[4] a;
        NoThrow b = NoThrow(1);
        _d_arraysetassign(a[], b);
        foreach (ref e; a)
            assert(e == NoThrow(1));
    }
    catch (Exception)
    {
        didThrow = true;
    }
    assert(!didThrow);
    // The array `a` is destroyed when the `try` block ends.
    assert(counter == 4);
}
