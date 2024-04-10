/*
 * Support for rich error messages generation with `assert`
 *
 * This module provides the `_d_assert_fail` hooks which are instantiated
 * by the compiler whenever `-checkaction=context` is used.
 * There are two hooks, one for unary expressions, and one for binary.
 * When used, the compiler will rewrite `assert(a >= b)` as
 * `assert(a >= b, _d_assert_fail!(typeof(a))(">=", a, b))`.
 * Temporaries will be created to avoid side effects if deemed necessary
 * by the compiler.
 *
 * For more information, refer to the implementation in DMD frontend
 * for `AssertExpression`'s semantic analysis.
 *
 * Copyright: D Language Foundation 2018 - 2020
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(LINK2 https://github.com/dlang/dmd/blob/master/druntime/src/core/internal/dassert.d, _dassert.d)
 * Documentation: https://dlang.org/phobos/core_internal_dassert.html
 */
module core.internal.dassert;

/**
 * Generates rich assert error messages for unary expressions
 *
 * The unary expression `assert(!una)` will be turned into
 * `assert(!una, _d_assert_fail("!", una))`.
 * This routine simply acts as if the user wrote `assert(una == false)`.
 *
 * Params:
 *   op = Operator that was used in the expression, currently only "!"
 *        is supported.
 *   a  = Result of the expression that was used in `assert` before
 *        its implicit conversion to `bool`.
 *
 * Returns:
 *   A string such as "$a != true" or "$a == true".
 */
string _d_assert_fail(A)(const scope string op, auto ref const scope A a)
{
    // Prevent InvalidMemoryOperationError when triggered from a finalizer
    if (inFinalizer())
        return "Assertion failed (rich formatting is disabled in finalizers)";

    string[2] vals = [ miniFormatFakeAttributes(a), "true" ];
    immutable token = op == "!" ? "==" : "!=";
    return combine(vals[0 .. 1], token, vals[1 .. $]);
}

/**
 * Generates rich assert error messages for binary expressions
 *
 * The binary expression `assert(x == y)` will be turned into
 * `assert(x == y, _d_assert_fail!(typeof(x))("==", x, y))`.
 *
 * Params:
 *   comp = Comparison operator that was used in the expression.
 *   a  = Left hand side operand (can be a tuple).
 *   b  = Right hand side operand (can be a tuple).
 *
 * Returns:
 *   A string such as "$a $comp $b".
 */
template _d_assert_fail(A...)
{
    string _d_assert_fail(B...)(
        const scope string comp, auto ref const scope A a, auto ref const scope B b)
    if (B.length != 0 || A.length != 1) // Resolve ambiguity with unary overload
    {
        // Prevent InvalidMemoryOperationError when triggered from a finalizer
        if (inFinalizer())
            return "Assertion failed (rich formatting is disabled in finalizers)";

        string[A.length + B.length] vals;
        static foreach (idx; 0 .. A.length)
            vals[idx] = miniFormatFakeAttributes(a[idx]);
        static foreach (idx; 0 .. B.length)
            vals[A.length + idx] = miniFormatFakeAttributes(b[idx]);
        immutable token = invertCompToken(comp);
        return combine(vals[0 .. A.length], token, vals[A.length .. $]);
    }
}

/// Combines the supplied arguments into one string `"valA token valB"`
private string combine(const scope string[] valA, const scope string token,
    const scope string[] valB) pure nothrow @nogc @safe
{
    // Each separator is 2 chars (", "), plus the two spaces around the token.
    size_t totalLen = (valA.length - 1) * 2 +
        (valB.length - 1) * 2 + 2 + token.length;

    // Empty arrays are printed as ()
    if (valA.length == 0) totalLen += 2;
    if (valB.length == 0) totalLen += 2;

    foreach (v; valA) totalLen += v.length;
    foreach (v; valB) totalLen += v.length;

    // Include braces when printing tuples
    const printBraces = (valA.length + valB.length) != 2;
    if (printBraces) totalLen += 4; // '(', ')' for both tuples

    char[] buffer = cast(char[]) pureAlloc(totalLen)[0 .. totalLen];
    // @nogc-concat of "<valA> <comp> <valB>"
    static void formatTuple (scope char[] buffer, ref size_t n, in string[] vals, in bool printBraces)
    {
        if (printBraces) buffer[n++] = '(';
        foreach (idx, v; vals)
        {
            if (idx)
            {
                buffer[n++] = ',';
                buffer[n++] = ' ';
            }
            buffer[n .. n + v.length] = v;
            n += v.length;
        }
        if (printBraces) buffer[n++] = ')';
    }

    size_t n;
    formatTuple(buffer, n, valA, printBraces);
    buffer[n++] = ' ';
    buffer[n .. n + token.length] = token;
    n += token.length;
    buffer[n++] = ' ';
    formatTuple(buffer, n, valB, printBraces);
    return (() @trusted => cast(string) buffer)();
}

/// Yields the appropriate `printf` format token for a type `T`
private template getPrintfFormat(T)
{
    static if (is(T == long))
    {
        enum getPrintfFormat = "%lld";
    }
    else static if (is(T == ulong))
    {
        enum getPrintfFormat = "%llu";
    }
    else static if (__traits(isIntegral, T))
    {
        static if (__traits(isUnsigned, T))
        {
            enum getPrintfFormat = "%u";
        }
        else
        {
            enum getPrintfFormat = "%d";
        }
    }
    else
    {
        static assert(0, "Unknown format");
    }
}

/**
 * Generates a textual representation of `v` without relying on Phobos.
 * The value is formatted as follows:
 *
 *  - primitive types and arrays yield their respective literals
 *  - pointers are printed as hexadecimal numbers
 *  - enum members are represented by their name
 *  - user-defined types are formatted by either calling `toString`
 *    if defined or printing all members, e.g. `S(1, 2)`
 *
 * Note that unions are rejected because this method cannot determine which
 * member is valid when calling this method.
 *
 * Params:
 *   v = the value to print
 *
 * Returns: a string respresenting `v` or `V.stringof` if `V` is not supported
 */
private string miniFormat(V)(const scope ref V v)
{
    import core.internal.traits: isAggregateType;

    /// `shared` values are formatted as their base type
    static if (is(V == shared T, T))
    {
        import core.atomic : atomicLoad;

        // Use atomics to avoid race conditions whenever possible
        static if (__traits(compiles, atomicLoad(v)))
        {
            if (!__ctfe)
            {
                T tmp = cast(T) atomicLoad(v);
                return miniFormat(tmp);
            }
        }

        // Fall back to a simple cast - we're violating the type system anyways
        return miniFormat(*cast(const T*) &v);
    }
    // Format enum members using their name
    else static if (is(V BaseType == enum))
    {
        // Always generate repeated if's instead of switch to skip the detection
        // of non-integral enums. This method doesn't need to be fast.
        static foreach (mem; __traits(allMembers, V))
        {
            if (v == __traits(getMember, V, mem))
                return mem;
        }

        // Format invalid enum values as their base type
        enum cast_ = "cast(" ~ V.stringof ~ ")";
        const val = miniFormat(__ctfe ? cast(const BaseType) v : *cast(const BaseType*) &v);
        return combine([ cast_ ], "", [ val ]);
    }
    else static if (is(V == bool))
    {
        return v ? "true" : "false";
    }
    // Detect vectors which match isIntegral / isFloating
    else static if (is(V == __vector(ET[N]), ET, size_t N))
    {
        string msg = "[";
        foreach (i; 0 .. N)
        {
            if (i > 0)
                msg ~= ", ";

            msg ~= miniFormat(v[i]);
        }
        msg ~= "]";
        return msg;
    }
    else static if (__traits(isIntegral, V))
    {
        static if (is(V == char))
        {
            // Avoid invalid code points
            if (v < 0x7F)
                return ['\'', v, '\''];

            uint tmp = v;
            return "cast(char) " ~ miniFormat(tmp);
        }
        else static if (is(V == wchar) || is(V == dchar))
        {
            import core.internal.utf: isValidDchar, toUTF8;

            // Avoid invalid code points
            if (isValidDchar(v))
                return toUTF8(['\'', v, '\'']);

            uint tmp = v;
            return "cast(" ~ V.stringof ~ ") " ~ miniFormat(tmp);
        }
        else
        {
            import core.internal.string;
            static if (__traits(isUnsigned, V))
                const val = unsignedToTempString(v);
            else
                const val = signedToTempString(v);
            return val.get().idup();
        }
    }
    else static if (__traits(isFloating, V))
    {
        import core.stdc.stdio : sprintf;
        import core.stdc.config : LD = c_long_double;

        // No suitable replacement for sprintf in druntime ATM
        if (__ctfe)
            return '<' ~ V.stringof ~ " not supported>";

        // Workaround for https://issues.dlang.org/show_bug.cgi?id=20759
        static if (is(LD == real))
            enum realFmt = "%Lg";
        else
            enum realFmt = "%g";

        char[60] val;
        int len;
        static if (is(V == float) || is(V == double))
            len = sprintf(&val[0], "%g", v);
        else static if (is(V == real))
            len = sprintf(&val[0], realFmt, cast(LD) v);
        else static if (is(V == cfloat) || is(V == cdouble))
            len = sprintf(&val[0], "%g + %gi", v.re, v.im);
        else static if (is(V == creal))
            len = sprintf(&val[0], realFmt ~ " + " ~ realFmt ~ 'i', cast(LD) v.re, cast(LD) v.im);
        else static if (is(V == ifloat) || is(V == idouble))
            len = sprintf(&val[0], "%gi", v);
        else // ireal
        {
            static assert(is(V == ireal));
            static if (is(LD == real))
                alias R = ireal;
            else
                alias R = idouble;
            len = sprintf(&val[0], realFmt ~ 'i', cast(R) v);
        }
        return val.idup[0 .. len];
    }
    // special-handling for void-arrays
    else static if (is(V == typeof(null)))
    {
        return "`null`";
    }
    else static if (is(V == U*, U))
    {
        // Format as ulong and prepend a 0x for pointers
        import core.internal.string;
        return cast(immutable) ("0x" ~ unsignedToTempString!16(cast(ulong) v));
    }
    // toString() isn't always const, e.g. classes inheriting from Object
    else static if (__traits(compiles, { string s = V.init.toString(); }))
    {
        // Object references / struct pointers may be null
        static if (is(V == class) || is(V == interface))
        {
            if (v is null)
                return "`null`";
        }

        try
        {
            // Prefer const overload of toString
            static if (__traits(compiles, { string s = v.toString(); }))
                return v.toString();
            else
                return (cast() v).toString();
        }
        catch (Exception e)
        {
            return `<toString() failed: "` ~ e.msg ~ `", called on ` ~ formatMembers(v) ~`>`;
        }
    }
    // Static arrays or slices (but not aggregates with `alias this`)
    else static if (is(V : U[], U) && !isAggregateType!V)
    {
        import core.internal.traits: Unqual;
        alias E = Unqual!U;

        // special-handling for void-arrays
        static if (is(E == void))
        {
            if (__ctfe)
                return "<void[] not supported>";

            const bytes = cast(byte[]) v;
            return miniFormat(bytes);
        }
        // anything string-like
        else static if (is(E == char) || is(E == dchar) || is(E == wchar))
        {
            const s = `"` ~ v ~ `"`;

            // v could be a char[], dchar[] or wchar[]
            static if (is(typeof(s) : const char[]))
                return cast(immutable) s;
            else
            {
                import core.internal.utf: toUTF8;
                return toUTF8(s);
            }
        }
        else
        {
            string msg = "[";
            foreach (i, ref el; v)
            {
                if (i > 0)
                    msg ~= ", ";

                // don't fully print big arrays
                if (i >= 30)
                {
                    msg ~= "...";
                    break;
                }
                msg ~= miniFormat(el);
            }
            msg ~= "]";
            return msg;
        }
    }
    else static if (is(V : Val[K], K, Val))
    {
        size_t i;
        string msg = "[";
        foreach (ref k, ref val; v)
        {
            if (i > 0)
                msg ~= ", ";
            // don't fully print big AAs
            if (i++ >= 30)
            {
                msg ~= "...";
                break;
            }
            msg ~= miniFormat(k) ~ ": " ~ miniFormat(val);
        }
        msg ~= "]";
        return msg;
    }
    else static if (is(V == struct))
    {
        return formatMembers(v);
    }
    // Extern C++ classes don't have a toString by default
    else static if (is(V == class) || is(V == interface))
    {
        if (v is null)
            return "null";

        // Extern classes might be opaque
        static if (is(typeof(v.tupleof)))
            return formatMembers(v);
        else
            return '<' ~ V.stringof ~ '>';
    }
    else
    {
        return V.stringof;
    }
}

/// Formats `v`'s members as `V(<member 1>, <member 2>, ...)`
private string formatMembers(V)(const scope ref V v)
{
    enum ctxPtr = __traits(isNested, V);
    enum isOverlapped = calcFieldOverlap([ v.tupleof.offsetof ]);

    string msg = V.stringof ~ "(";
    foreach (i, ref field; v.tupleof)
    {
        if (i > 0)
            msg ~= ", ";

        static if (isOverlapped[i])
        {
            msg ~= "<overlapped field>";
        }
        else
        {
            // Mark context pointer
            static if (ctxPtr && i == v.tupleof.length - 1)
                msg ~= "<context>: ";

            msg ~= miniFormat(field);
        }
    }
    msg ~= ")";
    return msg;
}

/**
 * Calculates whether fields are overlapped based on the passed offsets.
 *
 * Params:
 *   offsets = offsets of all fields matching the order of `.tupleof`
 *
 * Returns: an array such that arr[n] = true indicates that the n'th field
 *          overlaps with an adjacent field
 **/
private bool[] calcFieldOverlap(const scope size_t[] offsets)
{
    bool[] overlaps = new bool[](offsets.length);

    foreach (const idx; 1 .. overlaps.length)
    {
        if (offsets[idx - 1] == offsets[idx])
            overlaps[idx - 1] = overlaps[idx] = true;
    }

    return overlaps;
}

/// Negates a comparison token, e.g. `==` is mapped to `!=`
private string invertCompToken(scope string comp) pure nothrow @nogc @safe
{
    switch (comp)
    {
        case "==":
            return "!=";
        case "!=":
            return "==";
        case "<":
            return ">=";
        case "<=":
            return ">";
        case ">":
            return "<=";
        case ">=":
            return "<";
        case "is":
            return "!is";
        case "!is":
            return "is";
        case "in":
            return "!in";
        case "!in":
            return "in";
        default:
            assert(0, combine(["Invalid comparison operator '"], comp, ["'"]));
    }
}

/// Casts the function pointer to include `@safe`, `@nogc`, ...
private auto assumeFakeAttributes(T)(T t) @trusted
{
    import core.internal.traits : Parameters, ReturnType;
    alias RT = ReturnType!T;
    alias P = Parameters!T;
    alias type = RT function(P) nothrow @nogc @safe pure;
    return cast(type) t;
}

/// Wrapper for `miniFormat` which assumes that the implementation is `@safe`, `@nogc`, ...
/// s.t. it does not violate the constraints of the function containing the `assert`.
private string miniFormatFakeAttributes(T)(const scope ref T t)
{
    alias miniT = miniFormat!T;
    return assumeFakeAttributes(&miniT)(t);
}

/// Allocates an array of `t` bytes while pretending to be `@safe`, `@nogc`, ...
private auto pureAlloc(size_t t)
{
    static auto alloc(size_t len)
    {
        return new ubyte[len];
    }
    return assumeFakeAttributes(&alloc)(t);
}

/// Wrapper for GC.inFinalizer that fakes purity
private bool inFinalizer()() pure nothrow @nogc @safe
{
    // CTFE doesn't trigger InvalidMemoryErrors
    import core.memory : GC;
    return !__ctfe && assumeFakeAttributes(&GC.inFinalizer)();
}

// https://issues.dlang.org/show_bug.cgi?id=21544
unittest
{
    // Normal enum values
    enum E { A, BCDE }
    E e = E.A;
    assert(miniFormat(e) == "A");
    e = E.BCDE;
    assert(miniFormat(e) == "BCDE");

    // Invalid enum value is printed as their implicit base type (int)
    e = cast(E) 3;
    assert(miniFormat(e) == "cast(E)  3");

    // Non-integral enums work as well
    static struct S
    {
        int a;
        string str;
    }

    enum E2 : S { a2 = S(1, "Hello") }
    E2 es = E2.a2;
    assert(miniFormat(es) == `a2`);

    // Even invalid values
    es = cast(E2) S(2, "World");
    assert(miniFormat(es) == `cast(E2)  S(2, "World")`);
}

// vectors
unittest
{
    static if (is(__vector(float[4])))
    {
        __vector(float[4]) f = [-1.5f, 0.5f, 1.0f, 0.125f];
        assert(miniFormat(f) == "[-1.5, 0.5, 1, 0.125]");
    }

    static if (is(__vector(int[4])))
    {
        __vector(int[4]) i = [-1, 0, 1, 3];
        assert(miniFormat(i) == "[-1, 0, 1, 3]");
    }
}
