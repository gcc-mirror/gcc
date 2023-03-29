// Written in the D programming language.
/**
$(SCRIPT inhibitQuickIndex = 1;)

This module defines facilities for efficient checking of integral operations
against overflow, casting with loss of precision, unexpected change of sign,
etc. The checking (and possibly correction) can be done at operation level, for
example $(LREF opChecked)$(D !"+"(x, y, overflow)) adds two integrals `x` and
`y` and sets `overflow` to `true` if an overflow occurred. The flag `overflow`
(a `bool` passed by reference) is not touched if the operation succeeded, so the
same flag can be reused for a sequence of operations and tested at the end.

Issuing individual checked operations is flexible and efficient but often
tedious. The $(LREF Checked) facility offers encapsulated integral wrappers that
do all checking internally and have configurable behavior upon erroneous
results. For example, `Checked!int` is a type that behaves like `int` but aborts
execution immediately whenever involved in an operation that produces the
arithmetically wrong result. The accompanying convenience function $(LREF
checked) uses type deduction to convert a value `x` of integral type `T` to
`Checked!T` by means of `checked(x)`. For example:

---
void main()
{
    import std.checkedint, std.stdio;
    writeln((checked(5) + 7).get); // 12
    writeln((checked(10) * 1000 * 1000 * 1000).get); // Overflow
}
---

Similarly, $(D checked(-1) > uint(0)) aborts execution (even though the built-in
comparison $(D int(-1) > uint(0)) is surprisingly true due to language's
conversion rules modeled after C). Thus, `Checked!int` is a virtually drop-in
replacement for `int` useable in debug builds, to be replaced by `int` in
release mode if efficiency demands it.

`Checked`  has customizable behavior with the help of a second type parameter,
`Hook`. Depending on what methods `Hook` defines, core operations on the
underlying integral may be verified for overflow or completely redefined. If
`Hook` defines no method at all and carries no state, there is no change in
behavior, i.e. $(D Checked!(int, void)) is a wrapper around `int` that adds no
customization at all.

This module provides a few predefined hooks (below) that add useful behavior to
`Checked`:

$(BOOKTABLE ,
    $(TR $(TD $(LREF Abort)) $(TD
        fails every incorrect operation with a message to $(REF
        stderr, std, stdio) followed by a call to `assert(0)`. It is the default
        second parameter, i.e. `Checked!short` is the same as
        $(D Checked!(short, Abort)).
    ))
    $(TR $(TD $(LREF Throw)) $(TD
        fails every incorrect operation by throwing an exception.
    ))
    $(TR $(TD $(LREF Warn)) $(TD
        prints incorrect operations to $(REF stderr, std, stdio)
        but otherwise preserves the built-in behavior.
    ))
    $(TR $(TD $(LREF ProperCompare)) $(TD
        fixes the comparison operators `==`, `!=`, `<`, `<=`, `>`, and `>=`
        to return correct results in all circumstances,
        at a slight cost in efficiency. For example,
        $(D Checked!(uint, ProperCompare)(1) > -1) is `true`,
        which is not the case for the built-in comparison. Also, comparing
        numbers for equality with floating-point numbers only passes if the
        integral can be converted to the floating-point number precisely,
        so as to preserve transitivity of equality.
    ))
    $(TR $(TD $(LREF WithNaN)) $(TD
        reserves a special "Not a Number" (NaN) value akin to the homonym value
        reserved for floating-point values. Once a $(D Checked!(X, WithNaN))
        gets this special value, it preserves and propagates it until
        reassigned. $(LREF isNaN) can be used to query whether the object
        is not a number.
    ))
    $(TR $(TD $(LREF Saturate)) $(TD
        implements saturating arithmetic, i.e. $(D Checked!(int, Saturate))
        "stops" at `int.max` for all operations that would cause an `int` to
        overflow toward infinity, and at `int.min` for all operations that would
        correspondingly overflow toward negative infinity.
    ))
)


These policies may be used alone, e.g. $(D Checked!(uint, WithNaN)) defines a
`uint`-like type that reaches a stable NaN state for all erroneous operations.
They may also be "stacked" on top of each other, owing to the property that a
checked integral emulates an actual integral, which means another checked
integral can be built on top of it. Some combinations of interest include:

$(BOOKTABLE ,
    $(TR $(TD $(D Checked!(Checked!int, ProperCompare))))
    $(TR $(TD
defines an `int` with fixed
comparison operators that will fail with `assert(0)` upon overflow. (Recall that
`Abort` is the default policy.) The order in which policies are combined is
important because the outermost policy (`ProperCompare` in this case) has the
first crack at intercepting an operator. The converse combination $(D
Checked!(Checked!(int, ProperCompare))) is meaningless because `Abort` will
intercept comparison and will fail without giving `ProperCompare` a chance to
intervene.
    ))
    $(TR $(TD))
    $(TR $(TDNW $(D Checked!(Checked!(int, ProperCompare), WithNaN))))
    $(TR $(TD
defines an `int`-like
type that supports a NaN value. For values that are not NaN, comparison works
properly. Again the composition order is important; $(D Checked!(Checked!(int,
WithNaN), ProperCompare)) does not have good semantics because `ProperCompare`
intercepts comparisons before the numbers involved are tested for NaN.
    ))
)

The hook's members are looked up statically in a Design by Introspection manner
and are all optional. The table below illustrates the members that a hook type
may define and their influence over the behavior of the `Checked` type using it.
In the table, `hook` is an alias for `Hook` if the type `Hook` does not
introduce any state, or an object of type `Hook` otherwise.

$(TABLE ,
$(TR $(TH `Hook` member) $(TH Semantics in $(D Checked!(T, Hook)))
)
$(TR $(TD `defaultValue`) $(TD If defined, `Hook.defaultValue!T` is used as the
default initializer of the payload.)
)
$(TR $(TD `min`) $(TD If defined, `Hook.min!T` is used as the minimum value of
the payload.)
)
$(TR $(TD `max`) $(TD If defined, `Hook.max!T` is used as the maximum value of
the payload.)
)
$(TR $(TD `hookOpCast`) $(TD If defined, `hook.hookOpCast!U(get)` is forwarded
to unconditionally when the payload is to be cast to type `U`.)
)
$(TR $(TD `onBadCast`) $(TD If defined and `hookOpCast` is $(I not) defined,
`onBadCast!U(get)` is forwarded to when the payload is to be cast to type `U`
and the cast would lose information or force a change of sign.)
)
$(TR $(TD `hookOpEquals`) $(TD If defined, $(D hook.hookOpEquals(get, rhs)) is
forwarded to unconditionally when the payload is compared for equality against
value `rhs` of integral, floating point, or Boolean type.)
)
$(TR $(TD `hookOpCmp`) $(TD If defined, $(D hook.hookOpCmp(get, rhs)) is
forwarded to unconditionally when the payload is compared for ordering against
value `rhs` of integral, floating point, or Boolean type.)
)
$(TR $(TD `hookOpUnary`) $(TD If defined, `hook.hookOpUnary!op(get)` (where `op`
is the operator symbol) is forwarded to for unary operators `-` and `~`. In
addition, for unary operators `++` and `--`, `hook.hookOpUnary!op(payload)` is
called, where `payload` is a reference to the value wrapped by `Checked` so the
hook can change it.)
)
$(TR $(TD `hookOpBinary`) $(TD If defined, $(D hook.hookOpBinary!op(get, rhs))
(where `op` is the operator symbol and `rhs` is the right-hand side operand) is
forwarded to unconditionally for binary operators `+`,  `-`, `*`, `/`, `%`,
`^^`, `&`, `|`, `^`, `<<`, `>>`, and `>>>`.)
)
$(TR $(TD `hookOpBinaryRight`) $(TD If defined, $(D
hook.hookOpBinaryRight!op(lhs, get)) (where `op` is the operator symbol and
`lhs` is the left-hand side operand) is forwarded to unconditionally for binary
operators `+`,  `-`, `*`, `/`, `%`, `^^`, `&`, `|`, `^`, `<<`, `>>`, and `>>>`.)
)
$(TR $(TD `onOverflow`) $(TD If defined, `hook.onOverflow!op(get)` is forwarded
to for unary operators that overflow but only if `hookOpUnary` is not defined.
Unary `~` does not overflow; unary `-` overflows only when the most negative
value of a signed type is negated, and the result of the hook call is returned.
When the increment or decrement operators overflow, the payload is assigned the
result of `hook.onOverflow!op(get)`. When a binary operator overflows, the
result of $(D hook.onOverflow!op(get, rhs)) is returned, but only if `Hook` does
not define `hookOpBinary`.)
)
$(TR $(TD `hookOpOpAssign`) $(TD If defined, $(D hook.hookOpOpAssign!op(payload,
rhs)) (where `op` is the operator symbol and `rhs` is the right-hand side
operand) is forwarded to unconditionally for binary operators `+=`,  `-=`, `*=`, `/=`, `%=`,
`^^=`, `&=`, `|=`, `^=`, `<<=`, `>>=`, and `>>>=`.)
)
$(TR $(TD `onLowerBound`) $(TD If defined, $(D hook.onLowerBound(value, bound))
(where `value` is the value being assigned) is forwarded to when the result of
binary operators `+=`,  `-=`, `*=`, `/=`, `%=`, `^^=`, `&=`, `|=`, `^=`, `<<=`, `>>=`,
and `>>>=` is smaller than the smallest value representable by `T`.)
)
$(TR $(TD `onUpperBound`) $(TD If defined, $(D hook.onUpperBound(value, bound))
(where `value` is the value being assigned) is forwarded to when the result of
binary operators `+=`,  `-=`, `*=`, `/=`, `%=`, `^^=`, `&=`, `|=`, `^=`, `<<=`, `>>=`,
and `>>>=` is larger than the largest value representable by `T`.)
)
$(TR $(TD `hookToHash`) $(TD If defined, $(D hook.hookToHash(payload))
(where `payload` is a reference to the value wrapped by Checked) is forwarded
to when `toHash` is called on a Checked type. Custom hashing can be implemented
in a `Hook`, otherwise the built-in hashing is used.)
)
)

Source: $(PHOBOSSRC std/checkedint.d)
*/
module std.checkedint;
import std.traits : isFloatingPoint, isIntegral, isNumeric, isUnsigned, Unqual;

///
@safe unittest
{
    int[] concatAndAdd(int[] a, int[] b, int offset)
    {
        // Aborts on overflow on size computation
        auto r = new int[(checked(a.length) + b.length).get];
        // Aborts on overflow on element computation
        foreach (i; 0 .. a.length)
            r[i] = (a[i] + checked(offset)).get;
        foreach (i; 0 .. b.length)
            r[i + a.length] = (b[i] + checked(offset)).get;
        return r;
    }
    assert(concatAndAdd([1, 2, 3], [4, 5], -1) == [0, 1, 2, 3, 4]);
}


/// `Saturate` stops at an overflow
@safe unittest
{
    auto x = (cast(byte) 127).checked!Saturate;
    assert(x == 127);
    x++;
    assert(x == 127);
}

/// `WithNaN` has a special "Not a Number" (NaN) value akin to the homonym value reserved for floating-point values
@safe unittest
{
    auto x = 100.checked!WithNaN;
    assert(x == 100);
    x /= 0;
    assert(x.isNaN);
}

/// `ProperCompare` fixes the comparison operators ==, !=, <, <=, >, and >= to return correct results
@safe unittest
{
    uint x = 1;
    auto y = x.checked!ProperCompare;
    assert(x < -1); // built-in comparison
    assert(y > -1); // ProperCompare
}

/// `Throw` fails every incorrect operation by throwing an exception
@safe unittest
{
    import std.exception : assertThrown;
    auto x = -1.checked!Throw;
    assertThrown(x / 0);
    assertThrown(x + int.min);
    assertThrown(x == uint.max);
}

/**
Checked integral type wraps an integral `T` and customizes its behavior with the
help of a `Hook` type. The type wrapped must be one of the predefined integrals
(unqualified), or another instance of `Checked`.

Params:
    T    = type that is wrapped in the `Checked` type
    Hook = hook type that customizes the behavior of the `Checked` type
*/
struct Checked(T, Hook = Abort)
if (isIntegral!T || is(T == Checked!(U, H), U, H))
{
    import std.algorithm.comparison : among;
    import std.experimental.allocator.common : stateSize;
    import std.format.spec : FormatSpec;
    import std.range.primitives : isInputRange, ElementType;
    import std.traits : hasMember, isSomeChar;

    /**
    The type of the integral subject to checking.
    */
    alias Representation = T;

    // state {
    static if (hasMember!(Hook, "defaultValue"))
        private T payload = Hook.defaultValue!T;
    else
        private T payload;
    /**
    `hook` is a member variable if it has state, or an alias for `Hook`
    otherwise.
    */
    static if (stateSize!Hook > 0) Hook hook;
    else alias hook = Hook;
    // } state

    // get
    /**
    Returns:
        A copy of the underlying value.
    */
    auto get() inout { return payload; }
    ///
    @safe unittest
    {
        auto x = checked(ubyte(42));
        static assert(is(typeof(x.get()) == ubyte));
        assert(x.get == 42);
        const y = checked(ubyte(42));
        static assert(is(typeof(y.get()) == const ubyte));
        assert(y.get == 42);
    }

    /**
    Defines the minimum and maximum. These values are hookable by defining
    `Hook.min` and/or `Hook.max`.
    */
    static if (hasMember!(Hook, "min"))
    {
        enum Checked!(T, Hook) min = Checked!(T, Hook)(Hook.min!T);
        ///
        @safe unittest
        {
            assert(Checked!short.min == -32768);
            assert(Checked!(short, WithNaN).min == -32767);
            assert(Checked!(uint, WithNaN).max == uint.max - 1);
        }
    }
    else
    {
        /// ditto
        enum Checked!(T, Hook) min = Checked(T.min);
    }
    static if (hasMember!(Hook, "max"))
    {
        /// ditto
        enum Checked!(T, Hook) max = Checked(Hook.max!T);
    }
    else
    {
        /// ditto
        enum Checked!(T, Hook) max = Checked(T.max);
    }

    /**
    Constructor taking a value properly convertible to the underlying type. `U`
    may be either an integral that can be converted to `T` without a loss, or
    another `Checked` instance whose representation may be in turn converted to
    `T` without a loss.
    */
    this(U)(U rhs)
    if (valueConvertible!(U, T) ||
        !isIntegral!T && is(typeof(T(rhs))) ||
        is(U == Checked!(V, W), V, W) &&
            is(typeof(Checked!(T, Hook)(rhs.get))))
    {
        static if (isIntegral!U)
            payload = rhs;
        else
            payload = rhs.payload;
    }
    ///
    @safe unittest
    {
        auto a = checked(42L);
        assert(a == 42);
        auto b = Checked!long(4242); // convert 4242 to long
        assert(b == 4242);
    }

    /**
    Assignment operator. Has the same constraints as the constructor.

    Params:
        rhs = The value to assign

    Returns:
        A reference to `this`
    */
    ref Checked opAssign(U)(U rhs) return
    if (is(typeof(Checked!(T, Hook)(rhs))))
    {
        static if (isIntegral!U)
            payload = rhs;
        else
            payload = rhs.payload;
        return this;
    }
    ///
    @safe unittest
    {
        Checked!long a;
        a = 42L;
        assert(a == 42);
        a = 4242;
        assert(a == 4242);
    }

    ///
    @safe unittest
    {
        Checked!long a, b;
        a = b = 3;
        assert(a == 3 && b == 3);
    }

    /**
    Construct from a decimal string. The conversion follows the same rules as
    $(REF to, std, conv) converting a string to the wrapped `T` type.

    Params:
        str = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
              of characters
    */
    this(Range)(Range str)
    if (isInputRange!Range && isSomeChar!(ElementType!Range))
    {
        import std.conv : to;

        this(to!T(str));
    }

    /**
    $(REF to, std, conv) can convert a string to a `Checked!T`:
    */
    @system unittest
    {
        import std.conv : to;

        const a = to!long("1234");
        const b = to!(Checked!long)("1234");
        assert(a == b);
    }

    // opCast
    /**
    Casting operator to integral, `bool`, or floating point type.

    If a cast to a floating-point type is requested and `Hook` defines
    `onBadCast`, the cast is verified by ensuring $(D get == cast(T)
    U(get)). If that is not `true`, `hook.onBadCast!U(get)` is returned.

    If a cast to an integral type is requested and `Hook` defines `onBadCast`,
    the cast is verified by ensuring `get` and $(D cast(U)
    get) are the same arithmetic number. (Note that `int(-1)` and
    `uint(1)` are different values arithmetically although they have the same
    bitwise representation and compare equal by language rules.) If the numbers
    are not arithmetically equal, `hook.onBadCast!U(get)` is
    returned.

    Params:
        U = The type to cast to

    Returns:
        If `Hook`     defines `hookOpCast`, the call immediately returns
        `hook.hookOpCast!U(get)`. Otherwise, casting to `bool` yields $(D
        get != 0) and casting to another integral that can represent all
        values of `T` returns `get` promoted to `U`.
    */
    U opCast(U, this _)()
    if (isIntegral!U || isFloatingPoint!U || is(U == bool))
    {
        static if (hasMember!(Hook, "hookOpCast"))
        {
            return hook.hookOpCast!U(payload);
        }
        else static if (is(U == bool))
        {
            return payload != 0;
        }
        else static if (valueConvertible!(T, U))
        {
            return payload;
        }
        // may lose bits or precision
        else static if (!hasMember!(Hook, "onBadCast"))
        {
            return cast(U) payload;
        }
        else
        {
            if (isUnsigned!T || !isUnsigned!U ||
                    T.sizeof > U.sizeof || payload >= 0)
            {
                auto result = cast(U) payload;
                // If signedness is different, we need additional checks
                if (result == payload &&
                        (!isUnsigned!T || isUnsigned!U || result >= 0))
                    return result;
            }
            return hook.onBadCast!U(payload);
        }
    }
    ///
    @safe unittest
    {
        assert(cast(uint) checked(42) == 42);
        assert(cast(uint) checked!WithNaN(-42) == uint.max);
    }

    // opEquals
    /**
    Compares `this` against `rhs` for equality.

    If `U` is also an instance of `Checked`, both hooks (left- and right-hand
    side) are introspected for the method `hookOpEquals`. If both define it,
    priority is given to the left-hand side.

    Params:
        rhs = Right-hand side to compare for equality

    Returns:
        If `Hook` defines `hookOpEquals`, the function forwards to $(D
        hook.hookOpEquals(get, rhs)). Otherwise, the result of the
        built-in operation $(D get == rhs) is returned.

    */
    bool opEquals(U, this _)(U rhs)
    if (isIntegral!U || isFloatingPoint!U || is(U == bool) ||
        is(U == Checked!(V, W), V, W) && is(typeof(this == rhs.payload)))
    {
        static if (is(U == Checked!(V, W), V, W))
        {
            alias R = typeof(payload + rhs.payload);
            static if (is(Hook == W))
            {
                // Use the lhs hook if there
                return this == rhs.payload;
            }
            else static if (valueConvertible!(T, R) && valueConvertible!(V, R))
            {
                return payload == rhs.payload;
            }
            else static if (hasMember!(Hook, "hookOpEquals"))
            {
                return hook.hookOpEquals(payload, rhs.payload);
            }
            else static if (hasMember!(W, "hookOpEquals"))
            {
                return rhs.hook.hookOpEquals(rhs.payload, payload);
            }
            else
            {
                return payload == rhs.payload;
            }
        }
        else static if (hasMember!(Hook, "hookOpEquals"))
            return hook.hookOpEquals(payload, rhs);
        else static if (isIntegral!U || isFloatingPoint!U || is(U == bool))
            return payload == rhs;
    }

    ///
    static if (is(T == int) && is(Hook == void)) @safe unittest
    {
        import std.traits : isUnsigned;

        static struct MyHook
        {
            static bool thereWereErrors;
            static bool hookOpEquals(L, R)(L lhs, R rhs)
            {
                if (lhs != rhs) return false;
                static if (isUnsigned!L && !isUnsigned!R)
                {
                    if (lhs > 0 && rhs < 0) thereWereErrors = true;
                }
                else static if (isUnsigned!R && !isUnsigned!L)
                    if (lhs < 0 && rhs > 0) thereWereErrors = true;
                // Preserve built-in behavior.
                return true;
            }
        }
        auto a = checked!MyHook(-42);
        assert(a == uint(-42));
        assert(MyHook.thereWereErrors);
        MyHook.thereWereErrors = false;
        assert(checked!MyHook(uint(-42)) == -42);
        assert(MyHook.thereWereErrors);
        static struct MyHook2
        {
            static bool hookOpEquals(L, R)(L lhs, R rhs)
            {
                return lhs == rhs;
            }
        }
        MyHook.thereWereErrors = false;
        assert(checked!MyHook2(uint(-42)) == a);
        // Hook on left hand side takes precedence, so no errors
        assert(!MyHook.thereWereErrors);
    }

    // toHash
    /**
    Generates a hash for `this`. If `Hook` defines `hookToHash`, the call
    immediately returns `hook.hookToHash(payload)`. If `Hook` does not
    implement `hookToHash`, but it has state, a hash will be generated for
    the `Hook` using the built-in function and it will be xored with the
    hash of the `payload`.

    Returns:
        The hash of `this` instance.

    */
    size_t toHash() const nothrow @safe
    {
        static if (hasMember!(Hook, "hookToHash"))
        {
            return hook.hookToHash(payload);
        }
        else static if (stateSize!Hook > 0)
        {
            static if (hasMember!(typeof(payload), "toHash"))
            {
                return payload.toHash() ^ hashOf(hook);
            }
            else
            {
                return hashOf(payload) ^ hashOf(hook);
            }
        }
        else static if (hasMember!(typeof(payload), "toHash"))
        {
            return payload.toHash();
        }
        else
        {
            return .hashOf(payload);
        }
    }

    /// ditto
    size_t toHash(this _)() shared const nothrow @safe
    {
        import core.atomic : atomicLoad, MemoryOrder;
        static if (is(typeof(this.payload.atomicLoad!(MemoryOrder.acq)) P))
        {
            auto localPayload = __ctfe ? cast(P) this.payload
                                  : this.payload.atomicLoad!(MemoryOrder.acq);
        }
        else
        {
            alias localPayload = this.payload;
        }

        static if (hasMember!(Hook, "hookToHash"))
        {
            return hook.hookToHash(localPayload);
        }
        else static if (stateSize!Hook > 0)
        {
            static if (hasMember!(typeof(localPayload), "toHash"))
            {
                return localPayload.toHash() ^ hashOf(hook);
            }
            else
            {
                return hashOf(localPayload) ^ hashOf(hook);
            }
        }
        else static if (hasMember!(typeof(localPayload), "toHash"))
        {
            return localPayload.toHash();
        }
        else
        {
            return .hashOf(localPayload);
        }
    }

    /**
    Writes a string representation of this to a `sink`.

    Params:
      sink = A `Char` accepting
             $(REF_ALTTEXT output range, isOutputRange, std,range,primitives).
      fmt  = A $(REF FormatSpec, std, format) which controls how this
             is formatted.
    */
    void toString(Writer, Char)(scope ref Writer sink, scope const ref FormatSpec!Char fmt) const
    {
        import std.format.write : formatValue;
        if (fmt.spec == 's')
            return formatValue(sink, this, fmt);
        else
            return formatValue(sink, payload, fmt);
    }

    /**
    `toString` is rarely directly invoked; the usual way of using it is via
    $(REF format, std, format):
    */
    @system unittest
    {
        import std.format;

        assert(format("%04d", checked(15)) == "0015");
        assert(format("0x%02x", checked(15)) == "0x0f");
    }

    // opCmp
    /**

    Compares `this` against `rhs` for ordering. If `Hook` defines `hookOpCmp`,
    the function forwards to $(D hook.hookOpCmp(get, rhs)). Otherwise, the
    result of the built-in comparison operation is returned.

    If `U` is also an instance of `Checked`, both hooks (left- and right-hand
    side) are introspected for the method `hookOpCmp`. If both define it,
    priority is given to the left-hand side.

    Params:
        rhs   = The right-hand side operand
        U     = either the type of `rhs` or the underlying type
                if `rhs` is a `Checked` instance
        Hook1 = If `rhs` is a `Checked` instance, `Hook1` represents
                the instance's behavior hook

    Returns:
        The result of `hookOpCmp` if `hook` defines `hookOpCmp`. If
        `U` is an instance of `Checked` and `hook` does not define
        `hookOpCmp`, result of `rhs.hook.hookOpCmp` is returned.
        If none of the instances specify the behavior via `hookOpCmp`,
        `-1` is returned if `lhs` is lesser than `rhs`, `1` if `lhs`
        is greater than `rhs` and `0` on equality.
    */
    auto opCmp(U, this _)(const U rhs) //const pure @safe nothrow @nogc
    if (isIntegral!U || isFloatingPoint!U || is(U == bool))
    {
        static if (hasMember!(Hook, "hookOpCmp"))
        {
            return hook.hookOpCmp(payload, rhs);
        }
        else static if (valueConvertible!(T, U) || valueConvertible!(U, T))
        {
            return payload < rhs ? -1 : payload > rhs;
        }
        else static if (isFloatingPoint!U)
        {
            U lhs = payload;
            return lhs < rhs ? U(-1.0)
                : lhs > rhs ? U(1.0)
                : lhs == rhs ? U(0.0) : U.init;
        }
        else
        {
            return payload < rhs ? -1 : payload > rhs;
        }
    }

    /// ditto
    auto opCmp(U, Hook1, this _)(Checked!(U, Hook1) rhs)
    {
        alias R = typeof(payload + rhs.payload);
        static if (valueConvertible!(T, R) && valueConvertible!(U, R))
        {
            return payload < rhs.payload ? -1 : payload > rhs.payload;
        }
        else static if (is(Hook == Hook1))
        {
            // Use the lhs hook
            return this.opCmp(rhs.payload);
        }
        else static if (hasMember!(Hook, "hookOpCmp"))
        {
            return hook.hookOpCmp(get, rhs.get);
        }
        else static if (hasMember!(Hook1, "hookOpCmp"))
        {
            return -rhs.hook.hookOpCmp(rhs.payload, get);
        }
        else
        {
            return payload < rhs.payload ? -1 : payload > rhs.payload;
        }
    }

    ///
    static if (is(T == int) && is(Hook == void)) @safe unittest
    {
        import std.traits : isUnsigned;

        static struct MyHook
        {
            static bool thereWereErrors;
            static int hookOpCmp(L, R)(L lhs, R rhs)
            {
                static if (isUnsigned!L && !isUnsigned!R)
                {
                    if (rhs < 0 && rhs >= lhs)
                        thereWereErrors = true;
                }
                else static if (isUnsigned!R && !isUnsigned!L)
                {
                    if (lhs < 0 && lhs >= rhs)
                        thereWereErrors = true;
                }
                // Preserve built-in behavior.
                return lhs < rhs ? -1 : lhs > rhs;
            }
        }
        auto a = checked!MyHook(-42);
        assert(a > uint(42));
        assert(MyHook.thereWereErrors);
        static struct MyHook2
        {
            static int hookOpCmp(L, R)(L lhs, R rhs)
            {
                // Default behavior
                return lhs < rhs ? -1 : lhs > rhs;
            }
        }
        MyHook.thereWereErrors = false;
        assert(Checked!(uint, MyHook2)(uint(-42)) <= a);
        //assert(Checked!(uint, MyHook2)(uint(-42)) >= a);
        // Hook on left hand side takes precedence, so no errors
        assert(!MyHook.thereWereErrors);
        assert(a <= Checked!(uint, MyHook2)(uint(-42)));
        assert(MyHook.thereWereErrors);
    }

    // For coverage
    static if (is(T == int) && is(Hook == void)) @safe unittest
    {
        assert(checked(42) <= checked!void(42));
        assert(checked!void(42) <= checked(42u));
        assert(checked!void(42) <= checked!(void*)(42u));
    }

    // opUnary
    /**

    Defines unary operators `+`, `-`, `~`, `++`, and `--`. Unary `+` is not
    overridable and always has built-in behavior (returns `this`). For the
    others, if `Hook` defines `hookOpUnary`, `opUnary` forwards to $(D
    Checked!(typeof(hook.hookOpUnary!op(get)),
    Hook)(hook.hookOpUnary!op(get))).

    If `Hook` does not define `hookOpUnary` but defines `onOverflow`, `opUnary`
    forwards to `hook.onOverflow!op(get)` in case an overflow occurs.
    For `++` and `--`, the payload is assigned from the result of the call to
    `onOverflow`.

    Note that unary `-` is considered to overflow if `T` is a signed integral of
    32 or 64 bits and is equal to the most negative value. This is because that
    value has no positive negation.

    Params:
        op = The unary operator

    Returns:
        A `Checked` instance representing the result of the unary
        operation
    */
    auto opUnary(string op, this _)()
    if (op == "+" || op == "-" || op == "~")
    {
        static if (op == "+")
            return Checked(this); // "+" is not hookable
        else static if (hasMember!(Hook, "hookOpUnary"))
        {
            auto r = hook.hookOpUnary!op(payload);
            return Checked!(typeof(r), Hook)(r);
        }
        else static if (op == "-" && isIntegral!T && T.sizeof >= 4 &&
                !isUnsigned!T && hasMember!(Hook, "onOverflow"))
        {
            static assert(is(typeof(-payload) == typeof(payload)));
            bool overflow;
            import core.checkedint : negs;
            auto r = negs(payload, overflow);
            if (overflow) r = hook.onOverflow!op(payload);
            return Checked(r);
        }
        else
            return Checked(mixin(op ~ "payload"));
    }

    /// ditto
    ref Checked opUnary(string op)() return
    if (op == "++" || op == "--")
    {
        static if (hasMember!(Hook, "hookOpUnary"))
            hook.hookOpUnary!op(payload);
        else static if (hasMember!(Hook, "onOverflow"))
        {
            static if (op == "++")
            {
                if (payload == max.payload)
                    payload = hook.onOverflow!"++"(payload);
                else
                    ++payload;
            }
            else
            {
                if (payload == min.payload)
                    payload = hook.onOverflow!"--"(payload);
                else
                    --payload;
            }
        }
        else
            mixin(op ~ "payload;");
        return this;
    }

    ///
    static if (is(T == int) && is(Hook == void)) @safe unittest
    {
        static struct MyHook
        {
            static bool thereWereErrors;
            static L hookOpUnary(string x, L)(L lhs)
            {
                if (x == "-" && lhs == -lhs) thereWereErrors = true;
                return -lhs;
            }
        }
        auto a = checked!MyHook(long.min);
        assert(a == -a);
        assert(MyHook.thereWereErrors);
        auto b = checked!void(42);
        assert(++b == 43);
    }

    // opBinary
    /**

    Defines binary operators `+`, `-`, `*`, `/`, `%`, `^^`, `&`, `|`, `^`, `<<`, `>>`,
    and `>>>`. If `Hook` defines `hookOpBinary`, `opBinary` forwards to $(D
    Checked!(typeof(hook.hookOpBinary!op(get, rhs)),
    Hook)(hook.hookOpBinary!op(get, rhs))).

    If `Hook` does not define `hookOpBinary` but defines `onOverflow`,
    `opBinary` forwards to `hook.onOverflow!op(get, rhs)` in case an
    overflow occurs.

    If two `Checked` instances are involved in a binary operation and both
    define `hookOpBinary`, the left-hand side hook has priority. If both define
    `onOverflow`, a compile-time error occurs.

    Params:
        op    = The binary operator
        rhs   = The right hand side operand
        U     = If `rhs` is a `Checked` instance, `U` represents
                the underlying instance type
        Hook1 = If `rhs` is a `Checked` instance, `Hook1` represents
                the instance's behavior hook

    Returns:
        A `Checked` instance representing the result of the binary
        operation
    */
    auto opBinary(string op, Rhs)(const Rhs rhs)
    if (isIntegral!Rhs || isFloatingPoint!Rhs || is(Rhs == bool))
    {
        return opBinaryImpl!(op, Rhs, typeof(this))(rhs);
    }

    /// ditto
    auto opBinary(string op, Rhs)(const Rhs rhs) const
    if (isIntegral!Rhs || isFloatingPoint!Rhs || is(Rhs == bool))
    {
        return opBinaryImpl!(op, Rhs, typeof(this))(rhs);
    }

    private auto opBinaryImpl(string op, Rhs, this _)(const Rhs rhs)
    {
        alias R = typeof(mixin("payload" ~ op ~ "rhs"));
        static assert(is(typeof(mixin("payload" ~ op ~ "rhs")) == R));
        static if (isIntegral!R) alias Result = Checked!(R, Hook);
        else alias Result = R;

        static if (hasMember!(Hook, "hookOpBinary"))
        {
            auto r = hook.hookOpBinary!op(payload, rhs);
            return Checked!(typeof(r), Hook)(r);
        }
        else static if (is(Rhs == bool))
        {
            return mixin("this" ~ op ~ "ubyte(rhs)");
        }
        else static if (isFloatingPoint!Rhs)
        {
            return mixin("payload" ~ op ~ "rhs");
        }
        else static if (hasMember!(Hook, "onOverflow"))
        {
            bool overflow;
            auto r = opChecked!op(payload, rhs, overflow);
            if (overflow) r = hook.onOverflow!op(payload, rhs);
            return Result(r);
        }
        else
        {
            // Default is built-in behavior
            return Result(mixin("payload" ~ op ~ "rhs"));
        }
    }

    /// ditto
    auto opBinary(string op, U, Hook1)(Checked!(U, Hook1) rhs)
    {
        return opBinaryImpl2!(op, U, Hook1, typeof(this))(rhs);
    }

    /// ditto
    auto opBinary(string op, U, Hook1)(Checked!(U, Hook1) rhs) const
    {
        return opBinaryImpl2!(op, U, Hook1, typeof(this))(rhs);
    }

    private
    auto opBinaryImpl2(string op, U, Hook1, this _)(Checked!(U, Hook1) rhs)
    {
        alias R = typeof(get + rhs.payload);
        static if (valueConvertible!(T, R) && valueConvertible!(U, R) ||
            is(Hook == Hook1))
        {
            // Delegate to lhs
            return mixin("this" ~ op ~ "rhs.payload");
        }
        else static if (hasMember!(Hook, "hookOpBinary"))
        {
            return hook.hookOpBinary!op(payload, rhs);
        }
        else static if (hasMember!(Hook1, "hookOpBinary"))
        {
            // Delegate to rhs
            return mixin("this.payload" ~ op ~ "rhs");
        }
        else static if (hasMember!(Hook, "onOverflow") &&
            !hasMember!(Hook1, "onOverflow"))
        {
            // Delegate to lhs
            return mixin("this" ~ op ~ "rhs.payload");
        }
        else static if (hasMember!(Hook1, "onOverflow") &&
            !hasMember!(Hook, "onOverflow"))
        {
            // Delegate to rhs
            return mixin("this.payload" ~ op ~ "rhs");
        }
        else
        {
            static assert(0, "Conflict between lhs and rhs hooks," ~
                " use .get on one side to disambiguate.");
        }
    }

    static if (is(T == int) && is(Hook == void)) @safe unittest
    {
        const a = checked(42);
        assert(a + 1 == 43);
        assert(a + checked(uint(42)) == 84);
        assert(checked(42) + checked!void(42u) == 84);
        assert(checked!void(42) + checked(42u) == 84);

        static struct MyHook
        {
            static uint tally;
            static auto hookOpBinary(string x, L, R)(L lhs, R rhs)
            {
                ++tally;
                return mixin("lhs" ~ x ~ "rhs");
            }
        }
        assert(checked!MyHook(42) + checked(42u) == 84);
        assert(checked!void(42) + checked!MyHook(42u) == 84);
        assert(MyHook.tally == 2);
    }

    // opBinaryRight
    /**

    Defines binary operators `+`, `-`, `*`, `/`, `%`, `^^`, `&`, `|`, `^`, `<<`,
    `>>`, and `>>>` for the case when a built-in numeric or Boolean type is on
    the left-hand side, and a `Checked` instance is on the right-hand side.

    Params:
        op  = The binary operator
        lhs = The left hand side operand

    Returns:
        A `Checked` instance representing the result of the binary
        operation

    */
    auto opBinaryRight(string op, Lhs)(const Lhs lhs)
    if (isIntegral!Lhs || isFloatingPoint!Lhs || is(Lhs == bool))
    {
        return opBinaryRightImpl!(op, Lhs, typeof(this))(lhs);
    }

    /// ditto
    auto opBinaryRight(string op, Lhs)(const Lhs lhs) const
    if (isIntegral!Lhs || isFloatingPoint!Lhs || is(Lhs == bool))
    {
        return opBinaryRightImpl!(op, Lhs, typeof(this))(lhs);
    }

    private auto opBinaryRightImpl(string op, Lhs, this _)(const Lhs lhs)
    {
        static if (hasMember!(Hook, "hookOpBinaryRight"))
        {
            auto r = hook.hookOpBinaryRight!op(lhs, payload);
            return Checked!(typeof(r), Hook)(r);
        }
        else static if (hasMember!(Hook, "hookOpBinary"))
        {
            auto r = hook.hookOpBinary!op(lhs, payload);
            return Checked!(typeof(r), Hook)(r);
        }
        else static if (is(Lhs == bool))
        {
            return mixin("ubyte(lhs)" ~ op ~ "this");
        }
        else static if (isFloatingPoint!Lhs)
        {
            return mixin("lhs" ~ op ~ "payload");
        }
        else static if (hasMember!(Hook, "onOverflow"))
        {
            bool overflow;
            auto r = opChecked!op(lhs, T(payload), overflow);
            if (overflow) r = hook.onOverflow!op(lhs, payload);
            return Checked!(typeof(r), Hook)(r);
        }
        else
        {
            // Default is built-in behavior
            auto r = mixin("lhs" ~ op ~ "T(payload)");
            return Checked!(typeof(r), Hook)(r);
        }
    }

    static if (is(T == int) && is(Hook == void)) @safe unittest
    {
        assert(1 + checked(1) == 2);
        static uint tally;
        static struct MyHook
        {
            static auto hookOpBinaryRight(string x, L, R)(L lhs, R rhs)
            {
                ++tally;
                return mixin("lhs" ~ x ~ "rhs");
            }
        }
        assert(1 + checked!MyHook(1) == 2);
        assert(tally == 1);

        immutable x1 = checked(1);
        assert(1 + x1 == 2);
        immutable x2 = checked!MyHook(1);
        assert(1 + x2 == 2);
        assert(tally == 2);
    }

    // opOpAssign
    /**

    Defines operators `+=`, `-=`, `*=`, `/=`, `%=`, `^^=`, `&=`, `|=`, `^=`,
    `<<=`, `>>=`, and `>>>=`.

    If `Hook` defines `hookOpOpAssign`, `opOpAssign` forwards to
    `hook.hookOpOpAssign!op(payload, rhs)`, where `payload` is a reference to
    the internally held data so the hook can change it.

    Otherwise, the operator first evaluates $(D auto result =
    opBinary!op(payload, rhs).payload), which is subject to the hooks in
    `opBinary`. Then, if `result` is less than $(D Checked!(T, Hook).min) and if
    `Hook` defines `onLowerBound`, the payload is assigned from $(D
    hook.onLowerBound(result, min)). If `result` is greater than $(D Checked!(T,
    Hook).max) and if `Hook` defines `onUpperBound`, the payload is assigned
    from $(D hook.onUpperBound(result, min)).

    If the right-hand side is also a Checked but with a different hook or
    underlying type, the hook and underlying type of this Checked takes
    precedence.

    In all other cases, the built-in behavior is carried out.

    Params:
    op = The operator involved (without the `"="`, e.g. `"+"` for `"+="` etc)
    rhs = The right-hand side of the operator (left-hand side is `this`)

    Returns: A reference to `this`.
    */
    ref Checked opOpAssign(string op, Rhs)(const Rhs rhs) return
    if (isIntegral!Rhs || isFloatingPoint!Rhs || is(Rhs == bool))
    {
        static assert(is(typeof(mixin("payload" ~ op ~ "=rhs")) == T));

        static if (hasMember!(Hook, "hookOpOpAssign"))
        {
            hook.hookOpOpAssign!op(payload, rhs);
        }
        else
        {
            alias R = typeof(get + rhs);
            auto r = opBinary!op(rhs).get;
            import std.conv : unsigned;

            static if (ProperCompare.hookOpCmp(R.min, min.get) < 0 &&
                hasMember!(Hook, "onLowerBound"))
            {
                if (ProperCompare.hookOpCmp(r, min.get) < 0)
                {
                    // Example: Checked!uint(1) += int(-3)
                    payload = hook.onLowerBound(r, min.get);
                    return this;
                }
            }
            static if (ProperCompare.hookOpCmp(max.get, R.max) < 0 &&
                hasMember!(Hook, "onUpperBound"))
            {
                if (ProperCompare.hookOpCmp(r, max.get) > 0)
                {
                    // Example: Checked!uint(1) += long(uint.max)
                    payload = hook.onUpperBound(r, max.get);
                    return this;
                }
            }
            payload = cast(T) r;
        }
        return this;
    }

    /// ditto
    ref Checked opOpAssign(string op, Rhs)(const Rhs rhs) return
    if (is(Rhs == Checked!(RhsT, RhsHook), RhsT, RhsHook))
    {
        return opOpAssign!(op, typeof(rhs.payload))(rhs.payload);
    }

    ///
    static if (is(T == int) && is(Hook == void)) @safe unittest
    {
        static struct MyHook
        {
            static bool thereWereErrors;
            static T onLowerBound(Rhs, T)(Rhs rhs, T bound)
            {
                thereWereErrors = true;
                return bound;
            }
            static T onUpperBound(Rhs, T)(Rhs rhs, T bound)
            {
                thereWereErrors = true;
                return bound;
            }
        }
        auto x = checked!MyHook(byte.min);
        x -= 1;
        assert(MyHook.thereWereErrors);
        MyHook.thereWereErrors = false;
        x = byte.max;
        x += 1;
        assert(MyHook.thereWereErrors);
    }
}

///
@safe @nogc pure nothrow unittest
{
    // Hook that ignores all problems.
    static struct Ignore
    {
        @nogc nothrow pure @safe static:
        Dst onBadCast(Dst, Src)(Src src) { return cast(Dst) src; }
        Lhs onLowerBound(Rhs, T)(Rhs rhs, T bound) { return cast(T) rhs; }
        T onUpperBound(Rhs, T)(Rhs rhs, T bound) { return cast(T) rhs; }
        bool hookOpEquals(Lhs, Rhs)(Lhs lhs, Rhs rhs) { return lhs == rhs; }
        int hookOpCmp(Lhs, Rhs)(Lhs lhs, Rhs rhs) { return (lhs > rhs) - (lhs < rhs); }
        typeof(~Lhs()) onOverflow(string x, Lhs)(ref Lhs lhs) { return mixin(x ~ "lhs"); }
        typeof(Lhs() + Rhs()) onOverflow(string x, Lhs, Rhs)(Lhs lhs, Rhs rhs)
        {
            static if (x == "/")
                return typeof(lhs / rhs).min;
            else
                return mixin("lhs" ~ x ~ "rhs");
        }
    }

    auto x = Checked!(int, Ignore)(5) + 7;
}


/**

Convenience function that turns an integral into the corresponding `Checked`
instance by using template argument deduction. The hook type may be specified
(by default `Abort`).

Params:
    Hook  = type that customizes the behavior, by default `Abort`
    T     = type represetinfg the underlying represantion of the `Checked` instance
    value = the actual value of the representation

Returns:
    A `Checked` instance customized by the provided `Hook` and `value`
*/
Checked!(T, Hook) checked(Hook = Abort, T)(const T value)
if (is(typeof(Checked!(T, Hook)(value))))
{
    return Checked!(T, Hook)(value);
}

///
@safe unittest
{
    static assert(is(typeof(checked(42)) == Checked!int));
    assert(checked(42) == Checked!int(42));
    static assert(is(typeof(checked!WithNaN(42)) == Checked!(int, WithNaN)));
    assert(checked!WithNaN(42) == Checked!(int, WithNaN)(42));
}

// get
@safe unittest
{
    void test(T)()
    {
        assert(Checked!(T, void)(ubyte(22)).get == 22);
    }
    test!ubyte;
    test!(const ubyte);
    test!(immutable ubyte);
}

@system unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=21758
    assert(4 * checked(5L) == 20);
    assert(20 / checked(5L) == 4);
    assert(2 ^^ checked(3L) == 8);
    assert(12 % checked(5L) == 2);
    assert((0xff & checked(3L)) == 3);
    assert((0xf0 | checked(3L)) == 0xf3);
    assert((0xff ^ checked(3L)) == 0xfc);
}

// Abort
/**

Force all integral errors to fail by printing an error message to `stderr` and
then abort the program. `Abort` is the default second argument for `Checked`.

*/
struct Abort
{
static:
    /**

    Called automatically upon a bad cast (one that loses precision or attempts
    to convert a negative value to an unsigned type). The source type is `Src`
    and the destination type is `Dst`.

    Params:
        src = Souce operand

    Returns:
        Nominally the result is the desired value of the cast operation,
        which will be forwarded as the result of the cast. For `Abort`, the
        function never returns because it aborts the program.
    */
    Dst onBadCast(Dst, Src)(Src src)
    {
        Warn.onBadCast!Dst(src);
        assert(0);
    }

    /**

    Called automatically upon a bounds error.

    Params:
    rhs = The right-hand side value in the assignment, after the operator has
    been evaluated
    bound = The value of the bound being violated

    Returns: Nominally the result is the desired value of the operator, which
    will be forwarded as result. For `Abort`, the function never returns because
    it aborts the program.

    */
    T onLowerBound(Rhs, T)(Rhs rhs, T bound)
    {
        Warn.onLowerBound(rhs, bound);
        assert(0);
    }
    /// ditto
    T onUpperBound(Rhs, T)(Rhs rhs, T bound)
    {
        Warn.onUpperBound(rhs, bound);
        assert(0);
    }

    /**

    Called automatically upon a comparison for equality. In case of a erroneous
    comparison (one that would make a signed negative value appear equal to an
    unsigned positive value), this hook issues `assert(0)` which terminates the
    application.

    Params:
    lhs = The first argument of `Checked`, e.g. `int` if the left-hand side of
      the operator is `Checked!int`
    rhs = The right-hand side type involved in the operator

    Returns: Upon a correct comparison, returns the result of the comparison.
    Otherwise, the function terminates the application so it never returns.

    */
    static bool hookOpEquals(Lhs, Rhs)(Lhs lhs, Rhs rhs)
    {
        bool error;
        auto result = opChecked!"=="(lhs, rhs, error);
        if (error)
        {
            Warn.hookOpEquals(lhs, rhs);
            assert(0);
        }
        return result;
    }

    /**

    Called automatically upon a comparison for ordering using one of the
    operators `<`, `<=`, `>`, or `>=`. In case the comparison is erroneous (i.e.
    it would make a signed negative value appear greater than or equal to an
    unsigned positive value), then application is terminated with `assert(0)`.
    Otherwise, the three-state result is returned (positive if $(D lhs > rhs),
    negative if $(D lhs < rhs), `0` otherwise).

    Params:
    lhs = The first argument of `Checked`, e.g. `int` if the left-hand side of
      the operator is `Checked!int`
    rhs = The right-hand side type involved in the operator

    Returns: For correct comparisons, returns a positive integer if $(D lhs >
    rhs), a negative integer if  $(D lhs < rhs), `0` if the two are equal. Upon
    a mistaken comparison such as $(D int(-1) < uint(0)), the function never
    returns because it aborts the program.

    */
    int hookOpCmp(Lhs, Rhs)(Lhs lhs, Rhs rhs)
    {
        bool error;
        auto result = opChecked!"cmp"(lhs, rhs, error);
        if (error)
        {
            Warn.hookOpCmp(lhs, rhs);
            assert(0);
        }
        return result;
    }

    /**

    Called automatically upon an overflow during a unary or binary operation.

    Params:
    x = The operator, e.g. `-`
    lhs = The left-hand side (or sole) argument
    rhs = The right-hand side type involved in the operator

    Returns: Nominally the result is the desired value of the operator, which
    will be forwarded as result. For `Abort`, the function never returns because
    it aborts the program.

    */
    typeof(~Lhs()) onOverflow(string x, Lhs)(Lhs lhs)
    {
        Warn.onOverflow!x(lhs);
        assert(0);
    }
    /// ditto
    typeof(Lhs() + Rhs()) onOverflow(string x, Lhs, Rhs)(Lhs lhs, Rhs rhs)
    {
        Warn.onOverflow!x(lhs, rhs);
        assert(0);
    }
}

///
@safe unittest
{
    void test(T)()
    {
        Checked!(int, Abort) x;
        x = 42;
        auto x1 = cast(T) x;
        assert(x1 == 42);
        //x1 += long(int.max);
    }
    test!short;
    test!(const short);
    test!(immutable short);
}


// Throw
/**

Force all integral errors to fail by throwing an exception of type
`Throw.CheckFailure`. The message coming with the error is similar to the one
printed by `Warn`.

*/
struct Throw
{
    /**
    Exception type thrown upon any failure.
    */
    static class CheckFailure : Exception
    {
        /**
        Params:
            f    = format specifier
            vals = actual values for the format specifier
        */
        this(T...)(string f, T vals)
        {
            import std.format : format;
            super(format(f, vals));
        }
    }

    /**

    Called automatically upon a bad cast (one that loses precision or attempts
    to convert a negative value to an unsigned type). The source type is `Src`
    and the destination type is `Dst`.

    Params:
        src = source operand

    Returns:
        Nominally the result is the desired value of the cast operation,
        which will be forwarded as the result of the cast. For `Throw`, the
        function never returns because it throws an exception.

    Throws:
        `CheckFailure` on bad cast
    */
    static Dst onBadCast(Dst, Src)(Src src)
    {
        throw new CheckFailure("Erroneous cast: cast(%s) %s(%s)",
            Dst.stringof, Src.stringof, src);
    }

    /**

    Called automatically upon a bounds error.

    Params:
        rhs = The right-hand side value in the assignment, after the operator has
        been evaluated
        bound = The value of the bound being violated

    Returns:
        Nominally the result is the desired value of the operator, which
        will be forwarded as result. For `Throw`, the function never returns because
        it throws.

    Throws:
        `CheckFailure` on overflow

    */
    static T onLowerBound(Rhs, T)(Rhs rhs, T bound)
    {
        throw new CheckFailure("Lower bound error: %s(%s) < %s(%s)",
            Rhs.stringof, rhs, T.stringof, bound);
    }
    /// ditto
    static T onUpperBound(Rhs, T)(Rhs rhs, T bound)
    {
        throw new CheckFailure("Upper bound error: %s(%s) > %s(%s)",
            Rhs.stringof, rhs, T.stringof, bound);
    }

    /**

    Called automatically upon a comparison for equality. Throws upon an
    erroneous comparison (one that would make a signed negative value appear
    equal to an unsigned positive value).

    Params:
    lhs = The first argument of `Checked`, e.g. `int` if the left-hand side of
      the operator is `Checked!int`
    rhs = The right-hand side type involved in the operator

    Returns: The result of the comparison.

    Throws: `CheckFailure` if the comparison is mathematically erroneous.

    */
    static bool hookOpEquals(L, R)(L lhs, R rhs)
    {
        bool error;
        auto result = opChecked!"=="(lhs, rhs, error);
        if (error)
        {
            throw new CheckFailure("Erroneous comparison: %s(%s) == %s(%s)",
                L.stringof, lhs, R.stringof, rhs);
        }
        return result;
    }

    /**

    Called automatically upon a comparison for ordering using one of the
    operators `<`, `<=`, `>`, or `>=`. In case the comparison is erroneous (i.e.
    it would make a signed negative value appear greater than or equal to an
    unsigned positive value), throws a `Throw.CheckFailure` exception.
    Otherwise, the three-state result is returned (positive if $(D lhs > rhs),
    negative if $(D lhs < rhs), `0` otherwise).

    Params:
    lhs = The first argument of `Checked`, e.g. `int` if the left-hand side of
      the operator is `Checked!int`
    rhs = The right-hand side type involved in the operator

    Returns: For correct comparisons, returns a positive integer if $(D lhs >
    rhs), a negative integer if  $(D lhs < rhs), `0` if the two are equal.

    Throws: Upon a mistaken comparison such as $(D int(-1) < uint(0)), the
    function never returns because it throws a `Throw.CheckedFailure` exception.

    */
    static int hookOpCmp(Lhs, Rhs)(Lhs lhs, Rhs rhs)
    {
        bool error;
        auto result = opChecked!"cmp"(lhs, rhs, error);
        if (error)
        {
            throw new CheckFailure("Erroneous ordering comparison: %s(%s) and %s(%s)",
                Lhs.stringof, lhs, Rhs.stringof, rhs);
        }
        return result;
    }

    /**

    Called automatically upon an overflow during a unary or binary operation.

    Params:
        x = The operator, e.g. `-`
        lhs = The left-hand side (or sole) argument
        rhs = The right-hand side type involved in the operator

    Returns:
        Nominally the result is the desired value of the operator, which
        will be forwarded as result. For `Throw`, the function never returns because
        it throws an exception.

    Throws:
        `CheckFailure` on overflow

    */
    static typeof(~Lhs()) onOverflow(string x, Lhs)(Lhs lhs)
    {
        throw new CheckFailure("Overflow on unary operator: %s%s(%s)",
            x, Lhs.stringof, lhs);
    }
    /// ditto
    static typeof(Lhs() + Rhs()) onOverflow(string x, Lhs, Rhs)(Lhs lhs, Rhs rhs)
    {
        throw new CheckFailure("Overflow on binary operator: %s(%s) %s %s(%s)",
            Lhs.stringof, lhs, x, Rhs.stringof, rhs);
    }
}

///
@safe unittest
{
    void test(T)()
    {
        Checked!(int, Throw) x;
        x = 42;
        auto x1 = cast(T) x;
        assert(x1 == 42);
        x = T.max + 1;
        import std.exception : assertThrown, assertNotThrown;
        assertThrown(cast(T) x);
        x = x.max;
        assertThrown(x += 42);
        assertThrown(x += 42L);
        x = x.min;
        assertThrown(-x);
        assertThrown(x -= 42);
        assertThrown(x -= 42L);
        x = -1;
        assertNotThrown(x == -1);
        assertThrown(x == uint(-1));
        assertNotThrown(x <= -1);
        assertThrown(x <= uint(-1));
    }
    test!short;
    test!(const short);
    test!(immutable short);
}

// Warn
/**
Hook that prints to `stderr` a trace of all integral errors, without affecting
default behavior.
*/
struct Warn
{
    import std.stdio : writefln;
static:
    /**

    Called automatically upon a bad cast from `src` to type `Dst` (one that
    loses precision or attempts to convert a negative value to an unsigned
    type).

    Params:
    src = The source of the cast
    Dst = The target type of the cast

    Returns: `cast(Dst) src`

    */
    Dst onBadCast(Dst, Src)(Src src)
    {
        trustedStderr.writefln("Erroneous cast: cast(%s) %s(%s)",
            Dst.stringof, Src.stringof, src);
        return cast(Dst) src;
    }

    /**

    Called automatically upon a bad `opOpAssign` call (one that loses precision
    or attempts to convert a negative value to an unsigned type).

    Params:
    rhs = The right-hand side value in the assignment, after the operator has
    been evaluated
    bound = The bound being violated

    Returns: `cast(T) rhs`
    */
    T onLowerBound(Rhs, T)(Rhs rhs, T bound)
    {
        trustedStderr.writefln("Lower bound error: %s(%s) < %s(%s)",
            Rhs.stringof, rhs, T.stringof, bound);
        return cast(T) rhs;
    }
    /// ditto
    T onUpperBound(Rhs, T)(Rhs rhs, T bound)
    {
        trustedStderr.writefln("Upper bound error: %s(%s) > %s(%s)",
            Rhs.stringof, rhs, T.stringof, bound);
        return cast(T) rhs;
    }

    /**

    Called automatically upon a comparison for equality. In case of an Erroneous
    comparison (one that would make a signed negative value appear equal to an
    unsigned positive value), writes a warning message to `stderr` as a side
    effect.

    Params:
    lhs = The first argument of `Checked`, e.g. `int` if the left-hand side of
      the operator is `Checked!int`
    rhs = The right-hand side type involved in the operator

    Returns: In all cases the function returns the built-in result of $(D lhs ==
    rhs).

    */
    bool hookOpEquals(Lhs, Rhs)(Lhs lhs, Rhs rhs)
    {
        bool error;
        auto result = opChecked!"=="(lhs, rhs, error);
        if (error)
        {
            trustedStderr.writefln("Erroneous comparison: %s(%s) == %s(%s)",
                Lhs.stringof, lhs, Rhs.stringof, rhs);
            return lhs == rhs;
        }
        return result;
    }

    ///
    @safe unittest
    {
        auto x = checked!Warn(-42);
        // Passes
        assert(x == -42);
        // Passes but prints a warning
        // assert(x == uint(-42));
    }

    /**

    Called automatically upon a comparison for ordering using one of the
    operators `<`, `<=`, `>`, or `>=`. In case the comparison is erroneous (i.e.
    it would make a signed negative value appear greater than or equal to an
    unsigned positive value), then a warning message is printed to `stderr`.

    Params:
    lhs = The first argument of `Checked`, e.g. `int` if the left-hand side of
      the operator is `Checked!int`
    rhs = The right-hand side type involved in the operator

    Returns: In all cases, returns $(D lhs < rhs ? -1 : lhs > rhs). The result
    is  not autocorrected in case of an erroneous comparison.

    */
    int hookOpCmp(Lhs, Rhs)(Lhs lhs, Rhs rhs)
    {
        bool error;
        auto result = opChecked!"cmp"(lhs, rhs, error);
        if (error)
        {
            trustedStderr.writefln("Erroneous ordering comparison: %s(%s) and %s(%s)",
                Lhs.stringof, lhs, Rhs.stringof, rhs);
            return lhs < rhs ? -1 : lhs > rhs;
        }
        return result;
    }

    ///
    @safe unittest
    {
        auto x = checked!Warn(-42);
        // Passes
        assert(x <= -42);
        // Passes but prints a warning
        // assert(x <= uint(-42));
    }

    /**

    Called automatically upon an overflow during a unary or binary operation.

    Params:
        x   = The operator involved
        Lhs = The first argument of `Checked`, e.g. `int` if the left-hand side of
              the operator is `Checked!int`
        Rhs = The right-hand side type involved in the operator

    Returns:
        $(D mixin(x ~ "lhs")) for unary, $(D mixin("lhs" ~ x ~ "rhs")) for
        binary

    */
    typeof(~Lhs()) onOverflow(string x, Lhs)(ref Lhs lhs)
    {
        trustedStderr.writefln("Overflow on unary operator: %s%s(%s)",
            x, Lhs.stringof, lhs);
        return mixin(x ~ "lhs");
    }
    /// ditto
    typeof(Lhs() + Rhs()) onOverflow(string x, Lhs, Rhs)(Lhs lhs, Rhs rhs)
    {
        trustedStderr.writefln("Overflow on binary operator: %s(%s) %s %s(%s)",
            Lhs.stringof, lhs, x, Rhs.stringof, rhs);
        static if (x == "/")               // Issue 20743: mixin below would cause SIGFPE on POSIX
            return typeof(lhs / rhs).min;  // or EXCEPTION_INT_OVERFLOW on Windows
        else
            return mixin("lhs" ~ x ~ "rhs");
    }

    // This is safe because we do not assign to the reference returned by
    // `stderr`. The ability for the caller to do that is why `stderr` is not
    // safe in the general case.
    private @property auto ref trustedStderr() @trusted
    {
        import std.stdio : stderr;

        return stderr;
    }
}

///
@safe unittest
{
    auto x = checked!Warn(42);
    short x1 = cast(short) x;
    //x += long(int.max);
    auto y = checked!Warn(cast(const int) 42);
    short y1 = cast(const byte) y;
}

@system unittest
{
    auto a = checked!Warn(int.min);
    auto b = checked!Warn(-1);
    auto x = checked!Abort(int.min);
    auto y = checked!Abort(-1);

    // Temporarily redirect output to stderr to make sure we get the right output.
    import std.file : exists, remove;
    import std.process : uniqueTempPath;
    import std.stdio : stderr;
    auto tmpname = uniqueTempPath;
    scope(exit) if (exists(tmpname)) remove(tmpname);
    auto t = stderr;
    stderr.open(tmpname, "w");
    // Open a new scope to minimize code ran with stderr redirected.
    {
        scope(exit) stderr = t;
        assert(a / b == a * b);
        import std.exception : assertThrown;
        import core.exception : AssertError;
        assertThrown!AssertError(x / y);
    }
    import std.file : readText;
    import std.ascii : newline;
    auto witness = readText(tmpname);
    auto expected =
"Overflow on binary operator: int(-2147483648) / const(int)(-1)" ~ newline ~
"Overflow on binary operator: int(-2147483648) * const(int)(-1)" ~ newline ~
"Overflow on binary operator: int(-2147483648) / const(int)(-1)" ~ newline;
    assert(witness == expected, "'" ~ witness ~ "'");
}

// https://issues.dlang.org/show_bug.cgi?id=22249
@safe unittest
{
    alias _ = Warn.onLowerBound!(int, int);
}

// ProperCompare
/**

Hook that provides arithmetically correct comparisons for equality and ordering.
Comparing an object of type $(D Checked!(X, ProperCompare)) against another
integral (for equality or ordering) ensures that no surprising conversions from
signed to unsigned integral occur before the comparison. Using $(D Checked!(X,
ProperCompare)) on either side of a comparison for equality against a
floating-point number makes sure the integral can be properly converted to the
floating point type, thus making sure equality is transitive.

*/
struct ProperCompare
{
    /**
    Hook for `==` and `!=` that ensures comparison against integral values has
    the behavior expected by the usual arithmetic rules. The built-in semantics
    yield surprising behavior when comparing signed values against unsigned
    values for equality, for example $(D uint.max == -1) or $(D -1_294_967_296 ==
    3_000_000_000u). The call $(D hookOpEquals(x, y)) returns `true` if and only
    if `x` and `y` represent the same arithmetic number.

    If one of the numbers is an integral and the other is a floating-point
    number, $(D hookOpEquals(x, y)) returns `true` if and only if the integral
    can be converted exactly (without approximation) to the floating-point
    number. This is in order to preserve transitivity of equality: if $(D
    hookOpEquals(x, y)) and $(D hookOpEquals(y, z)) then $(D hookOpEquals(y,
    z)), in case `x`, `y`, and `z` are a mix of integral and floating-point
    numbers.

    Params:
    lhs = The left-hand side of the comparison for equality
    rhs = The right-hand side of the comparison for equality

    Returns:
    The result of the comparison, `true` if the values are equal
    */
    static bool hookOpEquals(L, R)(L lhs, R rhs)
    {
        alias C = typeof(lhs + rhs);
        static if (isFloatingPoint!C)
        {
            static if (!isFloatingPoint!L)
            {
                return hookOpEquals(rhs, lhs);
            }
            else static if (!isFloatingPoint!R)
            {
                static assert(isFloatingPoint!L && !isFloatingPoint!R);
                auto rhs1 = C(rhs);
                return lhs == rhs1 && cast(R) rhs1 == rhs;
            }
            else
                return lhs == rhs;
        }
        else
        {
            bool error;
            auto result = opChecked!"=="(lhs, rhs, error);
            if (error)
            {
                // Only possible error is a wrong "true"
                return false;
            }
            return result;
        }
    }

    /**
    Hook for `<`, `<=`, `>`, and `>=` that ensures comparison against integral
    values has the behavior expected by the usual arithmetic rules. The built-in
    semantics yield surprising behavior when comparing signed values against
    unsigned values, for example $(D 0u < -1). The call $(D hookOpCmp(x, y))
    returns `-1` if and only if `x` is smaller than `y` in abstract arithmetic
    sense.

    If one of the numbers is an integral and the other is a floating-point
    number, $(D hookOpEquals(x, y)) returns a floating-point number that is `-1`
    if `x < y`, `0` if `x == y`, `1` if `x > y`, and `NaN` if the floating-point
    number is `NaN`.

    Params:
    lhs = The left-hand side of the comparison for ordering
    rhs = The right-hand side of the comparison for ordering

    Returns:
    The result of the comparison (negative if $(D lhs < rhs), positive if $(D
    lhs > rhs), `0` if the values are equal)
    */
    static auto hookOpCmp(L, R)(L lhs, R rhs)
    {
        alias C = typeof(lhs + rhs);
        static if (isFloatingPoint!C)
        {
            return lhs < rhs
                ? C(-1)
                : lhs > rhs ? C(1) : lhs == rhs ? C(0) : C.init;
        }
        else
        {
            static if (!valueConvertible!(L, C) || !valueConvertible!(R, C))
            {
                static assert(isUnsigned!C);
                static assert(isUnsigned!L != isUnsigned!R);
                if (!isUnsigned!L && lhs < 0)
                    return -1;
                if (!isUnsigned!R && rhs < 0)
                    return 1;
            }
            return lhs < rhs ? -1 : lhs > rhs;
        }
    }
}

///
@safe unittest
{
    alias opEqualsProper = ProperCompare.hookOpEquals;
    assert(opEqualsProper(42, 42));
    assert(opEqualsProper(42.0, 42.0));
    assert(opEqualsProper(42u, 42));
    assert(opEqualsProper(42, 42u));
    assert(-1 == 4294967295u);
    assert(!opEqualsProper(-1, 4294967295u));
    assert(!opEqualsProper(const uint(-1), -1));
    assert(!opEqualsProper(uint(-1), -1.0));
    assert(3_000_000_000U == -1_294_967_296);
    assert(!opEqualsProper(3_000_000_000U, -1_294_967_296));
}

@safe unittest
{
    alias opCmpProper = ProperCompare.hookOpCmp;
    assert(opCmpProper(42, 42) == 0);
    assert(opCmpProper(42, 42.0) == 0);
    assert(opCmpProper(41, 42.0) < 0);
    assert(opCmpProper(42, 41.0) > 0);
    import std.math.traits : isNaN;
    assert(isNaN(opCmpProper(41, double.init)));
    assert(opCmpProper(42u, 42) == 0);
    assert(opCmpProper(42, 42u) == 0);
    assert(opCmpProper(-1, uint(-1)) < 0);
    assert(opCmpProper(uint(-1), -1) > 0);
    assert(opCmpProper(-1.0, -1) == 0);
}

@safe unittest
{
    auto x1 = Checked!(uint, ProperCompare)(42u);
    assert(x1.get < -1);
    assert(x1 > -1);
}

// WithNaN
/**

Hook that reserves a special value as a "Not a Number" representative. For
signed integrals, the reserved value is `T.min`. For signed integrals, the
reserved value is `T.max`.

The default value of a $(D Checked!(X, WithNaN)) is its NaN value, so care must
be taken that all variables are explicitly initialized. Any arithmetic and logic
operation involving at least on NaN becomes NaN itself. All of $(D a == b), $(D
a < b), $(D a > b), $(D a <= b), $(D a >= b) yield `false` if at least one of
`a` and `b` is NaN.

*/
struct WithNaN
{
static:
    /**
    The default value used for values not explicitly initialized. It is the NaN
    value, i.e. `T.min` for signed integrals and `T.max` for unsigned integrals.
    */
    enum T defaultValue(T) = T.min == 0 ? T.max : T.min;
    /**
    The maximum value representable is `T.max` for signed integrals, $(D
    T.max - 1) for unsigned integrals. The minimum value representable is $(D
    T.min + 1) for signed integrals, `0` for unsigned integrals.
    */
    enum T max(T) = cast(T) (T.min == 0 ? T.max - 1 : T.max);
    /// ditto
    enum T min(T) = cast(T) (T.min == 0 ? T(0) : T.min + 1);

    /**
    If `rhs` is `WithNaN.defaultValue!Rhs`, returns
    `WithNaN.defaultValue!Lhs`. Otherwise, returns $(D cast(Lhs) rhs).

    Params:
    rhs = the value being cast (`Rhs` is the first argument to `Checked`)
    Lhs = the target type of the cast

    Returns: The result of the cast operation.
    */
    Lhs hookOpCast(Lhs, Rhs)(Rhs rhs)
    {
        static if (is(Lhs == bool))
        {
            return rhs != defaultValue!Rhs && rhs != 0;
        }
        else static if (valueConvertible!(Rhs, Lhs))
        {
            return rhs != defaultValue!Rhs ? Lhs(rhs) : defaultValue!Lhs;
        }
        else
        {
            // Not value convertible, only viable option is rhs fits within the
            // bounds of Lhs
            static if (ProperCompare.hookOpCmp(Rhs.min, Lhs.min) < 0)
            {
                // Example: hookOpCast!short(int(42)), hookOpCast!uint(int(42))
                if (ProperCompare.hookOpCmp(rhs, Lhs.min) < 0)
                    return defaultValue!Lhs;
            }
            static if (ProperCompare.hookOpCmp(Rhs.max, Lhs.max) > 0)
            {
                // Example: hookOpCast!int(uint(42))
                if (ProperCompare.hookOpCmp(rhs, Lhs.max) > 0)
                    return defaultValue!Lhs;
            }
            return cast(Lhs) rhs;
        }
    }

    ///
    @safe unittest
    {
        auto x = checked!WithNaN(422);
        assert((cast(ubyte) x) == 255);
        x = checked!WithNaN(-422);
        assert((cast(byte) x) == -128);
        assert(cast(short) x == -422);
        assert(cast(bool) x);
        x = x.init; // set back to NaN
        assert(x != true);
        assert(x != false);
    }

    /**

    Returns `false` if $(D lhs == WithNaN.defaultValue!Lhs), $(D lhs == rhs)
    otherwise.

    Params:
    lhs = The left-hand side of the comparison (`Lhs` is the first argument to
    `Checked`)
    rhs = The right-hand side of the comparison

    Returns: `lhs != WithNaN.defaultValue!Lhs && lhs == rhs`
    */
    bool hookOpEquals(Lhs, Rhs)(Lhs lhs, Rhs rhs)
    {
        return lhs != defaultValue!Lhs && lhs == rhs;
    }

    /**

    If $(D lhs == WithNaN.defaultValue!Lhs), returns `double.init`. Otherwise,
    has the same semantics as the default comparison.

    Params:
    lhs = The left-hand side of the comparison (`Lhs` is the first argument to
    `Checked`)
    rhs = The right-hand side of the comparison

    Returns: `double.init` if `lhs == WitnNaN.defaultValue!Lhs`, `-1.0` if $(D
    lhs < rhs), `0.0` if $(D lhs == rhs), `1.0` if $(D lhs > rhs).

    */
    double hookOpCmp(Lhs, Rhs)(Lhs lhs, Rhs rhs)
    {
        if (lhs == defaultValue!Lhs) return double.init;
        return lhs < rhs
            ? -1.0
            : lhs > rhs ? 1.0 : lhs == rhs ? 0.0 : double.init;
    }

    ///
    @safe unittest
    {
        Checked!(int, WithNaN) x;
        assert(!(x < 0) && !(x > 0) && !(x == 0));
        x = 1;
        assert(x > 0 && !(x < 0) && !(x == 0));
    }

    /**
    Defines hooks for unary operators `-`, `~`, `++`, and `--`.

    For `-` and `~`, if $(D v == WithNaN.defaultValue!T), returns
    `WithNaN.defaultValue!T`. Otherwise, the semantics is the same as for the
    built-in operator.

    For `++` and `--`, if $(D v == WithNaN.defaultValue!Lhs) or the operation
    would result in an overflow, sets `v` to `WithNaN.defaultValue!T`.
    Otherwise, the semantics is the same as for the built-in operator.

    Params:
    x = The operator symbol
    v = The left-hand side of the comparison (`T` is the first argument to
    `Checked`)

    Returns: $(UL $(LI For $(D x == "-" || x == "~"): If  $(D v ==
    WithNaN.defaultValue!T), the function returns `WithNaN.defaultValue!T`.
    Otherwise it returns the normal result of the operator.) $(LI For $(D x ==
    "++" || x == "--"): The function returns `void`.))

    */
    auto hookOpUnary(string x, T)(ref T v)
    {
        static if (x == "-" || x == "~")
        {
            return v != defaultValue!T ? mixin(x ~ "v") : v;
        }
        else static if (x == "++")
        {
            static if (defaultValue!T == T.min)
            {
                if (v != defaultValue!T)
                {
                    if (v == T.max) v = defaultValue!T;
                    else ++v;
                }
            }
            else
            {
                static assert(defaultValue!T == T.max);
                if (v != defaultValue!T) ++v;
            }
        }
        else static if (x == "--")
        {
            if (v != defaultValue!T) --v;
        }
    }

    ///
    @safe unittest
    {
        Checked!(int, WithNaN) x;
        ++x;
        assert(x.isNaN);
        x = 1;
        assert(!x.isNaN);
        x = -x;
        ++x;
        assert(!x.isNaN);
    }

    @safe unittest // for coverage
    {
        Checked!(uint, WithNaN) y;
        ++y;
        assert(y.isNaN);
    }

    /**
    Defines hooks for binary operators `+`, `-`, `*`, `/`, `%`, `^^`, `&`, `|`,
     `^`, `<<`, `>>`, and `>>>` for cases where a `Checked` object is the
    left-hand side operand. If $(D lhs == WithNaN.defaultValue!Lhs), returns
    $(D WithNaN.defaultValue!(typeof(lhs + rhs))) without evaluating the
    operand. Otherwise, evaluates the operand. If evaluation does not overflow,
    returns the result. Otherwise, returns $(D WithNaN.defaultValue!(typeof(lhs +
    rhs))).

    Params:
    x = The operator symbol
    lhs = The left-hand side operand (`Lhs` is the first argument to `Checked`)
    rhs = The right-hand side operand

    Returns: If $(D lhs != WithNaN.defaultValue!Lhs) and the operator does not
    overflow, the function returns the same result as the built-in operator. In
    all other cases, returns $(D WithNaN.defaultValue!(typeof(lhs + rhs))).
    */
    auto hookOpBinary(string x, L, R)(L lhs, R rhs)
    {
        alias Result = typeof(lhs + rhs);
        if (lhs != defaultValue!L)
        {
            bool error;
            auto result = opChecked!x(lhs, rhs, error);
            if (!error) return result;
        }
        return defaultValue!Result;
    }

    ///
    @safe unittest
    {
        Checked!(int, WithNaN) x;
        assert((x + 1).isNaN);
        x = 100;
        assert(!(x + 1).isNaN);
    }

    /**
    Defines hooks for binary operators `+`, `-`, `*`, `/`, `%`, `^^`, `&`, `|`,
     `^`, `<<`, `>>`, and `>>>` for cases where a `Checked` object is the
    right-hand side operand. If $(D rhs == WithNaN.defaultValue!Rhs), returns
    $(D WithNaN.defaultValue!(typeof(lhs + rhs))) without evaluating the
    operand. Otherwise, evaluates the operand. If evaluation does not overflow,
    returns the result. Otherwise, returns $(D WithNaN.defaultValue!(typeof(lhs +
    rhs))).

    Params:
    x = The operator symbol
    lhs = The left-hand side operand
    rhs = The right-hand side operand (`Rhs` is the first argument to `Checked`)

    Returns: If $(D rhs != WithNaN.defaultValue!Rhs) and the operator does not
    overflow, the function returns the same result as the built-in operator. In
    all other cases, returns $(D WithNaN.defaultValue!(typeof(lhs + rhs))).
    */
    auto hookOpBinaryRight(string x, L, R)(L lhs, R rhs)
    {
        alias Result = typeof(lhs + rhs);
        if (rhs != defaultValue!R)
        {
            bool error;
            auto result = opChecked!x(lhs, rhs, error);
            if (!error) return result;
        }
        return defaultValue!Result;
    }
    ///
    @safe unittest
    {
        Checked!(int, WithNaN) x;
        assert((1 + x).isNaN);
        x = 100;
        assert(!(1 + x).isNaN);
    }

    /**

    Defines hooks for binary operators `+=`, `-=`, `*=`, `/=`, `%=`, `^^=`,
    `&=`, `|=`, `^=`, `<<=`, `>>=`, and `>>>=` for cases where a `Checked`
    object is the left-hand side operand. If $(D lhs ==
    WithNaN.defaultValue!Lhs), no action is carried. Otherwise, evaluates the
    operand. If evaluation does not overflow and fits in `Lhs` without loss of
    information or change of sign, sets `lhs` to the result. Otherwise, sets
    `lhs` to `WithNaN.defaultValue!Lhs`.

    Params:
    x = The operator symbol (without the `=`)
    lhs = The left-hand side operand (`Lhs` is the first argument to `Checked`)
    rhs = The right-hand side operand

    Returns: `void`
    */
    void hookOpOpAssign(string x, L, R)(ref L lhs, R rhs)
    {
        if (lhs == defaultValue!L)
            return;
        bool error;
        auto temp = opChecked!x(lhs, rhs, error);
        lhs = error
            ? defaultValue!L
            : hookOpCast!L(temp);
    }

    ///
    @safe unittest
    {
        Checked!(int, WithNaN) x;
        x += 4;
        assert(x.isNaN);
        x = 0;
        x += 4;
        assert(!x.isNaN);
        x += int.max;
        assert(x.isNaN);
    }
}

///
@safe unittest
{
    auto x1 = Checked!(int, WithNaN)();
    assert(x1.isNaN);
    assert(x1.get == int.min);
    assert(x1 != x1);
    assert(!(x1 < x1));
    assert(!(x1 > x1));
    assert(!(x1 == x1));
    ++x1;
    assert(x1.isNaN);
    assert(x1.get == int.min);
    --x1;
    assert(x1.isNaN);
    assert(x1.get == int.min);
    x1 = 42;
    assert(!x1.isNaN);
    assert(x1 == x1);
    assert(x1 <= x1);
    assert(x1 >= x1);
    static assert(x1.min == int.min + 1);
    x1 += long(int.max);
}

/**
Queries whether a $(D Checked!(T, WithNaN)) object is not a number (NaN).

Params:
    x = the `Checked` instance queried

Returns:
    `true` if `x` is a NaN, `false` otherwise
*/
bool isNaN(T)(const Checked!(T, WithNaN) x)
{
    return x.get == x.init.get;
}

///
@safe unittest
{
    auto x1 = Checked!(int, WithNaN)();
    assert(x1.isNaN);
    x1 = 1;
    assert(!x1.isNaN);
    x1 = x1.init;
    assert(x1.isNaN);
}

@safe unittest
{
    void test1(T)()
    {
        auto x1 = Checked!(T, WithNaN)();
        assert(x1.isNaN);
        assert(x1.get == int.min);
        assert(x1 != x1);
        assert(!(x1 < x1));
        assert(!(x1 > x1));
        assert(!(x1 == x1));
        assert(x1.get == int.min);
        auto x2 = Checked!(T, WithNaN)(42);
        assert(!x2.isNaN);
        assert(x2 == x2);
        assert(x2 <= x2);
        assert(x2 >= x2);
        static assert(x2.min == T.min + 1);
    }
    test1!int;
    test1!(const int);
    test1!(immutable int);

    void test2(T)()
    {
        auto x1 = Checked!(T, WithNaN)();
        assert(x1.get == T.min);
        assert(x1 != x1);
        assert(!(x1 < x1));
        assert(!(x1 > x1));
        assert(!(x1 == x1));
        ++x1;
        assert(x1.get == T.min);
        --x1;
        assert(x1.get == T.min);
        x1 = 42;
        assert(x1 == x1);
        assert(x1 <= x1);
        assert(x1 >= x1);
        static assert(x1.min == T.min + 1);
        x1 += long(T.max);
    }
    test2!int;
}

@safe unittest
{
    alias Smart(T) = Checked!(Checked!(T, ProperCompare), WithNaN);
    Smart!int x1;
    assert(x1 != x1);
    x1 = -1;
    assert(x1 < 1u);
    auto x2 = Smart!(const int)(42);
}

// Saturate
/**

Hook that implements $(I saturation), i.e. any arithmetic operation that would
overflow leaves the result at its extreme value (`min` or `max` depending on the
direction of the overflow).

Saturation is not sticky; if a value reaches its saturation value, another
operation may take it back to normal range.

*/
struct Saturate
{
static:
    /**

    Implements saturation for operators `+=`, `-=`, `*=`, `/=`, `%=`, `^^=`, `&=`, `|=`, `^=`, `<<=`, `>>=`,
    and `>>>=`. This hook is called if the result of the binary operation does
    not fit in `Lhs` without loss of information or a change in sign.

    Params:
    Rhs = The right-hand side type in the assignment, after the operation has
    been computed
    bound = The bound being violated

    Returns: `Lhs.max` if $(D rhs >= 0), `Lhs.min` otherwise.

    */
    T onLowerBound(Rhs, T)(Rhs, T bound)
    {
        return bound;
    }
    /// ditto
    T onUpperBound(Rhs, T)(Rhs, T bound)
    {
        return bound;
    }
    ///
    @safe unittest
    {
        auto x = checked!Saturate(short(100));
        x += 33000;
        assert(x == short.max);
        x -= 70000;
        assert(x == short.min);
    }

    /**

    Implements saturation for operators `+`, `-` (unary and binary), `*`, `/`,
    `%`, `^^`, `&`, `|`, `^`, `<<`, `>>`, and `>>>`.

    For unary `-`, `onOverflow` is called if $(D lhs == Lhs.min) and `Lhs` is a
    signed type. The function returns `Lhs.max`.

    For binary operators, the result is as follows: $(UL $(LI `Lhs.max` if the
    result overflows in the positive direction, on division by `0`, or on
    shifting right by a negative value) $(LI `Lhs.min` if the result overflows
    in the negative direction) $(LI `0` if `lhs` is being shifted left by a
    negative value, or shifted right by a large positive value))

    Params:
        x   = The operator involved in the `opAssign` operation
        Lhs = The left-hand side type of the operator (`Lhs` is the first argument to
              `Checked`)
        Rhs = The right-hand side type in the operator

    Returns: The saturated result of the operator.

    */
    auto onOverflow(string x, Lhs)(Lhs)
    {
        static assert(x == "-" || x == "++" || x == "--");
        return x == "--" ? Lhs.min : Lhs.max;
    }
    /// ditto
    typeof(Lhs() + Rhs()) onOverflow(string x, Lhs, Rhs)(Lhs lhs, Rhs rhs)
    {
        static if (x == "+")
            return rhs >= 0 ? Lhs.max : Lhs.min;
        else static if (x == "*")
            return (lhs >= 0) == (rhs >= 0) ? Lhs.max : Lhs.min;
        else static if (x == "^^")
            return lhs > 0 || !(rhs & 1) ? Lhs.max : Lhs.min;
        else static if (x == "-")
            return rhs >= 0 ? Lhs.min : Lhs.max;
        else static if (x == "/" || x == "%")
            return Lhs.max;
        else static if (x == "<<")
            return rhs >= 0 ? Lhs.max : 0;
        else static if (x == ">>" || x == ">>>")
            return rhs >= 0 ? 0 : Lhs.max;
        else
            static assert(false);
    }
    ///
    @safe unittest
    {
        assert(checked!Saturate(int.max) + 1 == int.max);
        assert(checked!Saturate(100) ^^ 10 == int.max);
        assert(checked!Saturate(-100) ^^ 10 == int.max);
        assert(checked!Saturate(100) / 0 == int.max);
        assert(checked!Saturate(100) << -1 == 0);
        assert(checked!Saturate(100) << 33 == int.max);
        assert(checked!Saturate(100) >> -1 == int.max);
        assert(checked!Saturate(100) >> 33 == 0);
    }
}

///
@safe unittest
{
    auto x = checked!Saturate(int.max);
    ++x;
    assert(x == int.max);
    --x;
    assert(x == int.max - 1);
    x = int.min;
    assert(-x == int.max);
    x -= 42;
    assert(x == int.min);
    assert(x * -2 == int.max);
}

/*
Yields `true` if `T1` is "value convertible" (by C's "value preserving" rule,
see $(HTTP c-faq.com/expr/preservingrules.html)) to `T2`, where the two are
integral types. That is, all of values in `T1` are also in `T2`. For example
`int` is value convertible to `long` but not to `uint` or `ulong`.
*/
private enum valueConvertible(T1, T2) = isIntegral!T1 && isIntegral!T2 &&
    is(T1 : T2) && (
        isUnsigned!T1 == isUnsigned!T2 || // same signedness
        !isUnsigned!T2 && T2.sizeof > T1.sizeof // safely convertible
    );

/**

Defines binary operations with overflow checking for any two integral types.
The result type obeys the language rules (even when they may be
counterintuitive), and `overflow` is set if an overflow occurs (including
inadvertent change of signedness, e.g. `-1` is converted to `uint`).
Conceptually the behavior is:

$(OL $(LI Perform the operation in infinite precision)
$(LI If the infinite-precision result fits in the result type, return it and
do not touch `overflow`)
$(LI Otherwise, set `overflow` to `true` and return an unspecified value)
)

The implementation exploits properties of types and operations to minimize
additional work.

Params:
x = The binary operator involved, e.g. `/`
lhs = The left-hand side of the operator
rhs = The right-hand side of the operator
overflow = The overflow indicator (assigned `true` in case there's an error)

Returns:
The result of the operation, which is the same as the built-in operator
*/
typeof(mixin(x == "cmp" ? "0" : ("L() " ~ x ~ " R()")))
opChecked(string x, L, R)(const L lhs, const R rhs, ref bool overflow)
if (isIntegral!L && isIntegral!R)
{
    static if (x == "cmp")
        alias Result = int;
    else
        alias Result = typeof(mixin("L() " ~ x ~ " R()"));

    import core.checkedint : addu, adds, subs, muls, subu, mulu;
    import std.algorithm.comparison : among;
    static if (x == "==")
    {
        alias C = typeof(lhs + rhs);
        static if (valueConvertible!(L, C) && valueConvertible!(R, C))
        {
            // Values are converted to R before comparison, cool.
            return lhs == rhs;
        }
        else
        {
            static assert(isUnsigned!C);
            static assert(isUnsigned!L != isUnsigned!R);
            if (lhs != rhs) return false;
            // R(lhs) and R(rhs) have the same bit pattern, yet may be
            // different due to signedness change.
            static if (!isUnsigned!R)
            {
                if (rhs >= 0)
                    return true;
            }
            else
            {
                if (lhs >= 0)
                    return true;
            }
            overflow = true;
            return true;
        }
    }
    else static if (x == "cmp")
    {
        alias C = typeof(lhs + rhs);
        static if (!valueConvertible!(L, C) || !valueConvertible!(R, C))
        {
            static assert(isUnsigned!C);
            static assert(isUnsigned!L != isUnsigned!R);
            if (!isUnsigned!L && lhs < 0)
            {
                overflow = true;
                return -1;
            }
            if (!isUnsigned!R && rhs < 0)
            {
                overflow = true;
                return 1;
            }
        }
        return lhs < rhs ? -1 : lhs > rhs;
    }
    else static if (x.among("<<", ">>", ">>>"))
    {
        // Handle shift separately from all others. The test below covers
        // negative rhs as well.
        import std.conv : unsigned;
        if (unsigned(rhs) > 8 * Result.sizeof) goto fail;
        return mixin("lhs" ~ x ~ "rhs");
    }
    else static if (x.among("&", "|", "^"))
    {
        // Nothing to check
        return mixin("lhs" ~ x ~ "rhs");
    }
    else static if (x == "^^")
    {
        // Exponentiation is weird, handle separately
        return pow(lhs, rhs, overflow);
    }
    else static if (valueConvertible!(L, Result) &&
            valueConvertible!(R, Result))
    {
        static if (L.sizeof < Result.sizeof && R.sizeof < Result.sizeof &&
            x.among("+", "-", "*"))
        {
            // No checks - both are value converted and result is in range
            return mixin("lhs" ~ x ~ "rhs");
        }
        else static if (x == "+")
        {
            static if (isUnsigned!Result) alias impl = addu;
            else alias impl = adds;
            return impl(Result(lhs), Result(rhs), overflow);
        }
        else static if (x == "-")
        {
            static if (isUnsigned!Result) alias impl = subu;
            else alias impl = subs;
            return impl(Result(lhs), Result(rhs), overflow);
        }
        else static if (x == "*")
        {
            static if (!isUnsigned!L && !isUnsigned!R &&
                is(L == Result))
            {
                if (lhs == Result.min && rhs == -1) goto fail;
            }
            static if (isUnsigned!Result) alias impl = mulu;
            else alias impl = muls;
            return impl(Result(lhs), Result(rhs), overflow);
        }
        else static if (x == "/" || x == "%")
        {
            static if (!isUnsigned!L && !isUnsigned!R &&
                is(L == Result) && x == "/")
            {
                if (lhs == Result.min && rhs == -1) goto fail;
            }
            if (rhs == 0) goto fail;
            return mixin("lhs" ~ x ~ "rhs");
        }
        else static assert(0, x);
    }
    else // Mixed signs
    {
        static assert(isUnsigned!Result);
        static assert(isUnsigned!L != isUnsigned!R);
        static if (x == "+")
        {
            static if (!isUnsigned!L)
            {
                if (lhs < 0)
                    return subu(Result(rhs), Result(-lhs), overflow);
            }
            else static if (!isUnsigned!R)
            {
                if (rhs < 0)
                    return subu(Result(lhs), Result(-rhs), overflow);
            }
            return addu(Result(lhs), Result(rhs), overflow);
        }
        else static if (x == "-")
        {
            static if (!isUnsigned!L)
            {
                if (lhs < 0) goto fail;
            }
            else static if (!isUnsigned!R)
            {
                if (rhs < 0)
                    return addu(Result(lhs), Result(-rhs), overflow);
            }
            return subu(Result(lhs), Result(rhs), overflow);
        }
        else static if (x == "*")
        {
            static if (!isUnsigned!L)
            {
                if (lhs < 0) goto fail;
            }
            else static if (!isUnsigned!R)
            {
                if (rhs < 0) goto fail;
            }
            return mulu(Result(lhs), Result(rhs), overflow);
        }
        else static if (x == "/" || x == "%")
        {
            static if (!isUnsigned!L)
            {
                if (lhs < 0 || rhs == 0) goto fail;
            }
            else static if (!isUnsigned!R)
            {
                if (rhs <= 0) goto fail;
            }
            return mixin("Result(lhs)" ~ x ~ "Result(rhs)");
        }
        else static assert(0, x);
    }
    debug assert(false);
fail:
    overflow = true;
    return Result(0);
}

///
@safe unittest
{
    bool overflow;
    assert(opChecked!"+"(const short(1), short(1), overflow) == 2 && !overflow);
    assert(opChecked!"+"(1, 1, overflow) == 2 && !overflow);
    assert(opChecked!"+"(1, 1u, overflow) == 2 && !overflow);
    assert(opChecked!"+"(-1, 1u, overflow) == 0 && !overflow);
    assert(opChecked!"+"(1u, -1, overflow) == 0 && !overflow);
}

///
@safe unittest
{
    bool overflow;
    assert(opChecked!"-"(1, 1, overflow) == 0 && !overflow);
    assert(opChecked!"-"(1, 1u, overflow) == 0 && !overflow);
    assert(opChecked!"-"(1u, -1, overflow) == 2 && !overflow);
    assert(opChecked!"-"(-1, 1u, overflow) == 0 && overflow);
}

@safe unittest
{
    bool overflow;
    assert(opChecked!"*"(2, 3, overflow) == 6 && !overflow);
    assert(opChecked!"*"(2, 3u, overflow) == 6 && !overflow);
    assert(opChecked!"*"(1u, -1, overflow) == 0 && overflow);
    //assert(mul(-1, 1u, overflow) == uint.max - 1 && overflow);
}

@safe unittest
{
    bool overflow;
    assert(opChecked!"/"(6, 3, overflow) == 2 && !overflow);
    assert(opChecked!"/"(6, 3, overflow) == 2 && !overflow);
    assert(opChecked!"/"(6u, 3, overflow) == 2 && !overflow);
    assert(opChecked!"/"(6, 3u, overflow) == 2 && !overflow);
    assert(opChecked!"/"(11, 0, overflow) == 0 && overflow);
    overflow = false;
    assert(opChecked!"/"(6u, 0, overflow) == 0 && overflow);
    overflow = false;
    assert(opChecked!"/"(-6, 2u, overflow) == 0 && overflow);
    overflow = false;
    assert(opChecked!"/"(-6, 0u, overflow) == 0 && overflow);
    overflow = false;
    assert(opChecked!"cmp"(0u, -6, overflow) == 1 && overflow);
    overflow = false;
    assert(opChecked!"|"(1, 2, overflow) == 3 && !overflow);
}

/*
Exponentiation function used by the implementation of operator `^^`.
*/
private pure @safe nothrow @nogc
auto pow(L, R)(const L lhs, const R rhs, ref bool overflow)
if (isIntegral!L && isIntegral!R)
{
    if (rhs <= 1)
    {
        if (rhs == 0) return 1;
        static if (!isUnsigned!R)
            return rhs == 1
                ? lhs
                : (rhs == -1 && (lhs == 1 || lhs == -1)) ? lhs : 0;
        else
            return lhs;
    }

    typeof(lhs ^^ rhs) b = void;
    static if (!isUnsigned!L && isUnsigned!(typeof(b)))
    {
        // Need to worry about mixed-sign stuff
        if (lhs < 0)
        {
            if (rhs & 1)
            {
                if (lhs < 0) overflow = true;
                return 0;
            }
            b = -lhs;
        }
        else
        {
            b = lhs;
        }
    }
    else
    {
        b = lhs;
    }
    if (b == 1) return 1;
    if (b == -1) return (rhs & 1) ? -1 : 1;
    if (rhs > 63)
    {
        overflow = true;
        return 0;
    }

    assert((b > 1 || b < -1) && rhs > 1);
    return powImpl(b, cast(uint) rhs, overflow);
}

// Inspiration: http://www.stepanovpapers.com/PAM.pdf
pure @safe nothrow @nogc
private T powImpl(T)(T b, uint e, ref bool overflow)
if (isIntegral!T && T.sizeof >= 4)
{
    assert(e > 1);

    import core.checkedint : muls, mulu;
    static if (isUnsigned!T) alias mul = mulu;
    else alias mul = muls;

    T r = b;
    --e;
    // Loop invariant: r * (b ^^ e) is the actual result
    for (;; e /= 2)
    {
        if (e % 2)
        {
            r = mul(r, b, overflow);
            if (e == 1) break;
        }
        b = mul(b, b, overflow);
    }
    return r;
}

@safe unittest
{
    static void testPow(T)(T x, uint e)
    {
        bool overflow;
        assert(opChecked!"^^"(T(0), 0, overflow) == 1);
        assert(opChecked!"^^"(-2, T(0), overflow) == 1);
        assert(opChecked!"^^"(-2, T(1), overflow) == -2);
        assert(opChecked!"^^"(-1, -1, overflow) == -1);
        assert(opChecked!"^^"(-2, 1, overflow) == -2);
        assert(opChecked!"^^"(-2, -1, overflow) == 0);
        assert(opChecked!"^^"(-2, 4u, overflow) == 16);
        assert(!overflow);
        assert(opChecked!"^^"(-2, 3u, overflow) == 0);
        assert(overflow);
        overflow = false;
        assert(opChecked!"^^"(3, 64u, overflow) == 0);
        assert(overflow);
        overflow = false;
        foreach (uint i; 0 .. e)
        {
            assert(opChecked!"^^"(x, i, overflow) == x ^^ i);
            assert(!overflow);
        }
        assert(opChecked!"^^"(x, e, overflow) == x ^^ e);
        assert(overflow);
    }

    testPow!int(3, 21);
    testPow!uint(3, 21);
    testPow!long(3, 40);
    testPow!ulong(3, 41);
}

version (StdUnittest) private struct CountOverflows
{
    uint calls;
    auto onOverflow(string op, Lhs)(Lhs lhs)
    {
        ++calls;
        return mixin(op ~ "lhs");
    }
    auto onOverflow(string op, Lhs, Rhs)(Lhs lhs, Rhs rhs)
    {
        ++calls;
        return mixin("lhs" ~ op ~ "rhs");
    }
    T onLowerBound(Rhs, T)(Rhs rhs, T)
    {
        ++calls;
        return cast(T) rhs;
    }
    T onUpperBound(Rhs, T)(Rhs rhs, T)
    {
        ++calls;
        return cast(T) rhs;
    }
}

// opBinary
@nogc nothrow pure @safe unittest
{
    static struct CountOpBinary
    {
        uint calls;
        auto hookOpBinary(string op, Lhs, Rhs)(Lhs lhs, Rhs rhs)
        {
            ++calls;
            return mixin("lhs" ~ op ~ "rhs");
        }
    }
    auto x = Checked!(const int, void)(42), y = Checked!(immutable int, void)(142);
    assert(x + y == 184);
    assert(x + 100 == 142);
    assert(y - x == 100);
    assert(200 - x == 158);
    assert(y * x == 142 * 42);
    assert(x / 1 == 42);
    assert(x % 20 == 2);

    auto x1 = Checked!(int, CountOverflows)(42);
    assert(x1 + 0 == 42);
    assert(x1 + false == 42);
    assert(is(typeof(x1 + 0.5) == double));
    assert(x1 + 0.5 == 42.5);
    assert(x1.hook.calls == 0);
    assert(x1 + int.max == int.max + 42);
    assert(x1.hook.calls == 1);
    assert(x1 * 2 == 84);
    assert(x1.hook.calls == 1);
    assert(x1 / 2 == 21);
    assert(x1.hook.calls == 1);
    assert(x1 % 20 == 2);
    assert(x1.hook.calls == 1);
    assert(x1 << 2 == 42 << 2);
    assert(x1.hook.calls == 1);
    assert(x1 << 42 == x1.get << x1.get);
    assert(x1.hook.calls == 2);
    x1 = int.min;
    assert(x1 - 1 == int.max);
    assert(x1.hook.calls == 3);

    auto x2 = Checked!(int, CountOpBinary)(42);
    assert(x2 + 1 == 43);
    assert(x2.hook.calls == 1);

    auto x3 = Checked!(uint, CountOverflows)(42u);
    assert(x3 + 1 == 43);
    assert(x3.hook.calls == 0);
    assert(x3 - 1 == 41);
    assert(x3.hook.calls == 0);
    assert(x3 + (-42) == 0);
    assert(x3.hook.calls == 0);
    assert(x3 - (-42) == 84);
    assert(x3.hook.calls == 0);
    assert(x3 * 2 == 84);
    assert(x3.hook.calls == 0);
    assert(x3 * -2 == -84);
    assert(x3.hook.calls == 1);
    assert(x3 / 2 == 21);
    assert(x3.hook.calls == 1);
    assert(x3 / -2 == 0);
    assert(x3.hook.calls == 2);
    assert(x3 ^^ 2 == 42 * 42);
    assert(x3.hook.calls == 2);

    auto x4 = Checked!(int, CountOverflows)(42);
    assert(x4 + 1 == 43);
    assert(x4.hook.calls == 0);
    assert(x4 + 1u == 43);
    assert(x4.hook.calls == 0);
    assert(x4 - 1 == 41);
    assert(x4.hook.calls == 0);
    assert(x4 * 2 == 84);
    assert(x4.hook.calls == 0);
    x4 = -2;
    assert(x4 + 2u == 0);
    assert(x4.hook.calls == 0);
    assert(x4 * 2u == -4);
    assert(x4.hook.calls == 1);

    auto x5 = Checked!(int, CountOverflows)(3);
    assert(x5 ^^ 0 == 1);
    assert(x5 ^^ 1 == 3);
    assert(x5 ^^ 2 == 9);
    assert(x5 ^^ 3 == 27);
    assert(x5 ^^ 4 == 81);
    assert(x5 ^^ 5 == 81 * 3);
    assert(x5 ^^ 6 == 81 * 9);
}

// opBinaryRight
@nogc nothrow pure @safe unittest
{
    auto x1 = Checked!(int, CountOverflows)(42);
    assert(1 + x1 == 43);
    assert(true + x1 == 43);
    assert(0.5 + x1 == 42.5);
    auto x2 = Checked!(int, void)(42);
    assert(x1 + x2 == 84);
    assert(x2 + x1   == 84);
}

// opOpAssign
@safe unittest
{
    auto x1 = Checked!(int, CountOverflows)(3);
    assert((x1 += 2) == 5);
    x1 *= 2_000_000_000L;
    assert(x1.hook.calls == 1);
    x1 *= -2_000_000_000L;
    assert(x1.hook.calls == 2);

    auto x2 = Checked!(ushort, CountOverflows)(ushort(3));
    assert((x2 += 2) == 5);
    assert(x2.hook.calls == 0);
    assert((x2 += ushort.max) == cast(ushort) (ushort(5) + ushort.max));
    assert(x2.hook.calls == 1);

    auto x3 = Checked!(uint, CountOverflows)(3u);
    x3 *= ulong(2_000_000_000);
    assert(x3.hook.calls == 1);
}

// opAssign
@safe unittest
{
    Checked!(int, void) x;
    x = 42;
    assert(x.get == 42);
    x = x;
    assert(x.get == 42);
    x = short(43);
    assert(x.get == 43);
    x = ushort(44);
    assert(x.get == 44);
}

@safe unittest
{
    static assert(!is(typeof(Checked!(short, void)(ushort(42)))));
    static assert(!is(typeof(Checked!(int, void)(long(42)))));
    static assert(!is(typeof(Checked!(int, void)(ulong(42)))));
    assert(Checked!(short, void)(short(42)).get == 42);
    assert(Checked!(int, void)(ushort(42)).get == 42);
}

// opCast
@nogc nothrow pure @safe unittest
{
    static assert(is(typeof(cast(float) Checked!(int, void)(42)) == float));
    assert(cast(float) Checked!(int, void)(42) == 42);

    assert(is(typeof(cast(long) Checked!(int, void)(42)) == long));
    assert(cast(long) Checked!(int, void)(42) == 42);
    static assert(is(typeof(cast(long) Checked!(uint, void)(42u)) == long));
    assert(cast(long) Checked!(uint, void)(42u) == 42);

    auto x = Checked!(int, void)(42);
    if (x) {} else assert(0);
    x = 0;
    if (x) assert(0);

    static struct Hook1
    {
        uint calls;
        Dst hookOpCast(Dst, Src)(Src value)
        {
            ++calls;
            return 42;
        }
    }
    auto y = Checked!(long, Hook1)(long.max);
    assert(cast(int) y == 42);
    assert(cast(uint) y == 42);
    assert(y.hook.calls == 2);

    static struct Hook2
    {
        uint calls;
        Dst onBadCast(Dst, Src)(Src value)
        {
            ++calls;
            return 42;
        }
    }
    auto x1 = Checked!(uint, Hook2)(100u);
    assert(cast(ushort) x1 == 100);
    assert(cast(short) x1 == 100);
    assert(cast(float) x1 == 100);
    assert(cast(double) x1 == 100);
    assert(cast(real) x1 == 100);
    assert(x1.hook.calls == 0);
    assert(cast(int) x1 == 100);
    assert(x1.hook.calls == 0);
    x1 = uint.max;
    assert(cast(int) x1 == 42);
    assert(x1.hook.calls == 1);

    auto x2 = Checked!(int, Hook2)(-100);
    assert(cast(short) x2 == -100);
    assert(cast(ushort) x2 == 42);
    assert(cast(uint) x2 == 42);
    assert(cast(ulong) x2 == 42);
    assert(x2.hook.calls == 3);
}

// opEquals
@nogc nothrow pure @safe unittest
{
    assert(Checked!(int, void)(42) == 42L);
    assert(42UL == Checked!(int, void)(42));

    static struct Hook1
    {
        uint calls;
        bool hookOpEquals(Lhs, Rhs)(const Lhs lhs, const Rhs rhs)
        {
            ++calls;
            return lhs != rhs;
        }
    }
    auto x1 = Checked!(int, Hook1)(100);
    assert(x1 != Checked!(long, Hook1)(100));
    assert(x1.hook.calls == 1);
    assert(x1 != 100u);
    assert(x1.hook.calls == 2);

    static struct Hook2
    {
        uint calls;
        bool hookOpEquals(Lhs, Rhs)(Lhs lhs, Rhs rhs)
        {
            ++calls;
            return false;
        }
    }
    auto x2 = Checked!(int, Hook2)(-100);
    assert(x2 != x1);
    // For coverage: lhs has no hookOpEquals, rhs does
    assert(Checked!(uint, void)(100u) != x2);
    // For coverage: different types, neither has a hookOpEquals
    assert(Checked!(uint, void)(100u) == Checked!(int, void*)(100));
    assert(x2.hook.calls == 0);
    assert(x2 != -100);
    assert(x2.hook.calls == 1);
    assert(x2 != cast(uint) -100);
    assert(x2.hook.calls == 2);
    x2 = 100;
    assert(x2 != cast(uint) 100);
    assert(x2.hook.calls == 3);
    x2 = -100;

    auto x3 = Checked!(uint, Hook2)(100u);
    assert(x3 != 100);
    x3 = uint.max;
    assert(x3 != -1);

    assert(x2 != x3);
}

// opCmp
@nogc nothrow pure @safe unittest
{
    Checked!(int, void) x;
    assert(x <= x);
    assert(x < 45);
    assert(x < 45u);
    assert(x > -45);
    assert(x < 44.2);
    assert(x > -44.2);
    assert(!(x < double.init));
    assert(!(x > double.init));
    assert(!(x <= double.init));
    assert(!(x >= double.init));

    static struct Hook1
    {
        uint calls;
        int hookOpCmp(Lhs, Rhs)(Lhs lhs, Rhs rhs)
        {
            ++calls;
            return 0;
        }
    }
    auto x1 = Checked!(int, Hook1)(42);
    assert(!(x1 < 43u));
    assert(!(43u < x1));
    assert(x1.hook.calls == 2);

    static struct Hook2
    {
        uint calls;
        int hookOpCmp(Lhs, Rhs)(Lhs lhs, Rhs rhs)
        {
            ++calls;
            return ProperCompare.hookOpCmp(lhs, rhs);
        }
    }
    auto x2 = Checked!(int, Hook2)(-42);
    assert(x2 < 43u);
    assert(43u > x2);
    assert(x2.hook.calls == 2);
    x2 = 42;
    assert(x2 > 41u);

    auto x3 = Checked!(uint, Hook2)(42u);
    assert(x3 > 41);
    assert(x3 > -41);
}

// opUnary
@nogc nothrow pure @safe unittest
{
    auto x = Checked!(int, void)(42);
    assert(x == +x);
    static assert(is(typeof(-x) == typeof(x)));
    assert(-x == Checked!(int, void)(-42));
    static assert(is(typeof(~x) == typeof(x)));
    assert(~x == Checked!(int, void)(~42));
    assert(++x == 43);
    assert(--x == 42);

    static struct Hook1
    {
        uint calls;
        auto hookOpUnary(string op, T)(T value) if (op == "-")
        {
            ++calls;
            return T(42);
        }
        auto hookOpUnary(string op, T)(T value) if (op == "~")
        {
            ++calls;
            return T(43);
        }
    }
    auto x1 = Checked!(int, Hook1)(100);
    assert(is(typeof(-x1) == typeof(x1)));
    assert(-x1 == Checked!(int, Hook1)(42));
    assert(is(typeof(~x1) == typeof(x1)));
    assert(~x1 == Checked!(int, Hook1)(43));
    assert(x1.hook.calls == 2);

    static struct Hook2
    {
        uint calls;
        void hookOpUnary(string op, T)(ref T value) if (op == "++")
        {
            ++calls;
            --value;
        }
        void hookOpUnary(string op, T)(ref T value) if (op == "--")
        {
            ++calls;
            ++value;
        }
    }
    auto x2 = Checked!(int, Hook2)(100);
    assert(++x2 == 99);
    assert(x2 == 99);
    assert(--x2 == 100);
    assert(x2 == 100);

    auto x3 = Checked!(int, CountOverflows)(int.max - 1);
    assert(++x3 == int.max);
    assert(x3.hook.calls == 0);
    assert(++x3 == int.min);
    assert(x3.hook.calls == 1);
    assert(-x3 == int.min);
    assert(x3.hook.calls == 2);

    x3 = int.min + 1;
    assert(--x3 == int.min);
    assert(x3.hook.calls == 2);
    assert(--x3 == int.max);
    assert(x3.hook.calls == 3);
}

//
@nogc nothrow pure @safe unittest
{
    Checked!(int, void) x;
    assert(x == x);
    assert(x == +x);
    assert(x == -x);
    ++x;
    assert(x == 1);
    x++;
    assert(x == 2);

    x = 42;
    assert(x == 42);
    const short _short = 43;
    x = _short;
    assert(x == _short);
    ushort _ushort = 44;
    x = _ushort;
    assert(x == _ushort);
    assert(x == 44.0);
    assert(x != 44.1);
    assert(x < 45);
    assert(x < 44.2);
    assert(x > -45);
    assert(x > -44.2);

    assert(cast(long) x == 44);
    assert(cast(short) x == 44);

    const Checked!(uint, void) y;
    assert(y <= y);
    assert(y == 0);
    assert(y < x);
    x = -1;
    assert(x > y);
}

@nogc nothrow pure @safe unittest
{
    alias cint = Checked!(int, void);
    cint a = 1, b = 2;
    a += b;
    assert(a == cint(3));

    alias ccint = Checked!(cint, Saturate);
    ccint c = 14;
    a += c;
    assert(a == cint(17));
}

// toHash
@safe unittest
{
    assert(checked(42).toHash() == checked(42).toHash());
    assert(checked(12).toHash() != checked(19).toHash());

    static struct Hook1
    {
        static size_t hookToHash(T)(T payload) nothrow @trusted
        {
            static if (size_t.sizeof == 4)
            {
                return typeid(payload).getHash(&payload) ^ 0xFFFF_FFFF;
            }
            else
            {
                return typeid(payload).getHash(&payload) ^ 0xFFFF_FFFF_FFFF_FFFF;
            }

        }
    }

    auto a = checked!Hook1(78);
    auto b = checked!Hook1(78);
    assert(a.toHash() == b.toHash());

    assert(checked!Hook1(12).toHash() != checked!Hook1(13).toHash());

    static struct Hook2
    {
        static if (size_t.sizeof == 4)
        {
            static size_t hashMask = 0xFFFF_0000;
        }
        else
        {
            static size_t hashMask = 0xFFFF_0000_FFFF_0000;
        }

        static size_t hookToHash(T)(T payload) nothrow @trusted
        {
            return typeid(payload).getHash(&payload) ^ hashMask;
        }
    }

    auto x = checked!Hook2(1901);
    auto y = checked!Hook2(1989);

    assert((() nothrow @safe => x.toHash() == x.toHash())());

    assert(x.toHash() == x.toHash());
    assert(x.toHash() != y.toHash());
    assert(checked!Hook1(1901).toHash() != x.toHash());

    immutable z = checked!Hook1(1901);
    immutable t = checked!Hook1(1901);
    immutable w = checked!Hook2(1901);

    assert(z.toHash() == t.toHash());
    assert(z.toHash() != x.toHash());
    assert(z.toHash() != w.toHash());

    const long c = 0xF0F0F0F0;
    const long d = 0xF0F0F0F0;

    assert(checked!Hook1(c).toHash() != checked!Hook2(c));
    assert(checked!Hook1(c).toHash() != checked!Hook1(d));

    // Hook with state, does not implement hookToHash
    static struct Hook3
    {
        ulong var1 = ulong.max;
        uint var2 = uint.max;
    }

    assert(checked!Hook3(12).toHash() != checked!Hook3(13).toHash());
    assert(checked!Hook3(13).toHash() == checked!Hook3(13).toHash());

    // Hook with no state and no hookToHash, payload has its own hashing function
    auto x1 = Checked!(Checked!int, ProperCompare)(123);
    auto x2 = Checked!(Checked!int, ProperCompare)(123);
    auto x3 = Checked!(Checked!int, ProperCompare)(144);

    assert(x1.toHash() == x2.toHash());
    assert(x1.toHash() != x3.toHash());
    assert(x2.toHash() != x3.toHash());

    // Check shared.
    {
        shared shared0 = checked(12345678);
        shared shared1 = checked!Hook1(123456789);
        shared shared2 = checked!Hook2(234567891);
        shared shared3 = checked!Hook3(345678912);
        assert(shared0.toHash() == hashOf(shared0));
        assert(shared1.toHash() == hashOf(shared1));
        assert(shared2.toHash() == hashOf(shared2));
        assert(shared3.toHash() == hashOf(shared3));
    }
}

///
@safe unittest
{
    struct MyHook
    {
        static size_t hookToHash(T)(const T payload) nothrow @trusted
        {
            return .hashOf(payload);
        }
    }

    int[Checked!(int, MyHook)] aa;
    Checked!(int, MyHook) var = 42;
    aa[var] = 100;

    assert(aa[var] == 100);

    int[Checked!(int, Abort)] bb;
    Checked!(int, Abort) var2 = 42;
    bb[var2] = 100;

    assert(bb[var2] == 100);
}
