// Written in the D programming language.
/**
This is a submodule of $(MREF std, algorithm).
It contains generic comparison algorithms.

$(SCRIPT inhibitQuickIndex = 1;)
$(BOOKTABLE Cheat Sheet,
$(TR $(TH Function Name) $(TH Description))
$(T2 among,
        Checks if a value is among a set of values, e.g.
        `if (v.among(1, 2, 3)) // `v` is 1, 2 or 3`)
$(T2 castSwitch,
        `(new A()).castSwitch((A a)=>1,(B b)=>2)` returns `1`.)
$(T2 clamp,
        `clamp(1, 3, 6)` returns `3`. `clamp(4, 3, 6)` returns `4`.)
$(T2 cmp,
        `cmp("abc", "abcd")` is `-1`, `cmp("abc", "aba")` is `1`,
        and `cmp("abc", "abc")` is `0`.)
$(T2 either,
        Return first parameter `p` that passes an `if (p)` test, e.g.
        `either(0, 42, 43)` returns `42`.)
$(T2 equal,
        Compares ranges for element-by-element equality, e.g.
        `equal([1, 2, 3], [1.0, 2.0, 3.0])` returns `true`.)
$(T2 isPermutation,
        `isPermutation([1, 2], [2, 1])` returns `true`.)
$(T2 isSameLength,
        `isSameLength([1, 2, 3], [4, 5, 6])` returns `true`.)
$(T2 levenshteinDistance,
        `levenshteinDistance("kitten", "sitting")` returns `3` by using
        the $(LINK2 https://en.wikipedia.org/wiki/Levenshtein_distance,
        Levenshtein distance algorithm).)
$(T2 levenshteinDistanceAndPath,
        `levenshteinDistanceAndPath("kitten", "sitting")` returns
        `tuple(3, "snnnsni")` by using the
        $(LINK2 https://en.wikipedia.org/wiki/Levenshtein_distance,
        Levenshtein distance algorithm).)
$(T2 max,
        `max(3, 4, 2)` returns `4`.)
$(T2 min,
        `min(3, 4, 2)` returns `2`.)
$(T2 mismatch,
        `mismatch("oh hi", "ohayo")` returns `tuple(" hi", "ayo")`.)
$(T2 predSwitch,
        `2.predSwitch(1, "one", 2, "two", 3, "three")` returns `"two"`.)
)

Copyright: Andrei Alexandrescu 2008-.

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP erdani.com, Andrei Alexandrescu)

Source: $(PHOBOSSRC std/algorithm/comparison.d)

Macros:
T2=$(TR $(TDNW $(LREF $1)) $(TD $+))
 */
module std.algorithm.comparison;

import std.functional : unaryFun, binaryFun, lessThan, greaterThan;
import std.range.primitives;
import std.traits;
import std.meta : allSatisfy, anySatisfy;
import std.typecons : tuple, Tuple, Flag, Yes;

import std.internal.attributes : betterC;

/**
Find `value` _among `values`, returning the 1-based index
of the first matching value in `values`, or `0` if `value`
is not _among `values`. The predicate `pred` is used to
compare values, and uses equality by default.

Params:
    pred = The predicate used to compare the values.
    value = The value to search for.
    values = The values to compare the value to.

Returns:
    0 if value was not found among the values, otherwise the index of the
    found value plus one is returned.

See_Also:
$(REF_ALTTEXT find, find, std,algorithm,searching) and $(REF_ALTTEXT canFind, canFind, std,algorithm,searching) for finding a value in a
range.
*/
uint among(alias pred = (a, b) => a == b, Value, Values...)
    (Value value, Values values)
if (Values.length != 0)
{
    foreach (uint i, ref v; values)
    {
        import std.functional : binaryFun;
        if (binaryFun!pred(value, v)) return i + 1;
    }
    return 0;
}

/// Ditto
template among(values...)
if (isExpressionTuple!values)
{
    uint among(Value)(Value value)
        if (!is(CommonType!(Value, values) == void))
    {
        switch (value)
        {
            foreach (uint i, v; values)
                case v:
                    return i + 1;
            default:
                return 0;
        }
    }
}

///
@safe @nogc @betterC unittest
{
    assert(3.among(1, 42, 24, 3, 2));

    if (auto pos = "bar".among("foo", "bar", "baz"))
        assert(pos == 2);
    else
        assert(false);

    // 42 is larger than 24
    assert(42.among!((lhs, rhs) => lhs > rhs)(43, 24, 100) == 2);
}

/**
Alternatively, `values` can be passed at compile-time, allowing for a more
efficient search, but one that only supports matching on equality:
*/
@safe @nogc @betterC unittest
{
    assert(3.among!(2, 3, 4));
    assert("bar".among!("foo", "bar", "baz") == 2);
}

@safe unittest
{
    import std.meta : AliasSeq;

    if (auto pos = 3.among(1, 2, 3))
        assert(pos == 3);
    else
        assert(false);
    assert(!4.among(1, 2, 3));

    auto position = "hello".among("hello", "world");
    assert(position);
    assert(position == 1);

    alias values = AliasSeq!("foo", "bar", "baz");
    auto arr = [values];
    assert(arr[0 .. "foo".among(values)] == ["foo"]);
    assert(arr[0 .. "bar".among(values)] == ["foo", "bar"]);
    assert(arr[0 .. "baz".among(values)] == arr);
    assert("foobar".among(values) == 0);

    if (auto pos = 3.among!(1, 2, 3))
        assert(pos == 3);
    else
        assert(false);
    assert(!4.among!(1, 2, 3));

    position = "hello".among!("hello", "world");
    assert(position);
    assert(position == 1);

    static assert(!__traits(compiles, "a".among!("a", 42)));
    static assert(!__traits(compiles, (Object.init).among!(42, "a")));
}

// Used in castSwitch to find the first choice that overshadows the last choice
// in a tuple.
private template indexOfFirstOvershadowingChoiceOnLast(choices...)
{
    alias firstParameterTypes = Parameters!(choices[0]);
    alias lastParameterTypes = Parameters!(choices[$ - 1]);

    static if (lastParameterTypes.length == 0)
    {
        // If the last is null-typed choice, check if the first is null-typed.
        enum isOvershadowing = firstParameterTypes.length == 0;
    }
    else static if (firstParameterTypes.length == 1)
    {
        // If the both first and last are not null-typed, check for overshadowing.
        enum isOvershadowing =
            is(firstParameterTypes[0] == Object) // Object overshadows all other classes!(this is needed for interfaces)
            || is(lastParameterTypes[0] : firstParameterTypes[0]);
    }
    else
    {
        // If the first is null typed and the last is not - the is no overshadowing.
        enum isOvershadowing = false;
    }

    static if (isOvershadowing)
    {
        enum indexOfFirstOvershadowingChoiceOnLast = 0;
    }
    else
    {
        enum indexOfFirstOvershadowingChoiceOnLast =
            1 + indexOfFirstOvershadowingChoiceOnLast!(choices[1..$]);
    }
}

/**
Executes and returns one of a collection of handlers based on the type of the
switch object.

The first choice that `switchObject` can be casted to the type
of argument it accepts will be called with `switchObject` casted to that
type, and the value it'll return will be returned by `castSwitch`.

If a choice's return type is void, the choice must throw an exception, unless
all the choices are void. In that case, castSwitch itself will return void.

Throws: If none of the choice matches, a `SwitchError` will be thrown.  $(D
SwitchError) will also be thrown if not all the choices are void and a void
choice was executed without throwing anything.

Params:
    choices = The `choices` needs to be composed of function or delegate
        handlers that accept one argument. There can also be a choice that
        accepts zero arguments. That choice will be invoked if the $(D
        switchObject) is null.
    switchObject = the object against which the tests are being made.

Returns:
    The value of the selected choice.

Note: `castSwitch` can only be used with object types.
*/
auto castSwitch(choices...)(Object switchObject)
{
    import core.exception : SwitchError;
    import std.format : format;

    // Check to see if all handlers return void.
    enum areAllHandlersVoidResult = {
        bool result = true;
        foreach (index, choice; choices)
        {
            result &= is(ReturnType!choice : void); // void or noreturn
        }
        return result;
    }();

    if (switchObject !is null)
    {
        // Checking for exact matches:
        const classInfo = typeid(switchObject);
        foreach (index, choice; choices)
        {
            static assert(isCallable!choice,
                    "A choice handler must be callable");

            alias choiceParameterTypes = Parameters!choice;
            static assert(choiceParameterTypes.length <= 1,
                    "A choice handler can not have more than one argument.");

            static if (choiceParameterTypes.length == 1)
            {
                alias CastClass = choiceParameterTypes[0];
                static assert(is(CastClass == class) || is(CastClass == interface),
                        "A choice handler can have only class or interface typed argument.");

                // Check for overshadowing:
                immutable indexOfOvershadowingChoice =
                    indexOfFirstOvershadowingChoiceOnLast!(choices[0 .. index + 1]);
                static assert(indexOfOvershadowingChoice == index,
                        "choice number %d(type %s) is overshadowed by choice number %d(type %s)".format(
                            index + 1, CastClass.stringof, indexOfOvershadowingChoice + 1,
                            Parameters!(choices[indexOfOvershadowingChoice])[0].stringof));

                if (classInfo == typeid(CastClass))
                {
                    static if (is(ReturnType!(choice) == void))
                    {
                        choice(cast(CastClass) switchObject);
                        static if (areAllHandlersVoidResult)
                        {
                            return;
                        }
                        else
                        {
                            throw new SwitchError("Handlers that return void should throw");
                        }
                    }
                    else
                    {
                        return choice(cast(CastClass) switchObject);
                    }
                }
            }
        }

        // Checking for derived matches:
        foreach (choice; choices)
        {
            alias choiceParameterTypes = Parameters!choice;
            static if (choiceParameterTypes.length == 1)
            {
                if (auto castedObject = cast(choiceParameterTypes[0]) switchObject)
                {
                    static if (is(ReturnType!(choice) == void))
                    {
                        choice(castedObject);
                        static if (areAllHandlersVoidResult)
                        {
                            return;
                        }
                        else
                        {
                            throw new SwitchError("Handlers that return void should throw");
                        }
                    }
                    else
                    {
                        return choice(castedObject);
                    }
                }
            }
        }
    }
    else // If switchObject is null:
    {
        // Checking for null matches:
        foreach (index, choice; choices)
        {
            static if (Parameters!(choice).length == 0)
            {
                immutable indexOfOvershadowingChoice =
                    indexOfFirstOvershadowingChoiceOnLast!(choices[0 .. index + 1]);

                // Check for overshadowing:
                static assert(indexOfOvershadowingChoice == index,
                        "choice number %d(null reference) is overshadowed by choice number %d(null reference)".format(
                            index + 1, indexOfOvershadowingChoice + 1));

                if (switchObject is null)
                {
                    static if (is(ReturnType!(choice) == void))
                    {
                        choice();
                        static if (areAllHandlersVoidResult)
                        {
                            return;
                        }
                        else
                        {
                            throw new SwitchError("Handlers that return void should throw");
                        }
                    }
                    else
                    {
                        return choice();
                    }
                }
            }
        }
    }

    // In case nothing matched:
    throw new SwitchError("Input not matched by any choice");
}

///
@system unittest
{
    import std.algorithm.iteration : map;
    import std.format : format;

    class A
    {
        int a;
        this(int a) {this.a = a;}
        @property int i() { return a; }
    }
    interface I { }
    class B : I { }

    Object[] arr = [new A(1), new B(), null];

    auto results = arr.map!(castSwitch!(
                                (A a) => "A with a value of %d".format(a.a),
                                (I i) => "derived from I",
                                ()    => "null reference",
                            ))();

    // A is handled directly:
    assert(results[0] == "A with a value of 1");
    // B has no handler - it is handled by the handler of I:
    assert(results[1] == "derived from I");
    // null is handled by the null handler:
    assert(results[2] == "null reference");
}

/// Using with void handlers:
@system unittest
{
    import std.exception : assertThrown;

    class A { }
    class B { }
    // Void handlers are allowed if they throw:
    assertThrown!Exception(
        new B().castSwitch!(
            (A a) => 1,
            (B d)    { throw new Exception("B is not allowed!"); }
        )()
    );

    // Void handlers are also allowed if all the handlers are void:
    new A().castSwitch!(
        (A a) { },
        (B b) { assert(false); },
    )();
}

@system unittest
{
    import core.exception : SwitchError;
    import std.exception : assertThrown;

    interface I { }
    class A : I { }
    class B { }

    // Nothing matches:
    assertThrown!SwitchError((new A()).castSwitch!(
                                 (B b) => 1,
                                 () => 2,
                             )());

    // Choices with multiple arguments are not allowed:
    static assert(!__traits(compiles,
                            (new A()).castSwitch!(
                                (A a, B b) => 0,
                            )()));

    // Only callable handlers allowed:
    static assert(!__traits(compiles,
                            (new A()).castSwitch!(
                                1234,
                            )()));

    // Only object arguments allowed:
    static assert(!__traits(compiles,
                            (new A()).castSwitch!(
                                (int x) => 0,
                            )()));

    // Object overshadows regular classes:
    static assert(!__traits(compiles,
                            (new A()).castSwitch!(
                                (Object o) => 0,
                                (A a) => 1,
                            )()));

    // Object overshadows interfaces:
    static assert(!__traits(compiles,
                            (new A()).castSwitch!(
                                (Object o) => 0,
                                (I i) => 1,
                            )()));

    // No multiple null handlers allowed:
    static assert(!__traits(compiles,
                            (new A()).castSwitch!(
                                () => 0,
                                () => 1,
                            )()));

    // No non-throwing void handlers allowed(when there are non-void handlers):
    assertThrown!SwitchError((new A()).castSwitch!(
                                 (A a)    {},
                                 (B b) => 2,
                             )());

    // All-void handlers work for the null case:
    null.castSwitch!(
        (Object o) { assert(false); },
        ()         { },
    )();

    // Throwing void handlers work for the null case:
    assertThrown!Exception(null.castSwitch!(
                               (Object o) => 1,
                               ()            { throw new Exception("null"); },
                           )());
}

@system unittest
{
    interface I { }
    class B : I { }
    class C : I { }

    assert((new B()).castSwitch!(
            (B b) => "class B",
            (I i) => "derived from I",
    ) == "class B");

    assert((new C()).castSwitch!(
            (B b) => "class B",
            (I i) => "derived from I",
    ) == "derived from I");
}

// https://issues.dlang.org/show_bug.cgi?id=22384
@system unittest
{
    // Use explicit methods to enforce return types
    static void objectSkip(Object) {}
    static void defaultSkip() {}

    static noreturn objectError(Object) { assert(false); }
    static noreturn defaultError() { assert(false); }

    {
        alias test = castSwitch!(objectSkip, defaultError);
        static assert(is(ReturnType!test == void));
    }{
        alias test = castSwitch!(objectError, defaultSkip);
        static assert(is(ReturnType!test == void));
    }{
        alias test = castSwitch!(objectError, defaultError);
        static assert(is(ReturnType!test == noreturn));
    }

    // Also works with non-void handlers
    static int objectValue(Object) { return 1;}
    static int defaultValue() { return 2; }

    {
        alias test = castSwitch!(objectValue, defaultError);
        static assert(is(ReturnType!test == int));
    }{
        alias test = castSwitch!(objectError, defaultValue);
        static assert(is(ReturnType!test == int));
    }

    // No confusion w.r.t. void callbacks
    alias FP = void function();
    static FP objectFunc(Object) { return &defaultSkip; }
    static FP defaultFunc() { return &defaultSkip; }

    {
        alias test = castSwitch!(objectFunc, defaultError);
        static assert(is(ReturnType!test == FP));
    }{
        alias test = castSwitch!(objectError, defaultFunc);
        static assert(is(ReturnType!test == FP));
    }
}

/** Clamps `val` into the given bounds. Result has the same type as `val`.

Params:
    val = The value to _clamp.
    lower = The _lower bound of the _clamp.
    upper = The _upper bound of the _clamp.

Returns:
    `lower` if `val` is less than `lower`, `upper` if `val` is greater than
    `upper`, and `val` in all other cases. Comparisons are made
    correctly (using $(REF lessThan, std,functional) and the return value
    is converted to the return type using the standard integer coversion rules
    $(REF greaterThan, std,functional)) even if the signedness of `T1`, `T2`,
    and `T3` are different.
*/
T1 clamp(T1, T2, T3)(T1 val, T2 lower, T3 upper)
if (is(typeof(val.lessThan(lower) ? lower : val.greaterThan(upper) ? upper : val))
    && (is(T2 : T1) && is(T3 : T1)))
// cannot use :
// `if (is(typeof(val.lessThan(lower) ? lower : val.greaterThan(upper) ? upper : val) : T1))
// because of https://issues.dlang.org/show_bug.cgi?id=16235.
// Once that is fixed, we can simply use the ternary in both the template constraint
// and the template body
in
{
    assert(!lower.greaterThan(upper), "Lower can't be greater than upper.");
}
do
{
    if (val.lessThan(lower))
        return lower;
    else if (val.greaterThan(upper))
        return upper;
    return val;
}

///
@safe @nogc @betterC unittest
{
    assert(clamp(2, 1, 3) == 2);
    assert(clamp(0, 1, 3) == 1);
    assert(clamp(4, 1, 3) == 3);

    assert(clamp(1, 1, 1) == 1);

    assert(clamp(5, -1, 2u) == 2);

    auto x = clamp(42, uint.max, uint.max);
    static assert(is(typeof(x) == int));
    assert(x == -1);
}

@safe unittest
{
    int a = 1;
    short b = 6;
    double c = 2;
    static assert(is(typeof(clamp(c,a,b)) == double));
    assert(clamp(c,   a, b) == c);
    assert(clamp(a-c, a, b) == a);
    assert(clamp(b+c, a, b) == b);
    // mixed sign
    a = -5;
    uint f = 5;
    static assert(is(typeof(clamp(f, a, b)) == uint));
    assert(clamp(f, a, b) == f);
    // similar type deduction for (u)long
    static assert(is(typeof(clamp(-1L, -2L, 2UL)) == long));

    // user-defined types
    import std.datetime : Date;
    assert(clamp(Date(1982, 1, 4), Date(1012, 12, 21), Date(2012, 12, 21)) == Date(1982, 1, 4));
    assert(clamp(Date(1982, 1, 4), Date.min, Date.max) == Date(1982, 1, 4));
    // UFCS style
    assert(Date(1982, 1, 4).clamp(Date.min, Date.max) == Date(1982, 1, 4));

    // Stability
    struct A {
        int x, y;
        int opCmp(ref const A rhs) const { return (x > rhs.x) - (x < rhs.x); }
    }
    A x, lo, hi;
    x.y = 42;
    assert(x.clamp(lo, hi).y == 42);
}

// https://issues.dlang.org/show_bug.cgi?id=23268
@safe pure nothrow @nogc unittest
{
    static assert(__traits(compiles, clamp(short.init, short.init, cast(const) short.init)));
}

// cmp
/**********************************
Performs a lexicographical comparison on two
$(REF_ALTTEXT input ranges, isInputRange, std,range,primitives).
Iterating `r1` and `r2` in lockstep, `cmp` compares each element
`e1` of `r1` with the corresponding element `e2` in `r2`. If one
of the ranges has been finished, `cmp` returns a negative value
if `r1` has fewer elements than `r2`, a positive value if `r1`
has more elements than `r2`, and `0` if the ranges have the same
number of elements.

If the ranges are strings, `cmp` performs UTF decoding
appropriately and compares the ranges one code point at a time.

A custom predicate may be specified, in which case `cmp` performs
a three-way lexicographical comparison using `pred`. Otherwise
the elements are compared using `opCmp`.

Params:
    pred = Predicate used for comparison. Without a predicate
        specified the ordering implied by `opCmp` is used.
    r1 = The first range.
    r2 = The second range.

Returns:
    `0` if the ranges compare equal. A negative value if `r1` is a prefix of `r2` or
    the first differing element of `r1` is less than the corresponding element of `r2`
    according to `pred`. A positive value if `r2` is a prefix of `r1` or the first
    differing element of `r2` is less than the corresponding element of `r1`
    according to `pred`.

Note:
    An earlier version of the documentation incorrectly stated that `-1` is the
    only negative value returned and `1` is the only positive value returned.
    Whether that is true depends on the types being compared.
*/
auto cmp(R1, R2)(R1 r1, R2 r2)
if (isInputRange!R1 && isInputRange!R2)
{
    alias E1 = ElementEncodingType!R1;
    alias E2 = ElementEncodingType!R2;

    static if (isDynamicArray!R1 && isDynamicArray!R2
        && __traits(isUnsigned, E1) && __traits(isUnsigned, E2)
        && E1.sizeof == 1 && E2.sizeof == 1
        // Both or neither must auto-decode.
        && (is(immutable E1 == immutable char) == is(immutable E2 == immutable char)))
    {
        // dstrcmp algorithm is correct for both ubyte[] and for char[].
        import core.internal.string : dstrcmp;
        return dstrcmp(cast(const char[]) r1, cast(const char[]) r2);
    }
    else static if (!(isSomeString!R1 && isSomeString!R2))
    {
        for (;; r1.popFront(), r2.popFront())
        {
            static if (is(typeof(r1.front.opCmp(r2.front)) R))
                alias Result = R;
            else
                alias Result = int;
            if (r2.empty) return Result(!r1.empty);
            if (r1.empty) return Result(-1);
            static if (is(typeof(r1.front.opCmp(r2.front))))
            {
                auto c = r1.front.opCmp(r2.front);
                if (c != 0) return c;
            }
            else
            {
                auto a = r1.front, b = r2.front;
                if (auto result = (b < a) - (a < b)) return result;
            }
        }
    }
    else
    {
        static if (typeof(r1[0]).sizeof == typeof(r2[0]).sizeof)
        {
            return () @trusted
            {
                auto p1 = r1.ptr, p2 = r2.ptr,
                    pEnd = p1 + min(r1.length, r2.length);
                for (; p1 != pEnd; ++p1, ++p2)
                {
                    if (*p1 != *p2) return cast(int) *p1 - cast(int) *p2;
                }
                static if (typeof(r1[0]).sizeof >= 2 && size_t.sizeof <= uint.sizeof)
                    return cast(int) r1.length - cast(int) r2.length;
                else
                    return int(r1.length > r2.length) - int(r1.length < r2.length);
            }();
        }
        else
        {
            import std.utf : decode;

            for (size_t i1, i2;;)
            {
                if (i1 == r1.length) return -int(i2 < r2.length);
                if (i2 == r2.length) return int(1);
                immutable c1 = decode(r1, i1),
                    c2 = decode(r2, i2);
                if (c1 != c2) return cast(int) c1 - cast(int) c2;
            }
        }
    }
}

/// ditto
int cmp(alias pred, R1, R2)(R1 r1, R2 r2)
if (isInputRange!R1 && isInputRange!R2)
{
    static if (!(isSomeString!R1 && isSomeString!R2))
    {
        for (;; r1.popFront(), r2.popFront())
        {
            if (r2.empty) return !r1.empty;
            if (r1.empty) return -1;
            auto a = r1.front, b = r2.front;
            if (binaryFun!pred(a, b)) return -1;
            if (binaryFun!pred(b, a)) return 1;
        }
    }
    else
    {
        import std.utf : decode;

        for (size_t i1, i2;;)
        {
            if (i1 == r1.length) return -int(i2 < r2.length);
            if (i2 == r2.length) return 1;
            immutable c1 = decode(r1, i1),
                c2 = decode(r2, i2);
            if (c1 != c2)
            {
                if (binaryFun!pred(c2, c1)) return 1;
                if (binaryFun!pred(c1, c2)) return -1;
            }
        }
    }
}

///
pure @safe unittest
{
    int result;

    result = cmp("abc", "abc");
    assert(result == 0);
    result = cmp("", "");
    assert(result == 0);
    result = cmp("abc", "abcd");
    assert(result < 0);
    result = cmp("abcd", "abc");
    assert(result > 0);
    result = cmp("abc"d, "abd");
    assert(result < 0);
    result = cmp("bbc", "abc"w);
    assert(result > 0);
    result = cmp("aaa", "aaaa"d);
    assert(result < 0);
    result = cmp("aaaa", "aaa"d);
    assert(result > 0);
    result = cmp("aaa", "aaa"d);
    assert(result == 0);
    result = cmp("aaa"d, "aaa"d);
    assert(result == 0);
    result = cmp(cast(int[])[], cast(int[])[]);
    assert(result == 0);
    result = cmp([1, 2, 3], [1, 2, 3]);
    assert(result == 0);
    result = cmp([1, 3, 2], [1, 2, 3]);
    assert(result > 0);
    result = cmp([1, 2, 3], [1L, 2, 3, 4]);
    assert(result < 0);
    result = cmp([1L, 2, 3], [1, 2]);
    assert(result > 0);
}

/// Example predicate that compares individual elements in reverse lexical order
pure @safe unittest
{
    int result;

    result = cmp!"a > b"("abc", "abc");
    assert(result == 0);
    result = cmp!"a > b"("", "");
    assert(result == 0);
    result = cmp!"a > b"("abc", "abcd");
    assert(result < 0);
    result = cmp!"a > b"("abcd", "abc");
    assert(result > 0);
    result = cmp!"a > b"("abc"d, "abd");
    assert(result > 0);
    result = cmp!"a > b"("bbc", "abc"w);
    assert(result < 0);
    result = cmp!"a > b"("aaa", "aaaa"d);
    assert(result < 0);
    result = cmp!"a > b"("aaaa", "aaa"d);
    assert(result > 0);
    result = cmp!"a > b"("aaa", "aaa"d);
    assert(result == 0);
    result = cmp("aaa"d, "aaa"d);
    assert(result == 0);
    result = cmp!"a > b"(cast(int[])[], cast(int[])[]);
    assert(result == 0);
    result = cmp!"a > b"([1, 2, 3], [1, 2, 3]);
    assert(result == 0);
    result = cmp!"a > b"([1, 3, 2], [1, 2, 3]);
    assert(result < 0);
    result = cmp!"a > b"([1, 2, 3], [1L, 2, 3, 4]);
    assert(result < 0);
    result = cmp!"a > b"([1L, 2, 3], [1, 2]);
    assert(result > 0);
}

// cmp for string with custom predicate fails if distinct chars can compare equal
// https://issues.dlang.org/show_bug.cgi?id=18286
@nogc nothrow pure @safe unittest
{
    static bool ltCi(dchar a, dchar b)// less than, case insensitive
    {
        import std.ascii : toUpper;
        return toUpper(a) < toUpper(b);
    }
    static assert(cmp!ltCi("apple2", "APPLE1") > 0);
    static assert(cmp!ltCi("apple1", "APPLE2") < 0);
    static assert(cmp!ltCi("apple", "APPLE1") < 0);
    static assert(cmp!ltCi("APPLE", "apple1") < 0);
    static assert(cmp!ltCi("apple", "APPLE") == 0);
}

// for non-string ranges check that opCmp is evaluated only once per pair.
// https://issues.dlang.org/show_bug.cgi?id=18280
@nogc nothrow @safe unittest
{
    static int ctr = 0;
    struct S
    {
        int opCmp(ref const S rhs) const
        {
            ++ctr;
            return 0;
        }
        bool opEquals(T)(T o) const { return false; }
        size_t toHash() const { return 0; }
    }
    immutable S[4] a;
    immutable S[4] b;
    immutable result = cmp(a[], b[]);
    assert(result == 0, "neither should compare greater than the other!");
    assert(ctr == a.length, "opCmp should be called exactly once per pair of items!");
}

nothrow pure @safe @nogc unittest
{
    import std.array : staticArray;
    // Test cmp when opCmp returns float.
    struct F
    {
        float value;
        float opCmp(const ref F rhs) const
        {
            return value - rhs.value;
        }
        bool opEquals(T)(T o) const { return false; }
        size_t toHash() const { return 0; }
    }
    auto result = cmp([F(1), F(2), F(3)].staticArray[], [F(1), F(2), F(3)].staticArray[]);
    assert(result == 0);
    assert(is(typeof(result) == float));
    result = cmp([F(1), F(3), F(2)].staticArray[], [F(1), F(2), F(3)].staticArray[]);
    assert(result > 0);
    result = cmp([F(1), F(2), F(3)].staticArray[], [F(1), F(2), F(3), F(4)].staticArray[]);
    assert(result < 0);
    result = cmp([F(1), F(2), F(3)].staticArray[], [F(1), F(2)].staticArray[]);
    assert(result > 0);
}

nothrow pure @safe unittest
{
    // Parallelism (was broken by inferred return type "immutable int")
    import std.parallelism : task;
    auto t = task!cmp("foo", "bar");
}

// equal
/**
Compares two or more ranges for equality, as defined by predicate `pred`
(which is `==` by default).
*/
template equal(alias pred = "a == b")
{
    /++
    Compares two or more ranges for equality. The ranges may have
    different element types, as long as all are comparable by means of
    the `pred`.
    Performs $(BIGOH min(rs[0].length, rs[1].length, ...)) evaluations of `pred`. However, if
    `equal` is invoked with the default predicate, the implementation may take the liberty
    to use faster implementations that have the theoretical worst-case
    $(BIGOH max(rs[0].length, rs[1].length, ...)).

    At least one of the ranges must be finite. If one range involved is infinite, the result is
    (statically known to be) `false`.

    If the ranges have different kinds of UTF code unit (`char`, `wchar`, or
    `dchar`), then they are compared using UTF decoding to avoid
    accidentally integer-promoting units.

    Params:
        rs = The ranges to be compared.

    Returns:
        `true` if and only if all ranges compare _equal element
        for element, according to binary predicate `pred`.
    +/
    bool equal(Ranges...)(Ranges rs)
    if (rs.length > 1
        && allSatisfy!(isInputRange, Ranges)
        && !allSatisfy!(isInfinite, Ranges)
        && is(typeof(binaryFun!pred(rs[0].front, rs[1].front)))
        && (rs.length == 2 || is(typeof(equal!pred(rs[1 .. $])) == bool))
        )
    {
        alias ElementEncodingTypes = staticMap!(ElementEncodingType, Ranges);
        enum differentSize(T) = T.sizeof != ElementEncodingTypes[0].sizeof;
        enum useCodePoint = allSatisfy!(isSomeChar, ElementEncodingTypes) &&
            anySatisfy!(differentSize, ElementEncodingTypes);
        enum bool comparableWithEq(alias r) = is(typeof(rs[0] == r));

        static if (anySatisfy!(isInfinite, Ranges))
        {
            return false;
        }
        else static if (useCodePoint)
        {
            import std.utf : byDchar;
            static bool allByDchar(size_t done, Ranges...)(auto ref Ranges rs)
            {
                static if (done == rs.length)
                    return equalLoop(rs);
                else
                    return allByDchar!(done + 1)(rs[0 .. done], rs[done].byDchar, rs[done + 1 .. $]);
            }
            return allByDchar!0(rs);
        }
        else static if (is(typeof(pred) == string) && pred == "a == b" &&
                allSatisfy!(isArray, Ranges) && allSatisfy!(comparableWithEq, rs))
        {
            static foreach (r; rs[1 .. $])
                if (rs[0] != r)
                    return false;
            return true;
        }
        // if one of the arguments is a string and the other isn't, then auto-decoding
        // can be avoided if they have the same ElementEncodingType
        // TODO: generalize this
        else static if (rs.length == 2 && is(typeof(pred) == string) && pred == "a == b" &&
                isAutodecodableString!(Ranges[0]) != isAutodecodableString!(Ranges[1]) &&
                is(immutable ElementEncodingType!(Ranges[0]) == immutable ElementEncodingType!(Ranges[1])))
        {
            import std.utf : byCodeUnit;
            static if (isAutodecodableString!(Ranges[0]))
                return equal(rs[0].byCodeUnit, rs[1]);
            else
                return equal(rs[1].byCodeUnit, rs[0]);
        }
        else
        {
            static foreach (i, R; Ranges)
            {
                static if (hasLength!R)
                {
                    static if (!is(typeof(firstLength)))
                    {
                        // Found the first range that has length
                        auto firstLength = rs[i].length;
                    }
                    else
                    {
                        // Compare the length of the current range against the first with length
                        if (firstLength != rs[i].length)
                            return false;
                    }
                }
            }
            return equalLoop(rs);
        }
    }

    private bool equalLoop(Rs...)(ref Rs rs)
    {
        for (; !rs[0].empty; rs[0].popFront)
            static foreach (r; rs[1 .. $])
                if (r.empty || !binaryFun!pred(rs[0].front, r.front))
                    return false;
                else
                    r.popFront;
        static foreach (r; rs[1 .. $])
            if (!r.empty)
                return false;
        return true;
    }
}

///
@safe @nogc unittest
{
    import std.algorithm.comparison : equal;
    import std.math.operations : isClose;

    int[4] a = [ 1, 2, 4, 3 ];
    assert(!equal(a[], a[1..$]));
    assert(equal(a[], a[]));
    assert(equal!((a, b) => a == b)(a[], a[]));

    // different types
    double[4] b = [ 1.0, 2, 4, 3];
    assert(!equal(a[], b[1..$]));
    assert(equal(a[], b[]));

    // predicated: ensure that two vectors are approximately equal
    double[4] c = [ 1.0000000005, 2, 4, 3];
    assert(equal!isClose(b[], c[]));
}

@safe @nogc unittest
{
    import std.algorithm.comparison : equal;
    import std.math.operations : isClose;

    auto s1 = "abc", s2 = "abc"w;
    assert(equal(s1, s2, s2));
    assert(equal(s1, s2, s2, s1));
    assert(!equal(s1, s2, s2[1 .. $]));

    int[4] a = [ 1, 2, 4, 3 ];
    assert(!equal(a[], a[1..$], a[]));
    assert(equal(a[], a[], a[]));
    assert(equal!((a, b) => a == b)(a[], a[], a[]));

    // different types
    double[4] b = [ 1.0, 2, 4, 3];
    assert(!equal(a[], b[1..$], b[]));
    assert(equal(a[], b[], a[], b[]));

    // predicated: ensure that two vectors are approximately equal
    double[4] c = [ 1.0000000005, 2, 4, 3];
    assert(equal!isClose(b[], c[], b[]));
}

/++
Tip: `equal` can itself be used as a predicate to other functions.
This can be very useful when the element type of a range is itself a
range. In particular, `equal` can be its own predicate, allowing
range of range (of range...) comparisons.
 +/
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : iota, chunks;
    assert(equal!(equal!equal)(
        [[[0, 1], [2, 3]], [[4, 5], [6, 7]]],
        iota(0, 8).chunks(2).chunks(2)
    ));
}

@safe unittest
{
    import std.algorithm.iteration : map;
    import std.internal.test.dummyrange : ReferenceForwardRange,
        ReferenceInputRange, ReferenceInfiniteForwardRange;
    import std.math.operations : isClose;

    // various strings
    assert(equal("æøå", "æøå")); //UTF8 vs UTF8
    assert(!equal("???", "æøå")); //UTF8 vs UTF8
    assert(equal("æøå"w, "æøå"d)); //UTF16 vs UTF32
    assert(!equal("???"w, "æøå"d));//UTF16 vs UTF32
    assert(equal("æøå"d, "æøå"d)); //UTF32 vs UTF32
    assert(!equal("???"d, "æøå"d));//UTF32 vs UTF32
    assert(!equal("hello", "world"));

    // same strings, but "explicit non default" comparison (to test the non optimized array comparison)
    assert( equal!("a == b")("æøå", "æøå")); //UTF8 vs UTF8
    assert(!equal!("a == b")("???", "æøå")); //UTF8 vs UTF8
    assert( equal!("a == b")("æøå"w, "æøå"d)); //UTF16 vs UTF32
    assert(!equal!("a == b")("???"w, "æøå"d));//UTF16 vs UTF32
    assert( equal!("a == b")("æøå"d, "æøå"d)); //UTF32 vs UTF32
    assert(!equal!("a == b")("???"d, "æøå"d));//UTF32 vs UTF32
    assert(!equal!("a == b")("hello", "world"));

    //Array of string
    assert(equal(["hello", "world"], ["hello", "world"]));
    assert(!equal(["hello", "world"], ["hello"]));
    assert(!equal(["hello", "world"], ["hello", "Bob!"]));

    //Should not compile, because "string == dstring" is illegal
    static assert(!is(typeof(equal(["hello", "world"], ["hello"d, "world"d]))));
    //However, arrays of non-matching string can be compared using equal!equal. Neat-o!
    equal!equal(["hello", "world"], ["hello"d, "world"d]);

    //Tests, with more fancy map ranges
    int[] a = [ 1, 2, 4, 3 ];
    assert(equal([2, 4, 8, 6], map!"a*2"(a)));
    double[] b = [ 1.0, 2, 4, 3];
    double[] c = [ 1.0000000005, 2, 4, 3];
    assert(equal!isClose(map!"a*2"(b), map!"a*2"(c)));
    assert(!equal([2, 4, 1, 3], map!"a*2"(a)));
    assert(!equal([2, 4, 1], map!"a*2"(a)));
    assert(!equal!isClose(map!"a*3"(b), map!"a*2"(c)));

    //Tests with some fancy reference ranges.
    ReferenceInputRange!int cir = new ReferenceInputRange!int([1, 2, 4, 3]);
    ReferenceForwardRange!int cfr = new ReferenceForwardRange!int([1, 2, 4, 3]);
    assert(equal(cir, a));
    cir = new ReferenceInputRange!int([1, 2, 4, 3]);
    assert(equal(cir, cfr.save));
    assert(equal(cfr.save, cfr.save));
    cir = new ReferenceInputRange!int([1, 2, 8, 1]);
    assert(!equal(cir, cfr));

    //Test with an infinite range
    auto ifr = new ReferenceInfiniteForwardRange!int;
    assert(!equal(a, ifr));
    assert(!equal(ifr, a));
    //Test InputRange without length
    assert(!equal(ifr, cir));
    assert(!equal(cir, ifr));
}

@safe @nogc pure unittest
{
    import std.utf : byChar, byDchar, byWchar;

    assert(equal("æøå".byChar, "æøå"));
    assert(equal("æøå".byChar, "æøå"w));
    assert(equal("æøå".byChar, "æøå"d));
    assert(equal("æøå", "æøå".byChar));
    assert(equal("æøå"w, "æøå".byChar));
    assert(equal("æøå"d, "æøå".byChar));

    assert(equal("æøå".byWchar, "æøå"));
    assert(equal("æøå".byWchar, "æøå"w));
    assert(equal("æøå".byWchar, "æøå"d));
    assert(equal("æøå", "æøå".byWchar));
    assert(equal("æøå"w, "æøå".byWchar));
    assert(equal("æøå"d, "æøå".byWchar));

    assert(equal("æøå".byDchar, "æøå"));
    assert(equal("æøå".byDchar, "æøå"w));
    assert(equal("æøå".byDchar, "æøå"d));
    assert(equal("æøå", "æøå".byDchar));
    assert(equal("æøå"w, "æøå".byDchar));
    assert(equal("æøå"d, "æøå".byDchar));
}

@safe @nogc pure unittest
{
    struct R(bool _empty) {
        enum empty = _empty;
        @property char front(){assert(0);}
        void popFront(){assert(0);}
    }
    alias I = R!false;
    static assert(!__traits(compiles, I().equal(I())));
    // strings have fixed length so don't need to compare elements
    assert(!I().equal("foo"));
    assert(!"bar".equal(I()));

    alias E = R!true;
    assert(E().equal(E()));
    assert(E().equal(""));
    assert("".equal(E()));
    assert(!E().equal("foo"));
    assert(!"bar".equal(E()));
}

// levenshteinDistance
/**
Encodes $(HTTP realityinteractive.com/rgrzywinski/archives/000249.html,
edit operations) necessary to transform one sequence into
another. Given sequences `s` (source) and `t` (target), a
sequence of `EditOp` encodes the steps that need to be taken to
convert `s` into `t`. For example, if `s = "cat"` and $(D
"cars"), the minimal sequence that transforms `s` into `t` is:
skip two characters, replace 't' with 'r', and insert an 's'. Working
with edit operations is useful in applications such as spell-checkers
(to find the closest word to a given misspelled word), approximate
searches, diff-style programs that compute the difference between
files, efficient encoding of patches, DNA sequence analysis, and
plagiarism detection.
*/

enum EditOp : char
{
    /** Current items are equal; no editing is necessary. */
    none = 'n',
    /** Substitute current item in target with current item in source. */
    substitute = 's',
    /** Insert current item from the source into the target. */
    insert = 'i',
    /** Remove current item from the target. */
    remove = 'r'
}

///
@safe unittest
{
    with(EditOp)
    {
        assert(levenshteinDistanceAndPath("foo", "foobar")[1] == [none, none, none, insert, insert, insert]);
        assert(levenshteinDistanceAndPath("banana", "fazan")[1] == [substitute, none, substitute, none, none, remove]);
    }
}

private struct Levenshtein(Range, alias equals, CostType = size_t)
{
    EditOp[] path()
    {
        import std.algorithm.mutation : reverse;

        EditOp[] result;
        size_t i = rows - 1, j = cols - 1;
        // restore the path
        while (i || j)
        {
            auto cIns = j == 0 ? CostType.max : matrix(i,j - 1);
            auto cDel = i == 0 ? CostType.max : matrix(i - 1,j);
            auto cSub = i == 0 || j == 0
                ? CostType.max
                : matrix(i - 1,j - 1);
            switch (min_index(cSub, cIns, cDel))
            {
            case 0:
                result ~= matrix(i - 1,j - 1) == matrix(i,j)
                    ? EditOp.none
                    : EditOp.substitute;
                --i;
                --j;
                break;
            case 1:
                result ~= EditOp.insert;
                --j;
                break;
            default:
                result ~= EditOp.remove;
                --i;
                break;
            }
        }
        reverse(result);
        return result;
    }

    ~this() {
        FreeMatrix();
    }

private:
    CostType _deletionIncrement = 1,
        _insertionIncrement = 1,
        _substitutionIncrement = 1;
    CostType[] _matrix;
    size_t rows, cols;

    // Treat _matrix as a rectangular array
    ref CostType matrix(size_t row, size_t col) { return _matrix[row * cols + col]; }

    void AllocMatrix(size_t r, size_t c) @trusted {
        import core.checkedint : mulu;
        bool overflow;
        const rc = mulu(r, c, overflow);
        assert(!overflow, "Overflow during multiplication to determine number "
                ~ " of matrix elements");
        rows = r;
        cols = c;
        if (_matrix.length < rc)
        {
            import core.exception : onOutOfMemoryError;
            import core.stdc.stdlib : realloc;
            const nbytes = mulu(rc, _matrix[0].sizeof, overflow);
            assert(!overflow, "Overflow during multiplication to determine "
                ~ " number of bytes of matrix");
            auto m = cast(CostType *) realloc(_matrix.ptr, nbytes);
            if (!m)
                onOutOfMemoryError();
            _matrix = m[0 .. r * c];
            InitMatrix();
        }
    }

    void FreeMatrix() @trusted {
        import core.stdc.stdlib : free;

        free(_matrix.ptr);
        _matrix = null;
    }

    void InitMatrix() {
        foreach (r; 0 .. rows)
            matrix(r,0) = r * _deletionIncrement;
        foreach (c; 0 .. cols)
            matrix(0,c) = c * _insertionIncrement;
    }

    static uint min_index(CostType i0, CostType i1, CostType i2)
    {
        if (i0 <= i1)
        {
            return i0 <= i2 ? 0 : 2;
        }
        else
        {
            return i1 <= i2 ? 1 : 2;
        }
    }

    CostType distanceWithPath(Range s, Range t)
    {
        auto slen = walkLength(s.save), tlen = walkLength(t.save);
        AllocMatrix(slen + 1, tlen + 1);
        foreach (i; 1 .. rows)
        {
            auto sfront = s.front;
            auto tt = t.save;
            foreach (j; 1 .. cols)
            {
                auto cSub = matrix(i - 1,j - 1)
                    + (equals(sfront, tt.front) ? 0 : _substitutionIncrement);
                tt.popFront();
                auto cIns = matrix(i,j - 1) + _insertionIncrement;
                auto cDel = matrix(i - 1,j) + _deletionIncrement;
                switch (min_index(cSub, cIns, cDel))
                {
                case 0:
                    matrix(i,j) = cSub;
                    break;
                case 1:
                    matrix(i,j) = cIns;
                    break;
                default:
                    matrix(i,j) = cDel;
                    break;
                }
            }
            s.popFront();
        }
        return matrix(slen,tlen);
    }

    CostType distanceLowMem(Range s, Range t, CostType slen, CostType tlen)
    {
        CostType lastdiag, olddiag;
        AllocMatrix(slen + 1, 1);
        foreach (y; 1 .. slen + 1)
        {
            _matrix[y] = y;
        }
        foreach (x; 1 .. tlen + 1)
        {
            auto tfront = t.front;
            auto ss = s.save;
            _matrix[0] = x;
            lastdiag = x - 1;
            foreach (y; 1 .. rows)
            {
                olddiag = _matrix[y];
                auto cSub = lastdiag + (equals(ss.front, tfront) ? 0 : _substitutionIncrement);
                ss.popFront();
                auto cIns = _matrix[y - 1] + _insertionIncrement;
                auto cDel = _matrix[y] + _deletionIncrement;
                switch (min_index(cSub, cIns, cDel))
                {
                case 0:
                    _matrix[y] = cSub;
                    break;
                case 1:
                    _matrix[y] = cIns;
                    break;
                default:
                    _matrix[y] = cDel;
                    break;
                }
                lastdiag = olddiag;
            }
            t.popFront();
        }
        return _matrix[slen];
    }
}

/**
Returns the $(HTTP wikipedia.org/wiki/Levenshtein_distance, Levenshtein
distance) between `s` and `t`. The Levenshtein distance computes
the minimal amount of edit operations necessary to transform `s`
into `t`.  Performs $(BIGOH s.length * t.length) evaluations of $(D
equals) and occupies $(BIGOH min(s.length, t.length)) storage.

Params:
    equals = The binary predicate to compare the elements of the two ranges.
    s = The original range.
    t = The transformation target

Returns:
    The minimal number of edits to transform s into t.

Does not allocate GC memory.
*/
size_t levenshteinDistance(alias equals = (a,b) => a == b, Range1, Range2)
    (Range1 s, Range2 t)
if (isForwardRange!(Range1) && isForwardRange!(Range2))
{
    alias eq = binaryFun!(equals);

    for (;;)
    {
        if (s.empty) return t.walkLength;
        if (t.empty) return s.walkLength;
        if (eq(s.front, t.front))
        {
            s.popFront();
            t.popFront();
            continue;
        }
        static if (isBidirectionalRange!(Range1) && isBidirectionalRange!(Range2))
        {
            if (eq(s.back, t.back))
            {
                s.popBack();
                t.popBack();
                continue;
            }
        }
        break;
    }

    auto slen = walkLength(s.save);
    auto tlen = walkLength(t.save);

    if (slen == 1 && tlen == 1)
    {
        return eq(s.front, t.front) ? 0 : 1;
    }

    if (slen < tlen)
    {
        Levenshtein!(Range1, eq, size_t) lev;
        return lev.distanceLowMem(s, t, slen, tlen);
    }
    else
    {
        Levenshtein!(Range2, eq, size_t) lev;
        return lev.distanceLowMem(t, s, tlen, slen);
    }
}

///
@safe unittest
{
    import std.algorithm.iteration : filter;
    import std.uni : toUpper;

    assert(levenshteinDistance("cat", "rat") == 1);
    assert(levenshteinDistance("parks", "spark") == 2);
    assert(levenshteinDistance("abcde", "abcde") == 0);
    assert(levenshteinDistance("abcde", "abCde") == 1);
    assert(levenshteinDistance("kitten", "sitting") == 3);
    assert(levenshteinDistance!((a, b) => toUpper(a) == toUpper(b))
        ("parks", "SPARK") == 2);
    assert(levenshteinDistance("parks".filter!"true", "spark".filter!"true") == 2);
    assert(levenshteinDistance("ID", "I♥D") == 1);
}

@safe @nogc nothrow unittest
{
    assert(levenshteinDistance("cat"d, "rat"d) == 1);
}

/// ditto
size_t levenshteinDistance(alias equals = (a,b) => a == b, Range1, Range2)
    (auto ref Range1 s, auto ref Range2 t)
if (isConvertibleToString!Range1 || isConvertibleToString!Range2)
{
    import std.meta : staticMap;
    alias Types = staticMap!(convertToString, Range1, Range2);
    return levenshteinDistance!(equals, Types)(s, t);
}

@safe unittest
{
    static struct S { string s; alias s this; }
    assert(levenshteinDistance(S("cat"), S("rat")) == 1);
    assert(levenshteinDistance("cat", S("rat")) == 1);
    assert(levenshteinDistance(S("cat"), "rat") == 1);
}

@safe @nogc nothrow unittest
{
    static struct S { dstring s; alias s this; }
    assert(levenshteinDistance(S("cat"d), S("rat"d)) == 1);
    assert(levenshteinDistance("cat"d, S("rat"d)) == 1);
    assert(levenshteinDistance(S("cat"d), "rat"d) == 1);
}

/**
Returns the Levenshtein distance and the edit path between `s` and
`t`.

Params:
    equals = The binary predicate to compare the elements of the two ranges.
    s = The original range.
    t = The transformation target

Returns:
    Tuple with the first element being the minimal amount of edits to transform s into t and
    the second element being the sequence of edits to effect this transformation.

Allocates GC memory for the returned EditOp[] array.
*/
Tuple!(size_t, EditOp[])
levenshteinDistanceAndPath(alias equals = (a,b) => a == b, Range1, Range2)
    (Range1 s, Range2 t)
if (isForwardRange!(Range1) && isForwardRange!(Range2))
{
    Levenshtein!(Range1, binaryFun!(equals)) lev;
    auto d = lev.distanceWithPath(s, t);
    return tuple(d, lev.path());
}

///
@safe unittest
{
    string a = "Saturday", b = "Sundays";
    auto p = levenshteinDistanceAndPath(a, b);
    assert(p[0] == 4);
    assert(equal(p[1], "nrrnsnnni"));
}

@safe unittest
{
    assert(levenshteinDistance("a", "a") == 0);
    assert(levenshteinDistance("a", "b") == 1);
    assert(levenshteinDistance("aa", "ab") == 1);
    assert(levenshteinDistance("aa", "abc") == 2);
    assert(levenshteinDistance("Saturday", "Sunday") == 3);
    assert(levenshteinDistance("kitten", "sitting") == 3);
}

/// ditto
Tuple!(size_t, EditOp[])
levenshteinDistanceAndPath(alias equals = (a,b) => a == b, Range1, Range2)
    (auto ref Range1 s, auto ref Range2 t)
if (isConvertibleToString!Range1 || isConvertibleToString!Range2)
{
    import std.meta : staticMap;
    alias Types = staticMap!(convertToString, Range1, Range2);
    return levenshteinDistanceAndPath!(equals, Types)(s, t);
}

@safe unittest
{
    static struct S { string s; alias s this; }
    assert(levenshteinDistanceAndPath(S("cat"), S("rat"))[0] == 1);
    assert(levenshteinDistanceAndPath("cat", S("rat"))[0] == 1);
    assert(levenshteinDistanceAndPath(S("cat"), "rat")[0] == 1);
}


// max
/**
Iterates the passed arguments and returns the maximum value.

Params:
    args = The values to select the maximum from. At least two arguments must
    be passed, and they must be comparable with `<`.

Returns:
    The maximum of the passed-in values. The type of the returned value is
    the type among the passed arguments that is able to store the largest value.
    If at least one of the arguments is NaN, the result is an unspecified value.
    See $(REF maxElement, std,algorithm,searching) for examples on how to cope
    with NaNs.

See_Also:
    $(REF maxElement, std,algorithm,searching)
*/
auto max(T...)(T args)
if (T.length >= 2 && !is(CommonType!T == void))
{
    // Get left-hand side of the comparison.
    static if (T.length == 2)
        alias a = args[0];
    else
        auto a = max(args[0 .. ($ + 1) / 2]);
    alias T0 = typeof(a);

    // Get right-hand side.
    static if (T.length <= 3)
        alias b = args[$ - 1];
    else
        auto b = max(args[($ + 1) / 2 .. $]);
    alias T1 = typeof(b);

    static assert(is(typeof(a < b)),
        "Invalid arguments: Cannot compare types " ~ T0.stringof ~
        " and " ~ T1.stringof ~ " for ordering.");

    // Compute the returned type.
    static if (is(typeof(mostNegative!T0 < mostNegative!T1)))
        // Both are numeric (or character or Boolean), so we choose the one with the highest maximum.
        // (We use mostNegative for num/bool/char testing purposes even if it's not used otherwise.)
        alias Result = Select!(T1.max > T0.max, T1, T0);
    else
        // At least one is non-numeric, so just go with the common type.
        alias Result = CommonType!(T0, T1);

    // Perform the computation.
    import std.functional : lessThan;
    immutable chooseB = lessThan!(T0, T1)(a, b);
    return cast(Result) (chooseB ? b : a);
}

/// ditto
T max(T, U)(T a, U b)
if (is(T == U) && is(typeof(a < b)))
{
   /* Handle the common case without all the template expansions
    * of the general case
    */
    return a < b ? b : a;
}

///
@safe @betterC @nogc unittest
{
    int a = 5;
    short b = 6;
    double c = 2;
    auto d = max(a, b);
    assert(is(typeof(d) == int));
    assert(d == 6);
    auto e = min(a, b, c);
    assert(is(typeof(e) == double));
    assert(e == 2);
}

@safe unittest  // not @nogc due to `Date`
{
    int a = 5;
    short b = 6;
    double c = 2;
    auto d = max(a, b);
    static assert(is(typeof(d) == int));
    assert(d == 6);
    auto e = max(a, b, c);
    static assert(is(typeof(e) == double));
    assert(e == 6);
    // mixed sign
    a = -5;
    uint f = 5;
    static assert(is(typeof(max(a, f)) == uint));
    assert(max(a, f) == 5);

    //Test user-defined types
    import std.datetime : Date;
    assert(max(Date(2012, 12, 21), Date(1982, 1, 4)) == Date(2012, 12, 21));
    assert(max(Date(1982, 1, 4), Date(2012, 12, 21)) == Date(2012, 12, 21));
    assert(max(Date(1982, 1, 4), Date.min) == Date(1982, 1, 4));
    assert(max(Date.min, Date(1982, 1, 4)) == Date(1982, 1, 4));
    assert(max(Date(1982, 1, 4), Date.max) == Date.max);
    assert(max(Date.max, Date(1982, 1, 4)) == Date.max);
    assert(max(Date.min, Date.max) == Date.max);
    assert(max(Date.max, Date.min) == Date.max);
}

// min
/**
Iterates the passed arguments and returns the minimum value.

Params:
    args = The values to select the minimum from. At least two arguments must
    be passed, and they must be comparable with `<`.

Returns:
    The minimum of the passed-in values. The type of the returned value is
    the type among the passed arguments that is able to store the smallest value.
    If at least one of the arguments is NaN, the result is an unspecified value.
    See $(REF minElement, std,algorithm,searching) for examples on how to cope
    with NaNs.

See_Also:
    $(REF minElement, std,algorithm,searching)
*/
auto min(T...)(T args)
if (T.length >= 2 && !is(CommonType!T == void))
{
    // Get the left-hand side of the comparison.
    static if (T.length <= 2)
        alias a = args[0];
    else
        auto a = min(args[0 .. ($ + 1) / 2]);
    alias T0 = typeof(a);

    // Get the right-hand side.
    static if (T.length <= 3)
        alias b = args[$ - 1];
    else
        auto b = min(args[($ + 1) / 2 .. $]);
    alias T1 = typeof(b);

    static assert(is(typeof(a < b)),
        "Invalid arguments: Cannot compare types " ~ T0.stringof ~
        " and " ~ T1.stringof ~ " for ordering.");

    // Compute the returned type.
    static if (is(typeof(mostNegative!T0 < mostNegative!T1)))
        // Both are numeric (or character or Boolean), so we choose the one with the lowest minimum.
        // If they have the same minimum, choose the one with the smallest size.
        // If both mostNegative and sizeof are equal, go for stability: pick the type of the first one.
        alias Result = Select!(mostNegative!T1 < mostNegative!T0 ||
                mostNegative!T1 == mostNegative!T0 && T1.sizeof < T0.sizeof,
            T1, T0);
    else
        // At least one is non-numeric, so just go with the common type.
        alias Result = CommonType!(T0, T1);

    // Engage!
    import std.functional : lessThan;
    immutable chooseB = lessThan!(T1, T0)(b, a);
    return cast(Result) (chooseB ? b : a);
}

/// ditto
T min(T, U)(T a, U b)
if (is(T == U) && is(typeof(a < b)))
{
   /* Handle the common case without all the template expansions
    * of the general case
    */
    return b < a ? b : a;
}


///
@safe @nogc @betterC unittest
{
    int a = 5;
    short b = 6;
    double c = 2;
    auto d = min(a, b);
    static assert(is(typeof(d) == int));
    assert(d == 5);
    auto e = min(a, b, c);
    static assert(is(typeof(e) == double));
    assert(e == 2);
    ulong f = 0xffff_ffff_ffff;
    const uint g = min(f, 0xffff_0000);
    assert(g == 0xffff_0000);
    dchar h = 100;
    uint i = 101;
    static assert(is(typeof(min(h, i)) == dchar));
    static assert(is(typeof(min(i, h)) == uint));
    assert(min(h, i) == 100);
}

/**
With arguments of mixed signedness, the return type is the one that can
store the lowest values.
*/
@safe @nogc @betterC unittest
{
    int a = -10;
    uint f = 10;
    static assert(is(typeof(min(a, f)) == int));
    assert(min(a, f) == -10);
}

/// User-defined types that support comparison with < are supported.
@safe unittest  // not @nogc due to `Date`
{
    import std.datetime;
    assert(min(Date(2012, 12, 21), Date(1982, 1, 4)) == Date(1982, 1, 4));
    assert(min(Date(1982, 1, 4), Date(2012, 12, 21)) == Date(1982, 1, 4));
    assert(min(Date(1982, 1, 4), Date.min) == Date.min);
    assert(min(Date.min, Date(1982, 1, 4)) == Date.min);
    assert(min(Date(1982, 1, 4), Date.max) == Date(1982, 1, 4));
    assert(min(Date.max, Date(1982, 1, 4)) == Date(1982, 1, 4));
    assert(min(Date.min, Date.max) == Date.min);
    assert(min(Date.max, Date.min) == Date.min);
}

// min must be stable: when in doubt, return the first argument.
@safe unittest
{
    assert(min(1.0, double.nan) == 1.0);
    assert(min(double.nan, 1.0) is double.nan);
    static struct A {
        int x;
        string y;
        int opCmp(const A a) const { return int(x > a.x) - int(x < a.x); }
    }
    assert(min(A(1, "first"), A(1, "second")) == A(1, "first"));
}

// mismatch
/**
Sequentially compares elements in `rs` in lockstep, and
stops at the first mismatch (according to `pred`, by default
equality). Returns a tuple with the reduced ranges that start with the
two mismatched values. Performs $(BIGOH min(r[0].length, r[1].length, ...))
evaluations of `pred`.
*/
Tuple!(Ranges)
mismatch(alias pred = (a, b) => a == b, Ranges...)(Ranges rs)
if (rs.length >= 2 && allSatisfy!(isInputRange, Ranges))
{
    loop: for (; !rs[0].empty; rs[0].popFront)
    {
        static foreach (r; rs[1 .. $])
        {
            if (r.empty || !binaryFun!pred(rs[0].front, r.front))
                break loop;
            r.popFront;
        }
    }
    return tuple(rs);
}

///
@safe @nogc unittest
{
    int[6] x = [ 1,   5, 2, 7,   4, 3 ];
    double[6] y = [ 1.0, 5, 2, 7.3, 4, 8 ];
    auto m = mismatch(x[], y[]);
    assert(m[0] == x[3 .. $]);
    assert(m[1] == y[3 .. $]);

    auto m2 = mismatch(x[], y[], x[], y[]);
    assert(m2[0] == x[3 .. $]);
    assert(m2[1] == y[3 .. $]);
    assert(m2[2] == x[3 .. $]);
    assert(m2[3] == y[3 .. $]);
}

@safe @nogc unittest
{
    import std.range : only;

    int[3] a = [ 1, 2, 3 ];
    int[4] b = [ 1, 2, 4, 5 ];
    auto mm = mismatch(a[], b[]);
    assert(equal(mm[0], only(3)));
    assert(equal(mm[1], only(4, 5)));
}

/**
Returns one of a collection of expressions based on the value of the switch
expression.

`choices` needs to be composed of pairs of test expressions and return
expressions. Each test-expression is compared with `switchExpression` using
`pred`(`switchExpression` is the first argument) and if that yields true -
the return expression is returned.

Both the test and the return expressions are lazily evaluated.

Params:

switchExpression = The first argument for the predicate.

choices = Pairs of test expressions and return expressions. The test
expressions will be the second argument for the predicate, and the return
expression will be returned if the predicate yields true with $(D
switchExpression) and the test expression as arguments.  May also have a
default return expression, that needs to be the last expression without a test
expression before it. A return expression may be of void type only if it
always throws.

Returns: The return expression associated with the first test expression that
made the predicate yield true, or the default return expression if no test
expression matched.

Throws: If there is no default return expression and the predicate does not
yield true with any test expression - `SwitchError` is thrown. $(D
SwitchError) is also thrown if a void return expression was executed without
throwing anything.
*/
auto predSwitch(alias pred = "a == b", T, R ...)(T switchExpression, lazy R choices)
{
    import core.exception : SwitchError;
    alias predicate = binaryFun!(pred);

    foreach (index, ChoiceType; R)
    {
        //The even places in `choices` are for the predicate.
        static if (index % 2 == 1)
        {
            if (predicate(switchExpression, choices[index - 1]()))
            {
                static if (is(typeof(choices[index]()) == void))
                {
                    choices[index]();
                    throw new SwitchError("Choices that return void should throw");
                }
                else
                {
                    return choices[index]();
                }
            }
        }
    }

    //In case nothing matched:
    static if (R.length % 2 == 1) //If there is a default return expression:
    {
        static if (is(typeof(choices[$ - 1]()) == void))
        {
            choices[$ - 1]();
            throw new SwitchError("Choices that return void should throw");
        }
        else
        {
            return choices[$ - 1]();
        }
    }
    else //If there is no default return expression:
    {
        throw new SwitchError("Input not matched by any pattern");
    }
}

///
@safe unittest
{
    string res = 2.predSwitch!"a < b"(
        1, "less than 1",
        5, "less than 5",
        10, "less than 10",
        "greater or equal to 10");

    assert(res == "less than 5");

    //The arguments are lazy, which allows us to use predSwitch to create
    //recursive functions:
    int factorial(int n)
    {
        return n.predSwitch!"a <= b"(
            -1, {throw new Exception("Can not calculate n! for n < 0");}(),
            0, 1, // 0! = 1
            n * factorial(n - 1) // n! = n * (n - 1)! for n >= 0
            );
    }
    assert(factorial(3) == 6);

    //Void return expressions are allowed if they always throw:
    import std.exception : assertThrown;
    assertThrown!Exception(factorial(-9));
}

@system unittest
{
    import core.exception : SwitchError;
    import std.exception : assertThrown;

    //Nothing matches - with default return expression:
    assert(20.predSwitch!"a < b"(
        1, "less than 1",
        5, "less than 5",
        10, "less than 10",
        "greater or equal to 10") == "greater or equal to 10");

    //Nothing matches - without default return expression:
    assertThrown!SwitchError(20.predSwitch!"a < b"(
        1, "less than 1",
        5, "less than 5",
        10, "less than 10",
        ));

    //Using the default predicate:
    assert(2.predSwitch(
                1, "one",
                2, "two",
                3, "three",
                ) == "two");

    //Void return expressions must always throw:
    assertThrown!SwitchError(1.predSwitch(
                0, "zero",
                1, {}(), //A void return expression that doesn't throw
                2, "two",
                ));
}

/**
Checks if two or more ranges have the same number of elements. This function is
optimized to always take advantage of the `length` member of either range
if it exists.

If all ranges have a `length` member or at least one is infinite,
`_isSameLength`'s complexity is $(BIGOH 1). Otherwise, complexity is
$(BIGOH n), where `n` is the smallest of the lengths of ranges with unknown
length.

Infinite ranges are considered of the same length. An infinite range has never
the same length as a finite range.

Params:
    rs = two or more $(REF_ALTTEXT input ranges, isInputRange, std,range,primitives)

Returns:
    `true` if both ranges have the same length, `false` otherwise.
*/
bool isSameLength(Ranges...)(Ranges rs)
if (allSatisfy!(isInputRange, Ranges))
{
    static if (anySatisfy!(isInfinite, Ranges))
    {
        return allSatisfy!(isInfinite, Ranges);
    }
    else static if (anySatisfy!(hasLength, Ranges))
    {
        // Compute the O(1) length
        auto baselineLength = size_t.max;
        static foreach (i, R; Ranges)
        {
            static if (hasLength!R)
            {
                if (baselineLength == size_t.max)
                    baselineLength = rs[i].length;
                else if (rs[i].length != baselineLength)
                    return false;
            }
        }
        // Iterate all ranges without known length
        foreach (_; 0 .. baselineLength)
            static foreach (i, R; Ranges)
            {
                static if (!hasLength!R)
                {
                    // All must be non-empty
                    if (rs[i].empty)
                        return false;
                    rs[i].popFront;
                }
            }
        static foreach (i, R; Ranges)
        {
            static if (!hasLength!R)
            {
                // All must be now empty
                if (!rs[i].empty)
                    return false;
            }
        }
        return true;
    }
    else
    {
        // All have unknown length, iterate in lockstep
        for (;;)
            static foreach (i, r; rs)
            {
                if (r.empty)
                {
                    // One is empty, so all must be empty
                    static if (i != 0)
                    {
                        return false;
                    }
                    else
                    {
                        static foreach (j, r1; rs[1 .. $])
                            if (!r1.empty)
                                return false;
                        return true;
                    }
                }
                r.popFront;
            }
    }
}

///
@safe nothrow pure unittest
{
    assert(isSameLength([1, 2, 3], [4, 5, 6]));
    assert(isSameLength([1, 2, 3], [4, 5, 6], [7, 8, 9]));
    assert(isSameLength([0.3, 90.4, 23.7, 119.2], [42.6, 23.6, 95.5, 6.3]));
    assert(isSameLength("abc", "xyz"));
    assert(isSameLength("abc", "xyz", [1, 2, 3]));

    int[] a;
    int[] b;
    assert(isSameLength(a, b));
    assert(isSameLength(a, b, a, a, b, b, b));

    assert(!isSameLength([1, 2, 3], [4, 5]));
    assert(!isSameLength([1, 2, 3], [4, 5, 6], [7, 8]));
    assert(!isSameLength([0.3, 90.4, 23.7], [42.6, 23.6, 95.5, 6.3]));
    assert(!isSameLength("abcd", "xyz"));
    assert(!isSameLength("abcd", "xyz", "123"));
    assert(!isSameLength("abcd", "xyz", "1234"));
}

// Test CTFE
@safe @nogc pure @betterC unittest
{
    static assert(isSameLength([1, 2, 3], [4, 5, 6]));
    static assert(isSameLength([1, 2, 3], [4, 5, 6], [7, 8, 9]));
    static assert(!isSameLength([0.3, 90.4, 23.7], [42.6, 23.6, 95.5, 6.3]));
    static assert(!isSameLength([1], [0.3, 90.4], [42]));
}

@safe @nogc pure unittest
{
    import std.range : only;
    assert(isSameLength(only(1, 2, 3), only(4, 5, 6)));
    assert(isSameLength(only(1, 2, 3), only(4, 5, 6), only(7, 8, 9)));
    assert(isSameLength(only(0.3, 90.4, 23.7, 119.2), only(42.6, 23.6, 95.5, 6.3)));
    assert(!isSameLength(only(1, 3, 3), only(4, 5)));
    assert(!isSameLength(only(1, 3, 3), only(1, 3, 3), only(4, 5)));
    assert(!isSameLength(only(1, 3, 3), only(4, 5), only(1, 3, 3)));
}

@safe nothrow pure unittest
{
    import std.internal.test.dummyrange;

    auto r1 = new ReferenceInputRange!int([1, 2, 3]);
    auto r2 = new ReferenceInputRange!int([4, 5, 6]);
    assert(isSameLength(r1, r2));

    auto r3 = new ReferenceInputRange!int([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Input) r4;
    assert(isSameLength(r3, r4));

    DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Input) r5;
    auto r6 = new ReferenceInputRange!int([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
    assert(isSameLength(r5, r6));

    auto r7 = new ReferenceInputRange!int([1, 2]);
    auto r8 = new ReferenceInputRange!int([4, 5, 6]);
    assert(!isSameLength(r7, r8));

    auto r9 = new ReferenceInputRange!int([1, 2, 3, 4, 5, 6, 7, 8]);
    DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Input) r10;
    assert(!isSameLength(r9, r10));

    DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Input) r11;
    auto r12 = new ReferenceInputRange!int([1, 2, 3, 4, 5, 6, 7, 8]);
    assert(!isSameLength(r11, r12));

    import std.algorithm.iteration : filter;

    assert(isSameLength(filter!"a >= 1"([1, 2, 3]), [4, 5, 6]));
    assert(!isSameLength(filter!"a > 1"([1, 2, 3]), [4, 5, 6]));

    assert(isSameLength(filter!"a > 1"([1, 2, 3]), filter!"a > 4"([4, 5, 6])));
    assert(isSameLength(filter!"a > 1"([1, 2, 3]),
        filter!"a > 4"([4, 5, 6]), filter!"a >= 5"([4, 5, 6])));
}

// Still functional but not documented anymore.
alias AllocateGC = Flag!"allocateGC";

/**
Checks if both ranges are permutations of each other.

This function can allocate if the `Yes.allocateGC` flag is passed. This has
the benefit of have better complexity than the `Yes.allocateGC` option. However,
this option is only available for ranges whose equality can be determined via each
element's `toHash` method. If customized equality is needed, then the `pred`
template parameter can be passed, and the function will automatically switch to
the non-allocating algorithm. See $(REF binaryFun, std,functional) for more details on
how to define `pred`.

Non-allocating forward range option: $(BIGOH n^2)
Non-allocating forward range option with custom `pred`: $(BIGOH n^2)
Allocating forward range option: amortized $(BIGOH r1.length) + $(BIGOH r2.length)

Params:
    pred = an optional parameter to change how equality is defined
    allocateGC = `Yes.allocateGC`/`No.allocateGC`
    r1 = A finite $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
    r2 = A finite $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)

Returns:
    `true` if all of the elements in `r1` appear the same number of times in `r2`.
    Otherwise, returns `false`.
*/

bool isPermutation(Flag!"allocateGC" allocateGC, Range1, Range2)
(Range1 r1, Range2 r2)
if (allocateGC == Yes.allocateGC &&
    isForwardRange!Range1 &&
    isForwardRange!Range2 &&
    !isInfinite!Range1 &&
    !isInfinite!Range2)
{
    alias E1 = Unqual!(ElementType!Range1);
    alias E2 = Unqual!(ElementType!Range2);

    if (!isSameLength(r1.save, r2.save))
    {
        return false;
    }

    // Skip the elements at the beginning where r1.front == r2.front,
    // they are in the same order and don't need to be counted.
    while (!r1.empty && !r2.empty && r1.front == r2.front)
    {
        r1.popFront();
        r2.popFront();
    }

    if (r1.empty && r2.empty)
    {
        return true;
    }

    int[CommonType!(E1, E2)] counts;

    foreach (item; r1)
    {
        ++counts[item];
    }

    foreach (item; r2)
    {
        if (--counts[item] < 0)
        {
            return false;
        }
    }

    return true;
}

/// ditto
bool isPermutation(alias pred = "a == b", Range1, Range2)
(Range1 r1, Range2 r2)
if (is(typeof(binaryFun!(pred))) &&
    isForwardRange!Range1 &&
    isForwardRange!Range2 &&
    !isInfinite!Range1 &&
    !isInfinite!Range2)
{
    import std.algorithm.searching : count;

    alias predEquals = binaryFun!(pred);
    alias E1 = Unqual!(ElementType!Range1);
    alias E2 = Unqual!(ElementType!Range2);

    if (!isSameLength(r1.save, r2.save))
    {
        return false;
    }

    // Skip the elements at the beginning where r1.front == r2.front,
    // they are in the same order and don't need to be counted.
    while (!r1.empty && !r2.empty && predEquals(r1.front, r2.front))
    {
        r1.popFront();
        r2.popFront();
    }

    if (r1.empty && r2.empty)
    {
        return true;
    }

    size_t r1_count;
    size_t r2_count;

    // At each element item, when computing the count of item, scan it while
    // also keeping track of the scanning index. If the first occurrence
    // of item in the scanning loop has an index smaller than the current index,
    // then you know that the element has been seen before
    size_t index;
    outloop: for (auto r1s1 = r1.save; !r1s1.empty; r1s1.popFront, index++)
    {
        auto item = r1s1.front;
        r1_count = 0;
        r2_count = 0;

        size_t i;
        for (auto r1s2 = r1.save; !r1s2.empty; r1s2.popFront, i++)
        {
            auto e = r1s2.front;
            if (predEquals(e, item) && i < index)
            {
                 continue outloop;
            }
            else if (predEquals(e, item))
            {
                ++r1_count;
            }
        }

        r2_count = r2.save.count!pred(item);

        if (r1_count != r2_count)
        {
            return false;
        }
    }

    return true;
}

///
@safe pure unittest
{
    import std.typecons : Yes;

    assert(isPermutation([1, 2, 3], [3, 2, 1]));
    assert(isPermutation([1.1, 2.3, 3.5], [2.3, 3.5, 1.1]));
    assert(isPermutation("abc", "bca"));

    assert(!isPermutation([1, 2], [3, 4]));
    assert(!isPermutation([1, 1, 2, 3], [1, 2, 2, 3]));
    assert(!isPermutation([1, 1], [1, 1, 1]));

    // Faster, but allocates GC handled memory
    assert(isPermutation!(Yes.allocateGC)([1.1, 2.3, 3.5], [2.3, 3.5, 1.1]));
    assert(!isPermutation!(Yes.allocateGC)([1, 2], [3, 4]));
}

// Test @nogc inference
@safe @nogc pure unittest
{
    static immutable arr1 = [1, 2, 3];
    static immutable arr2 = [3, 2, 1];
    assert(isPermutation(arr1, arr2));

    static immutable arr3 = [1, 1, 2, 3];
    static immutable arr4 = [1, 2, 2, 3];
    assert(!isPermutation(arr3, arr4));
}

@safe pure unittest
{
    import std.internal.test.dummyrange;

    auto r1 = new ReferenceForwardRange!int([1, 2, 3, 4]);
    auto r2 = new ReferenceForwardRange!int([1, 2, 4, 3]);
    assert(isPermutation(r1, r2));

    auto r3 = new ReferenceForwardRange!int([1, 2, 3, 4]);
    auto r4 = new ReferenceForwardRange!int([4, 2, 1, 3]);
    assert(isPermutation!(Yes.allocateGC)(r3, r4));

    auto r5 = new ReferenceForwardRange!int([1, 2, 3]);
    auto r6 = new ReferenceForwardRange!int([4, 2, 1, 3]);
    assert(!isPermutation(r5, r6));

    auto r7 = new ReferenceForwardRange!int([4, 2, 1, 3]);
    auto r8 = new ReferenceForwardRange!int([1, 2, 3]);
    assert(!isPermutation!(Yes.allocateGC)(r7, r8));

    DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random) r9;
    DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random) r10;
    assert(isPermutation(r9, r10));

    DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random) r11;
    DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random) r12;
    assert(isPermutation!(Yes.allocateGC)(r11, r12));

    alias mytuple = Tuple!(int, int);

    assert(isPermutation!"a[0] == b[0]"(
        [mytuple(1, 4), mytuple(2, 5)],
        [mytuple(2, 3), mytuple(1, 2)]
    ));
}

/**
Get the _first argument `a` that passes an `if (unaryFun!pred(a))` test.  If
no argument passes the test, return the last argument.

Similar to behaviour of the `or` operator in dynamic languages such as Lisp's
`(or ...)` and Python's `a or b or ...` except that the last argument is
returned upon no match.

Simplifies logic, for instance, in parsing rules where a set of alternative
matchers are tried. The _first one that matches returns it match result,
typically as an abstract syntax tree (AST).

Bugs:
Lazy parameters are currently, too restrictively, inferred by DMD to
always throw even though they don't need to be. This makes it impossible to
currently mark `either` as `nothrow`. See issue at $(BUGZILLA 12647).

Returns:
    The _first argument that passes the test `pred`.
*/
CommonType!(T, Ts) either(alias pred = a => a, T, Ts...)(T first, lazy Ts alternatives)
if (alternatives.length >= 1 &&
    !is(CommonType!(T, Ts) == void) &&
    allSatisfy!(ifTestable, T, Ts))
{
    alias predFun = unaryFun!pred;

    if (predFun(first)) return first;

    foreach (e; alternatives[0 .. $ - 1])
        if (predFun(e)) return e;

    return alternatives[$ - 1];
}

///
@safe pure @betterC unittest
{
    const a = 1;
    const b = 2;
    auto ab = either(a, b);
    static assert(is(typeof(ab) == const(int)));
    assert(ab == a);

    auto c = 2;
    const d = 3;
    auto cd = either!(a => a == 3)(c, d); // use predicate
    static assert(is(typeof(cd) == int));
    assert(cd == d);

    auto e = 0;
    const f = 2;
    auto ef = either(e, f);
    static assert(is(typeof(ef) == int));
    assert(ef == f);
}

///
@safe pure unittest
{
    immutable p = 1;
    immutable q = 2;
    auto pq = either(p, q);
    static assert(is(typeof(pq) == immutable(int)));
    assert(pq == p);

    assert(either(3, 4) == 3);
    assert(either(0, 4) == 4);
    assert(either(0, 0) == 0);
    assert(either("", "a") == "");
}

///
@safe pure unittest
{
    string r = null;
    assert(either(r, "a") == "a");
    assert(either("a", "") == "a");

    immutable s = [1, 2];
    assert(either(s, s) == s);

    assert(either([0, 1], [1, 2]) == [0, 1]);
    assert(either([0, 1], [1]) == [0, 1]);
    assert(either("a", "b") == "a");

    static assert(!__traits(compiles, either(1, "a")));
    static assert(!__traits(compiles, either(1.0, "a")));
    static assert(!__traits(compiles, either('a', "a")));
}
