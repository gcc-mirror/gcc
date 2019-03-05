// Written in the D programming language.
/**
This is a submodule of $(MREF std, algorithm).
It contains generic _comparison algorithms.

$(SCRIPT inhibitQuickIndex = 1;)
$(BOOKTABLE Cheat Sheet,
$(TR $(TH Function Name) $(TH Description))
$(T2 among,
        Checks if a value is among a set of values, e.g.
        $(D if (v.among(1, 2, 3)) // `v` is 1, 2 or 3))
$(T2 castSwitch,
        $(D (new A()).castSwitch((A a)=>1,(B b)=>2)) returns $(D 1).)
$(T2 clamp,
        $(D clamp(1, 3, 6)) returns $(D 3). $(D clamp(4, 3, 6)) returns $(D 4).)
$(T2 cmp,
        $(D cmp("abc", "abcd")) is $(D -1), $(D cmp("abc", "aba")) is $(D 1),
        and $(D cmp("abc", "abc")) is $(D 0).)
$(T2 either,
        Return first parameter $(D p) that passes an $(D if (p)) test, e.g.
        $(D either(0, 42, 43)) returns $(D 42).)
$(T2 equal,
        Compares ranges for element-by-element equality, e.g.
        $(D equal([1, 2, 3], [1.0, 2.0, 3.0])) returns $(D true).)
$(T2 isPermutation,
        $(D isPermutation([1, 2], [2, 1])) returns $(D true).)
$(T2 isSameLength,
        $(D isSameLength([1, 2, 3], [4, 5, 6])) returns $(D true).)
$(T2 levenshteinDistance,
        $(D levenshteinDistance("kitten", "sitting")) returns $(D 3) by using
        the $(LINK2 https://en.wikipedia.org/wiki/Levenshtein_distance,
        Levenshtein distance _algorithm).)
$(T2 levenshteinDistanceAndPath,
        $(D levenshteinDistanceAndPath("kitten", "sitting")) returns
        $(D tuple(3, "snnnsni")) by using the
        $(LINK2 https://en.wikipedia.org/wiki/Levenshtein_distance,
        Levenshtein distance _algorithm).)
$(T2 max,
        $(D max(3, 4, 2)) returns $(D 4).)
$(T2 min,
        $(D min(3, 4, 2)) returns $(D 2).)
$(T2 mismatch,
        $(D mismatch("oh hi", "ohayo")) returns $(D tuple(" hi", "ayo")).)
$(T2 predSwitch,
        $(D 2.predSwitch(1, "one", 2, "two", 3, "three")) returns $(D "two").)
)

Copyright: Andrei Alexandrescu 2008-.

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP erdani.com, Andrei Alexandrescu)

Source: $(PHOBOSSRC std/algorithm/_comparison.d)

Macros:
T2=$(TR $(TDNW $(LREF $1)) $(TD $+))
 */
module std.algorithm.comparison;

// FIXME
import std.functional; // : unaryFun, binaryFun;
import std.range.primitives;
import std.traits;
// FIXME
import std.meta : allSatisfy;
import std.typecons; // : tuple, Tuple, Flag, Yes;

/**
Find $(D value) _among $(D values), returning the 1-based index
of the first matching value in $(D values), or $(D 0) if $(D value)
is not _among $(D values). The predicate $(D pred) is used to
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
@safe unittest
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
Alternatively, $(D values) can be passed at compile-time, allowing for a more
efficient search, but one that only supports matching on equality:
*/
@safe unittest
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

The first choice that $(D switchObject) can be casted to the type
of argument it accepts will be called with $(D switchObject) casted to that
type, and the value it'll return will be returned by $(D castSwitch).

If a choice's return type is void, the choice must throw an exception, unless
all the choices are void. In that case, castSwitch itself will return void.

Throws: If none of the choice matches, a $(D SwitchError) will be thrown.  $(D
SwitchError) will also be thrown if not all the choices are void and a void
choice was executed without throwing anything.

Params:
    choices = The $(D choices) needs to be composed of function or delegate
        handlers that accept one argument. There can also be a choice that
        accepts zero arguments. That choice will be invoked if the $(D
        switchObject) is null.
    switchObject = the object against which the tests are being made.

Returns:
    The value of the selected choice.

Note: $(D castSwitch) can only be used with object types.
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
            result &= is(ReturnType!choice == void);
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

/** Clamps a value into the given bounds.

This functions is equivalent to $(D max(lower, min(upper,val))).

Params:
    val = The value to _clamp.
    lower = The _lower bound of the _clamp.
    upper = The _upper bound of the _clamp.

Returns:
    Returns $(D val), if it is between $(D lower) and $(D upper).
    Otherwise returns the nearest of the two.

*/
auto clamp(T1, T2, T3)(T1 val, T2 lower, T3 upper)
in
{
    import std.functional : greaterThan;
    assert(!lower.greaterThan(upper));
}
body
{
    return max(lower, min(upper, val));
}

///
@safe unittest
{
    assert(clamp(2, 1, 3) == 2);
    assert(clamp(0, 1, 3) == 1);
    assert(clamp(4, 1, 3) == 3);

    assert(clamp(1, 1, 1) == 1);

    assert(clamp(5, -1, 2u) == 2);
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
    static assert(is(typeof(clamp(f, a, b)) == int));
    assert(clamp(f, a, b) == f);
    // similar type deduction for (u)long
    static assert(is(typeof(clamp(-1L, -2L, 2UL)) == long));

    // user-defined types
    import std.datetime : Date;
    assert(clamp(Date(1982, 1, 4), Date(1012, 12, 21), Date(2012, 12, 21)) == Date(1982, 1, 4));
    assert(clamp(Date(1982, 1, 4), Date.min, Date.max) == Date(1982, 1, 4));
    // UFCS style
    assert(Date(1982, 1, 4).clamp(Date.min, Date.max) == Date(1982, 1, 4));

}

// cmp
/**********************************
Performs three-way lexicographical comparison on two
$(REF_ALTTEXT input ranges, isInputRange, std,range,primitives)
according to predicate $(D pred). Iterating $(D r1) and $(D r2) in
lockstep, $(D cmp) compares each element $(D e1) of $(D r1) with the
corresponding element $(D e2) in $(D r2). If one of the ranges has been
finished, $(D cmp) returns a negative value if $(D r1) has fewer
elements than $(D r2), a positive value if $(D r1) has more elements
than $(D r2), and $(D 0) if the ranges have the same number of
elements.

If the ranges are strings, $(D cmp) performs UTF decoding
appropriately and compares the ranges one code point at a time.

Params:
    pred = The predicate used for comparison.
    r1 = The first range.
    r2 = The second range.

Returns:
    0 if both ranges compare equal. -1 if the first differing element of $(D
    r1) is less than the corresponding element of $(D r2) according to $(D
    pred). 1 if the first differing element of $(D r2) is less than the
    corresponding element of $(D r1) according to $(D pred).

*/
int cmp(alias pred = "a < b", R1, R2)(R1 r1, R2 r2)
if (isInputRange!R1 && isInputRange!R2 && !(isSomeString!R1 && isSomeString!R2))
{
    for (;; r1.popFront(), r2.popFront())
    {
        if (r1.empty) return -cast(int)!r2.empty;
        if (r2.empty) return !r1.empty;
        auto a = r1.front, b = r2.front;
        if (binaryFun!pred(a, b)) return -1;
        if (binaryFun!pred(b, a)) return 1;
    }
}

/// ditto
int cmp(alias pred = "a < b", R1, R2)(R1 r1, R2 r2)
if (isSomeString!R1 && isSomeString!R2)
{
    import core.stdc.string : memcmp;
    import std.utf : decode;

    static if (is(typeof(pred) : string))
        enum isLessThan = pred == "a < b";
    else
        enum isLessThan = false;

    // For speed only
    static int threeWay(size_t a, size_t b)
    {
        static if (size_t.sizeof == int.sizeof && isLessThan)
            return a - b;
        else
            return binaryFun!pred(b, a) ? 1 : binaryFun!pred(a, b) ? -1 : 0;
    }
    // For speed only
    // @@@BUG@@@ overloading should be allowed for nested functions
    static int threeWayInt(int a, int b)
    {
        static if (isLessThan)
            return a - b;
        else
            return binaryFun!pred(b, a) ? 1 : binaryFun!pred(a, b) ? -1 : 0;
    }

    static if (typeof(r1[0]).sizeof == typeof(r2[0]).sizeof && isLessThan)
    {
        static if (typeof(r1[0]).sizeof == 1)
        {
            immutable len = min(r1.length, r2.length);
            immutable result = __ctfe ?
                {
                    foreach (i; 0 .. len)
                    {
                        if (r1[i] != r2[i])
                            return threeWayInt(r1[i], r2[i]);
                    }
                    return 0;
                }()
                : () @trusted { return memcmp(r1.ptr, r2.ptr, len); }();
            if (result) return result;
        }
        else
        {
            auto p1 = r1.ptr, p2 = r2.ptr,
                pEnd = p1 + min(r1.length, r2.length);
            for (; p1 != pEnd; ++p1, ++p2)
            {
                if (*p1 != *p2) return threeWayInt(cast(int) *p1, cast(int) *p2);
            }
        }
        return threeWay(r1.length, r2.length);
    }
    else
    {
        for (size_t i1, i2;;)
        {
            if (i1 == r1.length) return threeWay(i2, r2.length);
            if (i2 == r2.length) return threeWay(r1.length, i1);
            immutable c1 = decode(r1, i1),
                c2 = decode(r2, i2);
            if (c1 != c2) return threeWayInt(cast(int) c1, cast(int) c2);
        }
    }
}

///
@safe unittest
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

// equal
/**
Compares two ranges for equality, as defined by predicate $(D pred)
(which is $(D ==) by default).
*/
template equal(alias pred = "a == b")
{
    enum isEmptyRange(R) =
        isInputRange!R && __traits(compiles, {static assert(R.empty);});

    enum hasFixedLength(T) = hasLength!T || isNarrowString!T;

    /++
    Compares two ranges for equality. The ranges may have
    different element types, as long as $(D pred(r1.front, r2.front))
    evaluates to $(D bool).
    Performs $(BIGOH min(r1.length, r2.length)) evaluations of $(D pred).

    Params:
        r1 = The first range to be compared.
        r2 = The second range to be compared.

    Returns:
        $(D true) if and only if the two ranges compare _equal element
        for element, according to binary predicate $(D pred).

    See_Also:
        $(HTTP sgi.com/tech/stl/_equal.html, STL's _equal)
    +/
    bool equal(Range1, Range2)(Range1 r1, Range2 r2)
    if (isInputRange!Range1 && isInputRange!Range2 &&
        is(typeof(binaryFun!pred(r1.front, r2.front))))
    {
        static assert(!(isInfinite!Range1 && isInfinite!Range2),
            "Both ranges are known to be infinite");

        //No pred calls necessary
        static if (isEmptyRange!Range1 || isEmptyRange!Range2)
        {
            return r1.empty && r2.empty;
        }
        else static if ((isInfinite!Range1 && hasFixedLength!Range2) ||
            (hasFixedLength!Range1 && isInfinite!Range2))
        {
            return false;
        }
        //Detect default pred and compatible dynamic array
        else static if (is(typeof(pred) == string) && pred == "a == b" &&
            isArray!Range1 && isArray!Range2 && is(typeof(r1 == r2)))
        {
            return r1 == r2;
        }
        // if one of the arguments is a string and the other isn't, then auto-decoding
        // can be avoided if they have the same ElementEncodingType
        else static if (is(typeof(pred) == string) && pred == "a == b" &&
            isAutodecodableString!Range1 != isAutodecodableString!Range2 &&
            is(ElementEncodingType!Range1 == ElementEncodingType!Range2))
        {
            import std.utf : byCodeUnit;

            static if (isAutodecodableString!Range1)
            {
                return equal(r1.byCodeUnit, r2);
            }
            else
            {
                return equal(r2.byCodeUnit, r1);
            }
        }
        //Try a fast implementation when the ranges have comparable lengths
        else static if (hasLength!Range1 && hasLength!Range2 && is(typeof(r1.length == r2.length)))
        {
            immutable len1 = r1.length;
            immutable len2 = r2.length;
            if (len1 != len2) return false; //Short circuit return

            //Lengths are the same, so we need to do an actual comparison
            //Good news is we can squeeze out a bit of performance by not checking if r2 is empty
            for (; !r1.empty; r1.popFront(), r2.popFront())
            {
                if (!binaryFun!(pred)(r1.front, r2.front)) return false;
            }
            return true;
        }
        else
        {
            //Generic case, we have to walk both ranges making sure neither is empty
            for (; !r1.empty; r1.popFront(), r2.popFront())
            {
                if (r2.empty) return false;
                if (!binaryFun!(pred)(r1.front, r2.front)) return false;
            }
            static if (!isInfinite!Range1)
                return r2.empty;
        }
    }
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.math : approxEqual;

    int[] a = [ 1, 2, 4, 3 ];
    assert(!equal(a, a[1..$]));
    assert(equal(a, a));
    assert(equal!((a, b) => a == b)(a, a));

    // different types
    double[] b = [ 1.0, 2, 4, 3];
    assert(!equal(a, b[1..$]));
    assert(equal(a, b));

    // predicated: ensure that two vectors are approximately equal
    double[] c = [ 1.005, 2, 4, 3];
    assert(equal!approxEqual(b, c));
}

/++
Tip: $(D equal) can itself be used as a predicate to other functions.
This can be very useful when the element type of a range is itself a
range. In particular, $(D equal) can be its own predicate, allowing
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
    import std.math : approxEqual;

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
    double[] c = [ 1.005, 2, 4, 3];
    assert(equal!approxEqual(map!"a*2"(b), map!"a*2"(c)));
    assert(!equal([2, 4, 1, 3], map!"a*2"(a)));
    assert(!equal([2, 4, 1], map!"a*2"(a)));
    assert(!equal!approxEqual(map!"a*3"(b), map!"a*2"(c)));

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

@safe pure unittest
{
    import std.utf : byChar, byWchar, byDchar;

    assert(equal("æøå".byChar, "æøå"));
    assert(equal("æøå", "æøå".byChar));
    assert(equal("æøå".byWchar, "æøå"w));
    assert(equal("æøå"w, "æøå".byWchar));
    assert(equal("æøå".byDchar, "æøå"d));
    assert(equal("æøå"d, "æøå".byDchar));
}

@safe pure unittest
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

// MaxType
private template MaxType(T...)
if (T.length >= 1)
{
    static if (T.length == 1)
    {
        alias MaxType = T[0];
    }
    else static if (T.length == 2)
    {
        static if (!is(typeof(T[0].min)))
            alias MaxType = CommonType!T;
        else static if (T[1].max > T[0].max)
            alias MaxType = T[1];
        else
            alias MaxType = T[0];
    }
    else
    {
        alias MaxType = MaxType!(MaxType!(T[0 .. ($+1)/2]), MaxType!(T[($+1)/2 .. $]));
    }
}

// levenshteinDistance
/**
Encodes $(HTTP realityinteractive.com/rgrzywinski/archives/000249.html,
edit operations) necessary to transform one sequence into
another. Given sequences $(D s) (source) and $(D t) (target), a
sequence of $(D EditOp) encodes the steps that need to be taken to
convert $(D s) into $(D t). For example, if $(D s = "cat") and $(D
"cars"), the minimal sequence that transforms $(D s) into $(D t) is:
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
        if (overflow) assert(0);
        rows = r;
        cols = c;
        if (_matrix.length < rc)
        {
            import core.exception : onOutOfMemoryError;
            import core.stdc.stdlib : realloc;
            const nbytes = mulu(rc, _matrix[0].sizeof, overflow);
            if (overflow) assert(0);
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
distance) between $(D s) and $(D t). The Levenshtein distance computes
the minimal amount of edit operations necessary to transform $(D s)
into $(D t).  Performs $(BIGOH s.length * t.length) evaluations of $(D
equals) and occupies $(BIGOH s.length * t.length) storage.

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

    if (slen > tlen)
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
Returns the Levenshtein distance and the edit path between $(D s) and
$(D t).

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
Iterates the passed arguments and return the maximum value.

Params:
    args = The values to select the maximum from. At least two arguments must
    be passed.

Returns:
    The maximum of the passed-in args. The type of the returned value is
    the type among the passed arguments that is able to store the largest value.

See_Also:
    $(REF maxElement, std,algorithm,searching)
*/
MaxType!T max(T...)(T args)
if (T.length >= 2)
{
    //Get "a"
    static if (T.length <= 2)
        alias a = args[0];
    else
        auto a = max(args[0 .. ($+1)/2]);
    alias T0 = typeof(a);

    //Get "b"
    static if (T.length <= 3)
        alias b = args[$-1];
    else
        auto b = max(args[($+1)/2 .. $]);
    alias T1 = typeof(b);

    import std.algorithm.internal : algoFormat;
    static assert(is(typeof(a < b)),
        algoFormat("Invalid arguments: Cannot compare types %s and %s.", T0.stringof, T1.stringof));

    //Do the "max" proper with a and b
    import std.functional : lessThan;
    immutable chooseB = lessThan!(T0, T1)(a, b);
    return cast(typeof(return)) (chooseB ? b : a);
}

///
@safe unittest
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

@safe unittest
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

// MinType
private template MinType(T...)
if (T.length >= 1)
{
    static if (T.length == 1)
    {
        alias MinType = T[0];
    }
    else static if (T.length == 2)
    {
        static if (!is(typeof(T[0].min)))
            alias MinType = CommonType!T;
        else
        {
            enum hasMostNegative = is(typeof(mostNegative!(T[0]))) &&
                                   is(typeof(mostNegative!(T[1])));
            static if (hasMostNegative && mostNegative!(T[1]) < mostNegative!(T[0]))
                alias MinType = T[1];
            else static if (hasMostNegative && mostNegative!(T[1]) > mostNegative!(T[0]))
                alias MinType = T[0];
            else static if (T[1].max < T[0].max)
                alias MinType = T[1];
            else
                alias MinType = T[0];
        }
    }
    else
    {
        alias MinType = MinType!(MinType!(T[0 .. ($+1)/2]), MinType!(T[($+1)/2 .. $]));
    }
}

// min
/**
Iterates the passed arguments and returns the minimum value.

Params: args = The values to select the minimum from. At least two arguments
    must be passed, and they must be comparable with `<`.
Returns: The minimum of the passed-in values.
See_Also:
    $(REF minElement, std,algorithm,searching)
*/
MinType!T min(T...)(T args)
if (T.length >= 2)
{
    //Get "a"
    static if (T.length <= 2)
        alias a = args[0];
    else
        auto a = min(args[0 .. ($+1)/2]);
    alias T0 = typeof(a);

    //Get "b"
    static if (T.length <= 3)
        alias b = args[$-1];
    else
        auto b = min(args[($+1)/2 .. $]);
    alias T1 = typeof(b);

    import std.algorithm.internal : algoFormat;
    static assert(is(typeof(a < b)),
        algoFormat("Invalid arguments: Cannot compare types %s and %s.", T0.stringof, T1.stringof));

    //Do the "min" proper with a and b
    import std.functional : lessThan;
    immutable chooseA = lessThan!(T0, T1)(a, b);
    return cast(typeof(return)) (chooseA ? a : b);
}

///
@safe unittest
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

    // With arguments of mixed signedness, the return type is the one that can
    // store the lowest values.
    a = -10;
    uint f = 10;
    static assert(is(typeof(min(a, f)) == int));
    assert(min(a, f) == -10);

    // User-defined types that support comparison with < are supported.
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

// mismatch
/**
Sequentially compares elements in $(D r1) and $(D r2) in lockstep, and
stops at the first mismatch (according to $(D pred), by default
equality). Returns a tuple with the reduced ranges that start with the
two mismatched values. Performs $(BIGOH min(r1.length, r2.length))
evaluations of $(D pred).

See_Also:
    $(HTTP sgi.com/tech/stl/_mismatch.html, STL's _mismatch)
*/
Tuple!(Range1, Range2)
mismatch(alias pred = "a == b", Range1, Range2)(Range1 r1, Range2 r2)
if (isInputRange!(Range1) && isInputRange!(Range2))
{
    for (; !r1.empty && !r2.empty; r1.popFront(), r2.popFront())
    {
        if (!binaryFun!(pred)(r1.front, r2.front)) break;
    }
    return tuple(r1, r2);
}

///
@safe unittest
{
    int[]    x = [ 1,  5, 2, 7,   4, 3 ];
    double[] y = [ 1.0, 5, 2, 7.3, 4, 8 ];
    auto m = mismatch(x, y);
    assert(m[0] == x[3 .. $]);
    assert(m[1] == y[3 .. $]);
}

@safe unittest
{
    int[] a = [ 1, 2, 3 ];
    int[] b = [ 1, 2, 4, 5 ];
    auto mm = mismatch(a, b);
    assert(mm[0] == [3]);
    assert(mm[1] == [4, 5]);
}

/**
Returns one of a collection of expressions based on the value of the switch
expression.

$(D choices) needs to be composed of pairs of test expressions and return
expressions. Each test-expression is compared with $(D switchExpression) using
$(D pred)($(D switchExpression) is the first argument) and if that yields true
- the return expression is returned.

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
yield true with any test expression - $(D SwitchError) is thrown. $(D
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
Checks if the two ranges have the same number of elements. This function is
optimized to always take advantage of the $(D length) member of either range
if it exists.

If both ranges have a length member, this function is $(BIGOH 1). Otherwise,
this function is $(BIGOH min(r1.length, r2.length)).

Params:
    r1 = a finite $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    r2 = a finite $(REF_ALTTEXT input range, isInputRange, std,range,primitives)

Returns:
    $(D true) if both ranges have the same length, $(D false) otherwise.
*/
bool isSameLength(Range1, Range2)(Range1 r1, Range2 r2)
if (isInputRange!Range1 &&
    isInputRange!Range2 &&
    !isInfinite!Range1 &&
    !isInfinite!Range2)
{
    static if (hasLength!(Range1) && hasLength!(Range2))
    {
        return r1.length == r2.length;
    }
    else static if (hasLength!(Range1) && !hasLength!(Range2))
    {
        size_t length;

        while (!r2.empty)
        {
            r2.popFront;

            if (++length > r1.length)
            {
                return false;
            }
        }

        return !(length < r1.length);
    }
    else static if (!hasLength!(Range1) && hasLength!(Range2))
    {
        size_t length;

        while (!r1.empty)
        {
            r1.popFront;

            if (++length > r2.length)
            {
                return false;
            }
        }

        return !(length < r2.length);
    }
    else
    {
        while (!r1.empty)
        {
           if (r2.empty)
           {
              return false;
           }

           r1.popFront;
           r2.popFront;
        }

        return r2.empty;
    }
}

///
@safe nothrow pure unittest
{
    assert(isSameLength([1, 2, 3], [4, 5, 6]));
    assert(isSameLength([0.3, 90.4, 23.7, 119.2], [42.6, 23.6, 95.5, 6.3]));
    assert(isSameLength("abc", "xyz"));

    int[] a;
    int[] b;
    assert(isSameLength(a, b));

    assert(!isSameLength([1, 2, 3], [4, 5]));
    assert(!isSameLength([0.3, 90.4, 23.7], [42.6, 23.6, 95.5, 6.3]));
    assert(!isSameLength("abcd", "xyz"));
}

// Test CTFE
@safe pure unittest
{
    enum result1 = isSameLength([1, 2, 3], [4, 5, 6]);
    static assert(result1);

    enum result2 = isSameLength([0.3, 90.4, 23.7], [42.6, 23.6, 95.5, 6.3]);
    static assert(!result2);
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
}

/// For convenience
alias AllocateGC = Flag!"allocateGC";

/**
Checks if both ranges are permutations of each other.

This function can allocate if the $(D Yes.allocateGC) flag is passed. This has
the benefit of have better complexity than the $(D Yes.allocateGC) option. However,
this option is only available for ranges whose equality can be determined via each
element's $(D toHash) method. If customized equality is needed, then the $(D pred)
template parameter can be passed, and the function will automatically switch to
the non-allocating algorithm. See $(REF binaryFun, std,functional) for more details on
how to define $(D pred).

Non-allocating forward range option: $(BIGOH n^2)
Non-allocating forward range option with custom $(D pred): $(BIGOH n^2)
Allocating forward range option: amortized $(BIGOH r1.length) + $(BIGOH r2.length)

Params:
    pred = an optional parameter to change how equality is defined
    allocate_gc = $(D Yes.allocateGC)/$(D No.allocateGC)
    r1 = A finite $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
    r2 = A finite $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)

Returns:
    $(D true) if all of the elements in $(D r1) appear the same number of times in $(D r2).
    Otherwise, returns $(D false).
*/

bool isPermutation(AllocateGC allocate_gc, Range1, Range2)
(Range1 r1, Range2 r2)
if (allocate_gc == Yes.allocateGC &&
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
@safe pure unittest
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

    immutable p = 1;
    immutable q = 2;
    auto pq = either(p, q);
    static assert(is(typeof(pq) == immutable(int)));
    assert(pq == p);

    assert(either(3, 4) == 3);
    assert(either(0, 4) == 4);
    assert(either(0, 0) == 0);
    assert(either("", "a") == "");

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
