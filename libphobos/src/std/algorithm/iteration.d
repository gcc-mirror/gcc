// Written in the D programming language.
/**
This is a submodule of $(MREF std, algorithm).
It contains generic iteration algorithms.

$(SCRIPT inhibitQuickIndex = 1;)
$(BOOKTABLE Cheat Sheet,
$(TR $(TH Function Name) $(TH Description))
$(T2 cache,
        Eagerly evaluates and caches another range's `front`.)
$(T2 cacheBidirectional,
        As above, but also provides `back` and `popBack`.)
$(T2 chunkBy,
        `chunkBy!((a,b) => a[1] == b[1])([[1, 1], [1, 2], [2, 2], [2, 1]])`
        returns a range containing 3 subranges: the first with just
        `[1, 1]`; the second with the elements `[1, 2]` and `[2, 2]`;
        and the third with just `[2, 1]`.)
$(T2 cumulativeFold,
        `cumulativeFold!((a, b) => a + b)([1, 2, 3, 4])` returns a
        lazily-evaluated range containing the successive reduced values `1`,
        `3`, `6`, `10`.)
$(T2 each,
        `each!writeln([1, 2, 3])` eagerly prints the numbers `1`, `2`
        and `3` on their own lines.)
$(T2 filter,
        `filter!(a => a > 0)([1, -1, 2, 0, -3])` iterates over elements `1`
        and `2`.)
$(T2 filterBidirectional,
        Similar to `filter`, but also provides `back` and `popBack` at
        a small increase in cost.)
$(T2 fold,
        `fold!((a, b) => a + b)([1, 2, 3, 4])` returns `10`.)
$(T2 group,
        `group([5, 2, 2, 3, 3])` returns a range containing the tuples
        `tuple(5, 1)`, `tuple(2, 2)`, and `tuple(3, 2)`.)
$(T2 joiner,
        `joiner(["hello", "world!"], "; ")` returns a range that iterates
        over the characters `"hello; world!"`. No new string is created -
        the existing inputs are iterated.)
$(T2 map,
        `map!(a => a * 2)([1, 2, 3])` lazily returns a range with the numbers
        `2`, `4`, `6`.)
$(T2 mean,
        Colloquially known as the average, `mean([1, 2, 3])` returns `2`.)
$(T2 permutations,
        Lazily computes all permutations using Heap's algorithm.)
$(T2 reduce,
        `reduce!((a, b) => a + b)([1, 2, 3, 4])` returns `10`.
        This is the old implementation of `fold`.)
$(T2 splitWhen,
        Lazily splits a range by comparing adjacent elements.)
$(T2 splitter,
        Lazily splits a range by a separator.)
$(T2 substitute,
        `[1, 2].substitute(1, 0.1)` returns `[0.1, 2]`.)
$(T2 sum,
        Same as `fold`, but specialized for accurate summation.)
$(T2 uniq,
        Iterates over the unique elements in a range, which is assumed sorted.)
)

Copyright: Andrei Alexandrescu 2008-.

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP erdani.com, Andrei Alexandrescu)

Source: $(PHOBOSSRC std/algorithm/iteration.d)

Macros:
T2=$(TR $(TDNW $(LREF $1)) $(TD $+))
 */
module std.algorithm.iteration;

import std.functional : unaryFun, binaryFun;
import std.range.primitives;
import std.traits;
import std.typecons : Flag, Yes, No;

/++
`cache` eagerly evaluates $(REF_ALTTEXT front, front, std,range,primitives) of `range`
on each construction or call to $(REF_ALTTEXT popFront, popFront, std,range,primitives),
to store the result in a _cache.
The result is then directly returned when $(REF_ALTTEXT front, front, std,range,primitives) is called,
rather than re-evaluated.

This can be a useful function to place in a chain, after functions
that have expensive evaluation, as a lazy alternative to $(REF array, std,array).
In particular, it can be placed after a call to $(LREF map), or before a call
$(REF filter, std,range) or $(REF tee, std,range)

`cache` may provide
$(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives)
iteration if needed, but since this comes at an increased cost, it must be explicitly requested via the
call to `cacheBidirectional`. Furthermore, a bidirectional _cache will
evaluate the "center" element twice, when there is only one element left in
the range.

`cache` does not provide random access primitives,
as `cache` would be unable to _cache the random accesses.
If `Range` provides slicing primitives,
then `cache` will provide the same slicing primitives,
but `hasSlicing!Cache` will not yield true (as the $(REF hasSlicing, std,range,primitives)
trait also checks for random access).

Params:
    range = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)

Returns:
    An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) with the cached values of range
+/
auto cache(Range)(Range range)
if (isInputRange!Range)
{
    return _Cache!(Range, false)(range);
}

/// ditto
auto cacheBidirectional(Range)(Range range)
if (isBidirectionalRange!Range)
{
    return _Cache!(Range, true)(range);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range, std.stdio;
    import std.typecons : tuple;

    ulong counter = 0;
    double fun(int x)
    {
        ++counter;
        // http://en.wikipedia.org/wiki/Quartic_function
        return ( (x + 4.0) * (x + 1.0) * (x - 1.0) * (x - 3.0) ) / 14.0 + 0.5;
    }
    // Without cache, with array (greedy)
    auto result1 = iota(-4, 5).map!(a =>tuple(a, fun(a)))()
                             .filter!(a => a[1] < 0)()
                             .map!(a => a[0])()
                             .array();

    // the values of x that have a negative y are:
    assert(equal(result1, [-3, -2, 2]));

    // Check how many times fun was evaluated.
    // As many times as the number of items in both source and result.
    assert(counter == iota(-4, 5).length + result1.length);

    counter = 0;
    // Without array, with cache (lazy)
    auto result2 = iota(-4, 5).map!(a =>tuple(a, fun(a)))()
                             .cache()
                             .filter!(a => a[1] < 0)()
                             .map!(a => a[0])();

    // the values of x that have a negative y are:
    assert(equal(result2, [-3, -2, 2]));

    // Check how many times fun was evaluated.
    // Only as many times as the number of items in source.
    assert(counter == iota(-4, 5).length);
}

// https://issues.dlang.org/show_bug.cgi?id=15891
@safe pure unittest
{
    assert([1].map!(x=>[x].map!(y=>y)).cache.front.front == 1);
}

/++
Tip: `cache` is eager when evaluating elements. If calling front on the
underlying range has a side effect, it will be observable before calling
front on the actual cached range.

Furthermore, care should be taken composing `cache` with $(REF take, std,range).
By placing `take` before `cache`, then `cache` will be "aware"
of when the range ends, and correctly stop caching elements when needed.
If calling front has no side effect though, placing `take` after `cache`
may yield a faster range.

Either way, the resulting ranges will be equivalent, but maybe not at the
same cost or side effects.
+/
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range;
    int i = 0;

    auto r = iota(0, 4).tee!((a){i = a;}, No.pipeOnPop);
    auto r1 = r.take(3).cache();
    auto r2 = r.cache().take(3);

    assert(equal(r1, [0, 1, 2]));
    assert(i == 2); //The last "seen" element was 2. The data in cache has been cleared.

    assert(equal(r2, [0, 1, 2]));
    assert(i == 3); //cache has accessed 3. It is still stored internally by cache.
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range;
    auto a = [1, 2, 3, 4];
    assert(equal(a.map!(a => (a - 1) * a)().cache(),                      [ 0, 2, 6, 12]));
    assert(equal(a.map!(a => (a - 1) * a)().cacheBidirectional().retro(), [12, 6, 2,  0]));
    auto r1 = [1, 2, 3, 4].cache()             [1 .. $];
    auto r2 = [1, 2, 3, 4].cacheBidirectional()[1 .. $];
    assert(equal(r1, [2, 3, 4]));
    assert(equal(r2, [2, 3, 4]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    //immutable test
    static struct S
    {
        int i;
        this(int i)
        {
            //this.i = i;
        }
    }
    immutable(S)[] s = [S(1), S(2), S(3)];
    assert(equal(s.cache(),              s));
    assert(equal(s.cacheBidirectional(), s));
}

@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;

    //safety etc
    auto a = [1, 2, 3, 4];
    assert(equal(a.cache(),              a));
    assert(equal(a.cacheBidirectional(), a));
}

@safe unittest
{
    char[][] stringbufs = ["hello".dup, "world".dup];
    auto strings = stringbufs.map!((a)=>a.idup)().cache();
    assert(strings.front is strings.front);
}

@safe unittest
{
    import std.range : cycle;
    import std.algorithm.comparison : equal;

    auto c = [1, 2, 3].cycle().cache();
    c = c[1 .. $];
    auto d = c[0 .. 1];
    assert(d.equal([2]));
}

@safe unittest
{
    static struct Range
    {
        bool initialized = false;
        bool front() @property {return initialized = true;}
        void popFront() {initialized = false;}
        enum empty = false;
    }
    auto r = Range().cache();
    assert(r.source.initialized == true);
}

private struct _Cache(R, bool bidir)
{
    import core.exception : RangeError;

    private
    {
        import std.algorithm.internal : algoFormat;
        import std.meta : AliasSeq;

        alias E  = ElementType!R;
        alias UE = Unqual!E;

        R source;

        static if (bidir) alias CacheTypes = AliasSeq!(UE, UE);
        else              alias CacheTypes = AliasSeq!UE;
        CacheTypes caches;

        static assert(isAssignable!(UE, E) && is(UE : E),
            algoFormat(
                "Cannot instantiate range with %s because %s elements are not assignable to %s.",
                R.stringof,
                E.stringof,
                UE.stringof
            )
        );
    }

    this(R range)
    {
        source = range;
        if (!range.empty)
        {
            caches[0] = source.front;
            static if (bidir)
                caches[1] = source.back;
        }
        else
        {
            // needed, because the compiler cannot deduce, that 'caches' is initialized
            // see https://issues.dlang.org/show_bug.cgi?id=15891
            caches[0] = UE.init;
            static if (bidir)
                caches[1] = UE.init;
        }
    }

    static if (isInfinite!R)
        enum empty = false;
    else
        bool empty() @property
        {
            return source.empty;
        }

    mixin ImplementLength!source;

    E front() @property
    {
        version (assert) if (empty) throw new RangeError();
        return caches[0];
    }
    static if (bidir) E back() @property
    {
        version (assert) if (empty) throw new RangeError();
        return caches[1];
    }

    void popFront()
    {
        version (assert) if (empty) throw new RangeError();
        source.popFront();
        if (!source.empty)
            caches[0] = source.front;
        else
        {
            // see https://issues.dlang.org/show_bug.cgi?id=15891
            caches[0] = UE.init;
            static if (bidir)
                caches[1] = UE.init;
        }
    }
    static if (bidir) void popBack()
    {
        version (assert) if (empty) throw new RangeError();
        source.popBack();
        if (!source.empty)
            caches[1] = source.back;
        else
        {
            // see https://issues.dlang.org/show_bug.cgi?id=15891
            caches[0] = UE.init;
            caches[1] = UE.init;
        }
    }

    static if (isForwardRange!R)
    {
        private this(R source, ref CacheTypes caches)
        {
            this.source = source;
            this.caches = caches;
        }
        typeof(this) save() @property
        {
            return typeof(this)(source.save, caches);
        }
    }

    static if (hasSlicing!R)
    {
        enum hasEndSlicing = is(typeof(source[size_t.max .. $]));

        static if (hasEndSlicing)
        {
            private static struct DollarToken{}
            enum opDollar = DollarToken.init;

            auto opSlice(size_t low, DollarToken)
            {
                return typeof(this)(source[low .. $]);
            }
        }

        static if (!isInfinite!R)
        {
            typeof(this) opSlice(size_t low, size_t high)
            {
                return typeof(this)(source[low .. high]);
            }
        }
        else static if (hasEndSlicing)
        {
            auto opSlice(size_t low, size_t high)
            in
            {
                assert(low <= high, "Bounds error when slicing cache.");
            }
            do
            {
                import std.range : takeExactly;
                return this[low .. $].takeExactly(high - low);
            }
        }
    }
}

/**
Implements the homonym function (also known as `transform`) present
in many languages of functional flavor. The call `map!(fun)(range)`
returns a range of which elements are obtained by applying `fun(a)`
left to right for all elements `a` in `range`. The original ranges are
not changed. Evaluation is done lazily.

Params:
    fun = one or more transformation functions

See_Also:
    $(HTTP en.wikipedia.org/wiki/Map_(higher-order_function), Map (higher-order function))
*/
template map(fun...)
if (fun.length >= 1)
{
    /**
    Params:
        r = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    Returns:
        A range with each fun applied to all the elements. If there is more than one
        fun, the element type will be `Tuple` containing one element for each fun.
     */
    auto map(Range)(Range r)
    if (isInputRange!(Unqual!Range))
    {
        import std.meta : AliasSeq, staticMap;

        alias RE = ElementType!(Range);
        static if (fun.length > 1)
        {
            import std.functional : adjoin;
            import std.meta : staticIndexOf;

            alias _funs = staticMap!(unaryFun, fun);
            alias _fun = adjoin!_funs;

            // Once https://issues.dlang.org/show_bug.cgi?id=5710 is fixed
            // accross all compilers (as of 2020-04, it wasn't fixed in LDC and GDC),
            // this validation loop can be moved into a template.
            foreach (f; _funs)
            {
                static assert(!is(typeof(f(RE.init)) == void),
                    "Mapping function(s) must not return void: " ~ _funs.stringof);
            }
        }
        else
        {
            alias _fun = unaryFun!fun;
            alias _funs = AliasSeq!(_fun);

            // Do the validation separately for single parameters due to
            // https://issues.dlang.org/show_bug.cgi?id=15777.
            static assert(!is(typeof(_fun(RE.init)) == void),
                "Mapping function(s) must not return void: " ~ _funs.stringof);
        }

        return MapResult!(_fun, Range)(r);
    }
}

///
@safe @nogc unittest
{
    import std.algorithm.comparison : equal;
    import std.range : chain, only;
    auto squares =
        chain(only(1, 2, 3, 4), only(5, 6)).map!(a => a * a);
    assert(equal(squares, only(1, 4, 9, 16, 25, 36)));
}

/**
Multiple functions can be passed to `map`. In that case, the
element type of `map` is a tuple containing one element for each
function.
*/
@safe unittest
{
    auto sums = [2, 4, 6, 8];
    auto products = [1, 4, 9, 16];

    size_t i = 0;
    foreach (result; [ 1, 2, 3, 4 ].map!("a + a", "a * a"))
    {
        assert(result[0] == sums[i]);
        assert(result[1] == products[i]);
        ++i;
    }
}

/**
You may alias `map` with some function(s) to a symbol and use
it separately:
*/
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : to;

    alias stringize = map!(to!string);
    assert(equal(stringize([ 1, 2, 3, 4 ]), [ "1", "2", "3", "4" ]));
}

// Verify workaround for https://issues.dlang.org/show_bug.cgi?id=15777
@safe unittest
{
    import std.algorithm.mutation, std.string;
    auto foo(string[] args)
    {
        return args.map!strip;
    }
}

private struct MapResult(alias fun, Range)
{
    alias R = Unqual!Range;
    R _input;

    static if (isBidirectionalRange!R)
    {
        @property auto ref back()()
        {
            assert(!empty, "Attempting to fetch the back of an empty map.");
            return fun(_input.back);
        }

        void popBack()()
        {
            assert(!empty, "Attempting to popBack an empty map.");
            _input.popBack();
        }
    }

    this(R input)
    {
        _input = input;
    }

    static if (isInfinite!R)
    {
        // Propagate infinite-ness.
        enum bool empty = false;
    }
    else
    {
        @property bool empty()
        {
            return _input.empty;
        }
    }

    void popFront()
    {
        assert(!empty, "Attempting to popFront an empty map.");
        _input.popFront();
    }

    @property auto ref front()
    {
        assert(!empty, "Attempting to fetch the front of an empty map.");
        return fun(_input.front);
    }

    static if (isRandomAccessRange!R)
    {
        static if (is(typeof(Range.init[ulong.max])))
            private alias opIndex_t = ulong;
        else
            private alias opIndex_t = uint;

        auto ref opIndex(opIndex_t index)
        {
            return fun(_input[index]);
        }
    }

    mixin ImplementLength!_input;

    static if (hasSlicing!R)
    {
        static if (is(typeof(_input[ulong.max .. ulong.max])))
            private alias opSlice_t = ulong;
        else
            private alias opSlice_t = uint;

        static if (hasLength!R)
        {
            auto opSlice(opSlice_t low, opSlice_t high)
            {
                return typeof(this)(_input[low .. high]);
            }
        }
        else static if (is(typeof(_input[opSlice_t.max .. $])))
        {
            struct DollarToken{}
            enum opDollar = DollarToken.init;
            auto opSlice(opSlice_t low, DollarToken)
            {
                return typeof(this)(_input[low .. $]);
            }

            auto opSlice(opSlice_t low, opSlice_t high)
            {
                import std.range : takeExactly;
                return this[low .. $].takeExactly(high - low);
            }
        }
    }

    static if (isForwardRange!R)
    {
        @property auto save()
        {
            return typeof(this)(_input.save);
        }
    }
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : to;
    import std.functional : adjoin;

    alias stringize = map!(to!string);
    assert(equal(stringize([ 1, 2, 3, 4 ]), [ "1", "2", "3", "4" ]));

    uint counter;
    alias count = map!((a) { return counter++; });
    assert(equal(count([ 10, 2, 30, 4 ]), [ 0, 1, 2, 3 ]));

    counter = 0;
    adjoin!((a) { return counter++; }, (a) { return counter++; })(1);
    alias countAndSquare = map!((a) { return counter++; }, (a) { return counter++; });
    //assert(equal(countAndSquare([ 10, 2 ]), [ tuple(0u, 100), tuple(1u, 4) ]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.ascii : toUpper;
    import std.internal.test.dummyrange;
    import std.range;
    import std.typecons : tuple;
    import std.random : uniform, Random = Xorshift;

    int[] arr1 = [ 1, 2, 3, 4 ];
    const int[] arr1Const = arr1;
    int[] arr2 = [ 5, 6 ];
    auto squares = map!("a * a")(arr1Const);
    assert(squares[$ - 1] == 16);
    assert(equal(squares, [ 1, 4, 9, 16 ][]));
    assert(equal(map!("a * a")(chain(arr1, arr2)), [ 1, 4, 9, 16, 25, 36 ][]));

    // Test the caching stuff.
    assert(squares.back == 16);
    auto squares2 = squares.save;
    assert(squares2.back == 16);

    assert(squares2.front == 1);
    squares2.popFront();
    assert(squares2.front == 4);
    squares2.popBack();
    assert(squares2.front == 4);
    assert(squares2.back == 9);

    assert(equal(map!("a * a")(chain(arr1, arr2)), [ 1, 4, 9, 16, 25, 36 ][]));

    uint i;
    foreach (e; map!("a", "a * a")(arr1))
    {
        assert(e[0] == ++i);
        assert(e[1] == i * i);
    }

    // Test length.
    assert(squares.length == 4);
    assert(map!"a * a"(chain(arr1, arr2)).length == 6);

    // Test indexing.
    assert(squares[0] == 1);
    assert(squares[1] == 4);
    assert(squares[2] == 9);
    assert(squares[3] == 16);

    // Test slicing.
    auto squareSlice = squares[1 .. squares.length - 1];
    assert(equal(squareSlice, [4, 9][]));
    assert(squareSlice.back == 9);
    assert(squareSlice[1] == 9);

    // Test on a forward range to make sure it compiles when all the fancy
    // stuff is disabled.
    auto fibsSquares = map!"a * a"(recurrence!("a[n-1] + a[n-2]")(1, 1));
    assert(fibsSquares.front == 1);
    fibsSquares.popFront();
    fibsSquares.popFront();
    assert(fibsSquares.front == 4);
    fibsSquares.popFront();
    assert(fibsSquares.front == 9);

    auto repeatMap = map!"a"(repeat(1));
    auto gen = Random(123_456_789);
    auto index = uniform(0, 1024, gen);
    static assert(isInfinite!(typeof(repeatMap)));
    assert(repeatMap[index] == 1);

    auto intRange = map!"a"([1,2,3]);
    static assert(isRandomAccessRange!(typeof(intRange)));
    assert(equal(intRange, [1, 2, 3]));

    foreach (DummyType; AllDummyRanges)
    {
        DummyType d;
        auto m = map!"a * a"(d);

        static assert(propagatesRangeType!(typeof(m), DummyType));
        assert(equal(m, [1,4,9,16,25,36,49,64,81,100]));
    }

    //Test string access
    string  s1 = "hello world!";
    dstring s2 = "日本語";
    dstring s3 = "hello world!"d;
    auto ms1 = map!(toUpper)(s1);
    auto ms2 = map!(toUpper)(s2);
    auto ms3 = map!(toUpper)(s3);
    static assert(!is(ms1[0])); //narrow strings can't be indexed
    assert(ms2[0] == '日');
    assert(ms3[0] == 'H');
    static assert(!is(ms1[0 .. 1])); //narrow strings can't be sliced
    assert(equal(ms2[0 .. 2], "日本"w));
    assert(equal(ms3[0 .. 2], "HE"));

    // https://issues.dlang.org/show_bug.cgi?id=5753
    static void voidFun(int) {}
    static int nonvoidFun(int) { return 0; }
    static assert(!__traits(compiles, map!voidFun([1])));
    static assert(!__traits(compiles, map!(voidFun, voidFun)([1])));
    static assert(!__traits(compiles, map!(nonvoidFun, voidFun)([1])));
    static assert(!__traits(compiles, map!(voidFun, nonvoidFun)([1])));
    static assert(!__traits(compiles, map!(a => voidFun(a))([1])));

    // https://issues.dlang.org/show_bug.cgi?id=15480
    auto dd = map!(z => z * z, c => c * c * c)([ 1, 2, 3, 4 ]);
    assert(dd[0] == tuple(1, 1));
    assert(dd[1] == tuple(4, 8));
    assert(dd[2] == tuple(9, 27));
    assert(dd[3] == tuple(16, 64));
    assert(dd.length == 4);
}

// Verify fix for: https://issues.dlang.org/show_bug.cgi?id=16034
@safe unittest
{
    struct One
    {
        int entry = 1;
        @disable this(this);
    }

    One[] ones = [One(), One()];

    import std.algorithm.comparison : equal;

    assert(ones.map!`a.entry + 1`.equal([2, 2]));
}


@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range;
    auto LL = iota(1L, 4L);
    auto m = map!"a*a"(LL);
    assert(equal(m, [1L, 4L, 9L]));
}

@safe unittest
{
    import std.range : iota;

    // https://issues.dlang.org/show_bug.cgi?id=10130 - map of iota with const step.
    const step = 2;
    assert(map!(i => i)(iota(0, 10, step)).walkLength == 5);

    // Need these to all by const to repro the float case, due to the
    // CommonType template used in the float specialization of iota.
    const floatBegin = 0.0;
    const floatEnd = 1.0;
    const floatStep = 0.02;
    assert(map!(i => i)(iota(floatBegin, floatEnd, floatStep)).walkLength == 50);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range;
    //slicing infinites
    auto rr = iota(0, 5).cycle().map!"a * a"();
    alias RR = typeof(rr);
    static assert(hasSlicing!RR);
    rr = rr[6 .. $]; //Advances 1 cycle and 1 unit
    assert(equal(rr[0 .. 5], [1, 4, 9, 16, 0]));
}

@safe unittest
{
    import std.range;
    struct S {int* p;}
    auto m = immutable(S).init.repeat().map!"a".save;
    assert(m.front == immutable(S)(null));
}

// Issue 20928
@safe unittest
{
    struct Always3
    {
        enum empty = false;
        auto save() { return this; }
        long front() { return 3; }
        void popFront() {}
        long opIndex(ulong i) { return 3; }
        long opIndex(ulong i) immutable { return 3; }
    }

    import std.algorithm.iteration : map;
    Always3.init.map!(e => e)[ulong.max];
}

// each
/**
Eagerly iterates over `r` and calls `fun` with _each element.

If no function to call is specified, `each` defaults to doing nothing but
consuming the entire range. `r.front` will be evaluated, but that can be avoided
by specifying a lambda with a `lazy` parameter.

`each` also supports `opApply`-based types, so it works with e.g. $(REF
parallel, std,parallelism).

Normally the entire range is iterated. If partial iteration (early stopping) is
desired, `fun` needs to return a value of type $(REF Flag,
std,typecons)`!"each"` (`Yes.each` to continue iteration, or `No.each` to stop
iteration).

Params:
    fun = function to apply to _each element of the range
    r = range or iterable over which `each` iterates

Returns: `Yes.each` if the entire range was iterated, `No.each` in case of early
stopping.

See_Also: $(REF tee, std,range)
 */
template each(alias fun = "a")
{
    import std.meta : AliasSeq;
    import std.traits : Parameters;
    import std.typecons : Flag, Yes, No;

private:
    alias BinaryArgs = AliasSeq!(fun, "i", "a");

    enum isRangeUnaryIterable(R) =
        is(typeof(unaryFun!fun(R.init.front)));

    enum isRangeBinaryIterable(R) =
        is(typeof(binaryFun!BinaryArgs(0, R.init.front)));

    enum isRangeIterable(R) =
        isInputRange!R &&
        (isRangeUnaryIterable!R || isRangeBinaryIterable!R);

    enum isForeachUnaryIterable(R) =
        is(typeof((R r) {
            foreach (ref a; r)
                cast(void) unaryFun!fun(a);
        }));

    enum isForeachUnaryWithIndexIterable(R) =
        is(typeof((R r) {
            foreach (i, ref a; r)
                cast(void) binaryFun!BinaryArgs(i, a);
        }));

    enum isForeachBinaryIterable(R) =
        is(typeof((R r) {
            foreach (ref a, ref b; r)
                cast(void) binaryFun!fun(a, b);
        }));

    enum isForeachIterable(R) =
        (!isForwardRange!R || isDynamicArray!R) &&
        (isForeachUnaryIterable!R || isForeachBinaryIterable!R ||
         isForeachUnaryWithIndexIterable!R);

public:
    /**
    Params:
        r = range or iterable over which each iterates
     */
    Flag!"each" each(Range)(Range r)
    if (!isForeachIterable!Range && (
        isRangeIterable!Range ||
        __traits(compiles, typeof(r.front).length)))
    {
        static if (isRangeIterable!Range)
        {
            debug(each) pragma(msg, "Using while for ", Range.stringof);
            static if (isRangeUnaryIterable!Range)
            {
                while (!r.empty)
                {
                    static if (!is(typeof(unaryFun!fun(r.front)) == Flag!"each"))
                    {
                        cast(void) unaryFun!fun(r.front);
                    }
                    else
                    {
                        if (unaryFun!fun(r.front) == No.each) return No.each;
                    }

                    r.popFront();
                }
            }
            else // if (isRangeBinaryIterable!Range)
            {
                size_t i = 0;
                while (!r.empty)
                {
                    static if (!is(typeof(binaryFun!BinaryArgs(i, r.front)) == Flag!"each"))
                    {
                        cast(void) binaryFun!BinaryArgs(i, r.front);
                    }
                    else
                    {
                        if (binaryFun!BinaryArgs(i, r.front) == No.each) return No.each;
                    }
                    r.popFront();
                    i++;
                }
            }
        }
        else
        {
            // range interface with >2 parameters.
            for (auto range = r; !range.empty; range.popFront())
            {
                static if (!is(typeof(fun(r.front.expand)) == Flag!"each"))
                {
                    cast(void) fun(range.front.expand);
                }
                else
                {
                    if (fun(range.front.expand)) return No.each;
                }
            }
        }
        return Yes.each;
    }

    /// ditto
    Flag!"each" each(Iterable)(auto ref Iterable r)
    if (isForeachIterable!Iterable ||
        __traits(compiles, Parameters!(Parameters!(r.opApply))))
    {
        static if (isForeachIterable!Iterable)
        {
            static if (isForeachUnaryIterable!Iterable)
            {
                debug(each) pragma(msg, "Using foreach UNARY for ", Iterable.stringof);
                {
                    foreach (ref e; r)
                    {
                        static if (!is(typeof(unaryFun!fun(e)) == Flag!"each"))
                        {
                            cast(void) unaryFun!fun(e);
                        }
                        else
                        {
                            if (unaryFun!fun(e) == No.each) return No.each;
                        }
                    }
                }
            }
            else static if (isForeachBinaryIterable!Iterable)
            {
                debug(each) pragma(msg, "Using foreach BINARY for ", Iterable.stringof);
                foreach (ref a, ref b; r)
                {
                    static if (!is(typeof(binaryFun!fun(a, b)) == Flag!"each"))
                    {
                        cast(void) binaryFun!fun(a, b);
                    }
                    else
                    {
                        if (binaryFun!fun(a, b) == No.each) return No.each;
                    }
                }
            }
            else static if (isForeachUnaryWithIndexIterable!Iterable)
            {
                debug(each) pragma(msg, "Using foreach INDEX for ", Iterable.stringof);
                foreach (i, ref e; r)
                {
                    static if (!is(typeof(binaryFun!BinaryArgs(i, e)) == Flag!"each"))
                    {
                        cast(void) binaryFun!BinaryArgs(i, e);
                    }
                    else
                    {
                        if (binaryFun!BinaryArgs(i, e) == No.each) return No.each;
                    }
                }
            }
            else
            {
                static assert(0, "Invalid foreach iteratable type " ~ Iterable.stringof ~ " met.");
            }
            return Yes.each;
        }
        else
        {
            // opApply with >2 parameters. count the delegate args.
            // only works if it is not templated (otherwise we cannot count the args)
            auto result = Yes.each;
            auto dg(Parameters!(Parameters!(r.opApply)) params)
            {
                static if (!is(typeof(binaryFun!BinaryArgs(i, e)) == Flag!"each"))
                {
                    fun(params);
                    return 0; // tells opApply to continue iteration
                }
                else
                {
                    result = fun(params);
                    return result == Yes.each ? 0 : -1;
                }
            }
            r.opApply(&dg);
            return result;
        }
    }
}

///
@safe unittest
{
    import std.range : iota;
    import std.typecons : No;

    int[] arr;
    iota(5).each!(n => arr ~= n);
    assert(arr == [0, 1, 2, 3, 4]);

    // stop iterating early
    iota(5).each!((n) { arr ~= n; return No.each; });
    assert(arr == [0, 1, 2, 3, 4, 0]);

    // If the range supports it, the value can be mutated in place
    arr.each!((ref n) => n++);
    assert(arr == [1, 2, 3, 4, 5, 1]);

    arr.each!"a++";
    assert(arr == [2, 3, 4, 5, 6, 2]);

    auto m = arr.map!(n => n);
    // by-ref lambdas are not allowed for non-ref ranges
    static assert(!__traits(compiles, m.each!((ref n) => n++)));

    // The default predicate consumes the range
    (&m).each();
    assert(m.empty);
}

/// `each` can pass an index variable for iterable objects which support this
@safe unittest
{
    auto arr = new size_t[4];

    arr.each!"a=i"();
    assert(arr == [0, 1, 2, 3]);

    arr.each!((i, ref e) => e = i * 2);
    assert(arr == [0, 2, 4, 6]);
}

/// opApply iterators work as well
@system unittest
{
    static class S
    {
        int x;
        int opApply(scope int delegate(ref int _x) dg) { return dg(x); }
    }

    auto s = new S;
    s.each!"a++";
    assert(s.x == 1);
}

// binary foreach with two ref args
@system unittest
{
    import std.range : lockstep;

    auto a = [ 1, 2, 3 ];
    auto b = [ 2, 3, 4 ];

    a.lockstep(b).each!((ref x, ref y) { ++x; ++y; });

    assert(a == [ 2, 3, 4 ]);
    assert(b == [ 3, 4, 5 ]);
}

// https://issues.dlang.org/show_bug.cgi?id=15358
// application of `each` with >2 args (opApply)
@system unittest
{
    import std.range : lockstep;
    auto a = [0,1,2];
    auto b = [3,4,5];
    auto c = [6,7,8];

    lockstep(a, b, c).each!((ref x, ref y, ref z) { ++x; ++y; ++z; });

    assert(a == [1,2,3]);
    assert(b == [4,5,6]);
    assert(c == [7,8,9]);
}

// https://issues.dlang.org/show_bug.cgi?id=15358
// application of `each` with >2 args (range interface)
@safe unittest
{
    import std.range : zip;
    auto a = [0,1,2];
    auto b = [3,4,5];
    auto c = [6,7,8];

    int[] res;

    zip(a, b, c).each!((x, y, z) { res ~= x + y + z; });

    assert(res == [9, 12, 15]);
}

// https://issues.dlang.org/show_bug.cgi?id=16255
// `each` on opApply doesn't support ref
@safe unittest
{
    int[] dynamicArray = [1, 2, 3, 4, 5];
    int[5] staticArray = [1, 2, 3, 4, 5];

    dynamicArray.each!((ref x) => x++);
    assert(dynamicArray == [2, 3, 4, 5, 6]);

    staticArray.each!((ref x) => x++);
    assert(staticArray == [2, 3, 4, 5, 6]);

    staticArray[].each!((ref x) => x++);
    assert(staticArray == [3, 4, 5, 6, 7]);
}

// https://issues.dlang.org/show_bug.cgi?id=16255
// `each` on opApply doesn't support ref
@system unittest
{
    struct S
    {
       int x;
       int opApply(int delegate(ref int _x) dg) { return dg(x); }
    }

    S s;
    foreach (ref a; s) ++a;
    assert(s.x == 1);
    s.each!"++a";
    assert(s.x == 2);
}

// https://issues.dlang.org/show_bug.cgi?id=15357
// `each` should behave similar to foreach
@safe unittest
{
    import std.range : iota;

    auto arr = [1, 2, 3, 4];

    // 1 ref parameter
    arr.each!((ref e) => e = 0);
    assert(arr.sum == 0);

    // 1 ref parameter and index
    arr.each!((i, ref e) => e = cast(int) i);
    assert(arr.sum == 4.iota.sum);
}

// https://issues.dlang.org/show_bug.cgi?id=15357
// `each` should behave similar to foreach
@system unittest
{
    import std.range : iota, lockstep;

    // 2 ref parameters and index
    auto arrA = [1, 2, 3, 4];
    auto arrB = [5, 6, 7, 8];
    lockstep(arrA, arrB).each!((ref a, ref b) {
        a = 0;
        b = 1;
    });
    assert(arrA.sum == 0);
    assert(arrB.sum == 4);

    // 3 ref parameters
    auto arrC = [3, 3, 3, 3];
    lockstep(arrA, arrB, arrC).each!((ref a, ref b, ref c) {
        a = 1;
        b = 2;
        c = 3;
    });
    assert(arrA.sum == 4);
    assert(arrB.sum == 8);
    assert(arrC.sum == 12);
}

// https://issues.dlang.org/show_bug.cgi?id=15357
// `each` should behave similar to foreach
@system unittest
{
    import std.range : lockstep;
    import std.typecons : Tuple;

    auto a = "abc";
    auto b = "def";

    // print each character with an index
    {
        alias Element = Tuple!(size_t, "index", dchar, "value");
        Element[] rForeach, rEach;
        foreach (i, c ; a) rForeach ~= Element(i, c);
        a.each!((i, c) => rEach ~= Element(i, c));
        assert(rForeach == rEach);
        assert(rForeach == [Element(0, 'a'), Element(1, 'b'), Element(2, 'c')]);
    }

    // print pairs of characters
    {
        alias Element = Tuple!(dchar, "a", dchar, "b");
        Element[] rForeach, rEach;
        foreach (c1, c2 ; a.lockstep(b)) rForeach ~= Element(c1, c2);
        a.lockstep(b).each!((c1, c2) => rEach ~= Element(c1, c2));
        assert(rForeach == rEach);
        assert(rForeach == [Element('a', 'd'), Element('b', 'e'), Element('c', 'f')]);
    }
}

// filter
/**
`filter!(predicate)(range)` returns a new range containing only elements `x` in `range` for
which `predicate(x)` returns `true`.

The predicate is passed to $(REF unaryFun, std,functional), and can be either a string, or
any callable that can be executed via `pred(element)`.

Params:
    predicate = Function to apply to each element of range

Returns:
    An input range that contains the filtered elements. If `range` is at least a forward range, the return value of `filter`
    will also be a forward range.

See_Also:
    $(HTTP en.wikipedia.org/wiki/Filter_(higher-order_function), Filter (higher-order function)),
    $(REF filterBidirectional, std,algorithm,iteration)
 */
template filter(alias predicate)
if (is(typeof(unaryFun!predicate)))
{
    /**
    Params:
        range = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
        of elements
    Returns:
        A range containing only elements `x` in `range` for
        which `predicate(x)` returns `true`.
     */
    auto filter(Range)(Range range)
    if (isInputRange!(Unqual!Range))
    {
        return FilterResult!(unaryFun!predicate, Range)(range);
    }
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.math.operations : isClose;
    import std.range;

    int[] arr = [ 1, 2, 3, 4, 5 ];

    // Filter below 3
    auto small = filter!(a => a < 3)(arr);
    assert(equal(small, [ 1, 2 ]));

    // Filter again, but with Uniform Function Call Syntax (UFCS)
    auto sum = arr.filter!(a => a < 3);
    assert(equal(sum, [ 1, 2 ]));

    // In combination with chain() to span multiple ranges
    int[] a = [ 3, -2, 400 ];
    int[] b = [ 100, -101, 102 ];
    auto r = chain(a, b).filter!(a => a > 0);
    assert(equal(r, [ 3, 400, 100, 102 ]));

    // Mixing convertible types is fair game, too
    double[] c = [ 2.5, 3.0 ];
    auto r1 = chain(c, a, b).filter!(a => cast(int) a != a);
    assert(isClose(r1, [ 2.5 ]));
}

private struct FilterResult(alias pred, Range)
{
    alias R = Unqual!Range;
    R _input;
    private bool _primed;

    private void prime()
    {
        if (_primed) return;
        while (!_input.empty && !pred(_input.front))
        {
            _input.popFront();
        }
        _primed = true;
    }

    this(R r)
    {
        _input = r;
    }

    private this(R r, bool primed)
    {
        _input = r;
        _primed = primed;
    }

    auto opSlice() { return this; }

    static if (isInfinite!Range)
    {
        enum bool empty = false;
    }
    else
    {
        @property bool empty() { prime; return _input.empty; }
    }

    void popFront()
    {
        prime;
        do
        {
            _input.popFront();
        } while (!_input.empty && !pred(_input.front));
    }

    @property auto ref front()
    {
        prime;
        assert(!empty, "Attempting to fetch the front of an empty filter.");
        return _input.front;
    }

    static if (isForwardRange!R)
    {
        @property auto save()
        {
            return typeof(this)(_input.save, _primed);
        }
    }
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange;
    import std.range;

    auto shouldNotLoop4ever = repeat(1).filter!(x => x % 2 == 0);
    static assert(isInfinite!(typeof(shouldNotLoop4ever)));
    assert(!shouldNotLoop4ever.empty);

    int[] a = [ 3, 4, 2 ];
    auto r = filter!("a > 3")(a);
    static assert(isForwardRange!(typeof(r)));
    assert(equal(r, [ 4 ][]));

    a = [ 1, 22, 3, 42, 5 ];
    auto under10 = filter!("a < 10")(a);
    assert(equal(under10, [1, 3, 5][]));
    static assert(isForwardRange!(typeof(under10)));
    under10.front = 4;
    assert(equal(under10, [4, 3, 5][]));
    under10.front = 40;
    assert(equal(under10, [40, 3, 5][]));
    under10.front = 1;

    auto infinite = filter!"a > 2"(repeat(3));
    static assert(isInfinite!(typeof(infinite)));
    static assert(isForwardRange!(typeof(infinite)));
    assert(infinite.front == 3);

    foreach (DummyType; AllDummyRanges)
    {
        DummyType d;
        auto f = filter!"a & 1"(d);
        assert(equal(f, [1,3,5,7,9]));

        static if (isForwardRange!DummyType)
        {
            static assert(isForwardRange!(typeof(f)));
        }
    }

    // With delegates
    int x = 10;
    int overX(int a) { return a > x; }
    typeof(filter!overX(a)) getFilter()
    {
        return filter!overX(a);
    }
    auto r1 = getFilter();
    assert(equal(r1, [22, 42]));

    // With chain
    auto nums = [0,1,2,3,4];
    assert(equal(filter!overX(chain(a, nums)), [22, 42]));

    // With copying of inner struct Filter to Map
    auto arr = [1,2,3,4,5];
    auto m = map!"a + 1"(filter!"a < 4"(arr));
    assert(equal(m, [2, 3, 4]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    int[] a = [ 3, 4 ];
    const aConst = a;
    auto r = filter!("a > 3")(aConst);
    assert(equal(r, [ 4 ][]));

    a = [ 1, 22, 3, 42, 5 ];
    auto under10 = filter!("a < 10")(a);
    assert(equal(under10, [1, 3, 5][]));
    assert(equal(under10.save, [1, 3, 5][]));
    assert(equal(under10.save, under10));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.functional : compose, pipe;

    assert(equal(compose!(map!"2 * a", filter!"a & 1")([1,2,3,4,5]),
                    [2,6,10]));
    assert(equal(pipe!(filter!"a & 1", map!"2 * a")([1,2,3,4,5]),
            [2,6,10]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    int x = 10;
    int underX(int a) { return a < x; }
    const(int)[] list = [ 1, 2, 10, 11, 3, 4 ];
    assert(equal(filter!underX(list), [ 1, 2, 3, 4 ]));
}

// https://issues.dlang.org/show_bug.cgi?id=19823
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : dropOne;

    auto a = [1, 2, 3, 4];
    assert(a.filter!(a => a != 1).dropOne.equal([3, 4]));
    assert(a.filter!(a => a != 2).dropOne.equal([3, 4]));
    assert(a.filter!(a => a != 3).dropOne.equal([2, 4]));
    assert(a.filter!(a => a != 4).dropOne.equal([2, 3]));
    assert(a.filter!(a => a == 1).dropOne.empty);
    assert(a.filter!(a => a == 2).dropOne.empty);
    assert(a.filter!(a => a == 3).dropOne.empty);
    assert(a.filter!(a => a == 4).dropOne.empty);
}

/**
 * Similar to `filter`, except it defines a
 * $(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives).
 * There is a speed disadvantage - the constructor spends time
 * finding the last element in the range that satisfies the filtering
 * condition (in addition to finding the first one). The advantage is
 * that the filtered range can be spanned from both directions. Also,
 * $(REF retro, std,range) can be applied against the filtered range.
 *
 * The predicate is passed to $(REF unaryFun, std,functional), and can either
 * accept a string, or any callable that can be executed via `pred(element)`.
 *
 * Params:
 *     pred = Function to apply to each element of range
 */
template filterBidirectional(alias pred)
{
    /**
    Params:
        r = Bidirectional range of elements
    Returns:
        A range containing only the elements in `r` for which `pred` returns `true`.
     */
    auto filterBidirectional(Range)(Range r)
    if (isBidirectionalRange!(Unqual!Range))
    {
        return FilterBidiResult!(unaryFun!pred, Range)(r);
    }
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range;

    int[] arr = [ 1, 2, 3, 4, 5 ];
    auto small = filterBidirectional!("a < 3")(arr);
    static assert(isBidirectionalRange!(typeof(small)));
    assert(small.back == 2);
    assert(equal(small, [ 1, 2 ]));
    assert(equal(retro(small), [ 2, 1 ]));
    // In combination with chain() to span multiple ranges
    int[] a = [ 3, -2, 400 ];
    int[] b = [ 100, -101, 102 ];
    auto r = filterBidirectional!("a > 0")(chain(a, b));
    assert(r.back == 102);
}

private struct FilterBidiResult(alias pred, Range)
{
    alias R = Unqual!Range;
    R _input;

    this(R r)
    {
        _input = r;
        while (!_input.empty && !pred(_input.front)) _input.popFront();
        while (!_input.empty && !pred(_input.back)) _input.popBack();
    }

    @property bool empty() { return _input.empty; }

    void popFront()
    {
        do
        {
            _input.popFront();
        } while (!_input.empty && !pred(_input.front));
    }

    @property auto ref front()
    {
        assert(!empty, "Attempting to fetch the front of an empty filterBidirectional.");
        return _input.front;
    }

    void popBack()
    {
        do
        {
            _input.popBack();
        } while (!_input.empty && !pred(_input.back));
    }

    @property auto ref back()
    {
        assert(!empty, "Attempting to fetch the back of an empty filterBidirectional.");
        return _input.back;
    }

    @property auto save()
    {
        return typeof(this)(_input.save);
    }
}

/**
Groups consecutively equivalent elements into a single tuple of the element and
the number of its repetitions.

Similarly to `uniq`, `group` produces a range that iterates over unique
consecutive elements of the given range. Each element of this range is a tuple
of the element and the number of times it is repeated in the original range.
Equivalence of elements is assessed by using the predicate `pred`, which
defaults to `"a == b"`.  The predicate is passed to $(REF binaryFun, std,functional),
and can either accept a string, or any callable that can be executed via
`pred(element, element)`.

Params:
    pred = Binary predicate for determining equivalence of two elements.
    R = The range type
    r = The $(REF_ALTTEXT input range, isInputRange, std,range,primitives) to
        iterate over.

Returns: A range of elements of type `Tuple!(ElementType!R, uint)`,
representing each consecutively unique element and its respective number of
occurrences in that run.  This will be an input range if `R` is an input
range, and a forward range in all other cases.

See_Also: $(LREF chunkBy), which chunks an input range into subranges
    of equivalent adjacent elements.
*/
Group!(pred, Range) group(alias pred = "a == b", Range)(Range r)
{
    return typeof(return)(r);
}

/// ditto
struct Group(alias pred, R)
if (isInputRange!R)
{
    import std.typecons : Rebindable, tuple, Tuple;

    private alias comp = binaryFun!pred;

    private alias E = ElementType!R;
    static if ((is(E == class) || is(E == interface)) &&
               (is(E == const) || is(E == immutable)))
    {
        private alias MutableE = Rebindable!E;
    }
    else static if (is(E : Unqual!E))
    {
        private alias MutableE = Unqual!E;
    }
    else
    {
        private alias MutableE = E;
    }

    private R _input;
    private Tuple!(MutableE, uint) _current;

    ///
    this(R input)
    {
        _input = input;
        if (!_input.empty) popFront();
    }

    private this(R input, Tuple!(MutableE, uint) current)
    {
        _input = input;
        _current = current;
    }

    ///
    void popFront()
    {
        if (_input.empty)
        {
            _current[1] = 0;
        }
        else
        {
            _current = tuple(_input.front, 1u);
            _input.popFront();
            while (!_input.empty && comp(_current[0], _input.front))
            {
                ++_current[1];
                _input.popFront();
            }
        }
    }

    static if (isInfinite!R)
    {
        ///
        enum bool empty = false;  // Propagate infiniteness.
    }
    else
    {
        ///
        @property bool empty()
        {
            return _current[1] == 0;
        }
    }

    /// Returns: the front of the range
    @property auto ref front()
    {
        assert(!empty, "Attempting to fetch the front of an empty Group.");
        return _current;
    }

    static if (isForwardRange!R)
    {
        ///
        @property typeof(this) save()
        {
            return Group(_input.save, _current);
        }
    }
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : tuple, Tuple;

    int[] arr = [ 1, 2, 2, 2, 2, 3, 4, 4, 4, 5 ];
    assert(equal(group(arr), [ tuple(1, 1u), tuple(2, 4u), tuple(3, 1u),
        tuple(4, 3u), tuple(5, 1u) ][]));
}

/**
 * Using group, an associative array can be easily generated with the count of each
 * unique element in the range.
 */
@safe unittest
{
    import std.algorithm.sorting : sort;
    import std.array : assocArray;

    uint[string] result;
    auto range = ["a", "b", "a", "c", "b", "c", "c", "d", "e"];
    result = range.sort!((a, b) => a < b)
        .group
        .assocArray;

    assert(result == ["a": 2U, "b": 2U, "c": 3U, "d": 1U, "e": 1U]);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange;
    import std.typecons : tuple, Tuple;

    int[] arr = [ 1, 2, 2, 2, 2, 3, 4, 4, 4, 5 ];
    assert(equal(group(arr), [ tuple(1, 1u), tuple(2, 4u), tuple(3, 1u),
                            tuple(4, 3u), tuple(5, 1u) ][]));
    static assert(isForwardRange!(typeof(group(arr))));

    foreach (DummyType; AllDummyRanges)
    {
        DummyType d;
        auto g = group(d);

        static assert(d.rt == RangeType.Input || isForwardRange!(typeof(g)));

        assert(equal(g, [tuple(1, 1u), tuple(2, 1u), tuple(3, 1u), tuple(4, 1u),
            tuple(5, 1u), tuple(6, 1u), tuple(7, 1u), tuple(8, 1u),
            tuple(9, 1u), tuple(10, 1u)]));
    }
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : tuple;

    // https://issues.dlang.org/show_bug.cgi?id=13857
    immutable(int)[] a1 = [1,1,2,2,2,3,4,4,5,6,6,7,8,9,9,9];
    auto g1 = group(a1);
    assert(equal(g1, [ tuple(1, 2u), tuple(2, 3u), tuple(3, 1u),
                       tuple(4, 2u), tuple(5, 1u), tuple(6, 2u),
                       tuple(7, 1u), tuple(8, 1u), tuple(9, 3u)
                     ]));

    // https://issues.dlang.org/show_bug.cgi?id=13162
    immutable(ubyte)[] a2 = [1, 1, 1, 0, 0, 0];
    auto g2 = a2.group;
    assert(equal(g2, [ tuple(1, 3u), tuple(0, 3u) ]));

    // https://issues.dlang.org/show_bug.cgi?id=10104
    const a3 = [1, 1, 2, 2];
    auto g3 = a3.group;
    assert(equal(g3, [ tuple(1, 2u), tuple(2, 2u) ]));

    interface I {}
    static class C : I { override size_t toHash() const nothrow @safe { return 0; } }
    const C[] a4 = [new const C()];
    auto g4 = a4.group!"a is b";
    assert(g4.front[1] == 1);

    immutable I[] a5 = [new immutable C()];
    auto g5 = a5.group!"a is b";
    assert(g5.front[1] == 1);

    const(int[][]) a6 = [[1], [1]];
    auto g6 = a6.group;
    assert(equal(g6.front[0], [1]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : tuple;

    int[] arr = [ 1, 2, 2, 2, 2, 3, 4, 4, 4, 5 ];
    auto r = arr.group;
    assert(r.equal([ tuple(1,1u), tuple(2, 4u), tuple(3, 1u), tuple(4, 3u), tuple(5, 1u) ]));
    r.popFront;
    assert(r.equal([ tuple(2, 4u), tuple(3, 1u), tuple(4, 3u), tuple(5, 1u) ]));
    auto s = r.save;
    r.popFrontN(2);
    assert(r.equal([ tuple(4, 3u), tuple(5, 1u) ]));
    assert(s.equal([ tuple(2, 4u), tuple(3, 1u), tuple(4, 3u), tuple(5, 1u) ]));
    s.popFront;
    auto t = s.save;
    r.popFront;
    s.popFront;
    assert(r.equal([ tuple(5, 1u) ]));
    assert(s.equal([ tuple(4, 3u), tuple(5, 1u) ]));
    assert(t.equal([ tuple(3, 1u), tuple(4, 3u), tuple(5, 1u) ]));
}

// https://issues.dlang.org/show_bug.cgi?id=18657
pure @safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : refRange;
    string s = "foo";
    auto r = refRange(&s).group;
    assert(equal(r.save, "foo".group));
    assert(equal(r, "foo".group));
}

// Used by implementation of chunkBy for non-forward input ranges.
private struct ChunkByChunkImpl(alias pred, Range)
if (isInputRange!Range && !isForwardRange!Range)
{
    alias fun = binaryFun!pred;

    private Range *r;
    private ElementType!Range prev;

    this(ref Range range, ElementType!Range _prev)
    {
        r = &range;
        prev = _prev;
    }

    @property bool empty()
    {
        return r.empty || !fun(prev, r.front);
    }

    @property ElementType!Range front()
    {
        assert(!empty, "Attempting to fetch the front of an empty chunkBy chunk.");
        return r.front;
    }

    void popFront()
    {
        assert(!empty, "Attempting to popFront an empty chunkBy chunk.");
        r.popFront();
    }
}

private template ChunkByImplIsUnary(alias pred, Range)
{
    alias e = lvalueOf!(ElementType!Range);

    static if (is(typeof(binaryFun!pred(e, e)) : bool))
        enum ChunkByImplIsUnary = false;
    else static if (is(typeof(unaryFun!pred(e) == unaryFun!pred(e)) : bool))
        enum ChunkByImplIsUnary = true;
    else
        static assert(0, "chunkBy expects either a binary predicate or "~
                         "a unary predicate on range elements of type: "~
                         ElementType!Range.stringof);
}

// Implementation of chunkBy for non-forward input ranges.
private struct ChunkByImpl(alias pred, Range)
if (isInputRange!Range && !isForwardRange!Range)
{
    enum bool isUnary = ChunkByImplIsUnary!(pred, Range);

    static if (isUnary)
        alias eq = binaryFun!((a, b) => unaryFun!pred(a) == unaryFun!pred(b));
    else
        alias eq = binaryFun!pred;

    private Range r;
    private ElementType!Range _prev;
    private bool openChunk = false;

    this(Range _r)
    {
        r = _r;
        if (!empty)
        {
            // Check reflexivity if predicate is claimed to be an equivalence
            // relation.
            assert(eq(r.front, r.front),
                   "predicate is not reflexive");

            // _prev's type may be a nested struct, so must be initialized
            // directly in the constructor (cannot call savePred()).
            _prev = r.front;
        }
        else
        {
            // We won't use _prev, but must be initialized.
            _prev = typeof(_prev).init;
        }
    }
    @property bool empty() { return r.empty && !openChunk; }

    @property auto front()
    {
        assert(!empty, "Attempting to fetch the front of an empty chunkBy.");
        openChunk = true;
        static if (isUnary)
        {
            import std.typecons : tuple;
            return tuple(unaryFun!pred(_prev),
                         ChunkByChunkImpl!(eq, Range)(r, _prev));
        }
        else
        {
            return ChunkByChunkImpl!(eq, Range)(r, _prev);
        }
    }

    void popFront()
    {
        assert(!empty, "Attempting to popFront an empty chunkBy.");
        openChunk = false;
        while (!r.empty)
        {
            if (!eq(_prev, r.front))
            {
                _prev = r.front;
                break;
            }
            r.popFront();
        }
    }
}
// Outer range for forward range version of chunkBy
private struct ChunkByOuter(Range, bool eqEquivalenceAssured)
{
    size_t groupNum;
    Range  current;
    Range  next;
    static if (!eqEquivalenceAssured)
    {
        bool nextUpdated;
    }
}

// Inner range for forward range version of chunkBy
private struct ChunkByGroup(alias eq, Range, bool eqEquivalenceAssured)
{
    import std.typecons : RefCounted;

    alias OuterRange = ChunkByOuter!(Range, eqEquivalenceAssured);

    private size_t groupNum;
    static if (eqEquivalenceAssured)
    {
        private Range  start;
    }
    private Range  current;

    // using union prevents RefCounted destructor from propagating @system to
    // user code
    union { private RefCounted!(OuterRange) mothership; }
    private @trusted ref cargo() { return mothership.refCountedPayload; }

    private this(ref RefCounted!(OuterRange) origin)
    {
        () @trusted { mothership = origin; }();
        groupNum = cargo.groupNum;
        current = cargo.current.save;
        assert(!current.empty, "Passed range 'r' must not be empty");

        static if (eqEquivalenceAssured)
        {
            start = cargo.current.save;

            // Check for reflexivity.
            assert(eq(start.front, current.front),
                "predicate is not reflexive");
        }
    }

    // Cannot be a copy constructor due to https://issues.dlang.org/show_bug.cgi?id=22239
    this(this) scope @trusted
    {
        import core.lifetime : emplace;
        // since mothership has to be in a union, we have to manually trigger
        // an increment to the reference count.
        auto temp = mothership;
        mothership = temp;

        // prevents the reference count from falling back with brute force
        emplace(&temp);
    }

    @property bool empty() { return groupNum == size_t.max; }
    @property auto ref front() { return current.front; }

    void popFront()
    {
        static if (!eqEquivalenceAssured)
        {
            auto prevElement = current.front;
        }

        current.popFront();

        static if (eqEquivalenceAssured)
        {
            //this requires transitivity from the predicate.
            immutable nowEmpty = current.empty || !eq(start.front, current.front);
        }
        else
        {
            immutable nowEmpty = current.empty || !eq(prevElement, current.front);
        }


        if (nowEmpty)
        {
            if (groupNum == cargo.groupNum)
            {
                // If parent range hasn't moved on yet, help it along by
                // saving location of start of next Group.
                cargo.next = current.save;
                static if (!eqEquivalenceAssured)
                {
                    cargo.nextUpdated = true;
                }
            }

            groupNum = size_t.max;
        }
    }

    @property auto save()
    {
        auto copy = this;
        copy.current = current.save;
        return copy;
    }

    @trusted ~this()
    {
        mothership.destroy;
    }
}

private enum GroupingOpType{binaryEquivalent, binaryAny, unary}

// Single-pass implementation of chunkBy for forward ranges.
private struct ChunkByImpl(alias pred, alias eq, GroupingOpType opType, Range)
if (isForwardRange!Range)
{
    import std.typecons : RefCounted;

    enum bool eqEquivalenceAssured = opType != GroupingOpType.binaryAny;
    alias OuterRange = ChunkByOuter!(Range, eqEquivalenceAssured);
    alias InnerRange = ChunkByGroup!(eq, Range, eqEquivalenceAssured);

    static assert(isForwardRange!InnerRange);

    // using union prevents RefCounted destructor from propagating @system to
    // user code
    union { private RefCounted!OuterRange _impl; }
    private @trusted ref impl() { return _impl; }
    private @trusted ref implPL() { return _impl.refCountedPayload; }

    this(Range r)
    {
        import core.lifetime : move;

        auto savedR = r.save;

        static if (eqEquivalenceAssured) () @trusted
        {
            _impl = RefCounted!OuterRange(0, r, savedR.move);
        }();
        else () @trusted
        {
            _impl = RefCounted!OuterRange(0, r, savedR.move, false);
        }();
    }

    // Cannot be a copy constructor due to https://issues.dlang.org/show_bug.cgi?id=22239
    this(this) scope @trusted
    {
        import core.lifetime : emplace;
        // since _impl has to be in a union, we have to manually trigger
        // an increment to the reference count.
        auto temp = _impl;
        _impl = temp;

        // prevents the reference count from falling back with brute force
        emplace(&temp);
    }

    @property bool empty() { return implPL.current.empty; }

    static if (opType == GroupingOpType.unary) @property auto front()
    {
        import std.typecons : tuple;

        return tuple(unaryFun!pred(implPL.current.front), InnerRange(impl));
    }
    else @property auto front()
    {
        return InnerRange(impl);
    }

    static if (eqEquivalenceAssured) void popFront()
    {
        // Scan for next group. If we're lucky, one of our Groups would have
        // already set .next to the start of the next group, in which case the
        // loop is skipped.
        while (!implPL.next.empty && eq(implPL.current.front, implPL.next.front))
        {
            implPL.next.popFront();
        }

        implPL.current = implPL.next.save;

        // Indicate to any remaining Groups that we have moved on.
        implPL.groupNum++;
    }
    else void popFront()
    {
        if (implPL.nextUpdated)
        {
            implPL.current = implPL.next.save;
        }
        else while (true)
        {
            auto prevElement = implPL.current.front;
            implPL.current.popFront();
            if (implPL.current.empty) break;
            if (!eq(prevElement, implPL.current.front)) break;
        }

        implPL.nextUpdated = false;
        // Indicate to any remaining Groups that we have moved on.
        implPL.groupNum++;
    }

    @property auto save()
    {
        // Note: the new copy of the range will be detached from any existing
        // satellite Groups, and will not benefit from the .next acceleration.
        return typeof(this)(implPL.current.save);
    }

    static assert(isForwardRange!(typeof(this)), typeof(this).stringof
            ~ " must be a forward range");

    @trusted ~this()
    {
        _impl.destroy;
    }
}

//Test for https://issues.dlang.org/show_bug.cgi?id=14909
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : tuple;
    import std.stdio;
    auto n = 3;
    auto s = [1,2,3].chunkBy!(a => a+n);
    auto t = s.save.map!(x=>x[0]);
    auto u = s.map!(x=>x[1]);
    assert(t.equal([4,5,6]));
    assert(u.equal!equal([[1],[2],[3]]));
}

//Testing inferring @system correctly
@safe unittest
{
    struct DeadlySave
    {
        int front;
        @safe void popFront(){front++;}
        @safe bool empty(){return front >= 5;}
        @system auto save(){return this;}
    }

    auto test1()
    {
        DeadlySave src;
        return src.walkLength;

    }

    auto test2()
    {
        DeadlySave src;
        return src.chunkBy!((a,b) => a % 2 == b % 2).walkLength;
    }

    static assert(isSafe!test1);
    static assert(!isSafe!test2);
}

//Test for https://issues.dlang.org/show_bug.cgi?id=18751
@safe unittest
{
    import std.algorithm.comparison : equal;

    string[] data = [ "abc", "abc", "def" ];
    int[] indices = [ 0, 1, 2 ];

    auto chunks = indices.chunkBy!((i, j) => data[i] == data[j]);
    assert(chunks.equal!equal([ [ 0, 1 ], [ 2 ] ]));
}

//Additional test for fix for issues 14909 and 18751
@safe unittest
{
    import std.algorithm.comparison : equal;
    auto v = [2,4,8,3,6,9,1,5,7];
    auto i = 2;
    assert(v.chunkBy!((a,b) => a % i == b % i).equal!equal([[2,4,8],[3],[6],[9,1,5,7]]));
}

@system unittest
{
    import std.algorithm.comparison : equal;

    size_t popCount = 0;
    static class RefFwdRange
    {
        int[]  impl;
        size_t* pcount;

        @safe nothrow:

        this(int[] data, size_t* pcount) { impl = data; this.pcount = pcount; }
        @property bool empty() { return impl.empty; }
        @property auto ref front() { return impl.front; }
        void popFront()
        {
            impl.popFront();
            (*pcount)++;
        }
        @property auto save() { return new RefFwdRange(impl, pcount); }
    }
    static assert(isForwardRange!RefFwdRange);

    auto testdata = new RefFwdRange([1, 3, 5, 2, 4, 7, 6, 8, 9], &popCount);
    auto groups = testdata.chunkBy!((a,b) => (a % 2) == (b % 2));
    auto outerSave1 = groups.save;

    // Sanity test
    assert(groups.equal!equal([[1, 3, 5], [2, 4], [7], [6, 8], [9]]));
    assert(groups.empty);

    // Performance test for single-traversal use case: popFront should not have
    // been called more times than there are elements if we traversed the
    // segmented range exactly once.
    assert(popCount == 9);

    // Outer range .save test
    groups = outerSave1.save;
    assert(!groups.empty);

    // Inner range .save test
    auto grp1 = groups.front.save;
    auto grp1b = grp1.save;
    assert(grp1b.equal([1, 3, 5]));
    assert(grp1.save.equal([1, 3, 5]));

    // Inner range should remain consistent after outer range has moved on.
    groups.popFront();
    assert(grp1.save.equal([1, 3, 5]));

    // Inner range should not be affected by subsequent inner ranges.
    assert(groups.front.equal([2, 4]));
    assert(grp1.save.equal([1, 3, 5]));
}

/**
 * Chunks an input range into subranges of equivalent adjacent elements.
 * In other languages this is often called `partitionBy`, `groupBy`
 * or `sliceWhen`.
 *
 * Equivalence is defined by the predicate `pred`, which can be either
 * binary, which is passed to $(REF binaryFun, std,functional), or unary, which is
 * passed to $(REF unaryFun, std,functional). In the binary form, two range elements
 * `a` and `b` are considered equivalent if `pred(a,b)` is true. In
 * unary form, two elements are considered equivalent if `pred(a) == pred(b)`
 * is true.
 *
 * This predicate must be an equivalence relation, that is, it must be
 * reflexive (`pred(x,x)` is always true), symmetric
 * (`pred(x,y) == pred(y,x)`), and transitive (`pred(x,y) && pred(y,z)`
 * implies `pred(x,z)`). If this is not the case, the range returned by
 * chunkBy may assert at runtime or behave erratically. Use $(LREF splitWhen)
 * if you want to chunk by a predicate that is not an equivalence relation.
 *
 * Params:
 *  pred = Predicate for determining equivalence.
 *  r = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) to be chunked.
 *
 * Returns: With a binary predicate, a range of ranges is returned in which
 * all elements in a given subrange are equivalent under the given predicate.
 * With a unary predicate, a range of tuples is returned, with the tuple
 * consisting of the result of the unary predicate for each subrange, and the
 * subrange itself. Copying the range currently has reference semantics, but this may
 * change in the future.
 *
 * Notes:
 *
 * Equivalent elements separated by an intervening non-equivalent element will
 * appear in separate subranges; this function only considers adjacent
 * equivalence. Elements in the subranges will always appear in the same order
 * they appear in the original range.
 *
 * See_also:
 * $(LREF group), which collapses adjacent equivalent elements into a single
 * element.
 */
auto chunkBy(alias pred, Range)(Range r)
if (isInputRange!Range)
{
    static if (ChunkByImplIsUnary!(pred, Range))
    {
        enum opType = GroupingOpType.unary;
        alias eq = binaryFun!((a, b) => unaryFun!pred(a) == unaryFun!pred(b));
    }
    else
    {
        enum opType = GroupingOpType.binaryEquivalent;
        alias eq = binaryFun!pred;
    }
    static if (isForwardRange!Range)
        return ChunkByImpl!(pred, eq, opType, Range)(r);
    else
        return ChunkByImpl!(pred, Range)(r);
}

/// Showing usage with binary predicate:
@safe unittest
{
    import std.algorithm.comparison : equal;

    // Grouping by particular attribute of each element:
    auto data = [
        [1, 1],
        [1, 2],
        [2, 2],
        [2, 3]
    ];

    auto r1 = data.chunkBy!((a,b) => a[0] == b[0]);
    assert(r1.equal!equal([
        [[1, 1], [1, 2]],
        [[2, 2], [2, 3]]
    ]));

    auto r2 = data.chunkBy!((a,b) => a[1] == b[1]);
    assert(r2.equal!equal([
        [[1, 1]],
        [[1, 2], [2, 2]],
        [[2, 3]]
    ]));
}

/// Showing usage with unary predicate:
/* FIXME: pure nothrow*/ @safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range.primitives;
    import std.typecons : tuple;

    // Grouping by particular attribute of each element:
    auto range =
    [
        [1, 1],
        [1, 1],
        [1, 2],
        [2, 2],
        [2, 3],
        [2, 3],
        [3, 3]
    ];

    auto byX = chunkBy!(a => a[0])(range);
    auto expected1 =
    [
        tuple(1, [[1, 1], [1, 1], [1, 2]]),
        tuple(2, [[2, 2], [2, 3], [2, 3]]),
        tuple(3, [[3, 3]])
    ];
    foreach (e; byX)
    {
        assert(!expected1.empty);
        assert(e[0] == expected1.front[0]);
        assert(e[1].equal(expected1.front[1]));
        expected1.popFront();
    }

    auto byY = chunkBy!(a => a[1])(range);
    auto expected2 =
    [
        tuple(1, [[1, 1], [1, 1]]),
        tuple(2, [[1, 2], [2, 2]]),
        tuple(3, [[2, 3], [2, 3], [3, 3]])
    ];
    foreach (e; byY)
    {
        assert(!expected2.empty);
        assert(e[0] == expected2.front[0]);
        assert(e[1].equal(expected2.front[1]));
        expected2.popFront();
    }
}

/*FIXME: pure @safe nothrow*/ @system unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : tuple;

    struct Item { int x, y; }

    // Force R to have only an input range API with reference semantics, so
    // that we're not unknowingly making use of array semantics outside of the
    // range API.
    class RefInputRange(R)
    {
        R data;
        this(R _data) pure @safe nothrow { data = _data; }
        @property bool empty() pure @safe nothrow { return data.empty; }
        @property auto front() pure @safe nothrow { assert(!empty); return data.front; }
        void popFront() pure @safe nothrow { assert(!empty); data.popFront(); }
    }
    auto refInputRange(R)(R range) { return new RefInputRange!R(range); }

    // An input range API with value semantics.
    struct ValInputRange(R)
    {
        R data;
        this(R _data) pure @safe nothrow { data = _data; }
        @property bool empty() pure @safe nothrow { return data.empty; }
        @property auto front() pure @safe nothrow { assert(!empty); return data.front; }
        void popFront() pure @safe nothrow { assert(!empty); data.popFront(); }
    }
    auto valInputRange(R)(R range) { return ValInputRange!R(range); }

    {
        auto arr = [ Item(1,2), Item(1,3), Item(2,3) ];
        static assert(isForwardRange!(typeof(arr)));

        auto byX = chunkBy!(a => a.x)(arr);
        static assert(isForwardRange!(typeof(byX)));

        auto byX_subrange1 = byX.front[1].save;
        auto byX_subrange2 = byX.front[1].save;
        static assert(isForwardRange!(typeof(byX_subrange1)));
        static assert(isForwardRange!(typeof(byX_subrange2)));

        byX.popFront();
        assert(byX_subrange1.equal([ Item(1,2), Item(1,3) ]));
        byX_subrange1.popFront();
        assert(byX_subrange1.equal([ Item(1,3) ]));
        assert(byX_subrange2.equal([ Item(1,2), Item(1,3) ]));

        auto byY = chunkBy!(a => a.y)(arr);
        static assert(isForwardRange!(typeof(byY)));

        auto byY2 = byY.save;
        static assert(is(typeof(byY) == typeof(byY2)));
        byY.popFront();
        assert(byY.front[0] == 3);
        assert(byY.front[1].equal([ Item(1,3), Item(2,3) ]));
        assert(byY2.front[0] == 2);
        assert(byY2.front[1].equal([ Item(1,2) ]));
    }

    // Test non-forward input ranges with reference semantics.
    {
        auto range = refInputRange([ Item(1,1), Item(1,2), Item(2,2) ]);
        auto byX = chunkBy!(a => a.x)(range);
        assert(byX.front[0] == 1);
        assert(byX.front[1].equal([ Item(1,1), Item(1,2) ]));
        byX.popFront();
        assert(byX.front[0] == 2);
        assert(byX.front[1].equal([ Item(2,2) ]));
        byX.popFront();
        assert(byX.empty);
        assert(range.empty);

        range = refInputRange([ Item(1,1), Item(1,2), Item(2,2) ]);
        auto byY = chunkBy!(a => a.y)(range);
        assert(byY.front[0] == 1);
        assert(byY.front[1].equal([ Item(1,1) ]));
        byY.popFront();
        assert(byY.front[0] == 2);
        assert(byY.front[1].equal([ Item(1,2), Item(2,2) ]));
        byY.popFront();
        assert(byY.empty);
        assert(range.empty);
    }

    // Test non-forward input ranges with value semantics.
    {
        auto range = valInputRange([ Item(1,1), Item(1,2), Item(2,2) ]);
        auto byX = chunkBy!(a => a.x)(range);
        assert(byX.front[0] == 1);
        assert(byX.front[1].equal([ Item(1,1), Item(1,2) ]));
        byX.popFront();
        assert(byX.front[0] == 2);
        assert(byX.front[1].equal([ Item(2,2) ]));
        byX.popFront();
        assert(byX.empty);
        assert(!range.empty);    // Opposite of refInputRange test

        range = valInputRange([ Item(1,1), Item(1,2), Item(2,2) ]);
        auto byY = chunkBy!(a => a.y)(range);
        assert(byY.front[0] == 1);
        assert(byY.front[1].equal([ Item(1,1) ]));
        byY.popFront();
        assert(byY.front[0] == 2);
        assert(byY.front[1].equal([ Item(1,2), Item(2,2) ]));
        byY.popFront();
        assert(byY.empty);
        assert(!range.empty);    // Opposite of refInputRange test
    }

    /* https://issues.dlang.org/show_bug.cgi?id=19532
     * General behavior of non-forward input ranges.
     *
     * - If the same chunk is retrieved multiple times via front, the separate chunk
     *   instances refer to a shared range segment that advances as a single range.
     * - Emptying a chunk via popFront does not implicitly popFront the chunk off
     *   main range. The chunk is still available via front, it is just empty.
     */
    {
        import std.algorithm.comparison : equal;
        import core.exception : AssertError;
        import std.exception : assertThrown;

        auto a = [[0, 0], [0, 1],
                  [1, 2], [1, 3], [1, 4],
                  [2, 5], [2, 6],
                  [3, 7],
                  [4, 8]];

        // Value input range
        {
            auto r = valInputRange(a).chunkBy!((a, b) => a[0] == b[0]);

            size_t numChunks = 0;
            while (!r.empty)
            {
                ++numChunks;
                auto chunk = r.front;
                while (!chunk.empty)
                {
                    assert(r.front.front[1] == chunk.front[1]);
                    chunk.popFront;
                }
                assert(!r.empty);
                assert(r.front.empty);
                r.popFront;
            }

            assert(numChunks == 5);

            // Now front and popFront should assert.
            bool thrown = false;
            try r.front;
            catch (AssertError) thrown = true;
            assert(thrown);

            thrown = false;
            try r.popFront;
            catch (AssertError) thrown = true;
            assert(thrown);
        }

        // Reference input range
        {
            auto r = refInputRange(a).chunkBy!((a, b) => a[0] == b[0]);

            size_t numChunks = 0;
            while (!r.empty)
            {
                ++numChunks;
                auto chunk = r.front;
                while (!chunk.empty)
                {
                    assert(r.front.front[1] == chunk.front[1]);
                    chunk.popFront;
                }
                assert(!r.empty);
                assert(r.front.empty);
                r.popFront;
            }

            assert(numChunks == 5);

            // Now front and popFront should assert.
            bool thrown = false;
            try r.front;
            catch (AssertError) thrown = true;
            assert(thrown);

            thrown = false;
            try r.popFront;
            catch (AssertError) thrown = true;
            assert(thrown);
        }

        // Ensure that starting with an empty range doesn't create an empty chunk.
        {
            int[] emptyRange = [];

            auto r1 = valInputRange(emptyRange).chunkBy!((a, b) => a == b);
            auto r2 = refInputRange(emptyRange).chunkBy!((a, b) => a == b);

            assert(r1.empty);
            assert(r2.empty);

            bool thrown = false;
            try r1.front;
            catch (AssertError) thrown = true;
            assert(thrown);

            thrown = false;
            try r1.popFront;
            catch (AssertError) thrown = true;
            assert(thrown);

            thrown = false;
            try r2.front;
            catch (AssertError) thrown = true;
            assert(thrown);

            thrown = false;
            try r2.popFront;
            catch (AssertError) thrown = true;
            assert(thrown);
        }
    }

    // https://issues.dlang.org/show_bug.cgi?id=19532 - Using roundRobin/chunkBy
    {
        import std.algorithm.comparison : equal;
        import std.range : roundRobin;

        auto a0 = [0, 1, 3, 6];
        auto a1 = [0, 2, 4, 6, 7];
        auto a2 = [1, 2, 4, 6, 8, 8, 9];

        auto expected =
            [[0, 0], [1, 1], [2, 2], [3], [4, 4], [6, 6, 6], [7], [8, 8], [9]];

        auto r1 = roundRobin(valInputRange(a0), valInputRange(a1), valInputRange(a2))
            .chunkBy!((a, b) => a == b);
        assert(r1.equal!equal(expected));

        auto r2 = roundRobin(refInputRange(a0), refInputRange(a1), refInputRange(a2))
            .chunkBy!((a, b) => a == b);
        assert(r2.equal!equal(expected));

        auto r3 = roundRobin(a0, a1, a2).chunkBy!((a, b) => a == b);
        assert(r3.equal!equal(expected));
    }

    // https://issues.dlang.org/show_bug.cgi?id=19532 - Using merge/chunkBy
    {
        import std.algorithm.comparison : equal;
        import std.algorithm.sorting : merge;

        auto a0 = [2, 3, 5];
        auto a1 = [2, 4, 5];
        auto a2 = [1, 2, 4, 5];

        auto expected = [[1], [2, 2, 2], [3], [4, 4], [5, 5, 5]];

        auto r1 = merge(valInputRange(a0), valInputRange(a1), valInputRange(a2))
            .chunkBy!((a, b) => a == b);
        assert(r1.equal!equal(expected));

        auto r2 = merge(refInputRange(a0), refInputRange(a1), refInputRange(a2))
            .chunkBy!((a, b) => a == b);
        assert(r2.equal!equal(expected));

        auto r3 = merge(a0, a1, a2).chunkBy!((a, b) => a == b);
        assert(r3.equal!equal(expected));
    }

    // https://issues.dlang.org/show_bug.cgi?id=19532 - Using chunkBy/map-fold
    {
        import std.algorithm.comparison : equal;
        import std.algorithm.iteration : fold, map;

        auto a = [0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 6, 6, 6, 7, 8, 8, 9];
        auto expected = [0, 3, 4, 6, 8, 5, 18, 7, 16, 9];

        auto r1 = a
            .chunkBy!((a, b) => a == b)
            .map!(c => c.fold!((a, b) => a + b));
        assert(r1.equal(expected));

        auto r2 = valInputRange(a)
            .chunkBy!((a, b) => a == b)
            .map!(c => c.fold!((a, b) => a + b));
        assert(r2.equal(expected));

        auto r3 = refInputRange(a)
            .chunkBy!((a, b) => a == b)
            .map!(c => c.fold!((a, b) => a + b));
        assert(r3.equal(expected));
    }

    // https://issues.dlang.org/show_bug.cgi?id=16169
    // https://issues.dlang.org/show_bug.cgi?id=17966
    // https://issues.dlang.org/show_bug.cgi?id=19532
    // Using multiwayMerge/chunkBy
    {
        import std.algorithm.comparison : equal;
        import std.algorithm.setops : multiwayMerge;

        {
            auto a0 = [2, 3, 5];
            auto a1 = [2, 4, 5];
            auto a2 = [1, 2, 4, 5];

            auto expected = [[1], [2, 2, 2], [3], [4, 4], [5, 5, 5]];
            auto r = multiwayMerge([a0, a1, a2]).chunkBy!((a, b) => a == b);
            assert(r.equal!equal(expected));
        }
        {
            auto a0 = [2, 3, 5];
            auto a1 = [2, 4, 5];
            auto a2 = [1, 2, 4, 5];

            auto expected = [[1], [2, 2, 2], [3], [4, 4], [5, 5, 5]];
            auto r =
                multiwayMerge([valInputRange(a0), valInputRange(a1), valInputRange(a2)])
                .chunkBy!((a, b) => a == b);
            assert(r.equal!equal(expected));
        }
        {
            auto a0 = [2, 3, 5];
            auto a1 = [2, 4, 5];
            auto a2 = [1, 2, 4, 5];

            auto expected = [[1], [2, 2, 2], [3], [4, 4], [5, 5, 5]];
            auto r =
                multiwayMerge([refInputRange(a0), refInputRange(a1), refInputRange(a2)])
                .chunkBy!((a, b) => a == b);
            assert(r.equal!equal(expected));
        }
    }

    // https://issues.dlang.org/show_bug.cgi?id=20496
    {
        auto r = [1,1,1,2,2,2,3,3,3];
        r.chunkBy!((ref e1, ref e2) => e1 == e2);
    }
}



// https://issues.dlang.org/show_bug.cgi?id=13805
@safe unittest
{
    [""].map!((s) => s).chunkBy!((x, y) => true);
}

/**
Splits a forward range into subranges in places determined by a binary
predicate.

When iterating, one element of `r` is compared with `pred` to the next
element. If `pred` return true, a new subrange is started for the next element.
Otherwise, they are part of the same subrange.

If the elements are compared with an inequality (!=) operator, consider
$(LREF chunkBy) instead, as it's likely faster to execute.

Params:
pred = Predicate for determining where to split. The earlier element in the
source range is always given as the first argument.
r = A $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives) to be split.
Returns: a range of subranges of `r`, split such that within a given subrange,
calling `pred` with any pair of adjacent elements as arguments returns `false`.
Copying the range currently has reference semantics, but this may change in the future.

See_also:
$(LREF splitter), which uses elements as splitters instead of element-to-element
relations.
*/

auto splitWhen(alias pred, Range)(Range r)
if (isForwardRange!Range)
{   import std.functional : not;
    return ChunkByImpl!(not!pred, not!pred, GroupingOpType.binaryAny, Range)(r);
}

///
nothrow pure @safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : dropExactly;
    auto source = [4, 3, 2, 11, 0, -3, -3, 5, 3, 0];

    auto result1 = source.splitWhen!((a,b) => a <= b);
    assert(result1.save.equal!equal([
        [4, 3, 2],
        [11, 0, -3],
        [-3],
        [5, 3, 0]
    ]));

    //splitWhen, like chunkBy, is currently a reference range (this may change
    //in future). Remember to call `save` when appropriate.
    auto result2 = result1.dropExactly(2);
    assert(result1.save.equal!equal([
        [-3],
        [5, 3, 0]
    ]));
}

//ensure we don't iterate the underlying range twice
nothrow @safe unittest
{
    import std.algorithm.comparison : equal;
    import std.math.algebraic : abs;

    struct SomeRange
    {
        int[] elements;
        static int popfrontsSoFar;

        auto front(){return elements[0];}
        nothrow @safe void popFront()
        {   popfrontsSoFar++;
            elements = elements[1 .. $];
        }
        auto empty(){return elements.length == 0;}
        auto save(){return this;}
    }

    auto result = SomeRange([10, 9, 8, 5, 0, 1, 0, 8, 11, 10, 8, 12])
        .splitWhen!((a, b) => abs(a - b) >= 3);

    assert(result.equal!equal([
        [10, 9, 8],
        [5],
        [0, 1, 0],
        [8],
        [11, 10, 8],
        [12]
    ]));

    assert(SomeRange.popfrontsSoFar == 12);
}

// Issue 13595
@safe unittest
{
    import std.algorithm.comparison : equal;
    auto r = [1, 2, 3, 4, 5, 6, 7, 8, 9].splitWhen!((x, y) => ((x*y) % 3) > 0);
    assert(r.equal!equal([
        [1],
        [2, 3, 4],
        [5, 6, 7],
        [8, 9]
    ]));
}

nothrow pure @safe unittest
{
    // Grouping by maximum adjacent difference:
    import std.math.algebraic : abs;
    import std.algorithm.comparison : equal;
    auto r3 = [1, 3, 2, 5, 4, 9, 10].splitWhen!((a, b) => abs(a-b) >= 3);
    assert(r3.equal!equal([
        [1, 3, 2],
        [5, 4],
        [9, 10]
    ]));
}

// empty range splitWhen
@nogc nothrow pure @system unittest
{
    int[1] sliceable;
    auto result = sliceable[0 .. 0].splitWhen!((a,b) => a+b > 10);
    assert(result.empty);
}

// joiner
/**
Lazily joins a range of ranges with a separator. The separator itself
is a range. If a separator is not provided, then the ranges are
joined directly without anything in between them (often called `flatten`
in other languages).

Params:
    r = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) of input
        ranges to be joined.
    sep = A $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives) of
        element(s) to serve as separators in the joined range.

Returns:
A range of elements in the joined range. This will be a bidirectional range if
both outer and inner ranges of `RoR` are at least bidirectional ranges. Else if
both outer and inner ranges of `RoR` are forward ranges, the returned range will
be likewise. Otherwise it will be only an input range. The
$(REF_ALTTEXT range bidirectionality, isBidirectionalRange, std,range,primitives)
is propagated if no separator is specified.

See_also:
$(REF chain, std,range), which chains a sequence of ranges with compatible elements
into a single range.

Note:
When both outer and inner ranges of `RoR` are bidirectional and the joiner is
iterated from the back to the front, the separator will still be consumed from
front to back, even if it is a bidirectional range too.
 */
auto joiner(RoR, Separator)(RoR r, Separator sep)
{
    static assert(isInputRange!RoR, "The type of RoR '", RoR.stringof
            , " must be an InputRange (isInputRange!", RoR.stringof, ").");
    static assert(isInputRange!(ElementType!RoR), "The ElementyType of RoR '"
            , ElementType!(RoR).stringof, "' must be an InputRange "
            , "(isInputRange!(ElementType!(", RoR.stringof , "))).");
    static assert(isForwardRange!Separator, "The type of the Separator '"
            , Separator.stringof, "' must be a ForwardRange (isForwardRange!("
            , Separator.stringof, ")).");
    static assert(is(ElementType!Separator : ElementType!(ElementType!RoR))
            , "The type of the elements of the separator range does not match "
            , "the type of the elements that are joined. Separator type '"
            , ElementType!(Separator).stringof, "' is not implicitly"
            , "convertible to range element type '"
            , ElementType!(ElementType!RoR).stringof, "' (is(ElementType!"
            , Separator.stringof, " : ElementType!(ElementType!", RoR.stringof
            , "))).");

    static struct Result
    {
        private RoR _items;
        private ElementType!RoR _current;
        bool inputStartsWithEmpty = false;
        static if (isBidirectional)
        {
            private ElementType!RoR _currentBack;
            bool inputEndsWithEmpty = false;
        }
        enum isBidirectional = isBidirectionalRange!RoR &&
                               isBidirectionalRange!(ElementType!RoR);
        static if (isRandomAccessRange!Separator)
        {
            static struct CurrentSep
            {
                private Separator _sep;
                private size_t sepIndex;
                private size_t sepLength; // cache the length for performance
                auto front() { return _sep[sepIndex]; }
                void popFront() { sepIndex++; }
                auto empty() { return sepIndex >= sepLength; }
                auto save()
                {
                    auto copy = this;
                    copy._sep = _sep;
                    return copy;
                }
                void reset()
                {
                    sepIndex = 0;
                }

                void initialize(Separator sep)
                {
                    _sep = sep;
                    sepIndex = sepLength = _sep.length;
                }
            }
        }
        else
        {
            static struct CurrentSep
            {
                private Separator _sep;
                Separator payload;

                alias payload this;

                auto save()
                {
                    auto copy = this;
                    copy._sep = _sep;
                    return copy;
                }

                void reset()
                {
                    payload = _sep.save;
                }

                void initialize(Separator sep)
                {
                    _sep = sep;
                }
            }
        }

        private CurrentSep _currentSep;
        static if (isBidirectional)
        {
            private CurrentSep _currentBackSep;
        }

        private void setItem()
        {
            if (!_items.empty)
            {
                // If we're exporting .save, we must not consume any of the
                // subranges, since RoR.save does not guarantee that the states
                // of the subranges are also saved.
                static if (isForwardRange!RoR &&
                           isForwardRange!(ElementType!RoR))
                    _current = _items.front.save;
                else
                    _current = _items.front;
            }
        }

        private void useSeparator()
        {
            // Separator must always come after an item.
            assert(_currentSep.empty,
                    "Attempting to reset a non-empty separator");
            assert(!_items.empty,
                    "Attempting to use a separator in an empty joiner");
            _items.popFront();

            // If there are no more items, we're done, since separators are not
            // terminators.
            if (_items.empty) return;

            if (_currentSep._sep.empty)
            {
                // Advance to the next range in the
                // input
                while (_items.front.empty)
                {
                    _items.popFront();
                    if (_items.empty) return;
                }
                setItem;
            }
            else
            {
                _currentSep.reset;
                assert(!_currentSep.empty, "separator must not be empty");
            }
        }

        this(RoR items, Separator sep)
        {
            _items = items;
            _currentSep.initialize(sep);
            static if (isBidirectional)
                _currentBackSep.initialize(sep);

            //mixin(useItem); // _current should be initialized in place
            if (_items.empty)
            {
                _current = _current.init;   // set invalid state
                static if (isBidirectional)
                    _currentBack = _currentBack.init;
            }
            else
            {
                // If we're exporting .save, we must not consume any of the
                // subranges, since RoR.save does not guarantee that the states
                // of the subranges are also saved.
                static if (isForwardRange!RoR &&
                           isForwardRange!(ElementType!RoR))
                    _current = _items.front.save;
                else
                    _current = _items.front;

                static if (isBidirectional)
                {
                    _currentBack = _items.back.save;

                    if (_currentBack.empty)
                    {
                        // No data in the currentBack item - toggle to use
                        // the separator
                        inputEndsWithEmpty = true;
                    }
                }

                if (_current.empty)
                {
                    // No data in the current item - toggle to use the separator
                    inputStartsWithEmpty = true;

                    // If RoR contains a single empty element,
                    // the returned Result will always be empty
                    import std.range : dropOne;
                    static if (hasLength!RoR)
                    {
                        if (_items.length == 1)
                            _items.popFront;
                    }
                    else static if (isForwardRange!RoR)
                    {
                        if (_items.save.dropOne.empty)
                            _items.popFront;
                    }
                    else
                    {
                        auto _itemsCopy = _items;
                        if (_itemsCopy.dropOne.empty)
                            _items.popFront;
                    }
                }
            }
        }

        @property auto empty()
        {
            return _items.empty;
        }

        //no data in the first item of the initial range - use the separator
        private enum useSepIfFrontIsEmpty = q{
            if (inputStartsWithEmpty)
            {
                useSeparator();
                inputStartsWithEmpty = false;
            }
        };

        @property ElementType!(ElementType!RoR) front()
        {
            mixin(useSepIfFrontIsEmpty);
            if (!_currentSep.empty) return _currentSep.front;
            assert(!_current.empty, "Attempting to fetch the front of an empty joiner.");
            return _current.front;
        }

        void popFront()
        {
            assert(!_items.empty, "Attempting to popFront an empty joiner.");
            // Using separator?
            mixin(useSepIfFrontIsEmpty);

            if (!_currentSep.empty)
            {
                _currentSep.popFront();
                if (_currentSep.empty && !_items.empty)
                {
                    setItem;
                    if (_current.empty)
                    {
                        // No data in the current item - toggle to use the separator
                        useSeparator();
                    }
                }
            }
            else
            {
                // we're using the range
                _current.popFront();
                if (_current.empty)
                    useSeparator();
            }
        }

        static if (isForwardRange!RoR && isForwardRange!(ElementType!RoR))
        {
            @property auto save()
            {
                Result copy = this;
                copy._items = _items.save;
                copy._current = _current.save;
                copy._currentSep = _currentSep.save;
                static if (isBidirectional)
                {
                    copy._currentBack = _currentBack;
                    copy._currentBackSep = _currentBackSep;
                }
                return copy;
            }
        }

        static if (isBidirectional)
        {
            //no data in the last item of the initial range - use the separator
            private enum useSepIfBackIsEmpty = q{
                if (inputEndsWithEmpty)
                {
                    useBackSeparator;
                    inputEndsWithEmpty = false;
                }
            };

            private void setBackItem()
            {
                if (!_items.empty)
                {
                    _currentBack = _items.back.save;
                }
            }

            private void useBackSeparator()
            {
                // Separator must always come after an item.
                assert(_currentBackSep.empty,
                        "Attempting to reset a non-empty separator");
                assert(!_items.empty,
                        "Attempting to use a separator in an empty joiner");
                _items.popBack();

                // If there are no more items, we're done, since separators are not
                // terminators.
                if (_items.empty) return;

                if (_currentBackSep._sep.empty)
                {
                    // Advance to the next range in the
                    // input
                    while (_items.back.empty)
                    {
                        _items.popBack();
                        if (_items.empty) return;
                    }
                    setBackItem;
                }
                else
                {
                    _currentBackSep.reset;
                    assert(!_currentBackSep.empty, "separator must not be empty");
                }
            }

            @property ElementType!(ElementType!RoR) back()
            {
                mixin(useSepIfBackIsEmpty);

                if (!_currentBackSep.empty) return _currentBackSep.front;
                assert(!_currentBack.empty, "Attempting to fetch the back of an empty joiner.");
                return _currentBack.back;
            }

            void popBack()
            {
                assert(!_items.empty, "Attempting to popBack an empty joiner.");

                mixin(useSepIfBackIsEmpty);

                if (!_currentBackSep.empty)
                {
                    _currentBackSep.popFront();
                    if (_currentBackSep.empty && !_items.empty)
                    {
                        setBackItem;
                        if (_currentBack.empty)
                        {
                            // No data in the current item - toggle to use the separator
                            useBackSeparator();
                        }
                    }
                }
                else
                {
                    // we're using the range
                    _currentBack.popBack();
                    if (_currentBack.empty)
                        useBackSeparator();
                }
            }
        }
    }
    return Result(r, sep);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : text;

    assert(["abc", "def"].joiner.equal("abcdef"));
    assert(["Mary", "has", "a", "little", "lamb"]
        .joiner("...")
        .equal("Mary...has...a...little...lamb"));
    assert(["", "abc"].joiner("xyz").equal("xyzabc"));
    assert([""].joiner("xyz").equal(""));
    assert(["", ""].joiner("xyz").equal("xyz"));
}

@safe pure nothrow unittest
{
    //joiner with separator can return a bidirectional range
    assert(isBidirectionalRange!(typeof(["abc", "def"].joiner("..."))));
}

@system unittest
{
    import std.algorithm.comparison : equal;
    import std.range.interfaces;
    import std.range.primitives;
    // joiner() should work for non-forward ranges too.
    auto r = inputRangeObject(["abc", "def"]);
    assert(equal(joiner(r, "xyz"), "abcxyzdef"));
}

@system unittest
{
    import std.algorithm.comparison : equal;
    import std.range;

    // Related to https://issues.dlang.org/show_bug.cgi?id=8061
    auto r = joiner([
        inputRangeObject("abc"),
        inputRangeObject("def"),
    ], "-*-");

    assert(equal(r, "abc-*-def"));

    // Test case where separator is specified but is empty.
    auto s = joiner([
        inputRangeObject("abc"),
        inputRangeObject("def"),
    ], "");

    assert(equal(s, "abcdef"));

    // Test empty separator with some empty elements
    auto t = joiner([
        inputRangeObject("abc"),
        inputRangeObject(""),
        inputRangeObject("def"),
        inputRangeObject(""),
    ], "");

    assert(equal(t, "abcdef"));

    // Test empty elements with non-empty separator
    auto u = joiner([
        inputRangeObject(""),
        inputRangeObject("abc"),
        inputRangeObject(""),
        inputRangeObject("def"),
        inputRangeObject(""),
    ], "+-");

    assert(equal(u, "+-abc+-+-def+-"));

    // https://issues.dlang.org/show_bug.cgi?id=13441: only(x) as separator
    string[][] lines = [null];
    lines
        .joiner(only("b"))
        .array();
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    // Transience correctness test
    struct TransientRange
    {
    @safe:
        int[][] src;
        int[] buf;

        this(int[][] _src)
        {
            src = _src;
            buf.length = 100;
        }
        @property bool empty() { return src.empty; }
        @property int[] front()
        {
            assert(src.front.length <= buf.length);
            buf[0 .. src.front.length] = src.front[0..$];
            return buf[0 .. src.front.length];
        }
        void popFront() { src.popFront(); }
    }

    // Test embedded empty elements
    auto tr1 = TransientRange([[], [1,2,3], [], [4]]);
    assert(equal(joiner(tr1, [0]), [0,1,2,3,0,0,4]));

    // Test trailing empty elements
    auto tr2 = TransientRange([[], [1,2,3], []]);
    assert(equal(joiner(tr2, [0]), [0,1,2,3,0]));

    // Test no empty elements
    auto tr3 = TransientRange([[1,2], [3,4]]);
    assert(equal(joiner(tr3, [0,1]), [1,2,0,1,3,4]));

    // Test consecutive empty elements
    auto tr4 = TransientRange([[1,2], [], [], [], [3,4]]);
    assert(equal(joiner(tr4, [0,1]), [1,2,0,1,0,1,0,1,0,1,3,4]));

    // Test consecutive trailing empty elements
    auto tr5 = TransientRange([[1,2], [3,4], [], []]);
    assert(equal(joiner(tr5, [0,1]), [1,2,0,1,3,4,0,1,0,1]));
}

@safe unittest
{
    static assert(isInputRange!(typeof(joiner([""], ""))));
    static assert(isForwardRange!(typeof(joiner([""], ""))));
}

@safe pure unittest
{
    {
        import std.algorithm.comparison : equal;
        auto r = joiner(["abc", "def", "ghi"], "?!");
        char[] res;
        while (!r.empty)
        {
            res ~= r.back;
            r.popBack;
        }
        assert(res.equal("ihg?!fed?!cba"));
    }

    {
        wchar[] sep = ['Ș', 'Ț'];
        auto r = joiner(["","abc",""],sep);
        wchar[] resFront;
        wchar[] resBack;

        auto rCopy = r.save;
        while (!r.empty)
        {
            resFront ~= r.front;
            r.popFront;
        }

        while (!rCopy.empty)
        {
            resBack ~= rCopy.back;
            rCopy.popBack;
        }

        import std.algorithm.comparison : equal;

        assert(resFront.equal("ȘȚabcȘȚ"));
        assert(resBack.equal("ȘȚcbaȘȚ"));
    }

    {
        import std.algorithm.comparison : equal;
        auto r = [""];
        r.popBack;
        assert(r.joiner("AB").equal(""));
    }

    {
        auto r = ["", "", "", "abc", ""].joiner("../");
        auto rCopy = r.save;

        char[] resFront;
        char[] resBack;

        while (!r.empty)
        {
            resFront ~= r.front;
            r.popFront;
        }

        while (!rCopy.empty)
        {
            resBack ~= rCopy.back;
            rCopy.popBack;
        }

        import std.algorithm.comparison : equal;

        assert(resFront.equal("../../../abc../"));
        assert(resBack.equal("../cba../../../"));
    }

    {
        auto r = ["", "abc", ""].joiner("./");
        auto rCopy = r.save;
        r.popBack;
        rCopy.popFront;

        auto rRev = r.save;
        auto rCopyRev = rCopy.save;

        char[] r1, r2, r3, r4;

        while (!r.empty)
        {
            r1 ~= r.back;
            r.popBack;
        }

        while (!rCopy.empty)
        {
            r2 ~= rCopy.front;
            rCopy.popFront;
        }

        while (!rRev.empty)
        {
            r3 ~= rRev.front;
            rRev.popFront;
        }

        while (!rCopyRev.empty)
        {
            r4 ~= rCopyRev.back;
            rCopyRev.popBack;
        }

        import std.algorithm.comparison : equal;

        assert(r1.equal("/cba./"));
        assert(r2.equal("/abc./"));
        assert(r3.equal("./abc"));
        assert(r4.equal("./cba"));
    }
}

@system unittest
{
    import std.range;
    import std.algorithm.comparison : equal;

    assert(inputRangeObject([""]).joiner("lz").equal(""));
}

@safe pure unittest
{
    struct inputRangeStrings
    {
        private string[] strings;

        string front()
        {
            return strings[0];
        }

        void popFront()
        {
            strings = strings[1..$];
        }

        bool empty() const
        {
           return strings.length == 0;
        }
    }

    auto arr = inputRangeStrings([""]);

    import std.algorithm.comparison : equal;

    assert(arr.joiner("./").equal(""));
}

@safe pure unittest
{
    auto r = joiner(["", "", "abc", "", ""], "");
    char[] res;
    while (!r.empty)
    {
        res ~= r.back;
        r.popBack;
    }

    import std.algorithm.comparison : equal;

    assert(res.equal("cba"));
}

/// Ditto
auto joiner(RoR)(RoR r)
if (isInputRange!RoR && isInputRange!(Unqual!(ElementType!RoR)))
{
    static struct Result
    {
    private:
        RoR _items;
        Unqual!(ElementType!RoR) _current;
        enum isBidirectional = isForwardRange!RoR && isForwardRange!(ElementType!RoR) &&
                               isBidirectionalRange!RoR && isBidirectionalRange!(ElementType!RoR);
        static if (isBidirectional)
        {
            Unqual!(ElementType!RoR) _currentBack;
            bool reachedFinalElement;
        }

        this(RoR items, ElementType!RoR current)
        {
            _items = items;
            _current = current;
            static if (isBidirectional && hasNested!Result)
                _currentBack = typeof(_currentBack).init;
        }

        void replaceCurrent(typeof(_current) current) @trusted
        {
            import core.lifetime : move;

            current.move(_current);
        }

        static if (isBidirectional)
        {
            void replaceCurrentBack(typeof(_currentBack) currentBack) @trusted
            {
                import core.lifetime : move;

                currentBack.move(_currentBack);
            }
        }

    public:
        this(RoR r)
        {
            _items = r;
            // field _current must be initialized in constructor, because it is nested struct
            _current = typeof(_current).init;

            static if (isBidirectional && hasNested!Result)
                _currentBack = typeof(_currentBack).init;
            mixin(popFrontEmptyElements);
            static if (isBidirectional)
                mixin(popBackEmptyElements);
        }
        static if (isInfinite!RoR)
        {
            enum bool empty = false;
        }
        else
        {
            @property auto empty()
            {
                return _items.empty;
            }
        }
        @property auto ref front()
        {
            assert(!empty, "Attempting to fetch the front of an empty joiner.");
            return _current.front;
        }
        void popFront()
        {
            assert(!_current.empty, "Attempting to popFront an empty joiner.");
            _current.popFront();
            if (_current.empty)
            {
                assert(!_items.empty, "Attempting to popFront an empty joiner.");
                _items.popFront();
                mixin(popFrontEmptyElements);
            }
        }

        private enum popFrontEmptyElements = q{
            // Skip over empty subranges.
            while (!_items.empty && _items.front.empty)
            {
                _items.popFront();
            }
            if (!_items.empty)
            {
                // We cannot export .save method unless we ensure subranges are not
                // consumed when a .save'd copy of ourselves is iterated over. So
                // we need to .save each subrange we traverse.
                static if (isForwardRange!RoR && isForwardRange!(ElementType!RoR))
                    replaceCurrent(_items.front.save);
                else
                    replaceCurrent(_items.front);
            }
            else
            {
                replaceCurrent(typeof(_current).init);
            }
        };

        static if (isForwardRange!RoR && isForwardRange!(ElementType!RoR))
        {
            @property auto save()
            {
                // the null check is important if it is a class range, since null.save will segfault; issue #22359
                // could not just compare x is y here without static if due to a compiler assertion failure
                static if (is(typeof(null) : typeof(_current)))
                    auto r = Result(_items.save, _current is null ? null : _current.save);
                else
                    auto r = Result(_items.save, _current.save);
                static if (isBidirectional)
                {
                    static if (is(typeof(null) : typeof(_currentBack)))
                        r.replaceCurrentBack(_currentBack is null ? null : _currentBack.save);
                    else
                        r.replaceCurrentBack(_currentBack.save);
                    r.reachedFinalElement = reachedFinalElement;
                }
                return r;
            }
        }

        static if (hasAssignableElements!(ElementType!RoR))
        {
            @property void front(ElementType!(ElementType!RoR) element)
            {
                assert(!empty, "Attempting to assign to front of an empty joiner.");
                _current.front = element;
            }

            @property void front(ref ElementType!(ElementType!RoR) element)
            {
                assert(!empty, "Attempting to assign to front of an empty joiner.");
                _current.front = element;
            }
        }

        static if (isBidirectional)
        {
            bool checkFinalElement()
            {
                import std.range : dropOne;

                if (reachedFinalElement)
                    return true;

                static if (hasLength!(typeof(_items)))
                {
                    if (_items.length == 1)
                        reachedFinalElement = true;
                }
                else
                {
                    if (_items.save.dropOne.empty)
                        reachedFinalElement = true;
                }

                return false;
            }

            @property auto ref back()
            {
                assert(!empty, "Attempting to fetch the back of an empty joiner.");
                if (reachedFinalElement)
                    return _current.back;
                else
                    return _currentBack.back;
            }

            void popBack()
            {
                assert(!_current.empty, "Attempting to popBack an empty joiner.");
                if (checkFinalElement)
                    _current.popBack();
                else
                    _currentBack.popBack();

                bool isEmpty = reachedFinalElement ? _current.empty : _currentBack.empty;
                if (isEmpty)
                {
                    assert(!_items.empty, "Attempting to popBack an empty joiner.");
                    _items.popBack();
                    mixin(popBackEmptyElements);
                }
            }

            private enum popBackEmptyElements = q{
                // Skip over empty subranges.
                while (!_items.empty && _items.back.empty)
                {
                    _items.popBack();
                }
                if (!_items.empty)
                {
                    checkFinalElement;
                    // We cannot export .save method unless we ensure subranges are not
                    // consumed when a .save'd copy of ourselves is iterated over. So
                    // we need to .save each subrange we traverse.
                    static if (isForwardRange!RoR && isForwardRange!(ElementType!RoR))
                    {
                        if (reachedFinalElement)
                            replaceCurrent(_items.back.save);
                        else
                            replaceCurrentBack(_items.back.save);
                    }
                    else
                    {
                        if (reachedFinalElement)
                            replaceCurrent(_items.back);
                        else
                            replaceCurrentBack(_items.back);
                    }
                }
                else
                {
                    replaceCurrent(typeof(_current).init);
                    replaceCurrentBack(typeof(_currentBack).init);
                }
            };

            static if (hasAssignableElements!(ElementType!RoR))
            {
                @property void back(ElementType!(ElementType!RoR) element)
                {
                    assert(!empty, "Attempting to assign to back of an empty joiner.");
                    if (reachedFinalElement)
                        _current.back = element;
                    else
                        _currentBack.back = element;
                }

                @property void back(ref ElementType!(ElementType!RoR) element)
                {
                    assert(!empty, "Attempting to assign to back of an empty joiner.");
                    if (reachedFinalElement)
                        _current.back = element;
                    else
                        _currentBack.back = element;
                }
            }
        }
    }
    return Result(r);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : repeat;

    assert([""].joiner.equal(""));
    assert(["", ""].joiner.equal(""));
    assert(["", "abc"].joiner.equal("abc"));
    assert(["abc", ""].joiner.equal("abc"));
    assert(["abc", "def"].joiner.equal("abcdef"));
    assert(["Mary", "has", "a", "little", "lamb"].joiner.equal("Maryhasalittlelamb"));
    assert("abc".repeat(3).joiner.equal("abcabcabc"));
}

/// joiner allows in-place mutation!
@safe unittest
{
    import std.algorithm.comparison : equal;
    auto a = [ [1, 2, 3], [42, 43] ];
    auto j = joiner(a);
    j.front = 44;
    assert(a == [ [44, 2, 3], [42, 43] ]);
    assert(equal(j, [44, 2, 3, 42, 43]));
}

/// insert characters fully lazily into a string
@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import std.range : chain, cycle, iota, only, retro, take, zip;
    import std.format : format;

    static immutable number = "12345678";
    static immutable delimiter = ",";
    auto formatted = number.retro
        .zip(3.iota.cycle.take(number.length))
        .map!(z => chain(z[0].only, z[1] == 2 ? delimiter : null))
        .joiner
        .retro;
    static immutable expected = "12,345,678";
    assert(formatted.equal(expected));
}

@safe unittest
{
    import std.range.interfaces : inputRangeObject;
    static assert(isInputRange!(typeof(joiner([""]))));
    static assert(isForwardRange!(typeof(joiner([""]))));
}

@system unittest
{
    // this test is system because the virtual interface call to save
    // is flexible and thus cannot be inferred safe automatically

    // https://issues.dlang.org/show_bug.cgi?id=22359
    import std.range;
    ForwardRange!int bug(int[][] r)
    {
        import std.range : inputRangeObject;
        import std.algorithm.iteration : map, joiner;

        auto range = inputRangeObject(r);

        return range.map!(a =>inputRangeObject(a)).joiner.inputRangeObject;
    }
    auto f = bug([[]]);
    f.save(); // should not segfault
}

@safe unittest
{
    // Initial version of PR #6115 caused a compilation failure for
    // https://github.com/BlackEdder/ggplotd/blob/d4428c08db5ffdc05dfd29690bf7da9073ea1dc5/source/ggplotd/stat.d#L562-L583
    import std.range : zip;
    int[] xCoords = [1, 2, 3];
    int[] yCoords = [4, 5, 6];
    auto coords = zip(xCoords, xCoords[1..$]).map!( (xr) {
            return zip(yCoords, yCoords[1..$]).map!( (yr) {
                    return [
                    [[xr[0], xr[0], xr[1]],
                     [yr[0], yr[1], yr[1]]],
                    [[xr[0], xr[1], xr[1]],
                     [yr[0], yr[0], yr[1]]]
                     ];
            }).joiner;
    }).joiner;
}

@system unittest
{
    import std.algorithm.comparison : equal;
    import std.range.interfaces : inputRangeObject;
    import std.range : retro;

    // https://issues.dlang.org/show_bug.cgi?id=8240
    assert(equal(joiner([inputRangeObject("")]), ""));
    assert(equal(joiner([inputRangeObject("")]).retro, ""));

    // https://issues.dlang.org/show_bug.cgi?id=8792
    auto b = [[1], [2], [3]];
    auto jb = joiner(b);
    auto js = jb.save;
    assert(equal(jb, js));

    auto js2 = jb.save;
    jb.popFront();
    assert(!equal(jb, js));
    assert(equal(js2, js));
    js.popFront();
    assert(equal(jb, js));
    assert(!equal(js2, js));
}

// https://issues.dlang.org/show_bug.cgi?id=19213
@system unittest
{
    auto results = [[1,2], [3,4]].map!(q => q.chunkBy!"a").joiner;
    int i = 1;
    foreach (ref e; results)
        assert(e[0] == i++);
}

/// joiner can be bidirectional
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : retro;

    auto a = [[1, 2, 3], [4, 5]];
    auto j = a.joiner;
    j.back = 44;
    assert(a == [[1, 2, 3], [4, 44]]);
    assert(equal(j.retro, [44, 4, 3, 2, 1]));
}

// bidirectional joiner: test for filtering empty elements
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : retro;

    alias El = (e) => new int(e);
    auto a = [null, [null, El(1), null, El(2), null, El(3), null], null, [null, El(4), null, El(5), null]];
    auto j = a.joiner;

    alias deref = a => a is null ? -1 : *a;
    auto expected = [-1, 5, -1, 4, -1, -1, 3, -1, 2, -1, 1, -1];
    // works with .save.
    assert(j.save.retro.map!deref.equal(expected));
    // and without .save
    assert(j.retro.map!deref.equal(expected));
    assert(j.retro.map!deref.equal(expected));
}

// bidirectional joiner is @nogc
@safe @nogc unittest
{
    import std.algorithm.comparison : equal;
    import std.range : iota, only, retro;

    auto a = only(iota(1, 4), iota(4, 6));
    auto j = a.joiner;
    static immutable expected = [5 , 4, 3, 2, 1];
    assert(equal(j.retro, expected));
}

// bidirectional joiner supports assignment to the back
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : popBackN;

    auto a = [[1, 2, 3], [4, 5]];
    auto j = a.joiner;
    j.back = 55;
    assert(a == [[1, 2, 3], [4, 55]]);
    j.popBackN(2);
    j.back = 33;
    assert(a == [[1, 2, 33], [4, 55]]);
}

// bidirectional joiner works with auto-decoding
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : retro;

    auto a = ["😀😐", "😠"];
    auto j = a.joiner;
    assert(j.retro.equal("😠😐😀"));
}

// test two-side iteration
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : popBackN;

    auto arrs = [
        [[1], [2], [3], [4], [5]],
        [[1], [2, 3, 4], [5]],
        [[1, 2, 3, 4, 5]],
    ];
    foreach (arr; arrs)
    {
        auto a = arr.joiner;
        assert(a.front == 1);
        assert(a.back == 5);
        a.popFront;
        assert(a.front == 2);
        assert(a.back == 5);
        a.popBack;
        assert(a.front == 2);
        assert(a.back == 4);
        a.popFront;
        assert(a.front == 3);
        assert(a.back == 4);
        a.popBack;
        assert(a.front == 3);
        assert(a.back == 3);
        a.popBack;
        assert(a.empty);
    }
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    struct TransientRange
    {
    @safe:
        int[] _buf;
        int[][] _values;
        this(int[][] values)
        {
            _values = values;
            _buf = new int[128];
        }
        @property bool empty()
        {
            return _values.length == 0;
        }
        @property auto front()
        {
            foreach (i; 0 .. _values.front.length)
            {
                _buf[i] = _values[0][i];
            }
            return _buf[0 .. _values.front.length];
        }
        void popFront()
        {
            _values = _values[1 .. $];
        }
    }

    auto rr = TransientRange([[1,2], [3,4,5], [], [6,7]]);

    // Can't use array() or equal() directly because they fail with transient
    // .front.
    int[] result;
    foreach (c; rr.joiner())
    {
        result ~= c;
    }

    assert(equal(result, [1,2,3,4,5,6,7]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.internal : algoFormat;

    struct TransientRange
    {
    @safe:
        dchar[] _buf;
        dstring[] _values;
        this(dstring[] values)
        {
            _buf.length = 128;
            _values = values;
        }
        @property bool empty()
        {
            return _values.length == 0;
        }
        @property auto front()
        {
            foreach (i; 0 .. _values.front.length)
            {
                _buf[i] = _values[0][i];
            }
            return _buf[0 .. _values.front.length];
        }
        void popFront()
        {
            _values = _values[1 .. $];
        }
    }

    auto rr = TransientRange(["abc"d, "12"d, "def"d, "34"d]);

    // Can't use array() or equal() directly because they fail with transient
    // .front.
    dchar[] result;
    foreach (c; rr.joiner())
    {
        result ~= c;
    }

    import std.conv : to;
    assert(equal(result, "abc12def34"d),
        //Convert to string for assert's message
        to!string("Unexpected result: '%s'"d.algoFormat(result)));
}

// https://issues.dlang.org/show_bug.cgi?id=8061
@system unittest
{
    import std.conv : to;
    import std.range.interfaces;

    auto r = joiner([inputRangeObject("ab"), inputRangeObject("cd")]);
    assert(isForwardRange!(typeof(r)));

    auto str = to!string(r);
    assert(str == "abcd");
}

@safe unittest
{
    import std.range : repeat;

    class AssignableRange
    {
    @safe:
        int element;
        @property int front()
        {
            return element;
        }
        alias back = front;

        enum empty = false;

        auto save()
        {
            return this;
        }

        void popFront() {}
        alias popBack = popFront;

        @property void front(int newValue)
        {
            element = newValue;
        }
        alias back = front;
    }

    static assert(isInputRange!AssignableRange);
    static assert(is(ElementType!AssignableRange == int));
    static assert(hasAssignableElements!AssignableRange);
    static assert(!hasLvalueElements!AssignableRange);

    auto range = new AssignableRange();
    assert(range.element == 0);
    {
        auto joined = joiner(repeat(range));
        joined.front = 5;
        assert(range.element == 5);
        assert(joined.front == 5);

        joined.popFront;
        int byRef = 7;
        joined.front = byRef;
        assert(range.element == byRef);
        assert(joined.front == byRef);
    }
    {
        auto joined = joiner(repeat(range));
        joined.back = 5;
        assert(range.element == 5);
        assert(joined.back == 5);

        joined.popBack;
        int byRef = 7;
        joined.back = byRef;
        assert(range.element == byRef);
        assert(joined.back == byRef);
    }
}

// https://issues.dlang.org/show_bug.cgi?id=19850
@safe pure unittest
{
    assert([[0]].joiner.save.back == 0);
}

// https://issues.dlang.org/show_bug.cgi?id=22561
@safe pure unittest
{
    import std.range : only;

    static immutable struct S { int[] array; }
    assert([only(S(null))].joiner.front == S(null));
}

// https://issues.dlang.org/show_bug.cgi?id=22785
@safe unittest
{

    import std.algorithm.iteration : joiner, map;
    import std.array : array;

    static immutable struct S
    {
        int value;
    }

    static immutable struct T
    {
        S[] arr;
    }

    auto range = [T([S(3)]), T([S(4), S(5)])];

    assert(range.map!"a.arr".joiner.array == [S(3), S(4), S(5)]);
}

/++
Implements the homonym function (also known as `accumulate`, $(D
compress), `inject`, or `foldl`) present in various programming
languages of functional flavor. There is also $(LREF fold) which does
the same thing but with the opposite parameter order.
The call `reduce!(fun)(seed, range)` first assigns `seed` to
an internal variable `result`, also called the accumulator.
Then, for each element `x` in `range`, `result = fun(result, x)`
gets evaluated. Finally, `result` is returned.
The one-argument version `reduce!(fun)(range)`
works similarly, but it uses the first element of the range as the
seed (the range must be non-empty).

Returns:
    the accumulated `result`

Params:
    fun = one or more functions

See_Also:
    $(HTTP en.wikipedia.org/wiki/Fold_(higher-order_function), Fold (higher-order function))

    $(LREF fold) is functionally equivalent to $(LREF _reduce) with the argument
    order reversed, and without the need to use $(REF_ALTTEXT `tuple`,tuple,std,typecons)
    for multiple seeds. This makes it easier to use in UFCS chains.

    $(LREF sum) is similar to `reduce!((a, b) => a + b)` that offers
    pairwise summing of floating point numbers.
+/
template reduce(fun...)
if (fun.length >= 1)
{
    import std.meta : staticMap;

    alias binfuns = staticMap!(binaryFun, fun);
    static if (fun.length > 1)
        import std.typecons : tuple, isTuple;

    /++
    No-seed version. The first element of `r` is used as the seed's value.

    For each function `f` in `fun`, the corresponding
    seed type `S` is `Unqual!(typeof(f(e, e)))`, where `e` is an
    element of `r`: `ElementType!R` for ranges,
    and `ForeachType!R` otherwise.

    Once S has been determined, then `S s = e;` and `s = f(s, e);`
    must both be legal.

    Params:
        r = an iterable value as defined by `isIterable`

    Returns:
        the final result of the accumulator applied to the iterable

    Throws: `Exception` if `r` is empty
    +/
    auto reduce(R)(R r)
    if (isIterable!R)
    {
        import std.exception : enforce;
        alias E = Select!(isInputRange!R, ElementType!R, ForeachType!R);
        alias Args = staticMap!(ReduceSeedType!E, binfuns);

        static if (isInputRange!R)
        {
            // no need to throw if range is statically known to be non-empty
            static if (!__traits(compiles,
            {
                static assert(r.length > 0);
            }))
                enforce(!r.empty, "Cannot reduce an empty input range w/o an explicit seed value.");

            Args result = r.front;
            r.popFront();
            return reduceImpl!false(r, result);
        }
        else
        {
            auto result = Args.init;
            return reduceImpl!true(r, result);
        }
    }

    /++
    Seed version. The seed should be a single value if `fun` is a
    single function. If `fun` is multiple functions, then `seed`
    should be a $(REF Tuple, std,typecons), with one field per function in `f`.

    For convenience, if the seed is const, or has qualified fields, then
    `reduce` will operate on an unqualified copy. If this happens
    then the returned type will not perfectly match `S`.

    Use `fold` instead of `reduce` to use the seed version in a UFCS chain.

    Params:
        seed = the initial value of the accumulator
        r = an iterable value as defined by `isIterable`

    Returns:
        the final result of the accumulator applied to the iterable
    +/
    auto reduce(S, R)(S seed, R r)
    if (isIterable!R)
    {
        static if (fun.length == 1)
            return reducePreImpl(r, seed);
        else
        {
            import std.algorithm.internal : algoFormat;
            static assert(isTuple!S, algoFormat("Seed %s should be a Tuple", S.stringof));
            return reducePreImpl(r, seed.expand);
        }
    }

    private auto reducePreImpl(R, Args...)(R r, ref Args args)
    {
        alias Result = staticMap!(Unqual, Args);
        static if (is(Result == Args))
            alias result = args;
        else
            Result result = args;
        return reduceImpl!false(r, result);
    }

    private auto reduceImpl(bool mustInitialize, R, Args...)(R r, ref Args args)
    if (isIterable!R)
    {
        import std.algorithm.internal : algoFormat;
        static assert(Args.length == fun.length,
            algoFormat("Seed %s does not have the correct amount of fields (should be %s)", Args.stringof, fun.length));
        alias E = Select!(isInputRange!R, ElementType!R, ForeachType!R);

        static if (mustInitialize) bool initialized = false;
        foreach (/+auto ref+/ E e; r) // https://issues.dlang.org/show_bug.cgi?id=4707
        {
            foreach (i, f; binfuns)
            {
                static assert(!is(typeof(f(args[i], e))) || is(typeof(args[i] = f(args[i], e))),
                    algoFormat(
                        "Incompatible function/seed/element: %s/%s/%s",
                        fullyQualifiedName!f,
                        Args[i].stringof,
                        E.stringof
                    )
                );
            }

            static if (mustInitialize) if (initialized == false)
            {
                import core.internal.lifetime : emplaceRef;
                foreach (i, f; binfuns)
                    emplaceRef!(Args[i])(args[i], e);
                initialized = true;
                continue;
            }

            foreach (i, f; binfuns)
                args[i] = f(args[i], e);
        }
        static if (mustInitialize)
        // no need to throw if range is statically known to be non-empty
        static if (!__traits(compiles,
        {
            static assert(r.length > 0);
        }))
        {
            if (!initialized)
                throw new Exception("Cannot reduce an empty iterable w/o an explicit seed value.");
        }

        static if (Args.length == 1)
            return args[0];
        else
            return tuple(args);
    }
}

/**
Many aggregate range operations turn out to be solved with `reduce`
quickly and easily. The example below illustrates `reduce`'s
remarkable power and flexibility.
*/
@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.math.operations : isClose;
    import std.range;

    int[] arr = [ 1, 2, 3, 4, 5 ];
    // Sum all elements
    auto sum = reduce!((a,b) => a + b)(0, arr);
    assert(sum == 15);

    // Sum again, using a string predicate with "a" and "b"
    sum = reduce!"a + b"(0, arr);
    assert(sum == 15);

    // Compute the maximum of all elements
    auto largest = reduce!(max)(arr);
    assert(largest == 5);

    // Max again, but with Uniform Function Call Syntax (UFCS)
    largest = arr.reduce!(max);
    assert(largest == 5);

    // Compute the number of odd elements
    auto odds = reduce!((a,b) => a + (b & 1))(0, arr);
    assert(odds == 3);

    // Compute the sum of squares
    auto ssquares = reduce!((a,b) => a + b * b)(0, arr);
    assert(ssquares == 55);

    // Chain multiple ranges into seed
    int[] a = [ 3, 4 ];
    int[] b = [ 100 ];
    auto r = reduce!("a + b")(chain(a, b));
    assert(r == 107);

    // Mixing convertible types is fair game, too
    double[] c = [ 2.5, 3.0 ];
    auto r1 = reduce!("a + b")(chain(a, b, c));
    assert(isClose(r1, 112.5));

    // To minimize nesting of parentheses, Uniform Function Call Syntax can be used
    auto r2 = chain(a, b, c).reduce!("a + b");
    assert(isClose(r2, 112.5));
}

/**
Sometimes it is very useful to compute multiple aggregates in one pass.
One advantage is that the computation is faster because the looping overhead
is shared. That's why `reduce` accepts multiple functions.
If two or more functions are passed, `reduce` returns a
$(REF Tuple, std,typecons) object with one member per passed-in function.
The number of seeds must be correspondingly increased.
*/
@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.math.operations : isClose;
    import std.math.algebraic : sqrt;
    import std.typecons : tuple, Tuple;

    double[] a = [ 3.0, 4, 7, 11, 3, 2, 5 ];
    // Compute minimum and maximum in one pass
    auto r = reduce!(min, max)(a);
    // The type of r is Tuple!(int, int)
    assert(isClose(r[0], 2));  // minimum
    assert(isClose(r[1], 11)); // maximum

    // Compute sum and sum of squares in one pass
    r = reduce!("a + b", "a + b * b")(tuple(0.0, 0.0), a);
    assert(isClose(r[0], 35));  // sum
    assert(isClose(r[1], 233)); // sum of squares
    // Compute average and standard deviation from the above
    auto avg = r[0] / a.length;
    assert(avg == 5);
    auto stdev = sqrt(r[1] / a.length - avg * avg);
    assert(cast(int) stdev == 2);
}

@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.range : chain;
    import std.typecons : tuple, Tuple;

    double[] a = [ 3, 4 ];
    auto r = reduce!("a + b")(0.0, a);
    assert(r == 7);
    r = reduce!("a + b")(a);
    assert(r == 7);
    r = reduce!(min)(a);
    assert(r == 3);
    double[] b = [ 100 ];
    auto r1 = reduce!("a + b")(chain(a, b));
    assert(r1 == 107);

    // two funs
    auto r2 = reduce!("a + b", "a - b")(tuple(0.0, 0.0), a);
    assert(r2[0] == 7 && r2[1] == -7);
    auto r3 = reduce!("a + b", "a - b")(a);
    assert(r3[0] == 7 && r3[1] == -1);

    a = [ 1, 2, 3, 4, 5 ];
    // Stringize with commas
    string rep = reduce!("a ~ `, ` ~ to!(string)(b)")("", a);
    assert(rep[2 .. $] == "1, 2, 3, 4, 5", "["~rep[2 .. $]~"]");
}

@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.exception : assertThrown;
    import std.range : iota;
    import std.typecons : tuple, Tuple;

    // Test the opApply case.
    static struct OpApply
    {
        bool actEmpty;

        int opApply(scope int delegate(ref int) @safe dg)
        {
            int res;
            if (actEmpty) return res;

            foreach (i; 0 .. 100)
            {
                res = dg(i);
                if (res) break;
            }
            return res;
        }
    }

    OpApply oa;
    auto hundredSum = reduce!"a + b"(iota(100));
    assert(reduce!"a + b"(5, oa) == hundredSum + 5);
    assert(reduce!"a + b"(oa) == hundredSum);
    assert(reduce!("a + b", max)(oa) == tuple(hundredSum, 99));
    assert(reduce!("a + b", max)(tuple(5, 0), oa) == tuple(hundredSum + 5, 99));

    // Test for throwing on empty range plus no seed.
    assertThrown(reduce!"a + b"([1, 2][0 .. 0]));

    oa.actEmpty = true;
    assertThrown(reduce!"a + b"(oa));
}

@safe unittest
{
    const float a = 0.0;
    const float[] b = [ 1.2, 3, 3.3 ];
    float[] c = [ 1.2, 3, 3.3 ];
    auto r = reduce!"a + b"(a, b);
    r = reduce!"a + b"(a, c);
    assert(r == 7.5);
}

@safe unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=10408
    // Two-function reduce of a const array.
    import std.algorithm.comparison : max, min;
    import std.typecons : tuple, Tuple;

    const numbers = [10, 30, 20];
    immutable m = reduce!(min)(numbers);
    assert(m == 10);
    immutable minmax = reduce!(min, max)(numbers);
    assert(minmax == tuple(10, 30));
}

@safe unittest
{
    // https://issues.dlang.org/show_bug.cgi?id=10709
    import std.typecons : tuple, Tuple;

    enum foo = "a + 0.5 * b";
    auto r = [0, 1, 2, 3];
    auto r1 = reduce!foo(r);
    auto r2 = reduce!(foo, foo)(r);
    assert(r1 == 3);
    assert(r2 == tuple(3, 3));
}

@safe unittest
{
    static struct OpApply
    {
        int opApply(int delegate(ref int) @safe dg)
        {
            int[] a = [1, 2, 3];

            int res = 0;
            foreach (ref e; a)
            {
                res = dg(e);
                if (res) break;
            }
            return res;
        }
    }
    //test CTFE and functions with context
    int fun(int a, int b) @safe {return a + b + 1;}
    auto foo()
    {
        import std.algorithm.comparison : max;
        import std.typecons : tuple, Tuple;

        auto a = reduce!(fun)([1, 2, 3]);
        auto b = reduce!(fun, fun)([1, 2, 3]);
        auto c = reduce!(fun)(0, [1, 2, 3]);
        auto d = reduce!(fun, fun)(tuple(0, 0), [1, 2, 3]);
        auto e = reduce!(fun)(0, OpApply());
        auto f = reduce!(fun, fun)(tuple(0, 0), OpApply());

        return max(a, b.expand, c, d.expand, e, f.expand);
    }
    auto a = foo();
    assert(a == 9);
    enum b = foo();
    assert(b == 9);
}

@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.typecons : tuple, Tuple;

    //http://forum.dlang.org/post/oghtttkopzjshsuflelk@forum.dlang.org
    //Seed is tuple of const.
    static auto minmaxElement(alias F = min, alias G = max, R)(in R range)
    @safe pure nothrow
    if (isInputRange!R)
    {
        return reduce!(F, G)(tuple(ElementType!R.max,
                                   ElementType!R.min), range);
    }
    assert(minmaxElement([1, 2, 3]) == tuple(1, 3));
}

// https://issues.dlang.org/show_bug.cgi?id=12569
@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.typecons : tuple;
    dchar c = 'a';
    reduce!(min, max)(tuple(c, c), "hello"); // OK
    static assert(!is(typeof(reduce!(min, max)(tuple(c), "hello"))));
    static assert(!is(typeof(reduce!(min, max)(tuple(c, c, c), "hello"))));


    //"Seed dchar should be a Tuple"
    static assert(!is(typeof(reduce!(min, max)(c, "hello"))));
    //"Seed (dchar) does not have the correct amount of fields (should be 2)"
    static assert(!is(typeof(reduce!(min, max)(tuple(c), "hello"))));
    //"Seed (dchar, dchar, dchar) does not have the correct amount of fields (should be 2)"
    static assert(!is(typeof(reduce!(min, max)(tuple(c, c, c), "hello"))));
    //"Incompatible function/seed/element: all(alias pred = "a")/int/dchar"
    static assert(!is(typeof(reduce!all(1, "hello"))));
    static assert(!is(typeof(reduce!(all, all)(tuple(1, 1), "hello"))));
}

// https://issues.dlang.org/show_bug.cgi?id=13304
@safe unittest
{
    int[] data;
    static assert(is(typeof(reduce!((a, b) => a + b)(data))));
    assert(data.length == 0);
}

// https://issues.dlang.org/show_bug.cgi?id=13880
// reduce shouldn't throw if the length is statically known
pure nothrow @safe @nogc unittest
{
    import std.algorithm.comparison : min;
    int[5] arr;
    arr[2] = -1;
    assert(arr.reduce!min == -1);

    int[0] arr0;
    assert(reduce!min(42, arr0) == 42);
}

//Helper for Reduce
private template ReduceSeedType(E)
{
    static template ReduceSeedType(alias fun)
    {
        import std.algorithm.internal : algoFormat;

        alias ReduceSeedType = Unqual!(typeof(fun(lvalueOf!E, lvalueOf!E)));

        //Check the Seed type is useable.
        ReduceSeedType s = ReduceSeedType.init;
        static assert(is(typeof({ReduceSeedType s = lvalueOf!E;})) &&
            is(typeof(lvalueOf!ReduceSeedType = fun(lvalueOf!ReduceSeedType, lvalueOf!E))),
            algoFormat(
                "Unable to deduce an acceptable seed type for %s with element type %s.",
                fullyQualifiedName!fun,
                E.stringof
            )
        );
    }
}


/++
Implements the homonym function (also known as `accumulate`, $(D
compress), `inject`, or `foldl`) present in various programming
languages of functional flavor, iteratively calling one or more predicates.

$(P Each predicate in `fun` must take two arguments:)
* An accumulator value
* An element of the range `r`
$(P Each predicate must return a value which implicitly converts to the
type of the accumulator.)

$(P For a single predicate,
the call `fold!(fun)(range, seed)` will:)

* Use `seed` to initialize an internal variable `result` (also called
  the accumulator).
* For each element `e` in $(D range), evaluate `result = fun(result, e)`.
* Return $(D result).

$(P The one-argument version `fold!(fun)(range)`
works similarly, but it uses the first element of the range as the
seed (the range must be non-empty) and iterates over the remaining
elements.)

Multiple results are produced when using multiple predicates.

Params:
    fun = the predicate function(s) to apply to the elements

See_Also:
    * $(HTTP en.wikipedia.org/wiki/Fold_(higher-order_function), Fold (higher-order function))

    * $(LREF sum) is similar to `fold!((a, b) => a + b)` that offers
      precise summing of floating point numbers.

    * `fold` is functionally equivalent to $(LREF reduce) with the argument order
      reversed, and without the need to use $(REF_ALTTEXT `tuple`,tuple,std,typecons)
      for multiple seeds.
+/
template fold(fun...)
if (fun.length >= 1)
{
    /**
    Params:
        r = the $(REF_ALTTEXT input range, isInputRange, std,range,primitives) to fold
        seeds = the initial values of each accumulator (optional), one for each predicate
    Returns:
        Either the accumulated result for a single predicate, or a
        $(REF_ALTTEXT `Tuple`,Tuple,std,typecons) of results.
     */
    auto fold(R, S...)(R r, S seeds)
    {
        static if (S.length < 2)
        {
            return reduce!fun(seeds, r);
        }
        else
        {
            import std.typecons : tuple;
            return reduce!fun(tuple(seeds), r);
        }
    }
}

///
@safe pure unittest
{
    immutable arr = [1, 2, 3, 4, 5];

    // Sum all elements
    assert(arr.fold!((a, e) => a + e) == 15);

    // Sum all elements with explicit seed
    assert(arr.fold!((a, e) => a + e)(6) == 21);

    import std.algorithm.comparison : min, max;
    import std.typecons : tuple;

    // Compute minimum and maximum at the same time
    assert(arr.fold!(min, max) == tuple(1, 5));

    // Compute minimum and maximum at the same time with seeds
    assert(arr.fold!(min, max)(0, 7) == tuple(0, 7));

    // Can be used in a UFCS chain
    assert(arr.map!(a => a + 1).fold!((a, e) => a + e) == 20);

    // Return the last element of any range
    assert(arr.fold!((a, e) => e) == 5);
}

@safe @nogc pure nothrow unittest
{
    int[1] arr;
    static assert(!is(typeof(arr.fold!())));
    static assert(!is(typeof(arr.fold!(a => a))));
    static assert(is(typeof(arr.fold!((a, b) => a))));
    static assert(is(typeof(arr.fold!((a, b) => a)(1))));
    assert(arr.length == 1);
}

/++
Similar to `fold`, but returns a range containing the successive reduced values.
The call `cumulativeFold!(fun)(range, seed)` first assigns `seed` to an
internal variable `result`, also called the accumulator.
The returned range contains the values `result = fun(result, x)` lazily
evaluated for each element `x` in `range`. Finally, the last element has the
same value as `fold!(fun)(seed, range)`.
The one-argument version `cumulativeFold!(fun)(range)` works similarly, but
it returns the first element unchanged and uses it as seed for the next
elements.
This function is also known as
    $(HTTP en.cppreference.com/w/cpp/algorithm/partial_sum, partial_sum),
    $(HTTP docs.python.org/3/library/itertools.html#itertools.accumulate, accumulate),
    $(HTTP hackage.haskell.org/package/base-4.8.2.0/docs/Prelude.html#v:scanl, scan),
    $(HTTP mathworld.wolfram.com/CumulativeSum.html, Cumulative Sum).

Params:
    fun = one or more functions to use as fold operation

Returns:
    The function returns a range containing the consecutive reduced values. If
    there is more than one `fun`, the element type will be $(REF Tuple,
    std,typecons) containing one element for each `fun`.

See_Also:
    $(HTTP en.wikipedia.org/wiki/Prefix_sum, Prefix Sum)

Note:

    In functional programming languages this is typically called `scan`, `scanl`,
    `scanLeft` or `reductions`.
+/
template cumulativeFold(fun...)
if (fun.length >= 1)
{
    import std.meta : staticMap;
    private alias binfuns = staticMap!(binaryFun, fun);

    /++
    No-seed version. The first element of `r` is used as the seed's value.
    For each function `f` in `fun`, the corresponding seed type `S` is
    `Unqual!(typeof(f(e, e)))`, where `e` is an element of `r`:
    `ElementType!R`.
    Once `S` has been determined, then `S s = e;` and `s = f(s, e);` must
    both be legal.

    Params:
        range = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    Returns:
        a range containing the consecutive reduced values.
    +/
    auto cumulativeFold(R)(R range)
    if (isInputRange!(Unqual!R))
    {
        return cumulativeFoldImpl(range);
    }

    /++
    Seed version. The seed should be a single value if `fun` is a single
    function. If `fun` is multiple functions, then `seed` should be a
    $(REF Tuple, std,typecons), with one field per function in `f`.
    For convenience, if the seed is `const`, or has qualified fields, then
    `cumulativeFold` will operate on an unqualified copy. If this happens
    then the returned type will not perfectly match `S`.

    Params:
        range = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
        seed = the initial value of the accumulator
    Returns:
        a range containing the consecutive reduced values.
    +/
    auto cumulativeFold(R, S)(R range, S seed)
    if (isInputRange!(Unqual!R))
    {
        static if (fun.length == 1)
            return cumulativeFoldImpl(range, seed);
        else
            return cumulativeFoldImpl(range, seed.expand);
    }

    private auto cumulativeFoldImpl(R, Args...)(R range, ref Args args)
    {
        import std.algorithm.internal : algoFormat;

        static assert(Args.length == 0 || Args.length == fun.length,
            algoFormat("Seed %s does not have the correct amount of fields (should be %s)",
                Args.stringof, fun.length));

        static if (args.length)
            alias State = staticMap!(Unqual, Args);
        else
            alias State = staticMap!(ReduceSeedType!(ElementType!R), binfuns);

        foreach (i, f; binfuns)
        {
            static assert(!__traits(compiles, f(args[i], e)) || __traits(compiles,
                    { args[i] = f(args[i], e); }()),
                algoFormat("Incompatible function/seed/element: %s/%s/%s",
                    fullyQualifiedName!f, Args[i].stringof, E.stringof));
        }

        static struct Result
        {
        private:
            R source;
            State state;

            this(R range, ref Args args)
            {
                source = range;
                if (source.empty)
                    return;

                foreach (i, f; binfuns)
                {
                    static if (args.length)
                        state[i] = f(args[i], source.front);
                    else
                        state[i] = source.front;
                }
            }

        public:
            @property bool empty()
            {
                return source.empty;
            }

            @property auto front()
            {
                assert(!empty, "Attempting to fetch the front of an empty cumulativeFold.");
                static if (fun.length > 1)
                {
                    import std.typecons : tuple;
                    return tuple(state);
                }
                else
                {
                    return state[0];
                }
            }

            void popFront()
            {
                assert(!empty, "Attempting to popFront an empty cumulativeFold.");
                source.popFront;

                if (source.empty)
                    return;

                foreach (i, f; binfuns)
                    state[i] = f(state[i], source.front);
            }

            static if (isForwardRange!R)
            {
                @property auto save()
                {
                    auto result = this;
                    result.source = source.save;
                    return result;
                }
            }

            mixin ImplementLength!source;
        }

        return Result(range, args);
    }
}

///
@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.array : array;
    import std.math.operations : isClose;
    import std.range : chain;

    int[] arr = [1, 2, 3, 4, 5];
    // Partial sum of all elements
    auto sum = cumulativeFold!((a, b) => a + b)(arr, 0);
    assert(sum.array == [1, 3, 6, 10, 15]);

    // Partial sum again, using a string predicate with "a" and "b"
    auto sum2 = cumulativeFold!"a + b"(arr, 0);
    assert(sum2.array == [1, 3, 6, 10, 15]);

    // Compute the partial maximum of all elements
    auto largest = cumulativeFold!max(arr);
    assert(largest.array == [1, 2, 3, 4, 5]);

    // Partial max again, but with Uniform Function Call Syntax (UFCS)
    largest = arr.cumulativeFold!max;
    assert(largest.array == [1, 2, 3, 4, 5]);

    // Partial count of odd elements
    auto odds = arr.cumulativeFold!((a, b) => a + (b & 1))(0);
    assert(odds.array == [1, 1, 2, 2, 3]);

    // Compute the partial sum of squares
    auto ssquares = arr.cumulativeFold!((a, b) => a + b * b)(0);
    assert(ssquares.array == [1, 5, 14, 30, 55]);

    // Chain multiple ranges into seed
    int[] a = [3, 4];
    int[] b = [100];
    auto r = cumulativeFold!"a + b"(chain(a, b));
    assert(r.array == [3, 7, 107]);

    // Mixing convertible types is fair game, too
    double[] c = [2.5, 3.0];
    auto r1 = cumulativeFold!"a + b"(chain(a, b, c));
    assert(isClose(r1, [3, 7, 107, 109.5, 112.5]));

    // To minimize nesting of parentheses, Uniform Function Call Syntax can be used
    auto r2 = chain(a, b, c).cumulativeFold!"a + b";
    assert(isClose(r2, [3, 7, 107, 109.5, 112.5]));
}

/**
Sometimes it is very useful to compute multiple aggregates in one pass.
One advantage is that the computation is faster because the looping overhead
is shared. That's why `cumulativeFold` accepts multiple functions.
If two or more functions are passed, `cumulativeFold` returns a $(REF Tuple,
std,typecons) object with one member per passed-in function.
The number of seeds must be correspondingly increased.
*/
@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.algorithm.iteration : map;
    import std.math.operations : isClose;
    import std.typecons : tuple;

    double[] a = [3.0, 4, 7, 11, 3, 2, 5];
    // Compute minimum and maximum in one pass
    auto r = a.cumulativeFold!(min, max);
    // The type of r is Tuple!(int, int)
    assert(isClose(r.map!"a[0]", [3, 3, 3, 3, 3, 2, 2]));     // minimum
    assert(isClose(r.map!"a[1]", [3, 4, 7, 11, 11, 11, 11])); // maximum

    // Compute sum and sum of squares in one pass
    auto r2 = a.cumulativeFold!("a + b", "a + b * b")(tuple(0.0, 0.0));
    assert(isClose(r2.map!"a[0]", [3, 7, 14, 25, 28, 30, 35]));      // sum
    assert(isClose(r2.map!"a[1]", [9, 25, 74, 195, 204, 208, 233])); // sum of squares
}

@safe unittest
{
    import std.algorithm.comparison : equal, max, min;
    import std.conv : to;
    import std.range : chain;
    import std.typecons : tuple;

    double[] a = [3, 4];
    auto r = a.cumulativeFold!("a + b")(0.0);
    assert(r.equal([3, 7]));
    auto r2 = cumulativeFold!("a + b")(a);
    assert(r2.equal([3, 7]));
    auto r3 = cumulativeFold!(min)(a);
    assert(r3.equal([3, 3]));
    double[] b = [100];
    auto r4 = cumulativeFold!("a + b")(chain(a, b));
    assert(r4.equal([3, 7, 107]));

    // two funs
    auto r5 = cumulativeFold!("a + b", "a - b")(a, tuple(0.0, 0.0));
    assert(r5.equal([tuple(3, -3), tuple(7, -7)]));
    auto r6 = cumulativeFold!("a + b", "a - b")(a);
    assert(r6.equal([tuple(3, 3), tuple(7, -1)]));

    a = [1, 2, 3, 4, 5];
    // Stringize with commas
    auto rep = cumulativeFold!("a ~ `, ` ~ to!string(b)")(a, "");
    assert(rep.map!"a[2 .. $]".equal(["1", "1, 2", "1, 2, 3", "1, 2, 3, 4", "1, 2, 3, 4, 5"]));

    // Test for empty range
    a = [];
    assert(a.cumulativeFold!"a + b".empty);
    assert(a.cumulativeFold!"a + b"(2.0).empty);
}

@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.array : array;
    import std.math.operations : isClose;
    import std.typecons : tuple;

    const float a = 0.0;
    const float[] b = [1.2, 3, 3.3];
    float[] c = [1.2, 3, 3.3];

    auto r = cumulativeFold!"a + b"(b, a);
    assert(isClose(r, [1.2, 4.2, 7.5]));

    auto r2 = cumulativeFold!"a + b"(c, a);
    assert(isClose(r2, [1.2, 4.2, 7.5]));

    const numbers = [10, 30, 20];
    enum m = numbers.cumulativeFold!(min).array;
    assert(m == [10, 10, 10]);
    enum minmax = numbers.cumulativeFold!(min, max).array;
    assert(minmax == [tuple(10, 10), tuple(10, 30), tuple(10, 30)]);
}

@safe unittest
{
    import std.math.operations : isClose;
    import std.typecons : tuple;

    enum foo = "a + 0.5 * b";
    auto r = [0, 1, 2, 3];
    auto r1 = r.cumulativeFold!foo;
    auto r2 = r.cumulativeFold!(foo, foo);
    assert(isClose(r1, [0, 0.5, 1.5, 3]));
    assert(isClose(r2.map!"a[0]", [0, 0.5, 1.5, 3]));
    assert(isClose(r2.map!"a[1]", [0, 0.5, 1.5, 3]));
}

@safe unittest
{
    import std.algorithm.comparison : equal, max, min;
    import std.array : array;
    import std.typecons : tuple;

    //Seed is tuple of const.
    static auto minmaxElement(alias F = min, alias G = max, R)(in R range)
    @safe pure nothrow
    if (isInputRange!R)
    {
        return range.cumulativeFold!(F, G)(tuple(ElementType!R.max, ElementType!R.min));
    }

    assert(minmaxElement([1, 2, 3]).equal([tuple(1, 1), tuple(1, 2), tuple(1, 3)]));
}

// https://issues.dlang.org/show_bug.cgi?id=12569
@safe unittest
{
    import std.algorithm.comparison : equal, max, min;
    import std.typecons : tuple;

    dchar c = 'a';

    assert(cumulativeFold!(min, max)("hello", tuple(c, c)).equal([tuple('a', 'h'),
        tuple('a', 'h'), tuple('a', 'l'), tuple('a', 'l'), tuple('a', 'o')]));
    static assert(!__traits(compiles, cumulativeFold!(min, max)("hello", tuple(c))));
    static assert(!__traits(compiles, cumulativeFold!(min, max)("hello", tuple(c, c, c))));

    //"Seed dchar should be a Tuple"
    static assert(!__traits(compiles, cumulativeFold!(min, max)("hello", c)));
    //"Seed (dchar) does not have the correct amount of fields (should be 2)"
    static assert(!__traits(compiles, cumulativeFold!(min, max)("hello", tuple(c))));
    //"Seed (dchar, dchar, dchar) does not have the correct amount of fields (should be 2)"
    static assert(!__traits(compiles, cumulativeFold!(min, max)("hello", tuple(c, c, c))));
    //"Incompatible function/seed/element: all(alias pred = "a")/int/dchar"
    static assert(!__traits(compiles, cumulativeFold!all("hello", 1)));
    static assert(!__traits(compiles, cumulativeFold!(all, all)("hello", tuple(1, 1))));
}

// https://issues.dlang.org/show_bug.cgi?id=13304
@safe unittest
{
    int[] data;
    assert(data.cumulativeFold!((a, b) => a + b).empty);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : AllDummyRanges, propagatesLength,
        propagatesRangeType, RangeType;

    foreach (DummyType; AllDummyRanges)
    {
        DummyType d;
        auto m = d.cumulativeFold!"a * b";

        static assert(propagatesLength!(typeof(m), DummyType));
        static if (DummyType.rt <= RangeType.Forward)
            static assert(propagatesRangeType!(typeof(m), DummyType));

        assert(m.equal([1, 2, 6, 24, 120, 720, 5040, 40_320, 362_880, 3_628_800]));
    }
}

// splitter
/**
Lazily splits a range using an element or range as a separator.
Separator ranges can be any narrow string type or sliceable range type.

Two adjacent separators are considered to surround an empty element in
the split range. Use `filter!(a => !a.empty)` on the result to compress
empty elements.

The predicate is passed to $(REF binaryFun, std,functional) and accepts
any callable function that can be executed via `pred(element, s)`.

Notes:
    If splitting a string on whitespace and token compression is desired,
    consider using `splitter` without specifying a separator.

    If no separator is passed, the $(REF_ALTTEXT, unary, unaryFun, std,functional)
    predicate `isTerminator` decides whether to accept an element of `r`.

Params:
    pred = The predicate for comparing each element with the separator,
        defaulting to `"a == b"`.
    r = The $(REF_ALTTEXT input range, isInputRange, std,range,primitives) to be
        split. Must support slicing and `.length` or be a narrow string type.
    s = The element (or range) to be treated as the separator
        between range segments to be split.
    isTerminator = The predicate for deciding where to split the range when no separator is passed
    keepSeparators = The flag for deciding if the separators are kept

Constraints:
    The predicate `pred` needs to accept an element of `r` and the
    separator `s`.

Returns:
    An input range of the subranges of elements between separators. If `r`
    is a $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
    or $(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives),
    the returned range will be likewise.
    When a range is used a separator, bidirectionality isn't possible.

    If keepSeparators is equal to Yes.keepSeparators the output will also contain the
    separators.

    If an empty range is given, the result is an empty range. If a range with
    one separator is given, the result is a range with two empty elements.

See_Also:
 $(REF _splitter, std,regex) for a version that splits using a regular expression defined separator,
 $(REF _split, std,array) for a version that splits eagerly and
 $(LREF splitWhen), which compares adjacent elements instead of element against separator.
*/
auto splitter(alias pred = "a == b",
              Flag!"keepSeparators" keepSeparators = No.keepSeparators,
              Range,
              Separator)(Range r, Separator s)
if (is(typeof(binaryFun!pred(r.front, s)) : bool)
        && ((hasSlicing!Range && hasLength!Range) || isNarrowString!Range))
{
    import std.algorithm.searching : find;
    import std.conv : unsigned;

    struct Result
    {
    private:
        Range _input;
        Separator _separator;
        // Do we need hasLength!Range? popFront uses _input.length...
        enum size_t _unComputed = size_t.max - 1, _atEnd = size_t.max;
        size_t _frontLength = _unComputed;
        size_t _backLength = _unComputed;

        static if (isNarrowString!Range)
        {
            size_t _separatorLength;
        }
        else
        {
            enum _separatorLength = 1;
        }

        static if (keepSeparators)
        {
            bool _wasSeparator = true;
        }

        static if (isBidirectionalRange!Range)
        {
            size_t lastIndexOf(Range haystack, Separator needle)
            {
                import std.range : retro;
                auto r = haystack.retro().find!pred(needle);
                return r.retro().length - 1;
            }
        }

    public:
        this(Range input, Separator separator)
        {
            _input = input;
            _separator = separator;

            static if (isNarrowString!Range)
            {
                import std.utf : codeLength;

                _separatorLength = codeLength!(ElementEncodingType!Range)(separator);
            }
            if (_input.empty)
                _frontLength = _atEnd;
        }

        static if (isInfinite!Range)
        {
            enum bool empty = false;
        }
        else
        {
            @property bool empty()
            {
                return _frontLength == _atEnd;
            }
        }

        @property Range front()
        {
            assert(!empty, "Attempting to fetch the front of an empty splitter.");
            static if (keepSeparators)
            {
                if (!_wasSeparator)
                {
                    _frontLength = _separatorLength;
                    _wasSeparator = true;
                }
                else if (_frontLength == _unComputed)
                {
                    auto r = _input.find!pred(_separator);
                    _frontLength = _input.length - r.length;
                    _wasSeparator = false;
                }
            }
            else
            {
                if (_frontLength == _unComputed)
                {
                    auto r = _input.find!pred(_separator);
                    _frontLength = _input.length - r.length;
                }
            }
            return _input[0 .. _frontLength];
        }

        void popFront()
        {
            assert(!empty, "Attempting to popFront an empty splitter.");
            if (_frontLength == _unComputed)
            {
                front;
            }
            assert(_frontLength <= _input.length, "The front position must"
                    ~ " not exceed the input.length");
            static if (keepSeparators)
            {
                if (_frontLength == _input.length && !_wasSeparator)
                {
                    _frontLength = _atEnd;

                    _backLength = _atEnd;
                }
                else
                {
                    _input = _input[_frontLength .. _input.length];
                    _frontLength = _unComputed;
                }
            }
            else
            {
                if (_frontLength == _input.length)
                {
                    // no more input and need to fetch => done
                    _frontLength = _atEnd;

                    // Probably don't need this, but just for consistency:
                    _backLength = _atEnd;
                }
                else
                {
                    _input = _input[_frontLength + _separatorLength .. _input.length];
                    _frontLength = _unComputed;
                }
            }
        }

        static if (isForwardRange!Range)
        {
            @property typeof(this) save()
            {
                auto ret = this;
                ret._input = _input.save;
                return ret;
            }
        }

        static if (isBidirectionalRange!Range)
        {
            @property Range back()
            {
                assert(!empty, "Attempting to fetch the back of an empty splitter.");
                static if (keepSeparators)
                {
                    if (!_wasSeparator)
                    {
                        _backLength = _separatorLength;
                        _wasSeparator = true;
                    }
                    else if (_backLength == _unComputed)
                    {
                        immutable lastIndex = lastIndexOf(_input, _separator);
                        if (lastIndex == -1)
                        {
                            _backLength = _input.length;
                        }
                        else
                        {
                            _backLength = _input.length - lastIndex - 1;
                        }
                        _wasSeparator = false;
                    }
                }
                else
                {
                    if (_backLength == _unComputed)
                    {
                        immutable lastIndex = lastIndexOf(_input, _separator);
                        if (lastIndex == -1)
                        {
                            _backLength = _input.length;
                        }
                        else
                        {
                            _backLength = _input.length - lastIndex - 1;
                        }
                    }
                }
                return _input[_input.length - _backLength .. _input.length];
            }

            void popBack()
            {
                assert(!empty, "Attempting to popBack an empty splitter.");
                if (_backLength == _unComputed)
                {
                    // evaluate back to make sure it's computed
                    back;
                }
                assert(_backLength <= _input.length, "The end index must not"
                        ~ " exceed the length of the input");
                static if (keepSeparators)
                {
                    if (_backLength == _input.length && !_wasSeparator)
                    {
                        _frontLength = _atEnd;
                        _backLength = _atEnd;
                    }
                    else
                    {
                        _input = _input[0 .. _input.length - _backLength];
                        _backLength = _unComputed;
                    }
                }
                else
                {
                    if (_backLength == _input.length)
                    {
                        // no more input and need to fetch => done
                        _frontLength = _atEnd;
                        _backLength = _atEnd;
                    }
                    else
                    {
                        _input = _input[0 .. _input.length - _backLength - _separatorLength];
                        _backLength = _unComputed;
                    }
                }
            }
        }
    }

    return Result(r, s);
}

/// Basic splitting with characters and numbers.
@safe unittest
{
    import std.algorithm.comparison : equal;

    assert("a|bc|def".splitter('|').equal([ "a", "bc", "def" ]));

    int[] a = [1, 0, 2, 3, 0, 4, 5, 6];
    int[][] w = [ [1], [2, 3], [4, 5, 6] ];
    assert(a.splitter(0).equal(w));
}

/// Basic splitting with characters and numbers and keeping sentinels.
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : Yes;

    assert("a|bc|def".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "a", "|", "bc", "|", "def" ]));

    int[] a = [1, 0, 2, 3, 0, 4, 5, 6];
    int[][] w = [ [1], [0], [2, 3], [0], [4, 5, 6] ];
    assert(a.splitter!("a == b", Yes.keepSeparators)(0).equal(w));
}

/// Adjacent separators.
@safe unittest
{
    import std.algorithm.comparison : equal;

    assert("|ab|".splitter('|').equal([ "", "ab", "" ]));
    assert("ab".splitter('|').equal([ "ab" ]));

    assert("a|b||c".splitter('|').equal([ "a", "b", "", "c" ]));
    assert("hello  world".splitter(' ').equal([ "hello", "", "world" ]));

    auto a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    auto w = [ [1, 2], [], [3], [4, 5], [] ];
    assert(a.splitter(0).equal(w));
}

/// Adjacent separators and keeping sentinels.
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : Yes;

    assert("|ab|".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "", "|", "ab", "|", "" ]));
    assert("ab".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "ab" ]));

    assert("a|b||c".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "a", "|", "b", "|", "", "|", "c" ]));
    assert("hello  world".splitter!("a == b", Yes.keepSeparators)(' ')
        .equal([ "hello", " ", "", " ", "world" ]));

    auto a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    auto w = [ [1, 2], [0], [], [0], [3], [0], [4, 5], [0], [] ];
    assert(a.splitter!("a == b", Yes.keepSeparators)(0).equal(w));
}

/// Empty and separator-only ranges.
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : empty;

    assert("".splitter('|').empty);
    assert("|".splitter('|').equal([ "", "" ]));
    assert("||".splitter('|').equal([ "", "", "" ]));
}

/// Empty and separator-only ranges and keeping sentinels.
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : Yes;
    import std.range : empty;

    assert("".splitter!("a == b", Yes.keepSeparators)('|').empty);
    assert("|".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "", "|", "" ]));
    assert("||".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "", "|", "", "|", "" ]));
}

/// Use a range for splitting
@safe unittest
{
    import std.algorithm.comparison : equal;

    assert("a=>bc=>def".splitter("=>").equal([ "a", "bc", "def" ]));
    assert("a|b||c".splitter("||").equal([ "a|b", "c" ]));
    assert("hello  world".splitter("  ").equal([ "hello", "world" ]));

    int[] a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    int[][] w = [ [1, 2], [3, 0, 4, 5, 0] ];
    assert(a.splitter([0, 0]).equal(w));

    a = [ 0, 0 ];
    assert(a.splitter([0, 0]).equal([ (int[]).init, (int[]).init ]));

    a = [ 0, 0, 1 ];
    assert(a.splitter([0, 0]).equal([ [], [1] ]));
}

/// Use a range for splitting
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : Yes;

    assert("a=>bc=>def".splitter!("a == b", Yes.keepSeparators)("=>")
        .equal([ "a", "=>", "bc", "=>", "def" ]));
    assert("a|b||c".splitter!("a == b", Yes.keepSeparators)("||")
        .equal([ "a|b", "||", "c" ]));
    assert("hello  world".splitter!("a == b", Yes.keepSeparators)("  ")
        .equal([ "hello", "  ",  "world" ]));

    int[] a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    int[][] w = [ [1, 2], [0, 0], [3, 0, 4, 5, 0] ];
    assert(a.splitter!("a == b", Yes.keepSeparators)([0, 0]).equal(w));

    a = [ 0, 0 ];
    assert(a.splitter!("a == b", Yes.keepSeparators)([0, 0])
        .equal([ (int[]).init, [0, 0], (int[]).init ]));

    a = [ 0, 0, 1 ];
    assert(a.splitter!("a == b", Yes.keepSeparators)([0, 0])
        .equal([ [], [0, 0], [1] ]));
}

/// Custom predicate functions.
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.ascii : toLower;

    assert("abXcdxef".splitter!"a.toLower == b"('x').equal(
                 [ "ab", "cd", "ef" ]));

    auto w = [ [0], [1], [2] ];
    assert(w.splitter!"a.front == b"(1).equal([ [[0]], [[2]] ]));
}

/// Custom predicate functions.
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : Yes;
    import std.ascii : toLower;

    assert("abXcdxef".splitter!("a.toLower == b", Yes.keepSeparators)('x')
        .equal([ "ab", "X", "cd", "x", "ef" ]));

    auto w = [ [0], [1], [2] ];
    assert(w.splitter!("a.front == b", Yes.keepSeparators)(1)
        .equal([ [[0]], [[1]], [[2]] ]));
}

/// Use splitter without a separator
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range.primitives : front;

    assert(equal(splitter!(a => a == '|')("a|bc|def"), [ "a", "bc", "def" ]));
    assert(equal(splitter!(a => a == ' ')("hello  world"), [ "hello", "", "world" ]));

    int[] a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    int[][] w = [ [1, 2], [], [3], [4, 5], [] ];
    assert(equal(splitter!(a => a == 0)(a), w));

    a = [ 0 ];
    assert(equal(splitter!(a => a == 0)(a), [ (int[]).init, (int[]).init ]));

    a = [ 0, 1 ];
    assert(equal(splitter!(a => a == 0)(a), [ [], [1] ]));

    w = [ [0], [1], [2] ];
    assert(equal(splitter!(a => a.front == 1)(w), [ [[0]], [[2]] ]));
}

/// Leading separators, trailing separators, or no separators.
@safe unittest
{
    import std.algorithm.comparison : equal;

    assert("|ab|".splitter('|').equal([ "", "ab", "" ]));
    assert("ab".splitter('|').equal([ "ab" ]));
}

/// Leading separators, trailing separators, or no separators.
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : Yes;

    assert("|ab|".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "", "|", "ab", "|", "" ]));
    assert("ab".splitter!("a == b", Yes.keepSeparators)('|')
        .equal([ "ab" ]));
}

/// Splitter returns bidirectional ranges if the delimiter is a single element
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : retro;
    assert("a|bc|def".splitter('|').retro.equal([ "def", "bc", "a" ]));
}

/// Splitter returns bidirectional ranges if the delimiter is a single element
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : Yes;
    import std.range : retro;
    assert("a|bc|def".splitter!("a == b", Yes.keepSeparators)('|')
        .retro.equal([ "def", "|", "bc", "|", "a" ]));
}

/// Splitting by word lazily
@safe unittest
{
    import std.ascii : isWhite;
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : splitter;

    string str = "Hello World!";
    assert(str.splitter!(isWhite).equal(["Hello", "World!"]));
}

@safe unittest
{
    import std.algorithm;
    import std.array : array;
    import std.internal.test.dummyrange;
    import std.range : retro;

    assert(equal(splitter("hello  world", ' '), [ "hello", "", "world" ]));
    assert(equal(splitter("žlutoučkýřkůň", 'ř'), [ "žlutoučký", "kůň" ]));
    int[] a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    int[][] w = [ [1, 2], [], [3], [4, 5], [] ];
    static assert(isForwardRange!(typeof(splitter(a, 0))));

    assert(equal(splitter(a, 0), w));
    a = null;
    assert(equal(splitter(a, 0),  (int[][]).init));
    a = [ 0 ];
    assert(equal(splitter(a, 0), [ (int[]).init, (int[]).init ][]));
    a = [ 0, 1 ];
    assert(equal(splitter(a, 0), [ [], [1] ]));
    assert(equal(splitter(a, 0), [ [], [1] ][]));

    // Thoroughly exercise the bidirectional stuff.
    auto str = "abc abcd abcde ab abcdefg abcdefghij ab ac ar an at ada";
    assert(equal(
        retro(splitter(str, 'a')),
        retro(array(splitter(str, 'a')))
    ));

    // Test interleaving front and back.
    auto split = splitter(str, 'a');
    assert(split.front == "");
    assert(split.back == "");
    split.popBack();
    assert(split.back == "d");
    split.popFront();
    assert(split.front == "bc ");
    assert(split.back == "d");
    split.popFront();
    split.popBack();
    assert(split.back == "t ");
    split.popBack();
    split.popBack();
    split.popFront();
    split.popFront();
    assert(split.front == "b ");
    assert(split.back == "r ");

    // https://issues.dlang.org/show_bug.cgi?id=4408
    foreach (DummyType; AllDummyRanges)
    {
        static if (isRandomAccessRange!DummyType)
        {
            static assert(isBidirectionalRange!DummyType);
            DummyType d;
            auto s = splitter(d, 5);
            assert(equal(s.front, [1,2,3,4]));
            assert(equal(s.back, [6,7,8,9,10]));

            auto s2 = splitter(d, [4, 5]);
            assert(equal(s2.front, [1,2,3]));
        }
    }
}
@safe unittest
{
    import std.algorithm;
    import std.range;
    auto L = retro(iota(1L, 10L));
    auto s = splitter(L, 5L);
    assert(equal(s.front, [9L, 8L, 7L, 6L]));
    s.popFront();
    assert(equal(s.front, [4L, 3L, 2L, 1L]));
    s.popFront();
    assert(s.empty);
}

// https://issues.dlang.org/show_bug.cgi?id=18470
@safe unittest
{
    import std.algorithm.comparison : equal;

    const w = [[0], [1], [2]];
    assert(w.splitter!((a, b) => a.front() == b)(1).equal([[[0]], [[2]]]));
}

// https://issues.dlang.org/show_bug.cgi?id=18470
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.ascii : toLower;

    assert("abXcdxef".splitter!"a.toLower == b"('x').equal(["ab", "cd", "ef"]));
    assert("abXcdxef".splitter!((a, b) => a.toLower == b)('x').equal(["ab", "cd", "ef"]));
}

/// ditto
auto splitter(alias pred = "a == b",
              Flag!"keepSeparators" keepSeparators = No.keepSeparators,
              Range,
              Separator)(Range r, Separator s)
if (is(typeof(binaryFun!pred(r.front, s.front)) : bool)
        && (hasSlicing!Range || isNarrowString!Range)
        && isForwardRange!Separator
        && (hasLength!Separator || isNarrowString!Separator))
{
    import std.algorithm.searching : find;
    import std.conv : unsigned;

    static struct Result
    {
    private:
        Range _input;
        Separator _separator;
        // _frontLength == size_t.max means empty
        size_t _frontLength = size_t.max;

        static if (keepSeparators)
        {
            bool _wasSeparator = true;
        }

        @property auto separatorLength() { return _separator.length; }

        void ensureFrontLength()
        {
            if (_frontLength != _frontLength.max) return;
            static if (keepSeparators)
            {
                assert(!_input.empty || _wasSeparator, "The input must not be empty");
                if (_wasSeparator)
                {
                    _frontLength = _input.length -
                        find!pred(_input, _separator).length;
                    _wasSeparator = false;
                }
                else
                {
                    _frontLength = separatorLength();
                    _wasSeparator = true;
                }
            }
            else
            {
                assert(!_input.empty, "The input must not be empty");
                // compute front length
                _frontLength = (_separator.empty) ? 1 :
                           _input.length - find!pred(_input, _separator).length;
            }
        }

    public:
        this(Range input, Separator separator)
        {
            _input = input;
            _separator = separator;
        }

        @property Range front()
        {
            assert(!empty, "Attempting to fetch the front of an empty splitter.");
            ensureFrontLength();
            return _input[0 .. _frontLength];
        }

        static if (isInfinite!Range)
        {
            enum bool empty = false;  // Propagate infiniteness
        }
        else
        {
            @property bool empty()
            {
                static if (keepSeparators)
                {
                    return _frontLength == size_t.max && _input.empty && !_wasSeparator;
                }
                else
                {
                    return _frontLength == size_t.max && _input.empty;
                }
            }
        }

        void popFront()
        {
            assert(!empty, "Attempting to popFront an empty splitter.");
            ensureFrontLength();

            static if (keepSeparators)
            {
                _input = _input[_frontLength .. _input.length];
            }
            else
            {
                if (_frontLength == _input.length)
                {
                    // done, there's no separator in sight
                    _input = _input[_frontLength .. _frontLength];
                    _frontLength = _frontLength.max;
                    return;
                }
                if (_frontLength + separatorLength == _input.length)
                {
                    // Special case: popping the first-to-last item; there is
                    // an empty item right after this.
                    _input = _input[_input.length .. _input.length];
                    _frontLength = 0;
                    return;
                }
                // Normal case, pop one item and the separator, get ready for
                // reading the next item
                _input = _input[_frontLength + separatorLength .. _input.length];
            }
            // mark _frontLength as uninitialized
            _frontLength = _frontLength.max;
        }

        static if (isForwardRange!Range)
        {
            @property typeof(this) save()
            {
                auto ret = this;
                ret._input = _input.save;
                return ret;
            }
        }
    }

    return Result(r, s);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : Tuple;

    alias C = Tuple!(int, "x", int, "y");
    auto a = [C(1,0), C(2,0), C(3,1), C(4,0)];
    assert(equal(splitter!"a.x == b"(a, [2, 3]), [ [C(1,0)], [C(4,0)] ]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.array : split;
    import std.conv : text;

    auto s = ",abc, de, fg,hi,";
    auto sp0 = splitter(s, ',');
    assert(equal(sp0, ["", "abc", " de", " fg", "hi", ""][]));

    auto s1 = ", abc, de,  fg, hi, ";
    auto sp1 = splitter(s1, ", ");
    assert(equal(sp1, ["", "abc", "de", " fg", "hi", ""][]));
    static assert(isForwardRange!(typeof(sp1)));

    int[] a = [ 1, 2, 0, 3, 0, 4, 5, 0 ];
    int[][] w = [ [1, 2], [3], [4, 5], [] ];
    uint i;
    foreach (e; splitter(a, 0))
    {
        assert(i < w.length);
        assert(e == w[i++]);
    }
    assert(i == w.length);

    wstring names = ",peter,paul,jerry,";
    auto words = split(names, ",");
    assert(walkLength(words) == 5, text(walkLength(words)));
}

@safe unittest
{
    int[][] a = [ [1], [2], [0], [3], [0], [4], [5], [0] ];
    int[][][] w = [ [[1], [2]], [[3]], [[4], [5]], [] ];
    uint i;
    foreach (e; splitter!"a.front == 0"(a, 0))
    {
        assert(i < w.length);
        assert(e == w[i++]);
    }
    assert(i == w.length);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    auto s6 = ",";
    auto sp6 = splitter(s6, ',');
    foreach (e; sp6) {}
    assert(equal(sp6, ["", ""][]));
}

// https://issues.dlang.org/show_bug.cgi?id=10773
@safe unittest
{
    import std.algorithm.comparison : equal;

    auto s = splitter("abc", "");
    assert(s.equal(["a", "b", "c"]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    // Test by-reference separator
    static class RefSep {
    @safe:
        string _impl;
        this(string s) { _impl = s; }
        @property empty() { return _impl.empty; }
        @property auto front() { return _impl.front; }
        void popFront() { _impl = _impl[1..$]; }
        @property RefSep save() scope { return new RefSep(_impl); }
        @property auto length() { return _impl.length; }
    }
    auto sep = new RefSep("->");
    auto data = "i->am->pointing";
    auto words = splitter(data, sep);
    assert(words.equal([ "i", "am", "pointing" ]));
}

/// ditto
auto splitter(alias isTerminator, Range)(Range r)
if (isForwardRange!Range && is(typeof(unaryFun!isTerminator(r.front))))
{
    return SplitterResult!(unaryFun!isTerminator, Range)(r);
}

private struct SplitterResult(alias isTerminator, Range)
{
    import std.algorithm.searching : find;
    enum fullSlicing = (hasLength!Range && hasSlicing!Range) || isSomeString!Range;

    private Range _input;
    private size_t _end = 0;
    static if (!fullSlicing)
        private Range _next;

    private void findTerminator()
    {
        static if (fullSlicing)
        {
            auto r = find!isTerminator(_input.save);
            _end = _input.length - r.length;
        }
        else
            for ( _end = 0; !_next.empty ; _next.popFront)
            {
                if (isTerminator(_next.front))
                    break;
                ++_end;
            }
    }

    this(Range input)
    {
        _input = input;
        static if (!fullSlicing)
            _next = _input.save;

        if (!_input.empty)
            findTerminator();
        else
            _end = size_t.max;
    }

    static if (fullSlicing)
    {
        private this(Range input, size_t end)
        {
            _input = input;
            _end = end;
        }
    }
    else
    {
        private this(Range input, size_t end, Range next)
        {
            _input = input;
            _end = end;
            _next = next;
        }
    }

    static if (isInfinite!Range)
    {
        enum bool empty = false;  // Propagate infiniteness.
    }
    else
    {
        @property bool empty()
        {
            return _end == size_t.max;
        }
    }

    @property auto front()
    {
        version (assert)
        {
            import core.exception : RangeError;
            if (empty)
                throw new RangeError();
        }
        static if (fullSlicing)
            return _input[0 .. _end];
        else
        {
            import std.range : takeExactly;
            return _input.takeExactly(_end);
        }
    }

    void popFront()
    {
        version (assert)
        {
            import core.exception : RangeError;
            if (empty)
                throw new RangeError();
        }

        static if (fullSlicing)
        {
            _input = _input[_end .. _input.length];
            if (_input.empty)
            {
                _end = size_t.max;
                return;
            }
            _input.popFront();
        }
        else
        {
            if (_next.empty)
            {
                _input = _next;
                _end = size_t.max;
                return;
            }
            _next.popFront();
            _input = _next.save;
        }
        findTerminator();
    }

    @property typeof(this) save()
    {
        static if (fullSlicing)
            return SplitterResult(_input.save, _end);
        else
            return SplitterResult(_input.save, _end, _next.save);
    }
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : iota;

    auto L = iota(1L, 10L);
    auto s = splitter(L, [5L, 6L]);
    assert(equal(s.front, [1L, 2L, 3L, 4L]));
    s.popFront();
    assert(equal(s.front, [7L, 8L, 9L]));
    s.popFront();
    assert(s.empty);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.internal : algoFormat;
    import std.internal.test.dummyrange;

    void compare(string sentence, string[] witness)
    {
        auto r = splitter!"a == ' '"(sentence);
        assert(equal(r.save, witness), algoFormat("got: %(%s, %) expected: %(%s, %)", r, witness));
    }

    compare(" Mary  has a little lamb.   ",
            ["", "Mary", "", "has", "a", "little", "lamb.", "", "", ""]);
    compare("Mary  has a little lamb.   ",
            ["Mary", "", "has", "a", "little", "lamb.", "", "", ""]);
    compare("Mary  has a little lamb.",
            ["Mary", "", "has", "a", "little", "lamb."]);
    compare("", (string[]).init);
    compare(" ", ["", ""]);

    static assert(isForwardRange!(typeof(splitter!"a == ' '"("ABC"))));

    foreach (DummyType; AllDummyRanges)
    {
        static if (isRandomAccessRange!DummyType)
        {
            auto rangeSplit = splitter!"a == 5"(DummyType.init);
            assert(equal(rangeSplit.front, [1,2,3,4]));
            rangeSplit.popFront();
            assert(equal(rangeSplit.front, [6,7,8,9,10]));
        }
    }
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.internal : algoFormat;
    import std.range;

    struct Entry
    {
        int low;
        int high;
        int[][] result;
    }
    Entry[] entries = [
        Entry(0, 0, []),
        Entry(0, 1, [[0]]),
        Entry(1, 2, [[], []]),
        Entry(2, 7, [[2], [4], [6]]),
        Entry(1, 8, [[], [2], [4], [6], []]),
    ];
    foreach ( entry ; entries )
    {
        auto a = iota(entry.low, entry.high).filter!"true"();
        auto b = splitter!"a%2"(a);
        assert(equal!equal(b.save, entry.result), algoFormat("got: %(%s, %) expected: %(%s, %)", b, entry.result));
    }
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.uni : isWhite;

    // https://issues.dlang.org/show_bug.cgi?id=6791
    assert(equal(
        splitter("là dove terminava quella valle"),
        ["là", "dove", "terminava", "quella", "valle"]
    ));
    assert(equal(
        splitter!(isWhite)("là dove terminava quella valle"),
        ["là", "dove", "terminava", "quella", "valle"]
    ));
    assert(equal(splitter!"a=='本'"("日本語"), ["日", "語"]));
}

// https://issues.dlang.org/show_bug.cgi?id=18657
pure @safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : refRange;
    string s = "foobar";
    auto r = refRange(&s).splitter!(c => c == 'b');
    assert(equal!equal(r.save, ["foo", "ar"]));
    assert(equal!equal(r.save, ["foo", "ar"]));
}

/++
Lazily splits the character-based range `s` into words, using whitespace as the
delimiter.

This function is character-range specific and, contrary to
`splitter!(std.uni.isWhite)`, runs of whitespace will be merged together
(no empty tokens will be produced).

Params:
    s = The character-based range to be split. Must be a string, or a
    random-access range of character types.

Returns:
    An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) of slices of
    the original range split by whitespace.
 +/
auto splitter(Range)(Range s)
if (isSomeString!Range ||
    isRandomAccessRange!Range && hasLength!Range && hasSlicing!Range &&
    !isConvertibleToString!Range &&
    isSomeChar!(ElementEncodingType!Range))
{
    import std.algorithm.searching : find;
    static struct Result
    {
    private:
        import core.exception : RangeError;
        Range _s;
        size_t _frontLength;

        void getFirst()
        {
            import std.uni : isWhite;
            import std.traits : Unqual;

            static if (is(immutable ElementEncodingType!Range == immutable wchar) &&
                       is(immutable ElementType!Range == immutable dchar))
            {
                // all unicode whitespace characters fit into a wchar. However,
                // this range is a wchar array, so we will treat it like a
                // wchar array instead of decoding each code point.
                _frontLength = _s.length; // default condition, no spaces
                foreach (i; 0 .. _s.length)
                    if (isWhite(_s[i]))
                    {
                        _frontLength = i;
                        break;
                    }
            }
            else static if (is(immutable ElementType!Range == immutable dchar) ||
                            is(immutable ElementType!Range == immutable wchar))
            {
                // dchar or wchar range, we can just use find.
                auto r = find!(isWhite)(_s.save);
                _frontLength = _s.length - r.length;
            }
            else
            {
                // need to decode the characters until we find a space. This is
                // ported from std.string.stripLeft.
                static import std.ascii;
                static import std.uni;
                import std.utf : decodeFront;

                auto input = _s.save;
                size_t iLength = input.length;

                while (!input.empty)
                {
                    auto c = input.front;
                    if (std.ascii.isASCII(c))
                    {
                        if (std.ascii.isWhite(c))
                            break;
                        input.popFront();
                        --iLength;
                    }
                    else
                    {
                        auto dc = decodeFront(input);
                        if (std.uni.isWhite(dc))
                            break;
                        iLength = input.length;
                    }
                }

                // sanity check
                assert(iLength <= _s.length, "The current index must not"
                        ~ " exceed the length of the input");

                _frontLength = _s.length - iLength;
            }
        }

    public:
        this(Range s)
        {
            import std.string : stripLeft;
            _s = s.stripLeft();
            getFirst();
        }

        @property auto front()
        {
            version (assert) if (empty) throw new RangeError();
            return _s[0 .. _frontLength];
        }

        void popFront()
        {
            import std.string : stripLeft;
            version (assert) if (empty) throw new RangeError();
            _s = _s[_frontLength .. $].stripLeft();
            getFirst();
        }

        @property bool empty() const
        {
            return _s.empty;
        }

        @property inout(Result) save() inout @safe pure nothrow
        {
            return this;
        }
    }
    return Result(s);
}

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;
    auto a = " a     bcd   ef gh ";
    assert(equal(splitter(a), ["a", "bcd", "ef", "gh"][]));
}

@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import std.meta : AliasSeq;
    static foreach (S; AliasSeq!(string, wstring, dstring))
    {{
        import std.conv : to;
        S a = " a  \u2028   bcd   ef gh ";
        assert(equal(splitter(a), [to!S("a"), to!S("bcd"), to!S("ef"), to!S("gh")]));
        a = "";
        assert(splitter(a).empty);
    }}

    immutable string s = " a     bcd   ef gh ";
    assert(equal(splitter(s), ["a", "bcd", "ef", "gh"][]));
}

@safe unittest
{
    import std.conv : to;
    import std.string : strip;

    // TDPL example, page 8
    uint[string] dictionary;
    char[][3] lines;
    lines[0] = "line one".dup;
    lines[1] = "line \ttwo".dup;
    lines[2] = "yah            last   line\ryah".dup;
    foreach (line; lines)
    {
       foreach (word; splitter(strip(line)))
       {
            if (word in dictionary) continue; // Nothing to do
            auto newID = dictionary.length;
            dictionary[to!string(word)] = cast(uint) newID;
        }
    }
    assert(dictionary.length == 5);
    assert(dictionary["line"]== 0);
    assert(dictionary["one"]== 1);
    assert(dictionary["two"]== 2);
    assert(dictionary["yah"]== 3);
    assert(dictionary["last"]== 4);

}

@safe unittest
{
    // do it with byCodeUnit
    import std.conv : to;
    import std.string : strip;
    import std.utf : byCodeUnit;

    alias BCU = typeof("abc".byCodeUnit());

    // TDPL example, page 8
    uint[BCU] dictionary;
    BCU[3] lines;
    lines[0] = "line one".byCodeUnit;
    lines[1] = "line \ttwo".byCodeUnit;
    lines[2] = "yah            last   line\ryah".byCodeUnit;
    foreach (line; lines)
    {
       foreach (word; splitter(strip(line)))
       {
           static assert(is(typeof(word) == BCU));
            if (word in dictionary) continue; // Nothing to do
            auto newID = dictionary.length;
            dictionary[word] = cast(uint) newID;
        }
    }
    assert(dictionary.length == 5);
    assert(dictionary["line".byCodeUnit]== 0);
    assert(dictionary["one".byCodeUnit]== 1);
    assert(dictionary["two".byCodeUnit]== 2);
    assert(dictionary["yah".byCodeUnit]== 3);
    assert(dictionary["last".byCodeUnit]== 4);
}

// https://issues.dlang.org/show_bug.cgi?id=19238
@safe pure unittest
{
    import std.utf : byCodeUnit;
    import std.algorithm.comparison : equal;
    auto range = "hello    world".byCodeUnit.splitter;
    static assert(is(typeof(range.front()) == typeof("hello".byCodeUnit())));
    assert(range.equal(["hello".byCodeUnit, "world".byCodeUnit]));

    // test other space types, including unicode
    auto u = " a\t\v\r bcd\u3000 \u2028\t\nef\U00010001 gh";
    assert(equal(splitter(u), ["a", "bcd", "ef\U00010001", "gh"][]));
    assert(equal(splitter(u.byCodeUnit), ["a".byCodeUnit, "bcd".byCodeUnit,
                 "ef\U00010001".byCodeUnit, "gh".byCodeUnit][]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.internal : algoFormat;
    import std.array : split;
    import std.conv : text;

    // Check consistency:
    // All flavors of split should produce the same results
    foreach (input; [(int[]).init,
                     [0],
                     [0, 1, 0],
                     [1, 1, 0, 0, 1, 1],
                    ])
    {
        foreach (s; [0, 1])
        {
            auto result = split(input, s);

            assert(equal(result, split(input, [s])), algoFormat(`"[%(%s,%)]"`, split(input, [s])));
            //assert(equal(result, split(input, [s].filter!"true"())));                          //Not yet implemented
            assert(equal(result, split!((a) => a == s)(input)), text(split!((a) => a == s)(input)));

            //assert(equal!equal(result, split(input.filter!"true"(), s)));                      //Not yet implemented
            //assert(equal!equal(result, split(input.filter!"true"(), [s])));                    //Not yet implemented
            //assert(equal!equal(result, split(input.filter!"true"(), [s].filter!"true"())));    //Not yet implemented
            assert(equal!equal(result, split!((a) => a == s)(input.filter!"true"())));

            assert(equal(result, splitter(input, s)));
            assert(equal(result, splitter(input, [s])));
            //assert(equal(result, splitter(input, [s].filter!"true"())));                       //Not yet implemented
            assert(equal(result, splitter!((a) => a == s)(input)));

            //assert(equal!equal(result, splitter(input.filter!"true"(), s)));                   //Not yet implemented
            //assert(equal!equal(result, splitter(input.filter!"true"(), [s])));                 //Not yet implemented
            //assert(equal!equal(result, splitter(input.filter!"true"(), [s].filter!"true"()))); //Not yet implemented
            assert(equal!equal(result, splitter!((a) => a == s)(input.filter!"true"())));
        }
    }
    foreach (input; [string.init,
                     " ",
                     "  hello ",
                     "hello   hello",
                     " hello   what heck   this ?  "
                    ])
    {
        foreach (s; [' ', 'h'])
        {
            auto result = split(input, s);

            assert(equal(result, split(input, [s])));
            //assert(equal(result, split(input, [s].filter!"true"())));                          //Not yet implemented
            assert(equal(result, split!((a) => a == s)(input)));

            //assert(equal!equal(result, split(input.filter!"true"(), s)));                      //Not yet implemented
            //assert(equal!equal(result, split(input.filter!"true"(), [s])));                    //Not yet implemented
            //assert(equal!equal(result, split(input.filter!"true"(), [s].filter!"true"())));    //Not yet implemented
            assert(equal!equal(result, split!((a) => a == s)(input.filter!"true"())));

            assert(equal(result, splitter(input, s)));
            assert(equal(result, splitter(input, [s])));
            //assert(equal(result, splitter(input, [s].filter!"true"())));                       //Not yet implemented
            assert(equal(result, splitter!((a) => a == s)(input)));

            //assert(equal!equal(result, splitter(input.filter!"true"(), s)));                   //Not yet implemented
            //assert(equal!equal(result, splitter(input.filter!"true"(), [s])));                 //Not yet implemented
            //assert(equal!equal(result, splitter(input.filter!"true"(), [s].filter!"true"()))); //Not yet implemented
            assert(equal!equal(result, splitter!((a) => a == s)(input.filter!"true"())));
        }
    }
}

// In same combinations substitute needs to calculate the auto-decoded length
// of its needles
private template hasDifferentAutodecoding(Range, Needles...)
{
    import std.meta : anySatisfy;
    /* iff
       - the needles needs auto-decoding, but the incoming range doesn't (or vice versa)
       - both (range, needle) need auto-decoding and don't share the same common type
    */
    enum needlesAreNarrow = anySatisfy!(isNarrowString, Needles);
    enum sourceIsNarrow = isNarrowString!Range;
    enum hasDifferentAutodecoding = sourceIsNarrow != needlesAreNarrow ||
                                    (sourceIsNarrow && needlesAreNarrow &&
                                    is(CommonType!(Range, Needles) == void));
}

@safe nothrow @nogc pure unittest
{
    import std.meta : AliasSeq; // used for better clarity

    static assert(!hasDifferentAutodecoding!(string, AliasSeq!(string, string)));
    static assert(!hasDifferentAutodecoding!(wstring, AliasSeq!(wstring, wstring)));
    static assert(!hasDifferentAutodecoding!(dstring, AliasSeq!(dstring, dstring)));

    // the needles needs auto-decoding, but the incoming range doesn't (or vice versa)
    static assert(hasDifferentAutodecoding!(string, AliasSeq!(wstring, wstring)));
    static assert(hasDifferentAutodecoding!(string, AliasSeq!(dstring, dstring)));
    static assert(hasDifferentAutodecoding!(wstring, AliasSeq!(string, string)));
    static assert(hasDifferentAutodecoding!(wstring, AliasSeq!(dstring, dstring)));
    static assert(hasDifferentAutodecoding!(dstring, AliasSeq!(string, string)));
    static assert(hasDifferentAutodecoding!(dstring, AliasSeq!(wstring, wstring)));

    // both (range, needle) need auto-decoding and don't share the same common type
    static foreach (T; AliasSeq!(string, wstring, dstring))
    {
        static assert(hasDifferentAutodecoding!(T, AliasSeq!(wstring, string)));
        static assert(hasDifferentAutodecoding!(T, AliasSeq!(dstring, string)));
        static assert(hasDifferentAutodecoding!(T, AliasSeq!(wstring, dstring)));
    }
}

// substitute
/**
Returns a range with all occurrences of `substs` in `r`.
replaced with their substitution.

Single value replacements (`'ö'.substitute!('ä', 'a', 'ö', 'o', 'ü', 'u)`) are
supported as well and in $(BIGOH 1).

Params:
    r = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    value = a single value which can be substituted in $(BIGOH 1)
    substs = a set of replacements/substitutions
    pred = the equality function to test if element(s) are equal to
    a substitution

Returns: a range with the substitutions replaced.

See_Also:
$(REF replace, std, array) for an eager replace algorithm or
$(REF translate, std, string), and $(REF tr, std, string)
for string algorithms with translation tables.
*/
template substitute(substs...)
if (substs.length >= 2 && isExpressions!substs)
{
    import std.range.primitives : ElementType;
    import std.traits : CommonType;

    static assert(!(substs.length & 1), "The number of substitution parameters must be even");

    /**
      Substitute single values with compile-time substitution mappings.
      Complexity: $(BIGOH 1) due to D's `switch` guaranteeing $(BIGOH 1);
    */
    auto substitute(Value)(Value value)
    if (isInputRange!Value || !is(CommonType!(Value, typeof(substs[0])) == void))
    {
        static if (isInputRange!Value)
        {
            static if (!is(CommonType!(ElementType!Value, typeof(substs[0])) == void))
            {
                // Substitute single range elements with compile-time substitution mappings
                return value.map!(a => substitute(a));
            }
            else static if (isInputRange!Value &&
                    !is(CommonType!(ElementType!Value, ElementType!(typeof(substs[0]))) == void))
            {
                // not implemented yet, fallback to runtime variant for now
                return .substitute(value, substs);
            }
            else
            {
                static assert(0, `Compile-time substitutions must be elements or ranges of the same type of ` ~
                    Value.stringof ~ `.`);
            }
        }
        // Substitute single values with compile-time substitution mappings.
        else // static if (!is(CommonType!(Value, typeof(substs[0])) == void))
        {
            switch (value)
            {
                static foreach (i; 0 .. substs.length / 2)
                    case substs[2 * i]:
                        return substs[2 * i + 1];

                default: return value;
            }
        }
    }
}

/// ditto
auto substitute(alias pred = (a, b) => a == b, R, Substs...)(R r, Substs substs)
if (isInputRange!R && Substs.length >= 2 && !is(CommonType!(Substs) == void))
{
    import std.range.primitives : ElementType;
    import std.meta : allSatisfy;
    import std.traits : CommonType;

    static assert(!(Substs.length & 1), "The number of substitution parameters must be even");

    enum n = Substs.length / 2;

    // Substitute individual elements
    static if (!is(CommonType!(ElementType!R, Substs) == void))
    {
        import std.functional : binaryFun;

        // Imitate a value closure to be @nogc
        static struct ReplaceElement
        {
            private Substs substs;

            this(Substs substs)
            {
                this.substs = substs;
            }

            auto opCall(E)(E e)
            {
                static foreach (i; 0 .. n)
                    if (binaryFun!pred(e, substs[2 * i]))
                        return substs[2 * i + 1];

                return e;
            }
        }
        auto er = ReplaceElement(substs);
        return r.map!er;
    }
    // Substitute subranges
    else static if (!is(CommonType!(ElementType!R, ElementType!(Substs[0])) == void)  &&
                        allSatisfy!(isForwardRange, Substs))
    {
        import std.range : choose, take;
        import std.meta : Stride;

        auto replaceElement(E)(E e)
        {
            alias ReturnA = typeof(e[0]);
            alias ReturnB = typeof(substs[0 .. 1].take(1));

            // 1-based index
            const auto hitNr = e[1];
            switch (hitNr)
            {
                // no hit
                case 0:
                    // use choose trick for non-common range
                    static if (is(CommonType!(ReturnA, ReturnB) == void))
                        return choose(1, e[0], ReturnB.init);
                    else
                        return e[0];

                // all replacements
                static foreach (i; 0 .. n)
                    case i + 1:
                        // use choose trick for non-common ranges
                        static if (is(CommonType!(ReturnA, ReturnB) == void))
                            return choose(0, e[0], substs[2 * i + 1].take(size_t.max));
                        else
                            return substs[2 * i + 1].take(size_t.max);
                default:
                    assert(0, "hitNr should always be found.");
            }
        }

        alias Ins = Stride!(2, Substs);

        static struct SubstituteSplitter
        {
            import std.range : drop;
            import std.typecons : Tuple;

            private
            {
                typeof(R.init.drop(0)) rest;
                Ins needles;

                typeof(R.init.take(0)) skip; // skip before next hit
                alias Hit = size_t; // 0 iff no hit, otherwise hit in needles[index-1]
                alias E = Tuple!(typeof(skip), Hit);
                Hit hitNr; // hit number: 0 means no hit, otherwise index+1 to needles that matched
                bool hasHit; // is there a replacement hit which should be printed?

                enum hasDifferentAutodecoding = .hasDifferentAutodecoding!(typeof(rest), Ins);

                // calculating the needle length for narrow strings might be expensive -> cache it
                 static if (hasDifferentAutodecoding)
                     ptrdiff_t[n] needleLengths = -1;
            }

            this(R haystack, Ins needles)
            {
                this.rest = haystack.drop(0);
                this.needles = needles;
                if (!haystack.empty)
                {
                    hasHit = true;
                    popFront;
                }
                static if (hasNested!(typeof(skip)))
                    skip = rest.take(0);
            }

            /*  If `skip` is non-empty, it's returned as (skip, 0) tuple
                otherwise a similar (<empty>, hitNr) tuple is returned.
                `replaceElement` maps based on the second item (`hitNr`).
            */
            @property auto ref front()
            {
                assert(!empty, "Attempting to fetch the front of an empty substitute.");
                return !skip.empty ? E(skip, 0) : E(typeof(skip).init, hitNr);
            }

            static if (isInfinite!R)
                enum empty = false; // propagate infiniteness
            else
                @property bool empty()
                {
                    return skip.empty && !hasHit;
                }

            /* If currently in a skipping phase => reset.
               Otherwise try to find the next occurrence of `needles`
                  If valid match
                    - if there are elements before the match, set skip with these elements
                      (on the next popFront, the range will be in the skip state once)
                    - `rest`: advance to the end of the match
                    - set hasHit
               Otherwise skip to the end
            */
            void popFront()
            {
                assert(!empty, "Attempting to popFront an empty substitute.");
                if (!skip.empty)
                {
                    skip = typeof(skip).init; // jump over skip
                }
                else
                {
                    import std.algorithm.searching : countUntil, find;

                    auto match = rest.find!pred(needles);

                    static if (needles.length >= 2) // variadic version of find (returns a tuple)
                    {
                        // find with variadic needles returns a (range, needleNr) tuple
                        // needleNr is a 1-based index
                        auto hitValue = match[0];
                        hitNr = match[1];
                    }
                    else
                    {
                        // find with one needle returns the range
                        auto hitValue = match;
                        hitNr = match.empty ? 0 : 1;
                    }

                    if (hitNr == 0) // no more hits
                    {
                        skip = rest.take(size_t.max);
                        hasHit = false;
                        rest = typeof(rest).init;
                    }
                    else
                    {
                        auto hitLength = size_t.max;
                        switchL: switch (hitNr - 1)
                        {
                            static foreach (i; 0 .. n)
                            {
                                case i:
                                    static if (hasDifferentAutodecoding)
                                    {
                                        import std.utf : codeLength;

                                        // cache calculated needle length
                                        if (needleLengths[i] != -1)
                                            hitLength = needleLengths[i];
                                        else
                                            hitLength = needleLengths[i] = codeLength!dchar(needles[i]);
                                    }
                                    else
                                    {
                                        hitLength = needles[i].length;
                                    }
                                    break switchL;
                            }
                            default:
                                assert(0, "hitNr should always be found");
                        }

                        const pos = rest.countUntil(hitValue);
                        if (pos > 0) // match not at start of rest
                            skip = rest.take(pos);

                        hasHit = true;

                        // iff the source range and the substitutions are narrow strings,
                        // we can avoid calling the auto-decoding `popFront` (via drop)
                        static if (isNarrowString!(typeof(hitValue)) && !hasDifferentAutodecoding)
                            rest = hitValue[hitLength .. $];
                        else
                            rest = hitValue.drop(hitLength);
                    }
                }
            }
        }

        // extract inputs
        Ins ins;
        static foreach (i; 0 .. n)
            ins[i] = substs[2 * i];

        return SubstituteSplitter(r, ins)
                .map!(a => replaceElement(a))
                .joiner;
    }
    else
    {
        static assert(0, "The substitutions must either substitute a single element or a save-able subrange.");
    }
}

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    // substitute single elements
    assert("do_it".substitute('_', ' ').equal("do it"));

    // substitute multiple, single elements
    assert("do_it".substitute('_', ' ',
                               'd', 'g',
                               'i', 't',
                               't', 'o')
                  .equal("go to"));

    // substitute subranges
    assert("do_it".substitute("_", " ",
                              "do", "done")
                  .equal("done it"));

    // substitution works for any ElementType
    int[] x = [1, 2, 3];
    auto y = x.substitute(1, 0.1);
    assert(y.equal([0.1, 2, 3]));
    static assert(is(typeof(y.front) == double));

    import std.range : retro;
    assert([1, 2, 3].substitute(1, 0.1).retro.equal([3, 2, 0.1]));
}

/// Use the faster compile-time overload
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    // substitute subranges of a range
    assert("apple_tree".substitute!("apple", "banana",
                                    "tree", "shrub").equal("banana_shrub"));

    // substitute subranges of a range
    assert("apple_tree".substitute!('a', 'b',
                                    't', 'f').equal("bpple_free"));

    // substitute values
    assert('a'.substitute!('a', 'b', 't', 'f') == 'b');
}

/// Multiple substitutes
@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import std.range.primitives : ElementType;

    int[3] x = [1, 2, 3];
    auto y = x[].substitute(1, 0.1)
                .substitute(0.1, 0.2);
    static assert(is(typeof(y.front) == double));
    assert(y.equal([0.2, 2, 3]));

    auto z = "42".substitute('2', '3')
                 .substitute('3', '1');
    static assert(is(ElementType!(typeof(z)) == dchar));
    assert(equal(z, "41"));
}

// Test the first example with compile-time overloads
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    // substitute single elements
    assert("do_it".substitute!('_', ' ').equal("do it"));

    // substitute multiple, single elements
    assert("do_it".substitute!('_', ' ',
                               'd', 'g',
                               'i', 't',
                               't', 'o')
                  .equal(`go to`));

    // substitute subranges
    assert("do_it".substitute!("_", " ",
                               "do", "done")
                  .equal("done it"));

    // substitution works for any ElementType
    int[3] x = [1, 2, 3];
    auto y = x[].substitute!(1, 0.1);
    assert(y.equal([0.1, 2, 3]));
    static assert(is(typeof(y.front) == double));

    import std.range : retro;
    assert([1, 2, 3].substitute!(1, 0.1).retro.equal([3, 2, 0.1]));
}

// test infinite ranges
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.range : cycle, take;

    int[] x = [1, 2, 3];
    assert(x.cycle.substitute!(1, 0.1).take(4).equal([0.1, 2, 3, 0.1]));
    assert(x.cycle.substitute(1, 0.1).take(4).equal([0.1, 2, 3, 0.1]));
}

// test infinite ranges
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : AllDummyRanges;

    foreach (R; AllDummyRanges)
    {
        assert(R.init
                .substitute!(2, 22, 3, 33, 5, 55, 9, 99)
                .equal([1, 22, 33, 4, 55, 6, 7, 8, 99, 10]));

        assert(R.init
                .substitute(2, 22, 3, 33, 5, 55, 9, 99)
                .equal([1, 22, 33, 4, 55, 6, 7, 8, 99, 10]));
    }
}

// test multiple replacements
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    assert("alpha.beta.gamma"
            .substitute("alpha", "1",
                        "gamma", "3",
                        "beta", "2").equal("1.2.3"));

    assert("alpha.beta.gamma."
            .substitute("alpha", "1",
                        "gamma", "3",
                        "beta", "2").equal("1.2.3."));

    assert("beta.beta.beta"
            .substitute("alpha", "1",
                        "gamma", "3",
                        "beta", "2").equal("2.2.2"));

    assert("alpha.alpha.alpha"
            .substitute("alpha", "1",
                        "gamma", "3",
                        "beta", "2").equal("1.1.1"));
}

// test combination of subrange + element replacement
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    assert(("abcDe".substitute("a", "AA",
                               "b", "DD")
                   .substitute('A', 'y',
                               'D', 'x',
                               'e', '1'))
           .equal("yyxxcx1"));
}

// test const + immutable storage groups
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    auto xyz_abc(T)(T value)
    {
        immutable a = "a";
        const b = "b";
        auto c = "c";
        return value.substitute!("x", a,
                                 "y", b,
                                 "z", c);
    }
    assert(xyz_abc("_x").equal("_a"));
    assert(xyz_abc(".y.").equal(".b."));
    assert(xyz_abc("z").equal("c"));
    assert(xyz_abc("w").equal("w"));
}

// test with narrow strings (auto-decoding) and subranges
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    assert("äöü€".substitute("ä", "b", "ü", "u").equal("böu€"));
    assert("äöü€".substitute!("ä", "b", "ü", "u").equal("böu€"));
    assert("ä...öü€".substitute("ä", "b", "ü", "u").equal("b...öu€"));

    auto expected = "emoticons😄😅.😇😈Rock";
    assert("emoticons😄😅😆😇😈rock"
            .substitute("r", "R", "😆", ".").equal(expected));
    assert("emoticons😄😅😆😇😈rock"
            .substitute!("r", "R", "😆", ".").equal(expected));
}

// test with narrow strings (auto-decoding) and single elements
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    assert("äöü€".substitute('ä', 'b', 'ü', 'u').equal("böu€"));
    assert("äöü€".substitute!('ä', 'b', 'ü', 'u').equal("böu€"));

    auto expected = "emoticons😄😅.😇😈Rock";
    assert("emoticons😄😅😆😇😈rock"
            .substitute('r', 'R', '😆', '.').equal(expected));
    assert("emoticons😄😅😆😇😈rock"
            .substitute!('r', 'R', '😆', '.').equal(expected));
}

// test auto-decoding {n,w,d} strings X {n,w,d} strings
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    assert("ääöü€".substitute("ä", "b", "ü", "u").equal("bböu€"));
    assert("ääöü€".substitute("ä"w, "b"w, "ü"w, "u"w).equal("bböu€"));
    assert("ääöü€".substitute("ä"d, "b"d, "ü"d, "u"d).equal("bböu€"));

    assert("ääöü€"w.substitute("ä", "b", "ü", "u").equal("bböu€"));
    assert("ääöü€"w.substitute("ä"w, "b"w, "ü"w, "u"w).equal("bböu€"));
    assert("ääöü€"w.substitute("ä"d, "b"d, "ü"d, "u"d).equal("bböu€"));

    assert("ääöü€"d.substitute("ä", "b", "ü", "u").equal("bböu€"));
    assert("ääöü€"d.substitute("ä"w, "b"w, "ü"w, "u"w).equal("bböu€"));
    assert("ääöü€"d.substitute("ä"d, "b"d, "ü"d, "u"d).equal("bböu€"));

    // auto-decoding is done before by a different range
    assert("ääöü€".filter!(a => true).substitute("ä", "b", "ü", "u").equal("bböu€"));
    assert("ääöü€".filter!(a => true).substitute("ä"w, "b"w, "ü"w, "u"w).equal("bböu€"));
    assert("ääöü€".filter!(a => true).substitute("ä"d, "b"d, "ü"d, "u"d).equal("bböu€"));
}

// test repeated replacement
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;

    assert([1, 2, 3, 1, 1, 2].substitute(1, 0).equal([0, 2, 3, 0, 0, 2]));
    assert([1, 2, 3, 1, 1, 2].substitute!(1, 0).equal([0, 2, 3, 0, 0, 2]));
    assert([1, 2, 3, 1, 1, 2].substitute(1, 2, 2, 9).equal([2, 9, 3, 2, 2, 9]));
}

// test @nogc for single element replacements
@safe @nogc unittest
{
    import std.algorithm.comparison : equal;

    static immutable arr = [1, 2, 3, 1, 1, 2];
    static immutable expected = [0, 2, 3, 0, 0, 2];

    assert(arr.substitute!(1, 0).equal(expected));
    assert(arr.substitute(1, 0).equal(expected));
}

// test different range types
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : AllDummyRanges;

    static foreach (DummyType; AllDummyRanges)
    {{
        DummyType dummyRange;

        // single substitution
        dummyRange.substitute (2, 22).equal([1, 22, 3, 4, 5, 6, 7, 8, 9, 10]);
        dummyRange.substitute!(2, 22).equal([1, 22, 3, 4, 5, 6, 7, 8, 9, 10]);

        // multiple substitution
        dummyRange.substitute (2, 22, 5, 55, 7, 77).equal([1, 22, 3, 4, 55, 6, 77, 8, 9, 10]);
        dummyRange.substitute!(2, 22, 5, 55, 7, 77).equal([1, 22, 3, 4, 55, 6, 77, 8, 9, 10]);
    }}
}

// https://issues.dlang.org/show_bug.cgi?id=19207
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    assert([1, 2, 3, 4].substitute([1], [7]).equal([7, 2, 3, 4]));
    assert([1, 2, 3, 4].substitute([2], [7]).equal([1, 7, 3, 4]));
    assert([1, 2, 3, 4].substitute([4], [7]).equal([1, 2, 3, 7]));
    assert([1, 2, 3, 4].substitute([2, 3], [7]).equal([1, 7, 4]));
    assert([1, 2, 3, 4].substitute([3, 4], [7, 8]).equal([1, 2, 7, 8]));
}

// tests recognizing empty base ranges
nothrow pure @safe unittest
{
    import std.utf : byCodeUnit;
    import std.algorithm.comparison : equal;

    assert("".byCodeUnit.substitute('4', 'A').empty);
    assert("".byCodeUnit.substitute('0', 'O', '5', 'S', '1', 'l').empty);
    assert("".byCodeUnit.substitute("PKM".byCodeUnit, "PoKeMon".byCodeUnit).empty);
    assert("".byCodeUnit.substitute
    (   "ding".byCodeUnit,
        "dong".byCodeUnit,
        "click".byCodeUnit,
        "clack".byCodeUnit,
        "ping".byCodeUnit,
        "latency".byCodeUnit
    ).empty);
}

// sum
/**
Sums elements of `r`, which must be a finite
$(REF_ALTTEXT input range, isInputRange, std,range,primitives). Although
conceptually `sum(r)` is equivalent to $(LREF fold)!((a, b) => a +
b)(r, 0), `sum` uses specialized algorithms to maximize accuracy,
as follows.

$(UL
$(LI If $(REF ElementType, std,range,primitives)!R is a floating-point
type and `R` is a
$(REF_ALTTEXT random-access range, isRandomAccessRange, std,range,primitives) with
length and slicing, then `sum` uses the
$(HTTP en.wikipedia.org/wiki/Pairwise_summation, pairwise summation)
algorithm.)
$(LI If `ElementType!R` is a floating-point type and `R` is a
finite input range (but not a random-access range with slicing), then
`sum` uses the $(HTTP en.wikipedia.org/wiki/Kahan_summation,
Kahan summation) algorithm.)
$(LI In all other cases, a simple element by element addition is done.)
)

For floating point inputs, calculations are made in
$(DDLINK spec/type, Types, `real`)
precision for `real` inputs and in `double` precision otherwise
(Note this is a special case that deviates from `fold`'s behavior,
which would have kept `float` precision for a `float` range).
For all other types, the calculations are done in the same type obtained
from from adding two elements of the range, which may be a different
type from the elements themselves (for example, in case of
$(DDSUBLINK spec/type,integer-promotions, integral promotion)).

A seed may be passed to `sum`. Not only will this seed be used as an initial
value, but its type will override all the above, and determine the algorithm
and precision used for summation. If a seed is not passed, one is created with
the value of `typeof(r.front + r.front)(0)`, or `typeof(r.front + r.front).zero`
if no constructor exists that takes an int.

Note that these specialized summing algorithms execute more primitive operations
than vanilla summation. Therefore, if in certain cases maximum speed is required
at expense of precision, one can use `fold!((a, b) => a + b)(r, 0)`, which
is not specialized for summation.

Params:
    seed = the initial value of the summation
    r = a finite input range

Returns:
    The sum of all the elements in the range r.
 */
auto sum(R)(R r)
if (isInputRange!R && !isInfinite!R && is(typeof(r.front + r.front)))
{
    alias E = Unqual!(ElementType!R);
    static if (isFloatingPoint!E)
        alias Seed = typeof(E.init  + 0.0); //biggest of double/real
    else
        alias Seed = typeof(r.front + r.front);
    static if (is(typeof(Unqual!Seed(0))))
        enum seedValue = Unqual!Seed(0);
    else static if (is(typeof({ Unqual!Seed tmp = Seed.zero; })))
        enum Unqual!Seed seedValue = Seed.zero;
    else
        static assert(false,
            "Could not initiate an initial value for " ~ (Unqual!Seed).stringof
            ~ ". Please supply an initial value manually.");
    return sum(r, seedValue);
}
/// ditto
auto sum(R, E)(R r, E seed)
if (isInputRange!R && !isInfinite!R && is(typeof(seed = seed + r.front)))
{
    static if (isFloatingPoint!E)
    {
        static if (hasLength!R && hasSlicing!R)
        {
            if (r.empty) return seed;
            return seed + sumPairwise!E(r);
        }
        else
            return sumKahan!E(seed, r);
    }
    else
    {
        return reduce!"a + b"(seed, r);
    }
}

/// Ditto
@safe pure nothrow unittest
{
    import std.range;

    //simple integral sumation
    assert(sum([ 1, 2, 3, 4]) == 10);

    //with integral promotion
    assert(sum([false, true, true, false, true]) == 3);
    assert(sum(ubyte.max.repeat(100)) == 25500);

    //The result may overflow
    assert(uint.max.repeat(3).sum()           ==  4294967293U );
    //But a seed can be used to change the sumation primitive
    assert(uint.max.repeat(3).sum(ulong.init) == 12884901885UL);

    //Floating point sumation
    assert(sum([1.0, 2.0, 3.0, 4.0]) == 10);

    //Floating point operations have double precision minimum
    static assert(is(typeof(sum([1F, 2F, 3F, 4F])) == double));
    assert(sum([1F, 2, 3, 4]) == 10);

    //Force pair-wise floating point sumation on large integers
    import std.math.operations : isClose;
    assert(iota(ulong.max / 2, ulong.max / 2 + 4096).sum(0.0)
               .isClose((ulong.max / 2) * 4096.0 + 4096^^2 / 2));
}

// Pairwise summation http://en.wikipedia.org/wiki/Pairwise_summation
private auto sumPairwise(F, R)(R data)
if (isInputRange!R && !isInfinite!R)
{
    import core.bitop : bsf;
    // Works for r with at least length < 2^^(64 + log2(16)), in keeping with the use of size_t
    // elsewhere in std.algorithm and std.range on 64 bit platforms. The 16 in log2(16) comes
    // from the manual unrolling in sumPairWise16
    F[64] store = void;
    size_t idx = 0;

    void collapseStore(T)(T k)
    {
        auto lastToKeep = idx - cast(uint) bsf(k+1);
        while (idx > lastToKeep)
        {
            store[idx - 1] += store[idx];
            --idx;
        }
    }

    static if (hasLength!R)
    {
        foreach (k; 0 .. data.length / 16)
        {
            static if (isRandomAccessRange!R && hasSlicing!R)
            {
                store[idx] = sumPairwise16!F(data);
                data = data[16 .. data.length];
            }
            else store[idx] = sumPairwiseN!(16, false, F)(data);

            collapseStore(k);
            ++idx;
        }

        size_t i = 0;
        foreach (el; data)
        {
            store[idx] = el;
            collapseStore(i);
            ++idx;
            ++i;
        }
    }
    else
    {
        size_t k = 0;
        while (!data.empty)
        {
            store[idx] = sumPairwiseN!(16, true, F)(data);
            collapseStore(k);
            ++idx;
            ++k;
        }
    }

    F s = store[idx - 1];
    foreach_reverse (j; 0 .. idx - 1)
        s += store[j];

    return s;
}

private auto sumPairwise16(F, R)(R r)
if (isRandomAccessRange!R)
{
    return (((cast(F) r[ 0] + r[ 1]) + (cast(F) r[ 2] + r[ 3]))
          + ((cast(F) r[ 4] + r[ 5]) + (cast(F) r[ 6] + r[ 7])))
         + (((cast(F) r[ 8] + r[ 9]) + (cast(F) r[10] + r[11]))
          + ((cast(F) r[12] + r[13]) + (cast(F) r[14] + r[15])));
}

private auto sumPair(bool needEmptyChecks, F, R)(ref R r)
if (isForwardRange!R && !isRandomAccessRange!R)
{
    static if (needEmptyChecks) if (r.empty) return F(0);
    F s0 = r.front;
    r.popFront();
    static if (needEmptyChecks) if (r.empty) return s0;
    s0 += r.front;
    r.popFront();
    return s0;
}

private auto sumPairwiseN(size_t N, bool needEmptyChecks, F, R)(ref R r)
if (isForwardRange!R && !isRandomAccessRange!R)
{
    import std.math.traits : isPowerOf2;
    static assert(isPowerOf2(N), "N must be a power of 2");
    static if (N == 2) return sumPair!(needEmptyChecks, F)(r);
    else return sumPairwiseN!(N/2, needEmptyChecks, F)(r)
        + sumPairwiseN!(N/2, needEmptyChecks, F)(r);
}

// Kahan algo http://en.wikipedia.org/wiki/Kahan_summation_algorithm
private auto sumKahan(Result, R)(Result result, R r)
{
    static assert(isFloatingPoint!Result && isMutable!Result, "The type of"
            ~ " Result must be a mutable floating point, not "
            ~ Result.stringof);
    Result c = 0;
    for (; !r.empty; r.popFront())
    {
        immutable y = r.front - c;
        immutable t = result + y;
        c = (t - result) - y;
        result = t;
    }
    return result;
}

@safe pure nothrow unittest
{
    static assert(is(typeof(sum([cast( byte) 1])) ==  int));
    static assert(is(typeof(sum([cast(ubyte) 1])) ==  int));
    static assert(is(typeof(sum([  1,   2,   3,   4])) ==  int));
    static assert(is(typeof(sum([ 1U,  2U,  3U,  4U])) == uint));
    static assert(is(typeof(sum([ 1L,  2L,  3L,  4L])) ==  long));
    static assert(is(typeof(sum([1UL, 2UL, 3UL, 4UL])) == ulong));

    int[] empty;
    assert(sum(empty) == 0);
    assert(sum([42]) == 42);
    assert(sum([42, 43]) == 42 + 43);
    assert(sum([42, 43, 44]) == 42 + 43 + 44);
    assert(sum([42, 43, 44, 45]) == 42 + 43 + 44 + 45);
}

@safe pure nothrow unittest
{
    static assert(is(typeof(sum([1.0, 2.0, 3.0, 4.0])) == double));
    static assert(is(typeof(sum([ 1F,  2F,  3F,  4F])) == double));
    const(float[]) a = [1F, 2F, 3F, 4F];
    assert(sum(a) == 10F);
    static assert(is(typeof(sum(a)) == double));

    double[] empty;
    assert(sum(empty) == 0);
    assert(sum([42.]) == 42);
    assert(sum([42., 43.]) == 42 + 43);
    assert(sum([42., 43., 44.]) == 42 + 43 + 44);
    assert(sum([42., 43., 44., 45.5]) == 42 + 43 + 44 + 45.5);
}

@safe pure nothrow unittest
{
    import std.container;
    static assert(is(typeof(sum(SList!float()[])) == double));
    static assert(is(typeof(sum(SList!double()[])) == double));
    static assert(is(typeof(sum(SList!real()[])) == real));

    assert(sum(SList!double()[]) == 0);
    assert(sum(SList!double(1)[]) == 1);
    assert(sum(SList!double(1, 2)[]) == 1 + 2);
    assert(sum(SList!double(1, 2, 3)[]) == 1 + 2 + 3);
    assert(sum(SList!double(1, 2, 3, 4)[]) == 10);
}

// https://issues.dlang.org/show_bug.cgi?id=12434
@safe pure nothrow unittest
{
    immutable a = [10, 20];
    auto s1 = sum(a);
    assert(s1 == 30);
    auto s2 = a.map!(x => x).sum;
    assert(s2 == 30);
}

@system unittest
{
    import std.bigint;
    import std.range;

    immutable BigInt[] a = BigInt("1_000_000_000_000_000_000").repeat(10).array();
    immutable ulong[]  b = (ulong.max/2).repeat(10).array();
    auto sa = a.sum();
    auto sb = b.sum(BigInt(0)); //reduce ulongs into bigint
    assert(sa == BigInt("10_000_000_000_000_000_000"));
    assert(sb == (BigInt(ulong.max/2) * 10));
}

@safe pure nothrow @nogc unittest
{
    import std.range;
    foreach (n; iota(50))
        assert(repeat(1.0, n).sum == n);
}

// Issue 19525
@safe unittest
{
    import std.datetime : Duration, minutes;
    assert([1.minutes].sum() == 1.minutes);
}

/**
Finds the mean (colloquially known as the average) of a range.

For built-in numerical types, accurate Knuth & Welford mean calculation
is used. For user-defined types, element by element summation is used.
Additionally an extra parameter `seed` is needed in order to correctly
seed the summation with the equivalent to `0`.

The first overload of this function will return `T.init` if the range
is empty. However, the second overload will return `seed` on empty ranges.

This function is $(BIGOH r.length).

Params:
    T = The type of the return value.
    r = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    seed = For user defined types. Should be equivalent to `0`.

Returns:
    The mean of `r` when `r` is non-empty.
*/
T mean(T = double, R)(R r)
if (isInputRange!R &&
    isNumeric!(ElementType!R) &&
    !isInfinite!R)
{
    if (r.empty)
        return T.init;

    Unqual!T meanRes = 0;
    size_t i = 1;

    // Knuth & Welford mean calculation
    // division per element is slower, but more accurate
    for (; !r.empty; r.popFront())
    {
        T delta = r.front - meanRes;
        meanRes += delta / i++;
    }

    return meanRes;
}

/// ditto
auto mean(R, T)(R r, T seed)
if (isInputRange!R &&
    !isNumeric!(ElementType!R) &&
    is(typeof(r.front + seed)) &&
    is(typeof(r.front / size_t(1))) &&
    !isInfinite!R)
{
    import std.algorithm.iteration : sum, reduce;

    // per item division vis-a-vis the previous overload is too
    // inaccurate for integer division, which the user defined
    // types might be representing
    static if (hasLength!R)
    {
        if (r.length == 0)
            return seed;

        return sum(r, seed) / r.length;
    }
    else
    {
        import std.typecons : tuple;

        if (r.empty)
            return seed;

        auto pair = reduce!((a, b) => tuple(a[0] + 1, a[1] + b))
            (tuple(size_t(0), seed), r);
        return pair[1] / pair[0];
    }
}

///
@safe @nogc pure nothrow unittest
{
    import std.math.operations : isClose;
    import std.math.traits : isNaN;

    static immutable arr1 = [1, 2, 3];
    static immutable arr2 = [1.5, 2.5, 12.5];

    assert(arr1.mean.isClose(2));
    assert(arr2.mean.isClose(5.5));

    assert(arr1[0 .. 0].mean.isNaN);
}

@safe pure nothrow unittest
{
    import std.internal.test.dummyrange : ReferenceInputRange;
    import std.math.operations : isClose;

    auto r1 = new ReferenceInputRange!int([1, 2, 3]);
    assert(r1.mean.isClose(2));

    auto r2 = new ReferenceInputRange!double([1.5, 2.5, 12.5]);
    assert(r2.mean.isClose(5.5));
}

// Test user defined types
@system pure unittest
{
    import std.bigint : BigInt;
    import std.internal.test.dummyrange : ReferenceInputRange;
    import std.math.operations : isClose;

    auto bigint_arr = [BigInt("1"), BigInt("2"), BigInt("3"), BigInt("6")];
    auto bigint_arr2 = new ReferenceInputRange!BigInt([
        BigInt("1"), BigInt("2"), BigInt("3"), BigInt("6")
    ]);
    assert(bigint_arr.mean(BigInt(0)) == BigInt("3"));
    assert(bigint_arr2.mean(BigInt(0)) == BigInt("3"));

    BigInt[] bigint_arr3 = [];
    assert(bigint_arr3.mean(BigInt(0)) == BigInt(0));

    struct MyFancyDouble
    {
       double v;
       alias v this;
    }

    // both overloads
    auto d_arr = [MyFancyDouble(10), MyFancyDouble(15), MyFancyDouble(30)];
    assert(mean!(double)(cast(double[]) d_arr).isClose(18.33333333));
    assert(mean(d_arr, MyFancyDouble(0)).isClose(18.33333333));
}

// uniq
/**
Lazily iterates unique consecutive elements of the given range, which is
assumed to be sorted (functionality akin to the
$(HTTP wikipedia.org/wiki/_Uniq, _uniq) system
utility). Equivalence of elements is assessed by using the predicate
`pred`, by default `"a == b"`. The predicate is passed to
$(REF binaryFun, std,functional), and can either accept a string, or any callable
that can be executed via `pred(element, element)`. If the given range is
bidirectional, `uniq` also yields a
$(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives).

Params:
    pred = Predicate for determining equivalence between range elements.
    r = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) of
        elements to filter.

Returns:
    An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) of
    consecutively unique elements in the original range. If `r` is also a
    forward range or bidirectional range, the returned range will be likewise.
*/
auto uniq(alias pred = "a == b", Range)(Range r)
if (isInputRange!Range && is(typeof(binaryFun!pred(r.front, r.front)) == bool))
{
    return UniqResult!(binaryFun!pred, Range)(r);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.mutation : copy;

    int[] arr = [ 1, 2, 2, 2, 2, 3, 4, 4, 4, 5 ];
    assert(equal(uniq(arr), [ 1, 2, 3, 4, 5 ][]));

    // Filter duplicates in-place using copy
    arr.length -= arr.uniq().copy(arr).length;
    assert(arr == [ 1, 2, 3, 4, 5 ]);

    // Note that uniqueness is only determined consecutively; duplicated
    // elements separated by an intervening different element will not be
    // eliminated:
    assert(equal(uniq([ 1, 1, 2, 1, 1, 3, 1]), [1, 2, 1, 3, 1]));
}

private struct UniqResult(alias pred, Range)
{
    Range _input;

    this(Range input)
    {
        _input = input;
    }

    auto opSlice()
    {
        return this;
    }

    void popFront()
    {
        assert(!empty, "Attempting to popFront an empty uniq.");
        auto last = _input.front;
        do
        {
            _input.popFront();
        }
        while (!_input.empty && pred(last, _input.front));
    }

    @property ElementType!Range front()
    {
        assert(!empty, "Attempting to fetch the front of an empty uniq.");
        return _input.front;
    }

    static if (isBidirectionalRange!Range)
    {
        void popBack()
        {
            assert(!empty, "Attempting to popBack an empty uniq.");
            auto last = _input.back;
            do
            {
                _input.popBack();
            }
            while (!_input.empty && pred(last, _input.back));
        }

        @property ElementType!Range back()
        {
            assert(!empty, "Attempting to fetch the back of an empty uniq.");
            return _input.back;
        }
    }

    static if (isInfinite!Range)
    {
        enum bool empty = false;  // Propagate infiniteness.
    }
    else
    {
        @property bool empty() { return _input.empty; }
    }

    static if (isForwardRange!Range)
    {
        @property typeof(this) save() {
            return typeof(this)(_input.save);
        }
    }
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange;
    import std.range;

    int[] arr = [ 1, 2, 2, 2, 2, 3, 4, 4, 4, 5 ];
    auto r = uniq(arr);
    static assert(isForwardRange!(typeof(r)));

    assert(equal(r, [ 1, 2, 3, 4, 5 ][]));
    assert(equal(retro(r), retro([ 1, 2, 3, 4, 5 ][])));

    foreach (DummyType; AllDummyRanges)
    {
        DummyType d;
        auto u = uniq(d);
        assert(equal(u, [1,2,3,4,5,6,7,8,9,10]));

        static assert(d.rt == RangeType.Input || isForwardRange!(typeof(u)));

        static if (d.rt >= RangeType.Bidirectional)
        {
            assert(equal(retro(u), [10,9,8,7,6,5,4,3,2,1]));
        }
    }
}

// https://issues.dlang.org/show_bug.cgi?id=17264
@safe unittest
{
    import std.algorithm.comparison : equal;

    const(int)[] var = [0, 1, 1, 2];
    assert(var.uniq.equal([0, 1, 2]));
}

/**
Lazily computes all _permutations of `r` using $(HTTP
en.wikipedia.org/wiki/Heap%27s_algorithm, Heap's algorithm).

Params:
    Range = the range type
    r = the $(REF_ALTTEXT random access range, isRandomAccessRange, std,range,primitives)
    to find the permutations for.
Returns:
    A $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
    of elements of which are an $(REF indexed, std,range) view into `r`.

See_Also:
$(REF nextPermutation, std,algorithm,sorting).
*/
Permutations!Range permutations(Range)(Range r)
{
    static assert(isRandomAccessRange!Range, Range.stringof,
            " must be a RandomAccessRange");
    static assert(hasLength!Range, Range.stringof
            , " must have a length");

    return typeof(return)(r);
}

/// ditto
struct Permutations(Range)
{
    static assert(isRandomAccessRange!Range, Range.stringof,
            " must be a RandomAccessRange");
    static assert(hasLength!Range, Range.stringof
            , " must have a length");

    private size_t[] _indices, _state;
    private Range _r;
    private bool _empty;

    ///
    this(Range r)
    {
        import std.array : array;
        import std.range : iota;

        this._r = r;
        _state = r.length ? new size_t[r.length-1] : null;
        _indices = iota(size_t(r.length)).array;
        _empty = r.length == 0;
    }
    private this(size_t[] indices, size_t[] state, Range r, bool empty_)
    {
        _indices = indices;
        _state = state;
        _r = r;
        _empty = empty_;
    }
    /// Returns: `true` if the range is empty, `false` otherwise.
    @property bool empty() const pure nothrow @safe @nogc
    {
        return _empty;
    }

    /// Returns: the front of the range
    @property auto front()
    {
        import std.range : indexed;
        return _r.indexed(_indices);
    }

    ///
    void popFront()
    {
        void next(int n)
        {
            import std.algorithm.mutation : swap;

            if (n > _indices.length)
            {
                _empty = true;
                return;
            }

            if (n % 2 == 1)
                swap(_indices[0], _indices[n-1]);
            else
                swap(_indices[_state[n-2]], _indices[n-1]);

            if (++_state[n-2] == n)
            {
                _state[n-2] = 0;
                next(n+1);
            }
        }

        next(2);
    }
    /// Returns: an independent copy of the permutations range.
    auto save()
    {
        return typeof(this)(_indices.dup, _state.dup, _r.save, _empty);
    }
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : iota;
    assert(equal!equal(iota(3).permutations,
        [[0, 1, 2],
         [1, 0, 2],
         [2, 0, 1],
         [0, 2, 1],
         [1, 2, 0],
         [2, 1, 0]]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : ElementType;
    import std.array : array;
    auto p = [1, 2, 3].permutations;
    auto x = p.save.front;
    p.popFront;
    auto y = p.front;
    assert(x != y);
}
