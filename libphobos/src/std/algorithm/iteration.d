// Written in the D programming language.
/**
This is a submodule of $(MREF std, algorithm).
It contains generic _iteration algorithms.

$(SCRIPT inhibitQuickIndex = 1;)
$(BOOKTABLE Cheat Sheet,
$(TR $(TH Function Name) $(TH Description))
$(T2 cache,
        Eagerly evaluates and caches another range's $(D front).)
$(T2 cacheBidirectional,
        As above, but also provides $(D back) and $(D popBack).)
$(T2 chunkBy,
        $(D chunkBy!((a,b) => a[1] == b[1])([[1, 1], [1, 2], [2, 2], [2, 1]]))
        returns a range containing 3 subranges: the first with just
        $(D [1, 1]); the second with the elements $(D [1, 2]) and $(D [2, 2]);
        and the third with just $(D [2, 1]).)
$(T2 cumulativeFold,
        $(D cumulativeFold!((a, b) => a + b)([1, 2, 3, 4])) returns a
        lazily-evaluated range containing the successive reduced values `1`,
        `3`, `6`, `10`.)
$(T2 each,
        $(D each!writeln([1, 2, 3])) eagerly prints the numbers $(D 1), $(D 2)
        and $(D 3) on their own lines.)
$(T2 filter,
        $(D filter!(a => a > 0)([1, -1, 2, 0, -3])) iterates over elements $(D 1)
        and $(D 2).)
$(T2 filterBidirectional,
        Similar to $(D filter), but also provides $(D back) and $(D popBack) at
        a small increase in cost.)
$(T2 fold,
        $(D fold!((a, b) => a + b)([1, 2, 3, 4])) returns $(D 10).)
$(T2 group,
        $(D group([5, 2, 2, 3, 3])) returns a range containing the tuples
        $(D tuple(5, 1)), $(D tuple(2, 2)), and $(D tuple(3, 2)).)
$(T2 joiner,
        $(D joiner(["hello", "world!"], "; ")) returns a range that iterates
        over the characters $(D "hello; world!"). No new string is created -
        the existing inputs are iterated.)
$(T2 map,
        $(D map!(a => a * 2)([1, 2, 3])) lazily returns a range with the numbers
        $(D 2), $(D 4), $(D 6).)
$(T2 permutations,
        Lazily computes all permutations using Heap's algorithm.)
$(T2 reduce,
        $(D reduce!((a, b) => a + b)([1, 2, 3, 4])) returns $(D 10).
        This is the old implementation of `fold`.)
$(T2 splitter,
        Lazily splits a range by a separator.)
$(T2 sum,
        Same as $(D fold), but specialized for accurate summation.)
$(T2 uniq,
        Iterates over the unique elements in a range, which is assumed sorted.)
)

Copyright: Andrei Alexandrescu 2008-.

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP erdani.com, Andrei Alexandrescu)

Source: $(PHOBOSSRC std/algorithm/_iteration.d)

Macros:
T2=$(TR $(TDNW $(LREF $1)) $(TD $+))
 */
module std.algorithm.iteration;

// FIXME
import std.functional; // : unaryFun, binaryFun;
import std.range.primitives;
import std.traits;

private template aggregate(fun...)
if (fun.length >= 1)
{
    /* --Intentionally not ddoc--
     * Aggregates elements in each subrange of the given range of ranges using
     * the given aggregating function(s).
     * Params:
     *  fun = One or more aggregating functions (binary functions that return a
     *      single _aggregate value of their arguments).
     *  ror = A range of ranges to be aggregated.
     *
     * Returns:
     * A range representing the aggregated value(s) of each subrange
     * of the original range. If only one aggregating function is specified,
     * each element will be the aggregated value itself; if multiple functions
     * are specified, each element will be a tuple of the aggregated values of
     * each respective function.
     */
    auto aggregate(RoR)(RoR ror)
        if (isInputRange!RoR && isIterable!(ElementType!RoR))
    {
        return ror.map!(reduce!fun);
    }

    @safe unittest
    {
        import std.algorithm.comparison : equal, max, min;

        auto data = [[4, 2, 1, 3], [4, 9, -1, 3, 2], [3]];

        // Single aggregating function
        auto agg1 = data.aggregate!max;
        assert(agg1.equal([4, 9, 3]));

        // Multiple aggregating functions
        import std.typecons : tuple;
        auto agg2 = data.aggregate!(max, min);
        assert(agg2.equal([
            tuple(4, 1),
            tuple(9, -1),
            tuple(3, 3)
        ]));
    }
}

/++
$(D cache) eagerly evaluates $(D front) of $(D range)
on each construction or call to $(D popFront),
to store the result in a cache.
The result is then directly returned when $(D front) is called,
rather than re-evaluated.

This can be a useful function to place in a chain, after functions
that have expensive evaluation, as a lazy alternative to $(REF array, std,array).
In particular, it can be placed after a call to $(D map), or before a call
to $(D filter).

$(D cache) may provide
$(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives)
iteration if needed, but since this comes at an increased cost, it must be explicitly requested via the
call to $(D cacheBidirectional). Furthermore, a bidirectional cache will
evaluate the "center" element twice, when there is only one element left in
the range.

$(D cache) does not provide random access primitives,
as $(D cache) would be unable to cache the random accesses.
If $(D Range) provides slicing primitives,
then $(D cache) will provide the same slicing primitives,
but $(D hasSlicing!Cache) will not yield true (as the $(REF hasSlicing, std,_range,primitives)
trait also checks for random access).

Params:
    range = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)

Returns:
    an input range with the cached values of range
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

/++
Tip: $(D cache) is eager when evaluating elements. If calling front on the
underlying _range has a side effect, it will be observable before calling
front on the actual cached _range.

Furthermore, care should be taken composing $(D cache) with $(REF take, std,_range).
By placing $(D take) before $(D cache), then $(D cache) will be "aware"
of when the _range ends, and correctly stop caching elements when needed.
If calling front has no side effect though, placing $(D take) after $(D cache)
may yield a faster _range.

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
    }

    static if (isInfinite!R)
        enum empty = false;
    else
        bool empty() @property
        {
            return source.empty;
        }

    static if (hasLength!R) auto length() @property
    {
        return source.length;
    }

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
            caches = CacheTypes.init;
    }
    static if (bidir) void popBack()
    {
        version (assert) if (empty) throw new RangeError();
        source.popBack();
        if (!source.empty)
            caches[1] = source.back;
        else
            caches = CacheTypes.init;
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
            body
            {
                import std.range : takeExactly;
                return this[low .. $].takeExactly(high - low);
            }
        }
    }
}

/**
$(D auto map(Range)(Range r) if (isInputRange!(Unqual!Range));)

Implements the homonym function (also known as $(D transform)) present
in many languages of functional flavor. The call $(D map!(fun)(range))
returns a range of which elements are obtained by applying $(D fun(a))
left to right for all elements $(D a) in $(D range). The original ranges are
not changed. Evaluation is done lazily.

Params:
    fun = one or more transformation functions
    r = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)

Returns:
    a range with each fun applied to all the elements. If there is more than one
    fun, the element type will be $(D Tuple) containing one element for each fun.

See_Also:
    $(HTTP en.wikipedia.org/wiki/Map_(higher-order_function), Map (higher-order function))
*/
template map(fun...)
if (fun.length >= 1)
{
    auto map(Range)(Range r) if (isInputRange!(Unqual!Range))
    {
        import std.meta : AliasSeq, staticMap;

        alias RE = ElementType!(Range);
        static if (fun.length > 1)
        {
            import std.functional : adjoin;
            import std.meta : staticIndexOf;

            alias _funs = staticMap!(unaryFun, fun);
            alias _fun = adjoin!_funs;

            // Once DMD issue #5710 is fixed, this validation loop can be moved into a template.
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

            // Do the validation separately for single parameters due to DMD issue #15777.
            static assert(!is(typeof(_fun(RE.init)) == void),
                "Mapping function(s) must not return void: " ~ _funs.stringof);
        }

        return MapResult!(_fun, Range)(r);
    }
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : chain;
    int[] arr1 = [ 1, 2, 3, 4 ];
    int[] arr2 = [ 5, 6 ];
    auto squares = map!(a => a * a)(chain(arr1, arr2));
    assert(equal(squares, [ 1, 4, 9, 16, 25, 36 ]));
}

/**
Multiple functions can be passed to $(D map). In that case, the
element type of $(D map) is a tuple containing one element for each
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
You may alias $(D map) with some function(s) to a symbol and use
it separately:
*/
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : to;

    alias stringize = map!(to!string);
    assert(equal(stringize([ 1, 2, 3, 4 ]), [ "1", "2", "3", "4" ]));
}

@safe unittest
{
    // Verify workaround for DMD #15777

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
        static if (is(typeof(_input[ulong.max])))
            private alias opIndex_t = ulong;
        else
            private alias opIndex_t = uint;

        auto ref opIndex(opIndex_t index)
        {
            return fun(_input[index]);
        }
    }

    static if (hasLength!R)
    {
        @property auto length()
        {
            return _input.length;
        }

        alias opDollar = length;
    }

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
    import std.random : unpredictableSeed, uniform, Random;

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
    auto gen = Random(unpredictableSeed);
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
    auto ms1 = map!(std.ascii.toUpper)(s1);
    auto ms2 = map!(std.ascii.toUpper)(s2);
    auto ms3 = map!(std.ascii.toUpper)(s3);
    static assert(!is(ms1[0])); //narrow strings can't be indexed
    assert(ms2[0] == '日');
    assert(ms3[0] == 'H');
    static assert(!is(ms1[0 .. 1])); //narrow strings can't be sliced
    assert(equal(ms2[0 .. 2], "日本"w));
    assert(equal(ms3[0 .. 2], "HE"));

    // Issue 5753
    static void voidFun(int) {}
    static int nonvoidFun(int) { return 0; }
    static assert(!__traits(compiles, map!voidFun([1])));
    static assert(!__traits(compiles, map!(voidFun, voidFun)([1])));
    static assert(!__traits(compiles, map!(nonvoidFun, voidFun)([1])));
    static assert(!__traits(compiles, map!(voidFun, nonvoidFun)([1])));
    static assert(!__traits(compiles, map!(a => voidFun(a))([1])));

    // Phobos issue #15480
    auto dd = map!(z => z * z, c => c * c * c)([ 1, 2, 3, 4 ]);
    assert(dd[0] == tuple(1, 1));
    assert(dd[1] == tuple(4, 8));
    assert(dd[2] == tuple(9, 27));
    assert(dd[3] == tuple(16, 64));
    assert(dd.length == 4);
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

    // Issue #10130 - map of iota with const step.
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

// each
/**
Eagerly iterates over $(D r) and calls $(D pred) over _each element.

If no predicate is specified, $(D each) will default to doing nothing
but consuming the entire range. $(D .front) will be evaluated, but this
can be avoided by explicitly specifying a predicate lambda with a
$(D lazy) parameter.

$(D each) also supports $(D opApply)-based iterators, so it will work
with e.g. $(REF parallel, std,parallelism).

Params:
    pred = predicate to apply to each element of the range
    r = range or iterable over which each iterates

See_Also: $(REF tee, std,range)

 */
template each(alias pred = "a")
{
    import std.meta : AliasSeq;
    import std.traits : Parameters;

private:
    alias BinaryArgs = AliasSeq!(pred, "i", "a");

    enum isRangeUnaryIterable(R) =
        is(typeof(unaryFun!pred(R.init.front)));

    enum isRangeBinaryIterable(R) =
        is(typeof(binaryFun!BinaryArgs(0, R.init.front)));

    enum isRangeIterable(R) =
        isInputRange!R &&
        (isRangeUnaryIterable!R || isRangeBinaryIterable!R);

    enum isForeachUnaryIterable(R) =
        is(typeof((R r) {
            foreach (ref a; r)
                cast(void) unaryFun!pred(a);
        }));

    enum isForeachBinaryIterable(R) =
        is(typeof((R r) {
            foreach (ref i, ref a; r)
                cast(void) binaryFun!BinaryArgs(i, a);
        }));

    enum isForeachIterable(R) =
        (!isForwardRange!R || isDynamicArray!R) &&
        (isForeachUnaryIterable!R || isForeachBinaryIterable!R);

public:
    void each(Range)(Range r)
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
                    cast(void) unaryFun!pred(r.front);
                    r.popFront();
                }
            }
            else // if (isRangeBinaryIterable!Range)
            {
                size_t i = 0;
                while (!r.empty)
                {
                    cast(void) binaryFun!BinaryArgs(i, r.front);
                    r.popFront();
                    i++;
                }
            }
        }
        else
        {
            // range interface with >2 parameters.
            for (auto range = r; !range.empty; range.popFront())
                pred(range.front.expand);
        }
    }

    void each(Iterable)(auto ref Iterable r)
    if (isForeachIterable!Iterable ||
        __traits(compiles, Parameters!(Parameters!(r.opApply))))
    {
        static if (isForeachIterable!Iterable)
        {
            debug(each) pragma(msg, "Using foreach for ", Iterable.stringof);
            static if (isForeachUnaryIterable!Iterable)
            {
                foreach (ref e; r)
                    cast(void) unaryFun!pred(e);
            }
            else // if (isForeachBinaryIterable!Iterable)
            {
                foreach (ref i, ref e; r)
                    cast(void) binaryFun!BinaryArgs(i, e);
            }
        }
        else
        {
            // opApply with >2 parameters. count the delegate args.
            // only works if it is not templated (otherwise we cannot count the args)
            auto dg(Parameters!(Parameters!(r.opApply)) params) {
                pred(params);
                return 0; // tells opApply to continue iteration
            }
            r.opApply(&dg);
        }
    }
}

///
@system unittest
{
    import std.range : iota;

    long[] arr;
    iota(5).each!(n => arr ~= n);
    assert(arr == [0, 1, 2, 3, 4]);

    // If the range supports it, the value can be mutated in place
    arr.each!((ref n) => n++);
    assert(arr == [1, 2, 3, 4, 5]);

    arr.each!"a++";
    assert(arr == [2, 3, 4, 5, 6]);

    // by-ref lambdas are not allowed for non-ref ranges
    static assert(!is(typeof(arr.map!(n => n).each!((ref n) => n++))));

    // The default predicate consumes the range
    auto m = arr.map!(n => n);
    (&m).each();
    assert(m.empty);

    // Indexes are also available for in-place mutations
    arr[] = 0;
    arr.each!"a=i"();
    assert(arr == [0, 1, 2, 3, 4]);

    // opApply iterators work as well
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

// #15358: application of `each` with >2 args (opApply)
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

// #15358: application of `each` with >2 args (range interface)
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

// #16255: `each` on opApply doesn't support ref
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

// #16255: `each` on opApply doesn't support ref
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

// filter
/**
$(D auto filter(Range)(Range rs) if (isInputRange!(Unqual!Range));)

Implements the higher order _filter function. The predicate is passed to
$(REF unaryFun, std,functional), and can either accept a string, or any callable
that can be executed via $(D pred(element)).

Params:
    predicate = Function to apply to each element of range
    range = Input range of elements

Returns:
    $(D filter!(predicate)(range)) returns a new range containing only elements $(D x) in $(D range) for
    which $(D predicate(x)) returns $(D true).

See_Also:
    $(HTTP en.wikipedia.org/wiki/Filter_(higher-order_function), Filter (higher-order function))
 */
template filter(alias predicate)
if (is(typeof(unaryFun!predicate)))
{
    auto filter(Range)(Range range) if (isInputRange!(Unqual!Range))
    {
        return FilterResult!(unaryFun!predicate, Range)(range);
    }
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.math : approxEqual;
    import std.range;

    int[] arr = [ 1, 2, 3, 4, 5 ];

    // Sum all elements
    auto small = filter!(a => a < 3)(arr);
    assert(equal(small, [ 1, 2 ]));

    // Sum again, but with Uniform Function Call Syntax (UFCS)
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
    assert(approxEqual(r1, [ 2.5 ]));
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
        do
        {
            _input.popFront();
        } while (!_input.empty && !pred(_input.front));
        _primed = true;
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

/**
 * $(D auto filterBidirectional(Range)(Range r) if (isBidirectionalRange!(Unqual!Range));)
 *
 * Similar to $(D filter), except it defines a
 * $(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives).
 * There is a speed disadvantage - the constructor spends time
 * finding the last element in the range that satisfies the filtering
 * condition (in addition to finding the first one). The advantage is
 * that the filtered range can be spanned from both directions. Also,
 * $(REF retro, std,range) can be applied against the filtered range.
 *
 * The predicate is passed to $(REF unaryFun, std,functional), and can either
 * accept a string, or any callable that can be executed via $(D pred(element)).
 *
 * Params:
 *     pred = Function to apply to each element of range
 *     r = Bidirectional range of elements
 *
 * Returns:
 *     a new range containing only the elements in r for which pred returns $(D true).
 */
template filterBidirectional(alias pred)
{
    auto filterBidirectional(Range)(Range r) if (isBidirectionalRange!(Unqual!Range))
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

Similarly to $(D uniq), $(D group) produces a range that iterates over unique
consecutive elements of the given range. Each element of this range is a tuple
of the element and the number of times it is repeated in the original range.
Equivalence of elements is assessed by using the predicate $(D pred), which
defaults to $(D "a == b").  The predicate is passed to $(REF binaryFun, std,functional),
and can either accept a string, or any callable that can be executed via
$(D pred(element, element)).

Params:
    pred = Binary predicate for determining equivalence of two elements.
    r = The $(REF_ALTTEXT input range, isInputRange, std,range,primitives) to
        iterate over.

Returns: A range of elements of type $(D Tuple!(ElementType!R, uint)),
representing each consecutively unique element and its respective number of
occurrences in that run.  This will be an input range if $(D R) is an input
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

    ///
    @property auto ref front()
    {
        assert(!empty, "Attempting to fetch the front of an empty Group.");
        return _current;
    }

    static if (isForwardRange!R)
    {
        ///
        @property typeof(this) save() {
            typeof(this) ret = this;
            ret._input = this._input.save;
            ret._current = this._current;
            return ret;
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

    // Issue 13857
    immutable(int)[] a1 = [1,1,2,2,2,3,4,4,5,6,6,7,8,9,9,9];
    auto g1 = group(a1);
    assert(equal(g1, [ tuple(1, 2u), tuple(2, 3u), tuple(3, 1u),
                       tuple(4, 2u), tuple(5, 1u), tuple(6, 2u),
                       tuple(7, 1u), tuple(8, 1u), tuple(9, 3u)
                     ]));

    // Issue 13162
    immutable(ubyte)[] a2 = [1, 1, 1, 0, 0, 0];
    auto g2 = a2.group;
    assert(equal(g2, [ tuple(1, 3u), tuple(0, 3u) ]));

    // Issue 10104
    const a3 = [1, 1, 2, 2];
    auto g3 = a3.group;
    assert(equal(g3, [ tuple(1, 2u), tuple(2, 2u) ]));

    interface I {}
    class C : I {}
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

// Used by implementation of chunkBy for non-forward input ranges.
private struct ChunkByChunkImpl(alias pred, Range)
if (isInputRange!Range && !isForwardRange!Range)
{
    alias fun = binaryFun!pred;

    private Range r;
    private ElementType!Range prev;

    this(Range range, ElementType!Range _prev)
    {
        r = range;
        prev = _prev;
    }

    @property bool empty()
    {
        return r.empty || !fun(prev, r.front);
    }

    @property ElementType!Range front() { return r.front; }
    void popFront() { r.popFront(); }
}

private template ChunkByImplIsUnary(alias pred, Range)
{
    static if (is(typeof(binaryFun!pred(ElementType!Range.init,
                                        ElementType!Range.init)) : bool))
        enum ChunkByImplIsUnary = false;
    else static if (is(typeof(
            unaryFun!pred(ElementType!Range.init) ==
            unaryFun!pred(ElementType!Range.init))))
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
    @property bool empty() { return r.empty; }

    @property auto front()
    {
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

// Single-pass implementation of chunkBy for forward ranges.
private struct ChunkByImpl(alias pred, Range)
if (isForwardRange!Range)
{
    import std.typecons : RefCounted;

    enum bool isUnary = ChunkByImplIsUnary!(pred, Range);

    static if (isUnary)
        alias eq = binaryFun!((a, b) => unaryFun!pred(a) == unaryFun!pred(b));
    else
        alias eq = binaryFun!pred;

    // Outer range
    static struct Impl
    {
        size_t groupNum;
        Range  current;
        Range  next;
    }

    // Inner range
    static struct Group
    {
        private size_t groupNum;
        private Range  start;
        private Range  current;

        private RefCounted!Impl mothership;

        this(RefCounted!Impl origin)
        {
            groupNum = origin.groupNum;

            start = origin.current.save;
            current = origin.current.save;
            assert(!start.empty);

            mothership = origin;

            // Note: this requires reflexivity.
            assert(eq(start.front, current.front),
                   "predicate is not reflexive");
        }

        @property bool empty() { return groupNum == size_t.max; }
        @property auto ref front() { return current.front; }

        void popFront()
        {
            current.popFront();

            // Note: this requires transitivity.
            if (current.empty || !eq(start.front, current.front))
            {
                if (groupNum == mothership.groupNum)
                {
                    // If parent range hasn't moved on yet, help it along by
                    // saving location of start of next Group.
                    mothership.next = current.save;
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
    }
    static assert(isForwardRange!Group);

    private RefCounted!Impl impl;

    this(Range r)
    {
        impl = RefCounted!Impl(0, r, r.save);
    }

    @property bool empty() { return impl.current.empty; }

    @property auto front()
    {
        static if (isUnary)
        {
            import std.typecons : tuple;
            return tuple(unaryFun!pred(impl.current.front), Group(impl));
        }
        else
        {
            return Group(impl);
        }
    }

    void popFront()
    {
        // Scan for next group. If we're lucky, one of our Groups would have
        // already set .next to the start of the next group, in which case the
        // loop is skipped.
        while (!impl.next.empty && eq(impl.current.front, impl.next.front))
        {
            impl.next.popFront();
        }

        impl.current = impl.next.save;

        // Indicate to any remaining Groups that we have moved on.
        impl.groupNum++;
    }

    @property auto save()
    {
        // Note: the new copy of the range will be detached from any existing
        // satellite Groups, and will not benefit from the .next acceleration.
        return typeof(this)(impl.current.save);
    }

    static assert(isForwardRange!(typeof(this)));
}

@system unittest
{
    import std.algorithm.comparison : equal;

    size_t popCount = 0;
    class RefFwdRange
    {
        int[]  impl;

        @safe nothrow:

        this(int[] data) { impl = data; }
        @property bool empty() { return impl.empty; }
        @property auto ref front() { return impl.front; }
        void popFront()
        {
            impl.popFront();
            popCount++;
        }
        @property auto save() { return new RefFwdRange(impl); }
    }
    static assert(isForwardRange!RefFwdRange);

    auto testdata = new RefFwdRange([1, 3, 5, 2, 4, 7, 6, 8, 9]);
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
 * Equivalence is defined by the predicate $(D pred), which can be either
 * binary, which is passed to $(REF binaryFun, std,functional), or unary, which is
 * passed to $(REF unaryFun, std,functional). In the binary form, two _range elements
 * $(D a) and $(D b) are considered equivalent if $(D pred(a,b)) is true. In
 * unary form, two elements are considered equivalent if $(D pred(a) == pred(b))
 * is true.
 *
 * This predicate must be an equivalence relation, that is, it must be
 * reflexive ($(D pred(x,x)) is always true), symmetric
 * ($(D pred(x,y) == pred(y,x))), and transitive ($(D pred(x,y) && pred(y,z))
 * implies $(D pred(x,z))). If this is not the case, the range returned by
 * chunkBy may assert at runtime or behave erratically.
 *
 * Params:
 *  pred = Predicate for determining equivalence.
 *  r = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) to be chunked.
 *
 * Returns: With a binary predicate, a range of ranges is returned in which
 * all elements in a given subrange are equivalent under the given predicate.
 * With a unary predicate, a range of tuples is returned, with the tuple
 * consisting of the result of the unary predicate for each subrange, and the
 * subrange itself.
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
    return ChunkByImpl!(pred, Range)(r);
}

/// Showing usage with binary predicate:
/*FIXME: @safe*/ @system unittest
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

version (none) // this example requires support for non-equivalence relations
@safe unittest
{
    // Grouping by maximum adjacent difference:
    import std.math : abs;
    auto r3 = [1, 3, 2, 5, 4, 9, 10].chunkBy!((a, b) => abs(a-b) < 3);
    assert(r3.equal!equal([
        [1, 3, 2],
        [5, 4],
        [9, 10]
    ]));

}

/// Showing usage with unary predicate:
/* FIXME: pure @safe nothrow*/ @system unittest
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
        @property auto front() pure @safe nothrow { return data.front; }
        void popFront() pure @safe nothrow { data.popFront(); }
    }
    auto refInputRange(R)(R range) { return new RefInputRange!R(range); }

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

    // Test non-forward input ranges.
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
}

// Issue 13595
version (none) // This requires support for non-equivalence relations
@system unittest
{
    import std.algorithm.comparison : equal;
    auto r = [1, 2, 3, 4, 5, 6, 7, 8, 9].chunkBy!((x, y) => ((x*y) % 3) == 0);
    assert(r.equal!equal([
        [1],
        [2, 3, 4],
        [5, 6, 7],
        [8, 9]
    ]));
}

// Issue 13805
@system unittest
{
    [""].map!((s) => s).chunkBy!((x, y) => true);
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
A range of elements in the joined range. This will be a forward range if
both outer and inner ranges of $(D RoR) are forward ranges; otherwise it will
be only an input range.

See_also:
$(REF chain, std,range), which chains a sequence of ranges with compatible elements
into a single range.
 */
auto joiner(RoR, Separator)(RoR r, Separator sep)
if (isInputRange!RoR && isInputRange!(ElementType!RoR)
        && isForwardRange!Separator
        && is(ElementType!Separator : ElementType!(ElementType!RoR)))
{
    static struct Result
    {
        private RoR _items;
        private ElementType!RoR _current;
        private Separator _sep, _currentSep;

        // This is a mixin instead of a function for the following reason (as
        // explained by Kenji Hara): "This is necessary from 2.061.  If a
        // struct has a nested struct member, it must be directly initialized
        // in its constructor to avoid leaving undefined state.  If you change
        // setItem to a function, the initialization of _current field is
        // wrapped into private member function, then compiler could not detect
        // that is correctly initialized while constructing.  To avoid the
        // compiler error check, string mixin is used."
        private enum setItem =
        q{
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
        };

        private void useSeparator()
        {
            // Separator must always come after an item.
            assert(_currentSep.empty && !_items.empty,
                    "joiner: internal error");
            _items.popFront();

            // If there are no more items, we're done, since separators are not
            // terminators.
            if (_items.empty) return;

            if (_sep.empty)
            {
                // Advance to the next range in the
                // input
                while (_items.front.empty)
                {
                    _items.popFront();
                    if (_items.empty) return;
                }
                mixin(setItem);
            }
            else
            {
                _currentSep = _sep.save;
                assert(!_currentSep.empty);
            }
        }

        private enum useItem =
        q{
            // FIXME: this will crash if either _currentSep or _current are
            // class objects, because .init is null when the ctor invokes this
            // mixin.
            //assert(_currentSep.empty && _current.empty,
            //        "joiner: internal error");

            // Use the input
            if (_items.empty) return;
            mixin(setItem);
            if (_current.empty)
            {
                // No data in the current item - toggle to use the separator
                useSeparator();
            }
        };

        this(RoR items, Separator sep)
        {
            _items = items;
            _sep = sep;

            //mixin(useItem); // _current should be initialized in place
            if (_items.empty)
                _current = _current.init;   // set invalid state
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

                if (_current.empty)
                {
                    // No data in the current item - toggle to use the separator
                    useSeparator();
                }
            }
        }

        @property auto empty()
        {
            return _items.empty;
        }

        @property ElementType!(ElementType!RoR) front()
        {
            if (!_currentSep.empty) return _currentSep.front;
            assert(!_current.empty, "Attempting to fetch the front of an empty joiner.");
            return _current.front;
        }

        void popFront()
        {
            assert(!_items.empty, "Attempting to popFront an empty joiner.");
            // Using separator?
            if (!_currentSep.empty)
            {
                _currentSep.popFront();
                if (!_currentSep.empty) return;
                mixin(useItem);
            }
            else
            {
                // we're using the range
                _current.popFront();
                if (!_current.empty) return;
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
                copy._sep = _sep.save;
                copy._currentSep = _currentSep.save;
                return copy;
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

    // Related to issue 8061
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

    // Issue 13441: only(x) as separator
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

/// Ditto
auto joiner(RoR)(RoR r)
if (isInputRange!RoR && isInputRange!(ElementType!RoR))
{
    static struct Result
    {
    private:
        RoR _items;
        ElementType!RoR _current;
        enum prepare =
        q{
            // Skip over empty subranges.
            if (_items.empty) return;
            while (_items.front.empty)
            {
                _items.popFront();
                if (_items.empty) return;
            }
            // We cannot export .save method unless we ensure subranges are not
            // consumed when a .save'd copy of ourselves is iterated over. So
            // we need to .save each subrange we traverse.
            static if (isForwardRange!RoR && isForwardRange!(ElementType!RoR))
                _current = _items.front.save;
            else
                _current = _items.front;
        };
        this(RoR items, ElementType!RoR current)
        {
            _items = items;
            _current = current;
        }
    public:
        this(RoR r)
        {
            _items = r;
            //mixin(prepare); // _current should be initialized in place

            // Skip over empty subranges.
            while (!_items.empty && _items.front.empty)
                _items.popFront();

            if (_items.empty)
                _current = _current.init;   // set invalid state
            else
            {
                // We cannot export .save method unless we ensure subranges are not
                // consumed when a .save'd copy of ourselves is iterated over. So
                // we need to .save each subrange we traverse.
                static if (isForwardRange!RoR && isForwardRange!(ElementType!RoR))
                    _current = _items.front.save;
                else
                    _current = _items.front;
            }
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
                assert(!_items.empty);
                _items.popFront();
                mixin(prepare);
            }
        }
        static if (isForwardRange!RoR && isForwardRange!(ElementType!RoR))
        {
            @property auto save()
            {
                return Result(_items.save, _current.save);
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
    }
    return Result(r);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range.interfaces : inputRangeObject;
    import std.range : repeat;

    static assert(isInputRange!(typeof(joiner([""]))));
    static assert(isForwardRange!(typeof(joiner([""]))));
    assert(equal(joiner([""]), ""));
    assert(equal(joiner(["", ""]), ""));
    assert(equal(joiner(["", "abc"]), "abc"));
    assert(equal(joiner(["abc", ""]), "abc"));
    assert(equal(joiner(["abc", "def"]), "abcdef"));
    assert(equal(joiner(["Mary", "has", "a", "little", "lamb"]),
                    "Maryhasalittlelamb"));
    assert(equal(joiner(repeat("abc", 3)), "abcabcabc"));

    // joiner allows in-place mutation!
    auto a = [ [1, 2, 3], [42, 43] ];
    auto j = joiner(a);
    j.front = 44;
    assert(a == [ [44, 2, 3], [42, 43] ]);
    assert(equal(j, [44, 2, 3, 42, 43]));
}


@system unittest
{
    import std.algorithm.comparison : equal;
    import std.range.interfaces : inputRangeObject;

    // bugzilla 8240
    assert(equal(joiner([inputRangeObject("")]), ""));

    // issue 8792
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

// Issue 8061
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

        enum empty = false;

        void popFront()
        {
        }

        @property void front(int newValue)
        {
            element = newValue;
        }
    }

    static assert(isInputRange!AssignableRange);
    static assert(is(ElementType!AssignableRange == int));
    static assert(hasAssignableElements!AssignableRange);
    static assert(!hasLvalueElements!AssignableRange);

    auto range = new AssignableRange();
    assert(range.element == 0);

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

/++
Implements the homonym function (also known as $(D accumulate), $(D
compress), $(D inject), or $(D foldl)) present in various programming
languages of functional flavor. There is also $(LREF fold) which does
the same thing but with the opposite parameter order.
The call $(D reduce!(fun)(seed, range)) first assigns $(D seed) to
an internal variable $(D result), also called the accumulator.
Then, for each element $(D x) in $(D range), $(D result = fun(result, x))
gets evaluated. Finally, $(D result) is returned.
The one-argument version $(D reduce!(fun)(range))
works similarly, but it uses the first element of the range as the
seed (the range must be non-empty).

Returns:
    the accumulated $(D result)

Params:
    fun = one or more functions

See_Also:
    $(HTTP en.wikipedia.org/wiki/Fold_(higher-order_function), Fold (higher-order function))

    $(LREF fold) is functionally equivalent to $(LREF reduce) with the argument order reversed,
    and without the need to use $(LREF tuple) for multiple seeds. This makes it easier
    to use in UFCS chains.

    $(LREF sum) is similar to $(D reduce!((a, b) => a + b)) that offers
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
    No-seed version. The first element of $(D r) is used as the seed's value.

    For each function $(D f) in $(D fun), the corresponding
    seed type $(D S) is $(D Unqual!(typeof(f(e, e)))), where $(D e) is an
    element of $(D r): $(D ElementType!R) for ranges,
    and $(D ForeachType!R) otherwise.

    Once S has been determined, then $(D S s = e;) and $(D s = f(s, e);)
    must both be legal.

    If $(D r) is empty, an $(D Exception) is thrown.

    Params:
        r = an iterable value as defined by $(D isIterable)

    Returns:
        the final result of the accumulator applied to the iterable
    +/
    auto reduce(R)(R r)
    if (isIterable!R)
    {
        import std.exception : enforce;
        alias E = Select!(isInputRange!R, ElementType!R, ForeachType!R);
        alias Args = staticMap!(ReduceSeedType!E, binfuns);

        static if (isInputRange!R)
        {
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
    Seed version. The seed should be a single value if $(D fun) is a
    single function. If $(D fun) is multiple functions, then $(D seed)
    should be a $(REF Tuple, std,typecons), with one field per function in $(D f).

    For convenience, if the seed is const, or has qualified fields, then
    $(D reduce) will operate on an unqualified copy. If this happens
    then the returned type will not perfectly match $(D S).

    Use $(D fold) instead of $(D reduce) to use the seed version in a UFCS chain.

    Params:
        seed = the initial value of the accumulator
        r = an iterable value as defined by $(D isIterable)

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
        foreach (/+auto ref+/ E e; r) // @@@4707@@@
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
                import std.conv : emplaceRef;
                foreach (i, f; binfuns)
                    emplaceRef!(Args[i])(args[i], e);
                initialized = true;
                continue;
            }

            foreach (i, f; binfuns)
                args[i] = f(args[i], e);
        }
        static if (mustInitialize)
        if (!initialized)
            throw new Exception("Cannot reduce an empty iterable w/o an explicit seed value.");

        static if (Args.length == 1)
            return args[0];
        else
            return tuple(args);
    }
}

/**
Many aggregate range operations turn out to be solved with $(D reduce)
quickly and easily. The example below illustrates $(D reduce)'s
remarkable power and flexibility.
*/
@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.math : approxEqual;
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
    assert(approxEqual(r1, 112.5));

    // To minimize nesting of parentheses, Uniform Function Call Syntax can be used
    auto r2 = chain(a, b, c).reduce!("a + b");
    assert(approxEqual(r2, 112.5));
}

/**
Sometimes it is very useful to compute multiple aggregates in one pass.
One advantage is that the computation is faster because the looping overhead
is shared. That's why $(D reduce) accepts multiple functions.
If two or more functions are passed, $(D reduce) returns a
$(REF Tuple, std,typecons) object with one member per passed-in function.
The number of seeds must be correspondingly increased.
*/
@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.math : approxEqual, sqrt;
    import std.typecons : tuple, Tuple;

    double[] a = [ 3.0, 4, 7, 11, 3, 2, 5 ];
    // Compute minimum and maximum in one pass
    auto r = reduce!(min, max)(a);
    // The type of r is Tuple!(int, int)
    assert(approxEqual(r[0], 2));  // minimum
    assert(approxEqual(r[1], 11)); // maximum

    // Compute sum and sum of squares in one pass
    r = reduce!("a + b", "a + b * b")(tuple(0.0, 0.0), a);
    assert(approxEqual(r[0], 35));  // sum
    assert(approxEqual(r[1], 233)); // sum of squares
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

@system unittest
{
    import std.algorithm.comparison : max, min;
    import std.exception : assertThrown;
    import std.range : iota;
    import std.typecons : tuple, Tuple;

    // Test the opApply case.
    static struct OpApply
    {
        bool actEmpty;

        int opApply(scope int delegate(ref int) dg)
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
    // Issue #10408 - Two-function reduce of a const array.
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
    //10709
    import std.typecons : tuple, Tuple;

    enum foo = "a + 0.5 * b";
    auto r = [0, 1, 2, 3];
    auto r1 = reduce!foo(r);
    auto r2 = reduce!(foo, foo)(r);
    assert(r1 == 3);
    assert(r2 == tuple(3, 3));
}

@system unittest
{
    static struct OpApply
    {
        int opApply(int delegate(ref int) dg)
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

@safe unittest //12569
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

@safe unittest //13304
{
    int[] data;
    static assert(is(typeof(reduce!((a, b) => a + b)(data))));
    assert(data.length == 0);
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
Implements the homonym function (also known as $(D accumulate), $(D
compress), $(D inject), or $(D foldl)) present in various programming
languages of functional flavor. The call $(D fold!(fun)(range, seed))
first assigns $(D seed) to an internal variable $(D result),
also called the accumulator. Then, for each element $(D x) in $(D
range), $(D result = fun(result, x)) gets evaluated. Finally, $(D
result) is returned. The one-argument version $(D fold!(fun)(range))
works similarly, but it uses the first element of the range as the
seed (the range must be non-empty).

Returns:
    the accumulated $(D result)

See_Also:
    $(HTTP en.wikipedia.org/wiki/Fold_(higher-order_function), Fold (higher-order function))

    $(LREF sum) is similar to $(D fold!((a, b) => a + b)) that offers
    precise summing of floating point numbers.

    This is functionally equivalent to $(LREF reduce) with the argument order reversed,
    and without the need to use $(LREF tuple) for multiple seeds.
+/
template fold(fun...)
if (fun.length >= 1)
{
    auto fold(R, S...)(R r, S seed)
    {
        static if (S.length < 2)
        {
            return reduce!fun(seed, r);
        }
        else
        {
            import std.typecons : tuple;
            return reduce!fun(tuple(seed), r);
        }
    }
}

///
@safe pure unittest
{
    immutable arr = [1, 2, 3, 4, 5];

    // Sum all elements
    assert(arr.fold!((a, b) => a + b) == 15);

    // Sum all elements with explicit seed
    assert(arr.fold!((a, b) => a + b)(6) == 21);

    import std.algorithm.comparison : min, max;
    import std.typecons : tuple;

    // Compute minimum and maximum at the same time
    assert(arr.fold!(min, max) == tuple(1, 5));

    // Compute minimum and maximum at the same time with seeds
    assert(arr.fold!(min, max)(0, 7) == tuple(0, 7));

    // Can be used in a UFCS chain
    assert(arr.map!(a => a + 1).fold!((a, b) => a + b) == 20);

    // Return the last element of any range
    assert(arr.fold!((a, b) => b) == 5);
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
The call $(D cumulativeFold!(fun)(range, seed)) first assigns `seed` to an
internal variable `result`, also called the accumulator.
The returned range contains the values $(D result = fun(result, x)) lazily
evaluated for each element `x` in `range`. Finally, the last element has the
same value as $(D fold!(fun)(seed, range)).
The one-argument version $(D cumulativeFold!(fun)(range)) works similarly, but
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
+/
template cumulativeFold(fun...)
if (fun.length >= 1)
{
    import std.meta : staticMap;
    private alias binfuns = staticMap!(binaryFun, fun);

    /++
    No-seed version. The first element of `r` is used as the seed's value.
    For each function `f` in `fun`, the corresponding seed type `S` is
    $(D Unqual!(typeof(f(e, e)))), where `e` is an element of `r`:
    `ElementType!R`.
    Once `S` has been determined, then $(D S s = e;) and $(D s = f(s, e);) must
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

            static if (hasLength!R)
            {
                @property size_t length()
                {
                    return source.length;
                }
            }
        }

        return Result(range, args);
    }
}

///
@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.array : array;
    import std.math : approxEqual;
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
    assert(approxEqual(r1, [3, 7, 107, 109.5, 112.5]));

    // To minimize nesting of parentheses, Uniform Function Call Syntax can be used
    auto r2 = chain(a, b, c).cumulativeFold!"a + b";
    assert(approxEqual(r2, [3, 7, 107, 109.5, 112.5]));
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
    import std.math : approxEqual;
    import std.typecons : tuple;

    double[] a = [3.0, 4, 7, 11, 3, 2, 5];
    // Compute minimum and maximum in one pass
    auto r = a.cumulativeFold!(min, max);
    // The type of r is Tuple!(int, int)
    assert(approxEqual(r.map!"a[0]", [3, 3, 3, 3, 3, 2, 2]));     // minimum
    assert(approxEqual(r.map!"a[1]", [3, 4, 7, 11, 11, 11, 11])); // maximum

    // Compute sum and sum of squares in one pass
    auto r2 = a.cumulativeFold!("a + b", "a + b * b")(tuple(0.0, 0.0));
    assert(approxEqual(r2.map!"a[0]", [3, 7, 14, 25, 28, 30, 35]));      // sum
    assert(approxEqual(r2.map!"a[1]", [9, 25, 74, 195, 204, 208, 233])); // sum of squares
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
    import std.math : approxEqual;
    import std.typecons : tuple;

    const float a = 0.0;
    const float[] b = [1.2, 3, 3.3];
    float[] c = [1.2, 3, 3.3];

    auto r = cumulativeFold!"a + b"(b, a);
    assert(approxEqual(r, [1.2, 4.2, 7.5]));

    auto r2 = cumulativeFold!"a + b"(c, a);
    assert(approxEqual(r2, [1.2, 4.2, 7.5]));

    const numbers = [10, 30, 20];
    enum m = numbers.cumulativeFold!(min).array;
    assert(m == [10, 10, 10]);
    enum minmax = numbers.cumulativeFold!(min, max).array;
    assert(minmax == [tuple(10, 10), tuple(10, 30), tuple(10, 30)]);
}

@safe unittest
{
    import std.math : approxEqual;
    import std.typecons : tuple;

    enum foo = "a + 0.5 * b";
    auto r = [0, 1, 2, 3];
    auto r1 = r.cumulativeFold!foo;
    auto r2 = r.cumulativeFold!(foo, foo);
    assert(approxEqual(r1, [0, 0.5, 1.5, 3]));
    assert(approxEqual(r2.map!"a[0]", [0, 0.5, 1.5, 3]));
    assert(approxEqual(r2.map!"a[1]", [0, 0.5, 1.5, 3]));
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

@safe unittest //12569
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

@safe unittest //13304
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
Lazily splits a range using an element as a separator. This can be used with
any narrow string type or sliceable range type, but is most popular with string
types.

Two adjacent separators are considered to surround an empty element in
the split range. Use $(D filter!(a => !a.empty)) on the result to compress
empty elements.

The predicate is passed to $(REF binaryFun, std,functional), and can either accept
a string, or any callable that can be executed via $(D pred(element, s)).

If the empty range is given, the result is a range with one empty
element. If a range with one separator is given, the result is a range
with two empty elements.

If splitting a string on whitespace and token compression is desired,
consider using $(D splitter) without specifying a separator (see fourth overload
below).

Params:
    pred = The predicate for comparing each element with the separator,
        defaulting to $(D "a == b").
    r = The $(REF_ALTTEXT input range, isInputRange, std,range,primitives) to be
        split. Must support slicing and $(D .length).
    s = The element to be treated as the separator between range segments to be
        split.

Constraints:
    The predicate $(D pred) needs to accept an element of $(D r) and the
    separator $(D s).

Returns:
    An input range of the subranges of elements between separators. If $(D r)
    is a $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
    or $(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives),
    the returned range will be likewise.

See_Also:
 $(REF _splitter, std,regex) for a version that splits using a regular
expression defined separator.
*/
auto splitter(alias pred = "a == b", Range, Separator)(Range r, Separator s)
if (is(typeof(binaryFun!pred(r.front, s)) : bool)
        && ((hasSlicing!Range && hasLength!Range) || isNarrowString!Range))
{
    import std.algorithm.searching : find;
    import std.conv : unsigned;

    static struct Result
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

        static if (isBidirectionalRange!Range)
        {
            static size_t lastIndexOf(Range haystack, Separator needle)
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
            if (_frontLength == _unComputed)
            {
                auto r = _input.find!pred(_separator);
                _frontLength = _input.length - r.length;
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
            assert(_frontLength <= _input.length);
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
                assert(_backLength <= _input.length);
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

    return Result(r, s);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;

    assert(equal(splitter("hello  world", ' '), [ "hello", "", "world" ]));
    int[] a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    int[][] w = [ [1, 2], [], [3], [4, 5], [] ];
    assert(equal(splitter(a, 0), w));
    a = [ 0 ];
    assert(equal(splitter(a, 0), [ (int[]).init, (int[]).init ]));
    a = [ 0, 1 ];
    assert(equal(splitter(a, 0), [ [], [1] ]));
    w = [ [0], [1], [2] ];
    assert(equal(splitter!"a.front == b"(w, 1), [ [[0]], [[2]] ]));
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

    foreach (DummyType; AllDummyRanges) {  // Bug 4408
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

/**
Similar to the previous overload of $(D splitter), except this one uses another
range as a separator. This can be used with any narrow string type or sliceable
range type, but is most popular with string types. The predicate is passed to
$(REF binaryFun, std,functional), and can either accept a string, or any callable
that can be executed via $(D pred(r.front, s.front)).

Two adjacent separators are considered to surround an empty element in
the split range. Use $(D filter!(a => !a.empty)) on the result to compress
empty elements.

Params:
    pred = The predicate for comparing each element with the separator,
        defaulting to $(D "a == b").
    r = The $(REF_ALTTEXT input range, isInputRange, std,range,primitives) to be
        split.
    s = The $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives) to
        be treated as the separator between segments of $(D r) to be split.

Constraints:
    The predicate $(D pred) needs to accept an element of $(D r) and an
    element of $(D s).

Returns:
    An input range of the subranges of elements between separators. If $(D r)
    is a forward range or $(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives),
    the returned range will be likewise.

See_Also: $(REF _splitter, std,regex) for a version that splits using a regular
expression defined separator.
 */
auto splitter(alias pred = "a == b", Range, Separator)(Range r, Separator s)
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
        static if (isBidirectionalRange!Range)
            size_t _backLength = size_t.max;

        @property auto separatorLength() { return _separator.length; }

        void ensureFrontLength()
        {
            if (_frontLength != _frontLength.max) return;
            assert(!_input.empty);
            // compute front length
            _frontLength = (_separator.empty) ? 1 :
                           _input.length - find!pred(_input, _separator).length;
            static if (isBidirectionalRange!Range)
                if (_frontLength == _input.length) _backLength = _frontLength;
        }

        void ensureBackLength()
        {
            static if (isBidirectionalRange!Range)
                if (_backLength != _backLength.max) return;
            assert(!_input.empty);
            // compute back length
            static if (isBidirectionalRange!Range && isBidirectionalRange!Separator)
            {
                import std.range : retro;
                _backLength = _input.length -
                    find!pred(retro(_input), retro(_separator)).source.length;
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
                return _frontLength == size_t.max && _input.empty;
            }
        }

        void popFront()
        {
            assert(!empty, "Attempting to popFront an empty splitter.");
            ensureFrontLength();
            if (_frontLength == _input.length)
            {
                // done, there's no separator in sight
                _input = _input[_frontLength .. _frontLength];
                _frontLength = _frontLength.max;
                static if (isBidirectionalRange!Range)
                    _backLength = _backLength.max;
                return;
            }
            if (_frontLength + separatorLength == _input.length)
            {
                // Special case: popping the first-to-last item; there is
                // an empty item right after this.
                _input = _input[_input.length .. _input.length];
                _frontLength = 0;
                static if (isBidirectionalRange!Range)
                    _backLength = 0;
                return;
            }
            // Normal case, pop one item and the separator, get ready for
            // reading the next item
            _input = _input[_frontLength + separatorLength .. _input.length];
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

///
@safe unittest
{
    import std.algorithm.comparison : equal;

    assert(equal(splitter("hello  world", "  "), [ "hello", "world" ]));
    int[] a = [ 1, 2, 0, 0, 3, 0, 4, 5, 0 ];
    int[][] w = [ [1, 2], [3, 0, 4, 5, 0] ];
    assert(equal(splitter(a, [0, 0]), w));
    a = [ 0, 0 ];
    assert(equal(splitter(a, [0, 0]), [ (int[]).init, (int[]).init ]));
    a = [ 0, 0, 1 ];
    assert(equal(splitter(a, [0, 0]), [ [], [1] ]));
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
    // // Now go back
    // auto s2 = splitter(a, 0);

    // foreach (e; retro(s2))
    // {
    //     assert(i > 0);
    //     assert(equal(e, w[--i]), text(e));
    // }
    // assert(i == 0);

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

@safe unittest
{
    import std.algorithm.comparison : equal;

    // Issue 10773
    auto s = splitter("abc", "");
    assert(s.equal(["a", "b", "c"]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    // Test by-reference separator
    class RefSep {
    @safe:
        string _impl;
        this(string s) { _impl = s; }
        @property empty() { return _impl.empty; }
        @property auto front() { return _impl.front; }
        void popFront() { _impl = _impl[1..$]; }
        @property RefSep save() { return new RefSep(_impl); }
        @property auto length() { return _impl.length; }
    }
    auto sep = new RefSep("->");
    auto data = "i->am->pointing";
    auto words = splitter(data, sep);
    assert(words.equal([ "i", "am", "pointing" ]));
}

/**

Similar to the previous overload of $(D splitter), except this one does not use a separator.
Instead, the predicate is an unary function on the input range's element type.
The $(D isTerminator) predicate is passed to $(REF unaryFun, std,functional) and can
either accept a string, or any callable that can be executed via $(D pred(element, s)).

Two adjacent separators are considered to surround an empty element in
the split range. Use $(D filter!(a => !a.empty)) on the result to compress
empty elements.

Params:
    isTerminator = The predicate for deciding where to split the range.
    input = The $(REF_ALTTEXT input range, isInputRange, std,range,primitives) to
        be split.

Constraints:
    The predicate $(D isTerminator) needs to accept an element of $(D input).

Returns:
    An input range of the subranges of elements between separators. If $(D input)
    is a forward range or $(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives),
    the returned range will be likewise.

See_Also: $(REF _splitter, std,regex) for a version that splits using a regular
expression defined separator.
 */
auto splitter(alias isTerminator, Range)(Range input)
if (isForwardRange!Range && is(typeof(unaryFun!isTerminator(input.front))))
{
    return SplitterResult!(unaryFun!isTerminator, Range)(input);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range.primitives : front;

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
        auto ret = this;
        ret._input = _input.save;
        static if (!fullSlicing)
            ret._next = _next.save;
        return ret;
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

    //@@@6791@@@
    assert(equal(
        splitter("là dove terminava quella valle"),
        ["là", "dove", "terminava", "quella", "valle"]
    ));
    assert(equal(
        splitter!(std.uni.isWhite)("là dove terminava quella valle"),
        ["là", "dove", "terminava", "quella", "valle"]
    ));
    assert(equal(splitter!"a=='本'"("日本語"), ["日", "語"]));
}

/++
Lazily splits the string $(D s) into words, using whitespace as the delimiter.

This function is string specific and, contrary to
$(D splitter!(std.uni.isWhite)), runs of whitespace will be merged together
(no empty tokens will be produced).

Params:
    s = The string to be split.

Returns:
    An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) of slices of
    the original string split by whitespace.
 +/
auto splitter(C)(C[] s)
if (isSomeChar!C)
{
    import std.algorithm.searching : find;
    static struct Result
    {
    private:
        import core.exception : RangeError;
        C[] _s;
        size_t _frontLength;

        void getFirst() pure @safe
        {
            import std.uni : isWhite;

            auto r = find!(isWhite)(_s);
            _frontLength = _s.length - r.length;
        }

    public:
        this(C[] s) pure @safe
        {
            import std.string : strip;
            _s = s.strip();
            getFirst();
        }

        @property C[] front() pure @safe
        {
            version (assert) if (empty) throw new RangeError();
            return _s[0 .. _frontLength];
        }

        void popFront() pure @safe
        {
            import std.string : stripLeft;
            version (assert) if (empty) throw new RangeError();
            _s = _s[_frontLength .. $].stripLeft();
            getFirst();
        }

        @property bool empty() const @safe pure nothrow
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
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        import std.conv : to;
        S a = " a     bcd   ef gh ";
        assert(equal(splitter(a), [to!S("a"), to!S("bcd"), to!S("ef"), to!S("gh")]));
        a = "";
        assert(splitter(a).empty);
    }

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

// sum
/**
Sums elements of $(D r), which must be a finite
$(REF_ALTTEXT input range, isInputRange, std,range,primitives). Although
conceptually $(D sum(r)) is equivalent to $(LREF fold)!((a, b) => a +
b)(r, 0), $(D sum) uses specialized algorithms to maximize accuracy,
as follows.

$(UL
$(LI If $(D $(REF ElementType, std,range,primitives)!R) is a floating-point
type and $(D R) is a
$(REF_ALTTEXT random-access range, isRandomAccessRange, std,range,primitives) with
length and slicing, then $(D sum) uses the
$(HTTP en.wikipedia.org/wiki/Pairwise_summation, pairwise summation)
algorithm.)
$(LI If $(D ElementType!R) is a floating-point type and $(D R) is a
finite input range (but not a random-access range with slicing), then
$(D sum) uses the $(HTTP en.wikipedia.org/wiki/Kahan_summation,
Kahan summation) algorithm.)
$(LI In all other cases, a simple element by element addition is done.)
)

For floating point inputs, calculations are made in
$(DDLINK spec/type, Types, $(D real))
precision for $(D real) inputs and in $(D double) precision otherwise
(Note this is a special case that deviates from $(D fold)'s behavior,
which would have kept $(D float) precision for a $(D float) range).
For all other types, the calculations are done in the same type obtained
from from adding two elements of the range, which may be a different
type from the elements themselves (for example, in case of
$(DDSUBLINK spec/type,integer-promotions, integral promotion)).

A seed may be passed to $(D sum). Not only will this seed be used as an initial
value, but its type will override all the above, and determine the algorithm
and precision used for summation.

Note that these specialized summing algorithms execute more primitive operations
than vanilla summation. Therefore, if in certain cases maximum speed is required
at expense of precision, one can use $(D fold!((a, b) => a + b)(r, 0)), which
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
    return sum(r, Unqual!Seed(0));
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
    import std.math : isPowerOf2;
    static assert(isPowerOf2(N));
    static if (N == 2) return sumPair!(needEmptyChecks, F)(r);
    else return sumPairwiseN!(N/2, needEmptyChecks, F)(r)
        + sumPairwiseN!(N/2, needEmptyChecks, F)(r);
}

// Kahan algo http://en.wikipedia.org/wiki/Kahan_summation_algorithm
private auto sumKahan(Result, R)(Result result, R r)
{
    static assert(isFloatingPoint!Result && isMutable!Result);
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
    import std.math : approxEqual;
    assert(iota(ulong.max / 2, ulong.max / 2 + 4096).sum(0.0)
               .approxEqual((ulong.max / 2) * 4096.0 + 4096^^2 / 2));
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

@safe pure nothrow unittest // 12434
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

// uniq
/**
Lazily iterates unique consecutive elements of the given range (functionality
akin to the $(HTTP wikipedia.org/wiki/_Uniq, _uniq) system
utility). Equivalence of elements is assessed by using the predicate
$(D pred), by default $(D "a == b"). The predicate is passed to
$(REF binaryFun, std,functional), and can either accept a string, or any callable
that can be executed via $(D pred(element, element)). If the given range is
bidirectional, $(D uniq) also yields a
$(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives).

Params:
    pred = Predicate for determining equivalence between range elements.
    r = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) of
        elements to filter.

Returns:
    An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) of
    consecutively unique elements in the original range. If $(D r) is also a
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

@safe unittest // https://issues.dlang.org/show_bug.cgi?id=17264
{
    import std.algorithm.comparison : equal;

    const(int)[] var = [0, 1, 1, 2];
    assert(var.uniq.equal([0, 1, 2]));
}

/**
Lazily computes all _permutations of $(D r) using $(HTTP
en.wikipedia.org/wiki/Heap%27s_algorithm, Heap's algorithm).

Returns:
A $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
the elements of which are an $(REF indexed, std,range) view into $(D r).

See_Also:
$(REF nextPermutation, std,algorithm,sorting).
*/
Permutations!Range permutations(Range)(Range r)
if (isRandomAccessRange!Range && hasLength!Range)
{
    return typeof(return)(r);
}

/// ditto
struct Permutations(Range)
if (isRandomAccessRange!Range && hasLength!Range)
{
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

    ///
    @property bool empty() const pure nothrow @safe @nogc
    {
        return _empty;
    }

    ///
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
