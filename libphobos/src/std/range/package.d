// Written in the D programming language.

/**
This module defines the notion of a range. Ranges generalize the concept of
arrays, lists, or anything that involves sequential access. This abstraction
enables the same set of algorithms (see $(MREF std, algorithm)) to be used
with a vast variety of different concrete types. For example,
a linear search algorithm such as $(REF find, std, algorithm, searching)
works not just for arrays, but for linked-lists, input files,
incoming network data, etc.

Guides:

There are many articles available that can bolster understanding ranges:

$(UL
    $(LI Ali Çehreli's $(HTTP ddili.org/ders/d.en/ranges.html, tutorial on _ranges)
        for the basics of working with and creating range-based code.)
    $(LI Jonathan M. Davis $(LINK2 http://dconf.org/2015/talks/davis.html, $(I Introduction to Ranges))
        talk at DConf 2015 a vivid introduction from its core constructs to practical advice.)
    $(LI The DLang Tour's $(LINK2 http://tour.dlang.org/tour/en/basics/ranges, chapter on ranges)
        for an interactive introduction.)
    $(LI H. S. Teoh's $(LINK2 http://wiki.dlang.org/Component_programming_with_ranges, tutorial on
        component programming with ranges) for a real-world showcase of the influence
        of _range-based programming on complex algorithms.)
    $(LI Andrei Alexandrescu's article
        $(LINK2 http://www.informit.com/articles/printerfriendly.aspx?p=1407357$(AMP)rll=1,
        $(I On Iteration)) for conceptual aspect of ranges and the motivation
    )
)

Submodules:

This module has two submodules:

The $(MREF std, _range, primitives) submodule
provides basic _range functionality. It defines several templates for testing
whether a given object is a _range, what kind of _range it is, and provides
some common _range operations.

The $(MREF std, _range, interfaces) submodule
provides object-based interfaces for working with ranges via runtime
polymorphism.

The remainder of this module provides a rich set of _range creation and
composition templates that let you construct new ranges out of existing ranges:


$(SCRIPT inhibitQuickIndex = 1;)
$(BOOKTABLE ,
    $(TR $(TD $(LREF chain))
        $(TD Concatenates several ranges into a single _range.
    ))
    $(TR $(TD $(LREF choose))
        $(TD Chooses one of two ranges at runtime based on a boolean condition.
    ))
    $(TR $(TD $(LREF chooseAmong))
        $(TD Chooses one of several ranges at runtime based on an index.
    ))
    $(TR $(TD $(LREF chunks))
        $(TD Creates a _range that returns fixed-size chunks of the original
        _range.
    ))
    $(TR $(TD $(LREF cycle))
        $(TD Creates an infinite _range that repeats the given forward _range
        indefinitely. Good for implementing circular buffers.
    ))
    $(TR $(TD $(LREF drop))
        $(TD Creates the _range that results from discarding the first $(I n)
        elements from the given _range.
    ))
    $(TR $(TD $(LREF dropBack))
        $(TD Creates the _range that results from discarding the last $(I n)
        elements from the given _range.
    ))
    $(TR $(TD $(LREF dropExactly))
        $(TD Creates the _range that results from discarding exactly $(I n)
        of the first elements from the given _range.
    ))
    $(TR $(TD $(LREF dropBackExactly))
        $(TD Creates the _range that results from discarding exactly $(I n)
        of the last elements from the given _range.
    ))
    $(TR $(TD $(LREF dropOne))
        $(TD Creates the _range that results from discarding
        the first element from the given _range.
    ))
    $(TR $(TD $(D $(LREF dropBackOne)))
        $(TD Creates the _range that results from discarding
        the last element from the given _range.
    ))
    $(TR $(TD $(LREF enumerate))
        $(TD Iterates a _range with an attached index variable.
    ))
    $(TR $(TD $(LREF evenChunks))
        $(TD Creates a _range that returns a number of chunks of
        approximately equal length from the original _range.
    ))
    $(TR $(TD $(LREF frontTransversal))
        $(TD Creates a _range that iterates over the first elements of the
        given ranges.
    ))
    $(TR $(TD $(LREF generate))
        $(TD Creates a _range by successive calls to a given function. This
        allows to create ranges as a single delegate.
    ))
    $(TR $(TD $(LREF indexed))
        $(TD Creates a _range that offers a view of a given _range as though
        its elements were reordered according to a given _range of indices.
    ))
    $(TR $(TD $(LREF iota))
        $(TD Creates a _range consisting of numbers between a starting point
        and ending point, spaced apart by a given interval.
    ))
    $(TR $(TD $(LREF lockstep))
        $(TD Iterates $(I n) _ranges in lockstep, for use in a $(D foreach)
        loop. Similar to $(D zip), except that $(D lockstep) is designed
        especially for $(D foreach) loops.
    ))
    $(TR $(TD $(LREF NullSink))
        $(TD An output _range that discards the data it receives.
    ))
    $(TR $(TD $(LREF only))
        $(TD Creates a _range that iterates over the given arguments.
    ))
    $(TR $(TD $(LREF padLeft))
        $(TD Pads a _range to a specified length by adding a given element to
        the front of the _range. Is lazy if the _range has a known length.
    ))
    $(TR $(TD $(LREF padRight))
        $(TD Lazily pads a _range to a specified length by adding a given element to
        the back of the _range.
    ))
    $(TR $(TD $(LREF radial))
        $(TD Given a random-access _range and a starting point, creates a
        _range that alternately returns the next left and next right element to
        the starting point.
    ))
    $(TR $(TD $(LREF recurrence))
        $(TD Creates a forward _range whose values are defined by a
        mathematical recurrence relation.
    ))
    $(TR $(TD $(LREF refRange))
        $(TD Pass a _range by reference. Both the original _range and the RefRange
        will always have the exact same elements.
        Any operation done on one will affect the other.
    ))
    $(TR $(TD $(LREF repeat))
        $(TD Creates a _range that consists of a single element repeated $(I n)
        times, or an infinite _range repeating that element indefinitely.
    ))
    $(TR $(TD $(LREF retro))
        $(TD Iterates a bidirectional _range backwards.
    ))
    $(TR $(TD $(LREF roundRobin))
        $(TD Given $(I n) ranges, creates a new _range that return the $(I n)
        first elements of each _range, in turn, then the second element of each
        _range, and so on, in a round-robin fashion.
    ))
    $(TR $(TD $(LREF sequence))
        $(TD Similar to $(D recurrence), except that a random-access _range is
        created.
    ))
    $(COMMENT Explicitly undocumented to delay the release until 2.076
    $(TR $(TD $(D $(LREF slide)))
        $(TD Creates a _range that returns a fixed-size sliding window
        over the original _range. Unlike chunks,
        it advances a configurable number of items at a time,
        not one chunk at a time.
    ))
    )
    $(TR $(TD $(LREF stride))
        $(TD Iterates a _range with stride $(I n).
    ))
    $(TR $(TD $(LREF tail))
        $(TD Return a _range advanced to within $(D n) elements of the end of
        the given _range.
    ))
    $(TR $(TD $(LREF take))
        $(TD Creates a sub-_range consisting of only up to the first $(I n)
        elements of the given _range.
    ))
    $(TR $(TD $(LREF takeExactly))
        $(TD Like $(D take), but assumes the given _range actually has $(I n)
        elements, and therefore also defines the $(D length) property.
    ))
    $(TR $(TD $(LREF takeNone))
        $(TD Creates a random-access _range consisting of zero elements of the
        given _range.
    ))
    $(TR $(TD $(LREF takeOne))
        $(TD Creates a random-access _range consisting of exactly the first
        element of the given _range.
    ))
    $(TR $(TD $(LREF tee))
        $(TD Creates a _range that wraps a given _range, forwarding along
        its elements while also calling a provided function with each element.
    ))
    $(TR $(TD $(LREF transposed))
        $(TD Transposes a _range of ranges.
    ))
    $(TR $(TD $(LREF transversal))
        $(TD Creates a _range that iterates over the $(I n)'th elements of the
        given random-access ranges.
    ))
    $(TR $(TD $(LREF zip))
        $(TD Given $(I n) _ranges, creates a _range that successively returns a
        tuple of all the first elements, a tuple of all the second elements,
        etc.
    ))
)

Sortedness:

Ranges whose elements are sorted afford better efficiency with certain
operations. For this, the $(LREF assumeSorted) function can be used to
construct a $(LREF SortedRange) from a pre-sorted _range. The $(REF
sort, std, algorithm, sorting) function also conveniently
returns a $(LREF SortedRange). $(LREF SortedRange) objects provide some additional
_range operations that take advantage of the fact that the _range is sorted.

Source: $(PHOBOSSRC std/_range/_package.d)

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP erdani.com, Andrei Alexandrescu), David Simcha, Jonathan M Davis,
and Jack Stouffer. Credit for some of the ideas in building this module goes
to $(HTTP fantascienza.net/leonardo/so/, Leonardo Maffi).
 */
module std.range;

public import std.array;
public import std.range.interfaces;
public import std.range.primitives;
public import std.typecons : Flag, Yes, No;

import std.meta; // allSatisfy, staticMap
import std.traits; // CommonType, isCallable, isFloatingPoint, isIntegral,
    // isPointer, isSomeFunction, isStaticArray, Unqual


/**
Iterates a bidirectional range backwards. The original range can be
accessed by using the $(D source) property. Applying retro twice to
the same range yields the original range.

Params:
    r = the bidirectional range to iterate backwards

Returns:
    A bidirectional range with length if `r` also provides a length. Or,
    if `r` is a random access range, then the return value will be random
    access as well.
See_Also:
    $(REF reverse, std,algorithm,mutation) for mutating the source range directly.
 */
auto retro(Range)(Range r)
if (isBidirectionalRange!(Unqual!Range))
{
    // Check for retro(retro(r)) and just return r in that case
    static if (is(typeof(retro(r.source)) == Range))
    {
        return r.source;
    }
    else
    {
        static struct Result()
        {
            private alias R = Unqual!Range;

            // User code can get and set source, too
            R source;

            static if (hasLength!R)
            {
                size_t retroIndex(size_t n)
                {
                    return source.length - n - 1;
                }
            }

        public:
            alias Source = R;

            @property bool empty() { return source.empty; }
            @property auto save()
            {
                return Result(source.save);
            }
            @property auto ref front() { return source.back; }
            void popFront() { source.popBack(); }
            @property auto ref back() { return source.front; }
            void popBack() { source.popFront(); }

            static if (is(typeof(source.moveBack())))
            {
                ElementType!R moveFront()
                {
                    return source.moveBack();
                }
            }

            static if (is(typeof(source.moveFront())))
            {
                ElementType!R moveBack()
                {
                    return source.moveFront();
                }
            }

            static if (hasAssignableElements!R)
            {
                @property void front(ElementType!R val)
                {
                    source.back = val;
                }

                @property void back(ElementType!R val)
                {
                    source.front = val;
                }
            }

            static if (isRandomAccessRange!(R) && hasLength!(R))
            {
                auto ref opIndex(size_t n) { return source[retroIndex(n)]; }

                static if (hasAssignableElements!R)
                {
                    void opIndexAssign(ElementType!R val, size_t n)
                    {
                        source[retroIndex(n)] = val;
                    }
                }

                static if (is(typeof(source.moveAt(0))))
                {
                    ElementType!R moveAt(size_t index)
                    {
                        return source.moveAt(retroIndex(index));
                    }
                }

                static if (hasSlicing!R)
                    typeof(this) opSlice(size_t a, size_t b)
                    {
                        return typeof(this)(source[source.length - b .. source.length - a]);
                    }
            }

            static if (hasLength!R)
            {
                @property auto length()
                {
                    return source.length;
                }

                alias opDollar = length;
            }
        }

        return Result!()(r);
    }
}


///
pure @safe nothrow @nogc unittest
{
    import std.algorithm.comparison : equal;
    int[5] a = [ 1, 2, 3, 4, 5 ];
    int[5] b = [ 5, 4, 3, 2, 1 ];
    assert(equal(retro(a[]), b[]));
    assert(retro(a[]).source is a[]);
    assert(retro(retro(a[])) is a[]);
}

pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;
    static assert(isBidirectionalRange!(typeof(retro("hello"))));
    int[] a;
    static assert(is(typeof(a) == typeof(retro(retro(a)))));
    assert(retro(retro(a)) is a);
    static assert(isRandomAccessRange!(typeof(retro([1, 2, 3]))));
    void test(int[] input, int[] witness)
    {
        auto r = retro(input);
        assert(r.front == witness.front);
        assert(r.back == witness.back);
        assert(equal(r, witness));
    }
    test([ 1 ], [ 1 ]);
    test([ 1, 2 ], [ 2, 1 ]);
    test([ 1, 2, 3 ], [ 3, 2, 1 ]);
    test([ 1, 2, 3, 4 ], [ 4, 3, 2, 1 ]);
    test([ 1, 2, 3, 4, 5 ], [ 5, 4, 3, 2, 1 ]);
    test([ 1, 2, 3, 4, 5, 6 ], [ 6, 5, 4, 3, 2, 1 ]);

    immutable foo = [1,2,3].idup;
    auto r = retro(foo);
    assert(equal(r, [3, 2, 1]));
}

pure @safe nothrow unittest
{
    import std.internal.test.dummyrange : AllDummyRanges, propagatesRangeType,
        ReturnBy;

    foreach (DummyType; AllDummyRanges)
    {
        static if (!isBidirectionalRange!DummyType)
        {
            static assert(!__traits(compiles, Retro!DummyType));
        }
        else
        {
            DummyType dummyRange;
            dummyRange.reinit();

            auto myRetro = retro(dummyRange);
            static assert(propagatesRangeType!(typeof(myRetro), DummyType));
            assert(myRetro.front == 10);
            assert(myRetro.back == 1);
            assert(myRetro.moveFront() == 10);
            assert(myRetro.moveBack() == 1);

            static if (isRandomAccessRange!DummyType && hasLength!DummyType)
            {
                assert(myRetro[0] == myRetro.front);
                assert(myRetro.moveAt(2) == 8);

                static if (DummyType.r == ReturnBy.Reference)
                {
                    {
                        myRetro[9]++;
                        scope(exit) myRetro[9]--;
                        assert(dummyRange[0] == 2);
                        myRetro.front++;
                        scope(exit) myRetro.front--;
                        assert(myRetro.front == 11);
                        myRetro.back++;
                        scope(exit) myRetro.back--;
                        assert(myRetro.back == 3);
                    }

                    {
                        myRetro.front = 0xFF;
                        scope(exit) myRetro.front = 10;
                        assert(dummyRange.back == 0xFF);

                        myRetro.back = 0xBB;
                        scope(exit) myRetro.back = 1;
                        assert(dummyRange.front == 0xBB);

                        myRetro[1] = 11;
                        scope(exit) myRetro[1] = 8;
                        assert(dummyRange[8] == 11);
                    }
                }
            }
        }
    }
}

pure @safe nothrow @nogc unittest
{
    import std.algorithm.comparison : equal;
    auto LL = iota(1L, 4L);
    auto r = retro(LL);
    long[3] excepted = [3, 2, 1];
    assert(equal(r, excepted[]));
}

// Issue 12662
pure @safe nothrow @nogc unittest
{
    int[3] src = [1,2,3];
    int[] data = src[];
    foreach_reverse (x; data) {}
    foreach (x; data.retro) {}
}


/**
Iterates range $(D r) with stride $(D n). If the range is a
random-access range, moves by indexing into the range; otherwise,
moves by successive calls to $(D popFront). Applying stride twice to
the same range results in a stride with a step that is the
product of the two applications. It is an error for $(D n) to be 0.

Params:
    r = the input range to stride over
    n = the number of elements to skip over

Returns:
    At minimum, an input range. The resulting range will adopt the
    range primitives of the underlying range as long as
    $(REF hasLength, std,range,primitives) is `true`.
 */
auto stride(Range)(Range r, size_t n)
if (isInputRange!(Unqual!Range))
in
{
    assert(n != 0, "stride cannot have step zero.");
}
body
{
    import std.algorithm.comparison : min;

    static if (is(typeof(stride(r.source, n)) == Range))
    {
        // stride(stride(r, n1), n2) is stride(r, n1 * n2)
        return stride(r.source, r._n * n);
    }
    else
    {
        static struct Result
        {
            private alias R = Unqual!Range;
            public R source;
            private size_t _n;

            // Chop off the slack elements at the end
            static if (hasLength!R &&
                    (isRandomAccessRange!R && hasSlicing!R
                            || isBidirectionalRange!R))
                private void eliminateSlackElements()
                {
                    auto slack = source.length % _n;

                    if (slack)
                    {
                        slack--;
                    }
                    else if (!source.empty)
                    {
                        slack = min(_n, source.length) - 1;
                    }
                    else
                    {
                        slack = 0;
                    }
                    if (!slack) return;
                    static if (isRandomAccessRange!R && hasLength!R && hasSlicing!R)
                    {
                        source = source[0 .. source.length - slack];
                    }
                    else static if (isBidirectionalRange!R)
                    {
                        foreach (i; 0 .. slack)
                        {
                            source.popBack();
                        }
                    }
                }

            static if (isForwardRange!R)
            {
                @property auto save()
                {
                    return Result(source.save, _n);
                }
            }

            static if (isInfinite!R)
            {
                enum bool empty = false;
            }
            else
            {
                @property bool empty()
                {
                    return source.empty;
                }
            }

            @property auto ref front()
            {
                return source.front;
            }

            static if (is(typeof(.moveFront(source))))
            {
                ElementType!R moveFront()
                {
                    return source.moveFront();
                }
            }

            static if (hasAssignableElements!R)
            {
                @property void front(ElementType!R val)
                {
                    source.front = val;
                }
            }

            void popFront()
            {
                source.popFrontN(_n);
            }

            static if (isBidirectionalRange!R && hasLength!R)
            {
                void popBack()
                {
                    popBackN(source, _n);
                }

                @property auto ref back()
                {
                    eliminateSlackElements();
                    return source.back;
                }

                static if (is(typeof(.moveBack(source))))
                {
                    ElementType!R moveBack()
                    {
                        eliminateSlackElements();
                        return source.moveBack();
                    }
                }

                static if (hasAssignableElements!R)
                {
                    @property void back(ElementType!R val)
                    {
                        eliminateSlackElements();
                        source.back = val;
                    }
                }
            }

            static if (isRandomAccessRange!R && hasLength!R)
            {
                auto ref opIndex(size_t n)
                {
                    return source[_n * n];
                }

                /**
                   Forwards to $(D moveAt(source, n)).
                */
                static if (is(typeof(source.moveAt(0))))
                {
                    ElementType!R moveAt(size_t n)
                    {
                        return source.moveAt(_n * n);
                    }
                }

                static if (hasAssignableElements!R)
                {
                    void opIndexAssign(ElementType!R val, size_t n)
                    {
                        source[_n * n] = val;
                    }
                }
            }

            static if (hasSlicing!R && hasLength!R)
                typeof(this) opSlice(size_t lower, size_t upper)
                {
                    assert(upper >= lower && upper <= length);
                    immutable translatedUpper = (upper == 0) ? 0 :
                        (upper * _n - (_n - 1));
                    immutable translatedLower = min(lower * _n, translatedUpper);

                    assert(translatedLower <= translatedUpper);

                    return typeof(this)(source[translatedLower .. translatedUpper], _n);
                }

            static if (hasLength!R)
            {
                @property auto length()
                {
                    return (source.length + _n - 1) / _n;
                }

                alias opDollar = length;
            }
        }
        return Result(r, n);
    }
}

///
pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;

    int[] a = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 ];
    assert(equal(stride(a, 3), [ 1, 4, 7, 10 ][]));
    assert(stride(stride(a, 2), 3) == stride(a, 6));
}

pure @safe nothrow @nogc unittest
{
    import std.algorithm.comparison : equal;

    int[4] testArr = [1,2,3,4];
    static immutable result = [1, 3];
    assert(equal(testArr[].stride(2), result));
}

debug pure nothrow @system unittest
{//check the contract
    int[4] testArr = [1,2,3,4];
    bool passed = false;
    scope (success) assert(passed);
    import core.exception : AssertError;
    //std.exception.assertThrown won't do because it can't infer nothrow
    // @@@BUG@@@ 12647
    try
    {
        auto unused = testArr[].stride(0);
    }
    catch (AssertError unused)
    {
        passed = true;
    }
}

pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : AllDummyRanges, propagatesRangeType,
        ReturnBy;

    static assert(isRandomAccessRange!(typeof(stride([1, 2, 3], 2))));
    void test(size_t n, int[] input, int[] witness)
    {
        assert(equal(stride(input, n), witness));
    }
    test(1, [], []);
    int[] arr = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
    assert(stride(stride(arr, 2), 3) is stride(arr, 6));
    test(1, arr, arr);
    test(2, arr, [1, 3, 5, 7, 9]);
    test(3, arr, [1, 4, 7, 10]);
    test(4, arr, [1, 5, 9]);

    // Test slicing.
    auto s1 = stride(arr, 1);
    assert(equal(s1[1 .. 4], [2, 3, 4]));
    assert(s1[1 .. 4].length == 3);
    assert(equal(s1[1 .. 5], [2, 3, 4, 5]));
    assert(s1[1 .. 5].length == 4);
    assert(s1[0 .. 0].empty);
    assert(s1[3 .. 3].empty);
    // assert(s1[$ .. $].empty);
    assert(s1[s1.opDollar .. s1.opDollar].empty);

    auto s2 = stride(arr, 2);
    assert(equal(s2[0 .. 2], [1,3]));
    assert(s2[0 .. 2].length == 2);
    assert(equal(s2[1 .. 5], [3, 5, 7, 9]));
    assert(s2[1 .. 5].length == 4);
    assert(s2[0 .. 0].empty);
    assert(s2[3 .. 3].empty);
    // assert(s2[$ .. $].empty);
    assert(s2[s2.opDollar .. s2.opDollar].empty);

    // Test fix for Bug 5035
    auto m = [1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4]; // 3 rows, 4 columns
    auto col = stride(m, 4);
    assert(equal(col, [1, 1, 1]));
    assert(equal(retro(col), [1, 1, 1]));

    immutable int[] immi = [ 1, 2, 3 ];
    static assert(isRandomAccessRange!(typeof(stride(immi, 1))));

    // Check for infiniteness propagation.
    static assert(isInfinite!(typeof(stride(repeat(1), 3))));

    foreach (DummyType; AllDummyRanges)
    {
        DummyType dummyRange;
        dummyRange.reinit();

        auto myStride = stride(dummyRange, 4);

        // Should fail if no length and bidirectional b/c there's no way
        // to know how much slack we have.
        static if (hasLength!DummyType || !isBidirectionalRange!DummyType)
        {
            static assert(propagatesRangeType!(typeof(myStride), DummyType));
        }
        assert(myStride.front == 1);
        assert(myStride.moveFront() == 1);
        assert(equal(myStride, [1, 5, 9]));

        static if (hasLength!DummyType)
        {
            assert(myStride.length == 3);
        }

        static if (isBidirectionalRange!DummyType && hasLength!DummyType)
        {
            assert(myStride.back == 9);
            assert(myStride.moveBack() == 9);
        }

        static if (isRandomAccessRange!DummyType && hasLength!DummyType)
        {
            assert(myStride[0] == 1);
            assert(myStride[1] == 5);
            assert(myStride.moveAt(1) == 5);
            assert(myStride[2] == 9);

            static assert(hasSlicing!(typeof(myStride)));
        }

        static if (DummyType.r == ReturnBy.Reference)
        {
            // Make sure reference is propagated.

            {
                myStride.front++;
                scope(exit) myStride.front--;
                assert(dummyRange.front == 2);
            }
            {
                myStride.front = 4;
                scope(exit) myStride.front = 1;
                assert(dummyRange.front == 4);
            }

            static if (isBidirectionalRange!DummyType && hasLength!DummyType)
            {
                {
                    myStride.back++;
                    scope(exit) myStride.back--;
                    assert(myStride.back == 10);
                }
                {
                    myStride.back = 111;
                    scope(exit) myStride.back = 9;
                    assert(myStride.back == 111);
                }

                static if (isRandomAccessRange!DummyType)
                {
                    {
                        myStride[1]++;
                        scope(exit) myStride[1]--;
                        assert(dummyRange[4] == 6);
                    }
                    {
                        myStride[1] = 55;
                        scope(exit) myStride[1] = 5;
                        assert(dummyRange[4] == 55);
                    }
                }
            }
        }
    }
}

pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;

    auto LL = iota(1L, 10L);
    auto s = stride(LL, 3);
    assert(equal(s, [1L, 4L, 7L]));
}

/**
Spans multiple ranges in sequence. The function $(D chain) takes any
number of ranges and returns a $(D Chain!(R1, R2,...)) object. The
ranges may be different, but they must have the same element type. The
result is a range that offers the $(D front), $(D popFront), and $(D
empty) primitives. If all input ranges offer random access and $(D
length), $(D Chain) offers them as well.

If only one range is offered to $(D Chain) or $(D chain), the $(D
Chain) type exits the picture by aliasing itself directly to that
range's type.

Params:
    rs = the input ranges to chain together

Returns:
    An input range at minimum. If all of the ranges in `rs` provide
    a range primitive, the returned range will also provide that range
    primitive.

See_Also: $(LREF only) to chain values to a range
 */
auto chain(Ranges...)(Ranges rs)
if (Ranges.length > 0 &&
    allSatisfy!(isInputRange, staticMap!(Unqual, Ranges)) &&
    !is(CommonType!(staticMap!(ElementType, staticMap!(Unqual, Ranges))) == void))
{
    static if (Ranges.length == 1)
    {
        return rs[0];
    }
    else
    {
        static struct Result
        {
        private:
            alias R = staticMap!(Unqual, Ranges);
            alias RvalueElementType = CommonType!(staticMap!(.ElementType, R));
            private template sameET(A)
            {
                enum sameET = is(.ElementType!A == RvalueElementType);
            }

            enum bool allSameType = allSatisfy!(sameET, R);

            // This doesn't work yet
            static if (allSameType)
            {
                alias ElementType = ref RvalueElementType;
            }
            else
            {
                alias ElementType = RvalueElementType;
            }
            static if (allSameType && allSatisfy!(hasLvalueElements, R))
            {
                static ref RvalueElementType fixRef(ref RvalueElementType val)
                {
                    return val;
                }
            }
            else
            {
                static RvalueElementType fixRef(RvalueElementType val)
                {
                    return val;
                }
            }

            // This is the entire state
            R source;
            // TODO: use a vtable (or more) instead of linear iteration

        public:
            this(R input)
            {
                foreach (i, v; input)
                {
                    source[i] = v;
                }
            }

            import std.meta : anySatisfy;

            static if (anySatisfy!(isInfinite, R))
            {
                // Propagate infiniteness.
                enum bool empty = false;
            }
            else
            {
                @property bool empty()
                {
                    foreach (i, Unused; R)
                    {
                        if (!source[i].empty) return false;
                    }
                    return true;
                }
            }

            static if (allSatisfy!(isForwardRange, R))
                @property auto save()
                {
                    typeof(this) result = this;
                    foreach (i, Unused; R)
                    {
                        result.source[i] = result.source[i].save;
                    }
                    return result;
                }

            void popFront()
            {
                foreach (i, Unused; R)
                {
                    if (source[i].empty) continue;
                    source[i].popFront();
                    return;
                }
            }

            @property auto ref front()
            {
                foreach (i, Unused; R)
                {
                    if (source[i].empty) continue;
                    return fixRef(source[i].front);
                }
                assert(false);
            }

            static if (allSameType && allSatisfy!(hasAssignableElements, R))
            {
                // @@@BUG@@@
                //@property void front(T)(T v) if (is(T : RvalueElementType))

                @property void front(RvalueElementType v)
                {
                    foreach (i, Unused; R)
                    {
                        if (source[i].empty) continue;
                        source[i].front = v;
                        return;
                    }
                    assert(false);
                }
            }

            static if (allSatisfy!(hasMobileElements, R))
            {
                RvalueElementType moveFront()
                {
                    foreach (i, Unused; R)
                    {
                        if (source[i].empty) continue;
                        return source[i].moveFront();
                    }
                    assert(false);
                }
            }

            static if (allSatisfy!(isBidirectionalRange, R))
            {
                @property auto ref back()
                {
                    foreach_reverse (i, Unused; R)
                    {
                        if (source[i].empty) continue;
                        return fixRef(source[i].back);
                    }
                    assert(false);
                }

                void popBack()
                {
                    foreach_reverse (i, Unused; R)
                    {
                        if (source[i].empty) continue;
                        source[i].popBack();
                        return;
                    }
                }

                static if (allSatisfy!(hasMobileElements, R))
                {
                    RvalueElementType moveBack()
                    {
                        foreach_reverse (i, Unused; R)
                        {
                            if (source[i].empty) continue;
                            return source[i].moveBack();
                        }
                        assert(false);
                    }
                }

                static if (allSameType && allSatisfy!(hasAssignableElements, R))
                {
                    @property void back(RvalueElementType v)
                    {
                        foreach_reverse (i, Unused; R)
                        {
                            if (source[i].empty) continue;
                            source[i].back = v;
                            return;
                        }
                        assert(false);
                    }
                }
            }

            static if (allSatisfy!(hasLength, R))
            {
                @property size_t length()
                {
                    size_t result;
                    foreach (i, Unused; R)
                    {
                        result += source[i].length;
                    }
                    return result;
                }

                alias opDollar = length;
            }

            static if (allSatisfy!(isRandomAccessRange, R))
            {
                auto ref opIndex(size_t index)
                {
                    foreach (i, Range; R)
                    {
                        static if (isInfinite!(Range))
                        {
                            return source[i][index];
                        }
                        else
                        {
                            immutable length = source[i].length;
                            if (index < length) return fixRef(source[i][index]);
                            index -= length;
                        }
                    }
                    assert(false);
                }

                static if (allSatisfy!(hasMobileElements, R))
                {
                    RvalueElementType moveAt(size_t index)
                    {
                        foreach (i, Range; R)
                        {
                            static if (isInfinite!(Range))
                            {
                                return source[i].moveAt(index);
                            }
                            else
                            {
                                immutable length = source[i].length;
                                if (index < length) return source[i].moveAt(index);
                                index -= length;
                            }
                        }
                        assert(false);
                    }
                }

                static if (allSameType && allSatisfy!(hasAssignableElements, R))
                    void opIndexAssign(ElementType v, size_t index)
                    {
                        foreach (i, Range; R)
                        {
                            static if (isInfinite!(Range))
                            {
                                source[i][index] = v;
                            }
                            else
                            {
                                immutable length = source[i].length;
                                if (index < length)
                                {
                                    source[i][index] = v;
                                    return;
                                }
                                index -= length;
                            }
                        }
                        assert(false);
                    }
            }

            static if (allSatisfy!(hasLength, R) && allSatisfy!(hasSlicing, R))
                auto opSlice(size_t begin, size_t end)
                {
                    auto result = this;
                    foreach (i, Unused; R)
                    {
                        immutable len = result.source[i].length;
                        if (len < begin)
                        {
                            result.source[i] = result.source[i]
                                [len .. len];
                            begin -= len;
                        }
                        else
                        {
                            result.source[i] = result.source[i]
                                [begin .. len];
                            break;
                        }
                    }
                    auto cut = length;
                    cut = cut <= end ? 0 : cut - end;
                    foreach_reverse (i, Unused; R)
                    {
                        immutable len = result.source[i].length;
                        if (cut > len)
                        {
                            result.source[i] = result.source[i]
                                [0 .. 0];
                            cut -= len;
                        }
                        else
                        {
                            result.source[i] = result.source[i]
                                [0 .. len - cut];
                            break;
                        }
                    }
                    return result;
                }
        }
        return Result(rs);
    }
}

///
pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;

    int[] arr1 = [ 1, 2, 3, 4 ];
    int[] arr2 = [ 5, 6 ];
    int[] arr3 = [ 7 ];
    auto s = chain(arr1, arr2, arr3);
    assert(s.length == 7);
    assert(s[5] == 6);
    assert(equal(s, [1, 2, 3, 4, 5, 6, 7][]));
}

/**
 * Range primitives are carried over to the returned range if
 * all of the ranges provide them
 */
pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.sorting : sort;

    int[] arr1 = [5, 2, 8];
    int[] arr2 = [3, 7, 9];
    int[] arr3 = [1, 4, 6];

    // in-place sorting across all of the arrays
    auto s = arr1.chain(arr2, arr3).sort;

    assert(s.equal([1, 2, 3, 4, 5, 6, 7, 8, 9]));
    assert(arr1.equal([1, 2, 3]));
    assert(arr2.equal([4, 5, 6]));
    assert(arr3.equal([7, 8, 9]));
}

pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : AllDummyRanges, dummyLength,
                                          propagatesRangeType;

    {
        int[] arr1 = [ 1, 2, 3, 4 ];
        int[] arr2 = [ 5, 6 ];
        int[] arr3 = [ 7 ];
        int[] witness = [ 1, 2, 3, 4, 5, 6, 7 ];
        auto s1 = chain(arr1);
        static assert(isRandomAccessRange!(typeof(s1)));
        auto s2 = chain(arr1, arr2);
        static assert(isBidirectionalRange!(typeof(s2)));
        static assert(isRandomAccessRange!(typeof(s2)));
        s2.front = 1;
        auto s = chain(arr1, arr2, arr3);
        assert(s[5] == 6);
        assert(equal(s, witness));
        assert(s[5] == 6);
    }
    {
        int[] arr1 = [ 1, 2, 3, 4 ];
        int[] witness = [ 1, 2, 3, 4 ];
        assert(equal(chain(arr1), witness));
    }
    {
        uint[] foo = [1,2,3,4,5];
        uint[] bar = [1,2,3,4,5];
        auto c = chain(foo, bar);
        c[3] = 42;
        assert(c[3] == 42);
        assert(c.moveFront() == 1);
        assert(c.moveBack() == 5);
        assert(c.moveAt(4) == 5);
        assert(c.moveAt(5) == 1);
    }

    // Make sure bug 3311 is fixed.  ChainImpl should compile even if not all
    // elements are mutable.
    assert(equal(chain(iota(0, 3), iota(0, 3)), [0, 1, 2, 0, 1, 2]));

    // Test the case where infinite ranges are present.
    auto inf = chain([0,1,2][], cycle([4,5,6][]), [7,8,9][]); // infinite range
    assert(inf[0] == 0);
    assert(inf[3] == 4);
    assert(inf[6] == 4);
    assert(inf[7] == 5);
    static assert(isInfinite!(typeof(inf)));

    immutable int[] immi = [ 1, 2, 3 ];
    immutable float[] immf = [ 1, 2, 3 ];
    static assert(is(typeof(chain(immi, immf))));

    // Check that chain at least instantiates and compiles with every possible
    // pair of DummyRange types, in either order.

    foreach (DummyType1; AllDummyRanges)
    {
        DummyType1 dummy1;
        foreach (DummyType2; AllDummyRanges)
        {
            DummyType2 dummy2;
            auto myChain = chain(dummy1, dummy2);

            static assert(
                propagatesRangeType!(typeof(myChain), DummyType1, DummyType2)
            );

            assert(myChain.front == 1);
            foreach (i; 0 .. dummyLength)
            {
                myChain.popFront();
            }
            assert(myChain.front == 1);

            static if (isBidirectionalRange!DummyType1 &&
                      isBidirectionalRange!DummyType2) {
                assert(myChain.back == 10);
            }

            static if (isRandomAccessRange!DummyType1 &&
                      isRandomAccessRange!DummyType2) {
                assert(myChain[0] == 1);
            }

            static if (hasLvalueElements!DummyType1 && hasLvalueElements!DummyType2)
            {
                static assert(hasLvalueElements!(typeof(myChain)));
            }
            else
            {
                static assert(!hasLvalueElements!(typeof(myChain)));
            }
        }
    }
}

pure @safe nothrow @nogc unittest
{
    class Foo{}
    immutable(Foo)[] a;
    immutable(Foo)[] b;
    assert(chain(a, b).empty);
}

/**
Choose one of two ranges at runtime depending on a Boolean condition.

The ranges may be different, but they must have compatible element types (i.e.
$(D CommonType) must exist for the two element types). The result is a range
that offers the weakest capabilities of the two (e.g. $(D ForwardRange) if $(D
R1) is a random-access range and $(D R2) is a forward range).

Params:
    condition = which range to choose: $(D r1) if $(D true), $(D r2) otherwise
    r1 = the "true" range
    r2 = the "false" range

Returns:
    A range type dependent on $(D R1) and $(D R2).

Bugs:
    $(BUGZILLA 14660)
 */
auto choose(R1, R2)(bool condition, R1 r1, R2 r2)
if (isInputRange!(Unqual!R1) && isInputRange!(Unqual!R2) &&
    !is(CommonType!(ElementType!(Unqual!R1), ElementType!(Unqual!R2)) == void))
{
    static struct Result
    {
        import std.algorithm.comparison : max;
        import std.algorithm.internal : addressOf;
        import std.traits : hasElaborateCopyConstructor, hasElaborateDestructor;

        private union
        {
            void[max(R1.sizeof, R2.sizeof)] buffer = void;
            void* forAlignmentOnly = void;
        }
        private bool condition;
        private @property ref R1 r1()
        {
            assert(condition);
            return *cast(R1*) buffer.ptr;
        }
        private @property ref R2 r2()
        {
            assert(!condition);
            return *cast(R2*) buffer.ptr;
        }

        this(bool condition, R1 r1, R2 r2)
        {
            this.condition = condition;
            import std.conv : emplace;
            if (condition) emplace(addressOf(this.r1), r1);
            else emplace(addressOf(this.r2), r2);
        }

        // Carefully defined postblit to postblit the appropriate range
        static if (hasElaborateCopyConstructor!R1
            || hasElaborateCopyConstructor!R2)
        this(this)
        {
            if (condition)
            {
                static if (hasElaborateCopyConstructor!R1) r1.__postblit();
            }
            else
            {
                static if (hasElaborateCopyConstructor!R2) r2.__postblit();
            }
        }

        static if (hasElaborateDestructor!R1 || hasElaborateDestructor!R2)
        ~this()
        {
            if (condition) destroy(r1);
            else destroy(r2);
        }

        static if (isInfinite!R1 && isInfinite!R2)
            // Propagate infiniteness.
            enum bool empty = false;
        else
            @property bool empty()
            {
                return condition ? r1.empty : r2.empty;
            }

        @property auto ref front()
        {
            return condition ? r1.front : r2.front;
        }

        void popFront()
        {
            return condition ? r1.popFront : r2.popFront;
        }

        static if (isForwardRange!R1 && isForwardRange!R2)
            @property auto save()
            {
                auto result = this;
                if (condition) r1 = r1.save;
                else r2 = r2.save;
                return result;
            }

        @property void front(T)(T v)
        if (is(typeof({ r1.front = v; r2.front = v; })))
        {
            if (condition) r1.front = v; else r2.front = v;
        }

        static if (hasMobileElements!R1 && hasMobileElements!R2)
            auto moveFront()
            {
                return condition ? r1.moveFront : r2.moveFront;
            }

        static if (isBidirectionalRange!R1 && isBidirectionalRange!R2)
        {
            @property auto ref back()
            {
                return condition ? r1.back : r2.back;
            }

            void popBack()
            {
                return condition ? r1.popBack : r2.popBack;
            }

            static if (hasMobileElements!R1 && hasMobileElements!R2)
                auto moveBack()
                {
                    return condition ? r1.moveBack : r2.moveBack;
                }

            @property void back(T)(T v)
            if (is(typeof({ r1.back = v; r2.back = v; })))
            {
                if (condition) r1.back = v; else r2.back = v;
            }
        }

        static if (hasLength!R1 && hasLength!R2)
        {
            @property size_t length()
            {
                return condition ? r1.length : r2.length;
            }
            alias opDollar = length;
        }

        static if (isRandomAccessRange!R1 && isRandomAccessRange!R2)
        {
            auto ref opIndex(size_t index)
            {
                return condition ? r1[index] : r2[index];
            }

            static if (hasMobileElements!R1 && hasMobileElements!R2)
                auto moveAt(size_t index)
                {
                    return condition ? r1.moveAt(index) : r2.moveAt(index);
                }

            void opIndexAssign(T)(T v, size_t index)
            if (is(typeof({ r1[1] = v; r2[1] = v; })))
            {
                if (condition) r1[index] = v; else r2[index] = v;
            }
        }

        // BUG: this should work for infinite ranges, too
        static if (hasSlicing!R1 && hasSlicing!R2 &&
                !isInfinite!R2 && !isInfinite!R2)
            auto opSlice(size_t begin, size_t end)
            {
                auto result = this;
                if (condition) result.r1 = result.r1[begin .. end];
                else result.r2 = result.r2[begin .. end];
                return result;
            }
    }
    return Result(condition, r1, r2);
}

///
@system unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter, map;

    auto data1 = [ 1, 2, 3, 4 ].filter!(a => a != 3);
    auto data2 = [ 5, 6, 7, 8 ].map!(a => a + 1);

    // choose() is primarily useful when you need to select one of two ranges
    // with different types at runtime.
    static assert(!is(typeof(data1) == typeof(data2)));

    auto chooseRange(bool pickFirst)
    {
        // The returned range is a common wrapper type that can be used for
        // returning or storing either range without running into a type error.
        return choose(pickFirst, data1, data2);

        // Simply returning the chosen range without using choose() does not
        // work, because map() and filter() return different types.
        //return pickFirst ? data1 : data2; // does not compile
    }

    auto result = chooseRange(true);
    assert(result.equal([ 1, 2, 4 ]));

    result = chooseRange(false);
    assert(result.equal([ 6, 7, 8, 9 ]));
}

/**
Choose one of multiple ranges at runtime.

The ranges may be different, but they must have compatible element types. The
result is a range that offers the weakest capabilities of all $(D Ranges).

Params:
    index = which range to choose, must be less than the number of ranges
    rs = two or more ranges

Returns:
    The indexed range. If rs consists of only one range, the return type is an
    alias of that range's type.
 */
auto chooseAmong(Ranges...)(size_t index, Ranges rs)
if (Ranges.length >= 2
        && allSatisfy!(isInputRange, staticMap!(Unqual, Ranges))
        && !is(CommonType!(staticMap!(ElementType, Ranges)) == void))
{
    static if (Ranges.length == 2)
        return choose(index == 0, rs[0], rs[1]);
    else
        return choose(index == 0, rs[0], chooseAmong(index - 1, rs[1 .. $]));
}

///
@system unittest
{
    import std.algorithm.comparison : equal;

    int[] arr1 = [ 1, 2, 3, 4 ];
    int[] arr2 = [ 5, 6 ];
    int[] arr3 = [ 7 ];

    {
        auto s = chooseAmong(0, arr1, arr2, arr3);
        auto t = s.save;
        assert(s.length == 4);
        assert(s[2] == 3);
        s.popFront();
        assert(equal(t, [1, 2, 3, 4][]));
    }
    {
        auto s = chooseAmong(1, arr1, arr2, arr3);
        assert(s.length == 2);
        s.front = 8;
        assert(equal(s, [8, 6][]));
    }
    {
        auto s = chooseAmong(1, arr1, arr2, arr3);
        assert(s.length == 2);
        s[1] = 9;
        assert(equal(s, [8, 9][]));
    }
    {
        auto s = chooseAmong(1, arr2, arr1, arr3)[1 .. 3];
        assert(s.length == 2);
        assert(equal(s, [2, 3][]));
    }
    {
        auto s = chooseAmong(0, arr1, arr2, arr3);
        assert(s.length == 4);
        assert(s.back == 4);
        s.popBack();
        s.back = 5;
        assert(equal(s, [1, 2, 5][]));
        s.back = 3;
        assert(equal(s, [1, 2, 3][]));
    }
    {
        uint[] foo = [1,2,3,4,5];
        uint[] bar = [6,7,8,9,10];
        auto c = chooseAmong(1,foo, bar);
        assert(c[3] == 9);
        c[3] = 42;
        assert(c[3] == 42);
        assert(c.moveFront() == 6);
        assert(c.moveBack() == 10);
        assert(c.moveAt(4) == 10);
    }
    {
        import std.range : cycle;
        auto s = chooseAmong(1, cycle(arr2), cycle(arr3));
        assert(isInfinite!(typeof(s)));
        assert(!s.empty);
        assert(s[100] == 7);
    }
}

@system unittest
{
    int[] a = [1, 2, 3];
    long[] b = [4, 5, 6];
    auto c = chooseAmong(0, a, b);
    c[0] = 42;
    assert(c[0] == 42);
}


/**
$(D roundRobin(r1, r2, r3)) yields $(D r1.front), then $(D r2.front),
then $(D r3.front), after which it pops off one element from each and
continues again from $(D r1). For example, if two ranges are involved,
it alternately yields elements off the two ranges. $(D roundRobin)
stops after it has consumed all ranges (skipping over the ones that
finish early).
 */
auto roundRobin(Rs...)(Rs rs)
if (Rs.length > 1 && allSatisfy!(isInputRange, staticMap!(Unqual, Rs)))
{
    struct Result
    {
        import std.conv : to;

        public Rs source;
        private size_t _current = size_t.max;

        @property bool empty()
        {
            foreach (i, Unused; Rs)
            {
                if (!source[i].empty) return false;
            }
            return true;
        }

        @property auto ref front()
        {
            final switch (_current)
            {
                foreach (i, R; Rs)
                {
                    case i:
                        assert(
                            !source[i].empty,
                            "Attempting to fetch the front of an empty roundRobin"
                        );
                        return source[i].front;
                }
            }
            assert(0);
        }

        void popFront()
        {
            final switch (_current)
            {
                foreach (i, R; Rs)
                {
                    case i:
                        source[i].popFront();
                        break;
                }
            }

            auto next = _current == (Rs.length - 1) ? 0 : (_current + 1);
            final switch (next)
            {
                foreach (i, R; Rs)
                {
                    case i:
                        if (!source[i].empty)
                        {
                            _current = i;
                            return;
                        }
                        if (i == _current)
                        {
                            _current = _current.max;
                            return;
                        }
                        goto case (i + 1) % Rs.length;
                }
            }
        }

        static if (allSatisfy!(isForwardRange, staticMap!(Unqual, Rs)))
            @property auto save()
            {
                Result result = this;
                foreach (i, Unused; Rs)
                {
                    result.source[i] = result.source[i].save;
                }
                return result;
            }

        static if (allSatisfy!(hasLength, Rs))
        {
            @property size_t length()
            {
                size_t result;
                foreach (i, R; Rs)
                {
                    result += source[i].length;
                }
                return result;
            }

            alias opDollar = length;
        }
    }

    return Result(rs, 0);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;

    int[] a = [ 1, 2, 3 ];
    int[] b = [ 10, 20, 30, 40 ];
    auto r = roundRobin(a, b);
    assert(equal(r, [ 1, 10, 2, 20, 3, 30, 40 ]));
}

/**
 * roundRobin can be used to create "interleave" functionality which inserts
 * an element between each element in a range.
 */
@safe unittest
{
    import std.algorithm.comparison : equal;

    auto interleave(R, E)(R range, E element)
    if ((isInputRange!R && hasLength!R) || isForwardRange!R)
    {
        static if (hasLength!R)
            immutable len = range.length;
        else
            immutable len = range.save.walkLength;

        return roundRobin(
            range,
            element.repeat(len - 1)
        );
    }

    assert(interleave([1, 2, 3], 0).equal([1, 0, 2, 0, 3]));
}

/**
Iterates a random-access range starting from a given point and
progressively extending left and right from that point. If no initial
point is given, iteration starts from the middle of the
range. Iteration spans the entire range.

When `startingIndex` is 0 the range will be fully iterated in order
and in reverse order when `r.length` is given.

Params:
    r = a random access range with length and slicing
    startingIndex = the index to begin iteration from

Returns:
    A forward range with length
 */
auto radial(Range, I)(Range r, I startingIndex)
if (isRandomAccessRange!(Unqual!Range) && hasLength!(Unqual!Range) && hasSlicing!(Unqual!Range) && isIntegral!I)
{
    if (startingIndex != r.length) ++startingIndex;
    return roundRobin(retro(r[0 .. startingIndex]), r[startingIndex .. r.length]);
}

/// Ditto
auto radial(R)(R r)
if (isRandomAccessRange!(Unqual!R) && hasLength!(Unqual!R) && hasSlicing!(Unqual!R))
{
    return .radial(r, (r.length - !r.empty) / 2);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    int[] a = [ 1, 2, 3, 4, 5 ];
    assert(equal(radial(a), [ 3, 4, 2, 5, 1 ]));
    a = [ 1, 2, 3, 4 ];
    assert(equal(radial(a), [ 2, 3, 1, 4 ]));

    // If the left end is reached first, the remaining elements on the right
    // are concatenated in order:
    a = [ 0, 1, 2, 3, 4, 5 ];
    assert(equal(radial(a, 1), [ 1, 2, 0, 3, 4, 5 ]));

    // If the right end is reached first, the remaining elements on the left
    // are concatenated in reverse order:
    assert(equal(radial(a, 4), [ 4, 5, 3, 2, 1, 0 ]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : text;
    import std.exception : enforce;
    import std.internal.test.dummyrange : DummyRange, Length, RangeType, ReturnBy;

    void test(int[] input, int[] witness)
    {
        enforce(equal(radial(input), witness),
                text(radial(input), " vs. ", witness));
    }
    test([], []);
    test([ 1 ], [ 1 ]);
    test([ 1, 2 ], [ 1, 2 ]);
    test([ 1, 2, 3 ], [ 2, 3, 1 ]);
    test([ 1, 2, 3, 4 ], [ 2, 3, 1, 4 ]);
    test([ 1, 2, 3, 4, 5 ], [ 3, 4, 2, 5, 1 ]);
    test([ 1, 2, 3, 4, 5, 6 ], [ 3, 4, 2, 5, 1, 6 ]);

    int[] a = [ 1, 2, 3, 4, 5 ];
    assert(equal(radial(a, 1), [ 2, 3, 1, 4, 5 ]));
    assert(equal(radial(a, 0), [ 1, 2, 3, 4, 5 ])); // only right subrange
    assert(equal(radial(a, a.length), [ 5, 4, 3, 2, 1 ])); // only left subrange
    static assert(isForwardRange!(typeof(radial(a, 1))));

    auto r = radial([1,2,3,4,5]);
    for (auto rr = r.save; !rr.empty; rr.popFront())
    {
        assert(rr.front == moveFront(rr));
    }
    r.front = 5;
    assert(r.front == 5);

    // Test instantiation without lvalue elements.
    DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Random) dummy;
    assert(equal(radial(dummy, 4), [5, 6, 4, 7, 3, 8, 2, 9, 1, 10]));

    // immutable int[] immi = [ 1, 2 ];
    // static assert(is(typeof(radial(immi))));
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    auto LL = iota(1L, 6L);
    auto r = radial(LL);
    assert(equal(r, [3L, 4L, 2L, 5L, 1L]));
}

/**
Lazily takes only up to `n` elements of a range. This is
particularly useful when using with infinite ranges.

Unlike $(LREF takeExactly), `take` does not require that there
are `n` or more elements in `input`. As a consequence, length
information is not applied to the result unless `input` also has
length information.

Params:
    input = an input range to iterate over up to `n` times
    n = the number of elements to take

Returns:
    At minimum, an input range. If the range offers random access
    and `length`, `take` offers them as well.
 */
Take!R take(R)(R input, size_t n)
if (isInputRange!(Unqual!R))
{
    alias U = Unqual!R;
    static if (is(R T == Take!T))
    {
        import std.algorithm.comparison : min;
        return R(input.source, min(n, input._maxAvailable));
    }
    else static if (!isInfinite!U && hasSlicing!U)
    {
        import std.algorithm.comparison : min;
        return input[0 .. min(n, input.length)];
    }
    else
    {
        return Take!R(input, n);
    }
}

/// ditto
struct Take(Range)
if (isInputRange!(Unqual!Range) &&
    //take _cannot_ test hasSlicing on infinite ranges, because hasSlicing uses
    //take for slicing infinite ranges.
    !((!isInfinite!(Unqual!Range) && hasSlicing!(Unqual!Range)) || is(Range T == Take!T)))
{
    private alias R = Unqual!Range;

    /// User accessible in read and write
    public R source;

    private size_t _maxAvailable;

    alias Source = R;

    /// Range primitives
    @property bool empty()
    {
        return _maxAvailable == 0 || source.empty;
    }

    /// ditto
    @property auto ref front()
    {
        assert(!empty,
            "Attempting to fetch the front of an empty "
            ~ Take.stringof);
        return source.front;
    }

    /// ditto
    void popFront()
    {
        assert(!empty,
            "Attempting to popFront() past the end of a "
            ~ Take.stringof);
        source.popFront();
        --_maxAvailable;
    }

    static if (isForwardRange!R)
        /// ditto
        @property Take save()
        {
            return Take(source.save, _maxAvailable);
        }

    static if (hasAssignableElements!R)
        /// ditto
        @property void front(ElementType!R v)
        {
            assert(!empty,
                "Attempting to assign to the front of an empty "
                ~ Take.stringof);
            // This has to return auto instead of void because of Bug 4706.
            source.front = v;
        }

    static if (hasMobileElements!R)
    {
        /// ditto
        auto moveFront()
        {
            assert(!empty,
                "Attempting to move the front of an empty "
                ~ Take.stringof);
            return source.moveFront();
        }
    }

    static if (isInfinite!R)
    {
        /// ditto
        @property size_t length() const
        {
            return _maxAvailable;
        }

        /// ditto
        alias opDollar = length;

        //Note: Due to Take/hasSlicing circular dependency,
        //This needs to be a restrained template.
        /// ditto
        auto opSlice()(size_t i, size_t j)
        if (hasSlicing!R)
        {
            assert(i <= j, "Invalid slice bounds");
            assert(j <= length, "Attempting to slice past the end of a "
                ~ Take.stringof);
            return source[i .. j];
        }
    }
    else static if (hasLength!R)
    {
        /// ditto
        @property size_t length()
        {
            import std.algorithm.comparison : min;
            return min(_maxAvailable, source.length);
        }

        alias opDollar = length;
    }

    static if (isRandomAccessRange!R)
    {
        /// ditto
        void popBack()
        {
            assert(!empty,
                "Attempting to popBack() past the beginning of a "
                ~ Take.stringof);
            --_maxAvailable;
        }

        /// ditto
        @property auto ref back()
        {
            assert(!empty,
                "Attempting to fetch the back of an empty "
                ~ Take.stringof);
            return source[this.length - 1];
        }

        /// ditto
        auto ref opIndex(size_t index)
        {
            assert(index < length,
                "Attempting to index out of the bounds of a "
                ~ Take.stringof);
            return source[index];
        }

        static if (hasAssignableElements!R)
        {
            /// ditto
            @property void back(ElementType!R v)
            {
                // This has to return auto instead of void because of Bug 4706.
                assert(!empty,
                    "Attempting to assign to the back of an empty "
                    ~ Take.stringof);
                source[this.length - 1] = v;
            }

            /// ditto
            void opIndexAssign(ElementType!R v, size_t index)
            {
                assert(index < length,
                    "Attempting to index out of the bounds of a "
                    ~ Take.stringof);
                source[index] = v;
            }
        }

        static if (hasMobileElements!R)
        {
            /// ditto
            auto moveBack()
            {
                assert(!empty,
                    "Attempting to move the back of an empty "
                    ~ Take.stringof);
                return source.moveAt(this.length - 1);
            }

            /// ditto
            auto moveAt(size_t index)
            {
                assert(index < length,
                    "Attempting to index out of the bounds of a "
                    ~ Take.stringof);
                return source.moveAt(index);
            }
        }
    }

    /**
    Access to maximal length of the range.
    Note: the actual length of the range depends on the underlying range.
    If it has fewer elements, it will stop before maxLength is reached.
    */
    @property size_t maxLength() const
    {
        return _maxAvailable;
    }
}

/**
This template simply aliases itself to R and is useful for consistency in
generic code.
*/
template Take(R)
if (isInputRange!(Unqual!R) &&
    ((!isInfinite!(Unqual!R) && hasSlicing!(Unqual!R)) || is(R T == Take!T)))
{
    alias Take = R;
}

///
pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;

    int[] arr1 = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
    auto s = take(arr1, 5);
    assert(s.length == 5);
    assert(s[4] == 5);
    assert(equal(s, [ 1, 2, 3, 4, 5 ][]));
}

/**
 * If the range runs out before `n` elements, `take` simply returns the entire
 * range (unlike $(LREF takeExactly), which will cause an assertion failure if
 * the range ends prematurely):
 */
pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;

    int[] arr2 = [ 1, 2, 3 ];
    auto t = take(arr2, 5);
    assert(t.length == 3);
    assert(equal(t, [ 1, 2, 3 ]));
}

pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : AllDummyRanges;

    int[] arr1 = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
    auto s = take(arr1, 5);
    assert(s.length == 5);
    assert(s[4] == 5);
    assert(equal(s, [ 1, 2, 3, 4, 5 ][]));
    assert(equal(retro(s), [ 5, 4, 3, 2, 1 ][]));

    // Test fix for bug 4464.
    static assert(is(typeof(s) == Take!(int[])));
    static assert(is(typeof(s) == int[]));

    // Test using narrow strings.
    import std.exception : assumeWontThrow;

    auto myStr = "This is a string.";
    auto takeMyStr = take(myStr, 7);
    assert(assumeWontThrow(equal(takeMyStr, "This is")));
    // Test fix for bug 5052.
    auto takeMyStrAgain = take(takeMyStr, 4);
    assert(assumeWontThrow(equal(takeMyStrAgain, "This")));
    static assert(is (typeof(takeMyStrAgain) == typeof(takeMyStr)));
    takeMyStrAgain = take(takeMyStr, 10);
    assert(assumeWontThrow(equal(takeMyStrAgain, "This is")));

    foreach (DummyType; AllDummyRanges)
    {
        DummyType dummy;
        auto t = take(dummy, 5);
        alias T = typeof(t);

        static if (isRandomAccessRange!DummyType)
        {
            static assert(isRandomAccessRange!T);
            assert(t[4] == 5);

            assert(moveAt(t, 1) == t[1]);
            assert(t.back == moveBack(t));
        }
        else static if (isForwardRange!DummyType)
        {
            static assert(isForwardRange!T);
        }

        for (auto tt = t; !tt.empty; tt.popFront())
        {
            assert(tt.front == moveFront(tt));
        }

        // Bidirectional ranges can't be propagated properly if they don't
        // also have random access.

        assert(equal(t, [1,2,3,4,5]));

        //Test that take doesn't wrap the result of take.
        assert(take(t, 4) == take(dummy, 4));
    }

    immutable myRepeat = repeat(1);
    static assert(is(Take!(typeof(myRepeat))));
}

pure @safe nothrow @nogc unittest
{
    //check for correct slicing of Take on an infinite range
    import std.algorithm.comparison : equal;
    foreach (start; 0 .. 4)
        foreach (stop; start .. 4)
            assert(iota(4).cycle.take(4)[start .. stop]
                .equal(iota(start, stop)));
}

pure @safe nothrow @nogc unittest
{
    // Check that one can declare variables of all Take types,
    // and that they match the return type of the corresponding
    // take().  (See issue 4464.)
    int[] r1;
    Take!(int[]) t1;
    t1 = take(r1, 1);
    assert(t1.empty);

    string r2;
    Take!string t2;
    t2 = take(r2, 1);
    assert(t2.empty);

    Take!(Take!string) t3;
    t3 = take(t2, 1);
    assert(t3.empty);
}

pure @safe nothrow @nogc unittest
{
    alias R1 = typeof(repeat(1));
    alias R2 = typeof(cycle([1]));
    alias TR1 = Take!R1;
    alias TR2 = Take!R2;
    static assert(isBidirectionalRange!TR1);
    static assert(isBidirectionalRange!TR2);
}

pure @safe nothrow @nogc unittest //12731
{
    auto a = repeat(1);
    auto s = a[1 .. 5];
    s = s[1 .. 3];
    assert(s.length == 2);
    assert(s[0] == 1);
    assert(s[1] == 1);
}

pure @safe nothrow @nogc unittest //13151
{
    import std.algorithm.comparison : equal;

    auto r = take(repeat(1, 4), 3);
    assert(r.take(2).equal(repeat(1, 2)));
}


/**
Similar to $(LREF take), but assumes that $(D range) has at least $(D
n) elements. Consequently, the result of $(D takeExactly(range, n))
always defines the $(D length) property (and initializes it to $(D n))
even when $(D range) itself does not define $(D length).

The result of $(D takeExactly) is identical to that of $(LREF take) in
cases where the original range defines $(D length) or is infinite.

Unlike $(LREF take), however, it is illegal to pass a range with less than
$(D n) elements to $(D takeExactly); this will cause an assertion failure.
 */
auto takeExactly(R)(R range, size_t n)
if (isInputRange!R)
{
    static if (is(typeof(takeExactly(range._input, n)) == R))
    {
        assert(n <= range._n,
               "Attempted to take more than the length of the range with takeExactly.");
        // takeExactly(takeExactly(r, n1), n2) has the same type as
        // takeExactly(r, n1) and simply returns takeExactly(r, n2)
        range._n = n;
        return range;
    }
    //Also covers hasSlicing!R for finite ranges.
    else static if (hasLength!R)
    {
        assert(n <= range.length,
               "Attempted to take more than the length of the range with takeExactly.");
        return take(range, n);
    }
    else static if (isInfinite!R)
        return Take!R(range, n);
    else
    {
        static struct Result
        {
            R _input;
            private size_t _n;

            @property bool empty() const { return !_n; }
            @property auto ref front()
            {
                assert(_n > 0, "front() on an empty " ~ Result.stringof);
                return _input.front;
            }
            void popFront() { _input.popFront(); --_n; }
            @property size_t length() const { return _n; }
            alias opDollar = length;

            @property Take!R _takeExactly_Result_asTake()
            {
                return typeof(return)(_input, _n);
            }

            alias _takeExactly_Result_asTake this;

            static if (isForwardRange!R)
                @property auto save()
                {
                    return Result(_input.save, _n);
                }

            static if (hasMobileElements!R)
            {
                auto moveFront()
                {
                    assert(!empty,
                        "Attempting to move the front of an empty "
                        ~ typeof(this).stringof);
                    return _input.moveFront();
                }
            }

            static if (hasAssignableElements!R)
            {
                @property auto ref front(ElementType!R v)
                {
                    assert(!empty,
                        "Attempting to assign to the front of an empty "
                        ~ typeof(this).stringof);
                    return _input.front = v;
                }
            }
        }

        return Result(range, n);
    }
}

///
pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;

    auto a = [ 1, 2, 3, 4, 5 ];

    auto b = takeExactly(a, 3);
    assert(equal(b, [1, 2, 3]));
    static assert(is(typeof(b.length) == size_t));
    assert(b.length == 3);
    assert(b.front == 1);
    assert(b.back == 3);
}

pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter;

    auto a = [ 1, 2, 3, 4, 5 ];
    auto b = takeExactly(a, 3);
    assert(equal(b, [1, 2, 3]));
    auto c = takeExactly(b, 2);
    assert(equal(c, [1, 2]));



    auto d = filter!"a > 2"(a);
    auto e = takeExactly(d, 3);
    assert(equal(e, [3, 4, 5]));
    static assert(is(typeof(e.length) == size_t));
    assert(e.length == 3);
    assert(e.front == 3);

    assert(equal(takeExactly(e, 3), [3, 4, 5]));
}

pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : AllDummyRanges;

    auto a = [ 1, 2, 3, 4, 5 ];
    //Test that take and takeExactly are the same for ranges which define length
    //but aren't sliceable.
    struct L
    {
        @property auto front() { return _arr[0]; }
        @property bool empty() { return _arr.empty; }
        void popFront() { _arr.popFront(); }
        @property size_t length() { return _arr.length; }
        int[] _arr;
    }
    static assert(is(typeof(take(L(a), 3)) == typeof(takeExactly(L(a), 3))));
    assert(take(L(a), 3) == takeExactly(L(a), 3));

    //Test that take and takeExactly are the same for ranges which are sliceable.
    static assert(is(typeof(take(a, 3)) == typeof(takeExactly(a, 3))));
    assert(take(a, 3) == takeExactly(a, 3));

    //Test that take and takeExactly are the same for infinite ranges.
    auto inf = repeat(1);
    static assert(is(typeof(take(inf, 5)) == Take!(typeof(inf))));
    assert(take(inf, 5) == takeExactly(inf, 5));

    //Test that take and takeExactly are _not_ the same for ranges which don't
    //define length.
    static assert(!is(typeof(take(filter!"true"(a), 3)) == typeof(takeExactly(filter!"true"(a), 3))));

    foreach (DummyType; AllDummyRanges)
    {
        {
            DummyType dummy;
            auto t = takeExactly(dummy, 5);

            //Test that takeExactly doesn't wrap the result of takeExactly.
            assert(takeExactly(t, 4) == takeExactly(dummy, 4));
        }

        static if (hasMobileElements!DummyType)
        {
            {
                auto t = takeExactly(DummyType.init, 4);
                assert(t.moveFront() == 1);
                assert(equal(t, [1, 2, 3, 4]));
            }
        }

        static if (hasAssignableElements!DummyType)
        {
            {
                auto t = takeExactly(DummyType.init, 4);
                t.front = 9;
                assert(equal(t, [9, 2, 3, 4]));
            }
        }
    }
}

pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : DummyRange, Length, RangeType, ReturnBy;

    alias DummyType = DummyRange!(ReturnBy.Value, Length.No, RangeType.Forward);
    auto te = takeExactly(DummyType(), 5);
    Take!DummyType t = te;
    assert(equal(t, [1, 2, 3, 4, 5]));
    assert(equal(t, te));
}

/**
Returns a range with at most one element; for example, $(D
takeOne([42, 43, 44])) returns a range consisting of the integer $(D
42). Calling $(D popFront()) off that range renders it empty.

In effect $(D takeOne(r)) is somewhat equivalent to $(D take(r, 1)) but in
certain interfaces it is important to know statically that the range may only
have at most one element.

The type returned by $(D takeOne) is a random-access range with length
regardless of $(D R)'s capabilities, as long as it is a forward range.
(another feature that distinguishes $(D takeOne) from $(D take)). If
(D R) is an input range but not a forward range, return type is an input
range with all random-access capabilities except save.
 */
auto takeOne(R)(R source)
if (isInputRange!R)
{
    static if (hasSlicing!R)
    {
        return source[0 .. !source.empty];
    }
    else
    {
        static struct Result
        {
            private R _source;
            private bool _empty = true;
            @property bool empty() const { return _empty; }
            @property auto ref front()
            {
                assert(!empty, "Attempting to fetch the front of an empty takeOne");
                return _source.front;
            }
            void popFront()
            {
                assert(!empty, "Attempting to popFront an empty takeOne");
                _source.popFront();
                _empty = true;
            }
            void popBack()
            {
                assert(!empty, "Attempting to popBack an empty takeOne");
                _source.popFront();
                _empty = true;
            }
            static if (isForwardRange!(Unqual!R))
            {
                @property auto save() { return Result(_source.save, empty); }
            }
            @property auto ref back()
            {
                assert(!empty, "Attempting to fetch the back of an empty takeOne");
                return _source.front;
            }
            @property size_t length() const { return !empty; }
            alias opDollar = length;
            auto ref opIndex(size_t n)
            {
                assert(n < length, "Attempting to index a takeOne out of bounds");
                return _source.front;
            }
            auto opSlice(size_t m, size_t n)
            {
                assert(m <= n && n < length, "Attempting to index a takeOne out of bounds");
                return n > m ? this : Result(_source, false);
            }
            // Non-standard property
            @property R source() { return _source; }
        }

        return Result(source, source.empty);
    }
}

///
pure @safe nothrow unittest
{
    auto s = takeOne([42, 43, 44]);
    static assert(isRandomAccessRange!(typeof(s)));
    assert(s.length == 1);
    assert(!s.empty);
    assert(s.front == 42);
    s.front = 43;
    assert(s.front == 43);
    assert(s.back == 43);
    assert(s[0] == 43);
    s.popFront();
    assert(s.length == 0);
    assert(s.empty);
}

pure @safe nothrow @nogc unittest
{
    struct NonForwardRange
    {
        enum empty = false;
        int front() { return 42; }
        void popFront() {}
    }

    static assert(!isForwardRange!NonForwardRange);

    auto s = takeOne(NonForwardRange());
    assert(s.front == 42);
}

//guards against issue 16999
pure @safe unittest
{
    auto myIota = new class
    {
        int front = 0;
        @safe void popFront(){front++;}
        enum empty = false;
    };
    auto iotaPart = myIota.takeOne;
    int sum;
    foreach (var; chain(iotaPart, iotaPart, iotaPart))
    {
        sum += var;
    }
    assert(sum == 3);
    assert(iotaPart.front == 3);
}

/++
    Returns an empty range which is statically known to be empty and is
    guaranteed to have $(D length) and be random access regardless of $(D R)'s
    capabilities.
  +/
auto takeNone(R)()
if (isInputRange!R)
{
    return typeof(takeOne(R.init)).init;
}

///
pure @safe nothrow @nogc unittest
{
    auto range = takeNone!(int[])();
    assert(range.length == 0);
    assert(range.empty);
}

pure @safe nothrow @nogc unittest
{
    enum ctfe = takeNone!(int[])();
    static assert(ctfe.length == 0);
    static assert(ctfe.empty);
}


/++
    Creates an empty range from the given range in $(BIGOH 1). If it can, it
    will return the same range type. If not, it will return
    $(D takeExactly(range, 0)).
  +/
auto takeNone(R)(R range)
if (isInputRange!R)
{
    import std.traits : isDynamicArray;
    //Makes it so that calls to takeNone which don't use UFCS still work with a
    //member version if it's defined.
    static if (is(typeof(R.takeNone)))
        auto retval = range.takeNone();
    //@@@BUG@@@ 8339
    else static if (isDynamicArray!R)/+ ||
                   (is(R == struct) && __traits(compiles, {auto r = R.init;}) && R.init.empty))+/
    {
        auto retval = R.init;
    }
    //An infinite range sliced at [0 .. 0] would likely still not be empty...
    else static if (hasSlicing!R && !isInfinite!R)
        auto retval = range[0 .. 0];
    else
        auto retval = takeExactly(range, 0);

    //@@@BUG@@@ 7892 prevents this from being done in an out block.
    assert(retval.empty);
    return retval;
}

///
pure @safe nothrow unittest
{
    import std.algorithm.iteration : filter;
    assert(takeNone([42, 27, 19]).empty);
    assert(takeNone("dlang.org").empty);
    assert(takeNone(filter!"true"([42, 27, 19])).empty);
}

@safe unittest
{
    import std.algorithm.iteration : filter;
    import std.meta : AliasSeq;

    struct Dummy
    {
        mixin template genInput()
        {
        @safe:
            @property bool empty() { return _arr.empty; }
            @property auto front() { return _arr.front; }
            void popFront() { _arr.popFront(); }
            static assert(isInputRange!(typeof(this)));
        }
    }
    alias genInput = Dummy.genInput;

    static struct NormalStruct
    {
        //Disabled to make sure that the takeExactly version is used.
        @disable this();
        this(int[] arr) { _arr = arr; }
        mixin genInput;
        int[] _arr;
    }

    static struct SliceStruct
    {
        @disable this();
        this(int[] arr) { _arr = arr; }
        mixin genInput;
        @property auto save() { return this; }
        auto opSlice(size_t i, size_t j) { return typeof(this)(_arr[i .. j]); }
        @property size_t length() { return _arr.length; }
        int[] _arr;
    }

    static struct InitStruct
    {
        mixin genInput;
        int[] _arr;
    }

    static struct TakeNoneStruct
    {
        this(int[] arr) { _arr = arr; }
        @disable this();
        mixin genInput;
        auto takeNone() { return typeof(this)(null); }
        int[] _arr;
    }

    static class NormalClass
    {
        this(int[] arr) {_arr = arr;}
        mixin genInput;
        int[] _arr;
    }

    static class SliceClass
    {
    @safe:
        this(int[] arr) { _arr = arr; }
        mixin genInput;
        @property auto save() { return new typeof(this)(_arr); }
        auto opSlice(size_t i, size_t j) { return new typeof(this)(_arr[i .. j]); }
        @property size_t length() { return _arr.length; }
        int[] _arr;
    }

    static class TakeNoneClass
    {
    @safe:
        this(int[] arr) { _arr = arr; }
        mixin genInput;
        auto takeNone() { return new typeof(this)(null); }
        int[] _arr;
    }

    import std.format : format;

    foreach (range; AliasSeq!([1, 2, 3, 4, 5],
                             "hello world",
                             "hello world"w,
                             "hello world"d,
                             SliceStruct([1, 2, 3]),
                             //@@@BUG@@@ 8339 forces this to be takeExactly
                             //`InitStruct([1, 2, 3]),
                             TakeNoneStruct([1, 2, 3])))
    {
        static assert(takeNone(range).empty, typeof(range).stringof);
        assert(takeNone(range).empty);
        static assert(is(typeof(range) == typeof(takeNone(range))), typeof(range).stringof);
    }

    foreach (range; AliasSeq!(NormalStruct([1, 2, 3]),
                             InitStruct([1, 2, 3])))
    {
        static assert(takeNone(range).empty, typeof(range).stringof);
        assert(takeNone(range).empty);
        static assert(is(typeof(takeExactly(range, 0)) == typeof(takeNone(range))), typeof(range).stringof);
    }

    //Don't work in CTFE.
    auto normal = new NormalClass([1, 2, 3]);
    assert(takeNone(normal).empty);
    static assert(is(typeof(takeExactly(normal, 0)) == typeof(takeNone(normal))), typeof(normal).stringof);

    auto slice = new SliceClass([1, 2, 3]);
    assert(takeNone(slice).empty);
    static assert(is(SliceClass == typeof(takeNone(slice))), typeof(slice).stringof);

    auto taken = new TakeNoneClass([1, 2, 3]);
    assert(takeNone(taken).empty);
    static assert(is(TakeNoneClass == typeof(takeNone(taken))), typeof(taken).stringof);

    auto filtered = filter!"true"([1, 2, 3, 4, 5]);
    assert(takeNone(filtered).empty);
    //@@@BUG@@@ 8339 and 5941 force this to be takeExactly
    //static assert(is(typeof(filtered) == typeof(takeNone(filtered))), typeof(filtered).stringof);
}

/++
 + Return a _range advanced to within $(D _n) elements of the end of
 + $(D _range).
 +
 + Intended as the _range equivalent of the Unix
 + $(HTTP en.wikipedia.org/wiki/Tail_%28Unix%29, _tail) utility. When the length
 + of $(D _range) is less than or equal to $(D _n), $(D _range) is returned
 + as-is.
 +
 + Completes in $(BIGOH 1) steps for ranges that support slicing and have
 + length. Completes in $(BIGOH _range.length) time for all other ranges.
 +
 + Params:
 +    range = _range to get _tail of
 +    n = maximum number of elements to include in _tail
 +
 + Returns:
 +    Returns the _tail of $(D _range) augmented with length information
 +/
auto tail(Range)(Range range, size_t n)
if (isInputRange!Range && !isInfinite!Range &&
    (hasLength!Range || isForwardRange!Range))
{
    static if (hasLength!Range)
    {
        immutable length = range.length;
        if (n >= length)
            return range.takeExactly(length);
        else
            return range.drop(length - n).takeExactly(n);
    }
    else
    {
        Range scout = range.save;
        foreach (immutable i; 0 .. n)
        {
            if (scout.empty)
                return range.takeExactly(i);
            scout.popFront();
        }

        auto tail = range.save;
        while (!scout.empty)
        {
            assert(!tail.empty);
            scout.popFront();
            tail.popFront();
        }

        return tail.takeExactly(n);
    }
}

///
pure @safe nothrow unittest
{
    // tail -c n
    assert([1, 2, 3].tail(1) == [3]);
    assert([1, 2, 3].tail(2) == [2, 3]);
    assert([1, 2, 3].tail(3) == [1, 2, 3]);
    assert([1, 2, 3].tail(4) == [1, 2, 3]);
    assert([1, 2, 3].tail(0).length == 0);

    // tail --lines=n
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : joiner;
    import std.exception : assumeWontThrow;
    import std.string : lineSplitter;
    assert("one\ntwo\nthree"
        .lineSplitter
        .tail(2)
        .joiner("\n")
        .equal("two\nthree")
        .assumeWontThrow);
}

// @nogc prevented by @@@BUG@@@ 15408
pure nothrow @safe /+@nogc+/ unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : AllDummyRanges, DummyRange, Length,
        RangeType, ReturnBy;

    static immutable cheatsheet = [6, 7, 8, 9, 10];

    foreach (R; AllDummyRanges)
    {
        static if (isInputRange!R && !isInfinite!R &&
                   (hasLength!R || isForwardRange!R))
        {
            assert(R.init.tail(5).equal(cheatsheet));
            static assert(R.init.tail(5).equal(cheatsheet));

            assert(R.init.tail(0).length == 0);
            assert(R.init.tail(10).equal(R.init));
            assert(R.init.tail(11).equal(R.init));
        }
    }

    // Infinite ranges are not supported
    static assert(!__traits(compiles, repeat(0).tail(0)));

    // Neither are non-forward ranges without length
    static assert(!__traits(compiles, DummyRange!(ReturnBy.Value, Length.No,
        RangeType.Input).init.tail(5)));
}

pure @safe nothrow @nogc unittest
{
    static immutable input = [1, 2, 3];
    static immutable expectedOutput = [2, 3];
    assert(input.tail(2) == expectedOutput);
}

/++
    Convenience function which calls
    $(REF popFrontN, std, _range, primitives)`(range, n)` and returns `range`.
    `drop` makes it easier to pop elements from a range
    and then pass it to another function within a single expression,
    whereas `popFrontN` would require multiple statements.

    `dropBack` provides the same functionality but instead calls
    $(REF popBackN, std, _range, primitives)`(range, n)`

    Note: `drop` and `dropBack` will only pop $(I up to)
    `n` elements but will stop if the range is empty first.
    In other languages this is sometimes called `skip`.

    Params:
        range = the input range to drop from
        n = the number of elements to drop

    Returns:
        `range` with up to `n` elements dropped

    See_Also:
        $(REF popFront, std, _range, primitives), $(REF popBackN, std, _range, primitives)
  +/
R drop(R)(R range, size_t n)
if (isInputRange!R)
{
    range.popFrontN(n);
    return range;
}
/// ditto
R dropBack(R)(R range, size_t n)
if (isBidirectionalRange!R)
{
    range.popBackN(n);
    return range;
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;

    assert([0, 2, 1, 5, 0, 3].drop(3) == [5, 0, 3]);
    assert("hello world".drop(6) == "world");
    assert("hello world".drop(50).empty);
    assert("hello world".take(6).drop(3).equal("lo "));
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    assert([0, 2, 1, 5, 0, 3].dropBack(3) == [0, 2, 1]);
    assert("hello world".dropBack(6) == "hello");
    assert("hello world".dropBack(50).empty);
    assert("hello world".drop(4).dropBack(4).equal("o w"));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.container.dlist : DList;

    //Remove all but the first two elements
    auto a = DList!int(0, 1, 9, 9, 9, 9);
    a.remove(a[].drop(2));
    assert(a[].equal(a[].take(2)));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter;

    assert(drop("", 5).empty);
    assert(equal(drop(filter!"true"([0, 2, 1, 5, 0, 3]), 3), [5, 0, 3]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.container.dlist : DList;

    //insert before the last two elements
    auto a = DList!int(0, 1, 2, 5, 6);
    a.insertAfter(a[].dropBack(2), [3, 4]);
    assert(a[].equal(iota(0, 7)));
}

/++
    Similar to $(LREF drop) and $(D dropBack) but they call
    $(D range.$(LREF popFrontExactly)(n)) and $(D range.popBackExactly(n))
    instead.

    Note: Unlike $(D drop), $(D dropExactly) will assume that the
    range holds at least $(D n) elements. This makes $(D dropExactly)
    faster than $(D drop), but it also means that if $(D range) does
    not contain at least $(D n) elements, it will attempt to call $(D popFront)
    on an empty range, which is undefined behavior. So, only use
    $(D popFrontExactly) when it is guaranteed that $(D range) holds at least
    $(D n) elements.

    Params:
        range = the input range to drop from
        n = the number of elements to drop

    Returns:
        `range` with `n` elements dropped

    See_Also:
        $(REF popFrontExcatly, std, _range, primitives),
        $(REF popBackExcatly, std, _range, primitives)
+/
R dropExactly(R)(R range, size_t n)
if (isInputRange!R)
{
    popFrontExactly(range, n);
    return range;
}
/// ditto
R dropBackExactly(R)(R range, size_t n)
if (isBidirectionalRange!R)
{
    popBackExactly(range, n);
    return range;
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filterBidirectional;

    auto a = [1, 2, 3];
    assert(a.dropExactly(2) == [3]);
    assert(a.dropBackExactly(2) == [1]);

    string s = "日本語";
    assert(s.dropExactly(2) == "語");
    assert(s.dropBackExactly(2) == "日");

    auto bd = filterBidirectional!"true"([1, 2, 3]);
    assert(bd.dropExactly(2).equal([3]));
    assert(bd.dropBackExactly(2).equal([1]));
}

/++
    Convenience function which calls
    $(D range.popFront()) and returns $(D range). $(D dropOne)
    makes it easier to pop an element from a range
    and then pass it to another function within a single expression,
    whereas $(D popFront) would require multiple statements.

    $(D dropBackOne) provides the same functionality but instead calls
    $(D range.popBack()).
+/
R dropOne(R)(R range)
if (isInputRange!R)
{
    range.popFront();
    return range;
}
/// ditto
R dropBackOne(R)(R range)
if (isBidirectionalRange!R)
{
    range.popBack();
    return range;
}

///
pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filterBidirectional;
    import std.container.dlist : DList;

    auto dl = DList!int(9, 1, 2, 3, 9);
    assert(dl[].dropOne().dropBackOne().equal([1, 2, 3]));

    auto a = [1, 2, 3];
    assert(a.dropOne() == [2, 3]);
    assert(a.dropBackOne() == [1, 2]);

    string s = "日本語";
    import std.exception : assumeWontThrow;
    assert(assumeWontThrow(s.dropOne() == "本語"));
    assert(assumeWontThrow(s.dropBackOne() == "日本"));

    auto bd = filterBidirectional!"true"([1, 2, 3]);
    assert(bd.dropOne().equal([2, 3]));
    assert(bd.dropBackOne().equal([1, 2]));
}

/**
Create a range which repeats one value forever.

Params:
    value = the value to repeat

Returns:
    An infinite random access range with slicing.
*/
struct Repeat(T)
{
private:
    //Store a non-qualified T when possible: This is to make Repeat assignable
    static if ((is(T == class) || is(T == interface)) && (is(T == const) || is(T == immutable)))
    {
        import std.typecons : Rebindable;
        alias UT = Rebindable!T;
    }
    else static if (is(T : Unqual!T) && is(Unqual!T : T))
        alias UT = Unqual!T;
    else
        alias UT = T;
    UT _value;

public:
    /// Range primitives
    @property inout(T) front() inout { return _value; }

    /// ditto
    @property inout(T) back() inout { return _value; }

    /// ditto
    enum bool empty = false;

    /// ditto
    void popFront() {}

    /// ditto
    void popBack() {}

    /// ditto
    @property auto save() inout { return this; }

    /// ditto
    inout(T) opIndex(size_t) inout { return _value; }

    /// ditto
    auto opSlice(size_t i, size_t j)
    in
    {
        assert(
            i <= j,
            "Attempting to slice a Repeat with a larger first argument than the second."
        );
    }
    body
    {
        return this.takeExactly(j - i);
    }
    private static struct DollarToken {}

    /// ditto
    enum opDollar = DollarToken.init;

    /// ditto
    auto opSlice(size_t, DollarToken) inout { return this; }
}

/// Ditto
Repeat!T repeat(T)(T value) { return Repeat!T(value); }

///
pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;

    assert(equal(5.repeat().take(4), [ 5, 5, 5, 5 ]));
}

pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;

    auto  r = repeat(5);
    alias R = typeof(r);
    static assert(isBidirectionalRange!R);
    static assert(isForwardRange!R);
    static assert(isInfinite!R);
    static assert(hasSlicing!R);

    assert(r.back == 5);
    assert(r.front == 5);
    assert(r.take(4).equal([ 5, 5, 5, 5 ]));
    assert(r[0 .. 4].equal([ 5, 5, 5, 5 ]));

    R r2 = r[5 .. $];
    assert(r2.back == 5);
    assert(r2.front == 5);
}

/**
   Repeats $(D value) exactly $(D n) times. Equivalent to $(D
   take(repeat(value), n)).
*/
Take!(Repeat!T) repeat(T)(T value, size_t n)
{
    return take(repeat(value), n);
}

///
pure @safe nothrow @nogc unittest
{
    import std.algorithm.comparison : equal;

    assert(equal(5.repeat(4), 5.repeat().take(4)));
}

pure @safe nothrow unittest //12007
{
    static class C{}
    Repeat!(immutable int) ri;
    ri = ri.save;
    Repeat!(immutable C) rc;
    rc = rc.save;

    import std.algorithm.setops : cartesianProduct;
    import std.algorithm.comparison : equal;
    import std.typecons : tuple;
    immutable int[] A = [1,2,3];
    immutable int[] B = [4,5,6];

    assert(equal(cartesianProduct(A,B),
        [
            tuple(1, 4), tuple(1, 5), tuple(1, 6),
            tuple(2, 4), tuple(2, 5), tuple(2, 6),
            tuple(3, 4), tuple(3, 5), tuple(3, 6),
        ]));
}

/**
Given callable ($(REF isCallable, std,traits)) `fun`, create as a range
whose front is defined by successive calls to `fun()`.
This is especially useful to call function with global side effects (random
functions), or to create ranges expressed as a single delegate, rather than
an entire `front`/`popFront`/`empty` structure.
`fun` maybe be passed either a template alias parameter (existing
function, delegate, struct type defining `static opCall`) or
a run-time value argument (delegate, function object).
The result range models an InputRange
($(REF isInputRange, std,range,primitives)).
The resulting range will call `fun()` on construction, and every call to
`popFront`, and the cached value will be returned when `front` is called.

Returns: an `inputRange` where each element represents another call to fun.
*/
auto generate(Fun)(Fun fun)
if (isCallable!fun)
{
    auto gen = Generator!(Fun)(fun);
    gen.popFront(); // prime the first element
    return gen;
}

/// ditto
auto generate(alias fun)()
if (isCallable!fun)
{
    auto gen = Generator!(fun)();
    gen.popFront(); // prime the first element
    return gen;
}

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;

    int i = 1;
    auto powersOfTwo = generate!(() => i *= 2)().take(10);
    assert(equal(powersOfTwo, iota(1, 11).map!"2^^a"()));
}

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    //Returns a run-time delegate
    auto infiniteIota(T)(T low, T high)
    {
        T i = high;
        return (){if (i == high) i = low; return i++;};
    }
    //adapted as a range.
    assert(equal(generate(infiniteIota(1, 4)).take(10), [1, 2, 3, 1, 2, 3, 1, 2, 3, 1]));
}

///
@safe unittest
{
    import std.format : format;
    import std.random : uniform;

    auto r = generate!(() => uniform(0, 6)).take(10);
    format("%(%s %)", r);
}

private struct Generator(Fun...)
{
    static assert(Fun.length == 1);
    static assert(isInputRange!Generator);

private:
    static if (is(Fun[0]))
        Fun[0] fun;
    else
        alias fun = Fun[0];

    enum returnByRef_ = (functionAttributes!fun & FunctionAttribute.ref_) ? true : false;
    static if (returnByRef_)
        ReturnType!fun *elem_;
    else
        ReturnType!fun elem_;
public:
    /// Range primitives
    enum empty = false;

    static if (returnByRef_)
    {
        /// ditto
        ref front() @property
        {
            return *elem_;
        }
        /// ditto
        void popFront()
        {
            elem_ = &fun();
        }
    }
    else
    {
        /// ditto
        auto front() @property
        {
            return elem_;
        }
        /// ditto
        void popFront()
        {
            elem_ = fun();
        }
    }
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    struct StaticOpCall
    {
        static ubyte opCall() { return 5 ; }
    }

    assert(equal(generate!StaticOpCall().take(10), repeat(5).take(10)));
}

@safe pure unittest
{
    import std.algorithm.comparison : equal;

    struct OpCall
    {
        ubyte opCall() @safe pure { return 5 ; }
    }

    OpCall op;
    assert(equal(generate(op).take(10), repeat(5).take(10)));
}

// verify ref mechanism works
@system unittest
{
    int[10] arr;
    int idx;

    ref int fun() {
        auto x = idx++;
        idx %= arr.length;
        return arr[x];
    }
    int y = 1;
    foreach (ref x; generate!(fun).take(20))
    {
        x += y++;
    }
    import std.algorithm.comparison : equal;
    assert(equal(arr[], iota(12, 32, 2)));
}

// assure front isn't the mechanism to make generate go to the next element.
@safe unittest
{
    int i;
    auto g = generate!(() => ++i);
    auto f = g.front;
    assert(f == g.front);
    g = g.drop(5); // reassign because generate caches
    assert(g.front == f + 5);
}

/**
Repeats the given forward range ad infinitum. If the original range is
infinite (fact that would make $(D Cycle) the identity application),
$(D Cycle) detects that and aliases itself to the range type
itself. That works for non-forward ranges too.
If the original range has random access, $(D Cycle) offers
random access and also offers a constructor taking an initial position
$(D index). $(D Cycle) works with static arrays in addition to ranges,
mostly for performance reasons.

Note: The input range must not be empty.

Tip: This is a great way to implement simple circular buffers.
*/
struct Cycle(R)
if (isForwardRange!R && !isInfinite!R)
{
    static if (isRandomAccessRange!R && hasLength!R)
    {
        private R _original;
        private size_t _index;

        /// Range primitives
        this(R input, size_t index = 0)
        {
            _original = input;
            _index = index % _original.length;
        }

        /// ditto
        @property auto ref front()
        {
            return _original[_index];
        }

        static if (is(typeof((cast(const R)_original)[_index])))
        {
            /// ditto
            @property auto ref front() const
            {
                return _original[_index];
            }
        }

        static if (hasAssignableElements!R)
        {
            /// ditto
            @property void front(ElementType!R val)
            {
                _original[_index] = val;
            }
        }

        /// ditto
        enum bool empty = false;

        /// ditto
        void popFront()
        {
            ++_index;
            if (_index >= _original.length)
                _index = 0;
        }

        /// ditto
        auto ref opIndex(size_t n)
        {
            return _original[(n + _index) % _original.length];
        }

        static if (is(typeof((cast(const R)_original)[_index])) &&
                   is(typeof((cast(const R)_original).length)))
        {
            /// ditto
            auto ref opIndex(size_t n) const
            {
                return _original[(n + _index) % _original.length];
            }
        }

        static if (hasAssignableElements!R)
        {
            /// ditto
            void opIndexAssign(ElementType!R val, size_t n)
            {
                _original[(n + _index) % _original.length] = val;
            }
        }

        /// ditto
        @property Cycle save()
        {
            //No need to call _original.save, because Cycle never actually modifies _original
            return Cycle(_original, _index);
        }

        private static struct DollarToken {}

        /// ditto
        enum opDollar = DollarToken.init;

        static if (hasSlicing!R)
        {
            /// ditto
            auto opSlice(size_t i, size_t j)
            in
            {
                assert(i <= j);
            }
            body
            {
                return this[i .. $].takeExactly(j - i);
            }

            /// ditto
            auto opSlice(size_t i, DollarToken)
            {
                return typeof(this)(_original, _index + i);
            }
        }
    }
    else
    {
        private R _original;
        private R _current;

        /// ditto
        this(R input)
        {
            _original = input;
            _current = input.save;
        }

        /// ditto
        @property auto ref front()
        {
            return _current.front;
        }

        static if (is(typeof((cast(const R)_current).front)))
        {
            /// ditto
            @property auto ref front() const
            {
                return _current.front;
            }
        }

        static if (hasAssignableElements!R)
        {
            /// ditto
            @property auto front(ElementType!R val)
            {
                return _current.front = val;
            }
        }

        /// ditto
        enum bool empty = false;

        /// ditto
        void popFront()
        {
            _current.popFront();
            if (_current.empty)
                _current = _original.save;
        }

        /// ditto
        @property Cycle save()
        {
            //No need to call _original.save, because Cycle never actually modifies _original
            Cycle ret = this;
            ret._original = _original;
            ret._current =  _current.save;
            return ret;
        }
    }
}

/// ditto
template Cycle(R)
if (isInfinite!R)
{
    alias Cycle = R;
}

///
struct Cycle(R)
if (isStaticArray!R)
{
    private alias ElementType = typeof(R.init[0]);
    private ElementType* _ptr;
    private size_t _index;

nothrow:

    /// Range primitives
    this(ref R input, size_t index = 0) @system
    {
        _ptr = input.ptr;
        _index = index % R.length;
    }

    /// ditto
    @property ref inout(ElementType) front() inout @safe
    {
        static ref auto trustedPtrIdx(typeof(_ptr) p, size_t idx) @trusted
        {
            return p[idx];
        }
        return trustedPtrIdx(_ptr, _index);
    }

    /// ditto
    enum bool empty = false;

    /// ditto
    void popFront() @safe
    {
        ++_index;
        if (_index >= R.length)
            _index = 0;
    }

    /// ditto
    ref inout(ElementType) opIndex(size_t n) inout @safe
    {
        static ref auto trustedPtrIdx(typeof(_ptr) p, size_t idx) @trusted
        {
            return p[idx % R.length];
        }
        return trustedPtrIdx(_ptr, n + _index);
    }

    /// ditto
    @property inout(Cycle) save() inout @safe
    {
        return this;
    }

    private static struct DollarToken {}
    /// ditto
    enum opDollar = DollarToken.init;

    /// ditto
    auto opSlice(size_t i, size_t j) @safe
    in
    {
        assert(
            i <= j,
            "Attempting to slice a Repeat with a larger first argument than the second."
        );
    }
    body
    {
        return this[i .. $].takeExactly(j - i);
    }

    /// ditto
    inout(typeof(this)) opSlice(size_t i, DollarToken) inout @safe
    {
        static auto trustedCtor(typeof(_ptr) p, size_t idx) @trusted
        {
            return cast(inout) Cycle(*cast(R*)(p), idx);
        }
        return trustedCtor(_ptr, _index + i);
    }
}

/// Ditto
auto cycle(R)(R input)
if (isInputRange!R)
{
    static assert(isForwardRange!R || isInfinite!R,
        "Cycle requires a forward range argument unless it's statically known"
         ~ " to be infinite");
    assert(!input.empty, "Attempting to pass an empty input to cycle");
    static if (isInfinite!R) return input;
    else return Cycle!R(input);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : cycle, take;

    // Here we create an infinitive cyclic sequence from [1, 2]
    // (i.e. get here [1, 2, 1, 2, 1, 2 and so on]) then
    // take 5 elements of this sequence (so we have [1, 2, 1, 2, 1])
    // and compare them with the expected values for equality.
    assert(cycle([1, 2]).take(5).equal([ 1, 2, 1, 2, 1 ]));
}

/// Ditto
Cycle!R cycle(R)(R input, size_t index = 0)
if (isRandomAccessRange!R && !isInfinite!R)
{
    assert(!input.empty, "Attempting to pass an empty input to cycle");
    return Cycle!R(input, index);
}

/// Ditto
Cycle!R cycle(R)(ref R input, size_t index = 0) @system
if (isStaticArray!R)
{
    return Cycle!R(input, index);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : AllDummyRanges;

    static assert(isForwardRange!(Cycle!(uint[])));

    // Make sure ref is getting propagated properly.
    int[] nums = [1,2,3];
    auto c2 = cycle(nums);
    c2[3]++;
    assert(nums[0] == 2);

    immutable int[] immarr = [1, 2, 3];

    foreach (DummyType; AllDummyRanges)
    {
        static if (isForwardRange!DummyType)
        {
            DummyType dummy;
            auto cy = cycle(dummy);
            static assert(isForwardRange!(typeof(cy)));
            auto t = take(cy, 20);
            assert(equal(t, [1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10]));

            const cRange = cy;
            assert(cRange.front == 1);

            static if (hasAssignableElements!DummyType)
            {
                {
                    cy.front = 66;
                    scope(exit) cy.front = 1;
                    assert(dummy.front == 66);
                }

                static if (isRandomAccessRange!DummyType)
                {
                    {
                        cy[10] = 66;
                        scope(exit) cy[10] = 1;
                        assert(dummy.front == 66);
                    }

                    assert(cRange[10] == 1);
                }
            }

            static if (hasSlicing!DummyType)
            {
                auto slice = cy[5 .. 15];
                assert(equal(slice, [6, 7, 8, 9, 10, 1, 2, 3, 4, 5]));
                static assert(is(typeof(slice) == typeof(takeExactly(cy, 5))));

                auto infSlice = cy[7 .. $];
                assert(equal(take(infSlice, 5), [8, 9, 10, 1, 2]));
                static assert(isInfinite!(typeof(infSlice)));
            }
        }
    }
}

@system unittest // For static arrays.
{
    import std.algorithm.comparison : equal;

    int[3] a = [ 1, 2, 3 ];
    static assert(isStaticArray!(typeof(a)));
    auto c = cycle(a);
    assert(a.ptr == c._ptr);
    assert(equal(take(cycle(a), 5), [ 1, 2, 3, 1, 2 ][]));
    static assert(isForwardRange!(typeof(c)));

    // Test qualifiers on slicing.
    alias C = typeof(c);
    static assert(is(typeof(c[1 .. $]) == C));
    const cConst = c;
    static assert(is(typeof(cConst[1 .. $]) == const(C)));
}

@safe unittest // For infinite ranges
{
    struct InfRange
    {
        void popFront() { }
        @property int front() { return 0; }
        enum empty = false;
        auto save() { return this; }
    }
    struct NonForwardInfRange
    {
        void popFront() { }
        @property int front() { return 0; }
        enum empty = false;
    }

    InfRange i;
    NonForwardInfRange j;
    auto c = cycle(i);
    assert(c == i);
    //make sure it can alias out even non-forward infinite ranges
    static assert(is(typeof(j.cycle) == typeof(j)));
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    int[5] arr = [0, 1, 2, 3, 4];
    auto cleD = cycle(arr[]); //Dynamic
    assert(equal(cleD[5 .. 10], arr[]));

    //n is a multiple of 5 worth about 3/4 of size_t.max
    auto n = size_t.max/4 + size_t.max/2;
    n -= n % 5;

    //Test index overflow
    foreach (_ ; 0 .. 10)
    {
        cleD = cleD[n .. $];
        assert(equal(cleD[5 .. 10], arr[]));
    }
}

@system unittest
{
    import std.algorithm.comparison : equal;

    int[5] arr = [0, 1, 2, 3, 4];
    auto cleS = cycle(arr);   //Static
    assert(equal(cleS[5 .. 10], arr[]));

    //n is a multiple of 5 worth about 3/4 of size_t.max
    auto n = size_t.max/4 + size_t.max/2;
    n -= n % 5;

    //Test index overflow
    foreach (_ ; 0 .. 10)
    {
        cleS = cleS[n .. $];
        assert(equal(cleS[5 .. 10], arr[]));
    }
}

@system unittest
{
    import std.algorithm.comparison : equal;

    int[1] arr = [0];
    auto cleS = cycle(arr);
    cleS = cleS[10 .. $];
    assert(equal(cleS[5 .. 10], 0.repeat(5)));
    assert(cleS.front == 0);
}

@system unittest //10845
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter;

    auto a = inputRangeObject(iota(3).filter!"true");
    assert(equal(cycle(a).take(10), [0, 1, 2, 0, 1, 2, 0, 1, 2, 0]));
}

@safe unittest // 12177
{
    static assert(__traits(compiles, recurrence!q{a[n - 1] ~ a[n - 2]}("1", "0")));
}

// Issue 13390
@system unittest
{
    import core.exception : AssertError;
    import std.exception : assertThrown;
    assertThrown!AssertError(cycle([0, 1, 2][0 .. 0]));
}

private alias lengthType(R) = typeof(R.init.length.init);

/**
   Iterate several ranges in lockstep. The element type is a proxy tuple
   that allows accessing the current element in the $(D n)th range by
   using $(D e[n]).

   `zip` is similar to $(LREF lockstep), but `lockstep` doesn't
   bundle its elements and uses the `opApply` protocol.
   `lockstep` allows reference access to the elements in
   `foreach` iterations.

    Params:
        sp = controls what `zip` will do if the _ranges are different lengths
        ranges = the ranges to zip together
    Returns:
        At minimum, an input range. `Zip` offers the lowest range facilities
        of all components, e.g. it offers random access iff all ranges offer
        random access, and also offers mutation and swapping if all ranges offer
        it. Due to this, `Zip` is extremely powerful because it allows manipulating
        several ranges in lockstep.
    Throws:
        An `Exception` if all of the _ranges are not the same length and
        `sp` is set to `StoppingPolicy.requireSameLength`.
*/
struct Zip(Ranges...)
if (Ranges.length && allSatisfy!(isInputRange, Ranges))
{
    import std.format : format; //for generic mixins
    import std.typecons : Tuple;

    alias R = Ranges;
    private R ranges;
    alias ElementType = Tuple!(staticMap!(.ElementType, R));
    private StoppingPolicy stoppingPolicy = StoppingPolicy.shortest;

/**
   Builds an object. Usually this is invoked indirectly by using the
   $(LREF zip) function.
 */
    this(R rs, StoppingPolicy s = StoppingPolicy.shortest)
    {
        ranges[] = rs[];
        stoppingPolicy = s;
    }

/**
   Returns $(D true) if the range is at end. The test depends on the
   stopping policy.
*/
    static if (allSatisfy!(isInfinite, R))
    {
        // BUG:  Doesn't propagate infiniteness if only some ranges are infinite
        //       and s == StoppingPolicy.longest.  This isn't fixable in the
        //       current design since StoppingPolicy is known only at runtime.
        enum bool empty = false;
    }
    else
    {
        ///
        @property bool empty()
        {
            import std.exception : enforce;
            import std.meta : anySatisfy;

            final switch (stoppingPolicy)
            {
            case StoppingPolicy.shortest:
                foreach (i, Unused; R)
                {
                    if (ranges[i].empty) return true;
                }
                return false;
            case StoppingPolicy.longest:
                static if (anySatisfy!(isInfinite, R))
                {
                    return false;
                }
                else
                {
                    foreach (i, Unused; R)
                    {
                        if (!ranges[i].empty) return false;
                    }
                    return true;
                }
            case StoppingPolicy.requireSameLength:
                foreach (i, Unused; R[1 .. $])
                {
                    enforce(ranges[0].empty ==
                            ranges[i + 1].empty,
                            "Inequal-length ranges passed to Zip");
                }
                return ranges[0].empty;
            }
            assert(false);
        }
    }

    static if (allSatisfy!(isForwardRange, R))
    {
        ///
        @property Zip save()
        {
            //Zip(ranges[0].save, ranges[1].save, ..., stoppingPolicy)
            return mixin (q{Zip(%(ranges[%s].save%|, %), stoppingPolicy)}.format(iota(0, R.length)));
        }
    }

    private .ElementType!(R[i]) tryGetInit(size_t i)()
    {
        alias E = .ElementType!(R[i]);
        static if (!is(typeof({static E i;})))
            throw new Exception("Range with non-default constructable elements exhausted.");
        else
            return E.init;
    }

/**
   Returns the current iterated element.
*/
    @property ElementType front()
    {
        @property tryGetFront(size_t i)(){return ranges[i].empty ? tryGetInit!i() : ranges[i].front;}
        //ElementType(tryGetFront!0, tryGetFront!1, ...)
        return mixin(q{ElementType(%(tryGetFront!%s, %))}.format(iota(0, R.length)));
    }

/**
   Sets the front of all iterated ranges.
*/
    static if (allSatisfy!(hasAssignableElements, R))
    {
        @property void front(ElementType v)
        {
            foreach (i, Unused; R)
            {
                if (!ranges[i].empty)
                {
                    ranges[i].front = v[i];
                }
            }
        }
    }

/**
   Moves out the front.
*/
    static if (allSatisfy!(hasMobileElements, R))
    {
        ElementType moveFront()
        {
            @property tryMoveFront(size_t i)(){return ranges[i].empty ? tryGetInit!i() : ranges[i].moveFront();}
            //ElementType(tryMoveFront!0, tryMoveFront!1, ...)
            return mixin(q{ElementType(%(tryMoveFront!%s, %))}.format(iota(0, R.length)));
        }
    }

/**
   Returns the rightmost element.
*/
    static if (allSatisfy!(isBidirectionalRange, R))
    {
        @property ElementType back()
        {
            //TODO: Fixme! BackElement != back of all ranges in case of jagged-ness

            @property tryGetBack(size_t i)(){return ranges[i].empty ? tryGetInit!i() : ranges[i].back;}
            //ElementType(tryGetBack!0, tryGetBack!1, ...)
            return mixin(q{ElementType(%(tryGetBack!%s, %))}.format(iota(0, R.length)));
        }

/**
   Moves out the back.
*/
        static if (allSatisfy!(hasMobileElements, R))
        {
            ElementType moveBack()
            {
                //TODO: Fixme! BackElement != back of all ranges in case of jagged-ness

                @property tryMoveBack(size_t i)(){return ranges[i].empty ? tryGetInit!i() : ranges[i].moveFront();}
                //ElementType(tryMoveBack!0, tryMoveBack!1, ...)
                return mixin(q{ElementType(%(tryMoveBack!%s, %))}.format(iota(0, R.length)));
            }
        }

/**
   Returns the current iterated element.
*/
        static if (allSatisfy!(hasAssignableElements, R))
        {
            @property void back(ElementType v)
            {
                //TODO: Fixme! BackElement != back of all ranges in case of jagged-ness.
                //Not sure the call is even legal for StoppingPolicy.longest

                foreach (i, Unused; R)
                {
                    if (!ranges[i].empty)
                    {
                        ranges[i].back = v[i];
                    }
                }
            }
        }
    }

/**
   Advances to the next element in all controlled ranges.
*/
    void popFront()
    {
        import std.exception : enforce;

        final switch (stoppingPolicy)
        {
        case StoppingPolicy.shortest:
            foreach (i, Unused; R)
            {
                assert(!ranges[i].empty);
                ranges[i].popFront();
            }
            break;
        case StoppingPolicy.longest:
            foreach (i, Unused; R)
            {
                if (!ranges[i].empty) ranges[i].popFront();
            }
            break;
        case StoppingPolicy.requireSameLength:
            foreach (i, Unused; R)
            {
                enforce(!ranges[i].empty, "Invalid Zip object");
                ranges[i].popFront();
            }
            break;
        }
    }

/**
   Calls $(D popBack) for all controlled ranges.
*/
    static if (allSatisfy!(isBidirectionalRange, R))
    {
        void popBack()
        {
            //TODO: Fixme! In case of jaggedness, this is wrong.
            import std.exception : enforce;

            final switch (stoppingPolicy)
            {
            case StoppingPolicy.shortest:
                foreach (i, Unused; R)
                {
                    assert(!ranges[i].empty);
                    ranges[i].popBack();
                }
                break;
            case StoppingPolicy.longest:
                foreach (i, Unused; R)
                {
                    if (!ranges[i].empty) ranges[i].popBack();
                }
                break;
            case StoppingPolicy.requireSameLength:
                foreach (i, Unused; R)
                {
                    enforce(!ranges[i].empty, "Invalid Zip object");
                    ranges[i].popBack();
                }
                break;
            }
        }
    }

/**
   Returns the length of this range. Defined only if all ranges define
   $(D length).
*/
    static if (allSatisfy!(hasLength, R))
    {
        @property auto length()
        {
            static if (Ranges.length == 1)
                return ranges[0].length;
            else
            {
                if (stoppingPolicy == StoppingPolicy.requireSameLength)
                    return ranges[0].length;

                //[min|max](ranges[0].length, ranges[1].length, ...)
                import std.algorithm.comparison : min, max;
                if (stoppingPolicy == StoppingPolicy.shortest)
                    return mixin(q{min(%(ranges[%s].length%|, %))}.format(iota(0, R.length)));
                else
                    return mixin(q{max(%(ranges[%s].length%|, %))}.format(iota(0, R.length)));
            }
        }

        alias opDollar = length;
    }

/**
   Returns a slice of the range. Defined only if all range define
   slicing.
*/
    static if (allSatisfy!(hasSlicing, R))
    {
        auto opSlice(size_t from, size_t to)
        {
            //Slicing an infinite range yields the type Take!R
            //For finite ranges, the type Take!R aliases to R
            alias ZipResult = Zip!(staticMap!(Take, R));

            //ZipResult(ranges[0][from .. to], ranges[1][from .. to], ..., stoppingPolicy)
            return mixin (q{ZipResult(%(ranges[%s][from .. to]%|, %), stoppingPolicy)}.format(iota(0, R.length)));
        }
    }

/**
   Returns the $(D n)th element in the composite range. Defined if all
   ranges offer random access.
*/
    static if (allSatisfy!(isRandomAccessRange, R))
    {
        ElementType opIndex(size_t n)
        {
            //TODO: Fixme! This may create an out of bounds access
            //for StoppingPolicy.longest

            //ElementType(ranges[0][n], ranges[1][n], ...)
            return mixin (q{ElementType(%(ranges[%s][n]%|, %))}.format(iota(0, R.length)));
        }

/**
   Assigns to the $(D n)th element in the composite range. Defined if
   all ranges offer random access.
*/
        static if (allSatisfy!(hasAssignableElements, R))
        {
            void opIndexAssign(ElementType v, size_t n)
            {
                //TODO: Fixme! Not sure the call is even legal for StoppingPolicy.longest
                foreach (i, Range; R)
                {
                    ranges[i][n] = v[i];
                }
            }
        }

/**
   Destructively reads the $(D n)th element in the composite
   range. Defined if all ranges offer random access.
*/
        static if (allSatisfy!(hasMobileElements, R))
        {
            ElementType moveAt(size_t n)
            {
                //TODO: Fixme! This may create an out of bounds access
                //for StoppingPolicy.longest

                //ElementType(ranges[0].moveAt(n), ranges[1].moveAt(n), ..., )
                return mixin (q{ElementType(%(ranges[%s].moveAt(n)%|, %))}.format(iota(0, R.length)));
            }
        }
    }
}

/// Ditto
auto zip(Ranges...)(Ranges ranges)
if (Ranges.length && allSatisfy!(isInputRange, Ranges))
{
    return Zip!Ranges(ranges);
}

///
pure @safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;

    // pairwise sum
    auto arr = [0, 1, 2];
    assert(zip(arr, arr.dropOne).map!"a[0] + a[1]".equal([1, 3]));
}

///
pure @safe unittest
{
    import std.conv : to;

    int[] a = [ 1, 2, 3 ];
    string[] b = [ "a", "b", "c" ];
    string[] result;

    foreach (tup; zip(a, b))
    {
        result ~= tup[0].to!string ~ tup[1];
    }

    assert(result == [ "1a", "2b", "3c" ]);

    size_t idx = 0;
    // unpacking tuple elements with foreach
    foreach (e1, e2; zip(a, b))
    {
        assert(e1 == a[idx]);
        assert(e2 == b[idx]);
        ++idx;
    }
}

/// $(D zip) is powerful - the following code sorts two arrays in parallel:
pure @safe unittest
{
    import std.algorithm.sorting : sort;

    int[] a = [ 1, 2, 3 ];
    string[] b = [ "a", "c", "b" ];
    zip(a, b).sort!((t1, t2) => t1[0] > t2[0]);

    assert(a == [ 3, 2, 1 ]);
    // b is sorted according to a's sorting
    assert(b == [ "b", "c", "a" ]);
}

/// Ditto
auto zip(Ranges...)(StoppingPolicy sp, Ranges ranges)
if (Ranges.length && allSatisfy!(isInputRange, Ranges))
{
    return Zip!Ranges(ranges, sp);
}

/**
   Dictates how iteration in a $(D Zip) should stop. By default stop at
   the end of the shortest of all ranges.
*/
enum StoppingPolicy
{
    /// Stop when the shortest range is exhausted
    shortest,
    /// Stop when the longest range is exhausted
    longest,
    /// Require that all ranges are equal
    requireSameLength,
}

@system unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter, map;
    import std.algorithm.mutation : swap;
    import std.algorithm.sorting : sort;

    import std.exception : assertThrown, assertNotThrown;
    import std.typecons : tuple;

    int[] a = [ 1, 2, 3 ];
    float[] b = [ 1.0, 2.0, 3.0 ];
    foreach (e; zip(a, b))
    {
        assert(e[0] == e[1]);
    }

    swap(a[0], a[1]);
    auto z = zip(a, b);
    //swap(z.front(), z.back());
    sort!("a[0] < b[0]")(zip(a, b));
    assert(a == [1, 2, 3]);
    assert(b == [2.0, 1.0, 3.0]);

    z = zip(StoppingPolicy.requireSameLength, a, b);
    assertNotThrown(z.popBack());
    assertNotThrown(z.popBack());
    assertNotThrown(z.popBack());
    assert(z.empty);
    assertThrown(z.popBack());

    a = [ 1, 2, 3 ];
    b = [ 1.0, 2.0, 3.0 ];
    sort!("a[0] > b[0]")(zip(StoppingPolicy.requireSameLength, a, b));
    assert(a == [3, 2, 1]);
    assert(b == [3.0, 2.0, 1.0]);

    a = [];
    b = [];
    assert(zip(StoppingPolicy.requireSameLength, a, b).empty);

    // Test infiniteness propagation.
    static assert(isInfinite!(typeof(zip(repeat(1), repeat(1)))));

    // Test stopping policies with both value and reference.
    auto a1 = [1, 2];
    auto a2 = [1, 2, 3];
    auto stuff = tuple(tuple(a1, a2),
            tuple(filter!"a"(a1), filter!"a"(a2)));

    alias FOO = Zip!(immutable(int)[], immutable(float)[]);

    foreach (t; stuff.expand)
    {
        auto arr1 = t[0];
        auto arr2 = t[1];
        auto zShortest = zip(arr1, arr2);
        assert(equal(map!"a[0]"(zShortest), [1, 2]));
        assert(equal(map!"a[1]"(zShortest), [1, 2]));

        try {
            auto zSame = zip(StoppingPolicy.requireSameLength, arr1, arr2);
            foreach (elem; zSame) {}
            assert(0);
        } catch (Throwable) { /* It's supposed to throw.*/ }

        auto zLongest = zip(StoppingPolicy.longest, arr1, arr2);
        assert(!zLongest.ranges[0].empty);
        assert(!zLongest.ranges[1].empty);

        zLongest.popFront();
        zLongest.popFront();
        assert(!zLongest.empty);
        assert(zLongest.ranges[0].empty);
        assert(!zLongest.ranges[1].empty);

        zLongest.popFront();
        assert(zLongest.empty);
    }

    // BUG 8900
    assert(zip([1, 2], repeat('a')).array == [tuple(1, 'a'), tuple(2, 'a')]);
    assert(zip(repeat('a'), [1, 2]).array == [tuple('a', 1), tuple('a', 2)]);

    // Doesn't work yet.  Issues w/ emplace.
    // static assert(is(Zip!(immutable int[], immutable float[])));


    // These unittests pass, but make the compiler consume an absurd amount
    // of RAM and time.  Therefore, they should only be run if explicitly
    // uncommented when making changes to Zip.  Also, running them using
    // make -fwin32.mak unittest makes the compiler completely run out of RAM.
    // You need to test just this module.
    /+
     foreach (DummyType1; AllDummyRanges)
     {
         DummyType1 d1;
         foreach (DummyType2; AllDummyRanges)
         {
             DummyType2 d2;
             auto r = zip(d1, d2);
             assert(equal(map!"a[0]"(r), [1,2,3,4,5,6,7,8,9,10]));
             assert(equal(map!"a[1]"(r), [1,2,3,4,5,6,7,8,9,10]));

             static if (isForwardRange!DummyType1 && isForwardRange!DummyType2)
             {
                 static assert(isForwardRange!(typeof(r)));
             }

             static if (isBidirectionalRange!DummyType1 &&
                     isBidirectionalRange!DummyType2) {
                 static assert(isBidirectionalRange!(typeof(r)));
             }
             static if (isRandomAccessRange!DummyType1 &&
                     isRandomAccessRange!DummyType2) {
                 static assert(isRandomAccessRange!(typeof(r)));
             }
         }
     }
    +/
}

pure @safe unittest
{
    import std.algorithm.sorting : sort;

    auto a = [5,4,3,2,1];
    auto b = [3,1,2,5,6];
    auto z = zip(a, b);

    sort!"a[0] < b[0]"(z);

    assert(a == [1, 2, 3, 4, 5]);
    assert(b == [6, 5, 2, 1, 3]);
}

@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : tuple;

    auto LL = iota(1L, 1000L);
    auto z = zip(LL, [4]);

    assert(equal(z, [tuple(1L,4)]));

    auto LL2 = iota(0L, 500L);
    auto z2 = zip([7], LL2);
    assert(equal(z2, [tuple(7, 0L)]));
}

// Text for Issue 11196
@safe pure unittest
{
    import std.exception : assertThrown;

    static struct S { @disable this(); }
    assert(zip((S[5]).init[]).length == 5);
    assert(zip(StoppingPolicy.longest, cast(S[]) null, new int[1]).length == 1);
    assertThrown(zip(StoppingPolicy.longest, cast(S[]) null, new int[1]).front);
}

@safe pure unittest //12007
{
    static struct R
    {
        enum empty = false;
        void popFront(){}
        int front(){return 1;} @property
        R save(){return this;} @property
        void opAssign(R) @disable;
    }
    R r;
    auto z = zip(r, r);
    assert(z.save == z);
}

pure @system unittest
{
    import std.typecons : tuple;

    auto r1 = [0,1,2];
    auto r2 = [1,2,3];
    auto z1 = zip(refRange(&r1), refRange(&r2));
    auto z2 = z1.save;
    z1.popFront();
    assert(z1.front == tuple(1,2));
    assert(z2.front == tuple(0,1));
}

/*
    Generate lockstep's opApply function as a mixin string.
    If withIndex is true prepend a size_t index to the delegate.
*/
private string lockstepMixin(Ranges...)(bool withIndex, bool reverse)
{
    import std.format : format;

    string[] params;
    string[] emptyChecks;
    string[] dgArgs;
    string[] popFronts;
    string indexDef;
    string indexInc;

    if (withIndex)
    {
        params ~= "size_t";
        dgArgs ~= "index";
        if (reverse)
        {
            indexDef = q{
                size_t index = ranges[0].length-1;
                enforce(_stoppingPolicy == StoppingPolicy.requireSameLength,
                        "lockstep can only be used with foreach_reverse when stoppingPolicy == requireSameLength");

                foreach (range; ranges[1..$])
                    enforce(range.length == ranges[0].length);
                };
            indexInc = "--index;";
        }
        else
        {
            indexDef = "size_t index = 0;";
            indexInc = "++index;";
        }
    }

    foreach (idx, Range; Ranges)
    {
        params ~= format("%sElementType!(Ranges[%s])", hasLvalueElements!Range ? "ref " : "", idx);
        emptyChecks ~= format("!ranges[%s].empty", idx);
        if (reverse)
        {
            dgArgs ~= format("ranges[%s].back", idx);
            popFronts ~= format("ranges[%s].popBack();", idx);
        }
        else
        {
            dgArgs ~= format("ranges[%s].front", idx);
            popFronts ~= format("ranges[%s].popFront();", idx);
        }
    }

    string name = reverse ? "opApplyReverse" : "opApply";

    return format(
    q{
        int %s(scope int delegate(%s) dg)
        {
            import std.exception : enforce;

            auto ranges = _ranges;
            int res;
            %s

            while (%s)
            {
                res = dg(%s);
                if (res) break;
                %s
                %s
            }

            if (_stoppingPolicy == StoppingPolicy.requireSameLength)
            {
                foreach (range; ranges)
                    enforce(range.empty);
            }
            return res;
        }
    }, name, params.join(", "), indexDef,
       emptyChecks.join(" && "), dgArgs.join(", "),
       popFronts.join("\n                "),
       indexInc);
}

/**
   Iterate multiple ranges in lockstep using a $(D foreach) loop. In contrast to
   $(LREF zip) it allows reference access to its elements. If only a single
   range is passed in, the $(D Lockstep) aliases itself away.  If the
   ranges are of different lengths and $(D s) == $(D StoppingPolicy.shortest)
   stop after the shortest range is empty.  If the ranges are of different
   lengths and $(D s) == $(D StoppingPolicy.requireSameLength), throw an
   exception.  $(D s) may not be $(D StoppingPolicy.longest), and passing this
   will throw an exception.

   Iterating over $(D Lockstep) in reverse and with an index is only possible
   when $(D s) == $(D StoppingPolicy.requireSameLength), in order to preserve
   indexes. If an attempt is made at iterating in reverse when $(D s) ==
   $(D StoppingPolicy.shortest), an exception will be thrown.

   By default $(D StoppingPolicy) is set to $(D StoppingPolicy.shortest).

   See_Also: $(LREF zip)

       `lockstep` is similar to $(LREF zip), but `zip` bundles its
       elements and returns a range.
       `lockstep` also supports reference access.
       Use `zip` if you want to pass the result to a range function.
*/
struct Lockstep(Ranges...)
if (Ranges.length > 1 && allSatisfy!(isInputRange, Ranges))
{
    ///
    this(R ranges, StoppingPolicy sp = StoppingPolicy.shortest)
    {
        import std.exception : enforce;

        _ranges = ranges;
        enforce(sp != StoppingPolicy.longest,
                "Can't use StoppingPolicy.Longest on Lockstep.");
        _stoppingPolicy = sp;
    }

    mixin(lockstepMixin!Ranges(false, false));
    mixin(lockstepMixin!Ranges(true, false));
    static if (allSatisfy!(isBidirectionalRange, Ranges))
    {
        mixin(lockstepMixin!Ranges(false, true));
        static if (allSatisfy!(hasLength, Ranges))
        {
            mixin(lockstepMixin!Ranges(true, true));
        }
        else
        {
            mixin(lockstepReverseFailMixin!Ranges(true));
        }
    }
    else
    {
        mixin(lockstepReverseFailMixin!Ranges(false));
        mixin(lockstepReverseFailMixin!Ranges(true));
    }

private:
    alias R = Ranges;
    R _ranges;
    StoppingPolicy _stoppingPolicy;
}

string lockstepReverseFailMixin(Ranges...)(bool withIndex)
{
    import std.format : format;
    string[] params;
    string message;

    if (withIndex)
    {
        message = "Indexed reverse iteration with lockstep is only supported"
        ~"if all ranges are bidirectional and have a length.\n";
    }
    else
    {
        message = "Reverse iteration with lockstep is only supported if all ranges are bidirectional.\n";
    }

    if (withIndex)
    {
        params ~= "size_t";
    }

    foreach (idx, Range; Ranges)
    {
        params ~= format("%sElementType!(Ranges[%s])", hasLvalueElements!Range ? "ref " : "", idx);
    }

    return format(
    q{
        int opApplyReverse()(scope int delegate(%s) dg)
        {
            static assert(false, "%s");
        }
    }, params.join(", "), message);
}

// For generic programming, make sure Lockstep!(Range) is well defined for a
// single range.
template Lockstep(Range)
{
    alias Lockstep = Range;
}

/// Ditto
Lockstep!(Ranges) lockstep(Ranges...)(Ranges ranges)
if (allSatisfy!(isInputRange, Ranges))
{
    return Lockstep!(Ranges)(ranges);
}
/// Ditto
Lockstep!(Ranges) lockstep(Ranges...)(Ranges ranges, StoppingPolicy s)
if (allSatisfy!(isInputRange, Ranges))
{
    static if (Ranges.length > 1)
        return Lockstep!Ranges(ranges, s);
    else
        return ranges[0];
}

///
@system unittest
{
   auto arr1 = [1,2,3,4,5,100];
   auto arr2 = [6,7,8,9,10];

   foreach (ref a, b; lockstep(arr1, arr2))
   {
       a += b;
   }

   assert(arr1 == [7,9,11,13,15,100]);

   /// Lockstep also supports iterating with an index variable:
   foreach (index, a, b; lockstep(arr1, arr2))
   {
       assert(arr1[index] == a);
       assert(arr2[index] == b);
   }
}

@system unittest // Bugzilla 15860: foreach_reverse on lockstep
{
    auto arr1 = [0, 1, 2, 3];
    auto arr2 = [4, 5, 6, 7];

    size_t n = arr1.length -1;
    foreach_reverse (index, a, b; lockstep(arr1, arr2, StoppingPolicy.requireSameLength))
    {
        assert(n == index);
        assert(index == a);
        assert(arr1[index] == a);
        assert(arr2[index] == b);
        n--;
    }

    auto arr3 = [4, 5];
    n = 1;
    foreach_reverse (a, b; lockstep(arr1, arr3))
    {
        assert(a == arr1[$-n] && b == arr3[$-n]);
        n++;
    }
}

@system unittest
{
    import std.algorithm.iteration : filter;
    import std.conv : to;

    // The filters are to make these the lowest common forward denominator ranges,
    // i.e. w/o ref return, random access, length, etc.
    auto foo = filter!"a"([1,2,3,4,5]);
    immutable bar = [6f,7f,8f,9f,10f].idup;
    auto l = lockstep(foo, bar);

    // Should work twice.  These are forward ranges with implicit save.
    foreach (i; 0 .. 2)
    {
        uint[] res1;
        float[] res2;

        foreach (a, ref b; l)
        {
            res1 ~= a;
            res2 ~= b;
        }

        assert(res1 == [1,2,3,4,5]);
        assert(res2 == [6,7,8,9,10]);
        assert(bar == [6f,7f,8f,9f,10f]);
    }

    // Doc example.
    auto arr1 = [1,2,3,4,5];
    auto arr2 = [6,7,8,9,10];

    foreach (ref a, ref b; lockstep(arr1, arr2))
    {
        a += b;
    }

    assert(arr1 == [7,9,11,13,15]);

    // Make sure StoppingPolicy.requireSameLength doesn't throw.
    auto ls = lockstep(arr1, arr2, StoppingPolicy.requireSameLength);

    int k = 1;
    foreach (a, b; ls)
    {
        assert(a - b == k);
        ++k;
    }

    // Make sure StoppingPolicy.requireSameLength throws.
    arr2.popBack();
    ls = lockstep(arr1, arr2, StoppingPolicy.requireSameLength);

    try {
        foreach (a, b; ls) {}
        assert(0);
    } catch (Exception) {}

    // Just make sure 1-range case instantiates.  This hangs the compiler
    // when no explicit stopping policy is specified due to Bug 4652.
    auto stuff = lockstep([1,2,3,4,5], StoppingPolicy.shortest);
    foreach (i, a; stuff)
    {
        assert(stuff[i] == a);
    }

    // Test with indexing.
    uint[] res1;
    float[] res2;
    size_t[] indices;
    foreach (i, a, b; lockstep(foo, bar))
    {
        indices ~= i;
        res1 ~= a;
        res2 ~= b;
    }

    assert(indices == to!(size_t[])([0, 1, 2, 3, 4]));
    assert(res1 == [1,2,3,4,5]);
    assert(res2 == [6f,7f,8f,9f,10f]);

    // Make sure we've worked around the relevant compiler bugs and this at least
    // compiles w/ >2 ranges.
    lockstep(foo, foo, foo);

    // Make sure it works with const.
    const(int[])[] foo2 = [[1, 2, 3]];
    const(int[])[] bar2 = [[4, 5, 6]];
    auto c = chain(foo2, bar2);

    foreach (f, b; lockstep(c, c)) {}

    // Regression 10468
    foreach (x, y; lockstep(iota(0, 10), iota(0, 10))) { }
}

@system unittest
{
    struct RvalueRange
    {
        int[] impl;
        @property bool empty() { return impl.empty; }
        @property int front() { return impl[0]; } // N.B. non-ref
        void popFront() { impl.popFront(); }
    }
    auto data1 = [ 1, 2, 3, 4 ];
    auto data2 = [ 5, 6, 7, 8 ];
    auto r1 = RvalueRange(data1);
    auto r2 = data2;
    foreach (a, ref b; lockstep(r1, r2))
    {
        a++;
        b++;
    }
    assert(data1 == [ 1, 2, 3, 4 ]); // changes to a do not propagate to data
    assert(data2 == [ 6, 7, 8, 9 ]); // but changes to b do.

    // Since r1 is by-value only, the compiler should reject attempts to
    // foreach over it with ref.
    static assert(!__traits(compiles, {
        foreach (ref a, ref b; lockstep(r1, r2)) { a++; }
    }));
}

/**
Creates a mathematical sequence given the initial values and a
recurrence function that computes the next value from the existing
values. The sequence comes in the form of an infinite forward
range. The type $(D Recurrence) itself is seldom used directly; most
often, recurrences are obtained by calling the function $(D
recurrence).

When calling $(D recurrence), the function that computes the next
value is specified as a template argument, and the initial values in
the recurrence are passed as regular arguments. For example, in a
Fibonacci sequence, there are two initial values (and therefore a
state size of 2) because computing the next Fibonacci value needs the
past two values.

The signature of this function should be:
----
auto fun(R)(R state, size_t n)
----
where $(D n) will be the index of the current value, and $(D state) will be an
opaque state vector that can be indexed with array-indexing notation
$(D state[i]), where valid values of $(D i) range from $(D (n - 1)) to
$(D (n - State.length)).

If the function is passed in string form, the state has name $(D "a")
and the zero-based index in the recurrence has name $(D "n"). The
given string must return the desired value for $(D a[n]) given $(D a[n
- 1]), $(D a[n - 2]), $(D a[n - 3]),..., $(D a[n - stateSize]). The
state size is dictated by the number of arguments passed to the call
to $(D recurrence). The $(D Recurrence) struct itself takes care of
managing the recurrence's state and shifting it appropriately.
 */
struct Recurrence(alias fun, StateType, size_t stateSize)
{
    import std.functional : binaryFun;

    StateType[stateSize] _state;
    size_t _n;

    this(StateType[stateSize] initial) { _state = initial; }

    void popFront()
    {
        static auto trustedCycle(ref typeof(_state) s) @trusted
        {
            return cycle(s);
        }
        // The cast here is reasonable because fun may cause integer
        // promotion, but needs to return a StateType to make its operation
        // closed.  Therefore, we have no other choice.
        _state[_n % stateSize] = cast(StateType) binaryFun!(fun, "a", "n")(
            trustedCycle(_state), _n + stateSize);
        ++_n;
    }

    @property StateType front()
    {
        return _state[_n % stateSize];
    }

    @property typeof(this) save()
    {
        return this;
    }

    enum bool empty = false;
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;

    // The Fibonacci numbers, using function in string form:
    // a[0] = 1, a[1] = 1, and compute a[n+1] = a[n-1] + a[n]
    auto fib = recurrence!("a[n-1] + a[n-2]")(1, 1);
    assert(fib.take(10).equal([1, 1, 2, 3, 5, 8, 13, 21, 34, 55]));

    // The factorials, using function in lambda form:
    auto fac = recurrence!((a,n) => a[n-1] * n)(1);
    assert(take(fac, 10).equal([
        1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880
    ]));

    // The triangular numbers, using function in explicit form:
    static size_t genTriangular(R)(R state, size_t n)
    {
        return state[n-1] + n;
    }
    auto tri = recurrence!genTriangular(0);
    assert(take(tri, 10).equal([0, 1, 3, 6, 10, 15, 21, 28, 36, 45]));
}

/// Ditto
Recurrence!(fun, CommonType!(State), State.length)
recurrence(alias fun, State...)(State initial)
{
    CommonType!(State)[State.length] state;
    foreach (i, Unused; State)
    {
        state[i] = initial[i];
    }
    return typeof(return)(state);
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    auto fib = recurrence!("a[n-1] + a[n-2]")(1, 1);
    static assert(isForwardRange!(typeof(fib)));

    int[] witness = [1, 1, 2, 3, 5, 8, 13, 21, 34, 55 ];
    assert(equal(take(fib, 10), witness));
    foreach (e; take(fib, 10)) {}
    auto fact = recurrence!("n * a[n-1]")(1);
    assert( equal(take(fact, 10), [1, 1, 2, 2*3, 2*3*4, 2*3*4*5, 2*3*4*5*6,
                            2*3*4*5*6*7, 2*3*4*5*6*7*8, 2*3*4*5*6*7*8*9][]) );
    auto piapprox = recurrence!("a[n] + (n & 1 ? 4.0 : -4.0) / (2 * n + 3)")(4.0);
    foreach (e; take(piapprox, 20)) {}
    // Thanks to yebblies for this test and the associated fix
    auto r = recurrence!"a[n-2]"(1, 2);
    witness = [1, 2, 1, 2, 1];
    assert(equal(take(r, 5), witness));
}

/**
   $(D Sequence) is similar to $(D Recurrence) except that iteration is
   presented in the so-called $(HTTP en.wikipedia.org/wiki/Closed_form,
   closed form). This means that the $(D n)th element in the series is
   computable directly from the initial values and $(D n) itself. This
   implies that the interface offered by $(D Sequence) is a random-access
   range, as opposed to the regular $(D Recurrence), which only offers
   forward iteration.

   The state of the sequence is stored as a $(D Tuple) so it can be
   heterogeneous.
*/
struct Sequence(alias fun, State)
{
private:
    import std.functional : binaryFun;

    alias compute = binaryFun!(fun, "a", "n");
    alias ElementType = typeof(compute(State.init, cast(size_t) 1));
    State _state;
    size_t _n;

    static struct DollarToken{}

public:
    this(State initial, size_t n = 0)
    {
        _state = initial;
        _n = n;
    }

    @property ElementType front()
    {
        return compute(_state, _n);
    }

    void popFront()
    {
        ++_n;
    }

    enum opDollar = DollarToken();

    auto opSlice(size_t lower, size_t upper)
    in
    {
        assert(
            upper >= lower,
            "Attempting to slice a Sequence with a larger first argument than the second."
        );
    }
    body
    {
        return typeof(this)(_state, _n + lower).take(upper - lower);
    }

    auto opSlice(size_t lower, DollarToken)
    {
        return typeof(this)(_state, _n + lower);
    }

    ElementType opIndex(size_t n)
    {
        return compute(_state, n + _n);
    }

    enum bool empty = false;

    @property Sequence save() { return this; }
}

/// Ditto
auto sequence(alias fun, State...)(State args)
{
    import std.typecons : Tuple, tuple;
    alias Return = Sequence!(fun, Tuple!State);
    return Return(tuple(args));
}

/// Odd numbers, using function in string form:
@safe unittest
{
    auto odds = sequence!("a[0] + n * a[1]")(1, 2);
    assert(odds.front == 1);
    odds.popFront();
    assert(odds.front == 3);
    odds.popFront();
    assert(odds.front == 5);
}

/// Triangular numbers, using function in lambda form:
@safe unittest
{
    auto tri = sequence!((a,n) => n*(n+1)/2)();

    // Note random access
    assert(tri[0] == 0);
    assert(tri[3] == 6);
    assert(tri[1] == 1);
    assert(tri[4] == 10);
    assert(tri[2] == 3);
}

/// Fibonacci numbers, using function in explicit form:
@safe unittest
{
    import std.math : pow, round, sqrt;
    static ulong computeFib(S)(S state, size_t n)
    {
        // Binet's formula
        return cast(ulong)(round((pow(state[0], n+1) - pow(state[1], n+1)) /
                                 state[2]));
    }
    auto fib = sequence!computeFib(
        (1.0 + sqrt(5.0)) / 2.0,    // Golden Ratio
        (1.0 - sqrt(5.0)) / 2.0,    // Conjugate of Golden Ratio
        sqrt(5.0));

    // Note random access with [] operator
    assert(fib[1] == 1);
    assert(fib[4] == 5);
    assert(fib[3] == 3);
    assert(fib[2] == 2);
    assert(fib[9] == 55);
}

@safe unittest
{
    import std.typecons : Tuple, tuple;
    auto y = Sequence!("a[0] + n * a[1]", Tuple!(int, int))(tuple(0, 4));
    static assert(isForwardRange!(typeof(y)));

    //@@BUG
    //auto y = sequence!("a[0] + n * a[1]")(0, 4);
    //foreach (e; take(y, 15))
    {}                                 //writeln(e);

    auto odds = Sequence!("a[0] + n * a[1]", Tuple!(int, int))(
        tuple(1, 2));
    for (int currentOdd = 1; currentOdd <= 21; currentOdd += 2)
    {
        assert(odds.front == odds[0]);
        assert(odds[0] == currentOdd);
        odds.popFront();
    }
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    auto odds = sequence!("a[0] + n * a[1]")(1, 2);
    static assert(hasSlicing!(typeof(odds)));

    //Note: don't use drop or take as the target of an equal,
    //since they'll both just forward to opSlice, making the tests irrelevant

    // static slicing tests
    assert(equal(odds[0 .. 5], [1,  3,  5,  7,  9]));
    assert(equal(odds[3 .. 7], [7,  9, 11, 13]));

    // relative slicing test, testing slicing is NOT agnostic of state
    auto odds_less5 = odds.drop(5); //this should actually call odds[5 .. $]
    assert(equal(odds_less5[0 ..  3], [11, 13, 15]));
    assert(equal(odds_less5[0 .. 10], odds[5 .. 15]));

    //Infinite slicing tests
    odds = odds[10 .. $];
    assert(equal(odds.take(3), [21, 23, 25]));
}

// Issue 5036
@safe unittest
{
    auto s = sequence!((a, n) => new int)(0);
    assert(s.front != s.front);  // no caching
}

// iota
/**
   Creates a range of values that span the given starting and stopping
   values.

   Params:
   begin = The starting value.
   end = The value that serves as the stopping criterion. This value is not
        included in the range.
   step = The value to add to the current value at each iteration.

   Returns:
   A range that goes through the numbers $(D begin), $(D begin + step),
   $(D begin + 2 * step), $(D ...), up to and excluding $(D end).

   The two-argument overloads have $(D step = 1). If $(D begin < end && step <
   0) or $(D begin > end && step > 0) or $(D begin == end), then an empty range
   is returned. If $(D step == 0) then $(D begin == end) is an error.

   For built-in types, the range returned is a random access range. For
   user-defined types that support $(D ++), the range is an input
   range.

   An integral iota also supports $(D in) operator from the right. It takes
   the stepping into account, the integral won't be considered
   contained if it falls between two consecutive values of the range.
   $(D contains) does the same as in, but from lefthand side.

    Example:
    ---
    void main()
    {
        import std.stdio;

        // The following groups all produce the same output of:
        // 0 1 2 3 4

        foreach (i; 0 .. 5)
            writef("%s ", i);
        writeln();

        import std.range : iota;
        foreach (i; iota(0, 5))
            writef("%s ", i);
        writeln();

        writefln("%(%s %|%)", iota(0, 5));

        import std.algorithm.iteration : map;
        import std.algorithm.mutation : copy;
        import std.format;
        iota(0, 5).map!(i => format("%s ", i)).copy(stdout.lockingTextWriter());
        writeln();
    }
    ---
*/
auto iota(B, E, S)(B begin, E end, S step)
if ((isIntegral!(CommonType!(B, E)) || isPointer!(CommonType!(B, E)))
        && isIntegral!S)
{
    import std.conv : unsigned;

    alias Value = CommonType!(Unqual!B, Unqual!E);
    alias StepType = Unqual!S;

    assert(step != 0 || begin == end);

    static struct Result
    {
        private Value current, last;
        private StepType step; // by convention, 0 if range is empty

        this(Value current, Value pastLast, StepType step)
        {
            if (current < pastLast && step > 0)
            {
                // Iterating upward
                assert(unsigned((pastLast - current) / step) <= size_t.max);
                // Cast below can't fail because current < pastLast
                this.last = cast(Value) (pastLast - 1);
                this.last -= unsigned(this.last - current) % step;
            }
            else if (current > pastLast && step < 0)
            {
                // Iterating downward
                assert(unsigned((current - pastLast) / (0 - step)) <= size_t.max);
                // Cast below can't fail because current > pastLast
                this.last = cast(Value) (pastLast + 1);
                this.last += unsigned(current - this.last) % (0 - step);
            }
            else
            {
                // Initialize an empty range
                this.step = 0;
                return;
            }
            this.step = step;
            this.current = current;
        }

        @property bool empty() const { return step == 0; }
        @property inout(Value) front() inout { assert(!empty); return current; }
        void popFront()
        {
            assert(!empty);
            if (current == last) step = 0;
            else current += step;
        }

        @property inout(Value) back() inout
        {
            assert(!empty);
            return last;
        }
        void popBack()
        {
            assert(!empty);
            if (current == last) step = 0;
            else last -= step;
        }

        @property auto save() { return this; }

        inout(Value) opIndex(ulong n) inout
        {
            assert(n < this.length);

            // Just cast to Value here because doing so gives overflow behavior
            // consistent with calling popFront() n times.
            return cast(inout Value) (current + step * n);
        }
        auto opBinaryRight(string op)(Value val) const
        if (op == "in")
        {
            if (empty) return false;
            //cast to avoid becoming unsigned
            auto supposedIndex = cast(StepType)(val - current) / step;
            return supposedIndex < length && supposedIndex * step + current == val;
        }
        auto contains(Value x){return x in this;}
        inout(Result) opSlice() inout { return this; }
        inout(Result) opSlice(ulong lower, ulong upper) inout
        {
            assert(upper >= lower && upper <= this.length);

            return cast(inout Result) Result(
                cast(Value)(current + lower * step),
                cast(Value)(current + upper * step),
                step);
        }
        @property size_t length() const
        {
            if (step > 0)
                return 1 + cast(size_t) (unsigned(last - current) / step);
            if (step < 0)
                return 1 + cast(size_t) (unsigned(current - last) / (0 - step));
            return 0;
        }

        alias opDollar = length;
    }

    return Result(begin, end, step);
}

/// Ditto
auto iota(B, E)(B begin, E end)
if (isFloatingPoint!(CommonType!(B, E)))
{
    return iota(begin, end, CommonType!(B, E)(1));
}

/// Ditto
auto iota(B, E)(B begin, E end)
if (isIntegral!(CommonType!(B, E)) || isPointer!(CommonType!(B, E)))
{
    import std.conv : unsigned;

    alias Value = CommonType!(Unqual!B, Unqual!E);

    static struct Result
    {
        private Value current, pastLast;

        this(Value current, Value pastLast)
        {
            if (current < pastLast)
            {
                assert(unsigned(pastLast - current) <= size_t.max);

                this.current = current;
                this.pastLast = pastLast;
            }
            else
            {
                // Initialize an empty range
                this.current = this.pastLast = current;
            }
        }

        @property bool empty() const { return current == pastLast; }
        @property inout(Value) front() inout { assert(!empty); return current; }
        void popFront() { assert(!empty); ++current; }

        @property inout(Value) back() inout { assert(!empty); return cast(inout(Value))(pastLast - 1); }
        void popBack() { assert(!empty); --pastLast; }

        @property auto save() { return this; }

        inout(Value) opIndex(size_t n) inout
        {
            assert(n < this.length);

            // Just cast to Value here because doing so gives overflow behavior
            // consistent with calling popFront() n times.
            return cast(inout Value) (current + n);
        }
        auto opBinaryRight(string op)(Value val) const
        if (op == "in")
        {
            return current <= val && val < pastLast;
        }
        auto contains(Value x){return x in this;}
        inout(Result) opSlice() inout { return this; }
        inout(Result) opSlice(ulong lower, ulong upper) inout
        {
            assert(upper >= lower && upper <= this.length);

            return cast(inout Result) Result(cast(Value)(current + lower),
                                            cast(Value)(pastLast - (length - upper)));
        }
        @property size_t length() const
        {
            return cast(size_t)(pastLast - current);
        }

        alias opDollar = length;
    }

    return Result(begin, end);
}

/// Ditto
auto iota(E)(E end)
if (is(typeof(iota(E(0), end))))
{
    E begin = E(0);
    return iota(begin, end);
}

/// Ditto
// Specialization for floating-point types
auto iota(B, E, S)(B begin, E end, S step)
if (isFloatingPoint!(CommonType!(B, E, S)))
in
{
    assert(step != 0, "iota: step must not be 0");
    assert((end - begin) / step >= 0, "iota: incorrect startup parameters");
}
body
{
    alias Value = Unqual!(CommonType!(B, E, S));
    static struct Result
    {
        private Value start, step;
        private size_t index, count;

        this(Value start, Value end, Value step)
        {
            import std.conv : to;

            this.start = start;
            this.step = step;
            immutable fcount = (end - start) / step;
            count = to!size_t(fcount);
            auto pastEnd = start + count * step;
            if (step > 0)
            {
                if (pastEnd < end) ++count;
                assert(start + count * step >= end);
            }
            else
            {
                if (pastEnd > end) ++count;
                assert(start + count * step <= end);
            }
        }

        @property bool empty() const { return index == count; }
        @property Value front() const { assert(!empty); return start + step * index; }
        void popFront()
        {
            assert(!empty);
            ++index;
        }
        @property Value back() const
        {
            assert(!empty);
            return start + step * (count - 1);
        }
        void popBack()
        {
            assert(!empty);
            --count;
        }

        @property auto save() { return this; }

        Value opIndex(size_t n) const
        {
            assert(n < count);
            return start + step * (n + index);
        }
        inout(Result) opSlice() inout
        {
            return this;
        }
        inout(Result) opSlice(size_t lower, size_t upper) inout
        {
            assert(upper >= lower && upper <= count);

            Result ret = this;
            ret.index += lower;
            ret.count = upper - lower + ret.index;
            return cast(inout Result) ret;
        }
        @property size_t length() const
        {
            return count - index;
        }

        alias opDollar = length;
    }

    return Result(begin, end, step);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.math : approxEqual;

    auto r = iota(0, 10, 1);
    assert(equal(r, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
    assert(equal(r, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]));
    assert(3 in r);
    assert(r.contains(3)); //Same as above
    assert(!(10 in r));
    assert(!(-8 in r));
    r = iota(0, 11, 3);
    assert(equal(r, [0, 3, 6, 9]));
    assert(r[2] == 6);
    assert(!(2 in r));
    auto rf = iota(0.0, 0.5, 0.1);
    assert(approxEqual(rf, [0.0, 0.1, 0.2, 0.3, 0.4]));
}

nothrow @nogc @safe unittest
{
   //float overloads use std.conv.to so can't be @nogc or nothrow
    alias ssize_t = Signed!size_t;
    assert(iota(ssize_t.max, 0, -1).length == ssize_t.max);
    assert(iota(ssize_t.max, ssize_t.min, -1).length == size_t.max);
    assert(iota(ssize_t.max, ssize_t.min, -2).length == 1 + size_t.max / 2);
    assert(iota(ssize_t.min, ssize_t.max, 2).length == 1 + size_t.max / 2);
    assert(iota(ssize_t.max, ssize_t.min, -3).length == size_t.max / 3);
}

debug @system unittest
{//check the contracts
    import core.exception : AssertError;
    import std.exception : assertThrown;
    assertThrown!AssertError(iota(1,2,0));
    assertThrown!AssertError(iota(0f,1f,0f));
    assertThrown!AssertError(iota(1f,0f,0.1f));
    assertThrown!AssertError(iota(0f,1f,-0.1f));
}

@system unittest
{
    int[] a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    auto r1 = iota(a.ptr, a.ptr + a.length, 1);
    assert(r1.front == a.ptr);
    assert(r1.back == a.ptr + a.length - 1);
    assert(&a[4] in r1);
}

@safe unittest
{
    assert(iota(1UL, 0UL).length == 0);
    assert(iota(1UL, 0UL, 1).length == 0);
    assert(iota(0, 1, 1).length == 1);
    assert(iota(1, 0, -1).length == 1);
    assert(iota(0, 1, -1).length == 0);
    assert(iota(ulong.max, 0).length == 0);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.searching : count;
    import std.math : approxEqual, nextUp, nextDown;
    import std.meta : AliasSeq;

    static assert(is(ElementType!(typeof(iota(0f))) == float));

    static assert(hasLength!(typeof(iota(0, 2))));
    auto r = iota(0, 10, 1);
    assert(r[$ - 1] == 9);
    assert(equal(r, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9][]));

    auto rSlice = r[2 .. 8];
    assert(equal(rSlice, [2, 3, 4, 5, 6, 7]));

    rSlice.popFront();
    assert(rSlice[0] == rSlice.front);
    assert(rSlice.front == 3);

    rSlice.popBack();
    assert(rSlice[rSlice.length - 1] == rSlice.back);
    assert(rSlice.back == 6);

    rSlice = r[0 .. 4];
    assert(equal(rSlice, [0, 1, 2, 3]));
    assert(3 in rSlice);
    assert(!(4 in rSlice));

    auto rr = iota(10);
    assert(equal(rr, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9][]));

    r = iota(0, -10, -1);
    assert(equal(r, [0, -1, -2, -3, -4, -5, -6, -7, -8, -9][]));
    rSlice = r[3 .. 9];
    assert(equal(rSlice, [-3, -4, -5, -6, -7, -8]));

    r = iota(0, -6, -3);
    assert(equal(r, [0, -3][]));
    rSlice = r[1 .. 2];
    assert(equal(rSlice, [-3]));

    r = iota(0, -7, -3);
    assert(equal(r, [0, -3, -6][]));
    assert(0 in r);
    assert(-6 in r);
    rSlice = r[1 .. 3];
    assert(equal(rSlice, [-3, -6]));
    assert(!(0 in rSlice));
    assert(!(-2 in rSlice));
    assert(!(-5 in rSlice));
    assert(!(3 in rSlice));
    assert(!(-9 in rSlice));

    r = iota(0, 11, 3);
    assert(equal(r, [0, 3, 6, 9][]));
    assert(r[2] == 6);
    rSlice = r[1 .. 3];
    assert(equal(rSlice, [3, 6]));

    auto rf = iota(0.0, 0.5, 0.1);
    assert(approxEqual(rf, [0.0, 0.1, 0.2, 0.3, 0.4][]));
    assert(rf.length == 5);

    rf.popFront();
    assert(rf.length == 4);

    auto rfSlice = rf[1 .. 4];
    assert(rfSlice.length == 3);
    assert(approxEqual(rfSlice, [0.2, 0.3, 0.4]));

    rfSlice.popFront();
    assert(approxEqual(rfSlice[0], 0.3));

    rf.popFront();
    assert(rf.length == 3);

    rfSlice = rf[1 .. 3];
    assert(rfSlice.length == 2);
    assert(approxEqual(rfSlice, [0.3, 0.4]));
    assert(approxEqual(rfSlice[0], 0.3));

    // With something just above 0.5
    rf = iota(0.0, nextUp(0.5), 0.1);
    assert(approxEqual(rf, [0.0, 0.1, 0.2, 0.3, 0.4, 0.5][]));
    rf.popBack();
    assert(rf[rf.length - 1] == rf.back);
    assert(approxEqual(rf.back, 0.4));
    assert(rf.length == 5);

    // going down
    rf = iota(0.0, -0.5, -0.1);
    assert(approxEqual(rf, [0.0, -0.1, -0.2, -0.3, -0.4][]));
    rfSlice = rf[2 .. 5];
    assert(approxEqual(rfSlice, [-0.2, -0.3, -0.4]));

    rf = iota(0.0, nextDown(-0.5), -0.1);
    assert(approxEqual(rf, [0.0, -0.1, -0.2, -0.3, -0.4, -0.5][]));

    // iota of longs
    auto rl = iota(5_000_000L);
    assert(rl.length == 5_000_000L);
    assert(0 in rl);
    assert(4_000_000L in rl);
    assert(!(-4_000_000L in rl));
    assert(!(5_000_000L in rl));

    // iota of longs with steps
    auto iota_of_longs_with_steps = iota(50L, 101L, 10);
    assert(iota_of_longs_with_steps.length == 6);
    assert(equal(iota_of_longs_with_steps, [50L, 60L, 70L, 80L, 90L, 100L]));

    // iota of unsigned zero length (issue 6222, actually trying to consume it
    // is the only way to find something is wrong because the public
    // properties are all correct)
    auto iota_zero_unsigned = iota(0, 0u, 3);
    assert(count(iota_zero_unsigned) == 0);

    // unsigned reverse iota can be buggy if .length doesn't take them into
    // account (issue 7982).
    assert(iota(10u, 0u, -1).length == 10);
    assert(iota(10u, 0u, -2).length == 5);
    assert(iota(uint.max, uint.max-10, -1).length == 10);
    assert(iota(uint.max, uint.max-10, -2).length == 5);
    assert(iota(uint.max, 0u, -1).length == uint.max);

    assert(20 in iota(20u, 10u, -2));
    assert(16 in iota(20u, 10u, -2));
    assert(!(15 in iota(20u, 10u, -2)));
    assert(!(10 in iota(20u, 10u, -2)));
    assert(!(uint.max in iota(20u, 10u, -1)));
    assert(!(int.min in iota(20u, 10u, -1)));
    assert(!(int.max in iota(20u, 10u, -1)));


    // Issue 8920
    foreach (Type; AliasSeq!(byte, ubyte, short, ushort,
        int, uint, long, ulong))
    {
        Type val;
        foreach (i; iota(cast(Type) 0, cast(Type) 10)) { val++; }
        assert(val == 10);
    }
}

@safe unittest
{
    import std.algorithm.mutation : copy;
    auto idx = new size_t[100];
    copy(iota(0, idx.length), idx);
}

@safe unittest
{
    import std.meta : AliasSeq;
    foreach (range; AliasSeq!(iota(2, 27, 4),
                             iota(3, 9),
                             iota(2.7, 12.3, .1),
                             iota(3.2, 9.7)))
    {
        const cRange = range;
        const e = cRange.empty;
        const f = cRange.front;
        const b = cRange.back;
        const i = cRange[2];
        const s1 = cRange[];
        const s2 = cRange[0 .. 3];
        const l = cRange.length;
    }
}

@system unittest
{
    //The ptr stuff can't be done at compile time, so we unfortunately end
    //up with some code duplication here.
    auto arr = [0, 5, 3, 5, 5, 7, 9, 2, 0, 42, 7, 6];

    {
        const cRange = iota(arr.ptr, arr.ptr + arr.length, 3);
        const e = cRange.empty;
        const f = cRange.front;
        const b = cRange.back;
        const i = cRange[2];
        const s1 = cRange[];
        const s2 = cRange[0 .. 3];
        const l = cRange.length;
    }

    {
        const cRange = iota(arr.ptr, arr.ptr + arr.length);
        const e = cRange.empty;
        const f = cRange.front;
        const b = cRange.back;
        const i = cRange[2];
        const s1 = cRange[];
        const s2 = cRange[0 .. 3];
        const l = cRange.length;
    }
}

@nogc nothrow pure @safe
unittest
{
    {
        ushort start = 0, end = 10, step = 2;
        foreach (i; iota(start, end, step))
            static assert(is(typeof(i) == ushort));
    }
    {
        ubyte start = 0, end = 255, step = 128;
        uint x;
        foreach (i; iota(start, end, step))
        {
            static assert(is(typeof(i) == ubyte));
            ++x;
        }
        assert(x == 2);
    }
}

/* Generic overload that handles arbitrary types that support arithmetic
 * operations.
 *
 * User-defined types such as $(REF BigInt, std,bigint) are also supported, as long
 * as they can be incremented with $(D ++) and compared with $(D <) or $(D ==).
 */
/// ditto
auto iota(B, E)(B begin, E end)
if (!isIntegral!(CommonType!(B, E)) &&
    !isFloatingPoint!(CommonType!(B, E)) &&
    !isPointer!(CommonType!(B, E)) &&
    is(typeof((ref B b) { ++b; })) &&
    (is(typeof(B.init < E.init)) || is(typeof(B.init == E.init))) )
{
    static struct Result
    {
        B current;
        E end;

        @property bool empty()
        {
            static if (is(typeof(B.init < E.init)))
                return !(current < end);
            else static if (is(typeof(B.init != E.init)))
                return current == end;
            else
                static assert(0);
        }
        @property auto front() { return current; }
        void popFront()
        {
            assert(!empty);
            ++current;
        }
    }
    return Result(begin, end);
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    // Test iota() for a type that only supports ++ and != but does not have
    // '<'-ordering.
    struct Cyclic(int wrapAround)
    {
        int current;

        this(int start) { current = start % wrapAround; }

        bool opEquals(Cyclic c) const { return current == c.current; }
        bool opEquals(int i) const { return current == i; }
        void opUnary(string op)() if (op == "++")
        {
            current = (current + 1) % wrapAround;
        }
    }
    alias Cycle5 = Cyclic!5;

    // Easy case
    auto i1 = iota(Cycle5(1), Cycle5(4));
    assert(i1.equal([1, 2, 3]));

    // Wraparound case
    auto i2 = iota(Cycle5(3), Cycle5(2));
    assert(i2.equal([3, 4, 0, 1 ]));
}

/**
   Options for the $(LREF FrontTransversal) and $(LREF Transversal) ranges
   (below).
*/
enum TransverseOptions
{
/**
   When transversed, the elements of a range of ranges are assumed to
   have different lengths (e.g. a jagged array).
*/
    assumeJagged,                      //default
    /**
       The transversal enforces that the elements of a range of ranges have
       all the same length (e.g. an array of arrays, all having the same
       length). Checking is done once upon construction of the transversal
       range.
    */
        enforceNotJagged,
    /**
       The transversal assumes, without verifying, that the elements of a
       range of ranges have all the same length. This option is useful if
       checking was already done from the outside of the range.
    */
        assumeNotJagged,
        }

/**
   Given a range of ranges, iterate transversally through the first
   elements of each of the enclosed ranges.
*/
struct FrontTransversal(Ror,
        TransverseOptions opt = TransverseOptions.assumeJagged)
{
    alias RangeOfRanges = Unqual!(Ror);
    alias RangeType     = .ElementType!RangeOfRanges;
    alias ElementType   = .ElementType!RangeType;

    private void prime()
    {
        static if (opt == TransverseOptions.assumeJagged)
        {
            while (!_input.empty && _input.front.empty)
            {
                _input.popFront();
            }
            static if (isBidirectionalRange!RangeOfRanges)
            {
                while (!_input.empty && _input.back.empty)
                {
                    _input.popBack();
                }
            }
        }
    }

/**
   Construction from an input.
*/
    this(RangeOfRanges input)
    {
        _input = input;
        prime();
        static if (opt == TransverseOptions.enforceNotJagged)
            // (isRandomAccessRange!RangeOfRanges
            //     && hasLength!RangeType)
        {
            import std.exception : enforce;

            if (empty) return;
            immutable commonLength = _input.front.length;
            foreach (e; _input)
            {
                enforce(e.length == commonLength);
            }
        }
    }

/**
   Forward range primitives.
*/
    static if (isInfinite!RangeOfRanges)
    {
        enum bool empty = false;
    }
    else
    {
        @property bool empty()
        {
            static if (opt != TransverseOptions.assumeJagged)
            {
                if (!_input.empty)
                    return _input.front.empty;
            }

            return _input.empty;
        }
    }

    /// Ditto
    @property auto ref front()
    {
        assert(!empty, "Attempting to fetch the front of an empty FrontTransversal");
        return _input.front.front;
    }

    /// Ditto
    static if (hasMobileElements!RangeType)
    {
        ElementType moveFront()
        {
            return _input.front.moveFront();
        }
    }

    static if (hasAssignableElements!RangeType)
    {
        @property void front(ElementType val)
        {
            _input.front.front = val;
        }
    }

    /// Ditto
    void popFront()
    {
        assert(!empty, "Attempting to popFront an empty FrontTransversal");
        _input.popFront();
        prime();
    }

/**
   Duplicates this $(D frontTransversal). Note that only the encapsulating
   range of range will be duplicated. Underlying ranges will not be
   duplicated.
*/
    static if (isForwardRange!RangeOfRanges)
    {
        @property FrontTransversal save()
        {
            return FrontTransversal(_input.save);
        }
    }

    static if (isBidirectionalRange!RangeOfRanges)
    {
/**
   Bidirectional primitives. They are offered if $(D
   isBidirectionalRange!RangeOfRanges).
*/
        @property auto ref back()
        {
            assert(!empty, "Attempting to fetch the back of an empty FrontTransversal");
            return _input.back.front;
        }
        /// Ditto
        void popBack()
        {
            assert(!empty, "Attempting to popBack an empty FrontTransversal");
            _input.popBack();
            prime();
        }

        /// Ditto
        static if (hasMobileElements!RangeType)
        {
            ElementType moveBack()
            {
                return _input.back.moveFront();
            }
        }

        static if (hasAssignableElements!RangeType)
        {
            @property void back(ElementType val)
            {
                _input.back.front = val;
            }
        }
    }

    static if (isRandomAccessRange!RangeOfRanges &&
            (opt == TransverseOptions.assumeNotJagged ||
                    opt == TransverseOptions.enforceNotJagged))
    {
/**
   Random-access primitive. It is offered if $(D
   isRandomAccessRange!RangeOfRanges && (opt ==
   TransverseOptions.assumeNotJagged || opt ==
   TransverseOptions.enforceNotJagged)).
*/
        auto ref opIndex(size_t n)
        {
            return _input[n].front;
        }

        /// Ditto
        static if (hasMobileElements!RangeType)
        {
            ElementType moveAt(size_t n)
            {
                return _input[n].moveFront();
            }
        }
        /// Ditto
        static if (hasAssignableElements!RangeType)
        {
            void opIndexAssign(ElementType val, size_t n)
            {
                _input[n].front = val;
            }
        }
        /// Ditto
        static if (hasLength!RangeOfRanges)
        {
            @property size_t length()
            {
                return _input.length;
            }

            alias opDollar = length;
        }

/**
   Slicing if offered if $(D RangeOfRanges) supports slicing and all the
   conditions for supporting indexing are met.
*/
        static if (hasSlicing!RangeOfRanges)
        {
            typeof(this) opSlice(size_t lower, size_t upper)
            {
                return typeof(this)(_input[lower .. upper]);
            }
        }
    }

    auto opSlice() { return this; }

private:
    RangeOfRanges _input;
}

/// Ditto
FrontTransversal!(RangeOfRanges, opt) frontTransversal(
    TransverseOptions opt = TransverseOptions.assumeJagged,
    RangeOfRanges)
(RangeOfRanges rr)
{
    return typeof(return)(rr);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    int[][] x = new int[][2];
    x[0] = [1, 2];
    x[1] = [3, 4];
    auto ror = frontTransversal(x);
    assert(equal(ror, [ 1, 3 ][]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : AllDummyRanges, DummyRange, ReturnBy;

    static assert(is(FrontTransversal!(immutable int[][])));

    foreach (DummyType; AllDummyRanges)
    {
        auto dummies =
            [DummyType.init, DummyType.init, DummyType.init, DummyType.init];

        foreach (i, ref elem; dummies)
        {
            // Just violate the DummyRange abstraction to get what I want.
            elem.arr = elem.arr[i..$ - (3 - i)];
        }

        auto ft = frontTransversal!(TransverseOptions.assumeNotJagged)(dummies);
        static if (isForwardRange!DummyType)
        {
            static assert(isForwardRange!(typeof(ft)));
        }

        assert(equal(ft, [1, 2, 3, 4]));

        // Test slicing.
        assert(equal(ft[0 .. 2], [1, 2]));
        assert(equal(ft[1 .. 3], [2, 3]));

        assert(ft.front == ft.moveFront());
        assert(ft.back == ft.moveBack());
        assert(ft.moveAt(1) == ft[1]);


        // Test infiniteness propagation.
        static assert(isInfinite!(typeof(frontTransversal(repeat("foo")))));

        static if (DummyType.r == ReturnBy.Reference)
        {
            {
                ft.front++;
                scope(exit) ft.front--;
                assert(dummies.front.front == 2);
            }

            {
                ft.front = 5;
                scope(exit) ft.front = 1;
                assert(dummies[0].front == 5);
            }

            {
                ft.back = 88;
                scope(exit) ft.back = 4;
                assert(dummies.back.front == 88);
            }

            {
                ft[1] = 99;
                scope(exit) ft[1] = 2;
                assert(dummies[1].front == 99);
            }
        }
    }
}

// Issue 16363
@safe unittest
{
    import std.algorithm.comparison : equal;

    int[][] darr = [[0, 1], [4, 5]];
    auto ft = frontTransversal!(TransverseOptions.assumeNotJagged)(darr);

    assert(equal(ft, [0, 4]));
    static assert(isRandomAccessRange!(typeof(ft)));
}

// Bugzilla 16442
@safe unittest
{
    int[][] arr = [[], []];

    auto ft1 = frontTransversal!(TransverseOptions.assumeNotJagged)(arr);
    assert(ft1.empty);

    auto ft2 = frontTransversal!(TransverseOptions.enforceNotJagged)(arr);
    assert(ft2.empty);
}

/**
    Given a range of ranges, iterate transversally through the
    `n`th element of each of the enclosed ranges.

    Params:
        opt = Controls the assumptions the function makes about the lengths
        of the ranges
        rr = An input range of random access ranges
    Returns:
        At minimum, an input range. Range primitives such as bidirectionality
        and random access are given if the element type of `rr` provides them.
*/
struct Transversal(Ror,
        TransverseOptions opt = TransverseOptions.assumeJagged)
{
    private alias RangeOfRanges = Unqual!Ror;
    private alias InnerRange = ElementType!RangeOfRanges;
    private alias E = ElementType!InnerRange;

    private void prime()
    {
        static if (opt == TransverseOptions.assumeJagged)
        {
            while (!_input.empty && _input.front.length <= _n)
            {
                _input.popFront();
            }
            static if (isBidirectionalRange!RangeOfRanges)
            {
                while (!_input.empty && _input.back.length <= _n)
                {
                    _input.popBack();
                }
            }
        }
    }

/**
   Construction from an input and an index.
*/
    this(RangeOfRanges input, size_t n)
    {
        _input = input;
        _n = n;
        prime();
        static if (opt == TransverseOptions.enforceNotJagged)
        {
            import std.exception : enforce;

            if (empty) return;
            immutable commonLength = _input.front.length;
            foreach (e; _input)
            {
                enforce(e.length == commonLength);
            }
        }
    }

/**
   Forward range primitives.
*/
    static if (isInfinite!(RangeOfRanges))
    {
        enum bool empty = false;
    }
    else
    {
        @property bool empty()
        {
            return _input.empty;
        }
    }

    /// Ditto
    @property auto ref front()
    {
        assert(!empty, "Attempting to fetch the front of an empty Transversal");
        return _input.front[_n];
    }

    /// Ditto
    static if (hasMobileElements!InnerRange)
    {
        E moveFront()
        {
            return _input.front.moveAt(_n);
        }
    }

    /// Ditto
    static if (hasAssignableElements!InnerRange)
    {
        @property void front(E val)
        {
            _input.front[_n] = val;
        }
    }


    /// Ditto
    void popFront()
    {
        assert(!empty, "Attempting to popFront an empty Transversal");
        _input.popFront();
        prime();
    }

    /// Ditto
    static if (isForwardRange!RangeOfRanges)
    {
        @property typeof(this) save()
        {
            auto ret = this;
            ret._input = _input.save;
            return ret;
        }
    }

    static if (isBidirectionalRange!RangeOfRanges)
    {
/**
   Bidirectional primitives. They are offered if $(D
   isBidirectionalRange!RangeOfRanges).
*/
        @property auto ref back()
        {
            assert(!empty, "Attempting to fetch the back of an empty Transversal");
            return _input.back[_n];
        }

        /// Ditto
        void popBack()
        {
            assert(!empty, "Attempting to popBack an empty Transversal");
            _input.popBack();
            prime();
        }

        /// Ditto
        static if (hasMobileElements!InnerRange)
        {
            E moveBack()
            {
                return _input.back.moveAt(_n);
            }
        }

        /// Ditto
        static if (hasAssignableElements!InnerRange)
        {
            @property void back(E val)
            {
                _input.back[_n] = val;
            }
        }

    }

    static if (isRandomAccessRange!RangeOfRanges &&
            (opt == TransverseOptions.assumeNotJagged ||
                    opt == TransverseOptions.enforceNotJagged))
    {
/**
   Random-access primitive. It is offered if $(D
   isRandomAccessRange!RangeOfRanges && (opt ==
   TransverseOptions.assumeNotJagged || opt ==
   TransverseOptions.enforceNotJagged)).
*/
        auto ref opIndex(size_t n)
        {
            return _input[n][_n];
        }

        /// Ditto
        static if (hasMobileElements!InnerRange)
        {
            E moveAt(size_t n)
            {
                return _input[n].moveAt(_n);
            }
        }

        /// Ditto
        static if (hasAssignableElements!InnerRange)
        {
            void opIndexAssign(E val, size_t n)
            {
                _input[n][_n] = val;
            }
        }

        /// Ditto
        static if (hasLength!RangeOfRanges)
        {
            @property size_t length()
            {
                return _input.length;
            }

            alias opDollar = length;
        }

/**
   Slicing if offered if $(D RangeOfRanges) supports slicing and all the
   conditions for supporting indexing are met.
*/
        static if (hasSlicing!RangeOfRanges)
        {
            typeof(this) opSlice(size_t lower, size_t upper)
            {
                return typeof(this)(_input[lower .. upper], _n);
            }
        }
    }

    auto opSlice() { return this; }

private:
    RangeOfRanges _input;
    size_t _n;
}

/// Ditto
Transversal!(RangeOfRanges, opt) transversal
(TransverseOptions opt = TransverseOptions.assumeJagged, RangeOfRanges)
(RangeOfRanges rr, size_t n)
{
    return typeof(return)(rr, n);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    int[][] x = new int[][2];
    x[0] = [1, 2];
    x[1] = [3, 4];
    auto ror = transversal(x, 1);
    assert(equal(ror, [ 2, 4 ][]));
}

@safe unittest
{
    import std.internal.test.dummyrange : DummyRange, Length, RangeType, ReturnBy;

    int[][] x = new int[][2];
    x[0] = [ 1, 2 ];
    x[1] = [3, 4];
    auto ror = transversal!(TransverseOptions.assumeNotJagged)(x, 1);
    auto witness = [ 2, 4 ];
    uint i;
    foreach (e; ror) assert(e == witness[i++]);
    assert(i == 2);
    assert(ror.length == 2);

    static assert(is(Transversal!(immutable int[][])));

    // Make sure ref, assign is being propagated.
    {
        ror.front++;
        scope(exit) ror.front--;
        assert(x[0][1] == 3);
    }
    {
        ror.front = 5;
        scope(exit) ror.front = 2;
        assert(x[0][1] == 5);
        assert(ror.moveFront() == 5);
    }
    {
        ror.back = 999;
        scope(exit) ror.back = 4;
        assert(x[1][1] == 999);
        assert(ror.moveBack() == 999);
    }
    {
        ror[0] = 999;
        scope(exit) ror[0] = 2;
        assert(x[0][1] == 999);
        assert(ror.moveAt(0) == 999);
    }

    // Test w/o ref return.
    alias D = DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Random);
    auto drs = [D.init, D.init];
    foreach (num; 0 .. 10)
    {
        auto t = transversal!(TransverseOptions.enforceNotJagged)(drs, num);
        assert(t[0] == t[1]);
        assert(t[1] == num + 1);
    }

    static assert(isInfinite!(typeof(transversal(repeat([1,2,3]), 1))));

    // Test slicing.
    auto mat = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12], [13, 14, 15, 16]];
    auto mat1 = transversal!(TransverseOptions.assumeNotJagged)(mat, 1)[1 .. 3];
    assert(mat1[0] == 6);
    assert(mat1[1] == 10);
}

struct Transposed(RangeOfRanges)
if (isForwardRange!RangeOfRanges &&
    isInputRange!(ElementType!RangeOfRanges) &&
    hasAssignableElements!RangeOfRanges)
{
    //alias ElementType = typeof(map!"a.front"(RangeOfRanges.init));

    this(RangeOfRanges input)
    {
        this._input = input;
    }

    @property auto front()
    {
        import std.algorithm.iteration : filter, map;
        return _input.save
                     .filter!(a => !a.empty)
                     .map!(a => a.front);
    }

    void popFront()
    {
        // Advance the position of each subrange.
        auto r = _input.save;
        while (!r.empty)
        {
            auto e = r.front;
            if (!e.empty)
            {
                e.popFront();
                r.front = e;
            }

            r.popFront();
        }
    }

    // ElementType opIndex(size_t n)
    // {
    //     return _input[n].front;
    // }

    @property bool empty()
    {
        if (_input.empty) return true;
        foreach (e; _input.save)
        {
            if (!e.empty) return false;
        }
        return true;
    }

    @property Transposed save()
    {
        return Transposed(_input.save);
    }

    auto opSlice() { return this; }

private:
    RangeOfRanges _input;
}

@safe unittest
{
    // Boundary case: transpose of empty range should be empty
    int[][] ror = [];
    assert(transposed(ror).empty);
}

// Issue 9507
@safe unittest
{
    import std.algorithm.comparison : equal;

    auto r = [[1,2], [3], [4,5], [], [6]];
    assert(r.transposed.equal!equal([
        [1, 3, 4, 6],
        [2, 5]
    ]));
}

/**
Given a range of ranges, returns a range of ranges where the $(I i)'th subrange
contains the $(I i)'th elements of the original subranges.
 */
Transposed!RangeOfRanges transposed(RangeOfRanges)(RangeOfRanges rr)
if (isForwardRange!RangeOfRanges &&
    isInputRange!(ElementType!RangeOfRanges) &&
    hasAssignableElements!RangeOfRanges)
{
    return Transposed!RangeOfRanges(rr);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    int[][] ror = [
        [1, 2, 3],
        [4, 5, 6]
    ];
    auto xp = transposed(ror);
    assert(equal!"a.equal(b)"(xp, [
        [1, 4],
        [2, 5],
        [3, 6]
    ]));
}

///
@safe unittest
{
    int[][] x = new int[][2];
    x[0] = [1, 2];
    x[1] = [3, 4];
    auto tr = transposed(x);
    int[][] witness = [ [ 1, 3 ], [ 2, 4 ] ];
    uint i;

    foreach (e; tr)
    {
        assert(array(e) == witness[i++]);
    }
}

// Issue 8764
@safe unittest
{
    import std.algorithm.comparison : equal;
    ulong[1] t0 = [ 123 ];

    assert(!hasAssignableElements!(typeof(t0[].chunks(1))));
    assert(!is(typeof(transposed(t0[].chunks(1)))));
    assert(is(typeof(transposed(t0[].chunks(1).array()))));

    auto t1 = transposed(t0[].chunks(1).array());
    assert(equal!"a.equal(b)"(t1, [[123]]));
}

/**
This struct takes two ranges, $(D source) and $(D indices), and creates a view
of $(D source) as if its elements were reordered according to $(D indices).
$(D indices) may include only a subset of the elements of $(D source) and
may also repeat elements.

$(D Source) must be a random access range.  The returned range will be
bidirectional or random-access if $(D Indices) is bidirectional or
random-access, respectively.
*/
struct Indexed(Source, Indices)
if (isRandomAccessRange!Source && isInputRange!Indices &&
    is(typeof(Source.init[ElementType!(Indices).init])))
{
    this(Source source, Indices indices)
    {
        this._source = source;
        this._indices = indices;
    }

    /// Range primitives
    @property auto ref front()
    {
        assert(!empty, "Attempting to fetch the front of an empty Indexed");
        return _source[_indices.front];
    }

    /// Ditto
    void popFront()
    {
        assert(!empty, "Attempting to popFront an empty Indexed");
        _indices.popFront();
    }

    static if (isInfinite!Indices)
    {
        enum bool empty = false;
    }
    else
    {
        /// Ditto
        @property bool empty()
        {
            return _indices.empty;
        }
    }

    static if (isForwardRange!Indices)
    {
        /// Ditto
        @property typeof(this) save()
        {
            // Don't need to save _source because it's never consumed.
            return typeof(this)(_source, _indices.save);
        }
    }

    /// Ditto
    static if (hasAssignableElements!Source)
    {
        @property auto ref front(ElementType!Source newVal)
        {
            assert(!empty);
            return _source[_indices.front] = newVal;
        }
    }


    static if (hasMobileElements!Source)
    {
        /// Ditto
        auto moveFront()
        {
            assert(!empty);
            return _source.moveAt(_indices.front);
        }
    }

    static if (isBidirectionalRange!Indices)
    {
        /// Ditto
        @property auto ref back()
        {
            assert(!empty, "Attempting to fetch the back of an empty Indexed");
            return _source[_indices.back];
        }

        /// Ditto
        void popBack()
        {
           assert(!empty, "Attempting to popBack an empty Indexed");
           _indices.popBack();
        }

        /// Ditto
        static if (hasAssignableElements!Source)
        {
            @property auto ref back(ElementType!Source newVal)
            {
                assert(!empty);
                return _source[_indices.back] = newVal;
            }
        }


        static if (hasMobileElements!Source)
        {
            /// Ditto
            auto moveBack()
            {
                assert(!empty);
                return _source.moveAt(_indices.back);
            }
        }
    }

    static if (hasLength!Indices)
    {
        /// Ditto
         @property size_t length()
        {
            return _indices.length;
        }

        alias opDollar = length;
    }

    static if (isRandomAccessRange!Indices)
    {
        /// Ditto
        auto ref opIndex(size_t index)
        {
            return _source[_indices[index]];
        }

        static if (hasSlicing!Indices)
        {
            /// Ditto
            typeof(this) opSlice(size_t a, size_t b)
            {
                return typeof(this)(_source, _indices[a .. b]);
            }
        }


        static if (hasAssignableElements!Source)
        {
            /// Ditto
            auto opIndexAssign(ElementType!Source newVal, size_t index)
            {
                return _source[_indices[index]] = newVal;
            }
        }


        static if (hasMobileElements!Source)
        {
            /// Ditto
            auto moveAt(size_t index)
            {
                return _source.moveAt(_indices[index]);
            }
        }
    }

    // All this stuff is useful if someone wants to index an Indexed
    // without adding a layer of indirection.

    /**
    Returns the source range.
    */
    @property Source source()
    {
        return _source;
    }

    /**
    Returns the indices range.
    */
     @property Indices indices()
    {
        return _indices;
    }

    static if (isRandomAccessRange!Indices)
    {
        /**
        Returns the physical index into the source range corresponding to a
        given logical index.  This is useful, for example, when indexing
        an $(D Indexed) without adding another layer of indirection.
        */
        size_t physicalIndex(size_t logicalIndex)
        {
            return _indices[logicalIndex];
        }

        ///
        @safe unittest
        {
            auto ind = indexed([1, 2, 3, 4, 5], [1, 3, 4]);
            assert(ind.physicalIndex(0) == 1);
        }
    }

private:
    Source _source;
    Indices _indices;

}

/// Ditto
Indexed!(Source, Indices) indexed(Source, Indices)(Source source, Indices indices)
{
    return typeof(return)(source, indices);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    auto source = [1, 2, 3, 4, 5];
    auto indices = [4, 3, 1, 2, 0, 4];
    auto ind = indexed(source, indices);
    assert(equal(ind, [5, 4, 2, 3, 1, 5]));
    assert(equal(retro(ind), [5, 1, 3, 2, 4, 5]));
}

@safe unittest
{
    {
        auto ind = indexed([1, 2, 3, 4, 5], [1, 3, 4]);
        assert(ind.physicalIndex(0) == 1);
    }

    auto source = [1, 2, 3, 4, 5];
    auto indices = [4, 3, 1, 2, 0, 4];
    auto ind = indexed(source, indices);

    // When elements of indices are duplicated and Source has lvalue elements,
    // these are aliased in ind.
    ind[0]++;
    assert(ind[0] == 6);
    assert(ind[5] == 6);
}

@safe unittest
{
    import std.internal.test.dummyrange : AllDummyRanges, propagatesLength,
        propagatesRangeType, RangeType;

    foreach (DummyType; AllDummyRanges)
    {
        auto d = DummyType.init;
        auto r = indexed([1, 2, 3, 4, 5], d);
        static assert(propagatesRangeType!(DummyType, typeof(r)));
        static assert(propagatesLength!(DummyType, typeof(r)));
    }
}

/**
This range iterates over fixed-sized chunks of size $(D chunkSize) of a
$(D source) range. $(D Source) must be an input range. $(D chunkSize) must be
greater than zero.

If $(D !isInfinite!Source) and $(D source.walkLength) is not evenly
divisible by $(D chunkSize), the back element of this range will contain
fewer than $(D chunkSize) elements.

If `Source` is a forward range, the resulting range will be forward ranges as
well. Otherwise, the resulting chunks will be input ranges consuming the same
input: iterating over `front` will shrink the chunk such that subsequent
invocations of `front` will no longer return the full chunk, and calling
`popFront` on the outer range will invalidate any lingering references to
previous values of `front`.

Params:
    source = Range from which the chunks will be selected
    chunkSize = Chunk size

See_Also: $(LREF slide)

Returns: Range of chunks.
*/
struct Chunks(Source)
if (isInputRange!Source)
{
    static if (isForwardRange!Source)
    {
        /// Standard constructor
        this(Source source, size_t chunkSize)
        {
            assert(chunkSize != 0, "Cannot create a Chunk with an empty chunkSize");
            _source = source;
            _chunkSize = chunkSize;
        }

        /// Input range primitives. Always present.
        @property auto front()
        {
            assert(!empty, "Attempting to fetch the front of an empty Chunks");
            return _source.save.take(_chunkSize);
        }

        /// Ditto
        void popFront()
        {
            assert(!empty, "Attempting to popFront and empty Chunks");
            _source.popFrontN(_chunkSize);
        }

        static if (!isInfinite!Source)
            /// Ditto
            @property bool empty()
            {
                return _source.empty;
            }
        else
            // undocumented
            enum empty = false;

        /// Forward range primitives. Only present if `Source` is a forward range.
        @property typeof(this) save()
        {
            return typeof(this)(_source.save, _chunkSize);
        }

        static if (hasLength!Source)
        {
            /// Length. Only if $(D hasLength!Source) is $(D true)
            @property size_t length()
            {
                // Note: _source.length + _chunkSize may actually overflow.
                // We cast to ulong to mitigate the problem on x86 machines.
                // For x64 machines, we just suppose we'll never overflow.
                // The "safe" code would require either an extra branch, or a
                //   modulo operation, which is too expensive for such a rare case
                return cast(size_t)((cast(ulong)(_source.length) + _chunkSize - 1) / _chunkSize);
            }
            //Note: No point in defining opDollar here without slicing.
            //opDollar is defined below in the hasSlicing!Source section
        }

        static if (hasSlicing!Source)
        {
            //Used for various purposes
            private enum hasSliceToEnd = is(typeof(Source.init[_chunkSize .. $]) == Source);

            /**
            Indexing and slicing operations. Provided only if
            $(D hasSlicing!Source) is $(D true).
             */
            auto opIndex(size_t index)
            {
                immutable start = index * _chunkSize;
                immutable end   = start + _chunkSize;

                static if (isInfinite!Source)
                    return _source[start .. end];
                else
                {
                    import std.algorithm.comparison : min;
                    immutable len = _source.length;
                    assert(start < len, "chunks index out of bounds");
                    return _source[start .. min(end, len)];
                }
            }

            /// Ditto
            static if (hasLength!Source)
                typeof(this) opSlice(size_t lower, size_t upper)
                {
                    import std.algorithm.comparison : min;
                    assert(lower <= upper && upper <= length, "chunks slicing index out of bounds");
                    immutable len = _source.length;
                    return chunks(_source[min(lower * _chunkSize, len) .. min(upper * _chunkSize, len)], _chunkSize);
                }
            else static if (hasSliceToEnd)
                //For slicing an infinite chunk, we need to slice the source to the end.
                typeof(takeExactly(this, 0)) opSlice(size_t lower, size_t upper)
                {
                    assert(lower <= upper, "chunks slicing index out of bounds");
                    return chunks(_source[lower * _chunkSize .. $], _chunkSize).takeExactly(upper - lower);
                }

            static if (isInfinite!Source)
            {
                static if (hasSliceToEnd)
                {
                    private static struct DollarToken{}
                    DollarToken opDollar()
                    {
                        return DollarToken();
                    }
                    //Slice to dollar
                    typeof(this) opSlice(size_t lower, DollarToken)
                    {
                        return typeof(this)(_source[lower * _chunkSize .. $], _chunkSize);
                    }
                }
            }
            else
            {
                //Dollar token carries a static type, with no extra information.
                //It can lazily transform into _source.length on algorithmic
                //operations such as : chunks[$/2, $-1];
                private static struct DollarToken
                {
                    Chunks!Source* mom;
                    @property size_t momLength()
                    {
                        return mom.length;
                    }
                    alias momLength this;
                }
                DollarToken opDollar()
                {
                    return DollarToken(&this);
                }

                //Slice overloads optimized for using dollar. Without this, to slice to end, we would...
                //1. Evaluate chunks.length
                //2. Multiply by _chunksSize
                //3. To finally just compare it (with min) to the original length of source (!)
                //These overloads avoid that.
                typeof(this) opSlice(DollarToken, DollarToken)
                {
                    static if (hasSliceToEnd)
                        return chunks(_source[$ .. $], _chunkSize);
                    else
                    {
                        immutable len = _source.length;
                        return chunks(_source[len .. len], _chunkSize);
                    }
                }
                typeof(this) opSlice(size_t lower, DollarToken)
                {
                    import std.algorithm.comparison : min;
                    assert(lower <= length, "chunks slicing index out of bounds");
                    static if (hasSliceToEnd)
                        return chunks(_source[min(lower * _chunkSize, _source.length) .. $], _chunkSize);
                    else
                    {
                        immutable len = _source.length;
                        return chunks(_source[min(lower * _chunkSize, len) .. len], _chunkSize);
                    }
                }
                typeof(this) opSlice(DollarToken, size_t upper)
                {
                    assert(upper == length, "chunks slicing index out of bounds");
                    return this[$ .. $];
                }
            }
        }

        //Bidirectional range primitives
        static if (hasSlicing!Source && hasLength!Source)
        {
            /**
            Bidirectional range primitives. Provided only if both
            $(D hasSlicing!Source) and $(D hasLength!Source) are $(D true).
             */
            @property auto back()
            {
                assert(!empty, "back called on empty chunks");
                immutable len = _source.length;
                immutable start = (len - 1) / _chunkSize * _chunkSize;
                return _source[start .. len];
            }

            /// Ditto
            void popBack()
            {
                assert(!empty, "popBack() called on empty chunks");
                immutable end = (_source.length - 1) / _chunkSize * _chunkSize;
                _source = _source[0 .. end];
            }
        }

    private:
        Source _source;
        size_t _chunkSize;
    }
    else // is input range only
    {
        import std.typecons : RefCounted;

        static struct Chunk
        {
            private RefCounted!Impl impl;

            @property bool empty() { return impl.curSizeLeft == 0 || impl.r.empty; }
            @property auto front() { return impl.r.front; }
            void popFront()
            {
                assert(impl.curSizeLeft > 0 && !impl.r.empty);
                impl.curSizeLeft--;
                impl.r.popFront();
            }
        }

        static struct Impl
        {
            private Source r;
            private size_t chunkSize;
            private size_t curSizeLeft;
        }

        private RefCounted!Impl impl;

        private this(Source r, size_t chunkSize)
        {
            impl = RefCounted!Impl(r, r.empty ? 0 : chunkSize, chunkSize);
        }

        @property bool empty() { return impl.chunkSize == 0; }
        @property Chunk front() return { return Chunk(impl); }

        void popFront()
        {
            impl.curSizeLeft -= impl.r.popFrontN(impl.curSizeLeft);
            if (!impl.r.empty)
                impl.curSizeLeft = impl.chunkSize;
            else
                impl.chunkSize = 0;
        }

        static assert(isInputRange!(typeof(this)));
    }
}

/// Ditto
Chunks!Source chunks(Source)(Source source, size_t chunkSize)
if (isInputRange!Source)
{
    return typeof(return)(source, chunkSize);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    auto source = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    auto chunks = chunks(source, 4);
    assert(chunks[0] == [1, 2, 3, 4]);
    assert(chunks[1] == [5, 6, 7, 8]);
    assert(chunks[2] == [9, 10]);
    assert(chunks.back == chunks[2]);
    assert(chunks.front == chunks[0]);
    assert(chunks.length == 3);
    assert(equal(retro(array(chunks)), array(retro(chunks))));
}

/// Non-forward input ranges are supported, but with limited semantics.
@system /*@safe*/ unittest // FIXME: can't be @safe because RefCounted isn't.
{
    import std.algorithm.comparison : equal;

    int i;

    // The generator doesn't save state, so it cannot be a forward range.
    auto inputRange = generate!(() => ++i).take(10);

    // We can still process it in chunks, but it will be single-pass only.
    auto chunked = inputRange.chunks(2);

    assert(chunked.front.equal([1, 2]));
    assert(chunked.front.empty); // Iterating the chunk has consumed it
    chunked.popFront;
    assert(chunked.front.equal([3, 4]));
}

@system /*@safe*/ unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : ReferenceInputRange;

    auto data = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
    auto r = new ReferenceInputRange!int(data).chunks(3);
    assert(r.equal!equal([
        [ 1, 2, 3 ],
        [ 4, 5, 6 ],
        [ 7, 8, 9 ],
        [ 10 ]
    ]));

    auto data2 = [ 1, 2, 3, 4, 5, 6 ];
    auto r2 = new ReferenceInputRange!int(data2).chunks(3);
    assert(r2.equal!equal([
        [ 1, 2, 3 ],
        [ 4, 5, 6 ]
    ]));

    auto data3 = [ 1, 2, 3, 4, 5 ];
    auto r3 = new ReferenceInputRange!int(data3).chunks(2);
    assert(r3.front.equal([1, 2]));
    r3.popFront();
    assert(!r3.empty);
    r3.popFront();
    assert(r3.front.equal([5]));
}

@safe unittest
{
    auto source = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    auto chunks = chunks(source, 4);
    auto chunks2 = chunks.save;
    chunks.popFront();
    assert(chunks[0] == [5, 6, 7, 8]);
    assert(chunks[1] == [9, 10]);
    chunks2.popBack();
    assert(chunks2[1] == [5, 6, 7, 8]);
    assert(chunks2.length == 2);

    static assert(isRandomAccessRange!(typeof(chunks)));
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    //Extra toying with slicing and indexing.
    auto chunks1 = [0, 0, 1, 1, 2, 2, 3, 3, 4].chunks(2);
    auto chunks2 = [0, 0, 1, 1, 2, 2, 3, 3, 4, 4].chunks(2);

    assert(chunks1.length == 5);
    assert(chunks2.length == 5);
    assert(chunks1[4] == [4]);
    assert(chunks2[4] == [4, 4]);
    assert(chunks1.back == [4]);
    assert(chunks2.back == [4, 4]);

    assert(chunks1[0 .. 1].equal([[0, 0]]));
    assert(chunks1[0 .. 2].equal([[0, 0], [1, 1]]));
    assert(chunks1[4 .. 5].equal([[4]]));
    assert(chunks2[4 .. 5].equal([[4, 4]]));

    assert(chunks1[0 .. 0].equal((int[][]).init));
    assert(chunks1[5 .. 5].equal((int[][]).init));
    assert(chunks2[5 .. 5].equal((int[][]).init));

    //Fun with opDollar
    assert(chunks1[$ .. $].equal((int[][]).init)); //Quick
    assert(chunks2[$ .. $].equal((int[][]).init)); //Quick
    assert(chunks1[$ - 1 .. $].equal([[4]]));      //Semiquick
    assert(chunks2[$ - 1 .. $].equal([[4, 4]]));   //Semiquick
    assert(chunks1[$ .. 5].equal((int[][]).init)); //Semiquick
    assert(chunks2[$ .. 5].equal((int[][]).init)); //Semiquick

    assert(chunks1[$ / 2 .. $ - 1].equal([[2, 2], [3, 3]])); //Slow
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter;

    //ForwardRange
    auto r = filter!"true"([1, 2, 3, 4, 5]).chunks(2);
    assert(equal!"equal(a, b)"(r, [[1, 2], [3, 4], [5]]));

    //InfiniteRange w/o RA
    auto fibsByPairs = recurrence!"a[n-1] + a[n-2]"(1, 1).chunks(2);
    assert(equal!`equal(a, b)`(fibsByPairs.take(2),         [[ 1,  1], [ 2,  3]]));

    //InfiniteRange w/ RA and slicing
    auto odds = sequence!("a[0] + n * a[1]")(1, 2);
    auto oddsByPairs = odds.chunks(2);
    assert(equal!`equal(a, b)`(oddsByPairs.take(2),         [[ 1,  3], [ 5,  7]]));

    //Requires phobos#991 for Sequence to have slice to end
    static assert(hasSlicing!(typeof(odds)));
    assert(equal!`equal(a, b)`(oddsByPairs[3 .. 5],         [[13, 15], [17, 19]]));
    assert(equal!`equal(a, b)`(oddsByPairs[3 .. $].take(2), [[13, 15], [17, 19]]));
}



/**
This range splits a $(D source) range into $(D chunkCount) chunks of
approximately equal length. $(D Source) must be a forward range with
known length.

Unlike $(LREF chunks), $(D evenChunks) takes a chunk count (not size).
The returned range will contain zero or more $(D source.length /
chunkCount + 1) elements followed by $(D source.length / chunkCount)
elements. If $(D source.length < chunkCount), some chunks will be empty.

$(D chunkCount) must not be zero, unless $(D source) is also empty.
*/
struct EvenChunks(Source)
if (isForwardRange!Source && hasLength!Source)
{
    /// Standard constructor
    this(Source source, size_t chunkCount)
    {
        assert(chunkCount != 0 || source.empty, "Cannot create EvenChunks with a zero chunkCount");
        _source = source;
        _chunkCount = chunkCount;
    }

    /// Forward range primitives. Always present.
    @property auto front()
    {
        assert(!empty, "Attempting to fetch the front of an empty evenChunks");
        return _source.save.take(_chunkPos(1));
    }

    /// Ditto
    void popFront()
    {
        assert(!empty, "Attempting to popFront an empty evenChunks");
        _source.popFrontN(_chunkPos(1));
        _chunkCount--;
    }

    /// Ditto
    @property bool empty()
    {
        return _source.empty;
    }

    /// Ditto
    @property typeof(this) save()
    {
        return typeof(this)(_source.save, _chunkCount);
    }

    /// Length
    @property size_t length() const
    {
        return _chunkCount;
    }
    //Note: No point in defining opDollar here without slicing.
    //opDollar is defined below in the hasSlicing!Source section

    static if (hasSlicing!Source)
    {
        /**
        Indexing, slicing and bidirectional operations and range primitives.
        Provided only if $(D hasSlicing!Source) is $(D true).
         */
        auto opIndex(size_t index)
        {
            assert(index < _chunkCount, "evenChunks index out of bounds");
            return _source[_chunkPos(index) .. _chunkPos(index+1)];
        }

        /// Ditto
        typeof(this) opSlice(size_t lower, size_t upper)
        {
            assert(lower <= upper && upper <= length, "evenChunks slicing index out of bounds");
            return evenChunks(_source[_chunkPos(lower) .. _chunkPos(upper)], upper - lower);
        }

        /// Ditto
        @property auto back()
        {
            assert(!empty, "back called on empty evenChunks");
            return _source[_chunkPos(_chunkCount - 1) .. _source.length];
        }

        /// Ditto
        void popBack()
        {
            assert(!empty, "popBack() called on empty evenChunks");
            _source = _source[0 .. _chunkPos(_chunkCount - 1)];
            _chunkCount--;
        }
    }

private:
    Source _source;
    size_t _chunkCount;

    size_t _chunkPos(size_t i)
    {
        /*
            _chunkCount = 5, _source.length = 13:

               chunk0
                 |   chunk3
                 |     |
                 v     v
                +-+-+-+-+-+   ^
                |0|3|.| | |   |
                +-+-+-+-+-+   | div
                |1|4|.| | |   |
                +-+-+-+-+-+   v
                |2|5|.|
                +-+-+-+

                <----->
                  mod

                <--------->
                _chunkCount

            One column is one chunk.
            popFront and popBack pop the left-most
            and right-most column, respectively.
        */

        auto div = _source.length / _chunkCount;
        auto mod = _source.length % _chunkCount;
        auto pos = i <= mod
            ? i   * (div+1)
            : mod * (div+1) + (i-mod) * div
        ;
        //auto len = i < mod
        //    ? div+1
        //    : div
        //;
        return pos;
    }
}

/// Ditto
EvenChunks!Source evenChunks(Source)(Source source, size_t chunkCount)
if (isForwardRange!Source && hasLength!Source)
{
    return typeof(return)(source, chunkCount);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    auto source = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    auto chunks = evenChunks(source, 3);
    assert(chunks[0] == [1, 2, 3, 4]);
    assert(chunks[1] == [5, 6, 7]);
    assert(chunks[2] == [8, 9, 10]);
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    auto source = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    auto chunks = evenChunks(source, 3);
    assert(chunks.back == chunks[2]);
    assert(chunks.front == chunks[0]);
    assert(chunks.length == 3);
    assert(equal(retro(array(chunks)), array(retro(chunks))));

    auto chunks2 = chunks.save;
    chunks.popFront();
    assert(chunks[0] == [5, 6, 7]);
    assert(chunks[1] == [8, 9, 10]);
    chunks2.popBack();
    assert(chunks2[1] == [5, 6, 7]);
    assert(chunks2.length == 2);

    static assert(isRandomAccessRange!(typeof(chunks)));
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    int[] source = [];
    auto chunks = source.evenChunks(0);
    assert(chunks.length == 0);
    chunks = source.evenChunks(3);
    assert(equal(chunks, [[], [], []]));
    chunks = [1, 2, 3].evenChunks(5);
    assert(equal(chunks, [[1], [2], [3], [], []]));
}

/*
A fixed-sized sliding window iteration
of size `windowSize` over a `source` range by a custom `stepSize`.

The `Source` range must be at least an `ForwardRange` and
the `windowSize` must be greater than zero.

For `windowSize = 1` it splits the range into single element groups (aka `unflatten`)
For `windowSize = 2` it is similar to `zip(source, source.save.dropOne)`.

Params:
    f = If `Yes.withFewerElements` slide with fewer
        elements than `windowSize`. This can only happen if the initial range
        contains less elements than `windowSize`. In this case
        if `No.withFewerElements` an empty range will be returned.
    source = Range from which the slide will be selected
    windowSize = Sliding window size
    stepSize = Steps between the windows (by default 1)

Returns: Range of all sliding windows with propagated bi-directionality,
         forwarding, conditional random access, and slicing.

See_Also: $(LREF chunks)
*/
// Explicitly set to private to delay the release until 2.076
private
auto slide(Flag!"withFewerElements" f = Yes.withFewerElements,
            Source)(Source source, size_t windowSize, size_t stepSize = 1)
    if (isForwardRange!Source)
{
    return Slides!(f, Source)(source, windowSize, stepSize);
}

private struct Slides(Flag!"withFewerElements" withFewerElements = Yes.withFewerElements, Source)
    if (isForwardRange!Source)
{
private:
    Source _source;
    size_t _windowSize;
    size_t _stepSize;

    static if (hasLength!Source)
    {
        enum needsEndTracker = false;
    }
    else
    {
        // if there's no information about the length, track needs to be kept manually
        Source _nextSource;
        enum needsEndTracker = true;
    }

    bool _empty;

    static if (hasSlicing!Source)
    {
        enum hasSliceToEnd = hasSlicing!Source && is(typeof(Source.init[0 .. $]) == Source);
    }

public:
    /// Standard constructor
    this(Source source, size_t windowSize, size_t stepSize)
    {
        assert(windowSize > 0, "windowSize must be greater than zero");
        assert(stepSize > 0, "stepSize must be greater than zero");
        _source = source;
        _windowSize = windowSize;
        _stepSize = stepSize;

        static if (needsEndTracker)
        {
            // _nextSource is used to "look into the future" and check for the end
            _nextSource = source.save;
            _nextSource.popFrontN(windowSize);
        }

        static if (!withFewerElements)
        {
            // empty source range is needed, s.t. length, slicing etc. works properly
            static if (needsEndTracker)
            {
                if (_nextSource.empty)
                    _source = _nextSource;
            }
            else
            {
                if (_source.length < windowSize)
                {
                    static if (hasSlicing!Source)
                    {
                        // if possible use the faster opDollar overload
                        static if (hasSliceToEnd)
                            _source = _source[$ .. $];
                        else
                            _source = _source[_source.length .. _source.length];
                    }
                    else
                    {
                        _source.popFrontN(_source.length);
                    }
                }
            }
        }

        _empty = _source.empty;
    }

    /// Forward range primitives. Always present.
    @property auto front()
    {
        assert(!empty, "Attempting to access front on an empty slide");
        static if (hasSlicing!Source && hasLength!Source)
        {
            import std.algorithm.comparison : min;
            return _source[0 .. min(_windowSize, _source.length)];
        }
        else
        {
            return _source.save.take(_windowSize);
        }
    }

    /// Ditto
    void popFront()
    {
        assert(!empty, "Attempting to call popFront() on an empty slide");
        _source.popFrontN(_stepSize);

        // if the range has less elements than its window size,
        // we have seen the last full window (i.e. its empty)
        static if (needsEndTracker)
        {
            if (_nextSource.empty)
                _empty = true;
            else
                _nextSource.popFrontN(_stepSize);
        }
        else
        {
            if (_source.length < _windowSize)
                _empty = true;
        }
    }

    static if (!isInfinite!Source)
    {
        /// Ditto
        @property bool empty() const
        {
            return _empty;
        }
    }
    else
    {
        // undocumented
        enum empty = false;
    }

    /// Ditto
    @property typeof(this) save()
    {
        return typeof(this)(_source.save, _windowSize, _stepSize);
    }

    static if (hasLength!Source)
    {
        /// Length. Only if $(D hasLength!Source) is $(D true)
        @property size_t length()
        {
            if (_source.length < _windowSize)
            {
                static if (withFewerElements)
                    return 1;
                else
                    return 0;
            }
            else
            {
                return (_source.length - _windowSize + _stepSize) / _stepSize;
            }
        }
    }

    static if (hasSlicing!Source)
    {
        /**
        Indexing and slicing operations. Provided only if
        `hasSlicing!Source` is `true`.
         */
        auto opIndex(size_t index)
        {
            immutable start = index * _stepSize;

            static if (isInfinite!Source)
            {
                immutable end = start + _windowSize;
            }
            else
            {
                import std.algorithm.comparison : min;

                immutable len = _source.length;
                assert(start < len, "slide index out of bounds");
                immutable end = min(start + _windowSize, len);
            }

            return _source[start .. end];
        }

        static if (!isInfinite!Source)
        {
            /// ditto
            typeof(this) opSlice(size_t lower, size_t upper)
            {
                import std.algorithm.comparison : min;
                assert(lower <= upper && upper <= length, "slide slicing index out of bounds");

                lower *= _stepSize;
                upper *= _stepSize;

                immutable len = _source.length;

                /*
                * Notice that we only need to move for windowSize - 1 to the right:
                * source = [0, 1, 2, 3] (length: 4)
                * - source.slide(2) -> s = [[0, 1], [1, 2], [2, 3]]
                *   right pos for s[0 .. 3]: 3 (upper) + 2 (windowSize) - 1 = 4
                *
                * - source.slide(3) -> s = [[0, 1, 2], [1, 2, 3]]
                *   right pos for s[0 .. 2]: 2 (upper) + 3 (windowSize) - 1 = 4
                *
                * source = [0, 1, 2, 3, 4] (length: 5)
                * - source.slide(4) -> s = [[0, 1, 2, 3], [1, 2, 3, 4]]
                *   right pos for s[0 .. 2]: 2 (upper) + 4 (windowSize) - 1 = 5
                */
                return typeof(this)
                    (_source[min(lower, len) .. min(upper + _windowSize - 1, len)],
                     _windowSize, _stepSize);
            }
        }
        else static if (hasSliceToEnd)
        {
            //For slicing an infinite chunk, we need to slice the source to the infinite end.
            auto opSlice(size_t lower, size_t upper)
            {
                assert(lower <= upper, "slide slicing index out of bounds");
                return typeof(this)(_source[lower * _stepSize .. $],
                       _windowSize, _stepSize).takeExactly(upper - lower);
            }
        }

        static if (isInfinite!Source)
        {
            static if (hasSliceToEnd)
            {
                private static struct DollarToken{}
                DollarToken opDollar()
                {
                    return DollarToken();
                }
                //Slice to dollar
                typeof(this) opSlice(size_t lower, DollarToken)
                {
                    return typeof(this)(_source[lower * _stepSize .. $], _windowSize, _stepSize);
                }
            }
        }
        else
        {
            //Dollar token carries a static type, with no extra information.
            //It can lazily transform into _source.length on algorithmic
            //operations such as : slide[$/2, $-1];
            private static struct DollarToken
            {
                private size_t _length;
                alias _length this;
            }

            DollarToken opDollar()
            {
                return DollarToken(this.length);
            }

            // Optimized slice overloads optimized for using dollar.
            typeof(this) opSlice(DollarToken, DollarToken)
            {
                static if (hasSliceToEnd)
                {
                    return typeof(this)(_source[$ .. $], _windowSize, _stepSize);
                }
                else
                {
                    immutable len = _source.length;
                    return typeof(this)(_source[len .. len], _windowSize, _stepSize);
                }
            }

            // Optimized slice overloads optimized for using dollar.
            typeof(this) opSlice(size_t lower, DollarToken)
            {
                import std.algorithm.comparison : min;
                assert(lower <= length, "slide slicing index out of bounds");
                lower *= _stepSize;
                static if (hasSliceToEnd)
                {
                    return typeof(this)(_source[min(lower, _source.length) .. $], _windowSize, _stepSize);
                }
                else
                {
                    immutable len = _source.length;
                    return typeof(this)(_source[min(lower, len) .. len], _windowSize, _stepSize);
                }
            }

            // Optimized slice overloads optimized for using dollar.
            typeof(this) opSlice(DollarToken, size_t upper)
            {
                assert(upper == length, "slide slicing index out of bounds");
                return this[$ .. $];
            }
        }

        // Bidirectional range primitives
        static if (!isInfinite!Source)
        {
            /**
            Bidirectional range primitives. Provided only if both
            `hasSlicing!Source` and `!isInfinite!Source` are `true`.
             */
            @property auto back()
            {
                import std.algorithm.comparison : max;

                assert(!empty, "Attempting to access front on an empty slide");

                immutable len = _source.length;
                /*
                * Note:
                * - `end` in the following is the exclusive end as used in opSlice
                * - For the trivial case with `stepSize = 1`  `end` is at `len`:
                *
                *    iota(4).slide(2) = [[0, 1], [1, 2], [2, 3]    (end = 4)
                *    iota(4).slide(3) = [[0, 1, 2], [1, 2, 3]]     (end = 4)
                *
                * - For the non-trivial cases, we need to calculate the gap
                *   between `len` and `end` - this is the number of missing elements
                *   from the input range:
                *
                *    iota(7).slide(2, 3) = [[0, 1], [3, 4]] || <gap: 2> 6
                *    iota(7).slide(2, 4) = [[0, 1], [4, 5]] || <gap: 1> 6
                *    iota(7).slide(1, 5) = [[0], [5]]       || <gap: 1> 6
                *
                *   As it can be seen `gap` can be at most `stepSize - 1`
                *   More generally the elements of the sliding window with
                *   `w = windowSize` and `s = stepSize` are:
                *
                *     [0, w], [s, s + w], [2 * s, 2 * s + w], ... [n * s, n * s + w]
                *
                *  We can thus calculate the gap between the `end` and `len` as:
                *
                *     gap = len - (n * s + w) = len - w - (n * s)
                *
                *  As we aren't interested in exact value of `n`, but the best
                *  minimal `gap` value, we can use modulo to "cut" `len - w` optimally:
                *
                *     gap = len - w - (s - s ... - s) = (len - w) % s
                *
                *  So for example:
                *
                *    iota(7).slide(2, 3) = [[0, 1], [3, 4]]
                *      gap: (7 - 2) % 3 = 5 % 3 = 2
                *      end: 7 - 2 = 5
                *
                *    iota(7).slide(4, 2) = [[0, 1, 2, 3], [2, 3, 4, 5]]
                *      gap: (7 - 4) % 2 = 3 % 2 = 1
                *      end: 7 - 1 = 6
                */
                size_t gap = (len - _windowSize)  % _stepSize;

                // check for underflow
                immutable start = (len > _windowSize + gap) ? len - _windowSize - gap : 0;

                return _source[start .. len - gap];
            }

            /// Ditto
            void popBack()
            {
                assert(!empty, "Attempting to call popBack() on an empty slide");

                immutable end = _source.length > _stepSize ? _source.length - _stepSize : 0;
                _source = _source[0 .. end];

                if (_source.length < _windowSize)
                    _empty = true;
            }
        }
    }
}

//
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.array : array;

    assert([0, 1, 2, 3].slide(2).equal!equal(
        [[0, 1], [1, 2], [2, 3]]
    ));
    assert(5.iota.slide(3).equal!equal(
        [[0, 1, 2], [1, 2, 3], [2, 3, 4]]
    ));

    assert(iota(7).slide(2, 2).equal!equal([[0, 1], [2, 3], [4, 5]]));
    assert(iota(12).slide(2, 4).equal!equal([[0, 1], [4, 5], [8, 9]]));

    // set a custom stepsize (default 1)
    assert(6.iota.slide(1, 2).equal!equal(
        [[0], [2], [4]]
    ));

    assert(6.iota.slide(2, 4).equal!equal(
        [[0, 1], [4, 5]]
    ));

    // allow slide with less elements than the window size
    assert(3.iota.slide!(No.withFewerElements)(4).empty);
    assert(3.iota.slide!(Yes.withFewerElements)(4).equal!equal(
        [[0, 1, 2]]
    ));
}

// count k-mers
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : each;

    int[dstring] d;
    "AGAGA"d.slide(2).each!(a => d[a]++);
    assert(d == ["AG"d: 2, "GA"d: 2]);
}

// @nogc
@safe pure nothrow @nogc unittest
{
    import std.algorithm.comparison : equal;

    static immutable res1 = [[0], [1], [2], [3]];
    assert(4.iota.slide(1).equal!equal(res1));

    static immutable res2 = [[0, 1], [1, 2], [2, 3]];
    assert(4.iota.slide(2).equal!equal(res2));
}

// different window sizes
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.array : array;

    assert([0, 1, 2, 3].slide(1).array == [[0], [1], [2], [3]]);
    assert([0, 1, 2, 3].slide(2).array == [[0, 1], [1, 2], [2, 3]]);
    assert([0, 1, 2, 3].slide(3).array == [[0, 1, 2], [1, 2, 3]]);
    assert([0, 1, 2, 3].slide(4).array == [[0, 1, 2, 3]]);
    assert([0, 1, 2, 3].slide(5).array == [[0, 1, 2, 3]]);


    assert(iota(2).slide(2).front.equal([0, 1]));
    assert(iota(3).slide(2).equal!equal([[0, 1],[1, 2]]));
    assert(iota(3).slide(3).equal!equal([[0, 1, 2]]));
    assert(iota(3).slide(4).equal!equal([[0, 1, 2]]));
    assert(iota(1, 4).slide(1).equal!equal([[1], [2], [3]]));
    assert(iota(1, 4).slide(3).equal!equal([[1, 2, 3]]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    assert(6.iota.slide(1, 1).equal!equal(
        [[0], [1], [2], [3], [4], [5]]
    ));
    assert(6.iota.slide(1, 2).equal!equal(
        [[0], [2], [4]]
    ));
    assert(6.iota.slide(1, 3).equal!equal(
        [[0], [3]]
    ));
    assert(6.iota.slide(1, 4).equal!equal(
        [[0], [4]]
    ));
    assert(6.iota.slide(1, 5).equal!equal(
        [[0], [5]]
    ));
    assert(6.iota.slide(2, 1).equal!equal(
        [[0, 1], [1, 2], [2, 3], [3, 4], [4, 5]]
    ));
    assert(6.iota.slide(2, 2).equal!equal(
        [[0, 1], [2, 3], [4, 5]]
    ));
    assert(6.iota.slide(2, 3).equal!equal(
        [[0, 1], [3, 4]]
    ));
    assert(6.iota.slide(2, 4).equal!equal(
        [[0, 1], [4, 5]]
    ));
    assert(6.iota.slide(2, 5).equal!equal(
        [[0, 1]]
    ));
    assert(6.iota.slide(3, 1).equal!equal(
        [[0, 1, 2], [1, 2, 3], [2, 3, 4], [3, 4, 5]]
    ));
    assert(6.iota.slide(3, 2).equal!equal(
        [[0, 1, 2], [2, 3, 4]]
    ));
    assert(6.iota.slide(3, 3).equal!equal(
        [[0, 1, 2], [3, 4, 5]]
    ));
    assert(6.iota.slide(3, 4).equal!equal(
        [[0, 1, 2]]
    ));
    assert(6.iota.slide(4, 1).equal!equal(
        [[0, 1, 2, 3], [1, 2, 3, 4], [2, 3, 4, 5]]
    ));
    assert(6.iota.slide(4, 2).equal!equal(
        [[0, 1, 2, 3], [2, 3, 4, 5]]
    ));
    assert(6.iota.slide(4, 3).equal!equal(
        [[0, 1, 2, 3]]
    ));
    assert(6.iota.slide(5, 1).equal!equal(
        [[0, 1, 2, 3, 4], [1, 2, 3, 4, 5]]
    ));
    assert(6.iota.slide(5, 2).equal!equal(
        [[0, 1, 2, 3, 4]]
    ));
    assert(6.iota.slide(5, 3).equal!equal(
        [[0, 1, 2, 3, 4]]
    ));
}

// emptyness, copyability, strings
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : each, map;

    // check with empty input
    int[] d;
    assert(d.slide(2).empty);
    assert(d.slide(2, 2).empty);

    // is copyable?
    auto e = iota(5).slide(2);
    e.popFront;
    assert(e.save.equal!equal([[1, 2], [2, 3], [3, 4]]));
    assert(e.save.equal!equal([[1, 2], [2, 3], [3, 4]]));
    assert(e.map!"a.array".array == [[1, 2], [2, 3], [3, 4]]);

    // test with strings
    int[dstring] f;
    "AGAGA"d.slide(3).each!(a => f[a]++);
    assert(f == ["AGA"d: 2, "GAG"d: 1]);

    int[dstring] g;
    "ABCDEFG"d.slide(3, 3).each!(a => g[a]++);
    assert(g == ["ABC"d:1, "DEF"d:1]);
}

// test slicing, length
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.array : array;

    // test index
    assert(iota(3).slide(4)[0].equal([0, 1, 2]));
    assert(iota(5).slide(4)[1].equal([1, 2, 3, 4]));
    assert(iota(3).slide(4, 2)[0].equal([0, 1, 2]));
    assert(iota(5).slide(4, 2)[1].equal([2, 3, 4]));
    assert(iota(3).slide(4, 3)[0].equal([0, 1, 2]));
    assert(iota(5).slide(4, 3)[1].equal([3, 4,]));

    // test slicing
    assert(iota(3).slide(4)[0 .. $].equal!equal([[0, 1, 2]]));
    assert(iota(3).slide(2)[1 .. $].equal!equal([[1, 2]]));
    assert(iota(1, 5).slide(2)[0 .. 1].equal!equal([[1, 2]]));
    assert(iota(1, 5).slide(2)[0 .. 2].equal!equal([[1, 2], [2, 3]]));
    assert(iota(1, 5).slide(3)[0 .. 1].equal!equal([[1, 2, 3]]));
    assert(iota(1, 5).slide(3)[0 .. 2].equal!equal([[1, 2, 3], [2, 3, 4]]));
    assert(iota(1, 6).slide(3)[2 .. 3].equal!equal([[3, 4, 5]]));
    assert(iota(1, 5).slide(4)[0 .. 1].equal!equal([[1, 2, 3, 4]]));

    // length
    assert(iota(3).slide(1).length == 3);
    assert(iota(3).slide(1, 2).length == 2);
    assert(iota(3).slide(1, 3).length == 1);
    assert(iota(3).slide(1, 4).length == 1);
    assert(iota(3).slide(2).length == 2);
    assert(iota(3).slide(2, 2).length == 1);
    assert(iota(3).slide(2, 3).length == 1);
    assert(iota(3).slide(3).length == 1);
    assert(iota(3).slide(3, 2).length == 1);

    // opDollar
    assert(iota(3).slide(4)[$/2 .. $].equal!equal([[0, 1, 2]]));
    assert(iota(3).slide(4)[$ .. $].empty);
    assert(iota(3).slide(4)[$ .. 1].empty);

    assert(iota(5).slide(3, 1)[$/2 .. $].equal!equal([[1, 2, 3], [2, 3, 4]]));
    assert(iota(5).slide(3, 2)[$/2 .. $].equal!equal([[2, 3, 4]]));
    assert(iota(5).slide(3, 3)[$/2 .. $].equal!equal([[0, 1, 2]]));
    assert(iota(3).slide(4, 3)[$ .. $].empty);
    assert(iota(3).slide(4, 3)[$ .. 1].empty);
}

// test No.withFewerElements
@safe pure nothrow unittest
{
    assert(iota(3).slide(4).length == 1);
    assert(iota(3).slide(4, 4).length == 1);

    assert(iota(3).slide!(No.withFewerElements)(4).empty);
    assert(iota(3, 3).slide!(No.withFewerElements)(4).empty);
    assert(iota(3).slide!(No.withFewerElements)(4).length == 0);
    assert(iota(3).slide!(No.withFewerElements)(4, 4).length == 0);

    assert(iota(3).slide!(No.withFewerElements)(400).empty);
    assert(iota(3).slide!(No.withFewerElements)(400).length == 0);
    assert(iota(3).slide!(No.withFewerElements)(400, 10).length == 0);

    assert(iota(3).slide!(No.withFewerElements)(4)[0 .. $].empty);
    assert(iota(3).slide!(No.withFewerElements)(4)[$ .. $].empty);
    assert(iota(3).slide!(No.withFewerElements)(4)[$ .. 0].empty);
    assert(iota(3).slide!(No.withFewerElements)(4)[$/2 .. $].empty);

    // with different step sizes
    assert(iota(3).slide!(No.withFewerElements)(4, 5)[0 .. $].empty);
    assert(iota(3).slide!(No.withFewerElements)(4, 6)[$ .. $].empty);
    assert(iota(3).slide!(No.withFewerElements)(4, 7)[$ .. 0].empty);
    assert(iota(3).slide!(No.withFewerElements)(4, 8)[$/2 .. $].empty);
}

// test with infinite ranges
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;

    // InfiniteRange without RandomAccess
    auto fibs = recurrence!"a[n-1] + a[n-2]"(1, 1);
    assert(fibs.slide(2).take(2).equal!equal([[1,  1], [1,  2]]));
    assert(fibs.slide(2, 3).take(2).equal!equal([[1,  1], [3,  5]]));

    // InfiniteRange with RandomAccess and slicing
    auto odds = sequence!("a[0] + n * a[1]")(1, 2);
    auto oddsByPairs = odds.slide(2);
    assert(oddsByPairs.take(2).equal!equal([[ 1,  3], [ 3,  5]]));
    assert(oddsByPairs[1].equal([3, 5]));
    assert(oddsByPairs[4].equal([9, 11]));

    static assert(hasSlicing!(typeof(odds)));
    assert(oddsByPairs[3 .. 5].equal!equal([[7, 9], [9, 11]]));
    assert(oddsByPairs[3 .. $].take(2).equal!equal([[7, 9], [9, 11]]));

    auto oddsWithGaps = odds.slide(2, 4);
    assert(oddsWithGaps.take(3).equal!equal([[1, 3], [9, 11], [17, 19]]));
    assert(oddsWithGaps[2].equal([17, 19]));
    assert(oddsWithGaps[1 .. 3].equal!equal([[9, 11], [17, 19]]));
    assert(oddsWithGaps[1 .. $].take(2).equal!equal([[9, 11], [17, 19]]));
}

// test reverse
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;

    auto e = iota(3).slide(2);
    assert(e.retro.equal!equal([[1, 2], [0, 1]]));
    assert(e.retro.array.equal(e.array.retro));

    auto e2 = iota(5).slide(3);
    assert(e2.retro.equal!equal([[2, 3, 4], [1, 2, 3], [0, 1, 2]]));
    assert(e2.retro.array.equal(e2.array.retro));

    auto e3 = iota(3).slide(4);
    assert(e3.retro.equal!equal([[0, 1, 2]]));
    assert(e3.retro.array.equal(e3.array.retro));
}

// test reverse with different steps
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;

    assert(iota(7).slide(2, 1).retro.equal!equal(
        [[5, 6], [4, 5], [3, 4], [2, 3], [1, 2], [0, 1]]
    ));
    assert(iota(7).slide(2, 2).retro.equal!equal(
        [[4, 5], [2, 3], [0, 1]]
    ));
    assert(iota(7).slide(2, 3).retro.equal!equal(
        [[3, 4], [0, 1]]
    ));
    assert(iota(7).slide(2, 4).retro.equal!equal(
        [[4, 5], [0, 1]]
    ));
    assert(iota(7).slide(2, 5).retro.equal!equal(
        [[5, 6], [0, 1]]
    ));
    assert(iota(7).slide(3, 1).retro.equal!equal(
        [[4, 5, 6], [3, 4, 5], [2, 3, 4], [1, 2, 3], [0, 1, 2]]
    ));
    assert(iota(7).slide(3, 2).retro.equal!equal(
        [[4, 5, 6], [2, 3, 4], [0, 1, 2]]
    ));
    assert(iota(7).slide(4, 1).retro.equal!equal(
        [[3, 4, 5, 6], [2, 3, 4, 5], [1, 2, 3, 4], [0, 1, 2, 3]]
    ));
    assert(iota(7).slide(4, 2).retro.equal!equal(
        [[2, 3, 4, 5], [0, 1, 2, 3]]
    ));
    assert(iota(7).slide(4, 3).retro.equal!equal(
        [[3, 4, 5, 6], [0, 1, 2, 3]]
    ));
    assert(iota(7).slide(4, 4).retro.equal!equal(
        [[0, 1, 2, 3]]
    ));
    assert(iota(7).slide(5, 1).retro.equal!equal(
        [[2, 3, 4, 5, 6], [1, 2, 3, 4, 5], [0, 1, 2, 3, 4]]
    ));
    assert(iota(7).slide(5, 2).retro.equal!equal(
        [[2, 3, 4, 5, 6], [0, 1, 2, 3, 4]]
    ));
    assert(iota(7).slide(5, 3).retro.equal!equal(
        [[0, 1, 2, 3, 4]]
    ));
    assert(iota(7).slide(5, 4).retro.equal!equal(
        [[0, 1, 2, 3, 4]]
    ));
}

// step size
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;

    assert(iota(7).slide(2, 2).equal!equal([[0, 1], [2, 3], [4, 5]]));
    assert(iota(8).slide(2, 2).equal!equal([[0, 1], [2, 3], [4, 5], [6, 7]]));
    assert(iota(9).slide(2, 2).equal!equal([[0, 1], [2, 3], [4, 5], [6, 7]]));
    assert(iota(12).slide(2, 4).equal!equal([[0, 1], [4, 5], [8, 9]]));
    assert(iota(13).slide(2, 4).equal!equal([[0, 1], [4, 5], [8, 9]]));
}

// test with dummy ranges
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : DummyRange, Length, RangeType, ReturnBy, AllDummyRanges;
    import std.meta : AliasSeq;

    alias AllForwardDummyRanges = AliasSeq!(
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Forward),
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Bidirectional),
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random),
        DummyRange!(ReturnBy.Reference, Length.No, RangeType.Forward),
        DummyRange!(ReturnBy.Reference, Length.No, RangeType.Bidirectional),
        //DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Input),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Forward),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Bidirectional),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Random),
        //DummyRange!(ReturnBy.Value, Length.No, RangeType.Input),
        DummyRange!(ReturnBy.Value, Length.No, RangeType.Forward),
        DummyRange!(ReturnBy.Value, Length.No, RangeType.Bidirectional)
    );

    foreach (Range; AliasSeq!AllForwardDummyRanges)
    {
        Range r;
        assert(r.slide(1).equal!equal(
            [[1], [2], [3], [4], [5], [6], [7], [8], [9], [10]]
        ));
        assert(r.slide(2).equal!equal(
            [[1, 2], [2, 3], [3, 4], [4, 5], [5, 6], [6, 7], [7, 8], [8, 9], [9, 10]]
        ));
        assert(r.slide(3).equal!equal(
            [[1, 2, 3], [2, 3, 4], [3, 4, 5], [4, 5, 6],
            [5, 6, 7], [6, 7, 8], [7, 8, 9], [8, 9, 10]]
        ));
        assert(r.slide(6).equal!equal(
            [[1, 2, 3, 4, 5, 6], [2, 3, 4, 5, 6, 7], [3, 4, 5, 6, 7, 8],
            [4, 5, 6, 7, 8, 9], [5, 6, 7, 8, 9, 10]]
        ));
        assert(r.slide(15).equal!equal(
            [[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]]
        ));

        assert(r.slide!(No.withFewerElements)(15).empty);
    }

    alias BackwardsDummyRanges = AliasSeq!(
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Random),
    );

    foreach (Range; AliasSeq!BackwardsDummyRanges)
    {
        Range r;
        assert(r.slide(1).retro.equal!equal(
            [[10], [9], [8], [7], [6], [5], [4], [3], [2], [1]]
        ));
        assert(r.slide(2).retro.equal!equal(
            [[9, 10], [8, 9], [7, 8], [6, 7], [5, 6], [4, 5], [3, 4], [2, 3], [1, 2]]
        ));
        assert(r.slide(5).retro.equal!equal(
            [[6, 7, 8, 9, 10], [5, 6, 7, 8, 9], [4, 5, 6, 7, 8],
            [3, 4, 5, 6, 7], [2, 3, 4, 5, 6], [1, 2, 3, 4, 5]]
        ));
        assert(r.slide(15).retro.equal!equal(
            [[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]]
        ));

        // different step sizes
        assert(r.slide(2, 4)[2].equal([9, 10]));
        assert(r.slide(2, 1).equal!equal(
            [[1, 2], [2, 3], [3, 4], [4, 5], [5, 6], [6, 7], [7, 8], [8, 9], [9, 10]]
        ));
        assert(r.slide(2, 2).equal!equal(
            [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]
        ));
        assert(r.slide(2, 3).equal!equal(
            [[1, 2], [4, 5], [7, 8]]
        ));
        assert(r.slide(2, 4).equal!equal(
            [[1, 2], [5, 6], [9, 10]]
        ));

        // front = back
        foreach (windowSize; 1 .. 10)
            foreach (stepSize; 1 .. 10)
            {
                auto slider = r.slide(windowSize, stepSize);
                assert(slider.retro.retro.equal!equal(slider));
            }
    }

    assert(iota(1, 12).slide(2, 4)[0 .. 3].equal!equal([[1, 2], [5, 6], [9, 10]]));
    assert(iota(1, 12).slide(2, 4)[0 .. $].equal!equal([[1, 2], [5, 6], [9, 10]]));
    assert(iota(1, 12).slide(2, 4)[$/2 .. $].equal!equal([[5, 6], [9, 10]]));

    // reverse
    assert(iota(1, 12).slide(2, 4).retro.equal!equal([[9, 10], [5, 6], [1, 2]]));
}

// test different sliceable ranges
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : DummyRange, Length, RangeType, ReturnBy;
    import std.meta : AliasSeq;

    struct SliceableRange(Range, Flag!"withOpDollar" withOpDollar = No.withOpDollar,
                                 Flag!"withInfiniteness" withInfiniteness = No.withInfiniteness)
    {
        Range arr = 10.iota.array; // similar to DummyRange
        @property auto save() { return typeof(this)(arr); }
        @property auto front() { return arr[0]; }
        void popFront() { arr.popFront(); }
        auto opSlice(size_t i, size_t j)
        {
            // subslices can't be infinite
            return SliceableRange!(Range, withOpDollar, No.withInfiniteness)(arr[i .. j]);
        }

        static if (withInfiniteness)
        {
            enum empty = false;
        }
        else
        {
            @property bool empty() { return arr.empty; }
            @property auto length() { return arr.length; }
        }

        static if (withOpDollar)
        {
            static if (withInfiniteness)
            {
                struct Dollar {}
                Dollar opDollar() const { return Dollar.init; }

                //Slice to dollar
                typeof(this) opSlice(size_t lower, Dollar)
                {
                    return typeof(this)(arr[lower .. $]);
                }

            }
            else
            {
                alias opDollar = length;
            }
        }
    }

    alias T = int[];

    alias SliceableDummyRanges = AliasSeq!(
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random, T),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Random, T),
        SliceableRange!(T, No.withOpDollar, No.withInfiniteness),
        SliceableRange!(T, Yes.withOpDollar, No.withInfiniteness),
        SliceableRange!(T, Yes.withOpDollar, Yes.withInfiniteness),
    );

    foreach (Range; AliasSeq!SliceableDummyRanges)
    {
        Range r;
        r.arr = 10.iota.array; // for clarity

        static assert(isForwardRange!Range);
        enum hasSliceToEnd = hasSlicing!Range && is(typeof(Range.init[0 .. $]) == Range);

        assert(r.slide(2)[0].equal([0, 1]));
        assert(r.slide(2)[1].equal([1, 2]));

        // saveable
        auto s = r.slide(2);
        assert(s[0 .. 2].equal!equal([[0, 1], [1, 2]]));
        s.save.popFront;
        assert(s[0 .. 2].equal!equal([[0, 1], [1, 2]]));

        assert(r.slide(3)[1 .. 3].equal!equal([[1, 2, 3], [2, 3, 4]]));
    }

    alias SliceableDummyRangesWithoutInfinity = AliasSeq!(
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random, T),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Random, T),
        SliceableRange!(T, No.withOpDollar, No.withInfiniteness),
        SliceableRange!(T, Yes.withOpDollar, No.withInfiniteness),
    );

    foreach (Range; AliasSeq!SliceableDummyRangesWithoutInfinity)
    {
        static assert(hasSlicing!Range);
        static assert(hasLength!Range);

        Range r;
        r.arr = 10.iota.array; // for clarity

        assert(r.slide!(No.withFewerElements)(6).equal!equal(
            [[0, 1, 2, 3, 4, 5], [1, 2, 3, 4, 5, 6], [2, 3, 4, 5, 6, 7],
            [3, 4, 5, 6, 7, 8], [4, 5, 6, 7, 8, 9]]
        ));
        assert(r.slide!(No.withFewerElements)(16).empty);

        assert(r.slide(4)[0 .. $].equal(r.slide(4)));
        assert(r.slide(2)[$/2 .. $].equal!equal([[4, 5], [5, 6], [6, 7], [7, 8], [8, 9]]));
        assert(r.slide(2)[$ .. $].empty);

        assert(r.slide(3).retro.equal!equal(
            [[7, 8, 9], [6, 7, 8], [5, 6, 7], [4, 5, 6], [3, 4, 5], [2, 3, 4], [1, 2, 3], [0, 1, 2]]
        ));
    }

    // separate checks for infinity
    auto infIndex = SliceableRange!(T, No.withOpDollar, Yes.withInfiniteness)([0, 1, 2, 3]);
    assert(infIndex.slide(2)[0].equal([0, 1]));
    assert(infIndex.slide(2)[1].equal([1, 2]));

    auto infDollar = SliceableRange!(T, Yes.withOpDollar, Yes.withInfiniteness)();
    assert(infDollar.slide(2)[1 .. $].front.equal([1, 2]));
    assert(infDollar.slide(4)[0 .. $].front.equal([0, 1, 2, 3]));
    assert(infDollar.slide(4)[2 .. $].front.equal([2, 3, 4, 5]));
}

private struct OnlyResult(T, size_t arity)
{
    private this(Values...)(auto ref Values values)
    {
        this.data = [values];
        this.backIndex = arity;
    }

    bool empty() @property
    {
        return frontIndex >= backIndex;
    }

    T front() @property
    {
        assert(!empty, "Attempting to fetch the front of an empty Only range");
        return data[frontIndex];
    }

    void popFront()
    {
        assert(!empty, "Attempting to popFront an empty Only range");
        ++frontIndex;
    }

    T back() @property
    {
        assert(!empty, "Attempting to fetch the back of an empty Only range");
        return data[backIndex - 1];
    }

    void popBack()
    {
        assert(!empty, "Attempting to popBack an empty Only range");
        --backIndex;
    }

    OnlyResult save() @property
    {
        return this;
    }

    size_t length() const @property
    {
        return backIndex - frontIndex;
    }

    alias opDollar = length;

    T opIndex(size_t idx)
    {
        // when i + idx points to elements popped
        // with popBack
        assert(idx < length, "Attempting to fetch an out of bounds index from an Only range");
        return data[frontIndex + idx];
    }

    OnlyResult opSlice()
    {
        return this;
    }

    OnlyResult opSlice(size_t from, size_t to)
    {
        OnlyResult result = this;
        result.frontIndex += from;
        result.backIndex = this.frontIndex + to;
        assert(
            from <= to,
            "Attempting to slice an Only range with a larger first argument than the second."
        );
        assert(
            to <= length,
            "Attempting to slice using an out of bounds index on an Only range"
        );
        return result;
    }

    private size_t frontIndex = 0;
    private size_t backIndex = 0;

    // @@@BUG@@@ 10643
    version (none)
    {
        import std.traits : hasElaborateAssign;
        static if (hasElaborateAssign!T)
            private T[arity] data;
        else
            private T[arity] data = void;
    }
    else
        private T[arity] data;
}

// Specialize for single-element results
private struct OnlyResult(T, size_t arity : 1)
{
    @property T front()
    {
        assert(!empty, "Attempting to fetch the front of an empty Only range");
        return _value;
    }
    @property T back()
    {
        assert(!empty, "Attempting to fetch the back of an empty Only range");
        return _value;
    }
    @property bool empty() const { return _empty; }
    @property size_t length() const { return !_empty; }
    @property auto save() { return this; }
    void popFront()
    {
        assert(!_empty, "Attempting to popFront an empty Only range");
        _empty = true;
    }
    void popBack()
    {
        assert(!_empty, "Attempting to popBack an empty Only range");
        _empty = true;
    }
    alias opDollar = length;

    private this()(auto ref T value)
    {
        this._value = value;
        this._empty = false;
    }

    T opIndex(size_t i)
    {
        assert(!_empty && i == 0, "Attempting to fetch an out of bounds index from an Only range");
        return _value;
    }

    OnlyResult opSlice()
    {
        return this;
    }

    OnlyResult opSlice(size_t from, size_t to)
    {
        assert(
            from <= to,
            "Attempting to slice an Only range with a larger first argument than the second."
        );
        assert(
            to <= length,
            "Attempting to slice using an out of bounds index on an Only range"
        );
        OnlyResult copy = this;
        copy._empty = _empty || from == to;
        return copy;
    }

    private Unqual!T _value;
    private bool _empty = true;
}

// Specialize for the empty range
private struct OnlyResult(T, size_t arity : 0)
{
    private static struct EmptyElementType {}

    bool empty() @property { return true; }
    size_t length() const @property { return 0; }
    alias opDollar = length;
    EmptyElementType front() @property { assert(false); }
    void popFront() { assert(false); }
    EmptyElementType back() @property { assert(false); }
    void popBack() { assert(false); }
    OnlyResult save() @property { return this; }

    EmptyElementType opIndex(size_t i)
    {
        assert(false);
    }

    OnlyResult opSlice() { return this; }

    OnlyResult opSlice(size_t from, size_t to)
    {
        assert(from == 0 && to == 0);
        return this;
    }
}

/**
Assemble $(D values) into a range that carries all its
elements in-situ.

Useful when a single value or multiple disconnected values
must be passed to an algorithm expecting a range, without
having to perform dynamic memory allocation.

As copying the range means copying all elements, it can be
safely returned from functions. For the same reason, copying
the returned range may be expensive for a large number of arguments.

Params:
    values = the values to assemble together

Returns:
    A `RandomAccessRange` of the assembled values.

See_Also: $(LREF chain) to chain ranges
 */
auto only(Values...)(auto ref Values values)
if (!is(CommonType!Values == void) || Values.length == 0)
{
    return OnlyResult!(CommonType!Values, Values.length)(values);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter, joiner, map;
    import std.algorithm.searching : findSplitBefore;
    import std.uni : isUpper;

    assert(equal(only('♡'), "♡"));
    assert([1, 2, 3, 4].findSplitBefore(only(3))[0] == [1, 2]);

    assert(only("one", "two", "three").joiner(" ").equal("one two three"));

    string title = "The D Programming Language";
    assert(title
        .filter!isUpper // take the upper case letters
        .map!only       // make each letter its own range
        .joiner(".")    // join the ranges together lazily
        .equal("T.D.P.L"));
}

@safe unittest
{
    // Verify that the same common type and same arity
    // results in the same template instantiation
    static assert(is(typeof(only(byte.init, int.init)) ==
        typeof(only(int.init, byte.init))));

    static assert(is(typeof(only((const(char)[]).init, string.init)) ==
        typeof(only((const(char)[]).init, (const(char)[]).init))));
}

// Tests the zero-element result
@safe unittest
{
    import std.algorithm.comparison : equal;

    auto emptyRange = only();

    alias EmptyRange = typeof(emptyRange);
    static assert(isInputRange!EmptyRange);
    static assert(isForwardRange!EmptyRange);
    static assert(isBidirectionalRange!EmptyRange);
    static assert(isRandomAccessRange!EmptyRange);
    static assert(hasLength!EmptyRange);
    static assert(hasSlicing!EmptyRange);

    assert(emptyRange.empty);
    assert(emptyRange.length == 0);
    assert(emptyRange.equal(emptyRange[]));
    assert(emptyRange.equal(emptyRange.save));
    assert(emptyRange[0 .. 0].equal(emptyRange));
}

// Tests the single-element result
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : tuple;
    foreach (x; tuple(1, '1', 1.0, "1", [1]))
    {
        auto a = only(x);
        typeof(x)[] e = [];
        assert(a.front == x);
        assert(a.back == x);
        assert(!a.empty);
        assert(a.length == 1);
        assert(equal(a, a[]));
        assert(equal(a, a[0 .. 1]));
        assert(equal(a[0 .. 0], e));
        assert(equal(a[1 .. 1], e));
        assert(a[0] == x);

        auto b = a.save;
        assert(equal(a, b));
        a.popFront();
        assert(a.empty && a.length == 0 && a[].empty);
        b.popBack();
        assert(b.empty && b.length == 0 && b[].empty);

        alias A = typeof(a);
        static assert(isInputRange!A);
        static assert(isForwardRange!A);
        static assert(isBidirectionalRange!A);
        static assert(isRandomAccessRange!A);
        static assert(hasLength!A);
        static assert(hasSlicing!A);
    }

    auto imm = only!(immutable int)(1);
    immutable int[] imme = [];
    assert(imm.front == 1);
    assert(imm.back == 1);
    assert(!imm.empty);
    assert(imm.init.empty); // Issue 13441
    assert(imm.length == 1);
    assert(equal(imm, imm[]));
    assert(equal(imm, imm[0 .. 1]));
    assert(equal(imm[0 .. 0], imme));
    assert(equal(imm[1 .. 1], imme));
    assert(imm[0] == 1);
}

// Tests multiple-element results
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : joiner;
    import std.meta : AliasSeq;
    static assert(!__traits(compiles, only(1, "1")));

    auto nums = only!(byte, uint, long)(1, 2, 3);
    static assert(is(ElementType!(typeof(nums)) == long));
    assert(nums.length == 3);

    foreach (i; 0 .. 3)
        assert(nums[i] == i + 1);

    auto saved = nums.save;

    foreach (i; 1 .. 4)
    {
        assert(nums.front == nums[0]);
        assert(nums.front == i);
        nums.popFront();
        assert(nums.length == 3 - i);
    }

    assert(nums.empty);

    assert(saved.equal(only(1, 2, 3)));
    assert(saved.equal(saved[]));
    assert(saved[0 .. 1].equal(only(1)));
    assert(saved[0 .. 2].equal(only(1, 2)));
    assert(saved[0 .. 3].equal(saved));
    assert(saved[1 .. 3].equal(only(2, 3)));
    assert(saved[2 .. 3].equal(only(3)));
    assert(saved[0 .. 0].empty);
    assert(saved[3 .. 3].empty);

    alias data = AliasSeq!("one", "two", "three", "four");
    static joined =
        ["one two", "one two three", "one two three four"];
    string[] joinedRange = joined;

    foreach (argCount; AliasSeq!(2, 3, 4))
    {
        auto values = only(data[0 .. argCount]);
        alias Values = typeof(values);
        static assert(is(ElementType!Values == string));
        static assert(isInputRange!Values);
        static assert(isForwardRange!Values);
        static assert(isBidirectionalRange!Values);
        static assert(isRandomAccessRange!Values);
        static assert(hasSlicing!Values);
        static assert(hasLength!Values);

        assert(values.length == argCount);
        assert(values[0 .. $].equal(values[0 .. values.length]));
        assert(values.joiner(" ").equal(joinedRange.front));
        joinedRange.popFront();
    }

    assert(saved.retro.equal(only(3, 2, 1)));
    assert(saved.length == 3);

    assert(saved.back == 3);
    saved.popBack();
    assert(saved.length == 2);
    assert(saved.back == 2);

    assert(saved.front == 1);
    saved.popFront();
    assert(saved.length == 1);
    assert(saved.front == 2);

    saved.popBack();
    assert(saved.empty);

    auto imm = only!(immutable int, immutable int)(42, 24);
    alias Imm = typeof(imm);
    static assert(is(ElementType!Imm == immutable(int)));
    assert(!imm.empty);
    assert(imm.init.empty); // Issue 13441
    assert(imm.front == 42);
    imm.popFront();
    assert(imm.front == 24);
    imm.popFront();
    assert(imm.empty);

    static struct Test { int* a; }
    immutable(Test) test;
    cast(void) only(test, test); // Works with mutable indirection
}

/**
Iterate over `range` with an attached index variable.

Each element is a $(REF Tuple, std,typecons) containing the index
and the element, in that order, where the index member is named $(D index)
and the element member is named `value`.

The index starts at `start` and is incremented by one on every iteration.

Overflow:
    If `range` has length, then it is an error to pass a value for `start`
    so that `start + range.length` is bigger than `Enumerator.max`, thus
    it is ensured that overflow cannot happen.

    If `range` does not have length, and `popFront` is called when
    `front.index == Enumerator.max`, the index will overflow and
    continue from `Enumerator.min`.

Params:
    range = the input range to attach indexes to
    start = the number to start the index counter from

Returns:
    At minimum, an input range. All other range primitives are given in the
    resulting range if `range` has them. The exceptions are the bidirectional
    primitives, which are propagated only if `range` has length.

Example:
Useful for using $(D foreach) with an index loop variable:
----
    import std.stdio : stdin, stdout;
    import std.range : enumerate;

    foreach (lineNum, line; stdin.byLine().enumerate(1))
        stdout.writefln("line #%s: %s", lineNum, line);
----
*/
auto enumerate(Enumerator = size_t, Range)(Range range, Enumerator start = 0)
if (isIntegral!Enumerator && isInputRange!Range)
in
{
    static if (hasLength!Range)
    {
        // TODO: core.checkedint supports mixed signedness yet?
        import core.checkedint : adds, addu;
        import std.conv : ConvException, to;
        import std.traits : isSigned, Largest, Signed;

        alias LengthType = typeof(range.length);
        bool overflow;
        static if (isSigned!Enumerator && isSigned!LengthType)
            auto result = adds(start, range.length, overflow);
        else static if (isSigned!Enumerator)
        {
            Largest!(Enumerator, Signed!LengthType) signedLength;
            try signedLength = to!(typeof(signedLength))(range.length);
            catch (ConvException)
                overflow = true;
            catch (Exception)
                assert(false);

            auto result = adds(start, signedLength, overflow);
        }
        else
        {
            static if (isSigned!LengthType)
                assert(range.length >= 0);
            auto result = addu(start, range.length, overflow);
        }

        assert(!overflow && result <= Enumerator.max);
    }
}
body
{
    // TODO: Relax isIntegral!Enumerator to allow user-defined integral types
    static struct Result
    {
        import std.typecons : Tuple;

        private:
        alias ElemType = Tuple!(Enumerator, "index", ElementType!Range, "value");
        Range range;
        Enumerator index;

        public:
        ElemType front() @property
        {
            assert(!range.empty, "Attempting to fetch the front of an empty enumerate");
            return typeof(return)(index, range.front);
        }

        static if (isInfinite!Range)
            enum bool empty = false;
        else
        {
            bool empty() @property
            {
                return range.empty;
            }
        }

        void popFront()
        {
            assert(!range.empty, "Attempting to popFront an empty enumerate");
            range.popFront();
            ++index; // When !hasLength!Range, overflow is expected
        }

        static if (isForwardRange!Range)
        {
            Result save() @property
            {
                return typeof(return)(range.save, index);
            }
        }

        static if (hasLength!Range)
        {
            size_t length() @property
            {
                return range.length;
            }

            alias opDollar = length;

            static if (isBidirectionalRange!Range)
            {
                ElemType back() @property
                {
                    assert(!range.empty, "Attempting to fetch the back of an empty enumerate");
                    return typeof(return)(cast(Enumerator)(index + range.length - 1), range.back);
                }

                void popBack()
                {
                    assert(!range.empty, "Attempting to popBack an empty enumerate");
                    range.popBack();
                }
            }
        }

        static if (isRandomAccessRange!Range)
        {
             ElemType opIndex(size_t i)
             {
                return typeof(return)(cast(Enumerator)(index + i), range[i]);
             }
        }

        static if (hasSlicing!Range)
        {
            static if (hasLength!Range)
            {
                Result opSlice(size_t i, size_t j)
                {
                    return typeof(return)(range[i .. j], cast(Enumerator)(index + i));
                }
            }
            else
            {
                static struct DollarToken {}
                enum opDollar = DollarToken.init;

                Result opSlice(size_t i, DollarToken)
                {
                    return typeof(return)(range[i .. $], cast(Enumerator)(index + i));
                }

                auto opSlice(size_t i, size_t j)
                {
                    return this[i .. $].takeExactly(j - 1);
                }
            }
        }
    }

    return Result(range, start);
}

/// Can start enumeration from a negative position:
pure @safe nothrow unittest
{
    import std.array : assocArray;
    import std.range : enumerate;

    bool[int] aa = true.repeat(3).enumerate(-1).assocArray();
    assert(aa[-1]);
    assert(aa[0]);
    assert(aa[1]);
}

pure @safe nothrow unittest
{
    import std.internal.test.dummyrange : AllDummyRanges;
    import std.meta : AliasSeq;
    import std.typecons : tuple;

    static struct HasSlicing
    {
        typeof(this) front() @property { return typeof(this).init; }
        bool empty() @property { return true; }
        void popFront() {}

        typeof(this) opSlice(size_t, size_t)
        {
            return typeof(this)();
        }
    }

    foreach (DummyType; AliasSeq!(AllDummyRanges, HasSlicing))
    {
        alias R = typeof(enumerate(DummyType.init));
        static assert(isInputRange!R);
        static assert(isForwardRange!R == isForwardRange!DummyType);
        static assert(isRandomAccessRange!R == isRandomAccessRange!DummyType);
        static assert(!hasAssignableElements!R);

        static if (hasLength!DummyType)
        {
            static assert(hasLength!R);
            static assert(isBidirectionalRange!R ==
                isBidirectionalRange!DummyType);
        }

        static assert(hasSlicing!R == hasSlicing!DummyType);
    }

    static immutable values = ["zero", "one", "two", "three"];
    auto enumerated = values[].enumerate();
    assert(!enumerated.empty);
    assert(enumerated.front == tuple(0, "zero"));
    assert(enumerated.back == tuple(3, "three"));

    typeof(enumerated) saved = enumerated.save;
    saved.popFront();
    assert(enumerated.front == tuple(0, "zero"));
    assert(saved.front == tuple(1, "one"));
    assert(saved.length == enumerated.length - 1);
    saved.popBack();
    assert(enumerated.back == tuple(3, "three"));
    assert(saved.back == tuple(2, "two"));
    saved.popFront();
    assert(saved.front == tuple(2, "two"));
    assert(saved.back == tuple(2, "two"));
    saved.popFront();
    assert(saved.empty);

    size_t control = 0;
    foreach (i, v; enumerated)
    {
        static assert(is(typeof(i) == size_t));
        static assert(is(typeof(v) == typeof(values[0])));
        assert(i == control);
        assert(v == values[i]);
        assert(tuple(i, v) == enumerated[i]);
        ++control;
    }

    assert(enumerated[0 .. $].front == tuple(0, "zero"));
    assert(enumerated[$ - 1 .. $].front == tuple(3, "three"));

    foreach (i; 0 .. 10)
    {
        auto shifted = values[0 .. 2].enumerate(i);
        assert(shifted.front == tuple(i, "zero"));
        assert(shifted[0] == shifted.front);

        auto next = tuple(i + 1, "one");
        assert(shifted[1] == next);
        shifted.popFront();
        assert(shifted.front == next);
        shifted.popFront();
        assert(shifted.empty);
    }

    foreach (T; AliasSeq!(ubyte, byte, uint, int))
    {
        auto inf = 42.repeat().enumerate(T.max);
        alias Inf = typeof(inf);
        static assert(isInfinite!Inf);
        static assert(hasSlicing!Inf);

        // test overflow
        assert(inf.front == tuple(T.max, 42));
        inf.popFront();
        assert(inf.front == tuple(T.min, 42));

        // test slicing
        inf = inf[42 .. $];
        assert(inf.front == tuple(T.min + 42, 42));
        auto window = inf[0 .. 2];
        assert(window.length == 1);
        assert(window.front == inf.front);
        window.popFront();
        assert(window.empty);
    }
}

pure @safe unittest
{
    import std.algorithm.comparison : equal;
    import std.meta : AliasSeq;
    static immutable int[] values = [0, 1, 2, 3, 4];
    foreach (T; AliasSeq!(ubyte, ushort, uint, ulong))
    {
        auto enumerated = values.enumerate!T();
        static assert(is(typeof(enumerated.front.index) == T));
        assert(enumerated.equal(values[].zip(values)));

        foreach (T i; 0 .. 5)
        {
            auto subset = values[cast(size_t) i .. $];
            auto offsetEnumerated = subset.enumerate(i);
            static assert(is(typeof(enumerated.front.index) == T));
            assert(offsetEnumerated.equal(subset.zip(subset)));
        }
    }
}

version (none) // @@@BUG@@@ 10939
{
    // Re-enable (or remove) if 10939 is resolved.
    /+pure+/ @safe unittest // Impure because of std.conv.to
    {
        import core.exception : RangeError;
        import std.exception : assertNotThrown, assertThrown;
        import std.meta : AliasSeq;

        static immutable values = [42];

        static struct SignedLengthRange
        {
            immutable(int)[] _values = values;

            int front() @property { assert(false); }
            bool empty() @property { assert(false); }
            void popFront() { assert(false); }

            int length() @property
            {
                return cast(int)_values.length;
            }
        }

        SignedLengthRange svalues;
        foreach (Enumerator; AliasSeq!(ubyte, byte, ushort, short, uint, int, ulong, long))
        {
            assertThrown!RangeError(values[].enumerate!Enumerator(Enumerator.max));
            assertNotThrown!RangeError(values[].enumerate!Enumerator(Enumerator.max - values.length));
            assertThrown!RangeError(values[].enumerate!Enumerator(Enumerator.max - values.length + 1));

            assertThrown!RangeError(svalues.enumerate!Enumerator(Enumerator.max));
            assertNotThrown!RangeError(svalues.enumerate!Enumerator(Enumerator.max - values.length));
            assertThrown!RangeError(svalues.enumerate!Enumerator(Enumerator.max - values.length + 1));
        }

        foreach (Enumerator; AliasSeq!(byte, short, int))
        {
            assertThrown!RangeError(repeat(0, uint.max).enumerate!Enumerator());
        }

        assertNotThrown!RangeError(repeat(0, uint.max).enumerate!long());
    }
}

/**
  Returns true if $(D fn) accepts variables of type T1 and T2 in any order.
  The following code should compile:
  ---
  T1 foo();
  T2 bar();

  fn(foo(), bar());
  fn(bar(), foo());
  ---
*/
template isTwoWayCompatible(alias fn, T1, T2)
{
    enum isTwoWayCompatible = is(typeof( (){
            T1 foo();
            T2 bar();

            fn(foo(), bar());
            fn(bar(), foo());
        }
    ));
}


/**
   Policy used with the searching primitives $(D lowerBound), $(D
   upperBound), and $(D equalRange) of $(LREF SortedRange) below.
 */
enum SearchPolicy
{
    /**
       Searches in a linear fashion.
    */
    linear,

    /**
       Searches with a step that is grows linearly (1, 2, 3,...)
       leading to a quadratic search schedule (indexes tried are 0, 1,
       3, 6, 10, 15, 21, 28,...) Once the search overshoots its target,
       the remaining interval is searched using binary search. The
       search is completed in $(BIGOH sqrt(n)) time. Use it when you
       are reasonably confident that the value is around the beginning
       of the range.
    */
    trot,

    /**
       Performs a $(LINK2 https://en.wikipedia.org/wiki/Exponential_search,
       galloping search algorithm), i.e. searches
       with a step that doubles every time, (1, 2, 4, 8, ...)  leading
       to an exponential search schedule (indexes tried are 0, 1, 3,
       7, 15, 31, 63,...) Once the search overshoots its target, the
       remaining interval is searched using binary search. A value is
       found in $(BIGOH log(n)) time.
    */
        gallop,

    /**
       Searches using a classic interval halving policy. The search
       starts in the middle of the range, and each search step cuts
       the range in half. This policy finds a value in $(BIGOH log(n))
       time but is less cache friendly than $(D gallop) for large
       ranges. The $(D binarySearch) policy is used as the last step
       of $(D trot), $(D gallop), $(D trotBackwards), and $(D
       gallopBackwards) strategies.
    */
        binarySearch,

    /**
       Similar to $(D trot) but starts backwards. Use it when
       confident that the value is around the end of the range.
    */
        trotBackwards,

    /**
       Similar to $(D gallop) but starts backwards. Use it when
       confident that the value is around the end of the range.
    */
        gallopBackwards
        }

/**
Represents a sorted range. In addition to the regular range
primitives, supports additional operations that take advantage of the
ordering, such as merge and binary search. To obtain a $(D
SortedRange) from an unsorted range $(D r), use
$(REF sort, std,algorithm,sorting) which sorts $(D r) in place and returns the
corresponding $(D SortedRange). To construct a $(D SortedRange) from a range
$(D r) that is known to be already sorted, use $(LREF assumeSorted) described
below.
*/
struct SortedRange(Range, alias pred = "a < b")
if (isInputRange!Range)
{
    import std.functional : binaryFun;

    private alias predFun = binaryFun!pred;
    private bool geq(L, R)(L lhs, R rhs)
    {
        return !predFun(lhs, rhs);
    }
    private bool gt(L, R)(L lhs, R rhs)
    {
        return predFun(rhs, lhs);
    }
    private Range _input;

    // Undocummented because a clearer way to invoke is by calling
    // assumeSorted.
    this(Range input)
    out
    {
        // moved out of the body as a workaround for Issue 12661
        dbgVerifySorted();
    }
    body
    {
        this._input = input;
    }

    // Assertion only.
    private void dbgVerifySorted()
    {
        if (!__ctfe)
        debug
        {
            static if (isRandomAccessRange!Range && hasLength!Range)
            {
                import core.bitop : bsr;
                import std.algorithm.sorting : isSorted;

                // Check the sortedness of the input
                if (this._input.length < 2) return;

                immutable size_t msb = bsr(this._input.length) + 1;
                assert(msb > 0 && msb <= this._input.length);
                immutable step = this._input.length / msb;
                auto st = stride(this._input, step);

                assert(isSorted!pred(st), "Range is not sorted");
            }
        }
    }

    /// Range primitives.
    @property bool empty()             //const
    {
        return this._input.empty;
    }

    /// Ditto
    static if (isForwardRange!Range)
    @property auto save()
    {
        // Avoid the constructor
        typeof(this) result = this;
        result._input = _input.save;
        return result;
    }

    /// Ditto
    @property auto ref front()
    {
        return _input.front;
    }

    /// Ditto
    void popFront()
    {
        _input.popFront();
    }

    /// Ditto
    static if (isBidirectionalRange!Range)
    {
        @property auto ref back()
        {
            return _input.back;
        }

        /// Ditto
        void popBack()
        {
            _input.popBack();
        }
    }

    /// Ditto
    static if (isRandomAccessRange!Range)
        auto ref opIndex(size_t i)
        {
            return _input[i];
        }

    /// Ditto
    static if (hasSlicing!Range)
        auto opSlice(size_t a, size_t b)
        {
            assert(
                a <= b,
                "Attempting to slice a SortedRange with a larger first argument than the second."
            );
            typeof(this) result = this;
            result._input = _input[a .. b];// skip checking
            return result;
        }

    /// Ditto
    static if (hasLength!Range)
    {
        @property size_t length()          //const
        {
            return _input.length;
        }
        alias opDollar = length;
    }

/**
   Releases the controlled range and returns it.
*/
    auto release()
    {
        import std.algorithm.mutation : move;
        return move(_input);
    }

    // Assuming a predicate "test" that returns 0 for a left portion
    // of the range and then 1 for the rest, returns the index at
    // which the first 1 appears. Used internally by the search routines.
    private size_t getTransitionIndex(SearchPolicy sp, alias test, V)(V v)
    if (sp == SearchPolicy.binarySearch && isRandomAccessRange!Range && hasLength!Range)
    {
        size_t first = 0, count = _input.length;
        while (count > 0)
        {
            immutable step = count / 2, it = first + step;
            if (!test(_input[it], v))
            {
                first = it + 1;
                count -= step + 1;
            }
            else
            {
                count = step;
            }
        }
        return first;
    }

    // Specialization for trot and gallop
    private size_t getTransitionIndex(SearchPolicy sp, alias test, V)(V v)
    if ((sp == SearchPolicy.trot || sp == SearchPolicy.gallop)
        && isRandomAccessRange!Range)
    {
        if (empty || test(front, v)) return 0;
        immutable count = length;
        if (count == 1) return 1;
        size_t below = 0, above = 1, step = 2;
        while (!test(_input[above], v))
        {
            // Still too small, update below and increase gait
            below = above;
            immutable next = above + step;
            if (next >= count)
            {
                // Overshot - the next step took us beyond the end. So
                // now adjust next and simply exit the loop to do the
                // binary search thingie.
                above = count;
                break;
            }
            // Still in business, increase step and continue
            above = next;
            static if (sp == SearchPolicy.trot)
                ++step;
            else
                step <<= 1;
        }
        return below + this[below .. above].getTransitionIndex!(
            SearchPolicy.binarySearch, test, V)(v);
    }

    // Specialization for trotBackwards and gallopBackwards
    private size_t getTransitionIndex(SearchPolicy sp, alias test, V)(V v)
    if ((sp == SearchPolicy.trotBackwards || sp == SearchPolicy.gallopBackwards)
        && isRandomAccessRange!Range)
    {
        immutable count = length;
        if (empty || !test(back, v)) return count;
        if (count == 1) return 0;
        size_t below = count - 2, above = count - 1, step = 2;
        while (test(_input[below], v))
        {
            // Still too large, update above and increase gait
            above = below;
            if (below < step)
            {
                // Overshot - the next step took us beyond the end. So
                // now adjust next and simply fall through to do the
                // binary search thingie.
                below = 0;
                break;
            }
            // Still in business, increase step and continue
            below -= step;
            static if (sp == SearchPolicy.trot)
                ++step;
            else
                step <<= 1;
        }
        return below + this[below .. above].getTransitionIndex!(
            SearchPolicy.binarySearch, test, V)(v);
    }

// lowerBound
/**
   This function uses a search with policy $(D sp) to find the
   largest left subrange on which $(D pred(x, value)) is $(D true) for
   all $(D x) (e.g., if $(D pred) is "less than", returns the portion of
   the range with elements strictly smaller than $(D value)). The search
   schedule and its complexity are documented in
   $(LREF SearchPolicy).  See also STL's
   $(HTTP sgi.com/tech/stl/lower_bound.html, lower_bound).
*/
    auto lowerBound(SearchPolicy sp = SearchPolicy.binarySearch, V)(V value)
    if (isTwoWayCompatible!(predFun, ElementType!Range, V)
         && hasSlicing!Range)
    {
        return this[0 .. getTransitionIndex!(sp, geq)(value)];
    }

    ///
    @safe unittest
    {
        import std.algorithm.comparison : equal;
        auto a = assumeSorted([ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ]);
        auto p = a.lowerBound(4);
        assert(equal(p, [ 0, 1, 2, 3 ]));
    }

// upperBound
/**
This function searches with policy $(D sp) to find the largest right
subrange on which $(D pred(value, x)) is $(D true) for all $(D x)
(e.g., if $(D pred) is "less than", returns the portion of the range
with elements strictly greater than $(D value)). The search schedule
and its complexity are documented in $(LREF SearchPolicy).

For ranges that do not offer random access, $(D SearchPolicy.linear)
is the only policy allowed (and it must be specified explicitly lest it exposes
user code to unexpected inefficiencies). For random-access searches, all
policies are allowed, and $(D SearchPolicy.binarySearch) is the default.

See_Also: STL's $(HTTP sgi.com/tech/stl/lower_bound.html,upper_bound).
*/
    auto upperBound(SearchPolicy sp = SearchPolicy.binarySearch, V)(V value)
    if (isTwoWayCompatible!(predFun, ElementType!Range, V))
    {
        static assert(hasSlicing!Range || sp == SearchPolicy.linear,
            "Specify SearchPolicy.linear explicitly for "
            ~ typeof(this).stringof);
        static if (sp == SearchPolicy.linear)
        {
            for (; !_input.empty && !predFun(value, _input.front);
                 _input.popFront())
            {
            }
            return this;
        }
        else
        {
            return this[getTransitionIndex!(sp, gt)(value) .. length];
        }
    }

    ///
    @safe unittest
    {
        import std.algorithm.comparison : equal;
        auto a = assumeSorted([ 1, 2, 3, 3, 3, 4, 4, 5, 6 ]);
        auto p = a.upperBound(3);
        assert(equal(p, [4, 4, 5, 6]));
    }


// equalRange
/**
   Returns the subrange containing all elements $(D e) for which both $(D
   pred(e, value)) and $(D pred(value, e)) evaluate to $(D false) (e.g.,
   if $(D pred) is "less than", returns the portion of the range with
   elements equal to $(D value)). Uses a classic binary search with
   interval halving until it finds a value that satisfies the condition,
   then uses $(D SearchPolicy.gallopBackwards) to find the left boundary
   and $(D SearchPolicy.gallop) to find the right boundary. These
   policies are justified by the fact that the two boundaries are likely
   to be near the first found value (i.e., equal ranges are relatively
   small). Completes the entire search in $(BIGOH log(n)) time. See also
   STL's $(HTTP sgi.com/tech/stl/equal_range.html, equal_range).
*/
    auto equalRange(V)(V value)
    if (isTwoWayCompatible!(predFun, ElementType!Range, V)
        && isRandomAccessRange!Range)
    {
        size_t first = 0, count = _input.length;
        while (count > 0)
        {
            immutable step = count / 2;
            auto it = first + step;
            if (predFun(_input[it], value))
            {
                // Less than value, bump left bound up
                first = it + 1;
                count -= step + 1;
            }
            else if (predFun(value, _input[it]))
            {
                // Greater than value, chop count
                count = step;
            }
            else
            {
                // Equal to value, do binary searches in the
                // leftover portions
                // Gallop towards the left end as it's likely nearby
                immutable left = first
                    + this[first .. it]
                    .lowerBound!(SearchPolicy.gallopBackwards)(value).length;
                first += count;
                // Gallop towards the right end as it's likely nearby
                immutable right = first
                    - this[it + 1 .. first]
                    .upperBound!(SearchPolicy.gallop)(value).length;
                return this[left .. right];
            }
        }
        return this.init;
    }

    ///
    @safe unittest
    {
        import std.algorithm.comparison : equal;
        auto a = [ 1, 2, 3, 3, 3, 4, 4, 5, 6 ];
        auto r = a.assumeSorted.equalRange(3);
        assert(equal(r, [ 3, 3, 3 ]));
    }

// trisect
/**
Returns a tuple $(D r) such that $(D r[0]) is the same as the result
of $(D lowerBound(value)), $(D r[1]) is the same as the result of $(D
equalRange(value)), and $(D r[2]) is the same as the result of $(D
upperBound(value)). The call is faster than computing all three
separately. Uses a search schedule similar to $(D
equalRange). Completes the entire search in $(BIGOH log(n)) time.
*/
    auto trisect(V)(V value)
    if (isTwoWayCompatible!(predFun, ElementType!Range, V)
        && isRandomAccessRange!Range && hasLength!Range)
    {
        import std.typecons : tuple;
        size_t first = 0, count = _input.length;
        while (count > 0)
        {
            immutable step = count / 2;
            auto it = first + step;
            if (predFun(_input[it], value))
            {
                // Less than value, bump left bound up
                first = it + 1;
                count -= step + 1;
            }
            else if (predFun(value, _input[it]))
            {
                // Greater than value, chop count
                count = step;
            }
            else
            {
                // Equal to value, do binary searches in the
                // leftover portions
                // Gallop towards the left end as it's likely nearby
                immutable left = first
                    + this[first .. it]
                    .lowerBound!(SearchPolicy.gallopBackwards)(value).length;
                first += count;
                // Gallop towards the right end as it's likely nearby
                immutable right = first
                    - this[it + 1 .. first]
                    .upperBound!(SearchPolicy.gallop)(value).length;
                return tuple(this[0 .. left], this[left .. right],
                        this[right .. length]);
            }
        }
        // No equal element was found
        return tuple(this[0 .. first], this.init, this[first .. length]);
    }

    ///
    @safe unittest
    {
        import std.algorithm.comparison : equal;
        auto a = [ 1, 2, 3, 3, 3, 4, 4, 5, 6 ];
        auto r = assumeSorted(a).trisect(3);
        assert(equal(r[0], [ 1, 2 ]));
        assert(equal(r[1], [ 3, 3, 3 ]));
        assert(equal(r[2], [ 4, 4, 5, 6 ]));
    }

// contains
/**
Returns $(D true) if and only if $(D value) can be found in $(D
range), which is assumed to be sorted. Performs $(BIGOH log(r.length))
evaluations of $(D pred). See also STL's $(HTTP
sgi.com/tech/stl/binary_search.html, binary_search).
 */

    bool contains(V)(V value)
    if (isRandomAccessRange!Range)
    {
        if (empty) return false;
        immutable i = getTransitionIndex!(SearchPolicy.binarySearch, geq)(value);
        if (i >= length) return false;
        return !predFun(value, _input[i]);
    }

// groupBy
/**
Returns a range of subranges of elements that are equivalent according to the
sorting relation.
 */
    auto groupBy()()
    {
        import std.algorithm.iteration : chunkBy;
        return _input.chunkBy!((a, b) => !predFun(a, b) && !predFun(b, a));
    }
}

///
@safe unittest
{
    import std.algorithm.sorting : sort;
    auto a = [ 1, 2, 3, 42, 52, 64 ];
    auto r = assumeSorted(a);
    assert(r.contains(3));
    assert(!r.contains(32));
    auto r1 = sort!"a > b"(a);
    assert(r1.contains(3));
    assert(!r1.contains(32));
    assert(r1.release() == [ 64, 52, 42, 3, 2, 1 ]);
}

/**
$(D SortedRange) could accept ranges weaker than random-access, but it
is unable to provide interesting functionality for them. Therefore,
$(D SortedRange) is currently restricted to random-access ranges.

No copy of the original range is ever made. If the underlying range is
changed concurrently with its corresponding $(D SortedRange) in ways
that break its sorted-ness, $(D SortedRange) will work erratically.
*/
@safe unittest
{
    import std.algorithm.mutation : swap;
    auto a = [ 1, 2, 3, 42, 52, 64 ];
    auto r = assumeSorted(a);
    assert(r.contains(42));
    swap(a[3], a[5]);         // illegal to break sortedness of original range
    assert(!r.contains(42));  // passes although it shouldn't
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    auto a = [ 10, 20, 30, 30, 30, 40, 40, 50, 60 ];
    auto r = assumeSorted(a).trisect(30);
    assert(equal(r[0], [ 10, 20 ]));
    assert(equal(r[1], [ 30, 30, 30 ]));
    assert(equal(r[2], [ 40, 40, 50, 60 ]));

    r = assumeSorted(a).trisect(35);
    assert(equal(r[0], [ 10, 20, 30, 30, 30 ]));
    assert(r[1].empty);
    assert(equal(r[2], [ 40, 40, 50, 60 ]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    auto a = [ "A", "AG", "B", "E", "F" ];
    auto r = assumeSorted!"cmp(a,b) < 0"(a).trisect("B"w);
    assert(equal(r[0], [ "A", "AG" ]));
    assert(equal(r[1], [ "B" ]));
    assert(equal(r[2], [ "E", "F" ]));
    r = assumeSorted!"cmp(a,b) < 0"(a).trisect("A"d);
    assert(r[0].empty);
    assert(equal(r[1], [ "A" ]));
    assert(equal(r[2], [ "AG", "B", "E", "F" ]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    static void test(SearchPolicy pol)()
    {
        auto a = [ 1, 2, 3, 42, 52, 64 ];
        auto r = assumeSorted(a);
        assert(equal(r.lowerBound(42), [1, 2, 3]));

        assert(equal(r.lowerBound!(pol)(42), [1, 2, 3]));
        assert(equal(r.lowerBound!(pol)(41), [1, 2, 3]));
        assert(equal(r.lowerBound!(pol)(43), [1, 2, 3, 42]));
        assert(equal(r.lowerBound!(pol)(51), [1, 2, 3, 42]));
        assert(equal(r.lowerBound!(pol)(3), [1, 2]));
        assert(equal(r.lowerBound!(pol)(55), [1, 2, 3, 42, 52]));
        assert(equal(r.lowerBound!(pol)(420), a));
        assert(equal(r.lowerBound!(pol)(0), a[0 .. 0]));

        assert(equal(r.upperBound!(pol)(42), [52, 64]));
        assert(equal(r.upperBound!(pol)(41), [42, 52, 64]));
        assert(equal(r.upperBound!(pol)(43), [52, 64]));
        assert(equal(r.upperBound!(pol)(51), [52, 64]));
        assert(equal(r.upperBound!(pol)(53), [64]));
        assert(equal(r.upperBound!(pol)(55), [64]));
        assert(equal(r.upperBound!(pol)(420), a[0 .. 0]));
        assert(equal(r.upperBound!(pol)(0), a));
    }

    test!(SearchPolicy.trot)();
    test!(SearchPolicy.gallop)();
    test!(SearchPolicy.trotBackwards)();
    test!(SearchPolicy.gallopBackwards)();
    test!(SearchPolicy.binarySearch)();
}

@safe unittest
{
    // Check for small arrays
    int[] a;
    auto r = assumeSorted(a);
    a = [ 1 ];
    r = assumeSorted(a);
    a = [ 1, 2 ];
    r = assumeSorted(a);
    a = [ 1, 2, 3 ];
    r = assumeSorted(a);
}

@safe unittest
{
    import std.algorithm.mutation : swap;
    auto a = [ 1, 2, 3, 42, 52, 64 ];
    auto r = assumeSorted(a);
    assert(r.contains(42));
    swap(a[3], a[5]);                  // illegal to break sortedness of original range
    assert(!r.contains(42));            // passes although it shouldn't
}

@safe unittest
{
    immutable(int)[] arr = [ 1, 2, 3 ];
    auto s = assumeSorted(arr);
}

@system unittest
{
    import std.algorithm.comparison : equal;
    int[] arr = [100, 101, 102, 200, 201, 300];
    auto s = assumeSorted!((a, b) => a / 100 < b / 100)(arr);
    assert(s.groupBy.equal!equal([[100, 101, 102], [200, 201], [300]]));
}

// Test on an input range
@system unittest
{
    import std.conv : text;
    import std.file : exists, remove, tempDir;
    import std.path : buildPath;
    import std.stdio : File;
    import std.uuid : randomUUID;
    auto name = buildPath(tempDir(), "test.std.range.line-" ~ text(__LINE__) ~
                          "." ~ randomUUID().toString());
    auto f = File(name, "w");
    scope(exit) if (exists(name)) remove(name);
    // write a sorted range of lines to the file
    f.write("abc\ndef\nghi\njkl");
    f.close();
    f.open(name, "r");
    auto r = assumeSorted(f.byLine());
    auto r1 = r.upperBound!(SearchPolicy.linear)("def");
    assert(r1.front == "ghi", r1.front);
    f.close();
}

/**
Assumes $(D r) is sorted by predicate $(D pred) and returns the
corresponding $(D SortedRange!(pred, R)) having $(D r) as support. To
keep the checking costs low, the cost is $(BIGOH 1) in release mode
(no checks for sorted-ness are performed). In debug mode, a few random
elements of $(D r) are checked for sorted-ness. The size of the sample
is proportional $(BIGOH log(r.length)). That way, checking has no
effect on the complexity of subsequent operations specific to sorted
ranges (such as binary search). The probability of an arbitrary
unsorted range failing the test is very high (however, an
almost-sorted range is likely to pass it). To check for sorted-ness at
cost $(BIGOH n), use $(REF isSorted, std,algorithm,sorting).
 */
auto assumeSorted(alias pred = "a < b", R)(R r)
if (isInputRange!(Unqual!R))
{
    return SortedRange!(Unqual!R, pred)(r);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    static assert(isRandomAccessRange!(SortedRange!(int[])));
    int[] a = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ];
    auto p = assumeSorted(a).lowerBound(4);
    assert(equal(p, [0, 1, 2, 3]));
    p = assumeSorted(a).lowerBound(5);
    assert(equal(p, [0, 1, 2, 3, 4]));
    p = assumeSorted(a).lowerBound(6);
    assert(equal(p, [ 0, 1, 2, 3, 4, 5]));
    p = assumeSorted(a).lowerBound(6.9);
    assert(equal(p, [ 0, 1, 2, 3, 4, 5, 6]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    int[] a = [ 1, 2, 3, 3, 3, 4, 4, 5, 6 ];
    auto p = assumeSorted(a).upperBound(3);
    assert(equal(p, [4, 4, 5, 6 ]));
    p = assumeSorted(a).upperBound(4.2);
    assert(equal(p, [ 5, 6 ]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : text;

    int[] a = [ 1, 2, 3, 3, 3, 4, 4, 5, 6 ];
    auto p = assumeSorted(a).equalRange(3);
    assert(equal(p, [ 3, 3, 3 ]), text(p));
    p = assumeSorted(a).equalRange(4);
    assert(equal(p, [ 4, 4 ]), text(p));
    p = assumeSorted(a).equalRange(2);
    assert(equal(p, [ 2 ]));
    p = assumeSorted(a).equalRange(0);
    assert(p.empty);
    p = assumeSorted(a).equalRange(7);
    assert(p.empty);
    p = assumeSorted(a).equalRange(3.0);
    assert(equal(p, [ 3, 3, 3]));
}

@safe unittest
{
    int[] a = [ 1, 2, 3, 3, 3, 4, 4, 5, 6 ];
    if (a.length)
    {
        auto b = a[a.length / 2];
        //auto r = sort(a);
        //assert(r.contains(b));
    }
}

@safe unittest
{
    auto a = [ 5, 7, 34, 345, 677 ];
    auto r = assumeSorted(a);
    a = null;
    r = assumeSorted(a);
    a = [ 1 ];
    r = assumeSorted(a);
}

@system unittest
{
    bool ok = true;
    try
    {
        auto r2 = assumeSorted([ 677, 345, 34, 7, 5 ]);
        debug ok = false;
    }
    catch (Throwable)
    {
    }
    assert(ok);
}

// issue 15003
@nogc @safe unittest
{
    static immutable a = [1, 2, 3, 4];
    auto r = a.assumeSorted;
}

/++
    Wrapper which effectively makes it possible to pass a range by reference.
    Both the original range and the RefRange will always have the exact same
    elements. Any operation done on one will affect the other. So, for instance,
    if it's passed to a function which would implicitly copy the original range
    if it were passed to it, the original range is $(I not) copied but is
    consumed as if it were a reference type.

    Note:
        `save` works as normal and operates on a new _range, so if
        `save` is ever called on the `RefRange`, then no operations on the
        saved _range will affect the original.

    Params:
        range = the range to construct the `RefRange` from

    Returns:
        A `RefRange`. If the given _range is a class type
        (and thus is already a reference type), then the original
        range is returned rather than a `RefRange`.
  +/
struct RefRange(R)
if (isInputRange!R)
{
public:

    /++ +/
    this(R* range) @safe pure nothrow
    {
        _range = range;
    }


    /++
        This does not assign the pointer of $(D rhs) to this $(D RefRange).
        Rather it assigns the range pointed to by $(D rhs) to the range pointed
        to by this $(D RefRange). This is because $(I any) operation on a
        $(D RefRange) is the same is if it occurred to the original range. The
        one exception is when a $(D RefRange) is assigned $(D null) either
        directly or because $(D rhs) is $(D null). In that case, $(D RefRange)
        no longer refers to the original range but is $(D null).
      +/
    auto opAssign(RefRange rhs)
    {
        if (_range && rhs._range)
            *_range = *rhs._range;
        else
            _range = rhs._range;

        return this;
    }

    /++ +/
    void opAssign(typeof(null) rhs)
    {
        _range = null;
    }


    /++
        A pointer to the wrapped range.
      +/
    @property inout(R*) ptr() @safe inout pure nothrow
    {
        return _range;
    }


    version (StdDdoc)
    {
        /++ +/
        @property auto front() {assert(0);}
        /++ Ditto +/
        @property auto front() const {assert(0);}
        /++ Ditto +/
        @property auto front(ElementType!R value) {assert(0);}
    }
    else
    {
        @property auto front()
        {
            return (*_range).front;
        }

        static if (is(typeof((*(cast(const R*)_range)).front))) @property auto front() const
        {
            return (*_range).front;
        }

        static if (is(typeof((*_range).front = (*_range).front))) @property auto front(ElementType!R value)
        {
            return (*_range).front = value;
        }
    }


    version (StdDdoc)
    {
        @property bool empty(); ///
        @property bool empty() const; ///Ditto
    }
    else static if (isInfinite!R)
        enum empty = false;
    else
    {
        @property bool empty()
        {
            return (*_range).empty;
        }

        static if (is(typeof((*cast(const R*)_range).empty))) @property bool empty() const
        {
            return (*_range).empty;
        }
    }


    /++ +/
    void popFront()
    {
        return (*_range).popFront();
    }


    version (StdDdoc)
    {
        /++
            Only defined if $(D isForwardRange!R) is $(D true).
          +/
        @property auto save() {assert(0);}
        /++ Ditto +/
        @property auto save() const {assert(0);}
        /++ Ditto +/
        auto opSlice() {assert(0);}
        /++ Ditto +/
        auto opSlice() const {assert(0);}
    }
    else static if (isForwardRange!R)
    {
        import std.traits : isSafe;
        private alias S = typeof((*_range).save);

        static if (is(typeof((*cast(const R*)_range).save)))
            private alias CS = typeof((*cast(const R*)_range).save);

        static if (isSafe!((R* r) => (*r).save))
        {
            @property RefRange!S save() @trusted
            {
                mixin(_genSave());
            }

            static if (is(typeof((*cast(const R*)_range).save))) @property RefRange!CS save() @trusted const
            {
                mixin(_genSave());
            }
        }
        else
        {
            @property RefRange!S save()
            {
                mixin(_genSave());
            }

            static if (is(typeof((*cast(const R*)_range).save))) @property RefRange!CS save() const
            {
                mixin(_genSave());
            }
        }

        auto opSlice()()
        {
            return save;
        }

        auto opSlice()() const
        {
            return save;
        }

        private static string _genSave() @safe pure nothrow
        {
            return `import std.conv : emplace;` ~
                   `alias S = typeof((*_range).save);` ~
                   `static assert(isForwardRange!S, S.stringof ~ " is not a forward range.");` ~
                   `auto mem = new void[S.sizeof];` ~
                   `emplace!S(mem, cast(S)(*_range).save);` ~
                   `return RefRange!S(cast(S*) mem.ptr);`;
        }

        static assert(isForwardRange!RefRange);
    }


    version (StdDdoc)
    {
        /++
            Only defined if $(D isBidirectionalRange!R) is $(D true).
          +/
        @property auto back() {assert(0);}
        /++ Ditto +/
        @property auto back() const {assert(0);}
        /++ Ditto +/
        @property auto back(ElementType!R value) {assert(0);}
    }
    else static if (isBidirectionalRange!R)
    {
        @property auto back()
        {
            return (*_range).back;
        }

        static if (is(typeof((*(cast(const R*)_range)).back))) @property auto back() const
        {
            return (*_range).back;
        }

        static if (is(typeof((*_range).back = (*_range).back))) @property auto back(ElementType!R value)
        {
            return (*_range).back = value;
        }
    }


    /++ Ditto +/
    static if (isBidirectionalRange!R) void popBack()
    {
        return (*_range).popBack();
    }


    version (StdDdoc)
    {
        /++
            Only defined if $(D isRandomAccesRange!R) is $(D true).
          +/
        auto ref opIndex(IndexType)(IndexType index) {assert(0);}

        /++ Ditto +/
        auto ref opIndex(IndexType)(IndexType index) const {assert(0);}
    }
    else static if (isRandomAccessRange!R)
    {
        auto ref opIndex(IndexType)(IndexType index)
            if (is(typeof((*_range)[index])))
        {
            return (*_range)[index];
        }

        auto ref opIndex(IndexType)(IndexType index) const
            if (is(typeof((*cast(const R*)_range)[index])))
        {
            return (*_range)[index];
        }
    }


    /++
        Only defined if $(D hasMobileElements!R) and $(D isForwardRange!R) are
        $(D true).
      +/
    static if (hasMobileElements!R && isForwardRange!R) auto moveFront()
    {
        return (*_range).moveFront();
    }


    /++
        Only defined if $(D hasMobileElements!R) and $(D isBidirectionalRange!R)
        are $(D true).
      +/
    static if (hasMobileElements!R && isBidirectionalRange!R) auto moveBack()
    {
        return (*_range).moveBack();
    }


    /++
        Only defined if $(D hasMobileElements!R) and $(D isRandomAccessRange!R)
        are $(D true).
      +/
    static if (hasMobileElements!R && isRandomAccessRange!R) auto moveAt(size_t index)
    {
        return (*_range).moveAt(index);
    }


    version (StdDdoc)
    {
        /++
            Only defined if $(D hasLength!R) is $(D true).
          +/
        @property auto length() {assert(0);}

        /++ Ditto +/
        @property auto length() const {assert(0);}

        /++ Ditto +/
        alias opDollar = length;
    }
    else static if (hasLength!R)
    {
        @property auto length()
        {
            return (*_range).length;
        }

        static if (is(typeof((*cast(const R*)_range).length))) @property auto length() const
        {
            return (*_range).length;
        }

        alias opDollar = length;
    }


    version (StdDdoc)
    {
        /++
            Only defined if $(D hasSlicing!R) is $(D true).
          +/
        auto opSlice(IndexType1, IndexType2)
                    (IndexType1 begin, IndexType2 end) {assert(0);}

        /++ Ditto +/
        auto opSlice(IndexType1, IndexType2)
                    (IndexType1 begin, IndexType2 end) const {assert(0);}
    }
    else static if (hasSlicing!R)
    {
        private alias T = typeof((*_range)[1 .. 2]);
        static if (is(typeof((*cast(const R*)_range)[1 .. 2])))
        {
            private alias CT = typeof((*cast(const R*)_range)[1 .. 2]);
        }

        RefRange!T opSlice(IndexType1, IndexType2)
                    (IndexType1 begin, IndexType2 end)
            if (is(typeof((*_range)[begin .. end])))
        {
            mixin(_genOpSlice());
        }

        RefRange!CT opSlice(IndexType1, IndexType2)
                    (IndexType1 begin, IndexType2 end) const
            if (is(typeof((*cast(const R*)_range)[begin .. end])))
        {
            mixin(_genOpSlice());
        }

        private static string _genOpSlice() @safe pure nothrow
        {
            return `import std.conv : emplace;` ~
                   `alias S = typeof((*_range)[begin .. end]);` ~
                   `static assert(hasSlicing!S, S.stringof ~ " is not sliceable.");` ~
                   `auto mem = new void[S.sizeof];` ~
                   `emplace!S(mem, cast(S)(*_range)[begin .. end]);` ~
                   `return RefRange!S(cast(S*) mem.ptr);`;
        }
    }


private:

    R* _range;
}

/// Basic Example
@system unittest
{
    import std.algorithm.searching : find;
    ubyte[] buffer = [1, 9, 45, 12, 22];
    auto found1 = find(buffer, 45);
    assert(found1 == [45, 12, 22]);
    assert(buffer == [1, 9, 45, 12, 22]);

    auto wrapped1 = refRange(&buffer);
    auto found2 = find(wrapped1, 45);
    assert(*found2.ptr == [45, 12, 22]);
    assert(buffer == [45, 12, 22]);

    auto found3 = find(wrapped1.save, 22);
    assert(*found3.ptr == [22]);
    assert(buffer == [45, 12, 22]);

    string str = "hello world";
    auto wrappedStr = refRange(&str);
    assert(str.front == 'h');
    str.popFrontN(5);
    assert(str == " world");
    assert(wrappedStr.front == ' ');
    assert(*wrappedStr.ptr == " world");
}

/// opAssign Example.
@system unittest
{
    ubyte[] buffer1 = [1, 2, 3, 4, 5];
    ubyte[] buffer2 = [6, 7, 8, 9, 10];
    auto wrapped1 = refRange(&buffer1);
    auto wrapped2 = refRange(&buffer2);
    assert(wrapped1.ptr is &buffer1);
    assert(wrapped2.ptr is &buffer2);
    assert(wrapped1.ptr !is wrapped2.ptr);
    assert(buffer1 != buffer2);

    wrapped1 = wrapped2;

    //Everything points to the same stuff as before.
    assert(wrapped1.ptr is &buffer1);
    assert(wrapped2.ptr is &buffer2);
    assert(wrapped1.ptr !is wrapped2.ptr);

    //But buffer1 has changed due to the assignment.
    assert(buffer1 == [6, 7, 8, 9, 10]);
    assert(buffer2 == [6, 7, 8, 9, 10]);

    buffer2 = [11, 12, 13, 14, 15];

    //Everything points to the same stuff as before.
    assert(wrapped1.ptr is &buffer1);
    assert(wrapped2.ptr is &buffer2);
    assert(wrapped1.ptr !is wrapped2.ptr);

    //But buffer2 has changed due to the assignment.
    assert(buffer1 == [6, 7, 8, 9, 10]);
    assert(buffer2 == [11, 12, 13, 14, 15]);

    wrapped2 = null;

    //The pointer changed for wrapped2 but not wrapped1.
    assert(wrapped1.ptr is &buffer1);
    assert(wrapped2.ptr is null);
    assert(wrapped1.ptr !is wrapped2.ptr);

    //buffer2 is not affected by the assignment.
    assert(buffer1 == [6, 7, 8, 9, 10]);
    assert(buffer2 == [11, 12, 13, 14, 15]);
}

@system unittest
{
    import std.algorithm.iteration : filter;
    {
        ubyte[] buffer = [1, 2, 3, 4, 5];
        auto wrapper = refRange(&buffer);
        auto p = wrapper.ptr;
        auto f = wrapper.front;
        wrapper.front = f;
        auto e = wrapper.empty;
        wrapper.popFront();
        auto s = wrapper.save;
        auto b = wrapper.back;
        wrapper.back = b;
        wrapper.popBack();
        auto i = wrapper[0];
        wrapper.moveFront();
        wrapper.moveBack();
        wrapper.moveAt(0);
        auto l = wrapper.length;
        auto sl = wrapper[0 .. 1];
        assert(wrapper[0 .. $].length == buffer[0 .. $].length);
    }

    {
        ubyte[] buffer = [1, 2, 3, 4, 5];
        const wrapper = refRange(&buffer);
        const p = wrapper.ptr;
        const f = wrapper.front;
        const e = wrapper.empty;
        const s = wrapper.save;
        const b = wrapper.back;
        const i = wrapper[0];
        const l = wrapper.length;
        const sl = wrapper[0 .. 1];
    }

    {
        ubyte[] buffer = [1, 2, 3, 4, 5];
        auto filtered = filter!"true"(buffer);
        auto wrapper = refRange(&filtered);
        auto p = wrapper.ptr;
        auto f = wrapper.front;
        wrapper.front = f;
        auto e = wrapper.empty;
        wrapper.popFront();
        auto s = wrapper.save;
        wrapper.moveFront();
    }

    {
        ubyte[] buffer = [1, 2, 3, 4, 5];
        auto filtered = filter!"true"(buffer);
        const wrapper = refRange(&filtered);
        const p = wrapper.ptr;

        //Cannot currently be const. filter needs to be updated to handle const.
        /+
        const f = wrapper.front;
        const e = wrapper.empty;
        const s = wrapper.save;
        +/
    }

    {
        string str = "hello world";
        auto wrapper = refRange(&str);
        auto p = wrapper.ptr;
        auto f = wrapper.front;
        auto e = wrapper.empty;
        wrapper.popFront();
        auto s = wrapper.save;
        auto b = wrapper.back;
        wrapper.popBack();
    }

    {
        // Issue 16534 - opDollar should be defined if the
        // wrapped range defines length.
        auto range = 10.iota.takeExactly(5);
        auto wrapper = refRange(&range);
        assert(wrapper.length == 5);
        assert(wrapper[0 .. $ - 1].length == 4);
    }
}

//Test assignment.
@system unittest
{
    ubyte[] buffer1 = [1, 2, 3, 4, 5];
    ubyte[] buffer2 = [6, 7, 8, 9, 10];
    RefRange!(ubyte[]) wrapper1;
    RefRange!(ubyte[]) wrapper2 = refRange(&buffer2);
    assert(wrapper1.ptr is null);
    assert(wrapper2.ptr is &buffer2);

    wrapper1 = refRange(&buffer1);
    assert(wrapper1.ptr is &buffer1);

    wrapper1 = wrapper2;
    assert(wrapper1.ptr is &buffer1);
    assert(buffer1 == buffer2);

    wrapper1 = RefRange!(ubyte[]).init;
    assert(wrapper1.ptr is null);
    assert(wrapper2.ptr is &buffer2);
    assert(buffer1 == buffer2);
    assert(buffer1 == [6, 7, 8, 9, 10]);

    wrapper2 = null;
    assert(wrapper2.ptr is null);
    assert(buffer2 == [6, 7, 8, 9, 10]);
}

@system unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.mutation : bringToFront;
    import std.algorithm.searching : commonPrefix, find, until;
    import std.algorithm.sorting : sort;

    //Test that ranges are properly consumed.
    {
        int[] arr = [1, 42, 2, 41, 3, 40, 4, 42, 9];
        auto wrapper = refRange(&arr);

        assert(*find(wrapper, 41).ptr == [41, 3, 40, 4, 42, 9]);
        assert(arr == [41, 3, 40, 4, 42, 9]);

        assert(*drop(wrapper, 2).ptr == [40, 4, 42, 9]);
        assert(arr == [40, 4, 42, 9]);

        assert(equal(until(wrapper, 42), [40, 4]));
        assert(arr == [42, 9]);

        assert(find(wrapper, 12).empty);
        assert(arr.empty);
    }

    {
        string str = "Hello, world-like object.";
        auto wrapper = refRange(&str);

        assert(*find(wrapper, "l").ptr == "llo, world-like object.");
        assert(str == "llo, world-like object.");

        assert(equal(take(wrapper, 5), "llo, "));
        assert(str == "world-like object.");
    }

    //Test that operating on saved ranges does not consume the original.
    {
        int[] arr = [1, 42, 2, 41, 3, 40, 4, 42, 9];
        auto wrapper = refRange(&arr);
        auto saved = wrapper.save;
        saved.popFrontN(3);
        assert(*saved.ptr == [41, 3, 40, 4, 42, 9]);
        assert(arr == [1, 42, 2, 41, 3, 40, 4, 42, 9]);
    }

    {
        string str = "Hello, world-like object.";
        auto wrapper = refRange(&str);
        auto saved = wrapper.save;
        saved.popFrontN(13);
        assert(*saved.ptr == "like object.");
        assert(str == "Hello, world-like object.");
    }

    //Test that functions which use save work properly.
    {
        int[] arr = [1, 42];
        auto wrapper = refRange(&arr);
        assert(equal(commonPrefix(wrapper, [1, 27]), [1]));
    }

    {
        int[] arr = [4, 5, 6, 7, 1, 2, 3];
        auto wrapper = refRange(&arr);
        assert(bringToFront(wrapper[0 .. 4], wrapper[4 .. arr.length]) == 3);
        assert(arr == [1, 2, 3, 4, 5, 6, 7]);
    }

    //Test bidirectional functions.
    {
        int[] arr = [1, 42, 2, 41, 3, 40, 4, 42, 9];
        auto wrapper = refRange(&arr);

        assert(wrapper.back == 9);
        assert(arr == [1, 42, 2, 41, 3, 40, 4, 42, 9]);

        wrapper.popBack();
        assert(arr == [1, 42, 2, 41, 3, 40, 4, 42]);
    }

    {
        string str = "Hello, world-like object.";
        auto wrapper = refRange(&str);

        assert(wrapper.back == '.');
        assert(str == "Hello, world-like object.");

        wrapper.popBack();
        assert(str == "Hello, world-like object");
    }

    //Test random access functions.
    {
        int[] arr = [1, 42, 2, 41, 3, 40, 4, 42, 9];
        auto wrapper = refRange(&arr);

        assert(wrapper[2] == 2);
        assert(arr == [1, 42, 2, 41, 3, 40, 4, 42, 9]);

        assert(*wrapper[3 .. 6].ptr != null, [41, 3, 40]);
        assert(arr == [1, 42, 2, 41, 3, 40, 4, 42, 9]);
    }

    //Test move functions.
    {
        int[] arr = [1, 42, 2, 41, 3, 40, 4, 42, 9];
        auto wrapper = refRange(&arr);

        auto t1 = wrapper.moveFront();
        auto t2 = wrapper.moveBack();
        wrapper.front = t2;
        wrapper.back = t1;
        assert(arr == [9, 42, 2, 41, 3, 40, 4, 42, 1]);

        sort(wrapper.save);
        assert(arr == [1, 2, 3, 4, 9, 40, 41, 42, 42]);
    }
}

@system unittest
{
    struct S
    {
        @property int front() @safe const pure nothrow { return 0; }
        enum bool empty = false;
        void popFront() @safe pure nothrow { }
        @property auto save() @safe pure nothrow { return this; }
    }

    S s;
    auto wrapper = refRange(&s);
    static assert(isInfinite!(typeof(wrapper)));
}

@system unittest
{
    class C
    {
        @property int front() @safe const pure nothrow { return 0; }
        @property bool empty() @safe const pure nothrow { return false; }
        void popFront() @safe pure nothrow { }
        @property auto save() @safe pure nothrow { return this; }
    }
    static assert(isForwardRange!C);

    auto c = new C;
    auto cWrapper = refRange(&c);
    static assert(is(typeof(cWrapper) == C));
    assert(cWrapper is c);
}

@system unittest // issue 14373
{
    static struct R
    {
        @property int front() {return 0;}
        void popFront() {empty = true;}
        bool empty = false;
    }
    R r;
    refRange(&r).popFront();
    assert(r.empty);
}

@system unittest // issue 14575
{
    struct R
    {
        Object front;
        alias back = front;
        bool empty = false;
        void popFront() {empty = true;}
        alias popBack = popFront;
        @property R save() {return this;}
    }
    static assert(isBidirectionalRange!R);
    R r;
    auto rr = refRange(&r);

    struct R2
    {
        @property Object front() {return null;}
        @property const(Object) front() const {return null;}
        alias back = front;
        bool empty = false;
        void popFront() {empty = true;}
        alias popBack = popFront;
        @property R2 save() {return this;}
    }
    static assert(isBidirectionalRange!R2);
    R2 r2;
    auto rr2 = refRange(&r2);
}

/// ditto
auto refRange(R)(R* range)
if (isInputRange!R)
{
    static if (!is(R == class))
        return RefRange!R(range);
    else
        return *range;
}

/*****************************************************************************/

@safe unittest    // bug 9060
{
    import std.algorithm.iteration : map, joiner, group;
    import std.algorithm.searching : until;
    // fix for std.algorithm
    auto r = map!(x => 0)([1]);
    chain(r, r);
    zip(r, r);
    roundRobin(r, r);

    struct NRAR {
        typeof(r) input;
        @property empty() { return input.empty; }
        @property front() { return input.front; }
        void popFront()   { input.popFront(); }
        @property save()  { return NRAR(input.save); }
    }
    auto n1 = NRAR(r);
    cycle(n1);  // non random access range version

    assumeSorted(r);

    // fix for std.range
    joiner([r], [9]);

    struct NRAR2 {
        NRAR input;
        @property empty() { return true; }
        @property front() { return input; }
        void popFront() { }
        @property save()  { return NRAR2(input.save); }
    }
    auto n2 = NRAR2(n1);
    joiner(n2);

    group(r);

    until(r, 7);
    static void foo(R)(R r) { until!(x => x > 7)(r); }
    foo(r);
}

private struct Bitwise(R)
if (isInputRange!R && isIntegral!(ElementType!R))
{
private:
    alias ElemType = ElementType!R;
    alias UnsignedElemType = Unsigned!ElemType;

    R parent;
    enum bitsNum = ElemType.sizeof * 8;
    size_t maskPos = 1;

    static if (isBidirectionalRange!R)
    {
        size_t backMaskPos = bitsNum;
    }

public:
    this()(auto ref R range)
    {
        parent = range;
    }

    static if (isInfinite!R)
    {
        enum empty = false;
    }
    else
    {
        /**
         * Check if the range is empty
         *
         * Returns: a boolean true or false
         */
        bool empty()
        {
            static if (hasLength!R)
            {
                return length == 0;
            }
            else static if (isBidirectionalRange!R)
            {
                if (parent.empty)
                {
                    return true;
                }
                else
                {
                    /*
                       If we have consumed the last element of the range both from
                       the front and the back, then the masks positions will overlap
                     */
                    return parent.save.dropOne.empty && (maskPos > backMaskPos);
                }
            }
            else
            {
                /*
                   If we consumed the last element of the range, but not all the
                   bits in the last element
                 */
                return parent.empty;
            }
        }
    }

    bool front()
    {
        assert(!empty);
        return (parent.front & mask(maskPos)) != 0;
    }

    void popFront()
    {
        assert(!empty);
        ++maskPos;
        if (maskPos > bitsNum)
        {
            parent.popFront;
            maskPos = 1;
        }
    }

    static if (hasLength!R)
    {
        size_t length()
        {
            auto len = parent.length * bitsNum - (maskPos - 1);
            static if (isBidirectionalRange!R)
            {
                len -= bitsNum - backMaskPos;
            }
            return len;
        }

        alias opDollar = length;
    }

    static if (isForwardRange!R)
    {
        typeof(this) save()
        {
            auto result = this;
            result.parent = parent.save;
            return result;
        }
    }

    static if (isBidirectionalRange!R)
    {
        bool back()
        {
            assert(!empty);
            return (parent.back & mask(backMaskPos)) != 0;
        }

        void popBack()
        {
            assert(!empty);
            --backMaskPos;
            if (backMaskPos == 0)
            {
                parent.popBack;
                backMaskPos = bitsNum;
            }
        }
    }

    static if (isRandomAccessRange!R)
    {
        /**
          Return the `n`th bit within the range
         */
        bool opIndex(size_t n)
        in
        {
            /*
               If it does not have the length property, it means that R is
               an infinite range
             */
            static if (hasLength!R)
            {
                assert(n < length, "Index out of bounds");
            }
        }
        body
        {
            immutable size_t remainingBits = bitsNum - maskPos + 1;
            // If n >= maskPos, then the bit sign will be 1, otherwise 0
            immutable sizediff_t sign = (remainingBits - n - 1) >> (sizediff_t.sizeof * 8 - 1);
            /*
               By truncating n with remainingBits bits we have skipped the
               remaining bits in parent[0], so we need to add 1 to elemIndex.

               Because bitsNum is a power of 2, n / bitsNum == n >> bitsNum.bsf
             */
            import core.bitop : bsf;
            immutable size_t elemIndex = sign * (((n - remainingBits) >> bitsNum.bsf) + 1);

            /*
               Since the indexing is from LSB to MSB, we need to index at the
               remainder of (n - remainingBits).

               Because bitsNum is a power of 2, n % bitsNum == n & (bitsNum - 1)
             */
            immutable size_t elemMaskPos = (sign ^ 1) * (maskPos + n)
                             + sign * (1 + ((n - remainingBits) & (bitsNum - 1)));

            return (parent[elemIndex] & mask(elemMaskPos)) != 0;
        }

        static if (hasAssignableElements!R)
        {
            /**
              Assigns `flag` to the `n`th bit within the range
             */
            void opIndexAssign(bool flag, size_t n)
                in
                {
                    static if (hasLength!R)
                    {
                        assert(n < length, "Index out of bounds");
                    }
                }
            body
            {
                import core.bitop : bsf;

                immutable size_t remainingBits = bitsNum - maskPos + 1;
                immutable sizediff_t sign = (remainingBits - n - 1) >> (sizediff_t.sizeof * 8 - 1);
                immutable size_t elemIndex = sign * (((n - remainingBits) >> bitsNum.bsf) + 1);
                immutable size_t elemMaskPos = (sign ^ 1) * (maskPos + n)
                    + sign * (1 + ((n - remainingBits) & (bitsNum - 1)));

                auto elem = parent[elemIndex];
                auto elemMask = mask(elemMaskPos);
                parent[elemIndex] = cast(UnsignedElemType)(flag * (elem | elemMask)
                        + (flag ^ 1) * (elem & ~elemMask));
            }
        }

        Bitwise!R opSlice()
        {
            return this.save;
        }

        Bitwise!R opSlice(size_t start, size_t end)
        in
        {
            assert(start < end, "Invalid bounds: end <= start");
        }
        body
        {
            import core.bitop : bsf;

            size_t remainingBits = bitsNum - maskPos + 1;
            sizediff_t sign = (remainingBits - start - 1) >> (sizediff_t.sizeof * 8 - 1);
            immutable size_t startElemIndex = sign * (((start - remainingBits) >> bitsNum.bsf) + 1);
            immutable size_t startElemMaskPos = (sign ^ 1) * (maskPos + start)
                                              + sign * (1 + ((start - remainingBits) & (bitsNum - 1)));

            immutable size_t sliceLen = end - start - 1;
            remainingBits = bitsNum - startElemMaskPos + 1;
            sign = (remainingBits - sliceLen - 1) >> (sizediff_t.sizeof * 8 - 1);
            immutable size_t endElemIndex = startElemIndex
                                          + sign * (((sliceLen - remainingBits) >> bitsNum.bsf) + 1);
            immutable size_t endElemMaskPos = (sign ^ 1) * (startElemMaskPos + sliceLen)
                                            + sign * (1 + ((sliceLen - remainingBits) & (bitsNum - 1)));

            typeof(return) result;
            // Get the slice to be returned from the parent
            result.parent = (parent[startElemIndex .. endElemIndex + 1]).save;
            result.maskPos = startElemMaskPos;
            static if (isBidirectionalRange!R)
            {
                result.backMaskPos = endElemMaskPos;
            }
            return result;
        }
    }

private:
    auto mask(size_t maskPos)
    {
        return (1UL << (maskPos - 1UL));
    }
}

/**
Bitwise adapter over an integral type range. Consumes the range elements bit by
bit, from the least significant bit to the most significant bit.

Params:
    R = an integral input range to iterate over
    range = range to consume bit by by

Returns:
    A `Bitwise` input range with propagated forward, bidirectional
    and random access capabilities
*/
auto bitwise(R)(auto ref R range)
if (isInputRange!R && isIntegral!(ElementType!R))
{
    return Bitwise!R(range);
}

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;
    import std.format : format;

    // 00000011 00001001
    ubyte[] arr = [3, 9];
    auto r = arr.bitwise;

    // iterate through it as with any other range
    assert(format("%(%d%)", r) == "1100000010010000");
    assert(format("%(%d%)", r.retro).equal("1100000010010000".retro));

    auto r2 = r[5 .. $];
    // set a bit
    r[2] = 1;
    assert(arr[0] == 7);
    assert(r[5] == r2[0]);
}

/// You can use bitwise to implement an uniform bool generator
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.random : rndGen;

    auto rb = rndGen.bitwise;
    static assert(isInfinite!(typeof(rb)));

    auto rb2 = rndGen.bitwise;
    // Don't forget that structs are passed by value
    assert(rb.take(10).equal(rb2.take(10)));
}

// Test nogc inference
@safe @nogc unittest
{
    static ubyte[] arr = [3, 9];
    auto bw = arr.bitwise;
    auto bw2 = bw[];
    auto bw3 = bw[8 .. $];
    bw3[2] = true;

    assert(arr[1] == 13);
    assert(bw[$ - 6]);
    assert(bw[$ - 6] == bw2[$ - 6]);
    assert(bw[$ - 6] == bw3[$ - 6]);
}

// Test all range types over all integral types
@safe pure nothrow unittest
{
    import std.internal.test.dummyrange;

    alias IntegralTypes = AliasSeq!(byte, ubyte, short, ushort, int, uint,
            long, ulong);
    foreach (IntegralType; IntegralTypes)
    {
        foreach (T; AllDummyRangesType!(IntegralType[]))
        {
            T a;
            auto bw = Bitwise!T(a);

            static if (isForwardRange!T)
            {
                auto bwFwdSave = bw.save;
            }

            static if (isBidirectionalRange!T)
            {
                auto bwBack = bw.save;
                auto bwBackSave = bw.save;
            }

            static if (hasLength!T)
            {
                auto bwLength = bw.length;
                assert(bw.length == (IntegralType.sizeof * 8 * a.length));
                static if (isForwardRange!T)
                {
                    assert(bw.length == bwFwdSave.length);
                }
            }

            // Make sure front and back are not the mechanisms that modify the range
            long numCalls = 42;
            bool initialFrontValue;

            if (!bw.empty)
            {
                initialFrontValue = bw.front;
            }

            while (!bw.empty && (--numCalls))
            {
                bw.front;
                assert(bw.front == initialFrontValue);
            }

            /*
               Check that empty works properly and that popFront does not get called
               more times than it should
             */
            numCalls = 0;
            while (!bw.empty)
            {
                ++numCalls;

                static if (hasLength!T)
                {
                    assert(bw.length == bwLength);
                    --bwLength;
                }

                static if (isForwardRange!T)
                {
                    assert(bw.front == bwFwdSave.front);
                    bwFwdSave.popFront();
                }

                static if (isBidirectionalRange!T)
                {
                    assert(bwBack.front == bwBackSave.front);
                    bwBack.popBack();
                    bwBackSave.popBack();
                }
                bw.popFront();
            }

            auto rangeLen = numCalls / (IntegralType.sizeof * 8);
            assert(numCalls == (IntegralType.sizeof * 8 * rangeLen));
            assert(bw.empty);
            static if (isForwardRange!T)
            {
                assert(bwFwdSave.empty);
            }

            static if (isBidirectionalRange!T)
            {
                assert(bwBack.empty);
            }
        }
    }
}

// Test opIndex and opSlice
@system unittest
{
    alias IntegralTypes = AliasSeq!(byte, ubyte, short, ushort, int, uint,
            long, ulong);
    foreach (IntegralType; IntegralTypes)
    {
        size_t bitsNum = IntegralType.sizeof * 8;

        auto first = cast(IntegralType)(1);

        // 2 ^ (bitsNum - 1)
        auto second = cast(IntegralType)(cast(IntegralType)(1) << (bitsNum - 2));

        IntegralType[] a = [first, second];
        auto bw = Bitwise!(IntegralType[])(a);

        // Check against lsb of a[0]
        assert(bw[0] == true);
        // Check against msb - 1 of a[1]
        assert(bw[2 * bitsNum - 2] == true);

        bw.popFront();
        assert(bw[2 * bitsNum - 3] == true);

        import std.exception : assertThrown;

        // Check out of bounds error
        assertThrown!Error(bw[2 * bitsNum - 1]);

        bw[2] = true;
        assert(bw[2] == true);
        bw.popFront();
        assert(bw[1] == true);

        auto bw2 = bw[0 .. $ - 5];
        auto bw3 = bw2[];
        assert(bw2.length == (bw.length - 5));
        assert(bw2.length == bw3.length);
        bw2.popFront();
        assert(bw2.length != bw3.length);
    }
}

/*********************************
 * An OutputRange that discards the data it receives.
 */
struct NullSink
{
    void put(E)(E){}
}

///
@safe unittest
{
    import std.algorithm.iteration : map;
    import std.algorithm.mutation : copy;
    [4, 5, 6].map!(x => x * 2).copy(NullSink()); // data is discarded
}


/++

  Implements a "tee" style pipe, wrapping an input range so that elements of the
  range can be passed to a provided function or $(LREF OutputRange) as they are
  iterated over. This is useful for printing out intermediate values in a long
  chain of range code, performing some operation with side-effects on each call
  to $(D front) or $(D popFront), or diverting the elements of a range into an
  auxiliary $(LREF OutputRange).

  It is important to note that as the resultant range is evaluated lazily,
  in the case of the version of $(D tee) that takes a function, the function
  will not actually be executed until the range is "walked" using functions
  that evaluate ranges, such as $(REF array, std,array) or
  $(REF fold, std,algorithm,iteration).

  Params:
  pipeOnPop = If `Yes.pipeOnPop`, simply iterating the range without ever
  calling `front` is enough to have `tee` mirror elements to `outputRange` (or,
  respectively, `fun`). If `No.pipeOnPop`, only elements for which `front` does
  get called will be also sent to `outputRange`/`fun`.
  inputRange = The input range being passed through.
  outputRange = This range will receive elements of `inputRange` progressively
  as iteration proceeds.
  fun = This function will be called with elements of `inputRange`
  progressively as iteration proceeds.

  Returns:
  An input range that offers the elements of `inputRange`. Regardless of
  whether `inputRange` is a more powerful range (forward, bidirectional etc),
  the result is always an input range. Reading this causes `inputRange` to be
  iterated and returns its elements in turn. In addition, the same elements
  will be passed to `outputRange` or `fun` as well.

  See_Also: $(REF each, std,algorithm,iteration)
+/
auto tee(Flag!"pipeOnPop" pipeOnPop = Yes.pipeOnPop, R1, R2)(R1 inputRange, R2 outputRange)
if (isInputRange!R1 && isOutputRange!(R2, ElementType!R1))
{
    static struct Result
    {
        private R1 _input;
        private R2 _output;
        static if (!pipeOnPop)
        {
            private bool _frontAccessed;
        }

        static if (hasLength!R1)
        {
            @property auto length()
            {
                return _input.length;
            }
        }

        static if (isInfinite!R1)
        {
            enum bool empty = false;
        }
        else
        {
            @property bool empty() { return _input.empty; }
        }

        void popFront()
        {
            assert(!_input.empty, "Attempting to popFront an empty tee");
            static if (pipeOnPop)
            {
                put(_output, _input.front);
            }
            else
            {
                _frontAccessed = false;
            }
            _input.popFront();
        }

        @property auto ref front()
        {
            assert(!_input.empty, "Attempting to fetch the front of an empty tee");
            static if (!pipeOnPop)
            {
                if (!_frontAccessed)
                {
                    _frontAccessed = true;
                    put(_output, _input.front);
                }
            }
            return _input.front;
        }
    }

    return Result(inputRange, outputRange);
}

/// Ditto
auto tee(alias fun, Flag!"pipeOnPop" pipeOnPop = Yes.pipeOnPop, R1)(R1 inputRange)
if (is(typeof(fun) == void) || isSomeFunction!fun)
{
    import std.traits : isDelegate, isFunctionPointer;
    /*
        Distinguish between function literals and template lambdas
        when using either as an $(LREF OutputRange). Since a template
        has no type, typeof(template) will always return void.
        If it's a template lambda, it's first necessary to instantiate
        it with $(D ElementType!R1).
    */
    static if (is(typeof(fun) == void))
        alias _fun = fun!(ElementType!R1);
    else
        alias _fun = fun;

    static if (isFunctionPointer!_fun || isDelegate!_fun)
    {
        return tee!pipeOnPop(inputRange, _fun);
    }
    else
    {
        return tee!pipeOnPop(inputRange, &_fun);
    }
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter, map;

    // Sum values while copying
    int[] values = [1, 4, 9, 16, 25];
    int sum = 0;
    auto newValues = values.tee!(a => sum += a).array;
    assert(equal(newValues, values));
    assert(sum == 1 + 4 + 9 + 16 + 25);

    // Count values that pass the first filter
    int count = 0;
    auto newValues4 = values.filter!(a => a < 10)
                            .tee!(a => count++)
                            .map!(a => a + 1)
                            .filter!(a => a < 10);

    //Fine, equal also evaluates any lazy ranges passed to it.
    //count is not 3 until equal evaluates newValues4
    assert(equal(newValues4, [2, 5]));
    assert(count == 3);
}

//
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter, map;

    int[] values = [1, 4, 9, 16, 25];

    int count = 0;
    auto newValues = values.filter!(a => a < 10)
        .tee!(a => count++, No.pipeOnPop)
        .map!(a => a + 1)
        .filter!(a => a < 10);

    auto val = newValues.front;
    assert(count == 1);
    //front is only evaluated once per element
    val = newValues.front;
    assert(count == 1);

    //popFront() called, fun will be called
    //again on the next access to front
    newValues.popFront();
    newValues.front;
    assert(count == 2);

    int[] preMap = new int[](3), postMap = [];
    auto mappedValues = values.filter!(a => a < 10)
        //Note the two different ways of using tee
        .tee(preMap)
        .map!(a => a + 1)
        .tee!(a => postMap ~= a)
        .filter!(a => a < 10);
    assert(equal(mappedValues, [2, 5]));
    assert(equal(preMap, [1, 4, 9]));
    assert(equal(postMap, [2, 5, 10]));
}

//
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter, map;

    char[] txt = "Line one, Line 2".dup;

    bool isVowel(dchar c)
    {
        import std.string : indexOf;
        return "AaEeIiOoUu".indexOf(c) != -1;
    }

    int vowelCount = 0;
    int shiftedCount = 0;
    auto removeVowels = txt.tee!(c => isVowel(c) ? vowelCount++ : 0)
                                .filter!(c => !isVowel(c))
                                .map!(c => (c == ' ') ? c : c + 1)
                                .tee!(c => isVowel(c) ? shiftedCount++ : 0);
    assert(equal(removeVowels, "Mo o- Mo 3"));
    assert(vowelCount == 6);
    assert(shiftedCount == 3);
}

@safe unittest
{
    // Manually stride to test different pipe behavior.
    void testRange(Range)(Range r)
    {
        const int strideLen = 3;
        int i = 0;
        ElementType!Range elem1;
        ElementType!Range elem2;
        while (!r.empty)
        {
            if (i % strideLen == 0)
            {
                //Make sure front is only
                //evaluated once per item
                elem1 = r.front;
                elem2 = r.front;
                assert(elem1 == elem2);
            }
            r.popFront();
            i++;
        }
    }

    string txt = "abcdefghijklmnopqrstuvwxyz";

    int popCount = 0;
    auto pipeOnPop = txt.tee!(a => popCount++);
    testRange(pipeOnPop);
    assert(popCount == 26);

    int frontCount = 0;
    auto pipeOnFront = txt.tee!(a => frontCount++, No.pipeOnPop);
    testRange(pipeOnFront);
    assert(frontCount == 9);
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.meta : AliasSeq;

    //Test diverting elements to an OutputRange
    string txt = "abcdefghijklmnopqrstuvwxyz";

    dchar[] asink1 = [];
    auto fsink = (dchar c) { asink1 ~= c; };
    auto result1 = txt.tee(fsink).array;
    assert(equal(txt, result1) && (equal(result1, asink1)));

    dchar[] _asink1 = [];
    auto _result1 = txt.tee!((dchar c) { _asink1 ~= c; })().array;
    assert(equal(txt, _result1) && (equal(_result1, _asink1)));

    dchar[] asink2 = new dchar[](txt.length);
    void fsink2(dchar c) { static int i = 0; asink2[i] = c; i++; }
    auto result2 = txt.tee(&fsink2).array;
    assert(equal(txt, result2) && equal(result2, asink2));

    dchar[] asink3 = new dchar[](txt.length);
    auto result3 = txt.tee(asink3).array;
    assert(equal(txt, result3) && equal(result3, asink3));

    foreach (CharType; AliasSeq!(char, wchar, dchar))
    {
        auto appSink = appender!(CharType[])();
        auto appResult = txt.tee(appSink).array;
        assert(equal(txt, appResult) && equal(appResult, appSink.data));
    }

    foreach (StringType; AliasSeq!(string, wstring, dstring))
    {
        auto appSink = appender!StringType();
        auto appResult = txt.tee(appSink).array;
        assert(equal(txt, appResult) && equal(appResult, appSink.data));
    }
}

@safe unittest
{
    // Issue 13483
    static void func1(T)(T x) {}
    void func2(int x) {}

    auto r = [1, 2, 3, 4].tee!func1.tee!func2;
}

/**
Extends the length of the input range `r` by padding out the start of the
range with the element `e`. The element `e` must be of a common type with
the element type of the range `r` as defined by $(REF CommonType, std, traits).
If `n` is less than the length of of `r`, then `r` is returned unmodified.

If `r` is a string with Unicode characters in it, `padLeft` follows D's rules
about length for strings, which is not the number of characters, or
graphemes, but instead the number of encoding units. If you want to treat each
grapheme as only one encoding unit long, then call
$(REF byGrapheme, std, uni) before calling this function.

If `r` has a length, then this is $(BIGOH 1). Otherwise, it's $(BIGOH r.length).

Params:
    r = an input range with a length, or a forward range
    e = element to pad the range with
    n = the length to pad to

Returns:
    A range containing the elements of the original range with the extra padding

See Also:
    $(REF leftJustifier, std, string)
*/
auto padLeft(R, E)(R r, E e, size_t n)
if (
    ((isInputRange!R && hasLength!R) || isForwardRange!R) &&
    !is(CommonType!(ElementType!R, E) == void)
)
{
    static if (hasLength!R)
        auto dataLength = r.length;
    else
        auto dataLength = r.save.walkLength(n);

    return e.repeat(n > dataLength ? n - dataLength : 0).chain(r);
}

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    assert([1, 2, 3, 4].padLeft(0, 6).equal([0, 0, 1, 2, 3, 4]));
    assert([1, 2, 3, 4].padLeft(0, 3).equal([1, 2, 3, 4]));

    assert("abc".padLeft('_', 6).equal("___abc"));
}

@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : DummyRange, Length, RangeType, ReturnBy;
    import std.meta : AliasSeq;

    alias DummyRanges = AliasSeq!(
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Input),
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Forward),
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Bidirectional),
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random),
        DummyRange!(ReturnBy.Reference, Length.No, RangeType.Forward),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Input),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Forward),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Bidirectional),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Random),
        DummyRange!(ReturnBy.Value, Length.No, RangeType.Forward)
    );

    foreach (Range; DummyRanges)
    {
        Range r;
        assert(r
            .padLeft(0, 12)
            .equal([0, 0, 1U, 2U, 3U, 4U, 5U, 6U, 7U, 8U, 9U, 10U])
        );
    }
}

// Test nogc inference
@safe @nogc pure unittest
{
    import std.algorithm.comparison : equal;

    static immutable r1 = [1, 2, 3, 4];
    static immutable r2 = [0, 0, 1, 2, 3, 4];
    assert(r1.padLeft(0, 6).equal(r2));
}

/**
Extend the length of the input range `r` by padding out the end of the range
with the element `e`. The element `e` must be of a common type with the
element type of the range `r` as defined by $(REF CommonType, std, traits).
If `n` is less than the length of of `r`, then the contents of `r` are
returned.

The range primitives that the resulting range provides depends whether or not `r`
provides them. Except the functions `back` and `popBack`, which also require
the range to have a length as well as `back` and `popBack`

Params:
    r = an input range with a length
    e = element to pad the range with
    n = the length to pad to

Returns:
    A range containing the elements of the original range with the extra padding

See Also:
    $(REF rightJustifier, std, string)
*/
auto padRight(R, E)(R r, E e, size_t n)
if (
    isInputRange!R &&
    !isInfinite!R &&
    !is(CommonType!(ElementType!R, E) == void))
{
    static struct Result
    {
        private:
        R data;
        E element;
        size_t counter;
        static if (isBidirectionalRange!R && hasLength!R) size_t backPosition;
        size_t maxSize;

        public:
        bool empty() @property
        {
            return data.empty && counter >= maxSize;
        }

        auto front() @property
        {
            assert(!empty, "Attempting to fetch the front of an empty padRight");
            return data.empty ? element : data.front;
        }

        void popFront()
        {
            assert(!empty, "Attempting to popFront an empty padRight");
            ++counter;

            if (!data.empty)
            {
                data.popFront;
            }
        }

        static if (hasLength!R)
        {
            size_t length() @property
            {
                import std.algorithm.comparison : max;
                return max(data.length, maxSize);
            }
        }

        static if (isForwardRange!R)
        {
            auto save() @property
            {
                typeof(this) result = this;
                data = data.save;
                return result;
            }
        }

        static if (isBidirectionalRange!R && hasLength!R)
        {
            auto back() @property
            {
                assert(!empty, "Attempting to fetch the back of an empty padRight");
                return backPosition > data.length ? element : data.back;
            }

            void popBack()
            {
                assert(!empty, "Attempting to popBack an empty padRight");
                if (backPosition > data.length)
                {
                    --backPosition;
                    --maxSize;
                }
                else
                {
                    data.popBack;
                }
            }
        }

        static if (isRandomAccessRange!R && hasLength!R)
        {
            E opIndex(size_t index)
            {
                assert(index <= this.length, "Index out of bounds");
                return (index > data.length && index <= maxSize) ? element :
                    data[index];
            }
        }

        static if (hasSlicing!R && hasLength!R)
        {
            auto opSlice(size_t a, size_t b)
            {
                assert(
                    a <= b,
                    "Attempting to slice a padRight with a larger first argument than the second."
                );
                assert(
                    b <= length,
                    "Attempting to slice using an out of bounds index on a padRight"
                );
                return Result((b <= data.length) ? data[a .. b] : data[a .. data.length],
                    element, b - a);
            }

            alias opDollar = length;
        }

        this(R r, E e, size_t max)
        {
            data = r;
            element = e;
            maxSize = max;
            static if (isBidirectionalRange!R && hasLength!R)
                backPosition = max;
        }

        @disable this();
    }

    return Result(r, e, n);
}

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    assert([1, 2, 3, 4].padRight(0, 6).equal([1, 2, 3, 4, 0, 0]));
    assert([1, 2, 3, 4].padRight(0, 4).equal([1, 2, 3, 4]));

    assert("abc".padRight('_', 6).equal("abc___"));
}

pure @safe unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : AllDummyRanges, ReferenceInputRange;
    import std.meta : AliasSeq;

    auto string_input_range = new ReferenceInputRange!dchar(['a', 'b', 'c']);
    dchar padding = '_';
    assert(string_input_range.padRight(padding, 6).equal("abc___"));

    foreach (RangeType; AllDummyRanges)
    {
        RangeType r1;
        assert(r1
            .padRight(0, 12)
            .equal([1U, 2U, 3U, 4U, 5U, 6U, 7U, 8U, 9U, 10U, 0, 0])
        );

        // test if Result properly uses random access ranges
        static if (isRandomAccessRange!RangeType)
        {
            RangeType r3;
            assert(r3.padRight(0, 12)[0] == 1);
            assert(r3.padRight(0, 12)[2] == 3);
            assert(r3.padRight(0, 12)[11] == 0);
        }

        // test if Result properly uses slicing and opDollar
        static if (hasSlicing!RangeType)
        {
            RangeType r4;
            assert(r4
                .padRight(0, 12)[0 .. 3]
                .equal([1, 2, 3])
            );
            assert(r4
                .padRight(0, 12)[2 .. $]
                .equal([3U, 4U, 5U, 6U, 7U, 8U, 9U, 10U, 0, 0])
            );
            assert(r4
                .padRight(0, 12)[0 .. $]
                .equal([1U, 2U, 3U, 4U, 5U, 6U, 7U, 8U, 9U, 10U, 0, 0])
            );
        }
    }
}

// Test nogc inference
@safe @nogc pure unittest
{
    import std.algorithm.comparison : equal;

    static immutable r1 = [1, 2, 3, 4];
    static immutable r2 = [1, 2, 3, 4, 0, 0];
    assert(r1.padRight(0, 6).equal(r2));
}
