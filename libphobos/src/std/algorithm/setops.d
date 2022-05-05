// Written in the D programming language.
/**
This is a submodule of $(MREF std, algorithm).
It contains generic algorithms that implement set operations.

The functions $(LREF multiwayMerge), $(LREF multiwayUnion), $(LREF setDifference),
$(LREF setIntersection), $(LREF setSymmetricDifference) expect a range of sorted
ranges as input.

All algorithms are generalized to accept as input not only sets but also
$(HTTP https://en.wikipedia.org/wiki/Multiset, multisets). Each algorithm
documents behaviour in the presence of duplicated inputs.

$(SCRIPT inhibitQuickIndex = 1;)
$(BOOKTABLE Cheat Sheet,
$(TR $(TH Function Name) $(TH Description))
$(T2 cartesianProduct,
        Computes Cartesian product of two ranges.)
$(T2 largestPartialIntersection,
        Copies out the values that occur most frequently in a range of ranges.)
$(T2 largestPartialIntersectionWeighted,
        Copies out the values that occur most frequently (multiplied by
        per-value weights) in a range of ranges.)
$(T2 multiwayMerge,
        Merges a range of sorted ranges.)
$(T2 multiwayUnion,
        Computes the union of a range of sorted ranges.)
$(T2 setDifference,
        Lazily computes the set difference of two or more sorted ranges.)
$(T2 setIntersection,
        Lazily computes the intersection of two or more sorted ranges.)
$(T2 setSymmetricDifference,
        Lazily computes the symmetric set difference of two or more sorted
        ranges.)
)

Copyright: Andrei Alexandrescu 2008-.

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP erdani.com, Andrei Alexandrescu)

Source: $(PHOBOSSRC std/algorithm/setops.d)

Macros:
T2=$(TR $(TDNW $(LREF $1)) $(TD $+))
 */
module std.algorithm.setops;

import std.range.primitives;

import std.functional : unaryFun, binaryFun;
import std.traits;
import std.meta : AliasSeq, staticMap, allSatisfy, anySatisfy;

import std.algorithm.sorting : Merge;
import std.typecons : No;

// cartesianProduct
/**
Lazily computes the Cartesian product of two or more ranges. The product is a
range of tuples of elements from each respective range.

The conditions for the two-range case are as follows:

If both ranges are finite, then one must be (at least) a
$(REF_ALTTEXT forward range, isForwardRange, std,range,primitives) and the
other an $(REF_ALTTEXT input range, isInputRange, std,range,primitives).

If one range is infinite and the other finite, then the finite range must
be a forward range, and the infinite range can be an input range.

If both ranges are infinite, then both must be forward ranges.

When there are more than two ranges, the above conditions apply to each
adjacent pair of ranges.

Params:
    range1 = The first range
    range2 = The second range
    ranges = Two or more non-infinite forward ranges
    otherRanges = Zero or more non-infinite forward ranges

Returns:
    A forward range of $(REF Tuple, std,typecons) representing elements of the
    cartesian product of the given ranges.
*/
auto cartesianProduct(R1, R2)(R1 range1, R2 range2)
if (!allSatisfy!(isForwardRange, R1, R2) ||
    anySatisfy!(isInfinite, R1, R2))
{
    import std.algorithm.iteration : map, joiner;

    static if (isInfinite!R1 && isInfinite!R2)
    {
        static if (isForwardRange!R1 && isForwardRange!R2)
        {
            import std.range : zip, repeat, take, chain, sequence;

            // This algorithm traverses the cartesian product by alternately
            // covering the right and bottom edges of an increasing square area
            // over the infinite table of combinations. This schedule allows us
            // to require only forward ranges.
            return zip(sequence!"n"(cast(size_t) 0), range1.save, range2.save,
                       repeat(range1), repeat(range2))
                .map!(function(a) => chain(
                    zip(repeat(a[1]), take(a[4].save, a[0])),
                    zip(take(a[3].save, a[0]+1), repeat(a[2]))
                ))()
                .joiner();
        }
        else static assert(0, "cartesianProduct of infinite ranges requires "~
                              "forward ranges");
    }
    else static if (isInputRange!R1 && isForwardRange!R2 && !isInfinite!R2)
    {
        import std.range : zip, repeat;
        return joiner(map!((ElementType!R1 a) => zip(repeat(a), range2.save))
                          (range1));
    }
    else static if (isInputRange!R2 && isForwardRange!R1 && !isInfinite!R1)
    {
        import std.range : zip, repeat;
        return joiner(map!((ElementType!R2 a) => zip(range1.save, repeat(a)))
                          (range2));
    }
    else static assert(0, "cartesianProduct involving finite ranges must "~
                          "have at least one finite forward range");
}

///
@safe unittest
{
    import std.algorithm.searching : canFind;
    import std.range;
    import std.typecons : tuple;

    auto N = sequence!"n"(0);         // the range of natural numbers
    auto N2 = cartesianProduct(N, N); // the range of all pairs of natural numbers

    // Various arbitrary number pairs can be found in the range in finite time.
    assert(canFind(N2, tuple(0, 0)));
    assert(canFind(N2, tuple(123, 321)));
    assert(canFind(N2, tuple(11, 35)));
    assert(canFind(N2, tuple(279, 172)));
}

///
@safe unittest
{
    import std.algorithm.searching : canFind;
    import std.typecons : tuple;

    auto B = [ 1, 2, 3 ];
    auto C = [ 4, 5, 6 ];
    auto BC = cartesianProduct(B, C);

    foreach (n; [[1, 4], [2, 4], [3, 4], [1, 5], [2, 5], [3, 5], [1, 6],
                 [2, 6], [3, 6]])
    {
        assert(canFind(BC, tuple(n[0], n[1])));
    }
}

@safe unittest
{
    // Test cartesian product of two infinite ranges
    import std.algorithm.searching : canFind;
    import std.range;
    import std.typecons : tuple;

    auto Even = sequence!"2*n"(0);
    auto Odd = sequence!"2*n+1"(0);
    auto EvenOdd = cartesianProduct(Even, Odd);

    foreach (pair; [[0, 1], [2, 1], [0, 3], [2, 3], [4, 1], [4, 3], [0, 5],
                    [2, 5], [4, 5], [6, 1], [6, 3], [6, 5]])
    {
        assert(canFind(EvenOdd, tuple(pair[0], pair[1])));
    }

    // This should terminate in finite time
    assert(canFind(EvenOdd, tuple(124, 73)));
    assert(canFind(EvenOdd, tuple(0, 97)));
    assert(canFind(EvenOdd, tuple(42, 1)));
}

@safe unittest
{
    // Test cartesian product of an infinite input range and a finite forward
    // range.
    import std.algorithm.searching : canFind;
    import std.range;
    import std.typecons : tuple;

    auto N = sequence!"n"(0);
    auto M = [100, 200, 300];
    auto NM = cartesianProduct(N,M);

    foreach (pair; [[0, 100], [0, 200], [0, 300], [1, 100], [1, 200], [1, 300],
                    [2, 100], [2, 200], [2, 300], [3, 100], [3, 200],
                    [3, 300]])
    {
        assert(canFind(NM, tuple(pair[0], pair[1])));
    }

    // We can't solve the halting problem, so we can only check a finite
    // initial segment here.
    assert(!canFind(NM.take(100), tuple(100, 0)));
    assert(!canFind(NM.take(100), tuple(1, 1)));
    assert(!canFind(NM.take(100), tuple(100, 200)));

    auto MN = cartesianProduct(M,N);
    foreach (pair; [[100, 0], [200, 0], [300, 0], [100, 1], [200, 1], [300, 1],
                    [100, 2], [200, 2], [300, 2], [100, 3], [200, 3],
                    [300, 3]])
    {
        assert(canFind(MN, tuple(pair[0], pair[1])));
    }

    // We can't solve the halting problem, so we can only check a finite
    // initial segment here.
    assert(!canFind(MN.take(100), tuple(0, 100)));
    assert(!canFind(MN.take(100), tuple(0, 1)));
    assert(!canFind(MN.take(100), tuple(100, 200)));
}

@safe unittest
{
    import std.algorithm.searching : canFind;
    import std.typecons : tuple;

    // Test cartesian product of two finite ranges.
    auto X = [1, 2, 3];
    auto Y = [4, 5, 6];
    auto XY = cartesianProduct(X, Y);
    auto Expected = [[1, 4], [1, 5], [1, 6], [2, 4], [2, 5], [2, 6], [3, 4],
                     [3, 5], [3, 6]];

    // Verify Expected ⊆ XY
    foreach (pair; Expected)
    {
        assert(canFind(XY, tuple(pair[0], pair[1])));
    }

    // Verify XY ⊆ Expected
    foreach (pair; XY)
    {
        assert(canFind(Expected, [pair[0], pair[1]]));
    }

    // And therefore, by set comprehension, XY == Expected
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    import std.algorithm.searching : canFind;
    import std.typecons : tuple;

    import std.range;
    auto N = sequence!"n"(0);

    // To force the template to fall to the second case, we wrap N in a struct
    // that doesn't allow bidirectional access.
    struct FwdRangeWrapper(R)
    {
        R impl;

        // Input range API
        @property auto front() { return impl.front; }
        void popFront() { impl.popFront(); }
        static if (isInfinite!R)
            enum empty = false;
        else
            @property bool empty() { return impl.empty; }

        // Forward range API
        @property auto save() { return typeof(this)(impl.save); }
    }
    auto fwdWrap(R)(R range) { return FwdRangeWrapper!R(range); }

    // General test: two infinite bidirectional ranges
    auto N2 = cartesianProduct(N, N);

    assert(canFind(N2, tuple(0, 0)));
    assert(canFind(N2, tuple(123, 321)));
    assert(canFind(N2, tuple(11, 35)));
    assert(canFind(N2, tuple(279, 172)));

    // Test first case: forward range with bidirectional range
    auto fwdN = fwdWrap(N);
    auto N2_a = cartesianProduct(fwdN, N);

    assert(canFind(N2_a, tuple(0, 0)));
    assert(canFind(N2_a, tuple(123, 321)));
    assert(canFind(N2_a, tuple(11, 35)));
    assert(canFind(N2_a, tuple(279, 172)));

    // Test second case: bidirectional range with forward range
    auto N2_b = cartesianProduct(N, fwdN);

    assert(canFind(N2_b, tuple(0, 0)));
    assert(canFind(N2_b, tuple(123, 321)));
    assert(canFind(N2_b, tuple(11, 35)));
    assert(canFind(N2_b, tuple(279, 172)));

    // Test third case: finite forward range with (infinite) input range
    static struct InpRangeWrapper(R)
    {
        R impl;

        // Input range API
        @property auto front() { return impl.front; }
        void popFront() { impl.popFront(); }
        static if (isInfinite!R)
            enum empty = false;
        else
            @property bool empty() { return impl.empty; }
    }
    auto inpWrap(R)(R r) { return InpRangeWrapper!R(r); }

    auto inpN = inpWrap(N);
    auto B = [ 1, 2, 3 ];
    auto fwdB = fwdWrap(B);
    auto BN = cartesianProduct(fwdB, inpN);

    assert(equal(map!"[a[0],a[1]]"(BN.take(10)), [[1, 0], [2, 0], [3, 0],
                 [1, 1], [2, 1], [3, 1], [1, 2], [2, 2], [3, 2], [1, 3]]));

    // Test fourth case: (infinite) input range with finite forward range
    auto NB = cartesianProduct(inpN, fwdB);

    assert(equal(map!"[a[0],a[1]]"(NB.take(10)), [[0, 1], [0, 2], [0, 3],
                 [1, 1], [1, 2], [1, 3], [2, 1], [2, 2], [2, 3], [3, 1]]));

    // General finite range case
    auto C = [ 4, 5, 6 ];
    auto BC = cartesianProduct(B, C);

    foreach (n; [[1, 4], [2, 4], [3, 4], [1, 5], [2, 5], [3, 5], [1, 6],
                 [2, 6], [3, 6]])
    {
        assert(canFind(BC, tuple(n[0], n[1])));
    }
}

// https://issues.dlang.org/show_bug.cgi?id=13091
pure nothrow @safe @nogc unittest
{
    int[1] a = [1];
    foreach (t; cartesianProduct(a[], a[])) {}
}

/// ditto
auto cartesianProduct(RR...)(RR ranges)
if (ranges.length >= 2 &&
    allSatisfy!(isForwardRange, RR) &&
    !anySatisfy!(isInfinite, RR))
{
    // This overload uses a much less template-heavy implementation when
    // all ranges are finite forward ranges, which is the most common use
    // case, so that we don't run out of resources too quickly.
    //
    // For infinite ranges or non-forward ranges, we fall back to the old
    // implementation which expands an exponential number of templates.
    import std.typecons : tuple;

    static struct Result
    {
        RR ranges;
        RR current;
        bool empty = true;

        this(RR _ranges)
        {
            ranges = _ranges;
            empty = false;
            foreach (i, r; ranges)
            {
                current[i] = r.save;
                if (current[i].empty)
                    empty = true;
            }
        }
        @property auto front()
        {
            import std.algorithm.internal : algoFormat;
            import std.range : iota;
            return mixin(algoFormat("tuple(%(current[%d].front%|,%))",
                                    iota(0, current.length)));
        }
        void popFront() scope
        {
            foreach_reverse (i, ref r; current)
            {
                r.popFront();
                if (!r.empty) break;

                static if (i == 0)
                    empty = true;
                else
                    r = ranges[i].save; // rollover
            }
        }
        @property Result save() return scope
        {
            Result copy = this;
            foreach (i, r; ranges)
            {
                copy.ranges[i] = ranges[i].save;
                copy.current[i] = current[i].save;
            }
            return copy;
        }
    }
    static assert(isForwardRange!Result, Result.stringof ~ " must be a forward"
            ~ " range");

    return Result(ranges);
}

// cartesian product of empty ranges should be empty
// https://issues.dlang.org/show_bug.cgi?id=10693
@safe unittest
{
    int[] a, b, c, d, e;
    auto cprod = cartesianProduct(a,b,c,d,e);
    assert(cprod.empty);
    foreach (_; cprod) {} // should not crash

    // Test case where only one of the ranges is empty: the result should still
    // be empty.
    int[] p=[1], q=[];
    auto cprod2 = cartesianProduct(p,p,p,q,p);
    assert(cprod2.empty);
    foreach (_; cprod2) {} // should not crash
}

@safe unittest
{
    // .init value of cartesianProduct should be empty
    auto cprod = cartesianProduct([0,0], [1,1], [2,2]);
    assert(!cprod.empty);
    assert(cprod.init.empty);
}

// https://issues.dlang.org/show_bug.cgi?id=13393
@safe unittest
{
    assert(!cartesianProduct([0],[0],[0]).save.empty);
}

/// ditto
auto cartesianProduct(R1, R2, RR...)(R1 range1, R2 range2, RR otherRanges)
if (!allSatisfy!(isForwardRange, R1, R2, RR) ||
    anySatisfy!(isInfinite, R1, R2, RR))
{
    /* We implement the n-ary cartesian product by recursively invoking the
     * binary cartesian product. To make the resulting range nicer, we denest
     * one level of tuples so that a ternary cartesian product, for example,
     * returns 3-element tuples instead of nested 2-element tuples.
     */
    import std.algorithm.internal : algoFormat;
    import std.algorithm.iteration : map;
    import std.range : iota;

    enum string denest = algoFormat("tuple(a[0], %(a[1][%d]%|,%))",
                                iota(0, otherRanges.length+1));
    return map!denest(
        cartesianProduct(range1, cartesianProduct(range2, otherRanges))
    );
}

@safe unittest
{
    import std.algorithm.searching : canFind;
    import std.range;
    import std.typecons : tuple, Tuple;

    auto N = sequence!"n"(0);
    auto N3 = cartesianProduct(N, N, N);

    // Check that tuples are properly denested
    assert(is(ElementType!(typeof(N3)) == Tuple!(size_t,size_t,size_t)));

    assert(canFind(N3, tuple(0, 27, 7)));
    assert(canFind(N3, tuple(50, 23, 11)));
    assert(canFind(N3, tuple(9, 3, 0)));
}

@safe unittest
{
    import std.algorithm.searching : canFind;
    import std.range;
    import std.typecons : tuple, Tuple;

    auto N = sequence!"n"(0);
    auto N4 = cartesianProduct(N, N, N, N);

    // Check that tuples are properly denested
    assert(is(ElementType!(typeof(N4)) == Tuple!(size_t,size_t,size_t,size_t)));

    assert(canFind(N4, tuple(1, 2, 3, 4)));
    assert(canFind(N4, tuple(4, 3, 2, 1)));
    assert(canFind(N4, tuple(10, 3, 1, 2)));
}

// https://issues.dlang.org/show_bug.cgi?id=9878
///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : tuple;

    auto A = [ 1, 2, 3 ];
    auto B = [ 'a', 'b', 'c' ];
    auto C = [ "x", "y", "z" ];
    auto ABC = cartesianProduct(A, B, C);

    assert(ABC.equal([
        tuple(1, 'a', "x"), tuple(1, 'a', "y"), tuple(1, 'a', "z"),
        tuple(1, 'b', "x"), tuple(1, 'b', "y"), tuple(1, 'b', "z"),
        tuple(1, 'c', "x"), tuple(1, 'c', "y"), tuple(1, 'c', "z"),
        tuple(2, 'a', "x"), tuple(2, 'a', "y"), tuple(2, 'a', "z"),
        tuple(2, 'b', "x"), tuple(2, 'b', "y"), tuple(2, 'b', "z"),
        tuple(2, 'c', "x"), tuple(2, 'c', "y"), tuple(2, 'c', "z"),
        tuple(3, 'a', "x"), tuple(3, 'a', "y"), tuple(3, 'a', "z"),
        tuple(3, 'b', "x"), tuple(3, 'b', "y"), tuple(3, 'b', "z"),
        tuple(3, 'c', "x"), tuple(3, 'c', "y"), tuple(3, 'c', "z")
    ]));
}

pure @safe nothrow @nogc unittest
{
    import std.range.primitives : isForwardRange;
    int[2] A = [1,2];
    auto C = cartesianProduct(A[], A[], A[]);
    assert(isForwardRange!(typeof(C)));

    C.popFront();
    auto front1 = C.front;
    auto D = C.save;
    C.popFront();
    assert(D.front == front1);
}

// https://issues.dlang.org/show_bug.cgi?id=13935
@safe unittest
{
    import std.algorithm.iteration : map;
    auto seq = [1, 2].map!(x => x);
    foreach (pair; cartesianProduct(seq, seq)) {}
}

@system unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : tuple;

    static struct SystemRange
    {
        int[] data;

        int front() @system @property inout
        {
            return data[0];
        }

        bool empty() @system @property inout
        {
            return data.length == 0;
        }

        void popFront() @system
        {
            data = data[1 .. $];
        }

        SystemRange save() @system
        {
            return this;
        }
    }

    assert(SystemRange([1, 2]).cartesianProduct(SystemRange([3, 4]))
        .equal([tuple(1, 3), tuple(1, 4), tuple(2, 3), tuple(2, 4)]));
}

// largestPartialIntersection
/**
Given a range of sorted $(REF_ALTTEXT forward ranges, isForwardRange, std,range,primitives)
`ror`, copies to `tgt` the elements that are common to most ranges, along with their number
of occurrences. All ranges in `ror` are assumed to be sorted by $(D
less). Only the most frequent `tgt.length` elements are returned.

Params:
    less = The predicate the ranges are sorted by.
    ror = A range of forward ranges sorted by `less`.
    tgt = The target range to copy common elements to.
    sorted = Whether the elements copied should be in sorted order.

The function `largestPartialIntersection` is useful for
e.g. searching an $(LINK2 https://en.wikipedia.org/wiki/Inverted_index,
inverted index) for the documents most
likely to contain some terms of interest. The complexity of the search
is $(BIGOH n * log(tgt.length)), where `n` is the sum of lengths of
all input ranges. This approach is faster than keeping an associative
array of the occurrences and then selecting its top items, and also
requires less memory (`largestPartialIntersection` builds its
result directly in `tgt` and requires no extra memory).

If at least one of the ranges is a multiset, then all occurences
of a duplicate element are taken into account. The result is
equivalent to merging all ranges and picking the most frequent
`tgt.length` elements.

Warning: Because `largestPartialIntersection` does not allocate
extra memory, it will leave `ror` modified. Namely, $(D
largestPartialIntersection) assumes ownership of `ror` and
discretionarily swaps and advances elements of it. If you want $(D
ror) to preserve its contents after the call, you may want to pass a
duplicate to `largestPartialIntersection` (and perhaps cache the
duplicate in between calls).
 */
void largestPartialIntersection
(alias less = "a < b", RangeOfRanges, Range)
(RangeOfRanges ror, Range tgt, SortOutput sorted = No.sortOutput)
{
    struct UnitWeights
    {
        static int opIndex(ElementType!(ElementType!RangeOfRanges)) { return 1; }
    }
    return largestPartialIntersectionWeighted!less(ror, tgt, UnitWeights(),
            sorted);
}

///
@system unittest
{
    import std.typecons : tuple, Tuple;

    // Figure which number can be found in most arrays of the set of
    // arrays below.
    double[][] a =
    [
        [ 1, 4, 7, 8 ],
        [ 1, 7 ],
        [ 1, 7, 8],
        [ 4 ],
        [ 7 ],
    ];
    auto b = new Tuple!(double, uint)[1];
    // it will modify the input range, hence we need to create a duplicate
    largestPartialIntersection(a.dup, b);
    // First member is the item, second is the occurrence count
    assert(b[0] == tuple(7.0, 4u));
    // 7.0 occurs in 4 out of 5 inputs, more than any other number

    // If more of the top-frequent numbers are needed, just create a larger
    // tgt range
    auto c = new Tuple!(double, uint)[2];
    largestPartialIntersection(a, c);
    assert(c[0] == tuple(1.0, 3u));
    // 1.0 occurs in 3 inputs

    // multiset
    double[][] x =
    [
        [1, 1, 1, 1, 4, 7, 8],
        [1, 7],
        [1, 7, 8],
        [4, 7],
        [7]
    ];
    auto y = new Tuple!(double, uint)[2];
    largestPartialIntersection(x.dup, y);
    // 7.0 occurs 5 times
    assert(y[0] == tuple(7.0, 5u));
    // 1.0 occurs 6 times
    assert(y[1] == tuple(1.0, 6u));
}

import std.algorithm.sorting : SortOutput; // FIXME

// largestPartialIntersectionWeighted
/**
Similar to `largestPartialIntersection`, but associates a weight
with each distinct element in the intersection.

If at least one of the ranges is a multiset, then all occurences
of a duplicate element are taken into account. The result
is equivalent to merging all input ranges and picking the highest
`tgt.length`, weight-based ranking elements.

Params:
    less = The predicate the ranges are sorted by.
    ror = A range of $(REF_ALTTEXT forward ranges, isForwardRange, std,range,primitives)
    sorted by `less`.
    tgt = The target range to copy common elements to.
    weights = An associative array mapping elements to weights.
    sorted = Whether the elements copied should be in sorted order.

*/
void largestPartialIntersectionWeighted
(alias less = "a < b", RangeOfRanges, Range, WeightsAA)
(RangeOfRanges ror, Range tgt, WeightsAA weights, SortOutput sorted = No.sortOutput)
{
    import std.algorithm.iteration : group;
    import std.algorithm.sorting : topNCopy;

    if (tgt.empty) return;
    alias InfoType = ElementType!Range;
    bool heapComp(InfoType a, InfoType b)
    {
        return weights[a[0]] * a[1] > weights[b[0]] * b[1];
    }
    topNCopy!heapComp(group(multiwayMerge!less(ror)), tgt, sorted);
}

///
@system unittest
{
    import std.typecons : tuple, Tuple;

    // Figure which number can be found in most arrays of the set of
    // arrays below, with specific per-element weights
    double[][] a =
    [
        [ 1, 4, 7, 8 ],
        [ 1, 7 ],
        [ 1, 7, 8],
        [ 4 ],
        [ 7 ],
    ];
    auto b = new Tuple!(double, uint)[1];
    double[double] weights = [ 1:1.2, 4:2.3, 7:1.1, 8:1.1 ];
    largestPartialIntersectionWeighted(a, b, weights);
    // First member is the item, second is the occurrence count
    assert(b[0] == tuple(4.0, 2u));
    // 4.0 occurs 2 times -> 4.6 (2 * 2.3)
    // 7.0 occurs 3 times -> 4.4 (3 * 1.1)

   // multiset
    double[][] x =
    [
        [ 1, 1, 1, 4, 7, 8 ],
        [ 1, 7 ],
        [ 1, 7, 8],
        [ 4 ],
        [ 7 ],
    ];
    auto y = new Tuple!(double, uint)[1];
    largestPartialIntersectionWeighted(x, y, weights);
    assert(y[0] == tuple(1.0, 5u));
    // 1.0 occurs 5 times -> 1.2 * 5 = 6
}

@system unittest
{
    import std.conv : text;
    import std.typecons : tuple, Tuple, Yes;

    double[][] a =
        [
            [ 1, 4, 7, 8 ],
            [ 1, 7 ],
            [ 1, 7, 8],
            [ 4 ],
            [ 7 ],
        ];
    auto b = new Tuple!(double, uint)[2];
    largestPartialIntersection(a, b, Yes.sortOutput);
    assert(b == [ tuple(7.0, 4u), tuple(1.0, 3u) ][], text(b));
    assert(a[0].empty);
}

@system unittest
{
    import std.conv : text;
    import std.typecons : tuple, Tuple, Yes;

    string[][] a =
        [
            [ "1", "4", "7", "8" ],
            [ "1", "7" ],
            [ "1", "7", "8"],
            [ "4" ],
            [ "7" ],
        ];
    auto b = new Tuple!(string, uint)[2];
    largestPartialIntersection(a, b, Yes.sortOutput);
    assert(b == [ tuple("7", 4u), tuple("1", 3u) ][], text(b));
}

@system unittest
{
    import std.typecons : tuple, Tuple;

    // Figure which number can be found in most arrays of the set of
    // arrays below, with specific per-element weights
    double[][] a =
        [
            [ 1, 4, 7, 8 ],
            [ 1, 7 ],
            [ 1, 7, 8],
            [ 4 ],
            [ 7 ],
            ];
    auto b = new Tuple!(double, uint)[1];
    double[double] weights = [ 1:1.2, 4:2.3, 7:1.1, 8:1.1 ];
    largestPartialIntersectionWeighted(a, b, weights);
    // First member is the item, second is the occurrence count
    assert(b[0] == tuple(4.0, 2u));
}

@system unittest
{
    import std.container : Array;
    import std.typecons : Tuple;

    alias T = Tuple!(uint, uint);
    const Array!T arrayOne = Array!T( [ T(1,2), T(3,4) ] );
    const Array!T arrayTwo = Array!T([ T(1,2), T(3,4) ] );

    assert(arrayOne == arrayTwo);
}

// MultiwayMerge
/**
Merges multiple sets. The input sets are passed as a
range of ranges and each is assumed to be sorted by $(D
less). Computation is done lazily, one union element at a time. The
complexity of one `popFront` operation is $(BIGOH
log(ror.length)). However, the length of `ror` decreases as ranges
in it are exhausted, so the complexity of a full pass through $(D
MultiwayMerge) is dependent on the distribution of the lengths of ranges
contained within `ror`. If all ranges have the same length `n`
(worst case scenario), the complexity of a full pass through $(D
MultiwayMerge) is $(BIGOH n * ror.length * log(ror.length)), i.e., $(D
log(ror.length)) times worse than just spanning all ranges in
turn. The output comes sorted (unstably) by `less`.

The length of the resulting range is the sum of all lengths of
the ranges passed as input. This means that all elements (duplicates
included) are transferred to the resulting range.

For backward compatibility, `multiwayMerge` is available under
the name `nWayUnion` and `MultiwayMerge` under the name of `NWayUnion` .
Future code should use `multiwayMerge` and `MultiwayMerge` as `nWayUnion`
and `NWayUnion` will be deprecated.

Params:
    less = Predicate the given ranges are sorted by.
    ror = A range of ranges sorted by `less` to compute the union for.

Returns:
    A range of the union of the ranges in `ror`.

Warning: Because `MultiwayMerge` does not allocate extra memory, it
will leave `ror` modified. Namely, `MultiwayMerge` assumes ownership
of `ror` and discretionarily swaps and advances elements of it. If
you want `ror` to preserve its contents after the call, you may
want to pass a duplicate to `MultiwayMerge` (and perhaps cache the
duplicate in between calls).

See_Also: $(REF merge, std,algorithm,sorting) for an analogous function that
    takes a static number of ranges of possibly disparate types.
 */
struct MultiwayMerge(alias less, RangeOfRanges)
{
    import std.container : BinaryHeap;

    private alias ElementType = .ElementType!(.ElementType!RangeOfRanges);
    private alias comp = binaryFun!less;
    private RangeOfRanges _ror;

    ///
    static bool compFront(.ElementType!RangeOfRanges a,
            .ElementType!RangeOfRanges b)
    {
        // revert comparison order so we get the smallest elements first
        return comp(b.front, a.front);
    }
    private BinaryHeap!(RangeOfRanges, compFront) _heap;

    ///
    this(RangeOfRanges ror)
    {
        import std.algorithm.mutation : remove, SwapStrategy;

        // Preemptively get rid of all empty ranges in the input
        // No need for stability either
        _ror = remove!("a.empty", SwapStrategy.unstable)(ror);
        //Build the heap across the range
        _heap.acquire(_ror);
    }

    ///
    @property bool empty() { return _ror.empty; }

    ///
    @property auto ref front()
    {
        return _heap.front.front;
    }

    ///
    void popFront()
    {
        _heap.removeFront();
        // let's look at the guy just popped
        _ror.back.popFront();
        if (_ror.back.empty)
        {
            _ror.popBack();
            // nothing else to do: the empty range is not in the
            // heap and not in _ror
            return;
        }
        // Put the popped range back in the heap
        const bool worked = _heap.conditionalInsert(_ror.back);
        assert(worked, "Failed to insert item into heap");
    }
}

/// Ditto
MultiwayMerge!(less, RangeOfRanges) multiwayMerge
(alias less = "a < b", RangeOfRanges)
(RangeOfRanges ror)
{
    return typeof(return)(ror);
}

///
@system unittest
{
    import std.algorithm.comparison : equal;

    double[][] a =
    [
        [ 1, 4, 7, 8 ],
        [ 1, 7 ],
        [ 1, 7, 8],
        [ 4 ],
        [ 7 ],
    ];
    auto witness = [
        1, 1, 1, 4, 4, 7, 7, 7, 7, 8, 8
    ];
    assert(equal(multiwayMerge(a), witness));

    double[][] b =
    [
        // range with duplicates
        [ 1, 1, 4, 7, 8 ],
        [ 7 ],
        [ 1, 7, 8],
        [ 4 ],
        [ 7 ],
    ];
    // duplicates are propagated to the resulting range
    assert(equal(multiwayMerge(b), witness));
}

alias nWayUnion = multiwayMerge;
alias NWayUnion = MultiwayMerge;

/**
Computes the union of multiple ranges. The
$(REF_ALTTEXT input ranges, isInputRange, std,range,primitives) are passed
as a range of ranges and each is assumed to be sorted by $(D
less). Computation is done lazily, one union element at a time.
`multiwayUnion(ror)` is functionally equivalent to `multiwayMerge(ror).uniq`.

"The output of multiwayUnion has no duplicates even when its inputs contain duplicates."

Params:
    less = Predicate the given ranges are sorted by.
    ror = A range of ranges sorted by `less` to compute the intersection for.

Returns:
    A range of the union of the ranges in `ror`.

See also: $(LREF multiwayMerge)
 */
auto multiwayUnion(alias less = "a < b", RangeOfRanges)(RangeOfRanges ror)
{
    import std.algorithm.iteration : uniq;
    import std.functional : not;
    return ror.multiwayMerge!(less).uniq!(not!less);
}

///
@system unittest
{
    import std.algorithm.comparison : equal;

    // sets
    double[][] a =
    [
        [ 1, 4, 7, 8 ],
        [ 1, 7 ],
        [ 1, 7, 8],
        [ 4 ],
        [ 7 ],
    ];

    auto witness = [1, 4, 7, 8];
    assert(equal(multiwayUnion(a), witness));

    // multisets
    double[][] b =
    [
        [ 1, 1, 1, 4, 7, 8 ],
        [ 1, 7 ],
        [ 1, 7, 7, 8],
        [ 4 ],
        [ 7 ],
    ];
    assert(equal(multiwayUnion(b), witness));

    double[][] c =
    [
        [9, 8, 8, 8, 7, 6],
        [9, 8, 6],
        [9, 8, 5]
    ];
    auto witness2 = [9, 8, 7, 6, 5];
    assert(equal(multiwayUnion!"a > b"(c), witness2));
}

/**
Lazily computes the difference of `r1` and `r2`. The two ranges
are assumed to be sorted by `less`. The element types of the two
ranges must have a common type.


In the case of multisets, considering that element `a` appears `x`
times in `r1` and `y` times and `r2`, the number of occurences
of `a` in the resulting range is going to be `x-y` if x > y or 0 otherwise.

Params:
    less = Predicate the given ranges are sorted by.
    r1 = The first range.
    r2 = The range to subtract from `r1`.

Returns:
    A range of the difference of `r1` and `r2`.

See_also: $(LREF setSymmetricDifference)
 */
struct SetDifference(alias less = "a < b", R1, R2)
if (isInputRange!(R1) && isInputRange!(R2))
{
private:
    R1 r1;
    R2 r2;
    alias comp = binaryFun!(less);

    void adjustPosition()
    {
        while (!r1.empty)
        {
            if (r2.empty || comp(r1.front, r2.front)) break;
            if (comp(r2.front, r1.front))
            {
                r2.popFront();
            }
            else
            {
                // both are equal
                r1.popFront();
                r2.popFront();
            }
        }
    }

public:
    ///
    this(R1 r1, R2 r2)
    {
        this.r1 = r1;
        this.r2 = r2;
        // position to the first element
        adjustPosition();
    }

    ///
    void popFront()
    {
        r1.popFront();
        adjustPosition();
    }

    ///
    @property auto ref front()
    {
        assert(!empty, "Can not get front of empty SetDifference");
        return r1.front;
    }

    static if (isForwardRange!R1 && isForwardRange!R2)
    {
        ///
        @property typeof(this) save()
        {
            auto ret = this;
            ret.r1 = r1.save;
            ret.r2 = r2.save;
            return ret;
        }
    }

    ///
    @property bool empty() { return r1.empty; }
}

/// Ditto
SetDifference!(less, R1, R2) setDifference(alias less = "a < b", R1, R2)
(R1 r1, R2 r2)
{
    return typeof(return)(r1, r2);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range.primitives : isForwardRange;

    //sets
    int[] a = [ 1, 2, 4, 5, 7, 9 ];
    int[] b = [ 0, 1, 2, 4, 7, 8 ];
    assert(equal(setDifference(a, b), [5, 9]));
    static assert(isForwardRange!(typeof(setDifference(a, b))));

    // multisets
    int[] x = [1, 1, 1, 2, 3];
    int[] y = [1, 1, 2, 4, 5];
    auto r = setDifference(x, y);
    assert(equal(r, [1, 3]));
    assert(setDifference(r, x).empty);
}

// https://issues.dlang.org/show_bug.cgi?id=10460
@safe unittest
{
    import std.algorithm.comparison : equal;

    int[] a = [1, 2, 3, 4, 5];
    int[] b = [2, 4];
    foreach (ref e; setDifference(a, b))
        e = 0;
    assert(equal(a, [0, 2, 0, 4, 0]));
}

/**
Lazily computes the intersection of two or more
$(REF_ALTTEXT input ranges, isInputRange, std,range,primitives)
`ranges`. The ranges are assumed to be sorted by `less`. The element
types of the ranges must have a common type.

In the case of multisets, the range with the minimum number of
occurences of a given element, propagates the number of
occurences of this element to the resulting range.

Params:
    less = Predicate the given ranges are sorted by.
    ranges = The ranges to compute the intersection for.

Returns:
    A range containing the intersection of the given ranges.
 */
struct SetIntersection(alias less = "a < b", Rs...)
if (Rs.length >= 2 && allSatisfy!(isInputRange, Rs) &&
    !is(CommonType!(staticMap!(ElementType, Rs)) == void))
{
private:
    Rs _input;
    alias comp = binaryFun!less;
    alias ElementType = CommonType!(staticMap!(.ElementType, Rs));

    // Positions to the first elements that are all equal
    void adjustPosition()
    {
        if (empty) return;

        size_t done = Rs.length;
        static if (Rs.length > 1) while (true)
        {
            foreach (i, ref r; _input)
            {
                alias next = _input[(i + 1) % Rs.length];

                if (comp(next.front, r.front))
                {
                    do
                    {
                        next.popFront();
                        if (next.empty) return;
                    } while (comp(next.front, r.front));
                    done = Rs.length;
                }
                if (--done == 0) return;
            }
        }
    }

public:
    ///
    this(Rs input)
    {
        this._input = input;
        // position to the first element
        adjustPosition();
    }

    ///
    @property bool empty()
    {
        foreach (ref r; _input)
        {
            if (r.empty) return true;
        }
        return false;
    }

    ///
    void popFront()
    {
        assert(!empty, "Can not popFront of empty SetIntersection");
        static if (Rs.length > 1) foreach (i, ref r; _input)
        {
            alias next = _input[(i + 1) % Rs.length];
            assert(!comp(r.front, next.front), "Set elements must not"
                    ~ " contradict the less predicate");
        }

        foreach (ref r; _input)
        {
            r.popFront();
        }
        adjustPosition();
    }

    ///
    @property ElementType front()
    {
        assert(!empty, "Can not get front of empty SetIntersection");
        return _input[0].front;
    }

    static if (allSatisfy!(isForwardRange, Rs))
    {
        ///
        @property SetIntersection save()
        {
            auto ret = this;
            foreach (i, ref r; _input)
            {
                ret._input[i] = r.save;
            }
            return ret;
        }
    }
}

/// Ditto
SetIntersection!(less, Rs) setIntersection(alias less = "a < b", Rs...)(Rs ranges)
if (Rs.length >= 2 && allSatisfy!(isInputRange, Rs) &&
    !is(CommonType!(staticMap!(ElementType, Rs)) == void))
{
    return typeof(return)(ranges);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;

    // sets
    int[] a = [ 1, 2, 4, 5, 7, 9 ];
    int[] b = [ 0, 1, 2, 4, 7, 8 ];
    int[] c = [ 0, 1, 4, 5, 7, 8 ];
    assert(equal(setIntersection(a, a), a));
    assert(equal(setIntersection(a, b), [1, 2, 4, 7]));
    assert(equal(setIntersection(a, b, c), [1, 4, 7]));

    // multisets
    int[] d = [ 1, 1, 2, 2, 7, 7 ];
    int[] e = [ 1, 1, 1, 7];
    assert(equal(setIntersection(a, d), [1, 2, 7]));
    assert(equal(setIntersection(d, e), [1, 1, 7]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter;

    int[] a = [ 1, 2, 4, 5, 7, 9 ];
    int[] b = [ 0, 1, 2, 4, 7, 8 ];
    int[] c = [ 0, 1, 4, 5, 7, 8 ];
    int[] d = [ 1, 3, 4 ];
    int[] e = [ 4, 5 ];

    assert(equal(setIntersection(a, a), a));
    assert(equal(setIntersection(a, a, a), a));
    assert(equal(setIntersection(a, b), [1, 2, 4, 7]));
    assert(equal(setIntersection(a, b, c), [1, 4, 7]));
    assert(equal(setIntersection(a, b, c, d), [1, 4]));
    assert(equal(setIntersection(a, b, c, d, e), [4]));

    auto inpA = a.filter!(_ => true), inpB = b.filter!(_ => true);
    auto inpC = c.filter!(_ => true), inpD = d.filter!(_ => true);
    assert(equal(setIntersection(inpA, inpB, inpC, inpD), [1, 4]));

    assert(equal(setIntersection(a, b, b, a), [1, 2, 4, 7]));
    assert(equal(setIntersection(a, c, b), [1, 4, 7]));
    assert(equal(setIntersection(b, a, c), [1, 4, 7]));
    assert(equal(setIntersection(b, c, a), [1, 4, 7]));
    assert(equal(setIntersection(c, a, b), [1, 4, 7]));
    assert(equal(setIntersection(c, b, a), [1, 4, 7]));
}

/**
Lazily computes the symmetric difference of `r1` and `r2`,
i.e. the elements that are present in exactly one of `r1` and $(D
r2). The two ranges are assumed to be sorted by `less`, and the
output is also sorted by `less`. The element types of the two
ranges must have a common type.

If both ranges are sets (without duplicated elements), the resulting
range is going to be a set. If at least one of the ranges is a multiset,
the number of occurences of an element `x` in the resulting range is `abs(a-b)`
where `a` is the number of occurences of `x` in `r1`, `b` is the number of
occurences of `x` in `r2`, and `abs` is the absolute value.

If both arguments are ranges of L-values of the same type then
`SetSymmetricDifference` will also be a range of L-values of
that type.

Params:
    less = Predicate the given ranges are sorted by.
    r1 = The first range.
    r2 = The second range.

Returns:
    A range of the symmetric difference between `r1` and `r2`.

See_also: $(LREF setDifference)
 */
struct SetSymmetricDifference(alias less = "a < b", R1, R2)
if (isInputRange!(R1) && isInputRange!(R2))
{
private:
    R1 r1;
    R2 r2;
    //bool usingR2;
    alias comp = binaryFun!(less);

    void adjustPosition()
    {
        while (!r1.empty && !r2.empty)
        {
            if (comp(r1.front, r2.front) || comp(r2.front, r1.front))
            {
                break;
            }
            // equal, pop both
            r1.popFront();
            r2.popFront();
        }
    }

public:
    ///
    this(R1 r1, R2 r2)
    {
        this.r1 = r1;
        this.r2 = r2;
        // position to the first element
        adjustPosition();
    }

    ///
    void popFront()
    {
        assert(!empty, "Can not popFront of empty SetSymmetricDifference");
        if (r1.empty) r2.popFront();
        else if (r2.empty) r1.popFront();
        else
        {
            // neither is empty
            if (comp(r1.front, r2.front))
            {
                r1.popFront();
            }
            else
            {
                assert(comp(r2.front, r1.front), "Elements of R1 and R2"
                        ~ " must be different");
                r2.popFront();
            }
        }
        adjustPosition();
    }

    ///
    @property auto ref front()
    {
        assert(!empty, "Can not get the front of an empty"
                ~ " SetSymmetricDifference");
        immutable chooseR1 = r2.empty || !r1.empty && comp(r1.front, r2.front);
        assert(chooseR1 || r1.empty || comp(r2.front, r1.front), "Failed to"
                ~ " get appropriate front");
        return chooseR1 ? r1.front : r2.front;
    }

    static if (isForwardRange!R1 && isForwardRange!R2)
    {
        ///
        @property typeof(this) save()
        {
            auto ret = this;
            ret.r1 = r1.save;
            ret.r2 = r2.save;
            return ret;
        }
    }

    ///
    ref auto opSlice() { return this; }

    ///
    @property bool empty() { return r1.empty && r2.empty; }
}

/// Ditto
SetSymmetricDifference!(less, R1, R2)
setSymmetricDifference(alias less = "a < b", R1, R2)
(R1 r1, R2 r2)
{
    return typeof(return)(r1, r2);
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range.primitives : isForwardRange;

    // sets
    int[] a = [ 1, 2, 4, 5, 7, 9 ];
    int[] b = [ 0, 1, 2, 4, 7, 8 ];
    assert(equal(setSymmetricDifference(a, b), [0, 5, 8, 9][]));
    static assert(isForwardRange!(typeof(setSymmetricDifference(a, b))));

    //mutisets
    int[] c = [1, 1, 1, 1, 2, 2, 2, 4, 5, 6];
    int[] d = [1, 1, 2, 2, 2, 2, 4, 7, 9];
    assert(equal(setSymmetricDifference(c, d), setSymmetricDifference(d, c)));
    assert(equal(setSymmetricDifference(c, d), [1, 1, 2, 5, 6, 7, 9]));
}

// https://issues.dlang.org/show_bug.cgi?id=10460
@safe unittest
{
    import std.algorithm.comparison : equal;

    int[] a = [1, 2];
    double[] b = [2.0, 3.0];
    int[] c = [2, 3];

    alias R1 = typeof(setSymmetricDifference(a, b));
    static assert(is(ElementType!R1 == double));
    static assert(!hasLvalueElements!R1);

    alias R2 = typeof(setSymmetricDifference(a, c));
    static assert(is(ElementType!R2 == int));
    static assert(hasLvalueElements!R2);

    assert(equal(setSymmetricDifference(a, b), [1.0, 3.0]));
    assert(equal(setSymmetricDifference(a, c), [1, 3]));
}

/++
TODO: once SetUnion got deprecated we can provide the usual definition
(= merge + filter after uniqs)
See: https://github.com/dlang/phobos/pull/4249
/**
Lazily computes the union of two or more ranges `rs`. The ranges
are assumed to be sorted by `less`. Elements in the output are
unique. The element types of all ranges must have a common type.

Params:
    less = Predicate the given ranges are sorted by.
    rs = The ranges to compute the union for.

Returns:
    A range containing the unique union of the given ranges.

See_Also:
   $(REF merge, std,algorithm,sorting)
 */
auto setUnion(alias less = "a < b", Rs...)
(Rs rs)
{
    import std.algorithm.iteration : uniq;
    import std.algorithm.sorting : merge;
    return merge!(less, Rs)(rs).uniq;
}

///
@safe pure nothrow unittest
    ///
{
    import std.algorithm.comparison : equal;

    int[] a = [1, 3, 5];
    int[] b = [2, 3, 4];
    assert(a.setUnion(b).equal([1, 2, 3, 4, 5]));
}

@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;

    int[] a = [ 1, 2, 4, 5, 7, 9 ];
    int[] b = [ 0, 1, 2, 4, 7, 8 ];
    double[] c = [ 10.5 ];

    assert(equal(setUnion(a, b), [0, 1, 2, 4, 5, 7, 8, 9][]));
    assert(equal(setUnion(a, c, b),
                    [0, 1, 2, 4, 5, 7, 8, 9, 10.5][]));
}

@safe unittest
{
    // save
    import std.range : dropOne;
    int[] a = [0, 1, 2];
    int[] b = [0, 3];
    auto arr = a.setUnion(b);
    assert(arr.front == 0);
    assert(arr.save.dropOne.front == 1);
    assert(arr.front == 0);
}

@nogc @safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;

    static immutable a = [1, 3, 5];
    static immutable b = [2, 4];
    static immutable r = [1, 2, 3, 4, 5];
    assert(a.setUnion(b).equal(r));
}

@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange;
    import std.range : iota;

    auto dummyResult1 = [1, 1.5, 2, 3, 4, 5, 5.5, 6, 7, 8, 9, 10];
    auto dummyResult2 = iota(1, 11);
    foreach (DummyType; AllDummyRanges)
    {
        DummyType d;
        assert(d.setUnion([1, 1.5, 5.5]).equal(dummyResult1));
        assert(d.setUnion(d).equal(dummyResult2));
    }
}
++/
