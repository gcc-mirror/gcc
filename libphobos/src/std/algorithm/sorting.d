// Written in the D programming language.
/**
This is a submodule of $(MREF std, algorithm).
It contains generic sorting algorithms.

$(SCRIPT inhibitQuickIndex = 1;)
$(BOOKTABLE Cheat Sheet,
$(TR $(TH Function Name) $(TH Description))
$(T2 completeSort,
        If `a = [10, 20, 30]` and `b = [40, 6, 15]`, then
        `completeSort(a, b)` leaves `a = [6, 10, 15]` and `b = [20, 30, 40]`.
        The range `a` must be sorted prior to the call, and as a result the
        combination `$(REF chain, std,range)(a, b)` is sorted.)
$(T2 isPartitioned,
        `isPartitioned!"a < 0"([-1, -2, 1, 0, 2])` returns `true` because
        the predicate is `true` for a portion of the range and `false`
        afterwards.)
$(T2 isSorted,
        `isSorted([1, 1, 2, 3])` returns `true`.)
$(T2 isStrictlyMonotonic,
        `isStrictlyMonotonic([1, 1, 2, 3])` returns `false`.)
$(T2 ordered,
        `ordered(1, 1, 2, 3)` returns `true`.)
$(T2 strictlyOrdered,
        `strictlyOrdered(1, 1, 2, 3)` returns `false`.)
$(T2 makeIndex,
        Creates a separate index for a range.)
$(T2 merge,
        Lazily merges two or more sorted ranges.)
$(T2 multiSort,
        Sorts by multiple keys.)
$(T2 nextEvenPermutation,
        Computes the next lexicographically greater even permutation of a range
        in-place.)
$(T2 nextPermutation,
        Computes the next lexicographically greater permutation of a range
        in-place.)
$(T2 nthPermutation,
        Computes the nth permutation of a range
        in-place.)
$(T2 partialSort,
        If `a = [5, 4, 3, 2, 1]`, then `partialSort(a, 3)` leaves
        `a[0 .. 3] = [1, 2, 3]`.
        The other elements of `a` are left in an unspecified order.)
$(T2 partition,
        Partitions a range according to a unary predicate.)
$(T2 partition3,
        Partitions a range according to a binary predicate in three parts (less
        than, equal, greater than the given pivot). Pivot is not given as an
        index, but instead as an element independent from the range's content.)
$(T2 pivotPartition,
        Partitions a range according to a binary predicate in two parts: less
        than or equal, and greater than or equal to the given pivot, passed as
        an index in the range.)
$(T2 schwartzSort,
        Sorts with the help of the $(LINK2 https://en.wikipedia.org/wiki/Schwartzian_transform, Schwartzian transform).)
$(T2 sort,
        Sorts.)
$(T2 topN,
        Separates the top elements in a range, akin to $(LINK2 https://en.wikipedia.org/wiki/Quickselect, Quickselect).)
$(T2 topNCopy,
        Copies out the top elements of a range.)
$(T2 topNIndex,
        Builds an index of the top elements of a range.)
)

Copyright: Andrei Alexandrescu 2008-.

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP erdani.com, Andrei Alexandrescu)

Source: $(PHOBOSSRC std/algorithm/sorting.d)

Macros:
T2=$(TR $(TDNW $(LREF $1)) $(TD $+))
 */
module std.algorithm.sorting;

import std.algorithm.mutation : SwapStrategy;
import std.functional : unaryFun, binaryFun;
import std.range.primitives;
import std.typecons : Flag, No, Yes;
import std.meta : allSatisfy;
import std.range : SortedRange;
import std.traits;

/**
Specifies whether the output of certain algorithm is desired in sorted
format.

If set to `SortOutput.no`, the output should not be sorted.

Otherwise if set to `SortOutput.yes`, the output should be sorted.
 */
alias SortOutput = Flag!"sortOutput";

// completeSort
/**
Sorts the random-access range `chain(lhs, rhs)` according to
predicate `less`.

The left-hand side of the range `lhs` is assumed to be already sorted;
`rhs` is assumed to be unsorted.
The exact strategy chosen depends on the relative sizes of `lhs` and
`rhs`.  Performs $(BIGOH lhs.length + rhs.length * log(rhs.length))
(best case) to $(BIGOH (lhs.length + rhs.length) * log(lhs.length +
rhs.length)) (worst-case) evaluations of $(REF_ALTTEXT swap, swap, std,algorithm,mutation).

Params:
    less = The predicate to sort by.
    ss = The swapping strategy to use.
    lhs = The sorted, left-hand side of the random access range to be sorted.
    rhs = The unsorted, right-hand side of the random access range to be
        sorted.
*/
void completeSort(alias less = "a < b", SwapStrategy ss = SwapStrategy.unstable,
        Lhs , Rhs)(SortedRange!(Lhs, less) lhs, Rhs rhs)
if (hasLength!(Rhs) && hasSlicing!(Rhs)
        && hasSwappableElements!Lhs && hasSwappableElements!Rhs)
{
    import std.algorithm.mutation : bringToFront;
    import std.range : chain, assumeSorted;
    // Probably this algorithm can be optimized by using in-place
    // merge
    auto lhsOriginal = lhs.release();
    foreach (i; 0 .. rhs.length)
    {
        auto sortedSoFar = chain(lhsOriginal, rhs[0 .. i]);
        auto ub = assumeSorted!less(sortedSoFar).upperBound(rhs[i]);
        if (!ub.length) continue;
        bringToFront(ub.release(), rhs[i .. i + 1]);
    }
}

///
@safe unittest
{
    import std.range : assumeSorted;
    int[] a = [ 1, 2, 3 ];
    int[] b = [ 4, 0, 6, 5 ];
    completeSort(assumeSorted(a), b);
    assert(a == [ 0, 1, 2 ]);
    assert(b == [ 3, 4, 5, 6 ]);
}

// isSorted
/**
Checks whether a $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
is sorted according to the comparison operation `less`. Performs $(BIGOH r.length)
evaluations of `less`.

Unlike `isSorted`, `isStrictlyMonotonic` does not allow for equal values,
i.e. values for which both `less(a, b)` and `less(b, a)` are false.

With either function, the predicate must be a strict ordering just like with
`isSorted`. For example, using `"a <= b"` instead of `"a < b"` is
incorrect and will cause failed assertions.

Params:
    less = Predicate the range should be sorted by.
    r = Forward range to check for sortedness.

Returns:
    `true` if the range is sorted, false otherwise. `isSorted` allows
    duplicates, `isStrictlyMonotonic` not.
*/
bool isSorted(alias less = "a < b", Range)(Range r)
if (isForwardRange!(Range))
{
    if (r.empty) return true;

    static if (isRandomAccessRange!Range && hasLength!Range)
    {
        immutable limit = r.length - 1;
        foreach (i; 0 .. limit)
        {
            if (!binaryFun!less(r[i + 1], r[i])) continue;
            assert(
                !binaryFun!less(r[i], r[i + 1]),
                "Predicate for isSorted is not antisymmetric. Both" ~
                        " pred(a, b) and pred(b, a) are true for certain values.");
            return false;
        }
    }
    else
    {
        auto ahead = r.save;
        ahead.popFront();
        size_t i;

        for (; !ahead.empty; ahead.popFront(), r.popFront(), ++i)
        {
            if (!binaryFun!less(ahead.front, r.front)) continue;
            // Check for antisymmetric predicate
            assert(
                !binaryFun!less(r.front, ahead.front),
                "Predicate for isSorted is not antisymmetric. Both" ~
                        " pred(a, b) and pred(b, a) are true for certain values.");
            return false;
        }
    }
    return true;
}

///
@safe unittest
{
    assert([1, 1, 2].isSorted);
    // strictly monotonic doesn't allow duplicates
    assert(![1, 1, 2].isStrictlyMonotonic);

    int[] arr = [4, 3, 2, 1];
    assert(!isSorted(arr));
    assert(!isStrictlyMonotonic(arr));

    assert(isSorted!"a > b"(arr));
    assert(isStrictlyMonotonic!"a > b"(arr));

    sort(arr);
    assert(isSorted(arr));
    assert(isStrictlyMonotonic(arr));
}

@safe unittest
{
    import std.conv : to;

    // https://issues.dlang.org/show_bug.cgi?id=9457
    auto x = "abcd";
    assert(isSorted(x));
    auto y = "acbd";
    assert(!isSorted(y));

    int[] a = [1, 2, 3];
    assert(isSorted(a));
    int[] b = [1, 3, 2];
    assert(!isSorted(b));

    // ignores duplicates
    int[] c = [1, 1, 2];
    assert(isSorted(c));

    dchar[] ds = "コーヒーが好きです"d.dup;
    sort(ds);
    string s = to!string(ds);
    assert(isSorted(ds));  // random-access
    assert(isSorted(s));   // bidirectional
}

@nogc @safe nothrow pure unittest
{
    static immutable a = [1, 2, 3];
    assert(a.isSorted);
}

/// ditto
bool isStrictlyMonotonic(alias less = "a < b", Range)(Range r)
if (isForwardRange!Range)
{
    import std.algorithm.searching : findAdjacent;
    return findAdjacent!((a,b) => !binaryFun!less(a,b))(r).empty;
}

@safe unittest
{
    import std.conv : to;

    assert("abcd".isStrictlyMonotonic);
    assert(!"aacd".isStrictlyMonotonic);
    assert(!"acb".isStrictlyMonotonic);

    assert([1, 2, 3].isStrictlyMonotonic);
    assert(![1, 3, 2].isStrictlyMonotonic);
    assert(![1, 1, 2].isStrictlyMonotonic);

    // ー occurs twice -> can't be strict
    dchar[] ds = "コーヒーが好きです"d.dup;
    sort(ds);
    string s = to!string(ds);
    assert(!isStrictlyMonotonic(ds));  // random-access
    assert(!isStrictlyMonotonic(s));   // bidirectional

    dchar[] ds2 = "コーヒが好きです"d.dup;
    sort(ds2);
    string s2 = to!string(ds2);
    assert(isStrictlyMonotonic(ds2));  // random-access
    assert(isStrictlyMonotonic(s2));   // bidirectional
}

@nogc @safe nothrow pure unittest
{
    static immutable a = [1, 2, 3];
    assert(a.isStrictlyMonotonic);
}

/**
Like `isSorted`, returns `true` if the given `values` are ordered
according to the comparison operation `less`. Unlike `isSorted`, takes values
directly instead of structured in a range.

`ordered` allows repeated values, e.g. `ordered(1, 1, 2)` is `true`. To verify
that the values are ordered strictly monotonically, use `strictlyOrdered`;
`strictlyOrdered(1, 1, 2)` is `false`.

With either function, the predicate must be a strict ordering. For example,
using `"a <= b"` instead of `"a < b"` is incorrect and will cause failed
assertions.

Params:
    values = The tested value
    less = The comparison predicate

Returns:
    `true` if the values are ordered; `ordered` allows for duplicates,
    `strictlyOrdered` does not.
*/

bool ordered(alias less = "a < b", T...)(T values)
if ((T.length == 2 && is(typeof(binaryFun!less(values[1], values[0])) : bool))
    ||
    (T.length > 2 && is(typeof(ordered!less(values[0 .. 1 + $ / 2])))
        && is(typeof(ordered!less(values[$ / 2 .. $]))))
    )
{
    foreach (i, _; T[0 .. $ - 1])
    {
        if (binaryFun!less(values[i + 1], values[i]))
        {
            assert(!binaryFun!less(values[i], values[i + 1]),
                __FUNCTION__ ~ ": incorrect non-strict predicate.");
            return false;
        }
    }
    return true;
}

/// ditto
bool strictlyOrdered(alias less = "a < b", T...)(T values)
if (is(typeof(ordered!less(values))))
{
    foreach (i, _; T[0 .. $ - 1])
    {
        if (!binaryFun!less(values[i], values[i + 1]))
        {
            return false;
        }
        assert(!binaryFun!less(values[i + 1], values[i]),
            __FUNCTION__ ~ ": incorrect non-strict predicate.");
    }
    return true;
}

///
@safe unittest
{
    assert(ordered(42, 42, 43));
    assert(!strictlyOrdered(43, 42, 45));
    assert(ordered(42, 42, 43));
    assert(!strictlyOrdered(42, 42, 43));
    assert(!ordered(43, 42, 45));
    // Ordered lexicographically
    assert(ordered("Jane", "Jim", "Joe"));
    assert(strictlyOrdered("Jane", "Jim", "Joe"));
    // Incidentally also ordered by length decreasing
    assert(ordered!((a, b) => a.length > b.length)("Jane", "Jim", "Joe"));
    // ... but not strictly so: "Jim" and "Joe" have the same length
    assert(!strictlyOrdered!((a, b) => a.length > b.length)("Jane", "Jim", "Joe"));
}

// partition
/**
Partitions a range in two using the given `predicate`.

Specifically, reorders the range `r = [left, right$(RPAREN)` using $(REF_ALTTEXT swap, swap, std,algorithm,mutation)
such that all elements `i` for which `predicate(i)` is `true` come
before all elements `j` for which `predicate(j)` returns `false`.

Performs $(BIGOH r.length) (if unstable or semistable) or $(BIGOH
r.length * log(r.length)) (if stable) evaluations of `less` and $(REF_ALTTEXT swap, swap, std,algorithm,mutation).
The unstable version computes the minimum possible evaluations of `swap`
(roughly half of those performed by the semistable version).

Params:
    predicate = The predicate to partition by.
    ss = The swapping strategy to employ.
    r = The random-access range to partition.

Returns:

The right part of `r` after partitioning.

If `ss == SwapStrategy.stable`, `partition` preserves the relative
ordering of all elements `a`, `b` in `r` for which
`predicate(a) == predicate(b)`.
If `ss == SwapStrategy.semistable`, `partition` preserves
the relative ordering of all elements `a`, `b` in the left part of `r`
for which `predicate(a) == predicate(b)`.
*/
Range partition(alias predicate, SwapStrategy ss, Range)(Range r)
if (ss == SwapStrategy.stable && isRandomAccessRange!(Range) && hasLength!Range &&
        hasSlicing!Range && hasSwappableElements!Range)
{
    import std.algorithm.mutation : bringToFront;

    alias pred = unaryFun!(predicate);
    if (r.empty) return r;

    if (r.length == 1)
    {
        if (pred(r.front)) r.popFront();
        return r;
    }
    const middle = r.length / 2;
    alias recurse = .partition!(pred, ss, Range);
    auto lower = recurse(r[0 .. middle]);
    auto upper = recurse(r[middle .. r.length]);
    bringToFront(lower, r[middle .. r.length - upper.length]);
    return r[r.length - lower.length - upper.length .. r.length];
}

///ditto
Range partition(alias predicate, SwapStrategy ss = SwapStrategy.unstable, Range)(Range r)
if (ss != SwapStrategy.stable && isInputRange!Range && hasSwappableElements!Range)
{
    import std.algorithm.mutation : swap;
    alias pred = unaryFun!(predicate);

    static if (ss == SwapStrategy.semistable)
    {
        if (r.empty) return r;

        for (; !r.empty; r.popFront())
        {
            // skip the initial portion of "correct" elements
            if (pred(r.front)) continue;
            // hit the first "bad" element
            auto result = r;
            for (r.popFront(); !r.empty; r.popFront())
            {
                if (!pred(r.front)) continue;
                swap(result.front, r.front);
                result.popFront();
            }
            return result;
        }

        return r;
    }
    else
    {
        // Inspired from www.stepanovpapers.com/PAM3-partition_notes.pdf,
        // section "Bidirectional Partition Algorithm (Hoare)"
        static if (isDynamicArray!Range)
        {
            import std.algorithm.mutation : swapAt;
            // For dynamic arrays prefer index-based manipulation
            if (!r.length) return r;
            size_t lo = 0, hi = r.length - 1;
            for (;;)
            {
                for (;;)
                {
                    if (lo > hi) return r[lo .. r.length];
                    if (!pred(r[lo])) break;
                    ++lo;
                }
                // found the left bound
                assert(lo <= hi, "lo must be <= hi");
                for (;;)
                {
                    if (lo == hi) return r[lo .. r.length];
                    if (pred(r[hi])) break;
                    --hi;
                }
                // found the right bound, swap & make progress
                r.swapAt(lo++, hi--);
            }
        }
        else
        {
            import std.algorithm.mutation : swap;
            auto result = r;
            for (;;)
            {
                for (;;)
                {
                    if (r.empty) return result;
                    if (!pred(r.front)) break;
                    r.popFront();
                    result.popFront();
                }
                // found the left bound
                assert(!r.empty, "r must not be empty");
                for (;;)
                {
                    if (pred(r.back)) break;
                    r.popBack();
                    if (r.empty) return result;
                }
                // found the right bound, swap & make progress
                static if (is(typeof(swap(r.front, r.back))))
                {
                    swap(r.front, r.back);
                }
                else
                {
                    auto t1 = r.moveFront(), t2 = r.moveBack();
                    r.front = t2;
                    r.back = t1;
                }
                r.popFront();
                result.popFront();
                r.popBack();
            }
        }
    }
}

///
@safe unittest
{
    import std.algorithm.mutation : SwapStrategy;
    import std.algorithm.searching : count, find;
    import std.conv : text;
    import std.range.primitives : empty;

    auto Arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    auto arr = Arr.dup;
    static bool even(int a) { return (a & 1) == 0; }
    // Partition arr such that even numbers come first
    auto r = partition!(even)(arr);
    // Now arr is separated in evens and odds.
    // Numbers may have become shuffled due to instability
    assert(r == arr[5 .. $]);
    assert(count!(even)(arr[0 .. 5]) == 5);
    assert(find!(even)(r).empty);

    // Can also specify the predicate as a string.
    // Use 'a' as the predicate argument name
    arr[] = Arr[];
    r = partition!(q{(a & 1) == 0})(arr);
    assert(r == arr[5 .. $]);

    // Now for a stable partition:
    arr[] = Arr[];
    r = partition!(q{(a & 1) == 0}, SwapStrategy.stable)(arr);
    // Now arr is [2 4 6 8 10 1 3 5 7 9], and r points to 1
    assert(arr == [2, 4, 6, 8, 10, 1, 3, 5, 7, 9] && r == arr[5 .. $]);

    // In case the predicate needs to hold its own state, use a delegate:
    arr[] = Arr[];
    int x = 3;
    // Put stuff greater than 3 on the left
    bool fun(int a) { return a > x; }
    r = partition!(fun, SwapStrategy.semistable)(arr);
    // Now arr is [4 5 6 7 8 9 10 2 3 1] and r points to 2
    assert(arr == [4, 5, 6, 7, 8, 9, 10, 2, 3, 1] && r == arr[7 .. $]);
}

@safe unittest
{
    import std.algorithm.internal : rndstuff;
    static bool even(int a) { return (a & 1) == 0; }

    // test with random data
    auto a = rndstuff!int();
    partition!even(a);
    assert(isPartitioned!even(a));
    auto b = rndstuff!string();
    partition!`a.length < 5`(b);
    assert(isPartitioned!`a.length < 5`(b));
}

// pivotPartition
/**
Partitions `r` around `pivot` using comparison function `less`, algorithm akin
to $(LINK2 https://en.wikipedia.org/wiki/Quicksort#Hoare_partition_scheme,
Hoare partition).

Specifically, permutes elements of `r` and returns
an index `k < r.length` such that:

$(UL

$(LI `r[pivot]` is swapped to `r[k]`)

$(LI All elements `e` in subrange `r[0 .. k]` satisfy `!less(r[k], e)`
(i.e. `r[k]` is greater than or equal to each element to its left according to
predicate `less`))

$(LI All elements `e` in subrange `r[k .. $]` satisfy `!less(e, r[k])`
(i.e. `r[k]` is less than or equal to each element to its right
according to predicate `less`)))

If `r` contains equivalent elements, multiple permutations of `r` satisfy these
constraints. In such cases, `pivotPartition` attempts to distribute equivalent
elements fairly to the left and right of `k` such that `k` stays close to  $(D
r.length / 2).

Params:
less = The predicate used for comparison, modeled as a
        $(LINK2 https://en.wikipedia.org/wiki/Weak_ordering#Strict_weak_orderings,
        strict weak ordering) (irreflexive, antisymmetric, transitive, and implying a transitive
        equivalence)
r = The range being partitioned
pivot = The index of the pivot for partitioning, must be less than `r.length` or
`0` if `r.length` is `0`

Returns:
The new position of the pivot

See_Also:
$(HTTP jgrcs.info/index.php/jgrcs/article/view/142, Engineering of a Quicksort
Partitioning Algorithm), D. Abhyankar, Journal of Global Research in Computer
Science, February 2011. $(HTTPS youtube.com/watch?v=AxnotgLql0k, ACCU 2016
Keynote), Andrei Alexandrescu.
*/
size_t pivotPartition(alias less = "a < b", Range)
(Range r, size_t pivot)
if (isRandomAccessRange!Range && hasLength!Range && hasSlicing!Range && hasAssignableElements!Range)
{
    assert(pivot < r.length || r.length == 0 && pivot == 0, "pivot must be"
        ~ " less than the length of r or r must be empty and pivot zero");
    if (r.length <= 1) return 0;
    import std.algorithm.mutation : swapAt, move;
    alias lt = binaryFun!less;

    // Pivot at the front
    r.swapAt(pivot, 0);

    // Fork implementation depending on nothrow copy, assignment, and
    // comparison. If all of these are nothrow, use the specialized
    // implementation discussed at https://youtube.com/watch?v=AxnotgLql0k.
    static if (is(typeof(
            () nothrow { auto x = r.front; x = r.front; return lt(x, x); }
        )))
    {
        auto p = r[0];
        // Plant the pivot in the end as well as a sentinel
        size_t lo = 0, hi = r.length - 1;
        auto save = r.moveAt(hi);
        r[hi] = p; // Vacancy is in r[$ - 1] now
        // Start process
        for (;;)
        {
            // Loop invariant
            version (StdUnittest)
            {
                // this used to import std.algorithm.all, but we want to save
                // imports when unittests are enabled if possible.
                foreach (x; r[0 .. lo])
                    assert(!lt(p, x), "p must not be less than x");
                foreach (x; r[hi+1 .. r.length])
                    assert(!lt(x, p), "x must not be less than p");
            }
            do ++lo; while (lt(r[lo], p));
            r[hi] = r[lo];
            // Vacancy is now in r[lo]
            do --hi; while (lt(p, r[hi]));
            if (lo >= hi) break;
            r[lo] = r[hi];
            // Vacancy is not in r[hi]
        }
        // Fixup
        assert(lo - hi <= 2, "Following compare not possible");
        assert(!lt(p, r[hi]), "r[hi] must not be less than p");
        if (lo == hi + 2)
        {
            assert(!lt(r[hi + 1], p), "r[hi + 1] must not be less than p");
            r[lo] = r[hi + 1];
            --lo;
        }
        r[lo] = save;
        if (lt(p, save)) --lo;
        assert(!lt(p, r[lo]), "r[lo] must not be less than p");
    }
    else
    {
        size_t lo = 1, hi = r.length - 1;
        loop: for (;; lo++, hi--)
        {
            for (;; ++lo)
            {
                if (lo > hi) break loop;
                if (!lt(r[lo], r[0])) break;
            }
            // found the left bound:  r[lo] >= r[0]
            assert(lo <= hi, "lo must be less or equal than hi");
            for (;; --hi)
            {
                if (lo >= hi) break loop;
                if (!lt(r[0], r[hi])) break;
            }
            // found the right bound: r[hi] <= r[0], swap & make progress
            assert(!lt(r[lo], r[hi]), "r[lo] must not be less than r[hi]");
            r.swapAt(lo, hi);
        }
        --lo;
    }
    r.swapAt(lo, 0);
    return lo;
}

///
@safe nothrow unittest
{
    int[] a = [5, 3, 2, 6, 4, 1, 3, 7];
    size_t pivot = pivotPartition(a, a.length / 2);
    import std.algorithm.searching : all;
    assert(a[0 .. pivot].all!(x => x <= a[pivot]));
    assert(a[pivot .. $].all!(x => x >= a[pivot]));
}

@safe unittest
{
    void test(alias less)()
    {
        int[] a;
        size_t pivot;

        a = [-9, -4, -2, -2, 9];
        pivot = pivotPartition!less(a, a.length / 2);
        import std.algorithm.searching : all;
        assert(a[0 .. pivot].all!(x => x <= a[pivot]));
        assert(a[pivot .. $].all!(x => x >= a[pivot]));

        a = [9, 2, 8, -5, 5, 4, -8, -4, 9];
        pivot = pivotPartition!less(a, a.length / 2);
        assert(a[0 .. pivot].all!(x => x <= a[pivot]));
        assert(a[pivot .. $].all!(x => x >= a[pivot]));

        a = [ 42 ];
        pivot = pivotPartition!less(a, a.length / 2);
        assert(pivot == 0);
        assert(a == [ 42 ]);

        a = [ 43, 42 ];
        pivot = pivotPartition!less(a, 0);
        assert(pivot == 1);
        assert(a == [ 42, 43 ]);

        a = [ 43, 42 ];
        pivot = pivotPartition!less(a, 1);
        assert(pivot == 0);
        assert(a == [ 42, 43 ]);

        a = [ 42, 42 ];
        pivot = pivotPartition!less(a, 0);
        assert(pivot == 0 || pivot == 1);
        assert(a == [ 42, 42 ]);
        pivot = pivotPartition!less(a, 1);
        assert(pivot == 0 || pivot == 1);
        assert(a == [ 42, 42 ]);

        import std.algorithm.iteration : map;
        import std.array : array;
        import std.format : format;
        import std.random : Random, uniform, Xorshift;
        import std.range : iota;
        auto s = 123_456_789;
        auto g = Xorshift(s);
        a = iota(0, uniform(1, 1000, g))
            .map!(_ => uniform(-1000, 1000, g))
            .array;
        pivot = pivotPartition!less(a, a.length / 2);
        assert(a[0 .. pivot].all!(x => x <= a[pivot]), "RNG seed: %d".format(s));
        assert(a[pivot .. $].all!(x => x >= a[pivot]), "RNG seed: %d".format(s));
    }
    test!"a < b";
    static bool myLess(int a, int b)
    {
        static bool bogus;
        if (bogus) throw new Exception(""); // just to make it no-nothrow
        return a < b;
    }
    test!myLess;
}

/**
Params:
    pred = The predicate that the range should be partitioned by.
    r = The range to check.
Returns: `true` if `r` is partitioned according to predicate `pred`.
 */
bool isPartitioned(alias pred, Range)(Range r)
if (isForwardRange!(Range))
{
    for (; !r.empty; r.popFront())
    {
        if (unaryFun!(pred)(r.front)) continue;
        for (r.popFront(); !r.empty; r.popFront())
        {
            if (unaryFun!(pred)(r.front)) return false;
        }
        break;
    }
    return true;
}

///
@safe unittest
{
    int[] r = [ 1, 3, 5, 7, 8, 2, 4, ];
    assert(isPartitioned!"a & 1"(r));
}

// partition3
/**
Rearranges elements in `r` in three adjacent ranges and returns
them.

The first and leftmost range only contains elements in `r`
less than `pivot`. The second and middle range only contains
elements in `r` that are equal to `pivot`. Finally, the third
and rightmost range only contains elements in `r` that are greater
than `pivot`. The less-than test is defined by the binary function
`less`.

Params:
    less = The predicate to use for the rearrangement.
    ss = The swapping strategy to use.
    r = The random-access range to rearrange.
    pivot = The pivot element.

Returns:
    A $(REF Tuple, std,typecons) of the three resulting ranges. These ranges are
    slices of the original range.

BUGS: stable `partition3` has not been implemented yet.
 */
auto partition3(alias less = "a < b", SwapStrategy ss = SwapStrategy.unstable, Range, E)
(Range r, E pivot)
if (ss == SwapStrategy.unstable && isRandomAccessRange!Range
        && hasSwappableElements!Range && hasLength!Range && hasSlicing!Range
        && is(typeof(binaryFun!less(r.front, pivot)) == bool)
        && is(typeof(binaryFun!less(pivot, r.front)) == bool)
        && is(typeof(binaryFun!less(r.front, r.front)) == bool))
{
    // The algorithm is described in "Engineering a sort function" by
    // Jon Bentley et al, pp 1257.

    import std.algorithm.comparison : min;
    import std.algorithm.mutation : swap, swapAt, swapRanges;
    import std.typecons : tuple;

    alias lessFun = binaryFun!less;
    size_t i, j, k = r.length, l = k;

 bigloop:
    for (;;)
    {
        for (;; ++j)
        {
            if (j == k) break bigloop;
            assert(j < r.length, "j must be less than r.length");
            if (lessFun(r[j], pivot)) continue;
            if (lessFun(pivot, r[j])) break;
            r.swapAt(i++, j);
        }
        assert(j < k, "j must be less than k");
        for (;;)
        {
            assert(k > 0, "k must be positive");
            if (!lessFun(pivot, r[--k]))
            {
                if (lessFun(r[k], pivot)) break;
                r.swapAt(k, --l);
            }
            if (j == k) break bigloop;
        }
        // Here we know r[j] > pivot && r[k] < pivot
        r.swapAt(j++, k);
    }

    // Swap the equal ranges from the extremes into the middle
    auto strictlyLess = j - i, strictlyGreater = l - k;
    auto swapLen = min(i, strictlyLess);
    swapRanges(r[0 .. swapLen], r[j - swapLen .. j]);
    swapLen = min(r.length - l, strictlyGreater);
    swapRanges(r[k .. k + swapLen], r[r.length - swapLen .. r.length]);
    return tuple(r[0 .. strictlyLess],
            r[strictlyLess .. r.length - strictlyGreater],
            r[r.length - strictlyGreater .. r.length]);
}

///
@safe unittest
{
    auto a = [ 8, 3, 4, 1, 4, 7, 4 ];
    auto pieces = partition3(a, 4);
    assert(pieces[0] == [ 1, 3 ]);
    assert(pieces[1] == [ 4, 4, 4 ]);
    assert(pieces[2] == [ 8, 7 ]);
}

@safe unittest
{
    import std.random : Random = Xorshift, uniform;

    immutable uint[] seeds = [3923355730, 1927035882];
    foreach (s; seeds)
    {
        auto r = Random(s);
        auto a = new int[](uniform(0, 100, r));
        foreach (ref e; a)
        {
            e = uniform(0, 50, r);
        }
        auto pieces = partition3(a, 25);
        assert(pieces[0].length + pieces[1].length + pieces[2].length == a.length);
        foreach (e; pieces[0])
        {
            assert(e < 25);
        }
        foreach (e; pieces[1])
        {
            assert(e == 25);
        }
        foreach (e; pieces[2])
        {
            assert(e > 25);
        }
    }
}

// makeIndex
/**
Computes an index for `r` based on the comparison `less`.

The index is a sorted array of pointers or indices into the original
range. This technique is similar to sorting, but it is more flexible
because (1) it allows "sorting" of immutable collections, (2) allows
binary search even if the original collection does not offer random
access, (3) allows multiple indexes, each on a different predicate,
and (4) may be faster when dealing with large objects. However, using
an index may also be slower under certain circumstances due to the
extra indirection, and is always larger than a sorting-based solution
because it needs space for the index in addition to the original
collection. The complexity is the same as `sort`'s.

The first overload of `makeIndex` writes to a range containing
pointers, and the second writes to a range containing offsets. The
first overload requires `Range` to be a
$(REF_ALTTEXT forward range, isForwardRange, std,range,primitives), and the
latter requires it to be a random-access range.

`makeIndex` overwrites its second argument with the result, but
never reallocates it.

Params:
    less = The comparison to use.
    ss = The swapping strategy.
    r = The range to index.
    index = The resulting index.

Returns: The pointer-based version returns a `SortedRange` wrapper
over index, of type
`SortedRange!(RangeIndex, (a, b) => binaryFun!less(*a, *b))`
thus reflecting the ordering of the
index. The index-based version returns `void` because the ordering
relation involves not only `index` but also `r`.

Throws: If the second argument's length is less than that of the range
indexed, an exception is thrown.
*/
SortedRange!(RangeIndex, (a, b) => binaryFun!less(*a, *b))
makeIndex(
    alias less = "a < b",
    SwapStrategy ss = SwapStrategy.unstable,
    Range,
    RangeIndex)
(Range r, RangeIndex index)
if (isForwardRange!(Range) && isRandomAccessRange!(RangeIndex)
    && is(ElementType!(RangeIndex) : ElementType!(Range)*) && hasAssignableElements!RangeIndex)
{
    import std.algorithm.internal : addressOf;
    import std.exception : enforce;

    // assume collection already ordered
    size_t i;
    for (; !r.empty; r.popFront(), ++i)
        index[i] = addressOf(r.front);
    enforce(index.length == i);
    // sort the index
    sort!((a, b) => binaryFun!less(*a, *b), ss)(index);
    return typeof(return)(index);
}

/// Ditto
void makeIndex(
    alias less = "a < b",
    SwapStrategy ss = SwapStrategy.unstable,
    Range,
    RangeIndex)
(Range r, RangeIndex index)
if (isRandomAccessRange!Range && !isInfinite!Range &&
    isRandomAccessRange!RangeIndex && !isInfinite!RangeIndex &&
    isIntegral!(ElementType!RangeIndex) && hasAssignableElements!RangeIndex)
{
    import std.conv : to;
    import std.exception : enforce;

    alias IndexType = Unqual!(ElementType!RangeIndex);
    enforce(r.length == index.length,
        "r and index must be same length for makeIndex.");
    static if (IndexType.sizeof < size_t.sizeof)
    {
        enforce(r.length <= size_t(1) + IndexType.max, "Cannot create an index with " ~
            "element type " ~ IndexType.stringof ~ " with length " ~
            to!string(r.length) ~ ".");
    }

    // Use size_t as loop index to avoid overflow on ++i,
    // e.g. when squeezing 256 elements into a ubyte index.
    foreach (size_t i; 0 .. r.length)
        index[i] = cast(IndexType) i;

    // sort the index
    sort!((a, b) => binaryFun!less(r[cast(size_t) a], r[cast(size_t) b]), ss)
      (index);
}

///
@system unittest
{
    immutable(int[]) arr = [ 2, 3, 1, 5, 0 ];
    // index using pointers
    auto index1 = new immutable(int)*[arr.length];
    makeIndex!("a < b")(arr, index1);
    assert(isSorted!("*a < *b")(index1));
    // index using offsets
    auto index2 = new size_t[arr.length];
    makeIndex!("a < b")(arr, index2);
    assert(isSorted!
        ((size_t a, size_t b){ return arr[a] < arr[b];})
        (index2));
}

@system unittest
{
    immutable(int)[] arr = [ 2, 3, 1, 5, 0 ];
    // index using pointers
    auto index1 = new immutable(int)*[arr.length];
    alias ImmRange = typeof(arr);
    alias ImmIndex = typeof(index1);
    static assert(isForwardRange!(ImmRange));
    static assert(isRandomAccessRange!(ImmIndex));
    static assert(!isIntegral!(ElementType!(ImmIndex)));
    static assert(is(ElementType!(ImmIndex) : ElementType!(ImmRange)*));
    makeIndex!("a < b")(arr, index1);
    assert(isSorted!("*a < *b")(index1));

    // index using offsets
    auto index2 = new long[arr.length];
    makeIndex(arr, index2);
    assert(isSorted!
            ((long a, long b){
                return arr[cast(size_t) a] < arr[cast(size_t) b];
            })(index2));

    // index strings using offsets
    string[] arr1 = ["I", "have", "no", "chocolate"];
    auto index3 = new byte[arr1.length];
    makeIndex(arr1, index3);
    assert(isSorted!
            ((byte a, byte b){ return arr1[a] < arr1[b];})
            (index3));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : iota;

    ubyte[256] index = void;
    iota(256).makeIndex(index[]);
    assert(index[].equal(iota(256)));
    byte[128] sindex = void;
    iota(128).makeIndex(sindex[]);
    assert(sindex[].equal(iota(128)));

    auto index2 = new uint[10];
    10.iota.makeIndex(index2);
    assert(index2.equal(10.iota));
}

struct Merge(alias less = "a < b", Rs...)
if (Rs.length >= 2 &&
    allSatisfy!(isInputRange, Rs) &&
    !is(CommonType!(staticMap!(ElementType, Rs)) == void))
{
    public Rs source;
    private size_t _lastFrontIndex = size_t.max;
    static if (isBidirectional)
    {
        private size_t _lastBackIndex = size_t.max; // `size_t.max` means uninitialized,
    }

    import std.functional : binaryFun;
    import std.meta : anySatisfy;
    import std.traits : isCopyable;

    private alias comp = binaryFun!less;
    private alias ElementType = CommonType!(staticMap!(.ElementType, Rs));
    private enum isBidirectional = allSatisfy!(isBidirectionalRange, staticMap!(Unqual, Rs));

    debug private enum canCheckSortedness = isCopyable!ElementType && !hasAliasing!ElementType;

    this(Rs source)
    {
        this.source = source;
        this._lastFrontIndex = frontIndex;
    }

    static if (anySatisfy!(isInfinite, Rs))
    {
        enum bool empty = false; // propagate infiniteness
    }
    else
    {
        @property bool empty()
        {
            return _lastFrontIndex == size_t.max;
        }
    }

    @property auto ref front()
    {
        final switch (_lastFrontIndex)
        {
            foreach (i, _; Rs)
            {
            case i:
                assert(!source[i].empty, "Can not get front of empty Merge");
                return source[i].front;
            }
        }
    }

    private size_t frontIndex()
    {
        size_t bestIndex = size_t.max; // indicate undefined
        Unqual!ElementType bestElement;
        foreach (i, _; Rs)
        {
            if (source[i].empty) continue;
            if (bestIndex == size_t.max || // either this is the first or
                comp(source[i].front, bestElement))
            {
                bestIndex = i;
                bestElement = source[i].front;
            }
        }
        return bestIndex;
    }

    void popFront()
    {
        sw: final switch (_lastFrontIndex)
        {
            foreach (i, R; Rs)
            {
            case i:
                debug static if (canCheckSortedness)
                {
                    ElementType previousFront = source[i].front();
                }
                source[i].popFront();
                debug static if (canCheckSortedness)
                {
                    if (!source[i].empty)
                    {
                        assert(!comp(source[i].front, previousFront),
                               "Input " ~ i.stringof ~ " is unsorted"); // @nogc
                    }
                }
                break sw;
            }
        }
        _lastFrontIndex = frontIndex;
    }

    static if (isBidirectional)
    {
        @property auto ref back()
        {
            if (_lastBackIndex == size_t.max)
            {
                this._lastBackIndex = backIndex; // lazy initialization
            }
            final switch (_lastBackIndex)
            {
                foreach (i, _; Rs)
                {
                case i:
                    assert(!source[i].empty, "Can not get back of empty Merge");
                    return source[i].back;
                }
            }
        }

        private size_t backIndex()
        {
            size_t bestIndex = size_t.max; // indicate undefined
            Unqual!ElementType bestElement;
            foreach (i, _; Rs)
            {
                if (source[i].empty) continue;
                if (bestIndex == size_t.max || // either this is the first or
                    comp(bestElement, source[i].back))
                {
                    bestIndex = i;
                    bestElement = source[i].back;
                }
            }
            return bestIndex;
        }

        void popBack()
        {
            if (_lastBackIndex == size_t.max)
            {
                this._lastBackIndex = backIndex; // lazy initialization
            }
            sw: final switch (_lastBackIndex)
            {
                foreach (i, R; Rs)
                {
                case i:
                    debug static if (canCheckSortedness)
                    {
                        ElementType previousBack = source[i].back();
                    }
                    source[i].popBack();
                    debug static if (canCheckSortedness)
                    {
                        if (!source[i].empty)
                        {
                            assert(!comp(previousBack, source[i].back),
                                   "Input " ~ i.stringof ~ " is unsorted"); // @nogc
                        }
                    }
                    break sw;
                }
            }
            _lastBackIndex = backIndex;
            if (_lastBackIndex == size_t.max) // if emptied
            {
                _lastFrontIndex = size_t.max;
            }
        }
    }

    static if (allSatisfy!(isForwardRange, staticMap!(Unqual, Rs)))
    {
        @property auto save()
        {
            auto result = this;
            foreach (i, _; Rs)
            {
                result.source[i] = result.source[i].save;
            }
            return result;
        }
    }

    static if (allSatisfy!(hasLength, Rs))
    {
        @property size_t length()
        {
            size_t result;
            foreach (i, _; Rs)
            {
                result += source[i].length;
            }
            return result;
        }

        alias opDollar = length;
    }
}

/**
   Merge multiple sorted ranges `rs` with less-than predicate function `pred`
   into one single sorted output range containing the sorted union of the
   elements of inputs.

   Duplicates are not eliminated, meaning that the total
   number of elements in the output is the sum of all elements in the ranges
   passed to it; the `length` member is offered if all inputs also have
   `length`. The element types of all the inputs must have a common type
   `CommonType`.

Params:
    less = Predicate the given ranges are sorted by.
    rs = The ranges to compute the union for.

Returns:
    A range containing the union of the given ranges.

Details:

All of its inputs are assumed to be sorted. This can mean that inputs are
   instances of $(REF SortedRange, std,range). Use the result of $(REF sort,
   std,algorithm,sorting), or $(REF assumeSorted, std,range) to merge ranges
   known to be sorted (show in the example below). Note that there is currently
   no way of ensuring that two or more instances of $(REF SortedRange,
   std,range) are sorted using a specific comparison function `pred`. Therefore
   no checking is done here to assure that all inputs `rs` are instances of
   $(REF SortedRange, std,range).

   This algorithm is lazy, doing work progressively as elements are pulled off
   the result.

   Time complexity is proportional to the sum of element counts over all inputs.

   If all inputs have the same element type and offer it by `ref`, output
   becomes a range with mutable `front` (and `back` where appropriate) that
   reflects in the original inputs.

   If any of the inputs `rs` is infinite so is the result (`empty` being always
   `false`).

See_Also: $(REF multiwayMerge, std,algorithm,setops) for an analogous function
   that merges a dynamic number of ranges.
*/
Merge!(less, Rs) merge(alias less = "a < b", Rs...)(Rs rs)
if (Rs.length >= 2 &&
    allSatisfy!(isInputRange, Rs) &&
    !is(CommonType!(staticMap!(ElementType, Rs)) == void))
{
    return typeof(return)(rs);
}

///
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.range : retro;

    int[] a = [1, 3, 5];
    int[] b = [2, 3, 4];

    assert(a.merge(b).equal([1, 2, 3, 3, 4, 5]));
    assert(a.merge(b).retro.equal([5, 4, 3, 3, 2, 1]));
}

@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;

    int[] a = [ 1, 2, 4, 5, 7, 9 ];
    int[] b = [ 0, 1, 2, 4, 7, 8 ];
    double[] c = [ 10.5 ];

    assert(merge(a, b).length == a.length + b.length);
    assert(equal(merge(a, b), [0, 1, 1, 2, 2, 4, 4, 5, 7, 7, 8, 9][]));
    assert(equal(merge(a, c, b),
                    [0, 1, 1, 2, 2, 4, 4, 5, 7, 7, 8, 9, 10.5][]));
    auto u = merge(a, b);
    u.front--;
    assert(equal(u, [-1, 1, 1, 2, 2, 4, 4, 5, 7, 7, 8, 9][]));
}

@safe pure nothrow unittest
{
    // save
    import std.range : dropOne;
    int[] a = [1, 2];
    int[] b = [0, 3];
    auto arr = a.merge(b);
    assert(arr.front == 0);
    assert(arr.save.dropOne.front == 1);
    assert(arr.front == 0);
}

@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange;

    auto dummyResult1 = [1, 1, 1.5, 2, 3, 4, 5, 5.5, 6, 7, 8, 9, 10];
    auto dummyResult2 = [1, 1, 2, 2, 3, 3, 4, 4, 5, 5,
                         6, 6, 7, 7, 8, 8, 9, 9, 10, 10];
    foreach (DummyType; AllDummyRanges)
    {
        DummyType d;
        assert(d.merge([1, 1.5, 5.5]).equal(dummyResult1));
        assert(d.merge(d).equal(dummyResult2));
    }
}

@nogc @safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;

    static immutable a = [1, 3, 5];
    static immutable b = [2, 3, 4];
    static immutable r = [1, 2, 3, 3, 4, 5];
    assert(a.merge(b).equal(r));
}

/// test bi-directional access and common type
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.range : retro;
    import std.traits : CommonType;

    alias S = short;
    alias I = int;
    alias D = double;

    S[] a = [1, 2, 3];
    I[] b = [50, 60];
    D[] c = [10, 20, 30, 40];

    auto m = merge(a, b, c);

    static assert(is(typeof(m.front) == CommonType!(S, I, D)));

    assert(equal(m, [1, 2, 3, 10, 20, 30, 40, 50, 60]));
    assert(equal(m.retro, [60, 50, 40, 30, 20, 10, 3, 2, 1]));

    m.popFront();
    assert(equal(m, [2, 3, 10, 20, 30, 40, 50, 60]));
    m.popBack();
    assert(equal(m, [2, 3, 10, 20, 30, 40, 50]));
    m.popFront();
    assert(equal(m, [3, 10, 20, 30, 40, 50]));
    m.popBack();
    assert(equal(m, [3, 10, 20, 30, 40]));
    m.popFront();
    assert(equal(m, [10, 20, 30, 40]));
    m.popBack();
    assert(equal(m, [10, 20, 30]));
    m.popFront();
    assert(equal(m, [20, 30]));
    m.popBack();
    assert(equal(m, [20]));
    m.popFront();
    assert(m.empty);
}

// Issue 21810: Check for sortedness must not use `==`
@nogc @safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;
    import std.typecons : tuple;

    static immutable a = [
        tuple(1, 1),
        tuple(3, 1),
        tuple(3, 2),
        tuple(5, 1),
    ];
    static immutable b = [
        tuple(2, 1),
        tuple(3, 1),
        tuple(4, 1),
        tuple(4, 2),
    ];
    static immutable r = [
        tuple(1, 1),
        tuple(2, 1),
        tuple(3, 1),
        tuple(3, 2),
        tuple(3, 1),
        tuple(4, 1),
        tuple(4, 2),
        tuple(5, 1),
    ];
    assert(merge!"a[0] < b[0]"(a, b).equal(r));
}

private template validPredicates(E, less...)
{
    static if (less.length == 0)
        enum validPredicates = true;
    else static if (less.length == 1 && is(typeof(less[0]) == SwapStrategy))
        enum validPredicates = true;
    else
        enum validPredicates =
            is(typeof((E a, E b){ bool r = binaryFun!(less[0])(a, b); }))
            && validPredicates!(E, less[1 .. $]);
}

/**
Sorts a range by multiple keys.

The call $(D multiSort!("a.id < b.id",
"a.date > b.date")(r)) sorts the range `r` by `id` ascending,
and sorts elements that have the same `id` by `date`
descending. Such a call is equivalent to $(D sort!"a.id != b.id ? a.id
< b.id : a.date > b.date"(r)), but `multiSort` is faster because it
does fewer comparisons (in addition to being more convenient).

Returns:
    The initial range wrapped as a `SortedRange` with its predicates
    converted to an equivalent single predicate.
 */
template multiSort(less...) //if (less.length > 1)
{
    auto multiSort(Range)(Range r)
    if (validPredicates!(ElementType!Range, less) && hasSwappableElements!Range)
    {
        import std.meta : AliasSeq;
        import std.range : assumeSorted;
        static if (is(typeof(less[$ - 1]) == SwapStrategy))
        {
            enum ss = less[$ - 1];
            alias funs = less[0 .. $ - 1];
        }
        else
        {
            enum ss = SwapStrategy.unstable;
            alias funs = less;
        }

        static if (funs.length == 0)
            static assert(false, "No sorting predicate provided for multiSort");
        else
        static if (funs.length == 1)
            return sort!(funs[0], ss, Range)(r);
        else
        {
            multiSortImpl!(Range, ss, funs)(r);
            return assumeSorted!(multiSortPredFun!(Range, funs))(r);
        }
    }
}

///
@safe unittest
{
    import std.algorithm.mutation : SwapStrategy;
    static struct Point { int x, y; }
    auto pts1 = [ Point(0, 0), Point(5, 5), Point(0, 1), Point(0, 2) ];
    auto pts2 = [ Point(0, 0), Point(0, 1), Point(0, 2), Point(5, 5) ];
    multiSort!("a.x < b.x", "a.y < b.y", SwapStrategy.unstable)(pts1);
    assert(pts1 == pts2);
}

private bool multiSortPredFun(Range, funs...)(ElementType!Range a, ElementType!Range b)
{
    foreach (f; funs)
    {
        alias lessFun = binaryFun!(f);
        if (lessFun(a, b)) return true;
        if (lessFun(b, a)) return false;
    }
    return false;
}

private void multiSortImpl(Range, SwapStrategy ss, funs...)(Range r)
{
    alias lessFun = binaryFun!(funs[0]);

    static if (funs.length > 1)
    {
        while (r.length > 1)
        {
            auto p = getPivot!lessFun(r);
            auto t = partition3!(funs[0], ss)(r, r[p]);
            if (t[0].length <= t[2].length)
            {
                multiSortImpl!(Range, ss, funs)(t[0]);
                multiSortImpl!(Range, ss, funs[1 .. $])(t[1]);
                r = t[2];
            }
            else
            {
                multiSortImpl!(Range, ss, funs[1 .. $])(t[1]);
                multiSortImpl!(Range, ss, funs)(t[2]);
                r = t[0];
            }
        }
    }
    else
    {
        sort!(lessFun, ss)(r);
    }
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range;

    static struct Point { int x, y; }
    auto pts1 = [ Point(5, 6), Point(1, 0), Point(5, 7), Point(1, 1), Point(1, 2), Point(0, 1) ];
    auto pts2 = [ Point(0, 1), Point(1, 0), Point(1, 1), Point(1, 2), Point(5, 6), Point(5, 7) ];
    static assert(validPredicates!(Point, "a.x < b.x", "a.y < b.y"));
    multiSort!("a.x < b.x", "a.y < b.y", SwapStrategy.unstable)(pts1);
    assert(pts1 == pts2);

    auto pts3 = indexed(pts1, iota(pts1.length));
    assert(pts3.multiSort!("a.x < b.x", "a.y < b.y", SwapStrategy.unstable).release.equal(pts2));

    auto pts4 = iota(10).array;
    assert(pts4.multiSort!("a > b").release.equal(iota(10).retro));
}

//https://issues.dlang.org/show_bug.cgi?id=9160 (L-value only comparators)
@safe unittest
{
    static struct A
    {
        int x;
        int y;
    }

    static bool byX(const ref A lhs, const ref A rhs)
    {
        return lhs.x < rhs.x;
    }

    static bool byY(const ref A lhs, const ref A rhs)
    {
        return lhs.y < rhs.y;
    }

    auto points = [ A(4, 1), A(2, 4)];
    multiSort!(byX, byY)(points);
    assert(points[0] == A(2, 4));
    assert(points[1] == A(4, 1));
}

// cannot access frame of function
// https://issues.dlang.org/show_bug.cgi?id=16179
@safe unittest
{
    auto arr = [[1, 2], [2, 0], [1, 0], [1, 1]];
    int c = 3;

    arr.multiSort!(
        (a, b) => a[0] < b[0],
        (a, b) => c*a[1] < c*b[1]
    );
    assert(arr == [[1, 0], [1, 1], [1, 2], [2, 0]]);
}

// https://issues.dlang.org/show_bug.cgi?id=16413 - @system comparison function
@system unittest
{
    static @system bool lt(int a, int b) { return a < b; }
    auto a = [2, 1];
    a.multiSort!(lt, lt);
    assert(a == [1, 2]);
}

private size_t getPivot(alias less, Range)(Range r)
{
    auto mid = r.length / 2;
    if (r.length < 512)
    {
        if (r.length >= 32)
            medianOf!less(r, size_t(0), mid, r.length - 1);
        return mid;
    }

    // The plan here is to take the median of five by taking five elements in
    // the array, segregate around their median, and return the position of the
    // third. We choose first, mid, last, and two more in between those.

    auto quarter = r.length / 4;
    medianOf!less(r,
        size_t(0), mid - quarter, mid, mid + quarter, r.length - 1);
    return mid;
}

/*
Sorting routine that is optimized for short ranges. Note: uses insertion sort
going downward. Benchmarked a similar routine that goes upward, for some reason
it's slower.
*/
private void shortSort(alias less, Range)(Range r)
{
    import std.algorithm.mutation : swapAt;
    alias pred = binaryFun!(less);

    switch (r.length)
    {
        case 0: case 1:
            return;
        case 2:
            if (pred(r[1], r[0])) r.swapAt(0, 1);
            return;
        case 3:
            if (pred(r[2], r[0]))
            {
                if (pred(r[0], r[1]))
                {
                    r.swapAt(0, 1);
                    r.swapAt(0, 2);
                }
                else
                {
                    r.swapAt(0, 2);
                    if (pred(r[1], r[0])) r.swapAt(0, 1);
                }
            }
            else
            {
                if (pred(r[1], r[0]))
                {
                    r.swapAt(0, 1);
                }
                else
                {
                    if (pred(r[2], r[1])) r.swapAt(1, 2);
                }
            }
            return;
        case 4:
            if (pred(r[1], r[0])) r.swapAt(0, 1);
            if (pred(r[3], r[2])) r.swapAt(2, 3);
            if (pred(r[2], r[0])) r.swapAt(0, 2);
            if (pred(r[3], r[1])) r.swapAt(1, 3);
            if (pred(r[2], r[1])) r.swapAt(1, 2);
            return;
        default:
            sort5!pred(r[r.length - 5 .. r.length]);
            if (r.length == 5) return;
            break;
    }

    assert(r.length >= 6, "r must have more than 5 elements");
    /* The last 5 elements of the range are sorted. Proceed with expanding the
    sorted portion downward. */
    immutable maxJ = r.length - 2;
    for (size_t i = r.length - 6; ; --i)
    {
        static if (is(typeof(() nothrow
            {
                auto t = r[0]; if (pred(t, r[0])) r[0] = r[0];
            }))) // Can we afford to temporarily invalidate the array?
        {
            import core.lifetime : move;

            size_t j = i + 1;
            static if (hasLvalueElements!Range)
                auto temp = move(r[i]);
            else
                auto temp = r[i];

            if (pred(r[j], temp))
            {
                do
                {
                    static if (hasLvalueElements!Range)
                        trustedMoveEmplace(r[j], r[j - 1]);
                    else
                        r[j - 1] = r[j];
                    ++j;
                }
                while (j < r.length && pred(r[j], temp));

                static if (hasLvalueElements!Range)
                    trustedMoveEmplace(temp, r[j - 1]);
                else
                    r[j - 1] = move(temp);
            }
        }
        else
        {
            size_t j = i;
            while (pred(r[j + 1], r[j]))
            {
                r.swapAt(j, j + 1);
                if (j == maxJ) break;
                ++j;
            }
        }
        if (i == 0) break;
    }
}

/// @trusted wrapper for moveEmplace
private void trustedMoveEmplace(T)(ref T source, ref T target) @trusted
{
    import core.lifetime : moveEmplace;
    moveEmplace(source, target);
}

@safe unittest
{
    import std.random : Random = Xorshift, uniform;

    auto rnd = Random(1);
    auto a = new int[uniform(100, 200, rnd)];
    foreach (ref e; a)
    {
        e = uniform(-100, 100, rnd);
    }

    shortSort!(binaryFun!("a < b"), int[])(a);
    assert(isSorted(a));
}

/*
Sorts the first 5 elements exactly of range r.
*/
private void sort5(alias lt, Range)(Range r)
{
    assert(r.length >= 5, "r must have more than 4 elements");

    import std.algorithm.mutation : swapAt;

    // 1. Sort first two pairs
    if (lt(r[1], r[0])) r.swapAt(0, 1);
    if (lt(r[3], r[2])) r.swapAt(2, 3);

    // 2. Arrange first two pairs by the largest element
    if (lt(r[3], r[1]))
    {
        r.swapAt(0, 2);
        r.swapAt(1, 3);
    }
    assert(!lt(r[1], r[0]) && !lt(r[3], r[1]) && !lt(r[3], r[2]), "unexpected"
        ~ " order");

    // 3. Insert 4 into [0, 1, 3]
    if (lt(r[4], r[1]))
    {
        r.swapAt(3, 4);
        r.swapAt(1, 3);
        if (lt(r[1], r[0]))
        {
            r.swapAt(0, 1);
        }
    }
    else if (lt(r[4], r[3]))
    {
        r.swapAt(3, 4);
    }
    assert(!lt(r[1], r[0]) && !lt(r[3], r[1]) && !lt(r[4], r[3]), "unexpected"
        ~ " order");

    // 4. Insert 2 into [0, 1, 3, 4] (note: we already know the last is greater)
    assert(!lt(r[4], r[2]), "unexpected order");
    if (lt(r[2], r[1]))
    {
        r.swapAt(1, 2);
        if (lt(r[1], r[0]))
        {
            r.swapAt(0, 1);
        }
    }
    else if (lt(r[3], r[2]))
    {
        r.swapAt(2, 3);
    }
    // 7 comparisons, 0-9 swaps
}

@safe unittest
{
    import std.algorithm.iteration : permutations;
    import std.algorithm.mutation : copy;
    import std.range : iota;

    int[5] buf;
    foreach (per; iota(5).permutations)
    {
        per.copy(buf[]);
        sort5!((a, b) => a < b)(buf[]);
        assert(buf[].isSorted);
    }
}

// sort
/**
Sorts a random-access range according to the predicate `less`.

Performs $(BIGOH r.length * log(r.length)) evaluations of `less`. If `less` involves
expensive computations on the _sort key, it may be worthwhile to use
$(LREF schwartzSort) instead.

Stable sorting requires `hasAssignableElements!Range` to be true.

`sort` returns a $(REF SortedRange, std,range) over the original range,
allowing functions that can take advantage of sorted data to know that the
range is sorted and adjust accordingly. The $(REF SortedRange, std,range) is a
wrapper around the original range, so both it and the original range are sorted.
Other functions can't know that the original range has been sorted, but
they $(I can) know that $(REF SortedRange, std,range) has been sorted.

Preconditions:

The predicate is expected to satisfy certain rules in order for `sort` to
behave as expected - otherwise, the program may fail on certain inputs (but not
others) when not compiled in release mode, due to the cursory `assumeSorted`
check. Specifically, `sort` expects `less(a,b) && less(b,c)` to imply
`less(a,c)` (transitivity), and, conversely, `!less(a,b) && !less(b,c)` to
imply `!less(a,c)`. Note that the default predicate (`"a < b"`) does not
always satisfy these conditions for floating point types, because the expression
will always be `false` when either `a` or `b` is NaN.
Use $(REF cmp, std,math) instead.

Params:
    less = The predicate to sort by.
    ss = The swapping strategy to use.
    r = The range to sort.

Returns: The initial range wrapped as a `SortedRange` with the predicate
`binaryFun!less`.

Algorithms: $(HTTP en.wikipedia.org/wiki/Introsort, Introsort) is used for unstable sorting and
$(HTTP en.wikipedia.org/wiki/Timsort, Timsort) is used for stable sorting.
Each algorithm has benefits beyond stability. Introsort is generally faster but
Timsort may achieve greater speeds on data with low entropy or if predicate calls
are expensive. Introsort performs no allocations whereas Timsort will perform one
or more allocations per call. Both algorithms have $(BIGOH n log n) worst-case
time complexity.

See_Also:
    $(REF assumeSorted, std,range)$(BR)
    $(REF SortedRange, std,range)$(BR)
    $(REF SwapStrategy, std,algorithm,mutation)$(BR)
    $(REF binaryFun, std,functional)
*/
SortedRange!(Range, less)
sort(alias less = "a < b", SwapStrategy ss = SwapStrategy.unstable, Range)
(Range r)
    /+ Unstable sorting uses the quicksort algorithm, which uses swapAt,
       which either uses swap(...), requiring swappable elements, or just
       swaps using assignment.
       Stable sorting uses TimSort, which needs to copy elements into a buffer,
       requiring assignable elements. +/
{
    import std.range : assumeSorted;
    static if (ss == SwapStrategy.unstable)
    {
        static assert(hasSwappableElements!Range || hasAssignableElements!Range,
                  "When using SwapStrategy.unstable, the passed Range '"
                ~ Range.stringof ~ "' must"
                ~ " either fulfill hasSwappableElements, or"
                ~ " hasAssignableElements, both were not the case");
    }
    else
    {
        static assert(hasAssignableElements!Range, "When using a SwapStrategy"
                ~ " != unstable, the"
                ~ " passed Range '" ~ Range.stringof ~ "' must fulfill"
                ~ " hasAssignableElements, which it did not");
    }

    static assert(isRandomAccessRange!Range, "The passed Range '"
            ~ Range.stringof ~ "' must be a Random AccessRange "
            ~ "(isRandomAccessRange)");

    static assert(hasSlicing!Range, "The passed Range '"
            ~ Range.stringof ~ "' must allow Slicing (hasSlicing)");

    static assert(hasLength!Range, "The passed Range '"
            ~ Range.stringof ~ "' must have a length (hasLength)");

    alias lessFun = binaryFun!(less);
    alias LessRet = typeof(lessFun(r.front, r.front));    // instantiate lessFun

    static assert(is(LessRet == bool), "The return type of the template"
            ~ " argument 'less' when used with the binaryFun!less template"
            ~ " must be a bool. This is not the case, the returned type is '"
            ~ LessRet.stringof ~ "'");

    static if (ss == SwapStrategy.unstable)
        quickSortImpl!(lessFun)(r, r.length);
    else //use Tim Sort for semistable & stable
        TimSortImpl!(lessFun, Range).sort(r, null);

    assert(isSorted!lessFun(r), "Failed to sort range of type " ~ Range.stringof);
    return assumeSorted!less(r);
}

///
@safe pure nothrow unittest
{
    int[] array = [ 1, 2, 3, 4 ];

    // sort in descending order
    array.sort!("a > b");
    assert(array == [ 4, 3, 2, 1 ]);

    // sort in ascending order
    array.sort();
    assert(array == [ 1, 2, 3, 4 ]);

    // sort with reusable comparator and chain
    alias myComp = (x, y) => x > y;
    assert(array.sort!(myComp).release == [ 4, 3, 2, 1 ]);
}

///
@safe unittest
{
    // Showcase stable sorting
    import std.algorithm.mutation : SwapStrategy;
    string[] words = [ "aBc", "a", "abc", "b", "ABC", "c" ];
    sort!("toUpper(a) < toUpper(b)", SwapStrategy.stable)(words);
    assert(words == [ "a", "aBc", "abc", "ABC", "b", "c" ]);
}

///
@safe unittest
{
    // Sorting floating-point numbers in presence of NaN
    double[] numbers = [-0.0, 3.0, -2.0, double.nan, 0.0, -double.nan];

    import std.algorithm.comparison : equal;
    import std.math.operations : cmp;
    import std.math.traits : isIdentical;

    sort!((a, b) => cmp(a, b) < 0)(numbers);

    double[] sorted = [-double.nan, -2.0, -0.0, 0.0, 3.0, double.nan];
    assert(numbers.equal!isIdentical(sorted));
}

@safe unittest
{
    // Simple regression benchmark
    import std.algorithm.iteration, std.algorithm.mutation;
    import std.array : array;
    import std.random : Random, uniform;
    import std.range : iota;
    Random rng;
    int[] a = iota(20148).map!(_ => uniform(-1000, 1000, rng)).array;
    static uint comps;
    static bool less(int a, int b) { ++comps; return a < b; }
    sort!less(a); // random numbers
    sort!less(a); // sorted ascending
    a.reverse();
    sort!less(a); // sorted descending
    a[] = 0;
    sort!less(a); // all equal

    // This should get smaller with time. On occasion it may go larger, but only
    // if there's thorough justification.
    debug enum uint watermark = 1676220;
    else enum uint watermark = 1676220;

    import std.conv;
    assert(comps <= watermark, text("You seem to have pessimized sort! ",
        watermark, " < ", comps));
    assert(comps >= watermark, text("You seem to have improved sort!",
        " Please update watermark from ", watermark, " to ", comps));
}

@safe unittest
{
    import std.algorithm.internal : rndstuff;
    import std.algorithm.mutation : swapRanges;
    import std.random : Random = Xorshift, uniform;
    import std.uni : toUpper;

    // sort using delegate
    auto a = new int[100];
    auto rnd = Random(123_456_789);
    foreach (ref e; a)
    {
        e = uniform(-100, 100, rnd);
    }

    int i = 0;
    bool greater2(int a, int b) @safe { return a + i > b + i; }
    auto greater = &greater2;
    sort!(greater)(a);
    assert(isSorted!(greater)(a));

    // sort using string
    sort!("a < b")(a);
    assert(isSorted!("a < b")(a));

    // sort using function; all elements equal
    foreach (ref e; a)
    {
        e = 5;
    }
    static bool less(int a, int b) { return a < b; }
    sort!(less)(a);
    assert(isSorted!(less)(a));

    string[] words = [ "aBc", "a", "abc", "b", "ABC", "c" ];
    bool lessi(string a, string b) { return toUpper(a) < toUpper(b); }
    sort!(lessi, SwapStrategy.stable)(words);
    assert(words == [ "a", "aBc", "abc", "ABC", "b", "c" ]);

    // sort using ternary predicate
    //sort!("b - a")(a);
    //assert(isSorted!(less)(a));

    a = rndstuff!(int)();
    sort(a);
    assert(isSorted(a));
    auto b = rndstuff!(string)();
    sort!("toLower(a) < toLower(b)")(b);
    assert(isSorted!("toUpper(a) < toUpper(b)")(b));

    {
        // https://issues.dlang.org/show_bug.cgi?id=10317
        enum E_10317 { a, b }
        auto a_10317 = new E_10317[10];
        sort(a_10317);
    }

    {
        // https://issues.dlang.org/show_bug.cgi?id=7767
        // Unstable sort should complete without an excessive number of predicate calls
        // This would suggest it's running in quadratic time

        // Compilation error if predicate is not static, i.e. a nested function
        static uint comp;
        static bool pred(size_t a, size_t b)
        {
            ++comp;
            return a < b;
        }

        size_t[] arr;
        arr.length = 1024;

        foreach (k; 0 .. arr.length) arr[k] = k;
        swapRanges(arr[0..$/2], arr[$/2..$]);

        sort!(pred, SwapStrategy.unstable)(arr);
        assert(comp < 25_000);
    }

    {
        import std.algorithm.mutation : swap;

        bool proxySwapCalled;
        struct S
        {
            int i;
            alias i this;
            void proxySwap(ref S other) { swap(i, other.i); proxySwapCalled = true; }
            @disable void opAssign(S value);
        }

        alias R = S[];
        R r = [S(3), S(2), S(1)];
        static assert(hasSwappableElements!R);
        static assert(!hasAssignableElements!R);
        r.sort();
        assert(proxySwapCalled);
    }

    // https://issues.dlang.org/show_bug.cgi?id=20751
    {
        static bool refPred(ref int a, ref int b)
        {
            return a < b;
        }

        auto sortedArr = [5,4,3,2,1].sort!refPred;
        sortedArr.equalRange(3);
    }
}

private void quickSortImpl(alias less, Range)(Range r, size_t depth)
{
    import std.algorithm.comparison : min, max;
    import std.algorithm.mutation : swap, swapAt;

    alias Elem = ElementType!(Range);
    enum int size = Elem.sizeof;
    enum size_t shortSortGetsBetter = max(32, 1024 / size);
    static assert(shortSortGetsBetter >= 1, Elem.stringof ~ " "
        ~ size.stringof);

    // partition
    while (r.length > shortSortGetsBetter)
    {
        if (depth == 0)
        {
            HeapOps!(less, Range).heapSort(r);
            return;
        }
        depth = depth >= depth.max / 2 ? (depth / 3) * 2 : (depth * 2) / 3;

        const pivotIdx = getPivot!(less)(r);
        auto pivot = r[pivotIdx];

        // partition
        r.swapAt(pivotIdx, r.length - 1);
        size_t lessI = size_t.max, greaterI = r.length - 1;

        outer: for (;;)
        {
            alias pred = binaryFun!less;
            while (pred(r[++lessI], pivot)) {}
            assert(lessI <= greaterI, "sort: invalid comparison function.");
            for (;;)
            {
                if (greaterI == lessI) break outer;
                if (!pred(pivot, r[--greaterI])) break;
            }
            assert(lessI <= greaterI, "sort: invalid comparison function.");
            if (lessI == greaterI) break;
            r.swapAt(lessI, greaterI);
        }

        r.swapAt(r.length - 1, lessI);
        auto left = r[0 .. lessI], right = r[lessI + 1 .. r.length];
        if (right.length > left.length)
        {
            swap(left, right);
        }
        .quickSortImpl!(less, Range)(right, depth);
        r = left;
    }
    // residual sort
    static if (shortSortGetsBetter > 1)
    {
        shortSort!(less, Range)(r);
    }
}

// Heap operations for random-access ranges
package(std) template HeapOps(alias less, Range)
{
    import std.algorithm.mutation : swapAt;

    static assert(isRandomAccessRange!Range, Range.stringof ~ " must be a"
        ~ " RandomAccessRange");
    static assert(hasLength!Range, Range.stringof ~ " must have length");
    static assert(hasSwappableElements!Range || hasAssignableElements!Range,
        Range.stringof ~ " must have swappable or assignable Elements");

    alias lessFun = binaryFun!less;

    //template because of https://issues.dlang.org/show_bug.cgi?id=12410
    void heapSort()(Range r)
    {
        // If true, there is nothing to do
        if (r.length < 2) return;
        // Build Heap
        buildHeap(r);
        // Sort
        for (size_t i = r.length - 1; i > 0; --i)
        {
            r.swapAt(0, i);
            percolate(r, 0, i);
        }
    }

    //template because of https://issues.dlang.org/show_bug.cgi?id=12410
    void buildHeap()(Range r)
    {
        immutable n = r.length;
        for (size_t i = n / 2; i-- > 0; )
        {
            siftDown(r, i, n);
        }
        assert(isHeap(r), "r is not a heap");
    }

    bool isHeap()(Range r)
    {
        size_t parent = 0;
        foreach (child; 1 .. r.length)
        {
            if (lessFun(r[parent], r[child])) return false;
            // Increment parent every other pass
            parent += !(child & 1);
        }
        return true;
    }

    // Sifts down r[parent] (which is initially assumed to be messed up) so the
    // heap property is restored for r[parent .. end].
    // template because of https://issues.dlang.org/show_bug.cgi?id=12410
    void siftDown()(Range r, size_t parent, immutable size_t end)
    {
        for (;;)
        {
            auto child = (parent + 1) * 2;
            if (child >= end)
            {
                // Leftover left child?
                if (child == end && lessFun(r[parent], r[--child]))
                    r.swapAt(parent, child);
                break;
            }
            auto leftChild = child - 1;
            if (lessFun(r[child], r[leftChild])) child = leftChild;
            if (!lessFun(r[parent], r[child])) break;
            r.swapAt(parent, child);
            parent = child;
        }
    }

    // Alternate version of siftDown that performs fewer comparisons, see
    // https://en.wikipedia.org/wiki/Heapsort#Bottom-up_heapsort. The percolate
    // process first sifts the parent all the way down (without comparing it
    // against the leaves), and then a bit up until the heap property is
    // restored. So there are more swaps but fewer comparisons. Gains are made
    // when the final position is likely to end toward the bottom of the heap,
    // so not a lot of sifts back are performed.
    //template because of https://issues.dlang.org/show_bug.cgi?id=12410
    void percolate()(Range r, size_t parent, immutable size_t end)
    {
        immutable root = parent;

        // Sift down
        for (;;)
        {
            auto child = (parent + 1) * 2;

            if (child >= end)
            {
                if (child == end)
                {
                    // Leftover left node.
                    --child;
                    r.swapAt(parent, child);
                    parent = child;
                }
                break;
            }

            auto leftChild = child - 1;
            if (lessFun(r[child], r[leftChild])) child = leftChild;
            r.swapAt(parent, child);
            parent = child;
        }

        // Sift up
        for (auto child = parent; child > root; child = parent)
        {
            parent = (child - 1) / 2;
            if (!lessFun(r[parent], r[child])) break;
            r.swapAt(parent, child);
        }
    }
}

// Tim Sort implementation
private template TimSortImpl(alias pred, R)
{
    import core.bitop : bsr;
    import std.array : uninitializedArray;

    static assert(isRandomAccessRange!R, R.stringof ~ " must be a"
        ~ " RandomAccessRange");
    static assert(hasLength!R, R.stringof ~ " must have a length");
    static assert(hasSlicing!R, R.stringof ~ " must support slicing");
    static assert(hasAssignableElements!R, R.stringof ~ " must have"
        ~ " assignable elements");

    alias T = ElementType!R;

    alias less = binaryFun!pred;
    bool greater()(auto ref T a, auto ref T b) { return less(b, a); }
    bool greaterEqual()(auto ref T a, auto ref T b) { return !less(a, b); }
    bool lessEqual()(auto ref T a, auto ref T b) { return !less(b, a); }

    enum minimalMerge = 128;
    enum minimalGallop = 7;
    enum minimalStorage = 256;
    enum stackSize = 40;

    struct Slice{ size_t base, length; }

    // Entry point for tim sort
    void sort()(R range, T[] temp)
    {
        import std.algorithm.comparison : min;
        import std.format : format;

        // Do insertion sort on small range
        if (range.length <= minimalMerge)
        {
            binaryInsertionSort(range);
            return;
        }

        immutable minRun = minRunLength(range.length);
        immutable minTemp = min(range.length / 2, minimalStorage);
        size_t minGallop = minimalGallop;
        Slice[stackSize] stack = void;
        size_t stackLen = 0;

        // Allocate temporary memory if not provided by user
        if (temp.length < minTemp)
        {
            static if (hasElaborateAssign!T) temp = new T[](minTemp);
            else temp = () @trusted { return uninitializedArray!(T[])(minTemp); }();
        }

        for (size_t i = 0; i < range.length; )
        {
            // Find length of first run in list
            size_t runLen = firstRun(range[i .. range.length]);

            // If run has less than minRun elements, extend using insertion sort
            if (runLen < minRun)
            {
                // Do not run farther than the length of the range
                immutable force = range.length - i > minRun ? minRun : range.length - i;
                binaryInsertionSort(range[i .. i + force], runLen);
                runLen = force;
            }

            // Push run onto stack
            stack[stackLen++] = Slice(i, runLen);
            i += runLen;

            // Collapse stack so that (e1 > e2 + e3 && e2 > e3)
            // STACK is | ... e1 e2 e3 >
            while (stackLen > 1)
            {
                immutable run4 = stackLen - 1;
                immutable run3 = stackLen - 2;
                immutable run2 = stackLen - 3;
                immutable run1 = stackLen - 4;

                if ( (stackLen > 2 && stack[run2].length <= stack[run3].length + stack[run4].length) ||
                     (stackLen > 3 && stack[run1].length <= stack[run3].length + stack[run2].length) )
                {
                    immutable at = stack[run2].length < stack[run4].length ? run2 : run3;
                    mergeAt(range, stack[0 .. stackLen], at, minGallop, temp);
                }
                else if (stack[run3].length > stack[run4].length) break;
                else mergeAt(range, stack[0 .. stackLen], run3, minGallop, temp);

                stackLen -= 1;
            }

            // Assert that the code above established the invariant correctly
            version (StdUnittest)
            {
                if (stackLen == 2)
                {
                    assert(stack[0].length > stack[1].length, format!
                        "stack[0].length %s > stack[1].length %s"(
                            stack[0].length, stack[1].length
                        ));
                }
                else if (stackLen > 2)
                {
                    foreach (k; 2 .. stackLen)
                    {
                        assert(stack[k - 2].length > stack[k - 1].length + stack[k].length,
                            format!"stack[k - 2].length %s > stack[k - 1].length %s + stack[k].length %s"(
                                stack[k - 2].length, stack[k - 1].length, stack[k].length
                            ));
                        assert(stack[k - 1].length > stack[k].length,
                            format!"stack[k - 1].length %s > stack[k].length %s"(
                                stack[k - 1].length, stack[k].length
                            ));
                    }
                }
            }
        }

        // Force collapse stack until there is only one run left
        while (stackLen > 1)
        {
            immutable run3 = stackLen - 1;
            immutable run2 = stackLen - 2;
            immutable run1 = stackLen - 3;
            immutable at = stackLen >= 3 && stack[run1].length <= stack[run3].length
                ? run1 : run2;
            mergeAt(range, stack[0 .. stackLen], at, minGallop, temp);
            --stackLen;
        }
    }

    // Calculates optimal value for minRun:
    // take first 6 bits of n and add 1 if any lower bits are set
    size_t minRunLength()(size_t n)
    {
        immutable shift = bsr(n)-5;
        auto result = (n >> shift) + !!(n & ~((1 << shift)-1));
        return result;
    }

    // Returns length of first run in range
    size_t firstRun()(R range)
    out(ret)
    {
        assert(ret <= range.length, "ret must be less or equal than"
            ~ " range.length");
    }
    do
    {
        import std.algorithm.mutation : reverse;

        if (range.length < 2) return range.length;

        size_t i = 2;
        if (lessEqual(range[0], range[1]))
        {
            while (i < range.length && lessEqual(range[i-1], range[i])) ++i;
        }
        else
        {
            while (i < range.length && greater(range[i-1], range[i])) ++i;
            reverse(range[0 .. i]);
        }
        return i;
    }

    // A binary insertion sort for building runs up to minRun length
    void binaryInsertionSort()(R range, size_t sortedLen = 1)
    out
    {
        if (!__ctfe) assert(isSorted!pred(range), "range must be sorted");
    }
    do
    {
        import std.algorithm.mutation : move;

        for (; sortedLen < range.length; ++sortedLen)
        {
            T item = range.moveAt(sortedLen);
            size_t lower = 0;
            size_t upper = sortedLen;
            while (upper != lower)
            {
                size_t center = (lower + upper) / 2;
                if (less(item, range[center])) upper = center;
                else lower = center + 1;
            }
            //Currently (DMD 2.061) moveAll+retro is slightly less
            //efficient then stright 'for' loop
            //11 instructions vs 7 in the innermost loop [checked on Win32]
            //moveAll(retro(range[lower .. sortedLen]),
            //            retro(range[lower+1 .. sortedLen+1]));
            for (upper=sortedLen; upper > lower; upper--)
            {
                static if (hasLvalueElements!R)
                    move(range[upper -1], range[upper]);
                else
                    range[upper] = range.moveAt(upper - 1);
            }

            static if (hasLvalueElements!R)
                move(item, range[lower]);
            else
                range[lower] = move(item);
        }
    }

    // Merge two runs in stack (at, at + 1)
    void mergeAt()(R range, Slice[] stack, immutable size_t at, ref size_t minGallop, ref T[] temp)
    in
    {
        import std.format : format;
        assert(stack.length >= 2, "stack be be greater than 1");
        assert(stack.length - at == 2 || stack.length - at == 3,
            format!"stack.length - at %s must be 2 or 3"(stack.length - at));
    }
    do
    {
        immutable base = stack[at].base;
        immutable mid  = stack[at].length;
        immutable len  = stack[at + 1].length + mid;

        // Pop run from stack
        stack[at] = Slice(base, len);
        if (stack.length - at == 3) stack[$ - 2] = stack[$ - 1];

        // Merge runs (at, at + 1)
        return merge(range[base .. base + len], mid, minGallop, temp);
    }

    // Merge two runs in a range. Mid is the starting index of the second run.
    // minGallop and temp are references; The calling function must receive the updated values.
    void merge()(R range, size_t mid, ref size_t minGallop, ref T[] temp)
    in
    {
        if (!__ctfe)
        {
            assert(isSorted!pred(range[0 .. mid]), "range[0 .. mid] must be"
                ~ " sorted");
            assert(isSorted!pred(range[mid .. range.length]), "range[mid .."
                ~ " range.length] must be sorted");
        }
    }
    do
    {
        assert(mid < range.length, "mid must be less than the length of the"
            ~ " range");

        // Reduce range of elements
        immutable firstElement = gallopForwardUpper(range[0 .. mid], range[mid]);
        immutable lastElement  = gallopReverseLower(range[mid .. range.length], range[mid - 1]) + mid;
        range = range[firstElement .. lastElement];
        mid -= firstElement;

        if (mid == 0 || mid == range.length) return;

        // Call function which will copy smaller run into temporary memory
        if (mid <= range.length / 2)
        {
            temp = ensureCapacity(mid, temp);
            minGallop = mergeLo(range, mid, minGallop, temp);
        }
        else
        {
            temp = ensureCapacity(range.length - mid, temp);
            minGallop = mergeHi(range, mid, minGallop, temp);
        }
    }

    // Enlarge size of temporary memory if needed
    T[] ensureCapacity()(size_t minCapacity, T[] temp)
    out(ret)
    {
        assert(ret.length >= minCapacity, "ensuring the capacity failed");
    }
    do
    {
        if (temp.length < minCapacity)
        {
            size_t newSize = 1<<(bsr(minCapacity)+1);
            //Test for overflow
            if (newSize < minCapacity) newSize = minCapacity;

            // can't use `temp.length` if there's no default constructor
            static if (__traits(compiles, { T defaultConstructed; cast(void) defaultConstructed; }))
            {

                static if (hasElaborateAssign!T)
                    temp.length = newSize;
                else
                {
                    if (__ctfe) temp.length = newSize;
                    else temp = () @trusted { return uninitializedArray!(T[])(newSize); }();
                }
            }
            else
            {
                static assert(!hasElaborateAssign!T,
                              "Structs which have opAssign but cannot be default-initialized " ~
                              "do not currently work with stable sort: " ~
                              "https://issues.dlang.org/show_bug.cgi?id=24810");
                temp = () @trusted { return uninitializedArray!(T[])(newSize); }();
            }
        }
        return temp;
    }

    // Merge front to back. Returns new value of minGallop.
    // temp must be large enough to store range[0 .. mid]
    size_t mergeLo()(R range, immutable size_t mid, size_t minGallop, T[] temp)
    out
    {
        if (!__ctfe) assert(isSorted!pred(range), "the range must be sorted");
    }
    do
    {
        import std.algorithm.mutation : copy;

        assert(mid <= range.length, "mid must be less than the length of the"
            ~ " range");
        assert(temp.length >= mid, "temp.length must be greater or equal to mid");

        // Copy run into temporary memory
        temp = temp[0 .. mid];
        copy(range[0 .. mid], temp);

        // Move first element into place
        moveEntry(range, mid, range, 0);

        size_t i = 1, lef = 0, rig = mid + 1;
        size_t count_lef, count_rig;
        immutable lef_end = temp.length - 1;

        if (lef < lef_end && rig < range.length)
        outer: while (true)
        {
            count_lef = 0;
            count_rig = 0;

            // Linear merge
            while ((count_lef | count_rig) < minGallop)
            {
                if (lessEqual(temp[lef], range[rig]))
                {
                    moveEntry(temp, lef++, range, i++);
                    if (lef >= lef_end) break outer;
                    ++count_lef;
                    count_rig = 0;
                }
                else
                {
                    moveEntry(range, rig++, range, i++);
                    if (rig >= range.length) break outer;
                    count_lef = 0;
                    ++count_rig;
                }
            }

            // Gallop merge
            do
            {
                count_lef = gallopForwardUpper(temp[lef .. $], range[rig]);
                foreach (j; 0 .. count_lef) moveEntry(temp, lef++, range, i++);
                if (lef >= temp.length) break outer;

                count_rig = gallopForwardLower(range[rig .. range.length], temp[lef]);
                foreach (j; 0 .. count_rig) moveEntry(range, rig++, range, i++);
                if (rig >= range.length) while (true)
                {
                    moveEntry(temp, lef++, range, i++);
                    if (lef >= temp.length) break outer;
                }

                if (minGallop > 0) --minGallop;
            }
            while (count_lef >= minimalGallop || count_rig >= minimalGallop);

            minGallop += 2;
        }

        // Move remaining elements from right
        while (rig < range.length)
            moveEntry(range, rig++, range, i++);

        // Move remaining elements from left
        while (lef < temp.length)
            moveEntry(temp, lef++, range, i++);

        return minGallop > 0 ? minGallop : 1;
    }

    // Merge back to front. Returns new value of minGallop.
    // temp must be large enough to store range[mid .. range.length]
    size_t mergeHi()(R range, immutable size_t mid, size_t minGallop, T[] temp)
    out
    {
        if (!__ctfe) assert(isSorted!pred(range), "the range must be sorted");
    }
    do
    {
        import std.algorithm.mutation : copy;
        import std.format : format;

        assert(mid <= range.length, "mid must be less or equal to range.length");
        assert(temp.length >= range.length - mid, format!
            "temp.length %s >= range.length %s - mid %s"(temp.length,
            range.length, mid));

        // Copy run into temporary memory
        temp = temp[0 .. range.length - mid];
        copy(range[mid .. range.length], temp);

        // Move first element into place
        moveEntry(range, mid - 1, range, range.length - 1);

        size_t i = range.length - 2, lef = mid - 2, rig = temp.length - 1;
        size_t count_lef, count_rig;

        outer:
        while (true)
        {
            count_lef = 0;
            count_rig = 0;

            // Linear merge
            while ((count_lef | count_rig) < minGallop)
            {
                if (greaterEqual(temp[rig], range[lef]))
                {
                    moveEntry(temp, rig, range, i--);
                    if (rig == 1)
                    {
                        // Move remaining elements from left
                        while (true)
                        {
                            moveEntry(range, lef, range, i--);
                            if (lef == 0) break;
                            --lef;
                        }

                        // Move last element into place
                        moveEntry(temp, 0, range, i);

                        break outer;
                    }
                    --rig;
                    count_lef = 0;
                    ++count_rig;
                }
                else
                {
                    moveEntry(range, lef, range, i--);
                    if (lef == 0) while (true)
                    {
                        moveEntry(temp, rig, range, i--);
                        if (rig == 0) break outer;
                        --rig;
                    }
                    --lef;
                    ++count_lef;
                    count_rig = 0;
                }
            }

            // Gallop merge
            do
            {
                count_rig = rig - gallopReverseLower(temp[0 .. rig], range[lef]);
                foreach (j; 0 .. count_rig)
                {
                    moveEntry(temp, rig, range, i--);
                    if (rig == 0) break outer;
                    --rig;
                }

                count_lef = lef - gallopReverseUpper(range[0 .. lef], temp[rig]);
                foreach (j; 0 .. count_lef)
                {
                    moveEntry(range, lef, range, i--);
                    if (lef == 0) while (true)
                    {
                        moveEntry(temp, rig, range, i--);
                        if (rig == 0) break outer;
                        --rig;
                    }
                    --lef;
                }

                if (minGallop > 0) --minGallop;
            }
            while (count_lef >= minimalGallop || count_rig >= minimalGallop);

            minGallop += 2;
        }

        return minGallop > 0 ? minGallop : 1;
    }

    // false = forward / lower, true = reverse / upper
    template gallopSearch(bool forwardReverse, bool lowerUpper)
    {
        // Gallop search on range according to attributes forwardReverse and lowerUpper
        size_t gallopSearch(R)(R range, T value)
        out(ret)
        {
            assert(ret <= range.length, "ret must be less or equal to"
                ~ " range.length");
        }
        do
        {
            size_t lower = 0, center = 1, upper = range.length;
            alias gap = center;

            static if (forwardReverse)
            {
                static if (!lowerUpper) alias comp = lessEqual; // reverse lower
                static if (lowerUpper)  alias comp = less;      // reverse upper

                // Gallop Search Reverse
                while (gap <= upper)
                {
                    if (comp(value, range[upper - gap]))
                    {
                        upper -= gap;
                        gap *= 2;
                    }
                    else
                    {
                        lower = upper - gap;
                        break;
                    }
                }

                // Binary Search Reverse
                while (upper != lower)
                {
                    center = lower + (upper - lower) / 2;
                    if (comp(value, range[center])) upper = center;
                    else lower = center + 1;
                }
            }
            else
            {
                static if (!lowerUpper) alias comp = greater;      // forward lower
                static if (lowerUpper)  alias comp = greaterEqual; // forward upper

                // Gallop Search Forward
                while (lower + gap < upper)
                {
                    if (comp(value, range[lower + gap]))
                    {
                        lower += gap;
                        gap *= 2;
                    }
                    else
                    {
                        upper = lower + gap;
                        break;
                    }
                }

                // Binary Search Forward
                while (lower != upper)
                {
                    center = lower + (upper - lower) / 2;
                    if (comp(value, range[center])) lower = center + 1;
                    else upper = center;
                }
            }

            return lower;
        }
    }

    alias gallopForwardLower = gallopSearch!(false, false);
    alias gallopForwardUpper = gallopSearch!(false,  true);
    alias gallopReverseLower = gallopSearch!( true, false);
    alias gallopReverseUpper = gallopSearch!( true,  true);

    /// Helper method that moves from[fIdx] into to[tIdx] if both are lvalues and
    /// uses a plain assignment if not (necessary for backwards compatibility)
    void moveEntry(X, Y)(ref X from, const size_t fIdx, ref Y to, const size_t tIdx)
    {
        // This template is instantiated with different combinations of range (R) and temp (T[]).
        // T[] obviously has lvalue-elements, so checking R should be enough here
        static if (hasLvalueElements!R)
        {
            import core.lifetime : move;
            move(from[fIdx], to[tIdx]);
        }
        else
            to[tIdx] = from[fIdx];
    }
}

@safe unittest
{
    import std.random : Random, uniform, randomShuffle;

    // Element type with two fields
    static struct E
    {
        size_t value, index;
    }

    // Generates data especially for testing sorting with Timsort
    static E[] genSampleData(uint seed) @safe
    {
        import std.algorithm.mutation : swap, swapRanges;

        auto rnd = Random(seed);

        E[] arr;
        arr.length = 64 * 64;

        // We want duplicate values for testing stability
        foreach (i, ref v; arr) v.value = i / 64;

        // Swap ranges at random middle point (test large merge operation)
        immutable mid = uniform(arr.length / 4, arr.length / 4 * 3, rnd);
        swapRanges(arr[0 .. mid], arr[mid .. $]);

        // Shuffle last 1/8 of the array (test insertion sort and linear merge)
        randomShuffle(arr[$ / 8 * 7 .. $], rnd);

        // Swap few random elements (test galloping mode)
        foreach (i; 0 .. arr.length / 64)
        {
            immutable a = uniform(0, arr.length, rnd), b = uniform(0, arr.length, rnd);
            swap(arr[a], arr[b]);
        }

        // Now that our test array is prepped, store original index value
        // This will allow us to confirm the array was sorted stably
        foreach (i, ref v; arr) v.index = i;

        return arr;
    }

    // Tests the Timsort function for correctness and stability
    static bool testSort(uint seed)
    {
        import std.format : format;
        auto arr = genSampleData(seed);

        // Now sort the array!
        static bool comp(E a, E b)
        {
            return a.value < b.value;
        }

        sort!(comp, SwapStrategy.stable)(arr);

        // Test that the array was sorted correctly
        assert(isSorted!comp(arr), "arr must be sorted");

        // Test that the array was sorted stably
        foreach (i; 0 .. arr.length - 1)
        {
            if (arr[i].value == arr[i + 1].value)
                assert(arr[i].index < arr[i + 1].index, format!
                    "arr[i %s].index %s < arr[i + 1].index %s"(
                    i, arr[i].index, arr[i + 1].index));
        }

        return true;
    }

    enum seed = 310614065;
    testSort(seed);

    enum result = testSort(seed);
    assert(result == true, "invalid result");
}

// https://issues.dlang.org/show_bug.cgi?id=4584
@safe unittest
{
    assert(isSorted!"a < b"(sort!("a < b", SwapStrategy.stable)(
       [83, 42, 85, 86, 87, 22, 89, 30, 91, 46, 93, 94, 95, 6,
         97, 14, 33, 10, 101, 102, 103, 26, 105, 106, 107, 6]
    )));

}

@safe unittest
{
    //test stable sort + zip
    import std.range;
    auto x = [10, 50, 60, 60, 20];
    dchar[] y = "abcde"d.dup;

    sort!("a[0] < b[0]", SwapStrategy.stable)(zip(x, y));
    assert(x == [10, 20, 50, 60, 60]);
    assert(y == "aebcd"d);
}

// https://issues.dlang.org/show_bug.cgi?id=14223
@safe unittest
{
    import std.array, std.range;
    auto arr = chain(iota(0, 384), iota(0, 256), iota(0, 80), iota(0, 64), iota(0, 96)).array;
    sort!("a < b", SwapStrategy.stable)(arr);
}

@safe unittest
{
    static struct NoCopy
    {
        pure nothrow @nogc @safe:

        int key;
        this(scope const ref NoCopy)
        {
            assert(false, "Tried to copy struct!");
        }
        ref opAssign()(scope const auto ref NoCopy other)
        {
            assert(false, "Tried to copy struct!");
        }
        this(this) {}
    }

    static NoCopy[] makeArray(const size_t size)
    {
        NoCopy[] array = new NoCopy[](size);
        foreach (const i, ref t; array[0..$/2]) t.key = cast(int) (size - i);
        foreach (const i, ref t; array[$/2..$]) t.key = cast(int) i;
        return array;
    }

    alias cmp = (ref NoCopy a, ref NoCopy b) => a.key < b.key;
    enum minMerge = TimSortImpl!(cmp, NoCopy[]).minimalMerge;

    sort!(cmp, SwapStrategy.unstable)(makeArray(20));
    sort!(cmp, SwapStrategy.stable)(makeArray(minMerge - 5));
    sort!(cmp, SwapStrategy.stable)(makeArray(minMerge + 5));
}

// https://issues.dlang.org/show_bug.cgi?id=23668
@safe unittest
{
    static struct S
    {
        int opCmp(const S) const { return 1; }
        @disable this();
    }
    S[] array;
    array.sort!("a < b", SwapStrategy.stable);
}

// https://issues.dlang.org/show_bug.cgi?id=24773
@safe unittest
{
    static struct S
    {
        int i = 42;
        ~this() { assert(i == 42); }
    }

    auto array = new S[](400);
    array.sort!((a, b) => false, SwapStrategy.stable);
}

// https://issues.dlang.org/show_bug.cgi?id=24809
@safe unittest
{
    static struct E
    {
        int value;
        int valid = 42;

        ~this()
        {
            assert(valid == 42);
        }
    }

    import std.array : array;
    import std.range : chain, only, repeat;
    auto arr = chain(repeat(E(41), 18),
                     only(E(39)),
                     repeat(E(41), 16),
                     only(E(1)),
                     repeat(E(42), 33),
                     only(E(33)),
                     repeat(E(42), 16),
                     repeat(E(43), 27),
                     only(E(33)),
                     repeat(E(43), 34),
                     only(E(34)),
                     only(E(43)),
                     only(E(63)),
                     repeat(E(44), 42),
                     only(E(27)),
                     repeat(E(44), 11),
                     repeat(E(45), 64),
                     repeat(E(46), 3),
                     only(E(11)),
                     repeat(E(46), 7),
                     only(E(4)),
                     repeat(E(46), 34),
                     only(E(36)),
                     repeat(E(46), 17),
                     repeat(E(47), 36),
                     only(E(39)),
                     repeat(E(47), 26),
                     repeat(E(48), 17),
                     only(E(21)),
                     repeat(E(48), 5),
                     only(E(39)),
                     repeat(E(48), 14),
                     only(E(58)),
                     repeat(E(48), 24),
                     repeat(E(49), 13),
                     only(E(40)),
                     repeat(E(49), 38),
                     only(E(18)),
                     repeat(E(49), 11),
                     repeat(E(50), 6)).array();

    arr.sort!((a, b) => a.value < b.value, SwapStrategy.stable)();
}

// schwartzSort
/**
Alternative sorting method that should be used when comparing keys involves an
expensive computation.

Instead of using `less(a, b)` for comparing elements,
`schwartzSort` uses `less(transform(a), transform(b))`. The values of the
`transform` function are precomputed in a temporary array, thus saving on
repeatedly computing it. Conversely, if the cost of `transform` is small
compared to the cost of allocating and filling the precomputed array, `sort`
may be faster and therefore preferable.

This approach to sorting is akin to the $(HTTP
wikipedia.org/wiki/Schwartzian_transform, Schwartzian transform), also known as
the decorate-sort-undecorate pattern in Python and Lisp. The complexity is the
same as that of the corresponding `sort`, but `schwartzSort` evaluates
`transform` only `r.length` times (less than half when compared to regular
sorting). The usage can be best illustrated with an example.

Example:
----
uint hashFun(string) { ... expensive computation ... }
string[] array = ...;
// Sort strings by hash, slow
sort!((a, b) => hashFun(a) < hashFun(b))(array);
// Sort strings by hash, fast (only computes arr.length hashes):
schwartzSort!(hashFun, "a < b")(array);
----

The `schwartzSort` function might require less temporary data and
be faster than the Perl idiom or the decorate-sort-undecorate idiom
present in Python and Lisp. This is because sorting is done in-place
and only minimal extra data (one array of transformed elements) is
created.

To check whether an array was sorted and benefit of the speedup of
Schwartz sorting, a function `schwartzIsSorted` is not provided
because the effect can be achieved by calling $(D
isSorted!less(map!transform(r))).

Params:
    transform = The transformation to apply. Either a unary function
                (`unaryFun!transform(element)`), or a binary function
                (`binaryFun!transform(element, index)`).
    less = The predicate to sort the transformed elements by.
    ss = The swapping strategy to use.
    r = The range to sort.

Returns: The initial range wrapped as a `SortedRange` with the
predicate `(a, b) => binaryFun!less(transform(a), transform(b))`.
 */
SortedRange!(R, ((a, b) => binaryFun!less(unaryFun!transform(a),
                                          unaryFun!transform(b))))
schwartzSort(alias transform, alias less = "a < b",
        SwapStrategy ss = SwapStrategy.unstable, R)(R r)
if (isRandomAccessRange!R && hasLength!R && hasSwappableElements!R &&
    !is(typeof(binaryFun!less) == SwapStrategy))
{
    import core.lifetime : emplace;
    import std.range : zip, SortedRange;
    import std.string : representation;

    static if (is(typeof(unaryFun!transform(r.front))))
    {
        alias transformFun = unaryFun!transform;
        alias TB = typeof(transformFun(r.front));
        enum isBinary = false;
    }
    else static if (is(typeof(binaryFun!transform(r.front, 0))))
    {
        alias transformFun = binaryFun!transform;
        alias TB = typeof(transformFun(r.front, 0));
        enum isBinary = true;
    }
    else
        static assert(false, "unsupported `transform` alias");

    // The `transform` function might return a qualified type, e.g. const(int).
    // Strip qualifiers if possible s.t. the temporary array is sortable.
    static if (is(TB : Unqual!TB))
        alias T = Unqual!TB;
    else
        static assert(false, "`transform` returns an unsortable qualified type: " ~ TB.stringof);

    static trustedMalloc()(size_t len) @trusted
    {
        import core.checkedint : mulu;
        import core.memory : pureMalloc;
        bool overflow;
        const nbytes = mulu(len, T.sizeof, overflow);
        if (overflow) assert(false, "multiplication overflowed");
        T[] result = (cast(T*) pureMalloc(nbytes))[0 .. len];
        static if (hasIndirections!T)
        {
            import core.memory : GC;
            GC.addRange(result.ptr, nbytes);
        }
        return result;
    }
    auto xform1 = trustedMalloc(r.length);

    size_t length;
    scope(exit)
    {
        static if (hasElaborateDestructor!T)
        {
            foreach (i; 0 .. length) collectException(destroy(xform1[i]));
        }
        static void trustedFree()(T[] p) @trusted
        {
            import core.memory : pureFree;
            static if (hasIndirections!T)
            {
                import core.memory : GC;
                GC.removeRange(p.ptr);
            }
            pureFree(p.ptr);
        }
        trustedFree(xform1);
    }
    for (; length != r.length; ++length)
    {
        static if (isBinary)
            emplace(&xform1[length], transformFun(r[length], length));
        else
            emplace(&xform1[length], transformFun(r[length]));
    }
    // Make sure we use ubyte[] and ushort[], not char[] and wchar[]
    // for the intermediate array, lest zip gets confused.
    static if (isNarrowString!(typeof(xform1)))
    {
        auto xform = xform1.representation();
    }
    else
    {
        alias xform = xform1;
    }
    zip(xform, r).sort!((a, b) => binaryFun!less(a[0], b[0]), ss)();
    return typeof(return)(r);
}

/// ditto
auto schwartzSort(alias transform, SwapStrategy ss, R)(R r)
if (isRandomAccessRange!R && hasLength!R && hasSwappableElements!R)
{
    return schwartzSort!(transform, "a < b", ss, R)(r);
}

///
@safe pure unittest
{
    import std.algorithm.iteration : map;
    import std.numeric : entropy;

    auto lowEnt = [ 1.0, 0, 0 ],
         midEnt = [ 0.1, 0.1, 0.8 ],
        highEnt = [ 0.31, 0.29, 0.4 ];
    auto arr = new double[][3];
    arr[0] = midEnt;
    arr[1] = lowEnt;
    arr[2] = highEnt;

    schwartzSort!(entropy, "a > b")(arr);

    assert(arr[0] == highEnt);
    assert(arr[1] == midEnt);
    assert(arr[2] == lowEnt);
    assert(isSorted!("a > b")(map!(entropy)(arr)));
}

@safe pure unittest
{
    import std.algorithm.iteration : map;
    import std.numeric : entropy;

    auto lowEnt = [ 1.0, 0, 0 ],
        midEnt = [ 0.1, 0.1, 0.8 ],
        highEnt = [ 0.31, 0.29, 0.4 ];
    auto arr = new double[][3];
    arr[0] = midEnt;
    arr[1] = lowEnt;
    arr[2] = highEnt;

    schwartzSort!(entropy, "a < b")(arr);

    assert(arr[0] == lowEnt);
    assert(arr[1] == midEnt);
    assert(arr[2] == highEnt);
    assert(isSorted!("a < b")(map!(entropy)(arr)));
}

@safe pure unittest
{
    // binary transform function
    string[] strings = [ "one", "two", "three" ];
    schwartzSort!((element, index) => size_t.max - index)(strings);
    assert(strings == [ "three", "two", "one" ]);
}

// https://issues.dlang.org/show_bug.cgi?id=4909
@safe pure unittest
{
    import std.typecons : Tuple;
    Tuple!(char)[] chars;
    schwartzSort!"a[0]"(chars);
}

// https://issues.dlang.org/show_bug.cgi?id=5924
@safe pure unittest
{
    import std.typecons : Tuple;
    Tuple!(char)[] chars;
    schwartzSort!((Tuple!(char) c){ return c[0]; })(chars);
}

// https://issues.dlang.org/show_bug.cgi?id=13965
@safe pure unittest
{
    import std.typecons : Tuple;
    Tuple!(char)[] chars;
    schwartzSort!("a[0]", SwapStrategy.stable)(chars);
}

// https://issues.dlang.org/show_bug.cgi?id=13965
@safe pure unittest
{
    import std.algorithm.iteration : map;
    import std.numeric : entropy;

    auto lowEnt = [ 1.0, 0, 0 ],
        midEnt = [ 0.1, 0.1, 0.8 ],
        highEnt = [ 0.31, 0.29, 0.4 ];
    auto arr = new double[][3];
    arr[0] = midEnt;
    arr[1] = lowEnt;
    arr[2] = highEnt;

    schwartzSort!(entropy, SwapStrategy.stable)(arr);

    assert(arr[0] == lowEnt);
    assert(arr[1] == midEnt);
    assert(arr[2] == highEnt);
    assert(isSorted!("a < b")(map!(entropy)(arr)));
}

// https://issues.dlang.org/show_bug.cgi?id=20799
@safe unittest
{
    import std.range : iota, retro;
    import std.array : array;

    auto arr = 1_000_000.iota.retro.array;
    arr.schwartzSort!(
        n => new int(n),
        (a, b) => *a < *b
    );
    assert(arr.isSorted());
}

// https://issues.dlang.org/show_bug.cgi?id=21183
@safe unittest
{
    static T get(T)(int) { return T.init; }

    // There's no need to actually sort, just checking type interference
    if (false)
    {
        int[] arr;

        // Fine because there are no indirections
        arr.schwartzSort!(get!(const int));

        // Fine because it decays to immutable(int)*
        arr.schwartzSort!(get!(immutable int*));

        // Disallowed because it would require a non-const reference
        static assert(!__traits(compiles, arr.schwartzSort!(get!(const Object))));

        static struct Wrapper
        {
            int* ptr;
        }

        // Disallowed because Wrapper.ptr would become mutable
        static assert(!__traits(compiles, arr.schwartzSort!(get!(const Wrapper))));
    }
}

// partialSort
/**
Reorders the random-access range `r` such that the range `r[0 .. mid]`
is the same as if the entire `r` were sorted, and leaves
the range `r[mid .. r.length]` in no particular order.

Performs $(BIGOH r.length * log(mid)) evaluations of `pred`. The
implementation simply calls `topN!(less, ss)(r, n)` and then $(D
sort!(less, ss)(r[0 .. n])).

Params:
    less = The predicate to sort by.
    ss = The swapping strategy to use.
    r = The random-access range to reorder.
    n = The length of the initial segment of `r` to sort.
*/
void partialSort(alias less = "a < b", SwapStrategy ss = SwapStrategy.unstable,
    Range)(Range r, size_t n)
if (isRandomAccessRange!(Range) && hasLength!(Range) && hasSlicing!(Range))
{
    partialSort!(less, ss)(r[0 .. n], r[n .. $]);
}

///
@system unittest
{
    int[] a = [ 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 ];
    partialSort(a, 5);
    assert(a[0 .. 5] == [ 0, 1, 2, 3, 4 ]);
}

/**
Stores the smallest elements of the two ranges in the left-hand range in sorted order.

Params:
    less = The predicate to sort by.
    ss = The swapping strategy to use.
    r1 = The first range.
    r2 = The second range.
 */

void partialSort(alias less = "a < b", SwapStrategy ss = SwapStrategy.unstable,
    Range1, Range2)(Range1 r1, Range2 r2)
if (isRandomAccessRange!(Range1) && hasLength!Range1 &&
    isInputRange!Range2 && is(ElementType!Range1 == ElementType!Range2) &&
    hasLvalueElements!Range1 && hasLvalueElements!Range2)
{
    topN!(less, ss)(r1, r2);
    sort!(less, ss)(r1);
}
///
@system unittest
{
    int[] a = [5, 7, 2, 6, 7];
    int[] b = [2, 1, 5, 6, 7, 3, 0];

    partialSort(a, b);
    assert(a == [0, 1, 2, 2, 3]);
}

// topN
/**
Reorders the range `r` using $(REF_ALTTEXT swap, swap, std,algorithm,mutation)
such that `r[nth]` refers to the element that would fall there if the range
were fully sorted.

It is akin to $(LINK2 https://en.wikipedia.org/wiki/Quickselect, Quickselect),
and partitions `r` such that all elements
`e1` from `r[0]` to `r[nth]` satisfy `!less(r[nth], e1)`,
and all elements `e2` from `r[nth]` to `r[r.length]` satisfy
`!less(e2, r[nth])`. Effectively, it finds the `nth + 1` smallest
(according to `less`) elements in `r`. Performs an expected
$(BIGOH r.length) (if unstable) or $(BIGOH r.length * log(r.length))
(if stable) evaluations of `less` and $(REF_ALTTEXT swap, swap, std,algorithm,mutation).

If `n >= r.length`, the algorithm has no effect and returns
`r[0 .. r.length]`.

Params:
    less = The predicate to sort by.
    ss = The swapping strategy to use.
    r = The random-access range to reorder.
    nth = The index of the element that should be in sorted position after the
        function is done.

Returns: a slice from `r[0]` to `r[nth]`, excluding `r[nth]` itself.

See_Also:
    $(LREF topNIndex),

BUGS:

Stable topN has not been implemented yet.
*/
auto topN(alias less = "a < b",
        SwapStrategy ss = SwapStrategy.unstable,
        Range)(Range r, size_t nth)
if (isRandomAccessRange!(Range) && hasLength!Range &&
    hasSlicing!Range && hasAssignableElements!Range)
{
    static assert(ss == SwapStrategy.unstable,
            "Stable topN not yet implemented");
    if (nth >= r.length) return r[0 .. r.length];
    auto ret = r[0 .. nth];
    if (false)
    {
        // Workaround for https://issues.dlang.org/show_bug.cgi?id=16528
        // Safety checks: enumerate all potentially unsafe generic primitives
        // then use a @trusted implementation.
        cast(void) binaryFun!less(r[0], r[r.length - 1]);
        import std.algorithm.mutation : swapAt;
        r.swapAt(size_t(0), size_t(0));
        static assert(is(typeof(r.length) == size_t),
            typeof(r.length).stringof ~ " must be of type size_t");
        pivotPartition!less(r, 0);
    }
    bool useSampling = true;
    topNImpl!(binaryFun!less)(r, nth, useSampling);
    return ret;
}

///
@safe unittest
{
    int[] v = [ 25, 7, 9, 2, 0, 5, 21 ];
    topN!"a < b"(v, 100);
    assert(v == [ 25, 7, 9, 2, 0, 5, 21 ]);
    auto n = 4;
    topN!((a, b) => a < b)(v, n);
    assert(v[n] == 9);
}

// https://issues.dlang.org/show_bug.cgi?id=8341
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : zip;
    import std.typecons : tuple;
    auto a = [10, 30, 20];
    auto b = ["c", "b", "a"];
    assert(topN!"a[0] > b[0]"(zip(a, b), 2).equal([tuple(20, "a"), tuple(30, "b")]));
}

private @trusted
void topNImpl(alias less, R)(R r, size_t n, ref bool useSampling)
{
    for (;;)
    {
        import std.algorithm.mutation : swapAt;
        assert(n < r.length);
        size_t pivot = void;

        // Decide strategy for partitioning
        if (n == 0)
        {
            pivot = 0;
            foreach (i; 1 .. r.length)
                if (less(r[i], r[pivot])) pivot = i;
            r.swapAt(n, pivot);
            return;
        }
        if (n + 1 == r.length)
        {
            pivot = 0;
            foreach (i; 1 .. r.length)
                if (less(r[pivot], r[i])) pivot = i;
            r.swapAt(n, pivot);
            return;
        }
        if (r.length <= 12)
        {
            pivot = pivotPartition!less(r, r.length / 2);
        }
        else if (n * 16 <= (r.length - 1) * 7)
        {
            pivot = topNPartitionOffMedian!(less, No.leanRight)
                (r, n, useSampling);
            // Quality check
            if (useSampling)
            {
                if (pivot < n)
                {
                    if (pivot * 4 < r.length)
                    {
                        useSampling = false;
                    }
                }
                else if ((r.length - pivot) * 8 < r.length * 3)
                {
                    useSampling = false;
                }
            }
        }
        else if (n * 16 >= (r.length - 1) * 9)
        {
            pivot = topNPartitionOffMedian!(less, Yes.leanRight)
                (r, n, useSampling);
            // Quality check
            if (useSampling)
            {
                if (pivot < n)
                {
                    if (pivot * 8 < r.length * 3)
                    {
                        useSampling = false;
                    }
                }
                else if ((r.length - pivot) * 4 < r.length)
                {
                    useSampling = false;
                }
            }
        }
        else
        {
            pivot = topNPartition!less(r, n, useSampling);
            // Quality check
            if (useSampling &&
                (pivot * 9 < r.length * 2 || pivot * 9 > r.length * 7))
            {
                // Failed - abort sampling going forward
                useSampling = false;
            }
        }

        assert(pivot != size_t.max, "pivot must be not equal to size_t.max");
        // See how the pivot fares
        if (pivot == n)
        {
            return;
        }
        if (pivot > n)
        {
            r = r[0 .. pivot];
        }
        else
        {
            n -= pivot + 1;
            r = r[pivot + 1 .. r.length];
        }
    }
}

private size_t topNPartition(alias lp, R)(R r, size_t n, bool useSampling)
{
    import std.format : format;
    assert(r.length >= 9 && n < r.length, "length must be longer than 8"
        ~ " and n must be less than r.length");
    immutable ninth = r.length / 9;
    auto pivot = ninth / 2;
    // Position subrange r[lo .. hi] to have length equal to ninth and its upper
    // median r[lo .. hi][$ / 2] in exactly the same place as the upper median
    // of the entire range r[$ / 2]. This is to improve behavior for searching
    // the median in already sorted ranges.
    immutable lo = r.length / 2 - pivot, hi = lo + ninth;
    // We have either one straggler on the left, one on the right, or none.
    assert(lo - (r.length - hi) <= 1 || (r.length - hi) - lo <= 1,
        format!"straggler check failed lo %s, r.length %s, hi %s"(lo, r.length, hi));
    assert(lo >= ninth * 4, format!"lo %s >= ninth * 4 %s"(lo, ninth * 4));
    assert(r.length - hi >= ninth * 4,
        format!"r.length %s - hi %s >= ninth * 4 %s"(r.length, hi, ninth * 4));

    // Partition in groups of 3, and the mid tertile again in groups of 3
    if (!useSampling)
        p3!lp(r, lo - ninth, hi + ninth);
    p3!lp(r, lo, hi);

    // Get the median of medians of medians
    // Map the full interval of n to the full interval of the ninth
    pivot = (n * (ninth - 1)) / (r.length - 1);
    topNImpl!lp(r[lo .. hi], pivot, useSampling);
    return expandPartition!lp(r, lo, pivot + lo, hi);
}

private void p3(alias less, Range)(Range r, size_t lo, immutable size_t hi)
{
    import std.format : format;
    assert(lo <= hi && hi < r.length,
        format!"lo %s <= hi %s && hi < r.length %s"(lo, hi, r.length));
    immutable ln = hi - lo;
    for (; lo < hi; ++lo)
    {
        assert(lo >= ln, format!"lo %s >= ln %s"(lo, ln));
        assert(lo + ln < r.length, format!"lo %s + ln %s < r.length %s"(
            lo, ln, r.length));
        medianOf!less(r, lo - ln, lo, lo + ln);
    }
}

private void p4(alias less, Flag!"leanRight" f, Range)
    (Range r, size_t lo, immutable size_t hi)
{
    import std.format : format;
    assert(lo <= hi && hi < r.length, format!"lo %s <= hi %s && hi < r.length %s"(
        lo, hi, r.length));
    immutable ln = hi - lo, _2ln = ln * 2;
    for (; lo < hi; ++lo)
    {
        assert(lo >= ln, format!"lo %s >= ln %s"(lo, ln));
        assert(lo + ln < r.length, format!"lo %s + ln %s < r.length %s"(
            lo, ln, r.length));
        static if (f == Yes.leanRight)
            medianOf!(less, f)(r, lo - _2ln, lo - ln, lo, lo + ln);
        else
            medianOf!(less, f)(r, lo - ln, lo, lo + ln, lo + _2ln);
    }
}

private size_t topNPartitionOffMedian(alias lp, Flag!"leanRight" f, R)
    (R r, size_t n, bool useSampling)
{
    assert(r.length >= 12, "The length of r must be greater than 11");
    assert(n < r.length, "n must be less than the length of r");
    immutable _4 = r.length / 4;
    static if (f == Yes.leanRight)
        immutable leftLimit = 2 * _4;
    else
        immutable leftLimit = _4;
    // Partition in groups of 4, and the left quartile again in groups of 3
    if (!useSampling)
    {
        p4!(lp, f)(r, leftLimit, leftLimit + _4);
    }
    immutable _12 = _4 / 3;
    immutable lo = leftLimit + _12, hi = lo + _12;
    p3!lp(r, lo, hi);

    // Get the median of medians of medians
    // Map the full interval of n to the full interval of the ninth
    immutable pivot = (n * (_12 - 1)) / (r.length - 1);
    topNImpl!lp(r[lo .. hi], pivot, useSampling);
    return expandPartition!lp(r, lo, pivot + lo, hi);
}

/*
Params:
less = predicate
r = range to partition
pivot = pivot to partition around
lo = value such that r[lo .. pivot] already less than r[pivot]
hi = value such that r[pivot .. hi] already greater than r[pivot]

Returns: new position of pivot
*/
private
size_t expandPartition(alias lp, R)(R r, size_t lo, size_t pivot, size_t hi)
in
{
    import std.algorithm.searching : all;
    assert(lo <= pivot, "lo must be less than or equal pivot");
    assert(pivot < hi, "pivot must be less than hi");
    assert(hi <= r.length, "hi must be less than or equal to the length of r");
    assert(r[lo .. pivot + 1].all!(x => !lp(r[pivot], x)),
        "r[lo .. pivot + 1] failed less than test");
    assert(r[pivot + 1 .. hi].all!(x => !lp(x, r[pivot])),
        "r[pivot + 1 .. hi] failed less than test");
    }
out
{
    import std.algorithm.searching : all;
    assert(r[0 .. pivot + 1].all!(x => !lp(r[pivot], x)),
        "r[0 .. pivot + 1] failed less than test");
    assert(r[pivot + 1 .. r.length].all!(x => !lp(x, r[pivot])),
        "r[pivot + 1 .. r.length] failed less than test");
}
do
{
    import std.algorithm.mutation : swapAt;
    import std.algorithm.searching : all;
    // We work with closed intervals!
    --hi;

    size_t left = 0, rite = r.length - 1;
    loop: for (;; ++left, --rite)
    {
        for (;; ++left)
        {
            if (left == lo) break loop;
            if (!lp(r[left], r[pivot])) break;
        }
        for (;; --rite)
        {
            if (rite == hi) break loop;
            if (!lp(r[pivot], r[rite])) break;
        }
        r.swapAt(left, rite);
    }

    assert(r[lo .. pivot + 1].all!(x => !lp(r[pivot], x)),
        "r[lo .. pivot + 1] failed less than test");
    assert(r[pivot + 1 .. hi + 1].all!(x => !lp(x, r[pivot])),
        "r[pivot + 1 .. hi + 1] failed less than test");
    assert(r[0 .. left].all!(x => !lp(r[pivot], x)),
        "r[0 .. left] failed less than test");
    assert(r[rite + 1 .. r.length].all!(x => !lp(x, r[pivot])),
        "r[rite + 1 .. r.length] failed less than test");

    immutable oldPivot = pivot;

    if (left < lo)
    {
        // First loop: spend r[lo .. pivot]
        for (; lo < pivot; ++left)
        {
            if (left == lo) goto done;
            if (!lp(r[oldPivot], r[left])) continue;
            --pivot;
            assert(!lp(r[oldPivot], r[pivot]), "less check failed");
            r.swapAt(left, pivot);
        }
        // Second loop: make left and pivot meet
        for (;; ++left)
        {
            if (left == pivot) goto done;
            if (!lp(r[oldPivot], r[left])) continue;
            for (;;)
            {
                if (left == pivot) goto done;
                --pivot;
                if (lp(r[pivot], r[oldPivot]))
                {
                    r.swapAt(left, pivot);
                    break;
                }
            }
        }
    }

    // First loop: spend r[lo .. pivot]
    for (; hi != pivot; --rite)
    {
        if (rite == hi) goto done;
        if (!lp(r[rite], r[oldPivot])) continue;
        ++pivot;
        assert(!lp(r[pivot], r[oldPivot]), "less check failed");
        r.swapAt(rite, pivot);
    }
    // Second loop: make left and pivot meet
    for (; rite > pivot; --rite)
    {
        if (!lp(r[rite], r[oldPivot])) continue;
        while (rite > pivot)
        {
            ++pivot;
            if (lp(r[oldPivot], r[pivot]))
            {
                r.swapAt(rite, pivot);
                break;
            }
        }
    }

done:
    r.swapAt(oldPivot, pivot);
    return pivot;
}

@safe unittest
{
    auto a = [ 10, 5, 3, 4, 8,  11,  13, 3, 9, 4, 10 ];
    assert(expandPartition!((a, b) => a < b)(a, 4, 5, 6) == 9);

    import std.algorithm.iteration : map;
    import std.array : array;
    import std.random : uniform;
    import std.range : iota;
    auto size = uniform(1, 1000);
    a = iota(0, size).map!(_ => uniform(0, 1000)).array;
    if (a.length == 0) return;
    expandPartition!((a, b) => a < b)(a, a.length / 2, a.length / 2,
        a.length / 2 + 1);
}

@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.algorithm.iteration : reduce;

    int[] v = [ 7, 6, 5, 4, 3, 2, 1, 0 ];
    ptrdiff_t n = 3;
    topN!("a < b")(v, n);
    assert(reduce!max(v[0 .. n]) <= v[n]);
    assert(reduce!min(v[n + 1 .. $]) >= v[n]);
    //
    v = [3, 4, 5, 6, 7, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5];
    n = 3;
    topN(v, n);
    assert(reduce!max(v[0 .. n]) <= v[n]);
    assert(reduce!min(v[n + 1 .. $]) >= v[n]);
    //
    v = [3, 4, 5, 6, 7, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5];
    n = 1;
    topN(v, n);
    assert(reduce!max(v[0 .. n]) <= v[n]);
    assert(reduce!min(v[n + 1 .. $]) >= v[n]);
    //
    v = [3, 4, 5, 6, 7, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5];
    n = v.length - 1;
    topN(v, n);
    assert(v[n] == 7);
    //
    v = [3, 4, 5, 6, 7, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5];
    n = 0;
    topN(v, n);
    assert(v[n] == 1);

    double[][] v1 = [[-10, -5], [-10, -3], [-10, -5], [-10, -4],
            [-10, -5], [-9, -5], [-9, -3], [-9, -5],];

    // double[][] v1 = [ [-10, -5], [-10, -4], [-9, -5], [-9, -5],
    //         [-10, -5], [-10, -3], [-10, -5], [-9, -3],];
    double[]*[] idx = [ &v1[0], &v1[1], &v1[2], &v1[3], &v1[4], &v1[5], &v1[6],
            &v1[7], ];

    auto mid = v1.length / 2;
    topN!((a, b){ return (*a)[1] < (*b)[1]; })(idx, mid);
    foreach (e; idx[0 .. mid]) assert((*e)[1] <= (*idx[mid])[1]);
    foreach (e; idx[mid .. $]) assert((*e)[1] >= (*idx[mid])[1]);
}

@safe unittest
{
    import std.algorithm.comparison : max, min;
    import std.algorithm.iteration : reduce;
    import std.random : Random = Xorshift, uniform;

    immutable uint[] seeds = [90027751, 2709791795, 1374631933, 995751648, 3541495258, 984840953];
    foreach (s; seeds)
    {
        auto r = Random(s);

        int[] a = new int[uniform(1, 10000, r)];
        foreach (ref e; a) e = uniform(-1000, 1000, r);

        auto k = uniform(0, a.length, r);
        topN(a, k);
        if (k > 0)
        {
            auto left = reduce!max(a[0 .. k]);
            assert(left <= a[k]);
        }
        if (k + 1 < a.length)
        {
            auto right = reduce!min(a[k + 1 .. $]);
            assert(right >= a[k]);
        }
    }
}

// https://issues.dlang.org/show_bug.cgi?id=12987
@safe unittest
{
    int[] a = [ 25, 7, 9, 2, 0, 5, 21 ];
    auto n = 4;
    auto t = topN(a, n);
    sort(t);
    assert(t == [0, 2, 5, 7]);
}

/**
Stores the smallest elements of the two ranges in the left-hand range.

Params:
    less = The predicate to sort by.
    ss = The swapping strategy to use.
    r1 = The first range.
    r2 = The second range.
 */
auto topN(alias less = "a < b",
        SwapStrategy ss = SwapStrategy.unstable,
        Range1, Range2)(Range1 r1, Range2 r2)
if (isRandomAccessRange!(Range1) && hasLength!Range1 &&
    isInputRange!Range2 && is(ElementType!Range1 == ElementType!Range2) &&
    hasLvalueElements!Range1 && hasLvalueElements!Range2)
{
    import std.container : BinaryHeap;

    static assert(ss == SwapStrategy.unstable,
            "Stable topN not yet implemented");

    auto heap = BinaryHeap!(Range1, less)(r1);
    foreach (ref e; r2)
    {
        heap.conditionalSwap(e);
    }

    return r1;
}

///
@system unittest
{
    int[] a = [ 5, 7, 2, 6, 7 ];
    int[] b = [ 2, 1, 5, 6, 7, 3, 0 ];
    topN(a, b);
    sort(a);
    assert(a == [0, 1, 2, 2, 3]);
}

// https://issues.dlang.org/show_bug.cgi?id=15421
@system unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange;
    import std.meta : AliasSeq;

    alias RandomRanges = AliasSeq!(
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random)
    );

    alias ReferenceRanges = AliasSeq!(
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Forward),
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Bidirectional),
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random),
        DummyRange!(ReturnBy.Reference, Length.No, RangeType.Forward),
        DummyRange!(ReturnBy.Reference, Length.No, RangeType.Bidirectional));

    foreach (T1; RandomRanges)
    {
        foreach (T2; ReferenceRanges)
        {
            import std.array : array;

            T1 A;
            T2 B;

            A.reinit();
            B.reinit();

            topN(A, B);

            // BUG(?): sort doesn't accept DummyRanges (needs Slicing and Length)
            auto a = array(A);
            auto b = array(B);
            sort(a);
            sort(b);

            assert(equal(a, [ 1, 1, 2, 2, 3, 3, 4, 4, 5, 5 ]));
            assert(equal(b, [ 6, 6, 7, 7, 8, 8, 9, 9, 10, 10 ]));
        }
    }
}

// https://issues.dlang.org/show_bug.cgi?id=15421
@system unittest
{
    auto a = [ 9, 8, 0, 3, 5, 25, 43, 4, 2, 0, 7 ];
    auto b = [ 9, 8, 0, 3, 5, 25, 43, 4, 2, 0, 7 ];

    topN(a, 4);
    topN(b[0 .. 4], b[4 .. $]);

    sort(a[0 .. 4]);
    sort(a[4 .. $]);
    sort(b[0 .. 4]);
    sort(b[4 .. $]);

    assert(a[0 .. 4] == b[0 .. 4]);
    assert(a[4 .. $] == b[4 .. $]);
    assert(a == b);
}

// https://issues.dlang.org/show_bug.cgi?id=12987
@system unittest
{
    int[] a = [ 5, 7, 2, 6, 7 ];
    int[] b = [ 2, 1, 5, 6, 7, 3, 0 ];
    auto t = topN(a, b);
    sort(t);
    assert(t == [ 0, 1, 2, 2, 3 ]);
}

// https://issues.dlang.org/show_bug.cgi?id=15420
@system unittest
{
    int[] a = [ 5, 7, 2, 6, 7 ];
    int[] b = [ 2, 1, 5, 6, 7, 3, 0 ];
    topN!"a > b"(a, b);
    sort!"a > b"(a);
    assert(a == [ 7, 7, 7, 6, 6 ]);
}

/**
Copies the top `n` elements of the
$(REF_ALTTEXT input range, isInputRange, std,range,primitives) `source` into the
random-access range `target`, where `n = target.length`.

Elements of `source` are not touched. If $(D
sorted) is `true`, the target is sorted. Otherwise, the target
respects the $(HTTP en.wikipedia.org/wiki/Binary_heap, heap property).

Params:
    less = The predicate to sort by.
    source = The source range.
    target = The target range.
    sorted = Whether to sort the elements copied into `target`.

Returns: The slice of `target` containing the copied elements.
 */
TRange topNCopy(alias less = "a < b", SRange, TRange)
    (SRange source, TRange target, SortOutput sorted = No.sortOutput)
if (isInputRange!(SRange) && isRandomAccessRange!(TRange)
    && hasLength!(TRange) && hasSlicing!(TRange))
{
    import std.container : BinaryHeap;

    if (target.empty) return target;
    auto heap = BinaryHeap!(TRange, less)(target, 0);
    foreach (e; source) heap.conditionalInsert(e);
    auto result = target[0 .. heap.length];
    if (sorted == Yes.sortOutput)
    {
        while (!heap.empty) heap.removeFront();
    }
    return result;
}

///
@system unittest
{
    import std.typecons : Yes;

    int[] a = [ 10, 16, 2, 3, 1, 5, 0 ];
    int[] b = new int[3];
    topNCopy(a, b, Yes.sortOutput);
    assert(b == [ 0, 1, 2 ]);
}

@system unittest
{
    import std.random : Random = Xorshift, uniform, randomShuffle;
    import std.typecons : Yes;

    auto r = Random(123_456_789);
    ptrdiff_t[] a = new ptrdiff_t[uniform(1, 1000, r)];
    foreach (i, ref e; a) e = i;
    randomShuffle(a, r);
    auto n = uniform(0, a.length, r);
    ptrdiff_t[] b = new ptrdiff_t[n];
    topNCopy!(binaryFun!("a < b"))(a, b, Yes.sortOutput);
    assert(isSorted!(binaryFun!("a < b"))(b));
}

/**
Given a range of elements, constructs an index of its top $(I n) elements
(i.e., the first $(I n) elements if the range were sorted).

Similar to $(LREF topN), except that the range is not modified.

Params:
    less = A binary predicate that defines the ordering of range elements.
        Defaults to `a < b`.
    ss = $(RED (Not implemented yet.)) Specify the swapping strategy.
    r = A
        $(REF_ALTTEXT random-access range, isRandomAccessRange, std,range,primitives)
        of elements to make an index for.
    index = A
        $(REF_ALTTEXT random-access range, isRandomAccessRange, std,range,primitives)
        with assignable elements to build the index in. The length of this range
        determines how many top elements to index in `r`.

        This index range can either have integral elements, in which case the
        constructed index will consist of zero-based numerical indices into
        `r`; or it can have pointers to the element type of `r`, in which
        case the constructed index will be pointers to the top elements in
        `r`.
    sorted = Determines whether to sort the index by the elements they refer
        to.

See_also: $(LREF topN), $(LREF topNCopy).

BUGS:
The swapping strategy parameter is not implemented yet; currently it is
ignored.
*/
void topNIndex(alias less = "a < b", SwapStrategy ss = SwapStrategy.unstable,
               Range, RangeIndex)
              (Range r, RangeIndex index, SortOutput sorted = No.sortOutput)
if (isRandomAccessRange!Range &&
    isRandomAccessRange!RangeIndex &&
    hasAssignableElements!RangeIndex)
{
    static assert(ss == SwapStrategy.unstable,
                  "Stable swap strategy not implemented yet.");

    import std.container.binaryheap : BinaryHeap;
    if (index.empty) return;

    static if (isIntegral!(ElementType!(RangeIndex)))
    {
        import std.exception : enforce;

        enforce(ElementType!(RangeIndex).max >= index.length,
                "Index type too small");
        bool indirectLess(ElementType!(RangeIndex) a, ElementType!(RangeIndex) b)
        {
            return binaryFun!(less)(r[a], r[b]);
        }
        auto heap = BinaryHeap!(RangeIndex, indirectLess)(index, 0);
        foreach (i; 0 .. r.length)
        {
            heap.conditionalInsert(cast(ElementType!RangeIndex) i);
        }

    }
    else static if (is(ElementType!(RangeIndex) == ElementType!(Range)*))
    {
        static bool indirectLess(const ElementType!(RangeIndex) a,
                                 const ElementType!(RangeIndex) b)
        {
            return binaryFun!less(*a, *b);
        }
        auto heap = BinaryHeap!(RangeIndex, indirectLess)(index, 0);
        foreach (i; 0 .. r.length)
        {
            heap.conditionalInsert(&r[i]);
        }
    }
    else static assert(0, "Invalid ElementType");

    if (sorted == Yes.sortOutput)
    {
        while (!heap.empty) heap.removeFront();
    }
}

///
@system unittest
{
    import std.typecons : Yes;

    // Construct index to top 3 elements using numerical indices:
    int[] a = [ 10, 2, 7, 5, 8, 1 ];
    int[] index = new int[3];
    topNIndex(a, index, Yes.sortOutput);
    assert(index == [5, 1, 3]); // because a[5]==1, a[1]==2, a[3]==5

    // Construct index to top 3 elements using pointer indices:
    int*[] ptrIndex = new int*[3];
    topNIndex(a, ptrIndex, Yes.sortOutput);
    assert(ptrIndex == [ &a[5], &a[1], &a[3] ]);
}

@system unittest
{
    import std.conv : text;

    {
        int[] a = [ 10, 8, 9, 2, 4, 6, 7, 1, 3, 5 ];
        int*[] b = new int*[5];
        topNIndex!("a > b")(a, b, Yes.sortOutput);
        assert(b == [ &a[0], &a[2], &a[1], &a[6], &a[5]]);
    }
    {
        int[] a = [ 10, 8, 9, 2, 4, 6, 7, 1, 3, 5 ];
        auto b = new ubyte[5];
        topNIndex!("a > b")(a, b, Yes.sortOutput);
        assert(b == [ cast(ubyte) 0, cast(ubyte) 2, cast(ubyte) 1, cast(ubyte) 6, cast(ubyte) 5], text(b));
    }
}

// medianOf
/*
Private for the time being.

Computes the median of 2 to 5 arbitrary indexes in random-access range `r`
using hand-written specialized algorithms. The indexes must be distinct (if not,
behavior is implementation-defined). The function also partitions the elements
involved around the median, e.g. `medianOf(r, a, b, c)` not only fills `r[b]`
with the median of `r[a]`, `r[b]`, and `r[c]`, but also puts the minimum in
`r[a]` and the maximum in `r[c]`.

Params:
less = The comparison predicate used, modeled as a
    $(LINK2 https://en.wikipedia.org/wiki/Weak_ordering#Strict_weak_orderings, strict weak ordering)
    (irreflexive, antisymmetric, transitive, and implying a transitive equivalence).
flag = Used only for even values of `T.length`. If `No.leanRight`, the median
"leans left", meaning `medianOf(r, a, b, c, d)` puts the lower median of the
four in `r[b]`, the minimum in `r[a]`, and the two others in `r[c]` and `r[d]`.
Conversely, `median!("a < b", Yes.leanRight)(r, a, b, c, d)` puts the upper
median of the four in `r[c]`, the maximum in `r[d]`, and the two others in
`r[a]` and `r[b]`.
r = The range containing the indexes.
i = Two to five indexes inside `r`.
*/
private void medianOf(
        alias less = "a < b",
        Flag!"leanRight" flag = No.leanRight,
        Range,
        Indexes...)
    (Range r, Indexes i)
if (isRandomAccessRange!Range && hasLength!Range &&
    Indexes.length >= 2 && Indexes.length <= 5 &&
    allSatisfy!(isUnsigned, Indexes))
{
    assert(r.length >= Indexes.length, "r.length must be greater than or"
        ~ " equal to Indexes.length");
    import std.functional : binaryFun;
    alias lt = binaryFun!less;
    enum k = Indexes.length;
    import std.algorithm.mutation : swapAt;
    import std.format : format;

    alias a = i[0];
    static assert(is(typeof(a) == size_t), typeof(a).stringof ~ " must be"
        ~ " of type size_t");
    static if (k >= 2)
    {
        alias b = i[1];
        static assert(is(typeof(b) == size_t), typeof(b).stringof ~ " must be"
        ~ " of type size_t");
        assert(a != b, "a != b ");
    }
    static if (k >= 3)
    {
        alias c = i[2];
        static assert(is(typeof(c) == size_t), typeof(c).stringof ~ " must be"
        ~ " of type size_t");
        assert(a != c && b != c, "a != c && b != c");
    }
    static if (k >= 4)
    {
        alias d = i[3];
        static assert(is(typeof(d) == size_t), typeof(d).stringof ~ " must be"
        ~ " of type size_t");
        assert(a != d && b != d && c != d, "a != d && b != d && c != d failed");
    }
    static if (k >= 5)
    {
        alias e = i[4];
        static assert(is(typeof(e) == size_t), typeof(e).stringof ~ " must be"
        ~ " of type size_t");
        assert(a != e && b != e && c != e && d != e,
            "a != e && b != e && c != e && d != e failed");
    }

    static if (k == 2)
    {
        if (lt(r[b], r[a])) r.swapAt(a, b);
    }
    else static if (k == 3)
    {
        if (lt(r[c], r[a])) // c < a
        {
            if (lt(r[a], r[b])) // c < a < b
            {
                r.swapAt(a, b);
                r.swapAt(a, c);
            }
            else // c < a, b <= a
            {
                r.swapAt(a, c);
                if (lt(r[b], r[a])) r.swapAt(a, b);
            }
        }
        else // a <= c
        {
            if (lt(r[b], r[a])) // b < a <= c
            {
                r.swapAt(a, b);
            }
            else // a <= c, a <= b
            {
                if (lt(r[c], r[b])) r.swapAt(b, c);
            }
        }
        assert(!lt(r[b], r[a]), "less than check failed");
        assert(!lt(r[c], r[b]), "less than check failed");
    }
    else static if (k == 4)
    {
        static if (flag == No.leanRight)
        {
            // Eliminate the rightmost from the competition
            if (lt(r[d], r[c])) r.swapAt(c, d); // c <= d
            if (lt(r[d], r[b])) r.swapAt(b, d); // b <= d
            medianOf!lt(r, a, b, c);
        }
        else
        {
            // Eliminate the leftmost from the competition
            if (lt(r[b], r[a])) r.swapAt(a, b); // a <= b
            if (lt(r[c], r[a])) r.swapAt(a, c); // a <= c
            medianOf!lt(r, b, c, d);
        }
    }
    else static if (k == 5)
    {
        // Credit: Teppo Niinimäki
        version (StdUnittest) scope(success)
        {
            assert(!lt(r[c], r[a]), "less than check failed");
            assert(!lt(r[c], r[b]), "less than check failed");
            assert(!lt(r[d], r[c]), "less than check failed");
            assert(!lt(r[e], r[c]), "less than check failed");
        }

        if (lt(r[c], r[a])) r.swapAt(a, c);
        if (lt(r[d], r[b])) r.swapAt(b, d);
        if (lt(r[d], r[c]))
        {
            r.swapAt(c, d);
            r.swapAt(a, b);
        }
        if (lt(r[e], r[b])) r.swapAt(b, e);
        if (lt(r[e], r[c]))
        {
            r.swapAt(c, e);
            if (lt(r[c], r[a])) r.swapAt(a, c);
        }
        else
        {
            if (lt(r[c], r[b])) r.swapAt(b, c);
        }
    }
}

@safe unittest
{
    // Verify medianOf for all permutations of [1, 2, 2, 3, 4].
    int[5] data = [1, 2, 2, 3, 4];
    do
    {
        int[5] a = data;
        medianOf(a[], size_t(0), size_t(1));
        assert(a[0] <= a[1]);

        a[] = data[];
        medianOf(a[], size_t(0), size_t(1), size_t(2));
        assert(ordered(a[0], a[1], a[2]));

        a[] = data[];
        medianOf(a[], size_t(0), size_t(1), size_t(2), size_t(3));
        assert(a[0] <= a[1] && a[1] <= a[2] && a[1] <= a[3]);

        a[] = data[];
        medianOf!("a < b", Yes.leanRight)(a[], size_t(0), size_t(1),
            size_t(2), size_t(3));
        assert(a[0] <= a[2] && a[1] <= a[2] && a[2] <= a[3]);

        a[] = data[];
        medianOf(a[], size_t(0), size_t(1), size_t(2), size_t(3), size_t(4));
        assert(a[0] <= a[2] && a[1] <= a[2] && a[2] <= a[3] && a[2] <= a[4]);
    }
    while (nextPermutation(data[]));
}

// nextPermutation
/**
 * Permutes `range` in-place to the next lexicographically greater
 * permutation.
 *
 * The predicate `less` defines the lexicographical ordering to be used on
 * the range.
 *
 * If the range is currently the lexicographically greatest permutation, it is
 * permuted back to the least permutation and false is returned.  Otherwise,
 * true is returned. One can thus generate all permutations of a range by
 * sorting it according to `less`, which produces the lexicographically
 * least permutation, and then calling nextPermutation until it returns false.
 * This is guaranteed to generate all distinct permutations of the range
 * exactly once.  If there are $(I N) elements in the range and all of them are
 * unique, then $(I N)! permutations will be generated. Otherwise, if there are
 * some duplicated elements, fewer permutations will be produced.
----
// Enumerate all permutations
int[] a = [1,2,3,4,5];
do
{
    // use the current permutation and
    // proceed to the next permutation of the array.
} while (nextPermutation(a));
----
 * Params:
 *  less = The ordering to be used to determine lexicographical ordering of the
 *      permutations.
 *  range = The range to permute.
 *
 * Returns: false if the range was lexicographically the greatest, in which
 * case the range is reversed back to the lexicographically smallest
 * permutation; otherwise returns true.
 * See_Also:
 * $(REF permutations, std,algorithm,iteration).
 */
bool nextPermutation(alias less="a < b", BidirectionalRange)
                    (BidirectionalRange range)
if (isBidirectionalRange!BidirectionalRange &&
    hasSwappableElements!BidirectionalRange)
{
    import std.algorithm.mutation : reverse, swap;
    import std.algorithm.searching : find;
    import std.range : retro, takeExactly;
    // Ranges of 0 or 1 element have no distinct permutations.
    if (range.empty) return false;

    auto i = retro(range);
    auto last = i.save;

    // Find last occurring increasing pair of elements
    size_t n = 1;
    for (i.popFront(); !i.empty; i.popFront(), last.popFront(), n++)
    {
        if (binaryFun!less(i.front, last.front))
            break;
    }

    if (i.empty)
    {
        // Entire range is decreasing: it's lexicographically the greatest. So
        // wrap it around.
        range.reverse();
        return false;
    }

    // Find last element greater than i.front.
    auto j = find!((a) => binaryFun!less(i.front, a))(
                   takeExactly(retro(range), n));

    assert(!j.empty, "j must not be empty");   // shouldn't happen since i.front < last.front
    swap(i.front, j.front);
    reverse(takeExactly(retro(range), n));

    return true;
}

///
@safe unittest
{
    // Step through all permutations of a sorted array in lexicographic order
    int[] a = [1,2,3];
    assert(nextPermutation(a) == true);
    assert(a == [1,3,2]);
    assert(nextPermutation(a) == true);
    assert(a == [2,1,3]);
    assert(nextPermutation(a) == true);
    assert(a == [2,3,1]);
    assert(nextPermutation(a) == true);
    assert(a == [3,1,2]);
    assert(nextPermutation(a) == true);
    assert(a == [3,2,1]);
    assert(nextPermutation(a) == false);
    assert(a == [1,2,3]);
}

///
@safe unittest
{
    // Step through permutations of an array containing duplicate elements:
    int[] a = [1,1,2];
    assert(nextPermutation(a) == true);
    assert(a == [1,2,1]);
    assert(nextPermutation(a) == true);
    assert(a == [2,1,1]);
    assert(nextPermutation(a) == false);
    assert(a == [1,1,2]);
}

@safe unittest
{
    // Boundary cases: arrays of 0 or 1 element.
    int[] a1 = [];
    assert(!nextPermutation(a1));
    assert(a1 == []);

    int[] a2 = [1];
    assert(!nextPermutation(a2));
    assert(a2 == [1]);
}

@safe unittest
{
    import std.algorithm.comparison : equal;

    auto a1 = [1, 2, 3, 4];

    assert(nextPermutation(a1));
    assert(equal(a1, [1, 2, 4, 3]));

    assert(nextPermutation(a1));
    assert(equal(a1, [1, 3, 2, 4]));

    assert(nextPermutation(a1));
    assert(equal(a1, [1, 3, 4, 2]));

    assert(nextPermutation(a1));
    assert(equal(a1, [1, 4, 2, 3]));

    assert(nextPermutation(a1));
    assert(equal(a1, [1, 4, 3, 2]));

    assert(nextPermutation(a1));
    assert(equal(a1, [2, 1, 3, 4]));

    assert(nextPermutation(a1));
    assert(equal(a1, [2, 1, 4, 3]));

    assert(nextPermutation(a1));
    assert(equal(a1, [2, 3, 1, 4]));

    assert(nextPermutation(a1));
    assert(equal(a1, [2, 3, 4, 1]));

    assert(nextPermutation(a1));
    assert(equal(a1, [2, 4, 1, 3]));

    assert(nextPermutation(a1));
    assert(equal(a1, [2, 4, 3, 1]));

    assert(nextPermutation(a1));
    assert(equal(a1, [3, 1, 2, 4]));

    assert(nextPermutation(a1));
    assert(equal(a1, [3, 1, 4, 2]));

    assert(nextPermutation(a1));
    assert(equal(a1, [3, 2, 1, 4]));

    assert(nextPermutation(a1));
    assert(equal(a1, [3, 2, 4, 1]));

    assert(nextPermutation(a1));
    assert(equal(a1, [3, 4, 1, 2]));

    assert(nextPermutation(a1));
    assert(equal(a1, [3, 4, 2, 1]));

    assert(nextPermutation(a1));
    assert(equal(a1, [4, 1, 2, 3]));

    assert(nextPermutation(a1));
    assert(equal(a1, [4, 1, 3, 2]));

    assert(nextPermutation(a1));
    assert(equal(a1, [4, 2, 1, 3]));

    assert(nextPermutation(a1));
    assert(equal(a1, [4, 2, 3, 1]));

    assert(nextPermutation(a1));
    assert(equal(a1, [4, 3, 1, 2]));

    assert(nextPermutation(a1));
    assert(equal(a1, [4, 3, 2, 1]));

    assert(!nextPermutation(a1));
    assert(equal(a1, [1, 2, 3, 4]));
}

@safe unittest
{
    // Test with non-default sorting order
    int[] a = [3,2,1];
    assert(nextPermutation!"a > b"(a) == true);
    assert(a == [3,1,2]);
    assert(nextPermutation!"a > b"(a) == true);
    assert(a == [2,3,1]);
    assert(nextPermutation!"a > b"(a) == true);
    assert(a == [2,1,3]);
    assert(nextPermutation!"a > b"(a) == true);
    assert(a == [1,3,2]);
    assert(nextPermutation!"a > b"(a) == true);
    assert(a == [1,2,3]);
    assert(nextPermutation!"a > b"(a) == false);
    assert(a == [3,2,1]);
}

// https://issues.dlang.org/show_bug.cgi?id=13594
@safe unittest
{
    int[3] a = [1,2,3];
    assert(nextPermutation(a[]));
    assert(a == [1,3,2]);
}

// nextEvenPermutation
/**
 * Permutes `range` in-place to the next lexicographically greater $(I even)
 * permutation.
 *
 * The predicate `less` defines the lexicographical ordering to be used on
 * the range.
 *
 * An even permutation is one which is produced by swapping an even number of
 * pairs of elements in the original range. The set of $(I even) permutations
 * is distinct from the set of $(I all) permutations only when there are no
 * duplicate elements in the range. If the range has $(I N) unique elements,
 * then there are exactly $(I N)!/2 even permutations.
 *
 * If the range is already the lexicographically greatest even permutation, it
 * is permuted back to the least even permutation and false is returned.
 * Otherwise, true is returned, and the range is modified in-place to be the
 * lexicographically next even permutation.
 *
 * One can thus generate the even permutations of a range with unique elements
 * by starting with the lexicographically smallest permutation, and repeatedly
 * calling nextEvenPermutation until it returns false.
----
// Enumerate even permutations
int[] a = [1,2,3,4,5];
do
{
    // use the current permutation and
    // proceed to the next even permutation of the array.
} while (nextEvenPermutation(a));
----
 * One can also generate the $(I odd) permutations of a range by noting that
 * permutations obey the rule that even + even = even, and odd + even = odd.
 * Thus, by swapping the last two elements of a lexicographically least range,
 * it is turned into the first odd permutation. Then calling
 * nextEvenPermutation on this first odd permutation will generate the next
 * even permutation relative to this odd permutation, which is actually the
 * next odd permutation of the original range. Thus, by repeatedly calling
 * nextEvenPermutation until it returns false, one enumerates the odd
 * permutations of the original range.
----
// Enumerate odd permutations
int[] a = [1,2,3,4,5];
swap(a[$-2], a[$-1]);    // a is now the first odd permutation of [1,2,3,4,5]
do
{
    // use the current permutation and
    // proceed to the next odd permutation of the original array
    // (which is an even permutation of the first odd permutation).
} while (nextEvenPermutation(a));
----
 *
 * Warning: Since even permutations are only distinct from all permutations
 * when the range elements are unique, this function assumes that there are no
 * duplicate elements under the specified ordering. If this is not _true, some
 * permutations may fail to be generated. When the range has non-unique
 * elements, you should use $(MYREF nextPermutation) instead.
 *
 * Params:
 *  less = The ordering to be used to determine lexicographical ordering of the
 *      permutations.
 *  range = The range to permute.
 *
 * Returns: false if the range was lexicographically the greatest, in which
 * case the range is reversed back to the lexicographically smallest
 * permutation; otherwise returns true.
 */
bool nextEvenPermutation(alias less="a < b", BidirectionalRange)
                        (BidirectionalRange range)
if (isBidirectionalRange!BidirectionalRange &&
    hasSwappableElements!BidirectionalRange)
{
    import std.algorithm.mutation : reverse, swap;
    import std.algorithm.searching : find;
    import std.range : retro, takeExactly;
    // Ranges of 0 or 1 element have no distinct permutations.
    if (range.empty) return false;

    bool oddParity = false;
    bool ret = true;
    do
    {
        auto i = retro(range);
        auto last = i.save;

        // Find last occurring increasing pair of elements
        size_t n = 1;
        for (i.popFront(); !i.empty;
            i.popFront(), last.popFront(), n++)
        {
            if (binaryFun!less(i.front, last.front))
                break;
        }

        if (!i.empty)
        {
            // Find last element greater than i.front.
            auto j = find!((a) => binaryFun!less(i.front, a))(
                           takeExactly(retro(range), n));

            // shouldn't happen since i.front < last.front
            assert(!j.empty, "j must not be empty");

            swap(i.front, j.front);
            oddParity = !oddParity;
        }
        else
        {
            // Entire range is decreasing: it's lexicographically
            // the greatest.
            ret = false;
        }

        reverse(takeExactly(retro(range), n));
        if ((n / 2) % 2 == 1)
            oddParity = !oddParity;
    } while (oddParity);

    return ret;
}

///
@safe unittest
{
    // Step through even permutations of a sorted array in lexicographic order
    int[] a = [1,2,3];
    assert(nextEvenPermutation(a) == true);
    assert(a == [2,3,1]);
    assert(nextEvenPermutation(a) == true);
    assert(a == [3,1,2]);
    assert(nextEvenPermutation(a) == false);
    assert(a == [1,2,3]);
}

@safe unittest
{
    auto a3 = [ 1, 2, 3, 4 ];
    int count = 1;
    while (nextEvenPermutation(a3)) count++;
    assert(count == 12);
}

@safe unittest
{
    // Test with non-default sorting order
    auto a = [ 3, 2, 1 ];

    assert(nextEvenPermutation!"a > b"(a) == true);
    assert(a == [ 2, 1, 3 ]);
    assert(nextEvenPermutation!"a > b"(a) == true);
    assert(a == [ 1, 3, 2 ]);
    assert(nextEvenPermutation!"a > b"(a) == false);
    assert(a == [ 3, 2, 1 ]);
}

@safe unittest
{
    // Test various cases of rollover
    auto a = [ 3, 1, 2 ];
    assert(nextEvenPermutation(a) == false);
    assert(a == [ 1, 2, 3 ]);

    auto b = [ 3, 2, 1 ];
    assert(nextEvenPermutation(b) == false);
    assert(b == [ 1, 3, 2 ]);
}

// https://issues.dlang.org/show_bug.cgi?id=13594
@safe unittest
{
    int[3] a = [1,2,3];
    assert(nextEvenPermutation(a[]));
    assert(a == [2,3,1]);
}

/**
Even permutations are useful for generating coordinates of certain geometric
shapes. Here's a non-trivial example:
*/
@safe unittest
{
    import std.math.algebraic : sqrt;

    // Print the 60 vertices of a uniform truncated icosahedron (soccer ball)
    enum real Phi = (1.0 + sqrt(5.0)) / 2.0;    // Golden ratio
    real[][] seeds = [
        [0.0, 1.0, 3.0*Phi],
        [1.0, 2.0+Phi, 2.0*Phi],
        [Phi, 2.0, Phi^^3]
    ];
    size_t n;
    foreach (seed; seeds)
    {
        // Loop over even permutations of each seed
        do
        {
            // Loop over all sign changes of each permutation
            size_t i;
            do
            {
                // Generate all possible sign changes
                for (i=0; i < seed.length; i++)
                {
                    if (seed[i] != 0.0)
                    {
                        seed[i] = -seed[i];
                        if (seed[i] < 0.0)
                            break;
                    }
                }
                n++;
            } while (i < seed.length);
        } while (nextEvenPermutation(seed));
    }
    assert(n == 60);
}

/**
Permutes `range` into the `perm` permutation.

The algorithm has a constant runtime complexity with respect to the number of
permutations created.
Due to the number of unique values of `ulong` only the first 21 elements of
`range` can be permuted. The rest of the range will therefore not be
permuted.
This algorithm uses the $(HTTP en.wikipedia.org/wiki/Lehmer_code, Lehmer
Code).

The algorithm works as follows:
$(D_CODE
    auto pem = [4,0,4,1,0,0,0]; // permutation 2982 in factorial
    auto src = [0,1,2,3,4,5,6]; // the range to permutate

    auto i = 0;                    // range index
    // range index iterates pem and src in sync
    // pem[i] + i is used as index into src
    // first src[pem[i] + i] is stored in t
    auto t = 4;                    // tmp value
    src = [0,1,2,3,n,5,6];

    // then the values between i and pem[i] + i are moved one
    // to the right
    src = [n,0,1,2,3,5,6];
    // at last t is inserted into position i
    src = [4,0,1,2,3,5,6];
    // finally i is incremented
    ++i;

    // this process is repeated while i < pem.length

    t = 0;
    src = [4,n,1,2,3,5,6];
    src = [4,0,1,2,3,5,6];
    ++i;
    t = 6;
    src = [4,0,1,2,3,5,n];
    src = [4,0,n,1,2,3,5];
    src = [4,0,6,1,2,3,5];
)

Returns:
    The permuted range.

Params:
    range = The Range to permute. The original ordering will be lost.
    perm = The permutation to permutate `range` to.
*/
auto ref Range nthPermutation(Range)
                             (auto ref Range range, const ulong perm)
if (isRandomAccessRange!Range && hasLength!Range)
{
    if (!nthPermutationImpl(range, perm))
    {
        throw new Exception(
            "The range to permutate must not have less"
            ~ " elements than the factorial number has digits");
    }

    return range;
}

///
pure @safe unittest
{
    auto src = [0, 1, 2, 3, 4, 5, 6];
    auto rslt = [4, 0, 6, 2, 1, 3, 5];

    src = nthPermutation(src, 2982);
    assert(src == rslt);
}

/**
Returns: `true` in case the permutation worked, `false` in case `perm` had
    more digits in the factorial number system than range had elements.
    This case must not occur as this would lead to out of range accesses.
*/
bool nthPermutationImpl(Range)
                       (auto ref Range range, ulong perm)
if (isRandomAccessRange!Range && hasLength!Range)
{
    import std.range.primitives : ElementType;
    import std.numeric : decimalToFactorial;

    // ulong.max has 21 digits in the factorial number system
    ubyte[21] fac;
    size_t idx = decimalToFactorial(perm, fac);

    if (idx > range.length)
    {
        return false;
    }

    ElementType!Range tmp;
    size_t i = 0;

    for (; i < idx; ++i)
    {
        size_t re = fac[i];
        tmp = range[re + i];
        for (size_t j = re + i; j > i; --j)
        {
            range[j] = range[j - 1];
        }
        range[i] = tmp;
    }

    return true;
}

///
pure @safe unittest
{
    auto src = [0, 1, 2, 3, 4, 5, 6];
    auto rslt = [4, 0, 6, 2, 1, 3, 5];

    bool worked = nthPermutationImpl(src, 2982);
    assert(worked);
    assert(src == rslt);
}

pure @safe unittest
{
    auto rslt = [4, 0, 6, 2, 1, 3, 5];

    auto src = nthPermutation([0, 1, 2, 3, 4, 5, 6], 2982);
    assert(src == rslt);
}

pure @safe unittest
{
    auto src = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    auto rslt = [4, 0, 6, 2, 1, 3, 5, 7, 8, 9, 10];

    src = nthPermutation(src, 2982);
    assert(src == rslt);
}

pure @safe unittest
{
    import std.exception : assertThrown;

    auto src = [0, 1, 2, 3];

    assertThrown(nthPermutation(src, 2982));
}

pure @safe unittest
{
    import std.internal.test.dummyrange;
    import std.meta : AliasSeq;

    auto src = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    auto rsl = [4, 0, 6, 2, 1, 3, 5, 7, 8, 9, 10];

    foreach (T; AliasSeq!(
            DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random, int[]),
            DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Random, int[])))
    {
        static assert(isRandomAccessRange!(T));
        static assert(hasLength!(T));
        auto dr = T(src.dup);
        dr = nthPermutation(dr, 2982);

        int idx;
        foreach (it; dr)
        {
            assert(it == rsl[idx++]);
        }
    }
}
