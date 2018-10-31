// Written in the D programming language.

/**
This package implements generic algorithms oriented towards the processing of
sequences. Sequences processed by these functions define range-based
interfaces.  See also $(MREF_ALTTEXT Reference on ranges, std, range) and
$(HTTP ddili.org/ders/d.en/ranges.html, tutorial on ranges).

$(SCRIPT inhibitQuickIndex = 1;)

Algorithms are categorized into the following submodules:

$(DIVC quickindex,
$(BOOKTABLE ,
$(TR $(TH Submodule) $(TH Functions)
)
$(TR
     $(TDNW $(SUBMODULE Searching, searching))
     $(TD
        $(SUBREF searching, all)
        $(SUBREF searching, any)
        $(SUBREF searching, balancedParens)
        $(SUBREF searching, boyerMooreFinder)
        $(SUBREF searching, canFind)
        $(SUBREF searching, commonPrefix)
        $(SUBREF searching, count)
        $(SUBREF searching, countUntil)
        $(SUBREF searching, endsWith)
        $(SUBREF searching, find)
        $(SUBREF searching, findAdjacent)
        $(SUBREF searching, findAmong)
        $(SUBREF searching, findSkip)
        $(SUBREF searching, findSplit)
        $(SUBREF searching, findSplitAfter)
        $(SUBREF searching, findSplitBefore)
        $(SUBREF searching, minCount)
        $(SUBREF searching, maxCount)
        $(SUBREF searching, minElement)
        $(SUBREF searching, maxElement)
        $(SUBREF searching, minIndex)
        $(SUBREF searching, maxIndex)
        $(SUBREF searching, minPos)
        $(SUBREF searching, maxPos)
        $(SUBREF searching, skipOver)
        $(SUBREF searching, startsWith)
        $(SUBREF searching, until)
    )
)
$(TR
    $(TDNW $(SUBMODULE Comparison, comparison))
    $(TD
        $(SUBREF comparison, among)
        $(SUBREF comparison, castSwitch)
        $(SUBREF comparison, clamp)
        $(SUBREF comparison, cmp)
        $(SUBREF comparison, either)
        $(SUBREF comparison, equal)
        $(SUBREF comparison, isPermutation)
        $(SUBREF comparison, isSameLength)
        $(SUBREF comparison, levenshteinDistance)
        $(SUBREF comparison, levenshteinDistanceAndPath)
        $(SUBREF comparison, max)
        $(SUBREF comparison, min)
        $(SUBREF comparison, mismatch)
        $(SUBREF comparison, predSwitch)
    )
)
$(TR
    $(TDNW $(SUBMODULE Iteration, iteration))
    $(TD
        $(SUBREF iteration, cache)
        $(SUBREF iteration, cacheBidirectional)
        $(SUBREF iteration, chunkBy)
        $(SUBREF iteration, cumulativeFold)
        $(SUBREF iteration, each)
        $(SUBREF iteration, filter)
        $(SUBREF iteration, filterBidirectional)
        $(SUBREF iteration, fold)
        $(SUBREF iteration, group)
        $(SUBREF iteration, joiner)
        $(SUBREF iteration, map)
        $(SUBREF iteration, permutations)
        $(SUBREF iteration, reduce)
        $(SUBREF iteration, splitter)
        $(SUBREF iteration, sum)
        $(SUBREF iteration, uniq)
    )
)
$(TR
    $(TDNW $(SUBMODULE Sorting, sorting))
    $(TD
        $(SUBREF sorting, completeSort)
        $(SUBREF sorting, isPartitioned)
        $(SUBREF sorting, isSorted)
        $(SUBREF sorting, isStrictlyMonotonic)
        $(SUBREF sorting, ordered)
        $(SUBREF sorting, strictlyOrdered)
        $(SUBREF sorting, makeIndex)
        $(SUBREF sorting, merge)
        $(SUBREF sorting, multiSort)
        $(SUBREF sorting, nextEvenPermutation)
        $(SUBREF sorting, nextPermutation)
        $(SUBREF sorting, partialSort)
        $(SUBREF sorting, partition)
        $(SUBREF sorting, partition3)
        $(SUBREF sorting, schwartzSort)
        $(SUBREF sorting, sort)
        $(SUBREF sorting, topN)
        $(SUBREF sorting, topNCopy)
        $(SUBREF sorting, topNIndex)
    )
)
$(TR
    $(TDNW Set operations $(BR)($(SUBMODULE setops, setops)))
    $(TD
        $(SUBREF setops, cartesianProduct)
        $(SUBREF setops, largestPartialIntersection)
        $(SUBREF setops, largestPartialIntersectionWeighted)
        $(SUBREF setops, multiwayMerge)
        $(SUBREF setops, multiwayUnion)
        $(SUBREF setops, setDifference)
        $(SUBREF setops, setIntersection)
        $(SUBREF setops, setSymmetricDifference)
    )
)
$(TR
    $(TDNW $(SUBMODULE Mutation, mutation))
    $(TD
        $(SUBREF mutation, bringToFront)
        $(SUBREF mutation, copy)
        $(SUBREF mutation, fill)
        $(SUBREF mutation, initializeAll)
        $(SUBREF mutation, move)
        $(SUBREF mutation, moveAll)
        $(SUBREF mutation, moveSome)
        $(SUBREF mutation, moveEmplace)
        $(SUBREF mutation, moveEmplaceAll)
        $(SUBREF mutation, moveEmplaceSome)
        $(SUBREF mutation, remove)
        $(SUBREF mutation, reverse)
        $(SUBREF mutation, strip)
        $(SUBREF mutation, stripLeft)
        $(SUBREF mutation, stripRight)
        $(SUBREF mutation, swap)
        $(SUBREF mutation, swapRanges)
        $(SUBREF mutation, uninitializedFill)
    )
)
))

Many functions in this package are parameterized with a $(GLOSSARY predicate).
The predicate may be any suitable callable type
(a function, a delegate, a $(GLOSSARY functor), or a lambda), or a
compile-time string. The string may consist of $(B any) legal D
expression that uses the symbol $(D a) (for unary functions) or the
symbols $(D a) and $(D b) (for binary functions). These names will NOT
interfere with other homonym symbols in user code because they are
evaluated in a different context. The default for all binary
comparison predicates is $(D "a == b") for unordered operations and
$(D "a < b") for ordered operations.

Example:

----
int[] a = ...;
static bool greater(int a, int b)
{
    return a > b;
}
sort!greater(a);           // predicate as alias
sort!((a, b) => a > b)(a); // predicate as a lambda.
sort!"a > b"(a);           // predicate as string
                           // (no ambiguity with array name)
sort(a);                   // no predicate, "a < b" is implicit
----

Macros:
SUBMODULE = $(MREF_ALTTEXT $1, std, algorithm, $2)
SUBREF = $(REF_ALTTEXT $(TT $2), $2, std, algorithm, $1)$(NBSP)

Copyright: Andrei Alexandrescu 2008-.

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP erdani.com, Andrei Alexandrescu)

Source: $(PHOBOSSRC std/_algorithm/package.d)
 */
module std.algorithm;

public import std.algorithm.comparison;
public import std.algorithm.iteration;
public import std.algorithm.mutation;
public import std.algorithm.searching;
public import std.algorithm.setops;
public import std.algorithm.sorting;

static import std.functional;
