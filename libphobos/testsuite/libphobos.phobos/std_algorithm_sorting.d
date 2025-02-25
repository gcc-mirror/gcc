@safe unittest
{
    import std.algorithm.sorting;

    import std.range : assumeSorted;
    int[] a = [ 1, 2, 3 ];
    int[] b = [ 4, 0, 6, 5 ];
    completeSort(assumeSorted(a), b);
    assert(a == [ 0, 1, 2 ]);
    assert(b == [ 3, 4, 5, 6 ]);
}

@safe unittest
{
    import std.algorithm.sorting;

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
    import std.algorithm.sorting;

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

@safe unittest
{
    import std.algorithm.sorting;

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

@safe nothrow unittest
{
    import std.algorithm.sorting;

    int[] a = [5, 3, 2, 6, 4, 1, 3, 7];
    size_t pivot = pivotPartition(a, a.length / 2);
    import std.algorithm.searching : all;
    assert(a[0 .. pivot].all!(x => x <= a[pivot]));
    assert(a[pivot .. $].all!(x => x >= a[pivot]));
}

@safe unittest
{
    import std.algorithm.sorting;

    int[] r = [ 1, 3, 5, 7, 8, 2, 4, ];
    assert(isPartitioned!"a & 1"(r));
}

@safe unittest
{
    import std.algorithm.sorting;

    auto a = [ 8, 3, 4, 1, 4, 7, 4 ];
    auto pieces = partition3(a, 4);
    assert(pieces[0] == [ 1, 3 ]);
    assert(pieces[1] == [ 4, 4, 4 ]);
    assert(pieces[2] == [ 8, 7 ]);
}

@system unittest
{
    import std.algorithm.sorting;

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

@safe pure nothrow unittest
{
    import std.algorithm.sorting;

    import std.algorithm.comparison : equal;
    import std.range : retro;

    int[] a = [1, 3, 5];
    int[] b = [2, 3, 4];

    assert(a.merge(b).equal([1, 2, 3, 3, 4, 5]));
    assert(a.merge(b).retro.equal([5, 4, 3, 3, 2, 1]));
}

@safe pure nothrow unittest
{
    import std.algorithm.sorting;

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

@safe unittest
{
    import std.algorithm.sorting;

    import std.algorithm.mutation : SwapStrategy;
    static struct Point { int x, y; }
    auto pts1 = [ Point(0, 0), Point(5, 5), Point(0, 1), Point(0, 2) ];
    auto pts2 = [ Point(0, 0), Point(0, 1), Point(0, 2), Point(5, 5) ];
    multiSort!("a.x < b.x", "a.y < b.y", SwapStrategy.unstable)(pts1);
    assert(pts1 == pts2);
}

@safe pure nothrow unittest
{
    import std.algorithm.sorting;

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

@safe unittest
{
    import std.algorithm.sorting;

    // Showcase stable sorting
    import std.algorithm.mutation : SwapStrategy;
    string[] words = [ "aBc", "a", "abc", "b", "ABC", "c" ];
    sort!("toUpper(a) < toUpper(b)", SwapStrategy.stable)(words);
    assert(words == [ "a", "aBc", "abc", "ABC", "b", "c" ]);
}

@safe unittest
{
    import std.algorithm.sorting;

    // Sorting floating-point numbers in presence of NaN
    double[] numbers = [-0.0, 3.0, -2.0, double.nan, 0.0, -double.nan];

    import std.algorithm.comparison : equal;
    import std.math.operations : cmp;
    import std.math.traits : isIdentical;

    sort!((a, b) => cmp(a, b) < 0)(numbers);

    double[] sorted = [-double.nan, -2.0, -0.0, 0.0, 3.0, double.nan];
    assert(numbers.equal!isIdentical(sorted));
}

@safe pure unittest
{
    import std.algorithm.sorting;

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

@system unittest
{
    import std.algorithm.sorting;

    int[] a = [ 9, 8, 7, 6, 5, 4, 3, 2, 1, 0 ];
    partialSort(a, 5);
    assert(a[0 .. 5] == [ 0, 1, 2, 3, 4 ]);
}

@system unittest
{
    import std.algorithm.sorting;

    int[] a = [5, 7, 2, 6, 7];
    int[] b = [2, 1, 5, 6, 7, 3, 0];

    partialSort(a, b);
    assert(a == [0, 1, 2, 2, 3]);
}

@safe unittest
{
    import std.algorithm.sorting;

    int[] v = [ 25, 7, 9, 2, 0, 5, 21 ];
    topN!"a < b"(v, 100);
    assert(v == [ 25, 7, 9, 2, 0, 5, 21 ]);
    auto n = 4;
    topN!((a, b) => a < b)(v, n);
    assert(v[n] == 9);
}

@system unittest
{
    import std.algorithm.sorting;

    int[] a = [ 5, 7, 2, 6, 7 ];
    int[] b = [ 2, 1, 5, 6, 7, 3, 0 ];
    topN(a, b);
    sort(a);
    assert(a == [0, 1, 2, 2, 3]);
}

@system unittest
{
    import std.algorithm.sorting;

    import std.typecons : Yes;

    int[] a = [ 10, 16, 2, 3, 1, 5, 0 ];
    int[] b = new int[3];
    topNCopy(a, b, Yes.sortOutput);
    assert(b == [ 0, 1, 2 ]);
}

@system unittest
{
    import std.algorithm.sorting;

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

@safe unittest
{
    import std.algorithm.sorting;

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

@safe unittest
{
    import std.algorithm.sorting;

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
    import std.algorithm.sorting;

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
    import std.algorithm.sorting;

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

pure @safe unittest
{
    import std.algorithm.sorting;

    auto src = [0, 1, 2, 3, 4, 5, 6];
    auto rslt = [4, 0, 6, 2, 1, 3, 5];

    src = nthPermutation(src, 2982);
    assert(src == rslt);
}

pure @safe unittest
{
    import std.algorithm.sorting;

    auto src = [0, 1, 2, 3, 4, 5, 6];
    auto rslt = [4, 0, 6, 2, 1, 3, 5];

    bool worked = nthPermutationImpl(src, 2982);
    assert(worked);
    assert(src == rslt);
}

