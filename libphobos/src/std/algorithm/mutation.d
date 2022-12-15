// Written in the D programming language.
/**
This is a submodule of $(MREF std, algorithm).
It contains generic mutation algorithms.

$(SCRIPT inhibitQuickIndex = 1;)
$(BOOKTABLE Cheat Sheet,
$(TR $(TH Function Name) $(TH Description))
$(T2 bringToFront,
        If `a = [1, 2, 3]` and `b = [4, 5, 6, 7]`,
        `bringToFront(a, b)` leaves `a = [4, 5, 6]` and
        `b = [7, 1, 2, 3]`.)
$(T2 copy,
        Copies a range to another. If
        `a = [1, 2, 3]` and `b = new int[5]`, then `copy(a, b)`
        leaves `b = [1, 2, 3, 0, 0]` and returns `b[3 .. $]`.)
$(T2 fill,
        Fills a range with a pattern,
        e.g., if `a = new int[3]`, then `fill(a, 4)`
        leaves `a = [4, 4, 4]` and `fill(a, [3, 4])` leaves
        `a = [3, 4, 3]`.)
$(T2 initializeAll,
        If `a = [1.2, 3.4]`, then `initializeAll(a)` leaves
        `a = [double.init, double.init]`.)
$(T2 move,
        `move(a, b)` moves `a` into `b`. `move(a)` reads `a`
        destructively when necessary.)
$(T2 moveEmplace,
        Similar to `move` but assumes `target` is uninitialized.)
$(T2 moveAll,
        Moves all elements from one range to another.)
$(T2 moveEmplaceAll,
        Similar to `moveAll` but assumes all elements in `target` are uninitialized.)
$(T2 moveSome,
        Moves as many elements as possible from one range to another.)
$(T2 moveEmplaceSome,
        Similar to `moveSome` but assumes all elements in `target` are uninitialized.)
$(T2 remove,
        Removes elements from a range in-place, and returns the shortened
        range.)
$(T2 reverse,
        If `a = [1, 2, 3]`, `reverse(a)` changes it to `[3, 2, 1]`.)
$(T2 strip,
        Strips all leading and trailing elements equal to a value, or that
        satisfy a predicate.
        If `a = [1, 1, 0, 1, 1]`, then `strip(a, 1)` and
        `strip!(e => e == 1)(a)` returns `[0]`.)
$(T2 stripLeft,
        Strips all leading elements equal to a value, or that satisfy a
        predicate.  If `a = [1, 1, 0, 1, 1]`, then `stripLeft(a, 1)` and
        `stripLeft!(e => e == 1)(a)` returns `[0, 1, 1]`.)
$(T2 stripRight,
        Strips all trailing elements equal to a value, or that satisfy a
        predicate.
        If `a = [1, 1, 0, 1, 1]`, then `stripRight(a, 1)` and
        `stripRight!(e => e == 1)(a)` returns `[1, 1, 0]`.)
$(T2 swap,
        Swaps two values.)
$(T2 swapAt,
        Swaps two values by indices.)
$(T2 swapRanges,
        Swaps all elements of two ranges.)
$(T2 uninitializedFill,
        Fills a range (assumed uninitialized) with a value.)
)

Copyright: Andrei Alexandrescu 2008-.

License: $(HTTP boost.org/LICENSE_1_0.txt, Boost License 1.0).

Authors: $(HTTP erdani.com, Andrei Alexandrescu)

Source: $(PHOBOSSRC std/algorithm/mutation.d)

Macros:
T2=$(TR $(TDNW $(LREF $1)) $(TD $+))
 */
module std.algorithm.mutation;

import std.range.primitives;
import std.traits : isArray, isAssignable, isBlitAssignable, isNarrowString,
       Unqual, isSomeChar, isMutable;
import std.meta : allSatisfy;
import std.typecons : tuple, Tuple;

// bringToFront
/**
`bringToFront` takes two ranges `front` and `back`, which may
be of different types. Considering the concatenation of `front` and
`back` one unified range, `bringToFront` rotates that unified
range such that all elements in `back` are brought to the beginning
of the unified range. The relative ordering of elements in `front`
and `back`, respectively, remains unchanged.

The `bringToFront` function treats strings at the code unit
level and it is not concerned with Unicode character integrity.
`bringToFront` is designed as a function for moving elements
in ranges, not as a string function.

Performs $(BIGOH max(front.length, back.length)) evaluations of $(D
swap).

The `bringToFront` function can rotate elements in one buffer left or right, swap
buffers of equal length, and even move elements across disjoint
buffers of different types and different lengths.

Preconditions:

Either `front` and `back` are disjoint, or `back` is
reachable from `front` and `front` is not reachable from $(D
back).

Params:
    front = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    back = a $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)

Returns:
    The number of elements brought to the front, i.e., the length of `back`.

See_Also:
    $(LINK2 http://en.cppreference.com/w/cpp/algorithm/rotate, STL's `rotate`)
*/
size_t bringToFront(InputRange, ForwardRange)(InputRange front, ForwardRange back)
if (isInputRange!InputRange && isForwardRange!ForwardRange)
{
    import std.string : representation;

    static if (isNarrowString!InputRange)
    {
        auto frontW = representation(front);
    }
    else
    {
        alias frontW = front;
    }
    static if (isNarrowString!ForwardRange)
    {
        auto backW = representation(back);
    }
    else
    {
        alias backW = back;
    }

    return bringToFrontImpl(frontW, backW);
}

/**
The simplest use of `bringToFront` is for rotating elements in a
buffer. For example:
*/
@safe unittest
{
    auto arr = [4, 5, 6, 7, 1, 2, 3];
    auto p = bringToFront(arr[0 .. 4], arr[4 .. $]);
    assert(p == arr.length - 4);
    assert(arr == [ 1, 2, 3, 4, 5, 6, 7 ]);
}

/**
The `front` range may actually "step over" the `back`
range. This is very useful with forward ranges that cannot compute
comfortably right-bounded subranges like `arr[0 .. 4]` above. In
the example below, `r2` is a right subrange of `r1`.
*/
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.container : SList;
    import std.range.primitives : popFrontN;

    auto list = SList!(int)(4, 5, 6, 7, 1, 2, 3);
    auto r1 = list[];
    auto r2 = list[]; popFrontN(r2, 4);
    assert(equal(r2, [ 1, 2, 3 ]));
    bringToFront(r1, r2);
    assert(equal(list[], [ 1, 2, 3, 4, 5, 6, 7 ]));
}

/**
Elements can be swapped across ranges of different types:
*/
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.container : SList;

    auto list = SList!(int)(4, 5, 6, 7);
    auto vec = [ 1, 2, 3 ];
    bringToFront(list[], vec);
    assert(equal(list[], [ 1, 2, 3, 4 ]));
    assert(equal(vec, [ 5, 6, 7 ]));
}

/**
Unicode integrity is not preserved:
*/
@safe unittest
{
    import std.string : representation;
    auto ar = representation("a".dup);
    auto br = representation("ç".dup);

    bringToFront(ar, br);

    auto a = cast(char[]) ar;
    auto b = cast(char[]) br;

    // Illegal UTF-8
    assert(a == "\303");
    // Illegal UTF-8
    assert(b == "\247a");
}

private size_t bringToFrontImpl(InputRange, ForwardRange)(InputRange front, ForwardRange back)
if (isInputRange!InputRange && isForwardRange!ForwardRange)
{
    import std.array : sameHead;
    import std.range : take, Take;
    enum bool sameHeadExists = is(typeof(front.sameHead(back)));
    size_t result;

    for (bool semidone; !front.empty && !back.empty; )
    {
        static if (sameHeadExists)
        {
            if (front.sameHead(back)) break; // shortcut
        }
        // Swap elements until front and/or back ends.
        auto back0 = back.save;
        size_t nswaps;
        do
        {
            static if (sameHeadExists)
            {
                // Detect the stepping-over condition.
                if (front.sameHead(back0)) back0 = back.save;
            }
            swapFront(front, back);
            ++nswaps;
            front.popFront();
            back.popFront();
        }
        while (!front.empty && !back.empty);

        if (!semidone) result += nswaps;

        // Now deal with the remaining elements.
        if (back.empty)
        {
            if (front.empty) break;
            // Right side was shorter, which means that we've brought
            // all the back elements to the front.
            semidone = true;
            // Next pass: bringToFront(front, back0) to adjust the rest.
            back = back0;
        }
        else
        {
            assert(front.empty, "Expected front to be empty");
            // Left side was shorter. Let's step into the back.
            static if (is(InputRange == Take!ForwardRange))
            {
                front = take(back0, nswaps);
            }
            else
            {
                immutable subresult = bringToFront(take(back0, nswaps),
                                                   back);
                if (!semidone) result += subresult;
                break; // done
            }
        }
    }
    return result;
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : text;
    import std.random : Random = Xorshift, uniform;

    // a more elaborate test
    {
        auto rnd = Random(123_456_789);
        int[] a = new int[uniform(100, 200, rnd)];
        int[] b = new int[uniform(100, 200, rnd)];
        foreach (ref e; a) e = uniform(-100, 100, rnd);
        foreach (ref e; b) e = uniform(-100, 100, rnd);
        int[] c = a ~ b;
        // writeln("a= ", a);
        // writeln("b= ", b);
        auto n = bringToFront(c[0 .. a.length], c[a.length .. $]);
        //writeln("c= ", c);
        assert(n == b.length);
        assert(c == b ~ a, text(c, "\n", a, "\n", b));
    }
    // different types, moveFront, no sameHead
    {
        static struct R(T)
        {
            T[] data;
            size_t i;
            @property
            {
                R save() { return this; }
                bool empty() { return i >= data.length; }
                T front() { return data[i]; }
                T front(real e) { return data[i] = cast(T) e; }
            }
            void popFront() { ++i; }
        }
        auto a = R!int([1, 2, 3, 4, 5]);
        auto b = R!real([6, 7, 8, 9]);
        auto n = bringToFront(a, b);
        assert(n == 4);
        assert(a.data == [6, 7, 8, 9, 1]);
        assert(b.data == [2, 3, 4, 5]);
    }
    // front steps over back
    {
        int[] arr, r1, r2;

        // back is shorter
        arr = [4, 5, 6, 7, 1, 2, 3];
        r1 = arr;
        r2 = arr[4 .. $];
        bringToFront(r1, r2) == 3 || assert(0);
        assert(equal(arr, [1, 2, 3, 4, 5, 6, 7]));

        // front is shorter
        arr = [5, 6, 7, 1, 2, 3, 4];
        r1 = arr;
        r2 = arr[3 .. $];
        bringToFront(r1, r2) == 4 || assert(0);
        assert(equal(arr, [1, 2, 3, 4, 5, 6, 7]));
    }

    // https://issues.dlang.org/show_bug.cgi?id=16959
    auto arr = ['4', '5', '6', '7', '1', '2', '3'];
    auto p = bringToFront(arr[0 .. 4], arr[4 .. $]);

    assert(p == arr.length - 4);
    assert(arr == ['1', '2', '3', '4', '5', '6', '7']);
}

// Tests if types are arrays and support slice assign.
private enum bool areCopyCompatibleArrays(T1, T2) =
    isArray!T1 && isArray!T2 && is(typeof(T2.init[] = T1.init[]));

// copy
/**
Copies the content of `source` into `target` and returns the
remaining (unfilled) part of `target`.

Preconditions: `target` shall have enough room to accommodate
the entirety of `source`.

Params:
    source = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    target = an output range

Returns:
    The unfilled part of target
 */
TargetRange copy(SourceRange, TargetRange)(SourceRange source, TargetRange target)
if (isInputRange!SourceRange && isOutputRange!(TargetRange, ElementType!SourceRange))
{
    static if (areCopyCompatibleArrays!(SourceRange, TargetRange))
    {
        const tlen = target.length;
        const slen = source.length;
        assert(tlen >= slen,
                "Cannot copy a source range into a smaller target range.");

        immutable overlaps = () @trusted {
            return source.ptr < target.ptr + tlen &&
                target.ptr < source.ptr + slen; }();

        if (overlaps)
        {
            if (source.ptr < target.ptr)
            {
                foreach_reverse (idx; 0 .. slen)
                    target[idx] = source[idx];
            }
            else
            {
                foreach (idx; 0 .. slen)
                    target[idx] = source[idx];
            }
            return target[slen .. tlen];
        }
        else
        {
            // Array specialization.  This uses optimized memory copying
            // routines under the hood and is about 10-20x faster than the
            // generic implementation.
            target[0 .. slen] = source[];
            return target[slen .. $];
        }
    }
    else
    {
        // Specialize for 2 random access ranges.
        // Typically 2 random access ranges are faster iterated by common
        // index than by x.popFront(), y.popFront() pair
        static if (isRandomAccessRange!SourceRange &&
                hasLength!SourceRange &&
                hasSlicing!TargetRange &&
                isRandomAccessRange!TargetRange &&
                hasLength!TargetRange)
        {
            auto len = source.length;
            foreach (idx; 0 .. len)
                target[idx] = source[idx];
            return target[len .. target.length];
        }
        else
        {
            foreach (element; source)
                put(target, element);
            return target;
        }
    }
}

///
@safe unittest
{
    int[] a = [ 1, 5 ];
    int[] b = [ 9, 8 ];
    int[] buf = new int[](a.length + b.length + 10);
    auto rem = a.copy(buf);    // copy a into buf
    rem = b.copy(rem);         // copy b into remainder of buf
    assert(buf[0 .. a.length + b.length] == [1, 5, 9, 8]);
    assert(rem.length == 10);   // unused slots in buf
}

/**
As long as the target range elements support assignment from source
range elements, different types of ranges are accepted:
*/
@safe unittest
{
    float[] src = [ 1.0f, 5 ];
    double[] dest = new double[src.length];
    src.copy(dest);
}

/**
To _copy at most `n` elements from a range, you may want to use
$(REF take, std,range):
*/
@safe unittest
{
    import std.range;
    int[] src = [ 1, 5, 8, 9, 10 ];
    auto dest = new int[](3);
    src.take(dest.length).copy(dest);
    assert(dest == [ 1, 5, 8 ]);
}

/**
To _copy just those elements from a range that satisfy a predicate,
use $(LREF filter):
*/
@safe unittest
{
    import std.algorithm.iteration : filter;
    int[] src = [ 1, 5, 8, 9, 10, 1, 2, 0 ];
    auto dest = new int[src.length];
    auto rem = src
        .filter!(a => (a & 1) == 1)
        .copy(dest);
    assert(dest[0 .. $ - rem.length] == [ 1, 5, 9, 1 ]);
}

/**
$(REF retro, std,range) can be used to achieve behavior similar to
$(LINK2 http://en.cppreference.com/w/cpp/algorithm/copy_backward, STL's `copy_backward`'):
*/
@safe unittest
{
    import std.algorithm, std.range;
    int[] src = [1, 2, 4];
    int[] dest = [0, 0, 0, 0, 0];
    src.retro.copy(dest.retro);
    assert(dest == [0, 0, 1, 2, 4]);
}

// Test CTFE copy.
@safe unittest
{
    enum c = copy([1,2,3], [4,5,6,7]);
    assert(c == [7]);
}


@safe unittest
{
    import std.algorithm.iteration : filter;

    {
        int[] a = [ 1, 5 ];
        int[] b = [ 9, 8 ];
        auto e = copy(filter!("a > 1")(a), b);
        assert(b[0] == 5 && e.length == 1);
    }

    {
        int[] a = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        copy(a[5 .. 10], a[4 .. 9]);
        assert(a[4 .. 9] == [6, 7, 8, 9, 10]);
    }

    // https://issues.dlang.org/show_bug.cgi?id=21724
    {
        int[] a = [1, 2, 3, 4];
        copy(a[0 .. 2], a[1 .. 3]);
        assert(a == [1, 1, 2, 4]);
    }

    // https://issues.dlang.org/show_bug.cgi?id=7898
    {
        enum v =
        {
            import std.algorithm;
            int[] arr1 = [10, 20, 30, 40, 50];
            int[] arr2 = arr1.dup;
            copy(arr1, arr2);
            return 35;
        }();
        assert(v == 35);
    }
}

// https://issues.dlang.org/show_bug.cgi?id=13650
@safe unittest
{
    import std.meta : AliasSeq;
    static foreach (Char; AliasSeq!(char, wchar, dchar))
    {{
        Char[3] a1 = "123";
        Char[6] a2 = "456789";
        assert(copy(a1[], a2[]) is a2[3..$]);
        assert(a1[] == "123");
        assert(a2[] == "123789");
    }}
}

// https://issues.dlang.org/show_bug.cgi?id=18804
@safe unittest
{
    static struct NullSink
    {
        void put(E)(E) {}
    }
    int line = 0;
    struct R
    {
        int front;
        @property bool empty() { return line == 1; }
        void popFront() { line = 1; }
    }
    R r;
    copy(r, NullSink());
    assert(line == 1);
}

/**
Assigns `value` to each element of input range `range`.

Alternatively, instead of using a single `value` to fill the `range`,
a `filler` $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
can be provided. The length of `filler` and `range` do not need to match, but
`filler` must not be empty.

Params:
        range = An
                $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
                that exposes references to its elements and has assignable
                elements
        value = Assigned to each element of range
        filler = A
                $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)
                representing the _fill pattern.

Throws: If `filler` is empty.

See_Also:
        $(LREF uninitializedFill)
        $(LREF initializeAll)
 */
void fill(Range, Value)(auto ref Range range, auto ref Value value)
if ((isInputRange!Range && is(typeof(range.front = value)) ||
    isSomeChar!Value && is(typeof(range[] = value))))
{
    alias T = ElementType!Range;

    static if (is(typeof(range[] = value)))
    {
        range[] = value;
    }
    else static if (is(typeof(range[] = T(value))))
    {
        range[] = T(value);
    }
    else
    {
        for ( ; !range.empty; range.popFront() )
        {
            range.front = value;
        }
    }
}

///
@safe unittest
{
    int[] a = [ 1, 2, 3, 4 ];
    fill(a, 5);
    assert(a == [ 5, 5, 5, 5 ]);
}

// test fallback on mutable narrow strings
// https://issues.dlang.org/show_bug.cgi?id=16342
@safe unittest
{
    char[] chars = ['a', 'b'];
    fill(chars, 'c');
    assert(chars == "cc");

    char[2] chars2 = ['a', 'b'];
    fill(chars2, 'c');
    assert(chars2 == "cc");

    wchar[] wchars = ['a', 'b'];
    fill(wchars, wchar('c'));
    assert(wchars == "cc"w);

    dchar[] dchars = ['a', 'b'];
    fill(dchars, dchar('c'));
    assert(dchars == "cc"d);
}

@nogc @safe unittest
{
    const(char)[] chars;
    assert(chars.length == 0);
    static assert(!__traits(compiles, fill(chars, 'c')));
    wstring wchars;
    assert(wchars.length == 0);
    static assert(!__traits(compiles, fill(wchars, wchar('c'))));
}

@nogc @safe unittest
{
    char[] chars;
    fill(chars, 'c');
    assert(chars == ""c);
}

@safe unittest
{
    shared(char)[] chrs = ['r'];
    fill(chrs, 'c');
    assert(chrs == [shared(char)('c')]);
}

@nogc @safe unittest
{
    struct Str(size_t len)
    {
        private char[len] _data;
        void opIndexAssign(char value) @safe @nogc
        {_data[] = value;}
    }
    Str!2 str;
    str.fill(':');
    assert(str._data == "::");
}

@safe unittest
{
    char[] chars = ['a','b','c','d'];
    chars[1 .. 3].fill(':');
    assert(chars == "a::d");
}
// end https://issues.dlang.org/show_bug.cgi?id=16342

@safe unittest
{
    import std.conv : text;
    import std.internal.test.dummyrange;

    int[] a = [ 1, 2, 3 ];
    fill(a, 6);
    assert(a == [ 6, 6, 6 ], text(a));

    void fun0()
    {
        foreach (i; 0 .. 1000)
        {
            foreach (ref e; a) e = 6;
        }
    }
    void fun1() { foreach (i; 0 .. 1000) fill(a, 6); }

    // fill should accept InputRange
    alias InputRange = DummyRange!(ReturnBy.Reference, Length.No, RangeType.Input);
    enum filler = uint.max;
    InputRange range;
    fill(range, filler);
    foreach (value; range.arr)
        assert(value == filler);
}

@safe unittest
{
    //ER8638_1 IS_NOT self assignable
    static struct ER8638_1
    {
        void opAssign(int){}
    }

    //ER8638_1 IS self assignable
    static struct ER8638_2
    {
        void opAssign(ER8638_2){}
        void opAssign(int){}
    }

    auto er8638_1 = new ER8638_1[](10);
    auto er8638_2 = new ER8638_2[](10);
    er8638_1.fill(5); //generic case
    er8638_2.fill(5); //opSlice(T.init) case
}

@safe unittest
{
    {
        int[] a = [1, 2, 3];
        immutable(int) b = 0;
        a.fill(b);
        assert(a == [0, 0, 0]);
    }
    {
        double[] a = [1, 2, 3];
        immutable(int) b = 0;
        a.fill(b);
        assert(a == [0, 0, 0]);
    }
}

/// ditto
void fill(InputRange, ForwardRange)(InputRange range, ForwardRange filler)
if (isInputRange!InputRange
    && (isForwardRange!ForwardRange
    || (isInputRange!ForwardRange && isInfinite!ForwardRange))
    && is(typeof(InputRange.init.front = ForwardRange.init.front)))
{
    static if (isInfinite!ForwardRange)
    {
        //ForwardRange is infinite, no need for bounds checking or saving
        static if (hasSlicing!ForwardRange && hasLength!InputRange
            && is(typeof(filler[0 .. range.length])))
        {
            copy(filler[0 .. range.length], range);
        }
        else
        {
            //manual feed
            for ( ; !range.empty; range.popFront(), filler.popFront())
            {
                range.front = filler.front;
            }
        }
    }
    else
    {
        import std.exception : enforce;

        enforce(!filler.empty, "Cannot fill range with an empty filler");

        static if (hasLength!InputRange && hasLength!ForwardRange
            && is(typeof(range.length > filler.length)))
        {
            //Case we have access to length
            immutable len = filler.length;
            //Start by bulk copies
            while (range.length > len)
            {
                range = copy(filler.save, range);
            }

            //and finally fill the partial range. No need to save here.
            static if (hasSlicing!ForwardRange && is(typeof(filler[0 .. range.length])))
            {
                //use a quick copy
                auto len2 = range.length;
                range = copy(filler[0 .. len2], range);
            }
            else
            {
                //iterate. No need to check filler, it's length is longer than range's
                for (; !range.empty; range.popFront(), filler.popFront())
                {
                    range.front = filler.front;
                }
            }
        }
        else
        {
            //Most basic case.
            auto bck = filler.save;
            for (; !range.empty; range.popFront(), filler.popFront())
            {
                if (filler.empty) filler = bck.save;
                range.front = filler.front;
            }
        }
    }
}

///
@safe unittest
{
    int[] a = [ 1, 2, 3, 4, 5 ];
    int[] b = [ 8, 9 ];
    fill(a, b);
    assert(a == [ 8, 9, 8, 9, 8 ]);
}

@safe unittest
{
    import std.exception : assertThrown;
    import std.internal.test.dummyrange;

    int[] a = [ 1, 2, 3, 4, 5 ];
    int[] b = [1, 2];
    fill(a, b);
    assert(a == [ 1, 2, 1, 2, 1 ]);

    // fill should accept InputRange
    alias InputRange = DummyRange!(ReturnBy.Reference, Length.No, RangeType.Input);
    InputRange range;
    fill(range,[1,2]);
    foreach (i,value;range.arr)
    assert(value == (i%2 == 0?1:2));

    //test with a input being a "reference forward" range
    fill(a, new ReferenceForwardRange!int([8, 9]));
    assert(a == [8, 9, 8, 9, 8]);

    //test with a input being an "infinite input" range
    fill(a, new ReferenceInfiniteInputRange!int());
    assert(a == [0, 1, 2, 3, 4]);

    //empty filler test
    assertThrown(fill(a, a[$..$]));
}

/**
Initializes all elements of `range` with their `.init` value.
Assumes that the elements of the range are uninitialized.

This function is unavailable if `T` is a `struct` and  `T.this()` is annotated
with `@disable`.

Params:
        range = An
                $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
                that exposes references to its elements and has assignable
                elements

See_Also:
        $(LREF fill)
        $(LREF uninitializedFill)
 */
void initializeAll(Range)(Range range)
if (isInputRange!Range && hasLvalueElements!Range && hasAssignableElements!Range
    && __traits(compiles, { static ElementType!Range _; }))
{
    import core.stdc.string : memset, memcpy;
    import std.traits : hasElaborateAssign, isDynamicArray;

    alias T = ElementType!Range;
    static if (hasElaborateAssign!T)
    {
        import std.algorithm.internal : addressOf;
        //Elaborate opAssign. Must go the memcpy/memset road.
        static if (!__traits(isZeroInit, T))
        {
            for ( ; !range.empty ; range.popFront() )
            {
                import core.internal.lifetime : emplaceInitializer;
                emplaceInitializer(range.front);
            }
        }
        else
            static if (isDynamicArray!Range)
                memset(range.ptr, 0, range.length * T.sizeof);
            else
                for ( ; !range.empty ; range.popFront() )
                    memset(addressOf(range.front), 0, T.sizeof);
    }
    else
        fill(range, T.init);
}

/// ditto
void initializeAll(Range)(Range range)
if (is(Range == char[]) || is(Range == wchar[]))
{
    alias T = ElementEncodingType!Range;
    range[] = T.init;
}

///
@system unittest
{
    import core.stdc.stdlib : malloc, free;

    struct S
    {
        int a = 10;
    }

    auto s = (cast(S*) malloc(5 * S.sizeof))[0 .. 5];
    initializeAll(s);
    assert(s == [S(10), S(10), S(10), S(10), S(10)]);

    scope(exit) free(s.ptr);
}

@system unittest
{
    import std.algorithm.iteration : filter;
    import std.meta : AliasSeq;
    import std.traits : hasElaborateAssign;

    //Test strings:
    //Must work on narrow strings.
    //Must reject const
    char[3] a = void;
    a[].initializeAll();
    assert(a[] == [char.init, char.init, char.init]);
    string s;
    assert(!__traits(compiles, s.initializeAll()));
    assert(!__traits(compiles, s.initializeAll()));
    assert(s.empty);

    //Note: Cannot call uninitializedFill on narrow strings

    enum e {e1, e2}
    e[3] b1 = void;
    b1[].initializeAll();
    assert(b1[] == [e.e1, e.e1, e.e1]);
    e[3] b2 = void;
    b2[].uninitializedFill(e.e2);
    assert(b2[] == [e.e2, e.e2, e.e2]);

    static struct S1
    {
        int i;
    }
    static struct S2
    {
        int i = 1;
    }
    static struct S3
    {
        int i;
        this(this){}
    }
    static struct S4
    {
        int i = 1;
        this(this){}
    }
    static assert(!hasElaborateAssign!S1);
    static assert(!hasElaborateAssign!S2);
    static assert( hasElaborateAssign!S3);
    static assert( hasElaborateAssign!S4);
    assert(!typeid(S1).initializer().ptr);
    assert( typeid(S2).initializer().ptr);
    assert(!typeid(S3).initializer().ptr);
    assert( typeid(S4).initializer().ptr);

    static foreach (S; AliasSeq!(S1, S2, S3, S4))
    {
        //initializeAll
        {
            //Array
            S[3] ss1 = void;
            ss1[].initializeAll();
            assert(ss1[] == [S.init, S.init, S.init]);

            //Not array
            S[3] ss2 = void;
            auto sf = ss2[].filter!"true"();

            sf.initializeAll();
            assert(ss2[] == [S.init, S.init, S.init]);
        }
        //uninitializedFill
        {
            //Array
            S[3] ss1 = void;
            ss1[].uninitializedFill(S(2));
            assert(ss1[] == [S(2), S(2), S(2)]);

            //Not array
            S[3] ss2 = void;
            auto sf = ss2[].filter!"true"();
            sf.uninitializedFill(S(2));
            assert(ss2[] == [S(2), S(2), S(2)]);
        }
    }
}

// test that initializeAll works for arrays of static arrays of structs with
// elaborate assigns.
@system unittest
{
    struct Int {
        ~this() {}
        int x = 3;
    }
    Int[2] xs = [Int(1), Int(2)];
    struct R {
        bool done;
        bool empty() { return done; }
        ref Int[2] front() { return xs; }
        void popFront() { done = true; }
    }
    initializeAll(R());
    assert(xs[0].x == 3);
    assert(xs[1].x == 3);
}

// https://issues.dlang.org/show_bug.cgi?id=22105
@system unittest
{
    struct NoDefaultCtor
    {
        @disable this();
    }

    NoDefaultCtor[1] array = void;
    static assert(!__traits(compiles, array[].initializeAll));
}

// move
/**
Moves `source` into `target`, via a destructive copy when necessary.

If `T` is a struct with a destructor or postblit defined, source is reset
to its `.init` value after it is moved into target, otherwise it is
left unchanged.

Preconditions:
If source has internal pointers that point to itself and doesn't define
opPostMove, it cannot be moved, and will trigger an assertion failure.

Params:
    source = Data to copy.
    target = Where to copy into. The destructor, if any, is invoked before the
        copy is performed.
*/
void move(T)(ref T source, ref T target)
{
    moveImpl(target, source);
}

/// For non-struct types, `move` just performs `target = source`:
@safe unittest
{
    Object obj1 = new Object;
    Object obj2 = obj1;
    Object obj3;

    move(obj2, obj3);
    assert(obj3 is obj1);
    // obj2 unchanged
    assert(obj2 is obj1);
}

///
pure nothrow @safe @nogc unittest
{
    // Structs without destructors are simply copied
    struct S1
    {
        int a = 1;
        int b = 2;
    }
    S1 s11 = { 10, 11 };
    S1 s12;

    move(s11, s12);

    assert(s12 == S1(10, 11));
    assert(s11 == s12);

    // But structs with destructors or postblits are reset to their .init value
    // after copying to the target.
    struct S2
    {
        int a = 1;
        int b = 2;

        ~this() pure nothrow @safe @nogc { }
    }
    S2 s21 = { 3, 4 };
    S2 s22;

    move(s21, s22);

    assert(s21 == S2(1, 2));
    assert(s22 == S2(3, 4));
}

@safe unittest
{
    import std.exception : assertCTFEable;
    import std.traits;

    assertCTFEable!((){
        Object obj1 = new Object;
        Object obj2 = obj1;
        Object obj3;
        move(obj2, obj3);
        assert(obj3 is obj1);

        static struct S1 { int a = 1, b = 2; }
        S1 s11 = { 10, 11 };
        S1 s12;
        move(s11, s12);
        assert(s11.a == 10 && s11.b == 11 && s12.a == 10 && s12.b == 11);

        static struct S2 { int a = 1; int * b; }
        S2 s21 = { 10, null };
        s21.b = new int;
        S2 s22;
        move(s21, s22);
        assert(s21 == s22);
    });
    // https://issues.dlang.org/show_bug.cgi?id=5661 test(1)
    static struct S3
    {
        static struct X { int n = 0; ~this(){n = 0;} }
        X x;
    }
    static assert(hasElaborateDestructor!S3);
    S3 s31, s32;
    s31.x.n = 1;
    move(s31, s32);
    assert(s31.x.n == 0);
    assert(s32.x.n == 1);

    // https://issues.dlang.org/show_bug.cgi?id=5661 test(2)
    static struct S4
    {
        static struct X { int n = 0; this(this){n = 0;} }
        X x;
    }
    static assert(hasElaborateCopyConstructor!S4);
    S4 s41, s42;
    s41.x.n = 1;
    move(s41, s42);
    assert(s41.x.n == 0);
    assert(s42.x.n == 1);

    // https://issues.dlang.org/show_bug.cgi?id=13990 test
    class S5;

    S5 s51;
    S5 s52 = s51;
    S5 s53;
    move(s52, s53);
    assert(s53 is s51);
}

/// Ditto
T move(T)(return scope ref T source)
{
    return moveImpl(source);
}

/// Non-copyable structs can still be moved:
pure nothrow @safe @nogc unittest
{
    struct S
    {
        int a = 1;
        @disable this(this);
        ~this() pure nothrow @safe @nogc {}
    }
    S s1;
    s1.a = 2;
    S s2 = move(s1);
    assert(s1.a == 1);
    assert(s2.a == 2);
}

/// `opPostMove` will be called if defined:
pure nothrow @safe @nogc unittest
{
    struct S
    {
        int a;
        void opPostMove(const ref S old)
        {
            assert(a == old.a);
            a++;
        }
    }
    S s1;
    s1.a = 41;
    S s2 = move(s1);
    assert(s2.a == 42);
}

// https://issues.dlang.org/show_bug.cgi?id=20869
// `move` should propagate the attributes of `opPostMove`
@system unittest
{
    static struct S
    {
        void opPostMove(const ref S old) nothrow @system
        {
            __gshared int i;
            new int(i++); // Force @gc impure @system
        }
    }

    alias T = void function() @system nothrow;
    static assert(is(typeof({ S s; move(s); }) == T));
    static assert(is(typeof({ S s; move(s, s); }) == T));
}

private void moveImpl(T)(ref scope T target, ref return scope T source)
{
    import std.traits : hasElaborateDestructor;

    static if (is(T == struct))
    {
        //  Unsafe when compiling without -dip1000
        if ((() @trusted => &source == &target)()) return;

        // Destroy target before overwriting it
        static if (hasElaborateDestructor!T) target.__xdtor();
    }
    // move and emplace source into target
    moveEmplaceImpl(target, source);
}

private T moveImpl(T)(ref return scope T source)
{
    // Properly infer safety from moveEmplaceImpl as the implementation below
    // might void-initialize pointers in result and hence needs to be @trusted
    if (false) moveEmplaceImpl(source, source);

    return trustedMoveImpl(source);
}

private T trustedMoveImpl(T)(ref return scope T source) @trusted
{
    T result = void;
    moveEmplaceImpl(result, source);
    return result;
}

@safe unittest
{
    import std.exception : assertCTFEable;
    import std.traits;

    assertCTFEable!((){
        Object obj1 = new Object;
        Object obj2 = obj1;
        Object obj3 = move(obj2);
        assert(obj3 is obj1);

        static struct S1 { int a = 1, b = 2; }
        S1 s11 = { 10, 11 };
        S1 s12 = move(s11);
        assert(s11.a == 10 && s11.b == 11 && s12.a == 10 && s12.b == 11);

        static struct S2 { int a = 1; int * b; }
        S2 s21 = { 10, null };
        s21.b = new int;
        S2 s22 = move(s21);
        assert(s21 == s22);
    });

    // https://issues.dlang.org/show_bug.cgi?id=5661 test(1)
    static struct S3
    {
        static struct X { int n = 0; ~this(){n = 0;} }
        X x;
    }
    static assert(hasElaborateDestructor!S3);
    S3 s31;
    s31.x.n = 1;
    S3 s32 = move(s31);
    assert(s31.x.n == 0);
    assert(s32.x.n == 1);

    // https://issues.dlang.org/show_bug.cgi?id=5661 test(2)
    static struct S4
    {
        static struct X { int n = 0; this(this){n = 0;} }
        X x;
    }
    static assert(hasElaborateCopyConstructor!S4);
    S4 s41;
    s41.x.n = 1;
    S4 s42 = move(s41);
    assert(s41.x.n == 0);
    assert(s42.x.n == 1);

    // https://issues.dlang.org/show_bug.cgi?id=13990 test
    class S5;

    S5 s51;
    S5 s52 = s51;
    S5 s53;
    s53 = move(s52);
    assert(s53 is s51);
}

@system unittest
{
    static struct S { int n = 0; ~this() @system { n = 0; } }
    S a, b;
    static assert(!__traits(compiles, () @safe { move(a, b); }));
    static assert(!__traits(compiles, () @safe { move(a); }));
    a.n = 1;
    () @trusted { move(a, b); }();
    assert(a.n == 0);
    a.n = 1;
    () @trusted { move(a); }();
    assert(a.n == 0);
}

// https://issues.dlang.org/show_bug.cgi?id=6217
@safe unittest
{
    import std.algorithm.iteration : map;
    auto x = map!"a"([1,2,3]);
    x = move(x);
}

// https://issues.dlang.org/show_bug.cgi?id=8055
@safe unittest
{
    static struct S
    {
        int x;
        ~this()
        {
            assert(x == 0);
        }
    }
    S foo(S s)
    {
        return move(s);
    }
    S a;
    a.x = 0;
    auto b = foo(a);
    assert(b.x == 0);
}

// https://issues.dlang.org/show_bug.cgi?id=8057
@system unittest
{
    int n = 10;
    struct S
    {
        int x;
        ~this()
        {
            // Access to enclosing scope
            assert(n == 10);
        }
    }
    S foo(S s)
    {
        // Move nested struct
        return move(s);
    }
    S a;
    a.x = 1;
    auto b = foo(a);
    assert(b.x == 1);

    // Regression https://issues.dlang.org/show_bug.cgi?id=8171
    static struct Array(T)
    {
        // nested struct has no member
        struct Payload
        {
            ~this() {}
        }
    }
    Array!int.Payload x = void;
    move(x);
    move(x, x);
}

private void moveEmplaceImpl(T)(ref scope T target, ref return scope T source)
{
    import core.stdc.string : memcpy, memset;
    import std.traits : hasAliasing, hasElaborateAssign,
                        hasElaborateCopyConstructor, hasElaborateDestructor,
                        hasElaborateMove,
                        isAssignable, isStaticArray;

    static if (!is(T == class) && hasAliasing!T) if (!__ctfe)
    {
        import std.exception : doesPointTo;
        assert(!(doesPointTo(source, source) && !hasElaborateMove!T),
            "Cannot move object of type " ~ T.stringof ~ " with internal pointer unless `opPostMove` is defined.");
    }

    static if (is(T == struct))
    {
        //  Unsafe when compiling without -dip1000
        assert((() @trusted => &source !is &target)(), "source and target must not be identical");

        static if (hasElaborateAssign!T || !isAssignable!T)
            () @trusted { memcpy(&target, &source, T.sizeof); }();
        else
            target = source;

        static if (hasElaborateMove!T)
            __move_post_blt(target, source);

        // If the source defines a destructor or a postblit hook, we must obliterate the
        // object in order to avoid double freeing and undue aliasing
        static if (hasElaborateDestructor!T || hasElaborateCopyConstructor!T)
        {
            // If T is nested struct, keep original context pointer
            static if (__traits(isNested, T))
                enum sz = T.sizeof - (void*).sizeof;
            else
                enum sz = T.sizeof;

            static if (__traits(isZeroInit, T))
                () @trusted { memset(&source, 0, sz); }();
            else
                () @trusted { memcpy(&source, __traits(initSymbol, T).ptr, sz); }();
        }
    }
    else static if (isStaticArray!T)
    {
        for (size_t i = 0; i < source.length; ++i)
            move(source[i], target[i]);
    }
    else
    {
        // Primitive data (including pointers and arrays) or class -
        // assignment works great
        target = source;
    }
}

/**
 * Similar to $(LREF move) but assumes `target` is uninitialized. This
 * is more efficient because `source` can be blitted over `target`
 * without destroying or initializing it first.
 *
 * Params:
 *   source = value to be moved into target
 *   target = uninitialized value to be filled by source
 */
void moveEmplace(T)(ref T source, ref T target) pure @system
{
    moveEmplaceImpl(target, source);
}

///
pure nothrow @nogc @system unittest
{
    static struct Foo
    {
    pure nothrow @nogc:
        this(int* ptr) { _ptr = ptr; }
        ~this() { if (_ptr) ++*_ptr; }
        int* _ptr;
    }

    int val;
    Foo foo1 = void; // uninitialized
    auto foo2 = Foo(&val); // initialized
    assert(foo2._ptr is &val);

    // Using `move(foo2, foo1)` would have an undefined effect because it would destroy
    // the uninitialized foo1.
    // moveEmplace directly overwrites foo1 without destroying or initializing it first.
    moveEmplace(foo2, foo1);
    assert(foo1._ptr is &val);
    assert(foo2._ptr is null);
    assert(val == 0);
}

// https://issues.dlang.org/show_bug.cgi?id=18913
@safe unittest
{
    static struct NoCopy
    {
        int payload;
        ~this() { }
        @disable this(this);
    }

    static void f(NoCopy[2]) { }

    NoCopy[2] ncarray = [ NoCopy(1), NoCopy(2) ];

    static assert(!__traits(compiles, f(ncarray)));
    f(move(ncarray));
}

// moveAll
/**
Calls `move(a, b)` for each element `a` in `src` and the corresponding
element `b` in `tgt`, in increasing order.

Preconditions:
`walkLength(src) <= walkLength(tgt)`.
This precondition will be asserted. If you cannot ensure there is enough room in
`tgt` to accommodate all of `src` use $(LREF moveSome) instead.

Params:
    src = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) with
        movable elements.
    tgt = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) with
        elements that elements from `src` can be moved into.

Returns: The leftover portion of `tgt` after all elements from `src` have
been moved.
 */
InputRange2 moveAll(InputRange1, InputRange2)(InputRange1 src, InputRange2 tgt)
if (isInputRange!InputRange1 && isInputRange!InputRange2
        && is(typeof(move(src.front, tgt.front))))
{
    return moveAllImpl!move(src, tgt);
}

///
pure nothrow @safe @nogc unittest
{
    int[3] a = [ 1, 2, 3 ];
    int[5] b;
    assert(moveAll(a[], b[]) is b[3 .. $]);
    assert(a[] == b[0 .. 3]);
    int[3] cmp = [ 1, 2, 3 ];
    assert(a[] == cmp[]);
}

/**
 * Similar to $(LREF moveAll) but assumes all elements in `tgt` are
 * uninitialized. Uses $(LREF moveEmplace) to move elements from
 * `src` over elements from `tgt`.
 */
InputRange2 moveEmplaceAll(InputRange1, InputRange2)(InputRange1 src, InputRange2 tgt) @system
if (isInputRange!InputRange1 && isInputRange!InputRange2
        && is(typeof(moveEmplace(src.front, tgt.front))))
{
    return moveAllImpl!moveEmplace(src, tgt);
}

///
pure nothrow @nogc @system unittest
{
    static struct Foo
    {
        ~this() pure nothrow @nogc { if (_ptr) ++*_ptr; }
        int* _ptr;
    }
    int[3] refs = [0, 1, 2];
    Foo[3] src = [Foo(&refs[0]), Foo(&refs[1]), Foo(&refs[2])];
    Foo[5] dst = void;

    auto tail = moveEmplaceAll(src[], dst[]); // move 3 value from src over dst
    assert(tail.length == 2); // returns remaining uninitialized values
    initializeAll(tail);

    import std.algorithm.searching : all;
    assert(src[].all!(e => e._ptr is null));
    assert(dst[0 .. 3].all!(e => e._ptr !is null));
}

@system unittest
{
    struct InputRange
    {
        ref int front() { return data[0]; }
        void popFront() { data.popFront; }
        bool empty() { return data.empty; }
        int[] data;
    }
    auto a = InputRange([ 1, 2, 3 ]);
    auto b = InputRange(new int[5]);
    moveAll(a, b);
    assert(a.data == b.data[0 .. 3]);
    assert(a.data == [ 1, 2, 3 ]);
}

private InputRange2 moveAllImpl(alias moveOp, InputRange1, InputRange2)(
    ref InputRange1 src, ref InputRange2 tgt)
{
    import std.exception : enforce;

    static if (isRandomAccessRange!InputRange1 && hasLength!InputRange1 && hasLength!InputRange2
         && hasSlicing!InputRange2 && isRandomAccessRange!InputRange2)
    {
        auto toMove = src.length;
        assert(toMove <= tgt.length, "Source buffer needs to be smaller or equal to the target buffer.");
        foreach (idx; 0 .. toMove)
            moveOp(src[idx], tgt[idx]);
        return tgt[toMove .. tgt.length];
    }
    else
    {
        for (; !src.empty; src.popFront(), tgt.popFront())
        {
            assert(!tgt.empty, "Source buffer needs to be smaller or equal to the target buffer.");
            moveOp(src.front, tgt.front);
        }
        return tgt;
    }
}

// moveSome
/**
Calls `move(a, b)` for each element `a` in `src` and the corresponding
element `b` in `tgt`, in increasing order, stopping when either range has been
exhausted.

Params:
    src = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) with
        movable elements.
    tgt = An $(REF_ALTTEXT input range, isInputRange, std,range,primitives) with
        elements that elements from `src` can be moved into.

Returns: The leftover portions of the two ranges after one or the other of the
ranges have been exhausted.
 */
Tuple!(InputRange1, InputRange2) moveSome(InputRange1, InputRange2)(InputRange1 src, InputRange2 tgt)
if (isInputRange!InputRange1 && isInputRange!InputRange2
        && is(typeof(move(src.front, tgt.front))))
{
    return moveSomeImpl!move(src, tgt);
}

///
pure nothrow @safe @nogc unittest
{
    int[5] a = [ 1, 2, 3, 4, 5 ];
    int[3] b;
    assert(moveSome(a[], b[])[0] is a[3 .. $]);
    assert(a[0 .. 3] == b);
    assert(a == [ 1, 2, 3, 4, 5 ]);
}

/**
 * Same as $(LREF moveSome) but assumes all elements in `tgt` are
 * uninitialized. Uses $(LREF moveEmplace) to move elements from
 * `src` over elements from `tgt`.
 */
Tuple!(InputRange1, InputRange2) moveEmplaceSome(InputRange1, InputRange2)(InputRange1 src, InputRange2 tgt) @system
if (isInputRange!InputRange1 && isInputRange!InputRange2
        && is(typeof(move(src.front, tgt.front))))
{
    return moveSomeImpl!moveEmplace(src, tgt);
}

///
pure nothrow @nogc @system unittest
{
    static struct Foo
    {
        ~this() pure nothrow @nogc { if (_ptr) ++*_ptr; }
        int* _ptr;
    }
    int[4] refs = [0, 1, 2, 3];
    Foo[4] src = [Foo(&refs[0]), Foo(&refs[1]), Foo(&refs[2]), Foo(&refs[3])];
    Foo[3] dst = void;

    auto res = moveEmplaceSome(src[], dst[]);
    assert(res.length == 2);

    import std.algorithm.searching : all;
    assert(src[0 .. 3].all!(e => e._ptr is null));
    assert(src[3]._ptr !is null);
    assert(dst[].all!(e => e._ptr !is null));
}

private Tuple!(InputRange1, InputRange2) moveSomeImpl(alias moveOp, InputRange1, InputRange2)(
    ref InputRange1 src, ref InputRange2 tgt)
{
    for (; !src.empty && !tgt.empty; src.popFront(), tgt.popFront())
        moveOp(src.front, tgt.front);
    return tuple(src, tgt);
 }


// SwapStrategy
/**
Defines the swapping strategy for algorithms that need to swap
elements in a range (such as partition and sort). The strategy
concerns the swapping of elements that are not the core concern of the
algorithm. For example, consider an algorithm that sorts $(D [ "abc",
"b", "aBc" ]) according to `toUpper(a) < toUpper(b)`. That
algorithm might choose to swap the two equivalent strings `"abc"`
and `"aBc"`. That does not affect the sorting since both
`["abc", "aBc", "b" ]` and `[ "aBc", "abc", "b" ]` are valid
outcomes.

Some situations require that the algorithm must NOT ever change the
relative ordering of equivalent elements (in the example above, only
`[ "abc", "aBc", "b" ]` would be the correct result). Such
algorithms are called $(B stable). If the ordering algorithm may swap
equivalent elements discretionarily, the ordering is called $(B
unstable).

Yet another class of algorithms may choose an intermediate tradeoff by
being stable only on a well-defined subrange of the range. There is no
established terminology for such behavior; this library calls it $(B
semistable).

Generally, the `stable` ordering strategy may be more costly in
time and/or space than the other two because it imposes additional
constraints. Similarly, `semistable` may be costlier than $(D
unstable). As (semi-)stability is not needed very often, the ordering
algorithms in this module parameterized by `SwapStrategy` all
choose `SwapStrategy.unstable` as the default.
*/

enum SwapStrategy
{
    /**
       Allows freely swapping of elements as long as the output
       satisfies the algorithm's requirements.
    */
    unstable,
    /**
       In algorithms partitioning ranges in two, preserve relative
       ordering of elements only to the left of the partition point.
    */
    semistable,
    /**
       Preserve the relative ordering of elements to the largest
       extent allowed by the algorithm's requirements.
    */
    stable,
}

///
@safe unittest
{
    int[] a = [0, 1, 2, 3];
    assert(remove!(SwapStrategy.stable)(a, 1) == [0, 2, 3]);
    a = [0, 1, 2, 3];
    assert(remove!(SwapStrategy.unstable)(a, 1) == [0, 3, 2]);
}

///
@safe unittest
{
    import std.algorithm.sorting : partition;

    // Put stuff greater than 3 on the left
    auto arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    assert(partition!(a => a > 3, SwapStrategy.stable)(arr) == [1, 2, 3]);
    assert(arr == [4, 5, 6, 7, 8, 9, 10, 1, 2, 3]);

    arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    assert(partition!(a => a > 3, SwapStrategy.semistable)(arr) == [2, 3, 1]);
    assert(arr == [4, 5, 6, 7, 8, 9, 10, 2, 3, 1]);

    arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
    assert(partition!(a => a > 3, SwapStrategy.unstable)(arr) == [3, 2, 1]);
    assert(arr == [10, 9, 8, 4, 5, 6, 7, 3, 2, 1]);
}

private template isValidIntegralTuple(T)
{
    import std.traits : isIntegral;
    import std.typecons : isTuple;
    static if (isTuple!T)
    {
        enum isValidIntegralTuple = T.length == 2 &&
                isIntegral!(typeof(T.init[0])) && isIntegral!(typeof(T.init[0]));
    }
    else
    {
        enum isValidIntegralTuple = isIntegral!T;
    }
}


/**
Eliminates elements at given offsets from `range` and returns the shortened
range.

For example, here is how to remove a single element from an array:

----
string[] a = [ "a", "b", "c", "d" ];
a = a.remove(1); // remove element at offset 1
assert(a == [ "a", "c", "d"]);
----

Note that `remove` does not change the length of the original range directly;
instead, it returns the shortened range. If its return value is not assigned to
the original range, the original range will retain its original length, though
its contents will have changed:

----
int[] a = [ 3, 5, 7, 8 ];
assert(remove(a, 1) == [ 3, 7, 8 ]);
assert(a == [ 3, 7, 8, 8 ]);
----

The element at offset `1` has been removed and the rest of the elements have
shifted up to fill its place, however, the original array remains of the same
length. This is because all functions in `std.algorithm` only change $(I
content), not $(I topology). The value `8` is repeated because $(LREF move) was
invoked to rearrange elements, and on integers `move` simply copies the source
to the destination.  To replace `a` with the effect of the removal, simply
assign the slice returned by `remove` to it, as shown in the first example.

Multiple indices can be passed into `remove`. In that case,
elements at the respective indices are all removed. The indices must
be passed in increasing order, otherwise an exception occurs.

----
int[] a = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
assert(remove(a, 1, 3, 5) ==
    [ 0, 2, 4, 6, 7, 8, 9, 10 ]);
----

(Note that all indices refer to slots in the $(I original) array, not
in the array as it is being progressively shortened.)

Tuples of two integral offsets can be used to remove an indices range:

----
int[] a = [ 3, 4, 5, 6, 7];
assert(remove(a, 1, tuple(1, 3), 9) == [ 3, 6, 7 ]);
----

The tuple passes in a range closed to the left and open to
the right (consistent with built-in slices), e.g. `tuple(1, 3)`
means indices `1` and `2` but not `3`.

Finally, any combination of integral offsets and tuples composed of two integral
offsets can be passed in:

----
int[] a = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
assert(remove(a, 1, tuple(3, 5), 9) == [ 0, 2, 5, 6, 7, 8, 10 ]);
----

In this case, the slots at positions 1, 3, 4, and 9 are removed from
the array.

If the need is to remove some elements in the range but the order of
the remaining elements does not have to be preserved, you may want to
pass `SwapStrategy.unstable` to `remove`.

----
int[] a = [ 0, 1, 2, 3 ];
assert(remove!(SwapStrategy.unstable)(a, 1) == [ 0, 3, 2 ]);
----

In the case above, the element at slot `1` is removed, but replaced
with the last element of the range. Taking advantage of the relaxation
of the stability requirement, `remove` moved elements from the end
of the array over the slots to be removed. This way there is less data
movement to be done which improves the execution time of the function.

The function `remove` works on bidirectional ranges that have assignable
lvalue elements. The moving strategy is (listed from fastest to slowest):

$(UL
        $(LI If $(D s == SwapStrategy.unstable && isRandomAccessRange!Range &&
hasLength!Range && hasLvalueElements!Range), then elements are moved from the
end of the range into the slots to be filled. In this case, the absolute
minimum of moves is performed.)
        $(LI Otherwise, if $(D s ==
SwapStrategy.unstable && isBidirectionalRange!Range && hasLength!Range
&& hasLvalueElements!Range), then elements are still moved from the
end of the range, but time is spent on advancing between slots by repeated
calls to `range.popFront`.)
        $(LI Otherwise, elements are moved
incrementally towards the front of `range`; a given element is never
moved several times, but more elements are moved than in the previous
cases.)
)

Params:
    s = a SwapStrategy to determine if the original order needs to be preserved
    range = a $(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives)
    with a length member
    offset = which element(s) to remove

Returns:
    A range containing all of the elements of range with offset removed.
*/
Range remove
(SwapStrategy s = SwapStrategy.stable, Range, Offset ...)
(Range range, Offset offset)
if (Offset.length >= 1 && allSatisfy!(isValidIntegralTuple, Offset))
{
    // Activate this check when the deprecation of non-integral tuples is over
    //import std.traits : isIntegral;
    //import std.typecons : isTuple;
    //static foreach (T; Offset)
    //{
        //static if (isTuple!T)
        //{
            //static assert(T.length == 2 &&
                    //isIntegral!(typeof(T.init[0])) && isIntegral!(typeof(T.init[0])),
                //"Each offset must be an integral or a tuple of two integrals." ~
                //"Use `arr.remove(pos1, pos2)` or `arr.remove(tuple(start, begin))`");
        //}
        //else
        //{
            //static assert(isIntegral!T,
                //"Each offset must be an integral or a tuple of two integrals." ~
                //"Use `arr.remove(pos1, pos2)` or `arr.remove(tuple(start, begin))`");
        //}
    //}
    return removeImpl!s(range, offset);
}

/// ditto
deprecated("Use of non-integral tuples is deprecated. Use remove(tuple(start, end).")
Range remove
(SwapStrategy s = SwapStrategy.stable, Range, Offset ...)
(Range range, Offset offset)
if (Offset.length >= 1 && !allSatisfy!(isValidIntegralTuple, Offset))
{
    return removeImpl!s(range, offset);
}

///
@safe pure unittest
{
    import std.typecons : tuple;

    auto a = [ 0, 1, 2, 3, 4, 5 ];
    assert(remove!(SwapStrategy.stable)(a, 1) == [ 0, 2, 3, 4, 5 ]);
    a = [ 0, 1, 2, 3, 4, 5 ];
    assert(remove!(SwapStrategy.stable)(a, 1, 3) == [ 0, 2, 4, 5] );
    a = [ 0, 1, 2, 3, 4, 5 ];
    assert(remove!(SwapStrategy.stable)(a, 1, tuple(3, 6)) == [ 0, 2 ]);

    a = [ 0, 1, 2, 3, 4, 5 ];
    assert(remove!(SwapStrategy.unstable)(a, 1) == [0, 5, 2, 3, 4]);
    a = [ 0, 1, 2, 3, 4, 5 ];
    assert(remove!(SwapStrategy.unstable)(a, tuple(1, 4)) == [0, 5, 4]);
}

///
@safe pure unittest
{
    import std.typecons : tuple;

    // Delete an index
    assert([4, 5, 6].remove(1) == [4, 6]);

    // Delete multiple indices
    assert([4, 5, 6, 7, 8].remove(1, 3) == [4, 6, 8]);

    // Use an indices range
    assert([4, 5, 6, 7, 8].remove(tuple(1, 3)) == [4, 7, 8]);

    // Use an indices range and individual indices
    assert([4, 5, 6, 7, 8].remove(0, tuple(1, 3), 4) == [7]);
}

/// `SwapStrategy.unstable` is faster, but doesn't guarantee the same order of the original array
@safe pure unittest
{
    assert([5, 6, 7, 8].remove!(SwapStrategy.stable)(1) == [5, 7, 8]);
    assert([5, 6, 7, 8].remove!(SwapStrategy.unstable)(1) == [5, 8, 7]);
}

private auto removeImpl(SwapStrategy s, Range, Offset...)(Range range, Offset offset)
{
    static if (isNarrowString!Range)
    {
        static assert(isMutable!(typeof(range[0])),
                "Elements must be mutable to remove");
        static assert(s == SwapStrategy.stable,
                "Only stable removing can be done for character arrays");
        return removeStableString(range, offset);
    }
    else
    {
        static assert(isBidirectionalRange!Range,
                "Range must be bidirectional");
        static assert(hasLvalueElements!Range,
                "Range must have Lvalue elements (see std.range.hasLvalueElements)");

        static if (s == SwapStrategy.unstable)
        {
            static assert(hasLength!Range,
                    "Range must have `length` for unstable remove");
            return removeUnstable(range, offset);
        }
        else static if (s == SwapStrategy.stable)
            return removeStable(range, offset);
        else
            static assert(false,
                    "Only SwapStrategy.stable and SwapStrategy.unstable are supported");
    }
}

@safe unittest
{
    import std.exception : assertThrown;
    import std.range;

    // https://issues.dlang.org/show_bug.cgi?id=10173
    int[] test = iota(0, 10).array();
    assertThrown(remove!(SwapStrategy.stable)(test, tuple(2, 4), tuple(1, 3)));
    assertThrown(remove!(SwapStrategy.unstable)(test, tuple(2, 4), tuple(1, 3)));
    assertThrown(remove!(SwapStrategy.stable)(test, 2, 4, 1, 3));
    assertThrown(remove!(SwapStrategy.unstable)(test, 2, 4, 1, 3));
}

@safe unittest
{
    import std.range;
    int[] a = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
    a = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
    assert(remove!(SwapStrategy.stable)(a, 1) ==
        [ 0, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]);

    a = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
    assert(remove!(SwapStrategy.unstable)(a, 0, 10) ==
           [ 9, 1, 2, 3, 4, 5, 6, 7, 8 ]);

    a = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
    assert(remove!(SwapStrategy.unstable)(a, 0, tuple(9, 11)) ==
            [ 8, 1, 2, 3, 4, 5, 6, 7 ]);
    // https://issues.dlang.org/show_bug.cgi?id=5224
    a = [ 1, 2, 3, 4 ];
    assert(remove!(SwapStrategy.unstable)(a, 2) ==
           [ 1, 2, 4 ]);

    a = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
    assert(remove!(SwapStrategy.stable)(a, 1, 5) ==
        [ 0, 2, 3, 4, 6, 7, 8, 9, 10 ]);

    a = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
    assert(remove!(SwapStrategy.stable)(a, 1, 3, 5)
            == [ 0, 2, 4, 6, 7, 8, 9, 10]);
    a = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
    assert(remove!(SwapStrategy.stable)(a, 1, tuple(3, 5))
            == [ 0, 2, 5, 6, 7, 8, 9, 10]);

    a = iota(0, 10).array();
    assert(remove!(SwapStrategy.unstable)(a, tuple(1, 4), tuple(6, 7))
            == [0, 9, 8, 7, 4, 5]);
}

// https://issues.dlang.org/show_bug.cgi?id=11576
@safe unittest
{
    auto arr = [1,2,3];
    arr = arr.remove!(SwapStrategy.unstable)(2);
    assert(arr == [1,2]);

}

// https://issues.dlang.org/show_bug.cgi?id=12889
@safe unittest
{
    import std.range;
    int[1][] arr = [[0], [1], [2], [3], [4], [5], [6]];
    auto orig = arr.dup;
    foreach (i; iota(arr.length))
    {
        assert(orig == arr.remove!(SwapStrategy.unstable)(tuple(i,i)));
        assert(orig == arr.remove!(SwapStrategy.stable)(tuple(i,i)));
    }
}

@safe unittest
{
    char[] chars = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];
    remove(chars, 4);
    assert(chars == ['a', 'b', 'c', 'd', 'f', 'g', 'h', 'h']);

    char[] bigChars = "∑œ∆¬é˚˙ƒé∂ß¡¡".dup;
    assert(remove(bigChars, tuple(4, 6), 8) == ("∑œ∆¬˙ƒ∂ß¡¡"));

    import std.exception : assertThrown;
    assertThrown(remove(bigChars.dup, 1, 0));
    assertThrown(remove(bigChars.dup, tuple(4, 3)));
}

private Range removeUnstable(Range, Offset...)(Range range, Offset offset)
{
    Tuple!(size_t, "pos", size_t, "len")[offset.length] blackouts;
    foreach (i, v; offset)
    {
        static if (is(typeof(v[0]) : size_t) && is(typeof(v[1]) : size_t))
        {
            blackouts[i].pos = v[0];
            blackouts[i].len = v[1] - v[0];
        }
        else
        {
            static assert(is(typeof(v) : size_t), typeof(v).stringof);
            blackouts[i].pos = v;
            blackouts[i].len = 1;
        }
        static if (i > 0)
        {
            import std.exception : enforce;

            enforce(blackouts[i - 1].pos + blackouts[i - 1].len
                    <= blackouts[i].pos,
                "remove(): incorrect ordering of elements to remove");
        }
    }

    size_t left = 0, right = offset.length - 1;
    auto tgt = range.save;
    size_t tgtPos = 0;

    while (left <= right)
    {
        // Look for a blackout on the right
        if (blackouts[right].pos + blackouts[right].len >= range.length)
        {
            range.popBackExactly(blackouts[right].len);

            // Since right is unsigned, we must check for this case, otherwise
            // we might turn it into size_t.max and the loop condition will not
            // fail when it should.
            if (right > 0)
            {
                --right;
                continue;
            }
            else
                break;
        }
        // Advance to next blackout on the left
        assert(blackouts[left].pos >= tgtPos, "Next blackout on the left shouldn't appear before the target.");
        tgt.popFrontExactly(blackouts[left].pos - tgtPos);
        tgtPos = blackouts[left].pos;

        // Number of elements to the right of blackouts[right]
        immutable tailLen = range.length - (blackouts[right].pos + blackouts[right].len);
        size_t toMove = void;
        if (tailLen < blackouts[left].len)
        {
            toMove = tailLen;
            blackouts[left].pos += toMove;
            blackouts[left].len -= toMove;
        }
        else
        {
            toMove = blackouts[left].len;
            ++left;
        }
        tgtPos += toMove;
        foreach (i; 0 .. toMove)
        {
            move(range.back, tgt.front);
            range.popBack();
            tgt.popFront();
        }
    }

    return range;
}

private Range removeStable(Range, Offset...)(Range range, Offset offset)
{
    auto result = range;
    auto src = range, tgt = range;
    size_t pos;
    foreach (pass, i; offset)
    {
        static if (is(typeof(i[0])) && is(typeof(i[1])))
        {
            auto from = i[0], delta = i[1] - i[0];
        }
        else
        {
            auto from = i;
            enum delta = 1;
        }

        static if (pass > 0)
        {
            import std.exception : enforce;
            enforce(pos <= from,
                    "remove(): incorrect ordering of elements to remove");

            for (; pos < from; ++pos, src.popFront(), tgt.popFront())
            {
                move(src.front, tgt.front);
            }
        }
        else
        {
            src.popFrontExactly(from);
            tgt.popFrontExactly(from);
            pos = from;
        }
        // now skip source to the "to" position
        src.popFrontExactly(delta);
        result.popBackExactly(delta);
        pos += delta;
    }
    // leftover move
    moveAll(src, tgt);
    return result;
}

private Range removeStableString(Range, Offset...)(Range range, Offset offsets)
{
    import std.utf : stride;
    size_t charIdx = 0;
    size_t dcharIdx = 0;
    size_t charShift = 0;

    void skipOne()
    {
        charIdx += stride(range[charIdx .. $]);
        ++dcharIdx;
    }

    void copyBackOne()
    {
        auto encodedLen = stride(range[charIdx .. $]);
        foreach (j; charIdx .. charIdx + encodedLen)
            range[j - charShift] = range[j];
        charIdx += encodedLen;
        ++dcharIdx;
    }

    foreach (pass, i; offsets)
    {
        static if (is(typeof(i[0])) && is(typeof(i[1])))
        {
            auto from = i[0];
            auto delta = i[1] - i[0];
        }
        else
        {
            auto from = i;
            enum delta = 1;
        }

        import std.exception : enforce;
        enforce(dcharIdx <= from && delta >= 0,
                "remove(): incorrect ordering of elements to remove");

        while (dcharIdx < from)
            static if (pass == 0)
                skipOne();
            else
                copyBackOne();

        auto mark = charIdx;
        while (dcharIdx < from + delta)
            skipOne();
        charShift += charIdx - mark;
    }

    foreach (i; charIdx .. range.length)
        range[i - charShift] = range[i];

    return range[0 .. $ - charShift];
}

// Use of dynamic arrays as offsets is too error-prone
// https://issues.dlang.org/show_bug.cgi?id=12086
// Activate these tests once the deprecation period of remove with non-integral tuples is over
@safe unittest
{
    //static assert(!__traits(compiles, [0, 1, 2, 3, 4].remove([1, 3]) == [0, 3, 4]));
    static assert(__traits(compiles, [0, 1, 2, 3, 4].remove(1, 3) == [0, 2, 4]));
    //static assert(!__traits(compiles, assert([0, 1, 2, 3, 4].remove([1, 3, 4]) == [0, 3, 4])));
    //static assert(!__traits(compiles, assert([0, 1, 2, 3, 4].remove(tuple(1, 3, 4)) == [0, 3, 4])));

    import std.range : only;
    //static assert(!__traits(compiles, assert([0, 1, 2, 3, 4].remove(only(1, 3)) == [0, 3, 4])));
    static assert(__traits(compiles, assert([0, 1, 2, 3, 4].remove(1, 3) == [0, 2, 4])));
}

/**
Reduces the length of the
$(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives) `range` by removing
elements that satisfy `pred`. If `s = SwapStrategy.unstable`,
elements are moved from the right end of the range over the elements
to eliminate. If `s = SwapStrategy.stable` (the default),
elements are moved progressively to front such that their relative
order is preserved. Returns the filtered range.

Params:
    range = a bidirectional ranges with lvalue elements
        or mutable character arrays

Returns:
    the range with all of the elements where `pred` is `true`
    removed
*/
Range remove(alias pred, SwapStrategy s = SwapStrategy.stable, Range)(Range range)
{
    import std.functional : unaryFun;
    alias pred_ = unaryFun!pred;
    static if (isNarrowString!Range)
    {
        static assert(isMutable!(typeof(range[0])),
                "Elements must be mutable to remove");
        static assert(s == SwapStrategy.stable,
                "Only stable removing can be done for character arrays");
        return removePredString!pred_(range);
    }
    else
    {
        static assert(isBidirectionalRange!Range,
                "Range must be bidirectional");
        static assert(hasLvalueElements!Range,
                "Range must have Lvalue elements (see std.range.hasLvalueElements)");
        static if (s == SwapStrategy.unstable)
            return removePredUnstable!pred_(range);
        else static if (s == SwapStrategy.stable)
            return removePredStable!pred_(range);
        else
            static assert(false,
                    "Only SwapStrategy.stable and SwapStrategy.unstable are supported");
    }
}

///
@safe unittest
{
    static immutable base = [1, 2, 3, 2, 4, 2, 5, 2];

    int[] arr = base[].dup;

    // using a string-based predicate
    assert(remove!("a == 2")(arr) == [ 1, 3, 4, 5 ]);

    // The original array contents have been modified,
    // so we need to reset it to its original state.
    // The length is unmodified however.
    arr[] = base[];

    // using a lambda predicate
    assert(remove!(a => a == 2)(arr) == [ 1, 3, 4, 5 ]);
}

@safe unittest
{
    int[] a = [ 1, 2, 3, 2, 3, 4, 5, 2, 5, 6 ];
    assert(remove!("a == 2", SwapStrategy.unstable)(a) ==
            [ 1, 6, 3, 5, 3, 4, 5 ]);
    a = [ 1, 2, 3, 2, 3, 4, 5, 2, 5, 6 ];
    assert(remove!("a == 2", SwapStrategy.stable)(a) ==
            [ 1, 3, 3, 4, 5, 5, 6 ]);
}

@nogc @safe unittest
{
    // @nogc test
    static int[] arr = [0,1,2,3,4,5,6,7,8,9];
    alias pred = e => e < 5;

    auto r = arr[].remove!(SwapStrategy.unstable)(0);
    r = r.remove!(SwapStrategy.stable)(0);
    r = r.remove!(pred, SwapStrategy.unstable);
    r = r.remove!(pred, SwapStrategy.stable);
}

@safe unittest
{
    import std.algorithm.comparison : min;
    import std.algorithm.searching : all, any;
    import std.algorithm.sorting : isStrictlyMonotonic;
    import std.array : array;
    import std.meta : AliasSeq;
    import std.range : iota, only;
    import std.typecons : Tuple;
    alias E = Tuple!(int, int);
    alias S = Tuple!(E);
    S[] soffsets;
    foreach (start; 0 .. 5)
    foreach (end; min(start+1,5) .. 5)
          soffsets ~= S(E(start,end));
    alias D = Tuple!(E, E);
    D[] doffsets;
    foreach (start1; 0 .. 10)
    foreach (end1; min(start1+1,10) .. 10)
    foreach (start2; end1 .. 10)
    foreach (end2; min(start2+1,10) .. 10)
          doffsets ~= D(E(start1,end1),E(start2,end2));
    alias T = Tuple!(E, E, E);
    T[] toffsets;
    foreach (start1; 0 .. 15)
    foreach (end1; min(start1+1,15) .. 15)
    foreach (start2; end1 .. 15)
    foreach (end2; min(start2+1,15) .. 15)
    foreach (start3; end2 .. 15)
    foreach (end3; min(start3+1,15) .. 15)
            toffsets ~= T(E(start1,end1),E(start2,end2),E(start3,end3));

    static void verify(O...)(int[] r, int len, int removed, bool stable, O offsets)
    {
        assert(r.length == len - removed);
        assert(!stable || r.isStrictlyMonotonic);
        assert(r.all!(e => all!(o => e < o[0] || e >= o[1])(offsets.only)));
    }

    static foreach (offsets; AliasSeq!(soffsets,doffsets,toffsets))
    foreach (os; offsets)
    {
        int len = 5*os.length;
        auto w = iota(0, len).array;
        auto x = w.dup;
        auto y = w.dup;
        auto z = w.dup;
        alias pred = e => any!(o => o[0] <= e && e < o[1])(only(os.expand));
        w = w.remove!(SwapStrategy.unstable)(os.expand);
        x = x.remove!(SwapStrategy.stable)(os.expand);
        y = y.remove!(pred, SwapStrategy.unstable);
        z = z.remove!(pred, SwapStrategy.stable);
        int removed;
        foreach (o; os)
            removed += o[1] - o[0];
        verify(w, len, removed, false, os[]);
        verify(x, len, removed, true, os[]);
        verify(y, len, removed, false, os[]);
        verify(z, len, removed, true, os[]);
        assert(w == y);
        assert(x == z);
    }
}

@safe unittest
{
    char[] chars = "abcdefg".dup;
    assert(chars.remove!(dc => dc == 'c' || dc == 'f') == "abdeg");
    assert(chars == "abdegfg");

    assert(chars.remove!"a == 'd'" == "abegfg");

    char[] bigChars = "¥^¨^©é√∆π".dup;
    assert(bigChars.remove!(dc => dc == "¨"d[0] || dc == "é"d[0]) ==  "¥^^©√∆π");
}

private Range removePredUnstable(alias pred, Range)(Range range)
{
    auto result = range;
    for (;!range.empty;)
    {
        if (!pred(range.front))
        {
            range.popFront();
            continue;
        }
        move(range.back, range.front);
        range.popBack();
        result.popBack();
    }
    return result;
}

private Range removePredStable(alias pred, Range)(Range range)
{
    auto result = range;
    auto tgt = range;
    for (; !range.empty; range.popFront())
    {
        if (pred(range.front))
        {
            // yank this guy
            result.popBack();
            continue;
        }
        // keep this guy
        move(range.front, tgt.front);
        tgt.popFront();
    }
    return result;
}

private Range removePredString(alias pred, SwapStrategy s = SwapStrategy.stable, Range)
(Range range)
{
    import std.utf : decode;
    import std.functional : unaryFun;

    alias pred_ = unaryFun!pred;

    size_t charIdx = 0;
    size_t charShift = 0;
    while (charIdx < range.length)
    {
        size_t start = charIdx;
        if (pred_(decode(range, charIdx)))
        {
            charShift += charIdx - start;
            break;
        }
    }
    while (charIdx < range.length)
    {
        size_t start = charIdx;
        auto doRemove = pred_(decode(range, charIdx));
        auto encodedLen = charIdx - start;
        if (doRemove)
            charShift += encodedLen;
        else
            foreach (i; start .. charIdx)
                range[i - charShift] = range[i];
    }

    return range[0 .. $ - charShift];
}

// reverse
/**
Reverses `r` in-place.  Performs `r.length / 2` evaluations of `swap`.
UTF sequences consisting of multiple code units are preserved properly.

Params:
    r = a $(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives)
        with either swappable elements, a random access range with a length member,
        or a narrow string

Returns: `r`

Note:
    When passing a string with unicode modifiers on characters, such as `\u0301`,
    this function will not properly keep the position of the modifier. For example,
    reversing `ba\u0301d` ("bád") will result in d\u0301ab ("d́ab") instead of
    `da\u0301b` ("dáb").

See_Also: $(REF retro, std,range) for a lazy reverse without changing `r`
*/
Range reverse(Range)(Range r)
if (isBidirectionalRange!Range &&
        (hasSwappableElements!Range ||
         (hasAssignableElements!Range && hasLength!Range && isRandomAccessRange!Range) ||
         (isNarrowString!Range && isAssignable!(ElementType!Range))))
{
    static if (isRandomAccessRange!Range && hasLength!Range)
    {
        //swapAt is in fact the only way to swap non lvalue ranges
        immutable last = r.length - 1;
        immutable steps = r.length / 2;
        for (size_t i = 0; i < steps; i++)
        {
            r.swapAt(i, last - i);
        }
        return r;
    }
    else static if (isNarrowString!Range && isAssignable!(ElementType!Range))
    {
        import std.string : representation;
        import std.utf : stride;

        auto raw = representation(r);
        for (size_t i = 0; i < r.length;)
        {
            immutable step = stride(r, i);
            if (step > 1)
            {
                .reverse(raw[i .. i + step]);
                i += step;
            }
            else
            {
                ++i;
            }
        }
        reverse(raw);
        return r;
    }
    else
    {
        while (!r.empty)
        {
            swap(r.front, r.back);
            r.popFront();
            if (r.empty) break;
            r.popBack();
        }
        return r;
    }
}

///
@safe unittest
{
    int[] arr = [ 1, 2, 3 ];
    assert(arr.reverse == [ 3, 2, 1 ]);
}

@safe unittest
{
    int[] range = null;
    reverse(range);
    range = [ 1 ];
    reverse(range);
    assert(range == [1]);
    range = [1, 2];
    reverse(range);
    assert(range == [2, 1]);
    range = [1, 2, 3];
    assert(range.reverse == [3, 2, 1]);
}

///
@safe unittest
{
    char[] arr = "hello\U00010143\u0100\U00010143".dup;
    assert(arr.reverse == "\U00010143\u0100\U00010143olleh");
}

@safe unittest
{
    void test(string a, string b)
    {
        auto c = a.dup;
        reverse(c);
        assert(c == b, c ~ " != " ~ b);
    }

    test("a", "a");
    test(" ", " ");
    test("\u2029", "\u2029");
    test("\u0100", "\u0100");
    test("\u0430", "\u0430");
    test("\U00010143", "\U00010143");
    test("abcdefcdef", "fedcfedcba");
    test("hello\U00010143\u0100\U00010143", "\U00010143\u0100\U00010143olleh");
}

/**
    The strip group of functions allow stripping of either leading, trailing,
    or both leading and trailing elements.

    The `stripLeft` function will strip the `front` of the range,
    the `stripRight` function will strip the `back` of the range,
    while the `strip` function will strip both the `front` and `back`
    of the range.

    Note that the `strip` and `stripRight` functions require the range to
    be a $(LREF BidirectionalRange) range.

    All of these functions come in two varieties: one takes a target element,
    where the range will be stripped as long as this element can be found.
    The other takes a lambda predicate, where the range will be stripped as
    long as the predicate returns true.

    Params:
        range = a $(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives)
        or $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
        element = the elements to remove

    Returns:
        a Range with all of range except element at the start and end
*/
Range strip(Range, E)(Range range, E element)
if (isBidirectionalRange!Range && is(typeof(range.front == element) : bool))
{
    return range.stripLeft(element).stripRight(element);
}

/// ditto
Range strip(alias pred, Range)(Range range)
if (isBidirectionalRange!Range && is(typeof(pred(range.back)) : bool))
{
    return range.stripLeft!pred().stripRight!pred();
}

/// ditto
Range stripLeft(Range, E)(Range range, E element)
if (isInputRange!Range && is(typeof(range.front == element) : bool))
{
    import std.algorithm.searching : find;
    return find!((auto ref a) => a != element)(range);
}

/// ditto
Range stripLeft(alias pred, Range)(Range range)
if (isInputRange!Range && is(typeof(pred(range.front)) : bool))
{
    import std.algorithm.searching : find;
    import std.functional : not;

    return find!(not!pred)(range);
}

/// ditto
Range stripRight(Range, E)(Range range, E element)
if (isBidirectionalRange!Range && is(typeof(range.back == element) : bool))
{
    for (; !range.empty; range.popBack())
    {
        if (range.back != element)
            break;
    }
    return range;
}

/// ditto
Range stripRight(alias pred, Range)(Range range)
if (isBidirectionalRange!Range && is(typeof(pred(range.back)) : bool))
{
    for (; !range.empty; range.popBack())
    {
        if (!pred(range.back))
            break;
    }
    return range;
}

/// Strip leading and trailing elements equal to the target element.
@safe pure unittest
{
    assert("  foobar  ".strip(' ') == "foobar");
    assert("00223.444500".strip('0') == "223.4445");
    assert("ëëêéüŗōpéêëë".strip('ë') == "êéüŗōpéê");
    assert([1, 1, 0, 1, 1].strip(1) == [0]);
    assert([0.0, 0.01, 0.01, 0.0].strip(0).length == 2);
}

/// Strip leading and trailing elements while the predicate returns true.
@safe pure unittest
{
    assert("  foobar  ".strip!(a => a == ' ')() == "foobar");
    assert("00223.444500".strip!(a => a == '0')() == "223.4445");
    assert("ëëêéüŗōpéêëë".strip!(a => a == 'ë')() == "êéüŗōpéê");
    assert([1, 1, 0, 1, 1].strip!(a => a == 1)() == [0]);
    assert([0.0, 0.01, 0.5, 0.6, 0.01, 0.0].strip!(a => a < 0.4)().length == 2);
}

/// Strip leading elements equal to the target element.
@safe pure unittest
{
    assert("  foobar  ".stripLeft(' ') == "foobar  ");
    assert("00223.444500".stripLeft('0') == "223.444500");
    assert("ůůűniçodêéé".stripLeft('ů') == "űniçodêéé");
    assert([1, 1, 0, 1, 1].stripLeft(1) == [0, 1, 1]);
    assert([0.0, 0.01, 0.01, 0.0].stripLeft(0).length == 3);
}

/// Strip leading elements while the predicate returns true.
@safe pure unittest
{
    assert("  foobar  ".stripLeft!(a => a == ' ')() == "foobar  ");
    assert("00223.444500".stripLeft!(a => a == '0')() == "223.444500");
    assert("ůůűniçodêéé".stripLeft!(a => a == 'ů')() == "űniçodêéé");
    assert([1, 1, 0, 1, 1].stripLeft!(a => a == 1)() == [0, 1, 1]);
    assert([0.0, 0.01, 0.10, 0.5, 0.6].stripLeft!(a => a < 0.4)().length == 2);
}

/// Strip trailing elements equal to the target element.
@safe pure unittest
{
    assert("  foobar  ".stripRight(' ') == "  foobar");
    assert("00223.444500".stripRight('0') == "00223.4445");
    assert("ùniçodêéé".stripRight('é') == "ùniçodê");
    assert([1, 1, 0, 1, 1].stripRight(1) == [1, 1, 0]);
    assert([0.0, 0.01, 0.01, 0.0].stripRight(0).length == 3);
}

/// Strip trailing elements while the predicate returns true.
@safe pure unittest
{
    assert("  foobar  ".stripRight!(a => a == ' ')() == "  foobar");
    assert("00223.444500".stripRight!(a => a == '0')() == "00223.4445");
    assert("ùniçodêéé".stripRight!(a => a == 'é')() == "ùniçodê");
    assert([1, 1, 0, 1, 1].stripRight!(a => a == 1)() == [1, 1, 0]);
    assert([0.0, 0.01, 0.10, 0.5, 0.6].stripRight!(a => a > 0.4)().length == 3);
}

// swap
/**
Swaps `lhs` and `rhs`. The instances `lhs` and `rhs` are moved in
memory, without ever calling `opAssign`, nor any other function. `T`
need not be assignable at all to be swapped.

If `lhs` and `rhs` reference the same instance, then nothing is done.

`lhs` and `rhs` must be mutable. If `T` is a struct or union, then
its fields must also all be (recursively) mutable.

Params:
    lhs = Data to be swapped with `rhs`.
    rhs = Data to be swapped with `lhs`.
*/
void swap(T)(ref T lhs, ref T rhs) @trusted pure nothrow @nogc
if (isBlitAssignable!T && !is(typeof(lhs.proxySwap(rhs))))
{
    import std.traits : hasAliasing, hasElaborateAssign, isAssignable,
                        isStaticArray;
    static if (hasAliasing!T) if (!__ctfe)
    {
        import std.exception : doesPointTo;
        assert(!doesPointTo(lhs, lhs), "Swap: lhs internal pointer.");
        assert(!doesPointTo(rhs, rhs), "Swap: rhs internal pointer.");
        assert(!doesPointTo(lhs, rhs), "Swap: lhs points to rhs.");
        assert(!doesPointTo(rhs, lhs), "Swap: rhs points to lhs.");
    }

    static if (hasElaborateAssign!T || !isAssignable!T)
    {
        if (&lhs != &rhs)
        {
            // For structs with non-trivial assignment, move memory directly
            ubyte[T.sizeof] t = void;
            auto a = (cast(ubyte*) &lhs)[0 .. T.sizeof];
            auto b = (cast(ubyte*) &rhs)[0 .. T.sizeof];
            t[] = a[];
            a[] = b[];
            b[] = t[];
        }
    }
    else
    {
        //Avoid assigning overlapping arrays. Dynamic arrays are fine, because
        //it's their ptr and length properties which get assigned rather
        //than their elements when assigning them, but static arrays are value
        //types and therefore all of their elements get copied as part of
        //assigning them, which would be assigning overlapping arrays if lhs
        //and rhs were the same array.
        static if (isStaticArray!T)
        {
            if (lhs.ptr == rhs.ptr)
                return;
        }

        // For non-elaborate-assign types, suffice to do the classic swap
        static if (__traits(hasCopyConstructor, T))
        {
            // don't invoke any elaborate constructors either
            T tmp = void;
            tmp = lhs;
        }
        else
            auto tmp = lhs;
        lhs = rhs;
        rhs = tmp;
    }
}

///
@safe unittest
{
    // Swapping POD (plain old data) types:
    int a = 42, b = 34;
    swap(a, b);
    assert(a == 34 && b == 42);

    // Swapping structs with indirection:
    static struct S { int x; char c; int[] y; }
    S s1 = { 0, 'z', [ 1, 2 ] };
    S s2 = { 42, 'a', [ 4, 6 ] };
    swap(s1, s2);
    assert(s1.x == 42);
    assert(s1.c == 'a');
    assert(s1.y == [ 4, 6 ]);

    assert(s2.x == 0);
    assert(s2.c == 'z');
    assert(s2.y == [ 1, 2 ]);

    // Immutables cannot be swapped:
    immutable int imm1 = 1, imm2 = 2;
    static assert(!__traits(compiles, swap(imm1, imm2)));

    int c = imm1 + 0;
    int d = imm2 + 0;
    swap(c, d);
    assert(c == 2);
    assert(d == 1);
}

///
@safe unittest
{
    // Non-copyable types can still be swapped.
    static struct NoCopy
    {
        this(this) { assert(0); }
        int n;
        string s;
    }
    NoCopy nc1, nc2;
    nc1.n = 127; nc1.s = "abc";
    nc2.n = 513; nc2.s = "uvwxyz";

    swap(nc1, nc2);
    assert(nc1.n == 513 && nc1.s == "uvwxyz");
    assert(nc2.n == 127 && nc2.s == "abc");

    swap(nc1, nc1);
    swap(nc2, nc2);
    assert(nc1.n == 513 && nc1.s == "uvwxyz");
    assert(nc2.n == 127 && nc2.s == "abc");

    // Types containing non-copyable fields can also be swapped.
    static struct NoCopyHolder
    {
        NoCopy noCopy;
    }
    NoCopyHolder h1, h2;
    h1.noCopy.n = 31; h1.noCopy.s = "abc";
    h2.noCopy.n = 65; h2.noCopy.s = null;

    swap(h1, h2);
    assert(h1.noCopy.n == 65 && h1.noCopy.s == null);
    assert(h2.noCopy.n == 31 && h2.noCopy.s == "abc");

    swap(h1, h1);
    swap(h2, h2);
    assert(h1.noCopy.n == 65 && h1.noCopy.s == null);
    assert(h2.noCopy.n == 31 && h2.noCopy.s == "abc");

    // Const types cannot be swapped.
    const NoCopy const1, const2;
    assert(const1.n == 0 && const2.n == 0);
    static assert(!__traits(compiles, swap(const1, const2)));
}

// https://issues.dlang.org/show_bug.cgi?id=4789
@safe unittest
{
    int[1] s = [1];
    swap(s, s);

    int[3] a = [1, 2, 3];
    swap(a[1], a[2]);
    assert(a == [1, 3, 2]);
}

@safe unittest
{
    static struct NoAssign
    {
        int i;
        void opAssign(NoAssign) @disable;
    }
    auto s1 = NoAssign(1);
    auto s2 = NoAssign(2);
    swap(s1, s2);
    assert(s1.i == 2);
    assert(s2.i == 1);
}

@safe unittest
{
    struct S
    {
        const int i;
        int i2 = 2;
        int i3 = 3;
    }
    S s;
    static assert(!__traits(compiles, swap(s, s)));
    swap(s.i2, s.i3);
    assert(s.i2 == 3);
    assert(s.i3 == 2);
}

// https://issues.dlang.org/show_bug.cgi?id=11853
@safe unittest
{
    import std.traits : isAssignable;
    alias T = Tuple!(int, double);
    static assert(isAssignable!T);
}

// https://issues.dlang.org/show_bug.cgi?id=12024
@safe unittest
{
    import std.datetime;
    SysTime a, b;
    swap(a, b);
}

// https://issues.dlang.org/show_bug.cgi?id=9975
@system unittest
{
    import std.exception : doesPointTo, mayPointTo;
    static struct S2
    {
        union
        {
            size_t sz;
            string s;
        }
    }
    S2 a , b;
    a.sz = -1;
    assert(!doesPointTo(a, b));
    assert( mayPointTo(a, b));
    swap(a, b);

    //Note: we can catch an error here, because there is no RAII in this test
    import std.exception : assertThrown;
    void* p, pp;
    p = &p;
    assertThrown!Error(move(p));
    assertThrown!Error(move(p, pp));
    assertThrown!Error(swap(p, pp));
}

@system unittest
{
    static struct A
    {
        int* x;
        this(this) { x = new int; }
    }
    A a1, a2;
    swap(a1, a2);

    static struct B
    {
        int* x;
        void opAssign(B) { x = new int; }
    }
    B b1, b2;
    swap(b1, b2);
}

// issue 20732
@safe unittest
{
    static struct A
    {
        int x;
        this(scope ref return const A other)
        {
            import std.stdio;
            x = other.x;
            // note, struct functions inside @safe functions infer ALL
            // attributes, so the following 3 lines are meant to prevent this.
            new int; // prevent @nogc inference
            writeln("impure"); // prevent pure inference
            throw new Exception(""); // prevent nothrow inference
        }
    }

    A a1, a2;
    swap(a1, a2);

    A[1] a3, a4;
    swap(a3, a4);
}

/// ditto
void swap(T)(ref T lhs, ref T rhs)
if (is(typeof(lhs.proxySwap(rhs))))
{
    lhs.proxySwap(rhs);
}

/**
Swaps two elements in-place of a range `r`,
specified by their indices `i1` and `i2`.

Params:
    r  = a range with swappable elements
    i1 = first index
    i2 = second index
*/
void swapAt(R)(auto ref R r, size_t i1, size_t i2)
{
    static if (is(typeof(&r.swapAt)))
    {
        r.swapAt(i1, i2);
    }
    else static if (is(typeof(&r[i1])))
    {
        swap(r[i1], r[i2]);
    }
    else
    {
        if (i1 == i2) return;
        auto t1 = r.moveAt(i1);
        auto t2 = r.moveAt(i2);
        r[i2] = t1;
        r[i1] = t2;
    }
}

///
pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;
    auto a = [1, 2, 3];
    a.swapAt(1, 2);
    assert(a.equal([1, 3, 2]));
}

pure @safe nothrow unittest
{
    import std.algorithm.comparison : equal;
    auto a = [4, 5, 6];
    a.swapAt(1, 1);
    assert(a.equal([4, 5, 6]));
}

pure @safe nothrow unittest
{
    // test non random access ranges
    import std.algorithm.comparison : equal;
    import std.array : array;

    char[] b = ['a', 'b', 'c'];
    b.swapAt(1, 2);
    assert(b.equal(['a', 'c', 'b']));

    int[3] c = [1, 2, 3];
    c.swapAt(1, 2);
    assert(c.array.equal([1, 3, 2]));

    // opIndex returns lvalue
    struct RandomIndexType(T)
    {
        T payload;

        @property ref auto opIndex(size_t i)
        {
           return payload[i];
        }

    }
    auto d = RandomIndexType!(int[])([4, 5, 6]);
    d.swapAt(1, 2);
    assert(d.payload.equal([4, 6, 5]));

    // custom moveAt and opIndexAssign
    struct RandomMoveAtType(T)
    {
        T payload;

        ElementType!T moveAt(size_t i)
        {
           return payload.moveAt(i);
        }

        void opIndexAssign(ElementType!T val, size_t idx)
        {
            payload[idx] = val;
        }
    }
    auto e = RandomMoveAtType!(int[])([7, 8, 9]);
    e.swapAt(1, 2);
    assert(e.payload.equal([7, 9, 8]));


    // custom swapAt
    struct RandomSwapAtType(T)
    {
        T payload;

        void swapAt(size_t i)
        {
           return payload.swapAt(i);
        }
    }
    auto f = RandomMoveAtType!(int[])([10, 11, 12]);
    swapAt(f, 1, 2);
    assert(f.payload.equal([10, 12, 11]));
}

private void swapFront(R1, R2)(R1 r1, R2 r2)
if (isInputRange!R1 && isInputRange!R2)
{
    static if (is(typeof(swap(r1.front, r2.front))))
    {
        swap(r1.front, r2.front);
    }
    else
    {
        auto t1 = moveFront(r1), t2 = moveFront(r2);
        r1.front = move(t2);
        r2.front = move(t1);
    }
}

// swapRanges
/**
Swaps all elements of `r1` with successive elements in `r2`.
Returns a tuple containing the remainder portions of `r1` and $(D
r2) that were not swapped (one of them will be empty). The ranges may
be of different types but must have the same element type and support
swapping.

Params:
    r1 = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
         with swappable elements
    r2 = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
         with swappable elements

Returns:
    Tuple containing the remainder portions of r1 and r2 that were not swapped
*/
Tuple!(InputRange1, InputRange2)
swapRanges(InputRange1, InputRange2)(InputRange1 r1, InputRange2 r2)
if (hasSwappableElements!InputRange1 && hasSwappableElements!InputRange2
    && is(ElementType!InputRange1 == ElementType!InputRange2))
{
    for (; !r1.empty && !r2.empty; r1.popFront(), r2.popFront())
    {
        swap(r1.front, r2.front);
    }
    return tuple(r1, r2);
}

///
@safe unittest
{
    import std.range : empty;
    int[] a = [ 100, 101, 102, 103 ];
    int[] b = [ 0, 1, 2, 3 ];
    auto c = swapRanges(a[1 .. 3], b[2 .. 4]);
    assert(c[0].empty && c[1].empty);
    assert(a == [ 100, 2, 3, 103 ]);
    assert(b == [ 0, 1, 101, 102 ]);
}

/**
Initializes each element of `range` with `value`.
Assumes that the elements of the range are uninitialized.
This is of interest for structs that
define copy constructors (for all other types, $(LREF fill) and
uninitializedFill are equivalent).

Params:
        range = An
                $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
                that exposes references to its elements and has assignable
                elements
        value = Assigned to each element of range

See_Also:
        $(LREF fill)
        $(LREF initializeAll)
 */
void uninitializedFill(Range, Value)(Range range, Value value)
if (isInputRange!Range && hasLvalueElements!Range && is(typeof(range.front = value)))
{
    import std.traits : hasElaborateAssign;

    alias T = ElementType!Range;
    static if (hasElaborateAssign!T)
    {
        import core.internal.lifetime : emplaceRef;

        // Must construct stuff by the book
        for (; !range.empty; range.popFront())
            emplaceRef!T(range.front, value);
    }
    else
        // Doesn't matter whether fill is initialized or not
        return fill(range, value);
}

///
nothrow @system unittest
{
    import core.stdc.stdlib : malloc, free;

    auto s = (cast(int*) malloc(5 * int.sizeof))[0 .. 5];
    uninitializedFill(s, 42);
    assert(s == [ 42, 42, 42, 42, 42 ]);

    scope(exit) free(s.ptr);
}
