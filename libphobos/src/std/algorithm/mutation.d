// Written in the D programming language.
/**
This is a submodule of $(MREF std, algorithm).
It contains generic _mutation algorithms.

$(SCRIPT inhibitQuickIndex = 1;)
$(BOOKTABLE Cheat Sheet,
$(TR $(TH Function Name) $(TH Description))
$(T2 bringToFront,
        If $(D a = [1, 2, 3]) and $(D b = [4, 5, 6, 7]),
        $(D bringToFront(a, b)) leaves $(D a = [4, 5, 6]) and
        $(D b = [7, 1, 2, 3]).)
$(T2 copy,
        Copies a range to another. If
        $(D a = [1, 2, 3]) and $(D b = new int[5]), then $(D copy(a, b))
        leaves $(D b = [1, 2, 3, 0, 0]) and returns $(D b[3 .. $]).)
$(T2 fill,
        Fills a range with a pattern,
        e.g., if $(D a = new int[3]), then $(D fill(a, 4))
        leaves $(D a = [4, 4, 4]) and $(D fill(a, [3, 4])) leaves
        $(D a = [3, 4, 3]).)
$(T2 initializeAll,
        If $(D a = [1.2, 3.4]), then $(D initializeAll(a)) leaves
        $(D a = [double.init, double.init]).)
$(T2 move,
        $(D move(a, b)) moves $(D a) into $(D b). $(D move(a)) reads $(D a)
        destructively when necessary.)
$(T2 moveEmplace,
        Similar to $(D move) but assumes `target` is uninitialized.)
$(T2 moveAll,
        Moves all elements from one range to another.)
$(T2 moveEmplaceAll,
        Similar to $(D moveAll) but assumes all elements in `target` are uninitialized.)
$(T2 moveSome,
        Moves as many elements as possible from one range to another.)
$(T2 moveEmplaceSome,
        Similar to $(D moveSome) but assumes all elements in `target` are uninitialized.)
$(T2 remove,
        Removes elements from a range in-place, and returns the shortened
        range.)
$(T2 reverse,
        If $(D a = [1, 2, 3]), $(D reverse(a)) changes it to $(D [3, 2, 1]).)
$(T2 strip,
        Strips all leading and trailing elements equal to a value, or that
        satisfy a predicate.
        If $(D a = [1, 1, 0, 1, 1]), then $(D strip(a, 1)) and
        $(D strip!(e => e == 1)(a)) returns $(D [0]).)
$(T2 stripLeft,
        Strips all leading elements equal to a value, or that satisfy a
        predicate.  If $(D a = [1, 1, 0, 1, 1]), then $(D stripLeft(a, 1)) and
        $(D stripLeft!(e => e == 1)(a)) returns $(D [0, 1, 1]).)
$(T2 stripRight,
        Strips all trailing elements equal to a value, or that satisfy a
        predicate.
        If $(D a = [1, 1, 0, 1, 1]), then $(D stripRight(a, 1)) and
        $(D stripRight!(e => e == 1)(a)) returns $(D [1, 1, 0]).)
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

Source: $(PHOBOSSRC std/algorithm/_mutation.d)

Macros:
T2=$(TR $(TDNW $(LREF $1)) $(TD $+))
 */
module std.algorithm.mutation;

import std.range.primitives;
import std.traits : isArray, isBlitAssignable, isNarrowString, Unqual, isSomeChar;
// FIXME
import std.typecons; // : tuple, Tuple;

// bringToFront
/**
The $(D bringToFront) function has considerable flexibility and
usefulness. It can rotate elements in one buffer left or right, swap
buffers of equal length, and even move elements across disjoint
buffers of different types and different lengths.

$(D bringToFront) takes two ranges $(D front) and $(D back), which may
be of different types. Considering the concatenation of $(D front) and
$(D back) one unified range, $(D bringToFront) rotates that unified
range such that all elements in $(D back) are brought to the beginning
of the unified range. The relative ordering of elements in $(D front)
and $(D back), respectively, remains unchanged.

The $(D bringToFront) function treats strings at the code unit
level and it is not concerned with Unicode character integrity.
$(D bringToFront) is designed as a function for moving elements
in ranges, not as a string function.

Performs $(BIGOH max(front.length, back.length)) evaluations of $(D
swap).

Preconditions:

Either $(D front) and $(D back) are disjoint, or $(D back) is
reachable from $(D front) and $(D front) is not reachable from $(D
back).

Params:
    front = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    back = a $(REF_ALTTEXT forward range, isForwardRange, std,range,primitives)

Returns:
    The number of elements brought to the front, i.e., the length of $(D back).

See_Also:
    $(HTTP sgi.com/tech/stl/_rotate.html, STL's rotate)
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
            assert(front.empty);
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

/**
The simplest use of $(D bringToFront) is for rotating elements in a
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
The $(D front) range may actually "step over" the $(D back)
range. This is very useful with forward ranges that cannot compute
comfortably right-bounded subranges like $(D arr[0 .. 4]) above. In
the example below, $(D r2) is a right subrange of $(D r1).
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

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.conv : text;
    import std.random : Random, unpredictableSeed, uniform;

    // a more elaborate test
    {
        auto rnd = Random(unpredictableSeed);
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

    // Bugzilla 16959
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
Copies the content of $(D source) into $(D target) and returns the
remaining (unfilled) part of $(D target).

Preconditions: $(D target) shall have enough room to accommodate
the entirety of $(D source).

Params:
    source = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    target = an output range

Returns:
    The unfilled part of target

See_Also:
    $(HTTP sgi.com/tech/stl/_copy.html, STL's _copy)
 */
TargetRange copy(SourceRange, TargetRange)(SourceRange source, TargetRange target)
if (areCopyCompatibleArrays!(SourceRange, TargetRange))
{
    const tlen = target.length;
    const slen = source.length;
    assert(tlen >= slen,
            "Cannot copy a source range into a smaller target range.");

    immutable overlaps = __ctfe || () @trusted {
        return source.ptr < target.ptr + tlen &&
               target.ptr < source.ptr + slen; }();

    if (overlaps)
    {
        foreach (idx; 0 .. slen)
            target[idx] = source[idx];
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

/// ditto
TargetRange copy(SourceRange, TargetRange)(SourceRange source, TargetRange target)
if (!areCopyCompatibleArrays!(SourceRange, TargetRange) &&
    isInputRange!SourceRange &&
    isOutputRange!(TargetRange, ElementType!SourceRange))
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
        put(target, source);
        return target;
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
To _copy at most $(D n) elements from a range, you may want to use
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
$(HTTP sgi.com/tech/stl/copy_backward.html, STL's copy_backward'):
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

    {   // Test for bug 7898
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

@safe unittest
{
    // Issue 13650
    import std.meta : AliasSeq;
    foreach (Char; AliasSeq!(char, wchar, dchar))
    {
        Char[3] a1 = "123";
        Char[6] a2 = "456789";
        assert(copy(a1[], a2[]) is a2[3..$]);
        assert(a1[] == "123");
        assert(a2[] == "123789");
    }
}

/**
Assigns $(D value) to each element of input _range $(D range).

Params:
        range = An
                $(REF_ALTTEXT input _range, isInputRange, std,_range,primitives)
                that exposes references to its elements and has assignable
                elements
        value = Assigned to each element of range

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

// issue 16342, test fallback on mutable narrow strings
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
// end issue 16342

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

/**
Fills $(D range) with a pattern copied from $(D filler). The length of
$(D range) does not have to be a multiple of the length of $(D
filler). If $(D filler) is empty, an exception is thrown.

Params:
    range = An $(REF_ALTTEXT input _range, isInputRange, std,_range,primitives)
            that exposes references to its elements and has assignable elements.
    filler = The
             $(REF_ALTTEXT forward _range, isForwardRange, std,_range,primitives)
             representing the _fill pattern.
 */
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
Initializes all elements of $(D range) with their $(D .init) value.
Assumes that the elements of the range are uninitialized.

Params:
        range = An
                $(REF_ALTTEXT input _range, isInputRange, std,_range,primitives)
                that exposes references to its elements and has assignable
                elements

See_Also:
        $(LREF fill)
        $(LREF uninitializeFill)
 */
void initializeAll(Range)(Range range)
if (isInputRange!Range && hasLvalueElements!Range && hasAssignableElements!Range)
{
    import core.stdc.string : memset, memcpy;
    import std.traits : hasElaborateAssign, isDynamicArray;

    alias T = ElementType!Range;
    static if (hasElaborateAssign!T)
    {
        import std.algorithm.internal : addressOf;
        //Elaborate opAssign. Must go the memcpy road.
        //We avoid calling emplace here, because our goal is to initialize to
        //the static state of T.init,
        //So we want to avoid any un-necassarilly CC'ing of T.init
        auto p = typeid(T).initializer();
        if (p.ptr)
        {
            for ( ; !range.empty ; range.popFront() )
            {
                static if (__traits(isStaticArray, T))
                {
                    // static array initializer only contains initialization
                    // for one element of the static array.
                    auto elemp = cast(void *) addressOf(range.front);
                    auto endp = elemp + T.sizeof;
                    while (elemp < endp)
                    {
                        memcpy(elemp, p.ptr, p.length);
                        elemp += p.length;
                    }
                }
                else
                {
                    memcpy(addressOf(range.front), p.ptr, T.sizeof);
                }
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

    foreach (S; AliasSeq!(S1, S2, S3, S4))
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

// move
/**
Moves `source` into `target`, via a destructive copy when necessary.

If `T` is a struct with a destructor or postblit defined, source is reset
to its `.init` value after it is moved into target, otherwise it is
left unchanged.

Preconditions:
If source has internal pointers that point to itself, it cannot be moved, and
will trigger an assertion failure.

Params:
    source = Data to copy.
    target = Where to copy into. The destructor, if any, is invoked before the
        copy is performed.
*/
void move(T)(ref T source, ref T target)
{
    // test @safe destructible
    static if (__traits(compiles, (T t) @safe {}))
        trustedMoveImpl(source, target);
    else
        moveImpl(source, target);
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
    // Issue 5661 test(1)
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

    // Issue 5661 test(2)
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

    // Issue 13990 test
    class S5;

    S5 s51;
    S5 s52 = s51;
    S5 s53;
    move(s52, s53);
    assert(s53 is s51);
}

/// Ditto
T move(T)(ref T source)
{
    // test @safe destructible
    static if (__traits(compiles, (T t) @safe {}))
        return trustedMoveImpl(source);
    else
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

private void trustedMoveImpl(T)(ref T source, ref T target) @trusted
{
    moveImpl(source, target);
}

private void moveImpl(T)(ref T source, ref T target)
{
    import std.traits : hasElaborateDestructor;

    static if (is(T == struct))
    {
        if (&source == &target) return;
        // Destroy target before overwriting it
        static if (hasElaborateDestructor!T) target.__xdtor();
    }
    // move and emplace source into target
    moveEmplace(source, target);
}

private T trustedMoveImpl(T)(ref T source) @trusted
{
    return moveImpl(source);
}

private T moveImpl(T)(ref T source)
{
    T result = void;
    moveEmplace(source, result);
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

    // Issue 5661 test(1)
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

    // Issue 5661 test(2)
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

    // Issue 13990 test
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

@safe unittest//Issue 6217
{
    import std.algorithm.iteration : map;
    auto x = map!"a"([1,2,3]);
    x = move(x);
}

@safe unittest// Issue 8055
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

@system unittest// Issue 8057
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

    // Regression 8171
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

/**
 * Similar to $(LREF move) but assumes `target` is uninitialized. This
 * is more efficient because `source` can be blitted over `target`
 * without destroying or initializing it first.
 *
 * Params:
 *   source = value to be moved into target
 *   target = uninitialized value to be filled by source
 */
void moveEmplace(T)(ref T source, ref T target) @system
{
    import core.stdc.string : memcpy, memset;
    import std.traits : hasAliasing, hasElaborateAssign,
                        hasElaborateCopyConstructor, hasElaborateDestructor,
                        isAssignable;

    static if (!is(T == class) && hasAliasing!T) if (!__ctfe)
    {
        import std.exception : doesPointTo;
        assert(!doesPointTo(source, source), "Cannot move object with internal pointer.");
    }

    static if (is(T == struct))
    {
        assert(&source !is &target, "source and target must not be identical");

        static if (hasElaborateAssign!T || !isAssignable!T)
            memcpy(&target, &source, T.sizeof);
        else
            target = source;

        // If the source defines a destructor or a postblit hook, we must obliterate the
        // object in order to avoid double freeing and undue aliasing
        static if (hasElaborateDestructor!T || hasElaborateCopyConstructor!T)
        {
            // If T is nested struct, keep original context pointer
            static if (__traits(isNested, T))
                enum sz = T.sizeof - (void*).sizeof;
            else
                enum sz = T.sizeof;

            auto init = typeid(T).initializer();
            if (init.ptr is null) // null ptr means initialize to 0s
                memset(&source, 0, sz);
            else
                memcpy(&source, init.ptr, sz);
        }
    }
    else
    {
        // Primitive data (including pointers and arrays) or class -
        // assignment works great
        target = source;
    }
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
        elements that elements from $(D src) can be moved into.

Returns: The leftover portion of $(D tgt) after all elements from $(D src) have
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
        assert(toMove <= tgt.length);
        foreach (idx; 0 .. toMove)
            moveOp(src[idx], tgt[idx]);
        return tgt[toMove .. tgt.length];
    }
    else
    {
        for (; !src.empty; src.popFront(), tgt.popFront())
        {
            assert(!tgt.empty);
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
        elements that elements from $(D src) can be moved into.

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
"b", "aBc" ]) according to $(D toUpper(a) < toUpper(b)). That
algorithm might choose to swap the two equivalent strings $(D "abc")
and $(D "aBc"). That does not affect the sorting since both $(D [
"abc", "aBc", "b" ]) and $(D [ "aBc", "abc", "b" ]) are valid
outcomes.

Some situations require that the algorithm must NOT ever change the
relative ordering of equivalent elements (in the example above, only
$(D [ "abc", "aBc", "b" ]) would be the correct result). Such
algorithms are called $(B stable). If the ordering algorithm may swap
equivalent elements discretionarily, the ordering is called $(B
unstable).

Yet another class of algorithms may choose an intermediate tradeoff by
being stable only on a well-defined subrange of the range. There is no
established terminology for such behavior; this library calls it $(B
semistable).

Generally, the $(D stable) ordering strategy may be more costly in
time and/or space than the other two because it imposes additional
constraints. Similarly, $(D semistable) may be costlier than $(D
unstable). As (semi-)stability is not needed very often, the ordering
algorithms in this module parameterized by $(D SwapStrategy) all
choose $(D SwapStrategy.unstable) as the default.
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
    import std.stdio;
    import std.algorithm.sorting : partition;
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

/**
Eliminates elements at given offsets from `range` and returns the shortened
range.

For example, here is how to _remove a single element from an array:

----
string[] a = [ "a", "b", "c", "d" ];
a = a.remove(1); // remove element at offset 1
assert(a == [ "a", "c", "d"]);
----

Note that `remove` does not change the length of the original _range directly;
instead, it returns the shortened _range. If its return value is not assigned to
the original _range, the original _range will retain its original length, though
its contents will have changed:

----
int[] a = [ 3, 5, 7, 8 ];
assert(remove(a, 1) == [ 3, 7, 8 ]);
assert(a == [ 3, 7, 8, 8 ]);
----

The element at _offset `1` has been removed and the rest of the elements have
shifted up to fill its place, however, the original array remains of the same
length. This is because all functions in `std.algorithm` only change $(I
content), not $(I topology). The value `8` is repeated because $(LREF move) was
invoked to rearrange elements, and on integers `move` simply copies the source
to the destination.  To replace `a` with the effect of the removal, simply
assign the slice returned by `remove` to it, as shown in the first example.

Multiple indices can be passed into $(D remove). In that case,
elements at the respective indices are all removed. The indices must
be passed in increasing order, otherwise an exception occurs.

----
int[] a = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
assert(remove(a, 1, 3, 5) ==
    [ 0, 2, 4, 6, 7, 8, 9, 10 ]);
----

(Note that all indices refer to slots in the $(I original) array, not
in the array as it is being progressively shortened.) Finally, any
combination of integral offsets and tuples composed of two integral
offsets can be passed in.

----
int[] a = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ];
assert(remove(a, 1, tuple(3, 5), 9) == [ 0, 2, 5, 6, 7, 8, 10 ]);
----

In this case, the slots at positions 1, 3, 4, and 9 are removed from
the array. The tuple passes in a range closed to the left and open to
the right (consistent with built-in slices), e.g. $(D tuple(3, 5))
means indices $(D 3) and $(D 4) but not $(D 5).

If the need is to remove some elements in the range but the order of
the remaining elements does not have to be preserved, you may want to
pass $(D SwapStrategy.unstable) to $(D remove).

----
int[] a = [ 0, 1, 2, 3 ];
assert(remove!(SwapStrategy.unstable)(a, 1) == [ 0, 3, 2 ]);
----

In the case above, the element at slot $(D 1) is removed, but replaced
with the last element of the range. Taking advantage of the relaxation
of the stability requirement, $(D remove) moved elements from the end
of the array over the slots to be removed. This way there is less data
movement to be done which improves the execution time of the function.

The function $(D remove) works on bidirectional ranges that have assignable
lvalue elements. The moving strategy is (listed from fastest to slowest):
$(UL $(LI If $(D s == SwapStrategy.unstable && isRandomAccessRange!Range &&
hasLength!Range && hasLvalueElements!Range), then elements are moved from the
end of the range into the slots to be filled. In this case, the absolute
minimum of moves is performed.)  $(LI Otherwise, if $(D s ==
SwapStrategy.unstable && isBidirectionalRange!Range && hasLength!Range
&& hasLvalueElements!Range), then elements are still moved from the
end of the range, but time is spent on advancing between slots by repeated
calls to $(D range.popFront).)  $(LI Otherwise, elements are moved
incrementally towards the front of $(D range); a given element is never
moved several times, but more elements are moved than in the previous
cases.))

Params:
    s = a SwapStrategy to determine if the original order needs to be preserved
    range = a $(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,_range,primitives)
    with a length member
    offset = which element(s) to remove

Returns:
    a range containing all of the elements of range with offset removed
 */
Range remove
(SwapStrategy s = SwapStrategy.stable, Range, Offset...)
(Range range, Offset offset)
if (s != SwapStrategy.stable
    && isBidirectionalRange!Range
    && hasLvalueElements!Range
    && hasLength!Range
    && Offset.length >= 1)
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
        assert(blackouts[left].pos >= tgtPos);
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

/// Ditto
Range remove
(SwapStrategy s = SwapStrategy.stable, Range, Offset...)
(Range range, Offset offset)
if (s == SwapStrategy.stable
    && isBidirectionalRange!Range
    && hasLvalueElements!Range
    && Offset.length >= 1)
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

@safe unittest
{
    import std.exception : assertThrown;
    import std.range;

    // http://d.puremagic.com/issues/show_bug.cgi?id=10173
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
    // http://d.puremagic.com/issues/show_bug.cgi?id=5224
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

@safe unittest
{
    // Issue 11576
    auto arr = [1,2,3];
    arr = arr.remove!(SwapStrategy.unstable)(2);
    assert(arr == [1,2]);

}

@safe unittest
{
    import std.range;
    // Bug# 12889
    int[1][] arr = [[0], [1], [2], [3], [4], [5], [6]];
    auto orig = arr.dup;
    foreach (i; iota(arr.length))
    {
        assert(orig == arr.remove!(SwapStrategy.unstable)(tuple(i,i)));
        assert(orig == arr.remove!(SwapStrategy.stable)(tuple(i,i)));
    }
}

/**
Reduces the length of the
$(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,_range,primitives) $(D range) by removing
elements that satisfy $(D pred). If $(D s = SwapStrategy.unstable),
elements are moved from the right end of the range over the elements
to eliminate. If $(D s = SwapStrategy.stable) (the default),
elements are moved progressively to front such that their relative
order is preserved. Returns the filtered range.

Params:
    range = a bidirectional ranges with lvalue elements

Returns:
    the range with all of the elements where $(D pred) is $(D true)
    removed
*/
Range remove(alias pred, SwapStrategy s = SwapStrategy.stable, Range)
(Range range)
if (isBidirectionalRange!Range
    && hasLvalueElements!Range)
{
    import std.functional : unaryFun;
    auto result = range;
    static if (s != SwapStrategy.stable)
    {
        for (;!range.empty;)
        {
            if (!unaryFun!pred(range.front))
            {
                range.popFront();
                continue;
            }
            move(range.back, range.front);
            range.popBack();
            result.popBack();
        }
    }
    else
    {
        auto tgt = range;
        for (; !range.empty; range.popFront())
        {
            if (unaryFun!(pred)(range.front))
            {
                // yank this guy
                result.popBack();
                continue;
            }
            // keep this guy
            move(range.front, tgt.front);
            tgt.popFront();
        }
    }
    return result;
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

@nogc @system unittest
{
    // @nogc test
    int[10] arr = [0,1,2,3,4,5,6,7,8,9];
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
    alias S = Tuple!(int[2]);
    S[] soffsets;
    foreach (start; 0 .. 5)
    foreach (end; min(start+1,5) .. 5)
          soffsets ~= S([start,end]);
    alias D = Tuple!(int[2],int[2]);
    D[] doffsets;
    foreach (start1; 0 .. 10)
    foreach (end1; min(start1+1,10) .. 10)
    foreach (start2; end1 .. 10)
    foreach (end2; min(start2+1,10) .. 10)
          doffsets ~= D([start1,end1],[start2,end2]);
    alias T = Tuple!(int[2],int[2],int[2]);
    T[] toffsets;
    foreach (start1; 0 .. 15)
    foreach (end1; min(start1+1,15) .. 15)
    foreach (start2; end1 .. 15)
    foreach (end2; min(start2+1,15) .. 15)
    foreach (start3; end2 .. 15)
    foreach (end3; min(start3+1,15) .. 15)
            toffsets ~= T([start1,end1],[start2,end2],[start3,end3]);

    static void verify(O...)(int[] r, int len, int removed, bool stable, O offsets)
    {
        assert(r.length == len - removed);
        assert(!stable || r.isStrictlyMonotonic);
        assert(r.all!(e => all!(o => e < o[0] || e >= o[1])(offsets.only)));
    }

    foreach (offsets; AliasSeq!(soffsets,doffsets,toffsets))
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

// reverse
/**
Reverses $(D r) in-place.  Performs $(D r.length / 2) evaluations of $(D
swap).
Params:
    r = a $(REF_ALTTEXT bidirectional range, isBidirectionalRange, std,range,primitives)
    with swappable elements or a random access range with a length member

See_Also:
    $(HTTP sgi.com/tech/stl/_reverse.html, STL's _reverse), $(REF retro, std,range) for a lazy reversed range view
*/
void reverse(Range)(Range r)
if (isBidirectionalRange!Range && !isRandomAccessRange!Range
    && hasSwappableElements!Range)
{
    while (!r.empty)
    {
        swap(r.front, r.back);
        r.popFront();
        if (r.empty) break;
        r.popBack();
    }
}

///
@safe unittest
{
    int[] arr = [ 1, 2, 3 ];
    reverse(arr);
    assert(arr == [ 3, 2, 1 ]);
}

///ditto
void reverse(Range)(Range r)
if (isRandomAccessRange!Range && hasLength!Range)
{
    //swapAt is in fact the only way to swap non lvalue ranges
    immutable last = r.length-1;
    immutable steps = r.length/2;
    for (size_t i = 0; i < steps; i++)
    {
        r.swapAt(i, last-i);
    }
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
    reverse(range);
    assert(range == [3, 2, 1]);
}

/**
Reverses $(D r) in-place, where $(D r) is a narrow string (having
elements of type $(D char) or $(D wchar)). UTF sequences consisting of
multiple code units are preserved properly.

Params:
    s = a narrow string

Bugs:
    When passing a sting with unicode modifiers on characters, such as $(D \u0301),
    this function will not properly keep the position of the modifier. For example,
    reversing $(D ba\u0301d) ("bád") will result in d\u0301ab ("d́ab") instead of
    $(D da\u0301b) ("dáb").
*/
void reverse(Char)(Char[] s)
if (isNarrowString!(Char[]) && !is(Char == const) && !is(Char == immutable))
{
    import std.string : representation;
    import std.utf : stride;

    auto r = representation(s);
    for (size_t i = 0; i < s.length; )
    {
        immutable step = stride(s, i);
        if (step > 1)
        {
            .reverse(r[i .. i + step]);
            i += step;
        }
        else
        {
            ++i;
        }
    }
    reverse(r);
}

///
@safe unittest
{
    char[] arr = "hello\U00010143\u0100\U00010143".dup;
    reverse(arr);
    assert(arr == "\U00010143\u0100\U00010143olleh");
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

    The $(D stripLeft) function will strip the $(D front) of the range,
    the $(D stripRight) function will strip the $(D back) of the range,
    while the $(D strip) function will strip both the $(D front) and $(D back)
    of the range.

    Note that the $(D strip) and $(D stripRight) functions require the range to
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
Swaps $(D lhs) and $(D rhs). The instances $(D lhs) and $(D rhs) are moved in
memory, without ever calling $(D opAssign), nor any other function. $(D T)
need not be assignable at all to be swapped.

If $(D lhs) and $(D rhs) reference the same instance, then nothing is done.

$(D lhs) and $(D rhs) must be mutable. If $(D T) is a struct or union, then
its fields must also all be (recursively) mutable.

Params:
    lhs = Data to be swapped with $(D rhs).
    rhs = Data to be swapped with $(D lhs).
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

        // For non-struct types, suffice to do the classic swap
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

@safe unittest
{
    //Bug# 4789
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

@safe unittest
{
    //11853
    import std.traits : isAssignable;
    alias T = Tuple!(int, double);
    static assert(isAssignable!T);
}

@safe unittest
{
    // 12024
    import std.datetime;
    SysTime a, b;
    swap(a, b);
}

@system unittest // 9975
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
Swaps all elements of $(D r1) with successive elements in $(D r2).
Returns a tuple containing the remainder portions of $(D r1) and $(D
r2) that were not swapped (one of them will be empty). The ranges may
be of different types but must have the same element type and support
swapping.

Params:
    r1 = an $(REF_ALTTEXT input _range, isInputRange, std,_range,primitives)
         with swappable elements
    r2 = an $(REF_ALTTEXT input _range, isInputRange, std,_range,primitives)
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
Initializes each element of $(D range) with $(D value).
Assumes that the elements of the range are uninitialized.
This is of interest for structs that
define copy constructors (for all other types, $(LREF fill) and
uninitializedFill are equivalent).

Params:
        range = An
                $(REF_ALTTEXT input _range, isInputRange, std,_range,primitives)
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
        import std.conv : emplaceRef;

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
