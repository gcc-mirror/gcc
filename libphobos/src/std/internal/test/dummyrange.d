/**
For testing only.
Used with the dummy ranges for testing higher order ranges.
*/
module std.internal.test.dummyrange;

import std.meta;
import std.range.primitives;
import std.typecons;

enum RangeType
{
    Input,
    Forward,
    Bidirectional,
    Random
}

enum Length
{
    Yes,
    No
}

enum ReturnBy
{
    Reference,
    Value
}

import std.traits : isArray;

// Range that's useful for testing other higher order ranges,
// can be parametrized with attributes.  It just dumbs down an array of
// numbers 1 .. 10.
struct DummyRange(ReturnBy _r, Length _l, RangeType _rt, T = uint[])
if (isArray!T)
{
    private static immutable uinttestData =
        [1U, 2U, 3U, 4U, 5U, 6U, 7U, 8U, 9U, 10U];
    // These enums are so that the template params are visible outside
    // this instantiation.
    enum r = _r;
    enum l = _l;
    enum rt = _rt;

    static if (is(T == uint[]))
    {
        T arr = uinttestData;
    }
    else
    {
        T arr;
    }

    alias RetType = ElementType!(T);
    alias RetTypeNoAutoDecoding = ElementEncodingType!(T);

    void reinit()
    {
        // Workaround for DMD bug 4378
        static if (is(T == uint[]))
        {
            arr = uinttestData.dup;
        }
    }

    void popFront()
    {
        arr = arr[1..$];
    }

    @property bool empty() const
    {
        return arr.length == 0;
    }

    static if (r == ReturnBy.Reference)
    {
        @property ref inout(RetType) front() inout
        {
            return arr[0];
        }
    }
    else
    {
        @property RetType front() const
        {
            return arr[0];
        }

        @property void front(RetTypeNoAutoDecoding val)
        {
            arr[0] = val;
        }
    }

    static if (rt >= RangeType.Forward)
    {
        @property typeof(this) save()
        {
            return this;
        }
    }

    static if (rt >= RangeType.Bidirectional)
    {
        void popBack()
        {
            arr = arr[0..$ - 1];
        }

        static if (r == ReturnBy.Reference)
        {
            @property ref inout(RetType) back() inout
            {
                return arr[$ - 1];
            }
        }
        else
        {
            @property RetType back() const
            {
                return arr[$ - 1];
            }

            @property void back(RetTypeNoAutoDecoding val)
            {
                arr[$ - 1] = val;
            }
        }
    }

    static if (rt >= RangeType.Random)
    {
        static if (r == ReturnBy.Reference)
        {
            ref inout(RetType) opIndex(size_t index) inout
            {
                return arr[index];
            }
        }
        else
        {
            RetType opIndex(size_t index) const
            {
                return arr[index];
            }

            RetType opIndexAssign(RetTypeNoAutoDecoding val, size_t index)
            {
                return arr[index] = val;
            }

            RetType opIndexOpAssign(string op)(RetTypeNoAutoDecoding value, size_t index)
            {
                mixin("return arr[index] " ~ op ~ "= value;");
            }

            RetType opIndexUnary(string op)(size_t index)
            {
                mixin("return " ~ op ~ "arr[index];");
            }
        }

        typeof(this) opSlice(size_t lower, size_t upper)
        {
            auto ret = this;
            ret.arr = arr[lower .. upper];
            return ret;
        }

        typeof(this) opSlice()
        {
            return this;
        }
    }

    static if (l == Length.Yes)
    {
        @property size_t length() const
        {
            return arr.length;
        }

        alias opDollar = length;
    }
}

enum dummyLength = 10;

alias AllDummyRanges = AliasSeq!(
    DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Forward),
    DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Bidirectional),
    DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random),
    DummyRange!(ReturnBy.Reference, Length.No, RangeType.Forward),
    DummyRange!(ReturnBy.Reference, Length.No, RangeType.Bidirectional),
    DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Input),
    DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Forward),
    DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Bidirectional),
    DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Random),
    DummyRange!(ReturnBy.Value, Length.No, RangeType.Input),
    DummyRange!(ReturnBy.Value, Length.No, RangeType.Forward),
    DummyRange!(ReturnBy.Value, Length.No, RangeType.Bidirectional)
);

template AllDummyRangesType(T)
{
    alias AllDummyRangesType = AliasSeq!(
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Forward, T),
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Bidirectional, T),
        DummyRange!(ReturnBy.Reference, Length.Yes, RangeType.Random, T),
        DummyRange!(ReturnBy.Reference, Length.No, RangeType.Forward, T),
        DummyRange!(ReturnBy.Reference, Length.No, RangeType.Bidirectional, T),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Input, T),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Forward, T),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Bidirectional, T),
        DummyRange!(ReturnBy.Value, Length.Yes, RangeType.Random, T),
        DummyRange!(ReturnBy.Value, Length.No, RangeType.Input, T),
        DummyRange!(ReturnBy.Value, Length.No, RangeType.Forward, T),
        DummyRange!(ReturnBy.Value, Length.No, RangeType.Bidirectional, T)
    );
}

/**
Tests whether forward, bidirectional and random access properties are
propagated properly from the base range(s) R to the higher order range
H.  Useful in combination with DummyRange for testing several higher
order ranges.
*/
template propagatesRangeType(H, R...)
{
    static if (allSatisfy!(isRandomAccessRange, R))
        enum bool propagatesRangeType = isRandomAccessRange!H;
    else static if (allSatisfy!(isBidirectionalRange, R))
        enum bool propagatesRangeType = isBidirectionalRange!H;
    else static if (allSatisfy!(isForwardRange, R))
        enum bool propagatesRangeType = isForwardRange!H;
    else
        enum bool propagatesRangeType = isInputRange!H;
}

template propagatesLength(H, R...)
{
    static if (allSatisfy!(hasLength, R))
        enum bool propagatesLength = hasLength!H;
    else
        enum bool propagatesLength = !hasLength!H;
}

/**
Reference type input range
*/
class ReferenceInputRange(T)
{
    import std.array : array;

    this(Range)(Range r) if (isInputRange!Range) {_payload = array(r);}
    final @property ref T front(){return _payload.front;}
    final void popFront(){_payload.popFront();}
    final @property bool empty(){return _payload.empty;}
    protected T[] _payload;
}

/**
Infinite input range
*/
class ReferenceInfiniteInputRange(T)
{
    this(T first = T.init) {_val = first;}
    final @property T front(){return _val;}
    final void popFront(){++_val;}
    enum bool empty = false;
    protected T _val;
}

/**
Reference forward range
*/
class ReferenceForwardRange(T) : ReferenceInputRange!T
{
    this(Range)(Range r) if (isInputRange!Range) {super(r);}
    final @property auto save(this This)() {return new This( _payload);}
}

/**
Infinite forward range
*/
class ReferenceInfiniteForwardRange(T) : ReferenceInfiniteInputRange!T
{
    this(T first = T.init) {super(first);}
    final @property ReferenceInfiniteForwardRange save()
    {return new ReferenceInfiniteForwardRange!T(_val);}
}

/**
Reference bidirectional range
*/
class ReferenceBidirectionalRange(T) : ReferenceForwardRange!T
{
    this(Range)(Range r) if (isInputRange!Range) {super(r);}
    final @property ref T back(){return _payload.back;}
    final void popBack(){_payload.popBack();}
}

@safe unittest
{
    static assert(isInputRange!(ReferenceInputRange!int));
    static assert(isInputRange!(ReferenceInfiniteInputRange!int));

    static assert(isForwardRange!(ReferenceForwardRange!int));
    static assert(isForwardRange!(ReferenceInfiniteForwardRange!int));

    static assert(isBidirectionalRange!(ReferenceBidirectionalRange!int));
}

private:

pure struct Cmp(T)
if (is(T == uint))
{
    static auto iota(size_t low = 1, size_t high = 11)
    {
        import std.range : iota;
        return iota(cast(uint) low, cast(uint) high);
    }

    static void initialize(ref uint[] arr)
    {
        import std.array : array;
        arr = iota().array;
    }

    static bool function(uint,uint) cmp = function(uint a, uint b) { return a == b; };

    enum dummyValue = 1337U;
    enum dummyValueRslt = 1337U * 2;
}

pure struct Cmp(T)
if (is(T == double))
{
    import std.math : approxEqual;

    static auto iota(size_t low = 1, size_t high = 11)
    {
        import std.range : iota;
        return iota(cast(double) low, cast(double) high, 1.0);
    }

    static void initialize(ref double[] arr)
    {
        import std.array : array;
        arr = iota().array;
    }

    alias cmp = approxEqual!(double,double);

    enum dummyValue = 1337.0;
    enum dummyValueRslt = 1337.0 * 2.0;
}

struct TestFoo
{
    int a;

    bool opEquals(const ref TestFoo other) const
    {
        return this.a == other.a;
    }

    TestFoo opBinary(string op)(TestFoo other)
    {
        TestFoo ret = this;
        mixin("ret.a " ~ op ~ "= other.a;");
        return ret;
    }

    TestFoo opOpAssign(string op)(TestFoo other)
    {
        mixin("this.a " ~ op ~ "= other.a;");
        return this;
    }
}

pure struct Cmp(T)
if (is(T == TestFoo))
{
    import std.math : approxEqual;

    static auto iota(size_t low = 1, size_t high = 11)
    {
        import std.algorithm.iteration : map;
        import std.range : iota;
        return iota(cast(int) low, cast(int) high).map!(a => TestFoo(a));
    }

    static void initialize(ref TestFoo[] arr)
    {
        import std.array : array;
        arr = iota().array;
    }

    static bool function(TestFoo,TestFoo) cmp = function(TestFoo a, TestFoo b)
    {
        return a.a == b.a;
    };

    @property static TestFoo dummyValue()
    {
        return TestFoo(1337);
    }

    @property static TestFoo dummyValueRslt()
    {
        return TestFoo(1337 * 2);
    }
}

@system unittest
{
    import std.algorithm.comparison : equal;
    import std.range : iota, retro, repeat;
    import std.traits : Unqual;

    static void testInputRange(T,Cmp)()
    {
        T it;
        Cmp.initialize(it.arr);
        for (size_t numRuns = 0; numRuns < 2; ++numRuns)
        {
            if (numRuns == 1)
            {
                static if (is(Unqual!(ElementType!(T)) == uint))
                {
                    it.reinit();
                }

                Cmp.initialize(it.arr);
            }

            assert(equal!(Cmp.cmp)(it, Cmp.iota(1, 11)));

            static if (hasLength!T)
            {
                assert(it.length == 10);
            }

            assert(!Cmp.cmp(it.front, Cmp.dummyValue));
            auto s = it.front;
            it.front = Cmp.dummyValue;
            assert(Cmp.cmp(it.front, Cmp.dummyValue));
            it.front = s;

            auto cmp = Cmp.iota(1,11);

            size_t jdx = 0;
            while (!it.empty && !cmp.empty)
            {
                static if (hasLength!T)
                {
                    assert(it.length == 10 - jdx);
                }

                assert(Cmp.cmp(it.front, cmp.front));
                it.popFront();
                cmp.popFront();

                ++jdx;
            }

            assert(it.empty);
            assert(cmp.empty);
        }

    }

    static void testForwardRange(T,Cmp)()
    {
        T it;
        Cmp.initialize(it.arr);
        auto s = it.save();
        s.popFront();
        assert(!Cmp.cmp(s.front, it.front));
    }

    static void testBidirectionalRange(T,Cmp)()
    {
        T it;
        Cmp.initialize(it.arr);
        assert(equal!(Cmp.cmp)(it.retro, Cmp.iota().retro));

        auto s = it.back;
        assert(!Cmp.cmp(s, Cmp.dummyValue));
        it.back = Cmp.dummyValue;
        assert( Cmp.cmp(it.back, Cmp.dummyValue));
        it.back = s;
    }

    static void testRandomAccessRange(T,Cmp)()
    {
        T it;
        Cmp.initialize(it.arr);
        size_t idx = 0;
        foreach (jt; it)
        {
            assert(it[idx] == jt);

            T copy = it[idx .. $];
            auto cmp = Cmp.iota(idx + 1, it.length + 1);
            assert(equal!(Cmp.cmp)(copy, cmp));

            ++idx;
        }

        {
            auto copy = it;
            copy.arr = it.arr.dup;
            for (size_t i = 0; i < copy.length; ++i)
            {
                copy[i] = Cmp.dummyValue;
                copy[i] += Cmp.dummyValue;
            }
            assert(equal!(Cmp.cmp)(copy, Cmp.dummyValueRslt.repeat(copy.length)));
        }

        static if (it.r == ReturnBy.Reference)
        {
            T copy;
            copy.arr = it.arr.dup;
            for (size_t i = 0; i < copy.length; ++i)
            {
                copy[i] = Cmp.dummyValue;
                copy[i] += Cmp.dummyValue;
            }

            assert(equal!(Cmp.cmp)(copy, Cmp.dummyValueRslt.repeat(copy.length)));
        }
    }

    import std.meta : AliasSeq;

    foreach (S; AliasSeq!(uint, double, TestFoo))
    {
        foreach (T; AllDummyRangesType!(S[]))
        {
            testInputRange!(T,Cmp!S)();

            static if (isForwardRange!T)
            {
                testForwardRange!(T,Cmp!S)();
            }

            static if (isBidirectionalRange!T)
            {
                testBidirectionalRange!(T,Cmp!S)();
            }

            static if (isRandomAccessRange!T)
            {
                testRandomAccessRange!(T,Cmp!S)();
            }
        }
    }
}
