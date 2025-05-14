@safe unittest
{
    import std.bitmanip;

    struct A
    {
        int a;
        mixin(bitfields!(
            uint, "x",    2,
            int,  "y",    3,
            uint, "z",    2,
            bool, "flag", 1));
    }

    A obj;
    obj.x = 2;
    obj.z = obj.x;

    assert(obj.x == 2);
    assert(obj.y == 0);
    assert(obj.z == 2);
    assert(obj.flag == false);
}

@safe unittest
{
    import std.bitmanip;

    struct A
    {
        mixin(bitfields!(
            bool, "flag1",    1,
            bool, "flag2",    1,
            uint, "",         6));
    }

    A a;
    assert(a.flag1 == 0);
    a.flag1 = 1;
    assert(a.flag1 == 1);
    a.flag1 = 0;
    assert(a.flag1 == 0);
}

@safe unittest
{
    import std.bitmanip;

    enum ABC { A, B, C }
    struct EnumTest
    {
        mixin(bitfields!(
                  ABC, "x", 2,
                  bool, "y", 1,
                  ubyte, "z", 5));
    }
}

@safe unittest
{
    import std.bitmanip;

    struct A
    {
        int a;
        mixin(taggedPointer!(
            uint*, "x",
            bool, "b1", 1,
            bool, "b2", 1));
    }
    A obj;
    obj.x = new uint;
    obj.b1 = true;
    obj.b2 = false;
}

@safe unittest
{
    import std.bitmanip;

    struct A
    {
        int a;
        mixin(taggedClassRef!(
            Object, "o",
            uint, "i", 2));
    }
    A obj;
    obj.o = new Object();
    obj.i = 3;
}

@safe unittest
{
    import std.bitmanip;

    FloatRep rep = {value: 0};
    assert(rep.fraction == 0);
    assert(rep.exponent == 0);
    assert(!rep.sign);

    rep.value = 42;
    assert(rep.fraction == 2621440);
    assert(rep.exponent == 132);
    assert(!rep.sign);

    rep.value = 10;
    assert(rep.fraction == 2097152);
    assert(rep.exponent == 130);
}

@safe unittest
{
    import std.bitmanip;

    FloatRep rep = {value: 1};
    assert(rep.fraction == 0);
    assert(rep.exponent == 127);
    assert(!rep.sign);

    rep.exponent = 126;
    assert(rep.value == 0.5);

    rep.exponent = 130;
    assert(rep.value == 8);
}

@safe unittest
{
    import std.bitmanip;

    FloatRep rep = {value: 1};
    rep.value = -0.5;
    assert(rep.fraction == 0);
    assert(rep.exponent == 126);
    assert(rep.sign);

    rep.value = -1. / 3;
    assert(rep.fraction == 2796203);
    assert(rep.exponent == 125);
    assert(rep.sign);
}

@safe unittest
{
    import std.bitmanip;

    DoubleRep rep = {value: 0};
    assert(rep.fraction == 0);
    assert(rep.exponent == 0);
    assert(!rep.sign);

    rep.value = 42;
    assert(rep.fraction == 1407374883553280);
    assert(rep.exponent == 1028);
    assert(!rep.sign);

    rep.value = 10;
    assert(rep.fraction == 1125899906842624);
    assert(rep.exponent == 1026);
}

@safe unittest
{
    import std.bitmanip;

    DoubleRep rep = {value: 1};
    assert(rep.fraction == 0);
    assert(rep.exponent == 1023);
    assert(!rep.sign);

    rep.exponent = 1022;
    assert(rep.value == 0.5);

    rep.exponent = 1026;
    assert(rep.value == 8);
}

@safe unittest
{
    import std.bitmanip;

    DoubleRep rep = {value: 1};
    rep.value = -0.5;
    assert(rep.fraction == 0);
    assert(rep.exponent == 1022);
    assert(rep.sign);

    rep.value = -1. / 3;
    assert(rep.fraction == 1501199875790165);
    assert(rep.exponent == 1021);
    assert(rep.sign);
}

@safe unittest
{
    import std.bitmanip;

    DoubleRep x;
    x.value = 1.0;
    assert(x.fraction == 0 && x.exponent == 1023 && !x.sign);
    x.value = -0.5;
    assert(x.fraction == 0 && x.exponent == 1022 && x.sign);
    x.value = 0.5;
    assert(x.fraction == 0 && x.exponent == 1022 && !x.sign);
}

@safe unittest
{
    import std.bitmanip;

    DoubleRep x;
    x.fraction = 1125899906842624;
    x.exponent = 1025;
    x.sign = true;
    assert(x.value == -5.0);
}

@system unittest
{
    import std.bitmanip;

        import std.algorithm.comparison : equal;

        bool[] input = [true, false, false, true, true];
        auto a = BitArray(input);
        assert(a.length == 5);
        assert(a.bitsSet.equal([0, 3, 4]));

        // This also works because an implicit cast to bool[] occurs for this array.
        auto b = BitArray([0, 0, 1]);
        assert(b.length == 3);
        assert(b.bitsSet.equal([2]));
    
}

@system unittest
{
    import std.bitmanip;

        import std.algorithm.comparison : equal;
        import std.array : array;
        import std.range : iota, repeat;

        BitArray a = true.repeat(70).array;
        assert(a.length == 70);
        assert(a.bitsSet.equal(iota(0, 70)));
    
}

@system unittest
{
    import std.bitmanip;

        import std.algorithm.comparison : equal;

        auto a = BitArray([1, 0, 0, 1, 1]);

        // Inverse of the cast.
        auto v = cast(void[]) a;
        auto b = BitArray(v, a.length);

        assert(b.length == 5);
        assert(b.bitsSet.equal([0, 3, 4]));

        // a and b share the underlying data.
        a[0] = 0;
        assert(b[0] == 0);
        assert(a == b);
    
}

@system unittest
{
    import std.bitmanip;

        import std.algorithm.comparison : equal;

        size_t[] source = [0b1100, 0b0011];
        enum sbits = size_t.sizeof * 8;
        auto ba = BitArray(source, source.length * sbits);
        // The least significant bit in each unit is this unit's starting bit.
        assert(ba.bitsSet.equal([2, 3, sbits, sbits + 1]));
    
}

@system unittest
{
    import std.bitmanip;

        // Example from the doc for this constructor.
        static immutable size_t[] sourceData = [1, 0b101, 3, 3424234, 724398, 230947, 389492];
        size_t[] source = sourceData.dup;
        enum sbits = size_t.sizeof * 8;
        auto ba = BitArray(source, source.length * sbits);
        foreach (n; 0 .. source.length * sbits)
        {
            auto nth_bit = cast(bool) (source[n / sbits] & (1L << (n % sbits)));
            assert(ba[n] == nth_bit);
        }

        // Example of mapping only part of the array.
        import std.algorithm.comparison : equal;

        auto bc = BitArray(source, sbits + 1);
        assert(bc.bitsSet.equal([0, sbits]));
        // Source array has not been modified.
        assert(source == sourceData);
    
}

@system unittest
{
    import std.bitmanip;

        static void fun(const BitArray arr)
        {
            auto x = arr[0];
            assert(x == 1);
        }
        BitArray a;
        a.length = 3;
        a[0] = 1;
        fun(a);
    
}

@system pure nothrow unittest
{
    import std.bitmanip;

        import std.algorithm.comparison : equal;

        auto b = BitArray([1, 0, 1, 0, 1, 1]);

        b[] = true;
        // all bits are set
        assert(b.bitsSet.equal([0, 1, 2, 3, 4, 5]));

        b[] = false;
        // none of the bits are set
        assert(b.bitsSet.empty);
    
}

@system pure nothrow unittest
{
    import std.bitmanip;

        import std.algorithm.comparison : equal;
        import std.range : iota;
        import std.stdio;

        auto b = BitArray([1, 0, 0, 0, 1, 1, 0]);
        b[1 .. 3] = true;
        assert(b.bitsSet.equal([0, 1, 2, 4, 5]));

        bool[72] bitArray;
        auto b1 = BitArray(bitArray);
        b1[63 .. 67] = true;
        assert(b1.bitsSet.equal([63, 64, 65, 66]));
        b1[63 .. 67] = false;
        assert(b1.bitsSet.empty);
        b1[0 .. 64] = true;
        assert(b1.bitsSet.equal(iota(0, 64)));
        b1[0 .. 64] = false;
        assert(b1.bitsSet.empty);

        bool[256] bitArray2;
        auto b2 = BitArray(bitArray2);
        b2[3 .. 245] = true;
        assert(b2.bitsSet.equal(iota(3, 245)));
        b2[3 .. 245] = false;
        assert(b2.bitsSet.empty);
    
}

@system pure nothrow unittest
{
    import std.bitmanip;

        import std.algorithm.comparison : equal;
        import std.range : iota;

        // positions 0, 2, 4 are set
        auto b = BitArray([1, 0, 1, 0, 1, 0]);
        b.flip();
        // after flipping, positions 1, 3, 5 are set
        assert(b.bitsSet.equal([1, 3, 5]));

        bool[270] bits;
        auto b1 = BitArray(bits);
        b1.flip();
        assert(b1.bitsSet.equal(iota(0, 270)));
    
}

@system pure nothrow unittest
{
    import std.bitmanip;

        auto ax = BitArray([1, 0, 0, 1]);
        ax.flip(0);
        assert(ax[0] == 0);

        bool[200] y;
        y[90 .. 130] = true;
        auto ay = BitArray(y);
        ay.flip(100);
        assert(ay[100] == 0);
    
}

@system pure nothrow unittest
{
    import std.bitmanip;

        auto a = BitArray([0, 1, 1, 0, 0, 1, 1]);
        assert(a.count == 4);

        BitArray b;
        assert(b.count == 0);

        bool[200] boolArray;
        boolArray[45 .. 130] = true;
        auto c = BitArray(boolArray);
        assert(c.count == 85);
    
}

@system unittest
{
    import std.bitmanip;

        BitArray a;
        BitArray b;

        a.length = 3;
        a[0] = 1; a[1] = 0; a[2] = 1;
        b = a.dup;
        assert(b.length == 3);
        foreach (i; 0 .. 3)
            assert(b[i] == (((i ^ 1) & 1) ? true : false));
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0,1];

        auto a = BitArray(ba);

        int i;
        foreach (b;a)
        {
            switch (i)
            {
                case 0: assert(b == true); break;
                case 1: assert(b == false); break;
                case 2: assert(b == true); break;
                default: assert(0);
            }
            i++;
        }

        foreach (j,b;a)
        {
            switch (j)
            {
                case 0: assert(b == true); break;
                case 1: assert(b == false); break;
                case 2: assert(b == true); break;
                default: assert(0);
            }
        }
    
}

@system unittest
{
    import std.bitmanip;

        BitArray b;
        bool[5] data = [1,0,1,1,0];

        b = BitArray(data);
        b.reverse;
        foreach (i; 0 .. data.length)
            assert(b[i] == data[4 - i]);
    
}

@system unittest
{
    import std.bitmanip;

        size_t x = 0b1100011000;
        auto ba = BitArray(10, &x);
        ba.sort;
        foreach (i; 0 .. 6)
            assert(ba[i] == false);
        foreach (i; 6 .. 10)
            assert(ba[i] == true);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1];
        bool[] bc = [1,0,1,0,1,0,1];
        bool[] bd = [1,0,1,1,1];
        bool[] be = [1,0,1,0,1];
        bool[] bf = [1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];
        bool[] bg = [1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1];

        auto a = BitArray(ba);
        auto b = BitArray(bb);
        auto c = BitArray(bc);
        auto d = BitArray(bd);
        auto e = BitArray(be);
        auto f = BitArray(bf);
        auto g = BitArray(bg);

        assert(a != b);
        assert(a != c);
        assert(a != d);
        assert(a == e);
        assert(f != g);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1];
        bool[] bc = [1,0,1,0,1,0,1];
        bool[] bd = [1,0,1,1,1];
        bool[] be = [1,0,1,0,1];
        bool[] bf = [1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1];
        bool[] bg = [1,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);
        auto c = BitArray(bc);
        auto d = BitArray(bd);
        auto e = BitArray(be);
        auto f = BitArray(bf);
        auto g = BitArray(bg);

        assert(a >  b);
        assert(a >= b);
        assert(a <  c);
        assert(a <= c);
        assert(a <  d);
        assert(a <= d);
        assert(a == e);
        assert(a <= e);
        assert(a >= e);
        assert(f <  g);
        assert(g <= g);
    
}

@system unittest
{
    import std.bitmanip;

        import std.array : array;
        import std.range : repeat, take;

        // bit array with 300 elements
        auto a = BitArray(true.repeat.take(300).array);
        size_t[] v = cast(size_t[]) a;
        const blockSize = size_t.sizeof * 8;
        assert(v.length == (a.length + blockSize - 1) / blockSize);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0,1,0,1];

        auto a = BitArray(ba);
        BitArray b = ~a;

        assert(b[0] == 0);
        assert(b[1] == 1);
        assert(b[2] == 0);
        assert(b[3] == 1);
        assert(b[4] == 0);
    
}

@system unittest
{
    import std.bitmanip;

        static bool[] ba = [1,0,1,0,1];
        static bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        BitArray c = a & b;

        assert(c[0] == 1);
        assert(c[1] == 0);
        assert(c[2] == 1);
        assert(c[3] == 0);
        assert(c[4] == 0);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        BitArray c = a | b;

        assert(c[0] == 1);
        assert(c[1] == 0);
        assert(c[2] == 1);
        assert(c[3] == 1);
        assert(c[4] == 1);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        BitArray c = a ^ b;

        assert(c[0] == 0);
        assert(c[1] == 0);
        assert(c[2] == 0);
        assert(c[3] == 1);
        assert(c[4] == 1);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        BitArray c = a - b;

        assert(c[0] == 0);
        assert(c[1] == 0);
        assert(c[2] == 0);
        assert(c[3] == 0);
        assert(c[4] == 1);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0,1,0,1,1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];
        auto a = BitArray(ba);
        auto b = BitArray(bb);
        BitArray c = a;
        c.length = 5;
        c &= b;
        assert(a[5] == 1);
        assert(a[6] == 0);
        assert(a[7] == 1);
        assert(a[8] == 0);
        assert(a[9] == 1);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        a &= b;
        assert(a[0] == 1);
        assert(a[1] == 0);
        assert(a[2] == 1);
        assert(a[3] == 0);
        assert(a[4] == 0);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        a |= b;
        assert(a[0] == 1);
        assert(a[1] == 0);
        assert(a[2] == 1);
        assert(a[3] == 1);
        assert(a[4] == 1);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        a ^= b;
        assert(a[0] == 0);
        assert(a[1] == 0);
        assert(a[2] == 0);
        assert(a[3] == 1);
        assert(a[4] == 1);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0,1,0,1];
        bool[] bb = [1,0,1,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);

        a -= b;
        assert(a[0] == 0);
        assert(a[1] == 0);
        assert(a[2] == 0);
        assert(a[3] == 0);
        assert(a[4] == 1);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0,1,0,1];

        auto a = BitArray(ba);
        BitArray b;

        b = (a ~= true);
        assert(a[0] == 1);
        assert(a[1] == 0);
        assert(a[2] == 1);
        assert(a[3] == 0);
        assert(a[4] == 1);
        assert(a[5] == 1);

        assert(b == a);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0];
        bool[] bb = [0,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);
        BitArray c;

        c = (a ~= b);
        assert(a.length == 5);
        assert(a[0] == 1);
        assert(a[1] == 0);
        assert(a[2] == 0);
        assert(a[3] == 1);
        assert(a[4] == 0);

        assert(c == a);
    
}

@system unittest
{
    import std.bitmanip;

        bool[] ba = [1,0];
        bool[] bb = [0,1,0];

        auto a = BitArray(ba);
        auto b = BitArray(bb);
        BitArray c;

        c = (a ~ b);
        assert(c.length == 5);
        assert(c[0] == 1);
        assert(c[1] == 0);
        assert(c[2] == 0);
        assert(c[3] == 1);
        assert(c[4] == 0);

        c = (a ~ true);
        assert(c.length == 3);
        assert(c[0] == 1);
        assert(c[1] == 0);
        assert(c[2] == 1);

        c = (false ~ a);
        assert(c.length == 3);
        assert(c[0] == 0);
        assert(c[1] == 1);
        assert(c[2] == 0);
    
}

@system unittest
{
    import std.bitmanip;

        import std.format : format;

        auto b = BitArray([1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1]);

        b <<= 1;
        assert(format("%b", b) == "01100_10101101");

        b >>= 1;
        assert(format("%b", b) == "11001_01011010");

        b <<= 4;
        assert(format("%b", b) == "00001_10010101");

        b >>= 5;
        assert(format("%b", b) == "10010_10100000");

        b <<= 13;
        assert(format("%b", b) == "00000_00000000");

        b = BitArray([1, 0, 1, 1, 0, 1, 1, 1]);
        b >>= 8;
        assert(format("%b", b) == "00000000");

    
}

@system pure unittest
{
    import std.bitmanip;

        import std.format : format;

        auto b = BitArray([0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);

        auto s1 = format("%s", b);
        assert(s1 == "[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]");

        auto s2 = format("%b", b);
        assert(s2 == "00001111_00001111");
    
}

@system unittest
{
    import std.bitmanip;

        import std.algorithm.comparison : equal;

        auto b1 = BitArray([0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);
        assert(b1.bitsSet.equal([4, 5, 6, 7, 12, 13, 14, 15]));

        BitArray b2;
        b2.length = 1000;
        b2[333] = true;
        b2[666] = true;
        b2[999] = true;
        assert(b2.bitsSet.equal([333, 666, 999]));
    
}

@system unittest
{
    import std.bitmanip;

    import std.algorithm.comparison : equal;
    import std.range : iota;

    bool[] buf = new bool[64 * 3];
    buf[0 .. 64] = true;
    BitArray b = BitArray(buf);
    assert(b.bitsSet.equal(iota(0, 64)));
    b <<= 64;
    assert(b.bitsSet.equal(iota(64, 128)));
}

@system unittest
{
    import std.bitmanip;

    import std.algorithm.comparison : equal;

    auto b = BitArray([1, 0]);
    b ~= true;
    assert(b[2] == 1);
    b ~= BitArray([0, 1]);
    auto c = BitArray([1, 0, 1, 0, 1]);
    assert(b == c);
    assert(b.bitsSet.equal([0, 2, 4]));
}

@system unittest
{
    import std.bitmanip;

    import std.algorithm.comparison : equal;

    auto b = BitArray([1, 1, 0, 1]);
    b &= BitArray([0, 1, 1, 0]);
    assert(b.bitsSet.equal([1]));
    b.flip;
    assert(b.bitsSet.equal([0, 2, 3]));
}

@system unittest
{
    import std.bitmanip;

    import std.format : format;
    auto b = BitArray([1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%b", b) == "1_00001111_00001111");
}

@system unittest
{
    import std.bitmanip;

    import std.format : format;

    BitArray b;

    b = BitArray([]);
    assert(format("%s", b) == "[]");
    assert(format("%b", b) is null);

    b = BitArray([1]);
    assert(format("%s", b) == "[1]");
    assert(format("%b", b) == "1");

    b = BitArray([0, 0, 0, 0]);
    assert(format("%b", b) == "0000");

    b = BitArray([0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%s", b) == "[0, 0, 0, 0, 1, 1, 1, 1]");
    assert(format("%b", b) == "00001111");

    b = BitArray([0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%s", b) == "[0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]");
    assert(format("%b", b) == "00001111_00001111");

    b = BitArray([1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%b", b) == "1_00001111");

    b = BitArray([1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1]);
    assert(format("%b", b) == "1_00001111_00001111");
}

@safe unittest
{
    import std.bitmanip;

    assert(42.swapEndian == 704643072);
    assert(42.swapEndian.swapEndian == 42); // reflexive
    assert(1.swapEndian == 16777216);

    assert(true.swapEndian == true);
    assert(byte(10).swapEndian == 10);
    assert(char(10).swapEndian == 10);

    assert(ushort(10).swapEndian == 2560);
    assert(long(10).swapEndian == 720575940379279360);
    assert(ulong(10).swapEndian == 720575940379279360);
}

@safe unittest
{
    import std.bitmanip;

    int i = 12345;
    ubyte[4] swappedI = nativeToBigEndian(i);
    assert(i == bigEndianToNative!int(swappedI));

    float f = 123.45f;
    ubyte[4] swappedF = nativeToBigEndian(f);
    assert(f == bigEndianToNative!float(swappedF));

    const float cf = 123.45f;
    ubyte[4] swappedCF = nativeToBigEndian(cf);
    assert(cf == bigEndianToNative!float(swappedCF));

    double d = 123.45;
    ubyte[8] swappedD = nativeToBigEndian(d);
    assert(d == bigEndianToNative!double(swappedD));

    const double cd = 123.45;
    ubyte[8] swappedCD = nativeToBigEndian(cd);
    assert(cd == bigEndianToNative!double(swappedCD));
}

@safe unittest
{
    import std.bitmanip;

    ushort i = 12345;
    ubyte[2] swappedI = nativeToBigEndian(i);
    assert(i == bigEndianToNative!ushort(swappedI));

    dchar c = 'D';
    ubyte[4] swappedC = nativeToBigEndian(c);
    assert(c == bigEndianToNative!dchar(swappedC));
}

@safe unittest
{
    import std.bitmanip;

    int i = 12345;
    ubyte[4] swappedI = nativeToLittleEndian(i);
    assert(i == littleEndianToNative!int(swappedI));

    float f = 123.45f;
    ubyte[4] swappedF = nativeToLittleEndian(f);
    assert(f == littleEndianToNative!float(swappedF));

    const float cf = 123.45f;
    ubyte[4] swappedCF = nativeToLittleEndian(cf);
    assert(cf == littleEndianToNative!float(swappedCF));

    double d = 123.45;
    ubyte[8] swappedD = nativeToLittleEndian(d);
    assert(d == littleEndianToNative!double(swappedD));

    const double cd = 123.45;
    ubyte[8] swappedCD = nativeToLittleEndian(cd);
    assert(cd == littleEndianToNative!double(swappedCD));
}

@safe unittest
{
    import std.bitmanip;

    ushort i = 12345;
    ubyte[2] swappedI = nativeToLittleEndian(i);
    assert(i == littleEndianToNative!ushort(swappedI));

    dchar c = 'D';
    ubyte[4] swappedC = nativeToLittleEndian(c);
    assert(c == littleEndianToNative!dchar(swappedC));
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [1, 5, 22, 9, 44, 255, 8];
    assert(buffer.peek!uint() == 17110537);
    assert(buffer.peek!ushort() == 261);
    assert(buffer.peek!ubyte() == 1);

    assert(buffer.peek!uint(2) == 369700095);
    assert(buffer.peek!ushort(2) == 5641);
    assert(buffer.peek!ubyte(2) == 22);

    size_t index = 0;
    assert(buffer.peek!ushort(&index) == 261);
    assert(index == 2);

    assert(buffer.peek!uint(&index) == 369700095);
    assert(index == 6);

    assert(buffer.peek!ubyte(&index) == 8);
    assert(index == 7);
}

@safe unittest
{
    import std.bitmanip;

    import std.algorithm.iteration : filter;
    ubyte[] buffer = [1, 5, 22, 9, 44, 255, 7];
    auto range = filter!"true"(buffer);
    assert(range.peek!uint() == 17110537);
    assert(range.peek!ushort() == 261);
    assert(range.peek!ubyte() == 1);
}

@safe unittest
{
    import std.bitmanip;

    import std.range.primitives : empty;
    ubyte[] buffer = [1, 5, 22, 9, 44, 255, 8];
    assert(buffer.length == 7);

    assert(buffer.read!ushort() == 261);
    assert(buffer.length == 5);

    assert(buffer.read!uint() == 369700095);
    assert(buffer.length == 1);

    assert(buffer.read!ubyte() == 8);
    assert(buffer.empty);
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0];
    buffer.write!uint(29110231u, 0);
    assert(buffer == [1, 188, 47, 215, 0, 0, 0, 0]);

    buffer.write!ushort(927, 0);
    assert(buffer == [3, 159, 47, 215, 0, 0, 0, 0]);

    buffer.write!ubyte(42, 0);
    assert(buffer == [42, 159, 47, 215, 0, 0, 0, 0]);
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0, 0];
    buffer.write!uint(142700095u, 2);
    assert(buffer == [0, 0, 8, 129, 110, 63, 0, 0, 0]);

    buffer.write!ushort(19839, 2);
    assert(buffer == [0, 0, 77, 127, 110, 63, 0, 0, 0]);

    buffer.write!ubyte(132, 2);
    assert(buffer == [0, 0, 132, 127, 110, 63, 0, 0, 0]);
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0];
    size_t index = 0;
    buffer.write!ushort(261, &index);
    assert(buffer == [1, 5, 0, 0, 0, 0, 0, 0]);
    assert(index == 2);

    buffer.write!uint(369700095u, &index);
    assert(buffer == [1, 5, 22, 9, 44, 255, 0, 0]);
    assert(index == 6);

    buffer.write!ubyte(8, &index);
    assert(buffer == [1, 5, 22, 9, 44, 255, 8, 0]);
    assert(index == 7);
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [0, 0];
    buffer.write!bool(false, 0);
    assert(buffer == [0, 0]);

    buffer.write!bool(true, 0);
    assert(buffer == [1, 0]);

    buffer.write!bool(true, 1);
    assert(buffer == [1, 1]);

    buffer.write!bool(false, 1);
    assert(buffer == [1, 0]);

    size_t index = 0;
    buffer.write!bool(false, &index);
    assert(buffer == [0, 0]);
    assert(index == 1);

    buffer.write!bool(true, &index);
    assert(buffer == [0, 1]);
    assert(index == 2);
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [0, 0, 0];

    buffer.write!char('a', 0);
    assert(buffer == [97, 0, 0]);

    buffer.write!char('b', 1);
    assert(buffer == [97, 98, 0]);

    size_t index = 0;
    buffer.write!char('a', &index);
    assert(buffer == [97, 98, 0]);
    assert(index == 1);

    buffer.write!char('b', &index);
    assert(buffer == [97, 98, 0]);
    assert(index == 2);

    buffer.write!char('c', &index);
    assert(buffer == [97, 98, 99]);
    assert(index == 3);
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [0, 0, 0, 0];

    buffer.write!wchar('ą', 0);
    assert(buffer == [1, 5, 0, 0]);

    buffer.write!wchar('”', 2);
    assert(buffer == [1, 5, 32, 29]);

    size_t index = 0;
    buffer.write!wchar('ć', &index);
    assert(buffer == [1, 7, 32, 29]);
    assert(index == 2);

    buffer.write!wchar('ą', &index);
    assert(buffer == [1, 7, 1, 5]);
    assert(index == 4);
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0];

    buffer.write!dchar('ą', 0);
    assert(buffer == [0, 0, 1, 5, 0, 0, 0, 0]);

    buffer.write!dchar('”', 4);
    assert(buffer == [0, 0, 1, 5, 0, 0, 32, 29]);

    size_t index = 0;
    buffer.write!dchar('ć', &index);
    assert(buffer == [0, 0, 1, 7, 0, 0, 32, 29]);
    assert(index == 4);

    buffer.write!dchar('ą', &index);
    assert(buffer == [0, 0, 1, 7, 0, 0, 1, 5]);
    assert(index == 8);
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0];

    buffer.write!float(32.0f, 0);
    assert(buffer == [66, 0, 0, 0, 0, 0, 0, 0]);

    buffer.write!float(25.0f, 4);
    assert(buffer == [66, 0, 0, 0, 65, 200, 0, 0]);

    size_t index = 0;
    buffer.write!float(25.0f, &index);
    assert(buffer == [65, 200, 0, 0, 65, 200, 0, 0]);
    assert(index == 4);

    buffer.write!float(32.0f, &index);
    assert(buffer == [65, 200, 0, 0, 66, 0, 0, 0]);
    assert(index == 8);
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    buffer.write!double(32.0, 0);
    assert(buffer == [64, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);

    buffer.write!double(25.0, 8);
    assert(buffer == [64, 64, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0]);

    size_t index = 0;
    buffer.write!double(25.0, &index);
    assert(buffer == [64, 57, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0]);
    assert(index == 8);

    buffer.write!double(32.0, &index);
    assert(buffer == [64, 57, 0, 0, 0, 0, 0, 0, 64, 64, 0, 0, 0, 0, 0, 0]);
    assert(index == 16);
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    enum Foo
    {
        one = 10,
        two = 20,
        three = 30
    }

    buffer.write!Foo(Foo.one, 0);
    assert(buffer == [0, 0, 0, 10, 0, 0, 0, 0, 0, 0, 0, 0]);

    buffer.write!Foo(Foo.two, 4);
    assert(buffer == [0, 0, 0, 10, 0, 0, 0, 20, 0, 0, 0, 0]);

    buffer.write!Foo(Foo.three, 8);
    assert(buffer == [0, 0, 0, 10, 0, 0, 0, 20, 0, 0, 0, 30]);

    size_t index = 0;
    buffer.write!Foo(Foo.three, &index);
    assert(buffer == [0, 0, 0, 30, 0, 0, 0, 20, 0, 0, 0, 30]);
    assert(index == 4);

    buffer.write!Foo(Foo.one, &index);
    assert(buffer == [0, 0, 0, 30, 0, 0, 0, 10, 0, 0, 0, 30]);
    assert(index == 8);

    buffer.write!Foo(Foo.two, &index);
    assert(buffer == [0, 0, 0, 30, 0, 0, 0, 10, 0, 0, 0, 20]);
    assert(index == 12);
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0];

    enum Float: float
    {
        one = 32.0f,
        two = 25.0f
    }

    buffer.write!Float(Float.one, 0);
    assert(buffer == [66, 0, 0, 0, 0, 0, 0, 0]);

    buffer.write!Float(Float.two, 4);
    assert(buffer == [66, 0, 0, 0, 65, 200, 0, 0]);

    size_t index = 0;
    buffer.write!Float(Float.two, &index);
    assert(buffer == [65, 200, 0, 0, 65, 200, 0, 0]);
    assert(index == 4);

    buffer.write!Float(Float.one, &index);
    assert(buffer == [65, 200, 0, 0, 66, 0, 0, 0]);
    assert(index == 8);
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    enum Double: double
    {
        one = 32.0,
        two = 25.0
    }

    buffer.write!Double(Double.one, 0);
    assert(buffer == [64, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]);

    buffer.write!Double(Double.two, 8);
    assert(buffer == [64, 64, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0]);

    size_t index = 0;
    buffer.write!Double(Double.two, &index);
    assert(buffer == [64, 57, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0]);
    assert(index == 8);

    buffer.write!Double(Double.one, &index);
    assert(buffer == [64, 57, 0, 0, 0, 0, 0, 0, 64, 64, 0, 0, 0, 0, 0, 0]);
    assert(index == 16);
}

@system unittest
{
    import std.bitmanip;

    ubyte[] buffer = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    enum Real: real
    {
        one = 32.0,
        two = 25.0
    }

    static assert(!__traits(compiles, buffer.write!Real(Real.one)));
}

@safe unittest
{
    import std.bitmanip;

    import std.array;
    auto buffer = appender!(const ubyte[])();
    buffer.append!ushort(261);
    assert(buffer.data == [1, 5]);

    buffer.append!uint(369700095u);
    assert(buffer.data == [1, 5, 22, 9, 44, 255]);

    buffer.append!ubyte(8);
    assert(buffer.data == [1, 5, 22, 9, 44, 255, 8]);
}

@safe unittest
{
    import std.bitmanip;

    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    buffer.append!bool(true);
    assert(buffer.data == [1]);

    buffer.append!bool(false);
    assert(buffer.data == [1, 0]);
}

@safe unittest
{
    import std.bitmanip;

    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    buffer.append!char('a');
    assert(buffer.data == [97]);

    buffer.append!char('b');
    assert(buffer.data == [97, 98]);

    buffer.append!wchar('ą');
    assert(buffer.data == [97, 98, 1, 5]);

    buffer.append!dchar('ą');
        assert(buffer.data == [97, 98, 1, 5, 0, 0, 1, 5]);
}

@safe unittest
{
    import std.bitmanip;

    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    buffer.append!float(32.0f);
    assert(buffer.data == [66, 0, 0, 0]);

    buffer.append!double(32.0);
    assert(buffer.data == [66, 0, 0, 0, 64, 64, 0, 0, 0, 0, 0, 0]);
}

@safe unittest
{
    import std.bitmanip;

    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    enum Foo
    {
        one = 10,
        two = 20,
        three = 30
    }

    buffer.append!Foo(Foo.one);
    assert(buffer.data == [0, 0, 0, 10]);

    buffer.append!Foo(Foo.two);
    assert(buffer.data == [0, 0, 0, 10, 0, 0, 0, 20]);

    buffer.append!Foo(Foo.three);
    assert(buffer.data == [0, 0, 0, 10, 0, 0, 0, 20, 0, 0, 0, 30]);
}

@safe unittest
{
    import std.bitmanip;

    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    enum Bool: bool
    {
        bfalse = false,
        btrue = true,
    }

    buffer.append!Bool(Bool.btrue);
    assert(buffer.data == [1]);

    buffer.append!Bool(Bool.bfalse);
    assert(buffer.data == [1, 0]);

    buffer.append!Bool(Bool.btrue);
    assert(buffer.data == [1, 0, 1]);
}

@safe unittest
{
    import std.bitmanip;

    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    enum Float: float
    {
        one = 32.0f,
        two = 25.0f
    }

    buffer.append!Float(Float.one);
    assert(buffer.data == [66, 0, 0, 0]);

    buffer.append!Float(Float.two);
    assert(buffer.data == [66, 0, 0, 0, 65, 200, 0, 0]);
}

@safe unittest
{
    import std.bitmanip;

    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    enum Double: double
    {
        one = 32.0,
        two = 25.0
    }

    buffer.append!Double(Double.one);
    assert(buffer.data == [64, 64, 0, 0, 0, 0, 0, 0]);

    buffer.append!Double(Double.two);
    assert(buffer.data == [64, 64, 0, 0, 0, 0, 0, 0, 64, 57, 0, 0, 0, 0, 0, 0]);
}

@safe unittest
{
    import std.bitmanip;

    import std.array : appender;
    auto buffer = appender!(const ubyte[])();

    enum Real: real
    {
        one = 32.0,
        two = 25.0
    }

    static assert(!__traits(compiles, buffer.append!Real(Real.one)));
}

@safe unittest
{
    import std.bitmanip;

    import std.algorithm.comparison : equal;
    import std.range : iota;

    assert(bitsSet(1).equal([0]));
    assert(bitsSet(5).equal([0, 2]));
    assert(bitsSet(-1).equal(iota(32)));
    assert(bitsSet(int.min).equal([31]));
}

