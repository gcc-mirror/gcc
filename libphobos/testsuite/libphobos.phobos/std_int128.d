@safe unittest
{
    import std.int128;

        const Int128 a = Int128(0xffff_ffff_ffff_ffffL, 0x0123_4567_89ab_cdefL);
        assert(cast(long) a == 0x0123_4567_89ab_cdefL);
        assert(cast(int)  a ==           0x89ab_cdef);
        assert(cast(byte) a == cast(byte) 0xef);
    
}

@safe unittest
{
    import std.int128;

        const Int128 a = Int128(-1L << 60);
        assert(cast(double) a == -(2.0 ^^ 60));
        assert(cast(double) (a * a) == 2.0 ^^ 120);
    
}

@safe unittest
{
    import std.int128;

        import std.format : format;

        assert(format("%s", Int128.max) == "170141183460469231731687303715884105727");
        assert(format("%s", Int128.min) == "-170141183460469231731687303715884105728");
        assert(format("%x", Int128.max) == "7fffffffffffffffffffffffffffffff");
        assert(format("%X", Int128.max) == "7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
        assert(format("%032X", Int128(123L)) == "0000000000000000000000000000007B");
        assert(format("%+ 40d", Int128(123L)) == "                                    +123");
        assert(format("%+-40d", Int128(123L)) == "+123                                    ");
    
}

@safe unittest
{
    import std.int128;

        import std.conv : to;

        assert(to!wstring(Int128.max) == "170141183460469231731687303715884105727"w);
        assert(to!dstring(Int128.max) == "170141183460469231731687303715884105727"d);
    
}

@safe pure nothrow @nogc unittest
{
    import std.int128;

    Int128 c = Int128(5, 6);
    assert(c == c);
    assert(c == +c);
    assert(c == - -c);
    assert(~c == Int128(~5, ~6));
    ++c;
    assert(c == Int128(5, 7));
    assert(--c == Int128(5, 6));
    assert(!!c);
    assert(!Int128());

    assert(c + Int128(10, 20) == Int128(15, 26));
    assert(c - Int128(1, 2)   == Int128(4, 4));
    assert(c * Int128(100, 2) == Int128(610, 12));
    assert(c / Int128(3, 2)   == Int128(0, 1));
    assert(c % Int128(3, 2)   == Int128(2, 4));
    assert((c & Int128(3, 2)) == Int128(1, 2));
    assert((c | Int128(3, 2)) == Int128(7, 6));
    assert((c ^ Int128(3, 2)) == Int128(6, 4));

    assert(c + 15   == Int128(5, 21));
    assert(c - 15   == Int128(4, -9));
    assert(c * 15   == Int128(75, 90));
    assert(c / 15   == Int128(0, 6148914691236517205));
    assert(c % 15   == Int128(0, 11));
    assert((c & 15) == Int128(0, 6));
    assert((c | 15) == Int128(5, 15));
    assert((c ^ 15) == Int128(5, 9));

    assert(15 + c   == Int128(5, 21));
    assert(15 - c   == Int128(-5, 9));
    assert(15 * c   == Int128(75, 90));
    assert(15 / c   == Int128(0, 0));
    assert(15 % c   == Int128(0, 15));
    assert((15 & c) == Int128(0, 6));
    assert((15 | c) == Int128(5, 15));
    assert((15 ^ c) == Int128(5, 9));

    assert(c << 1 == Int128(10, 12));
    assert(-c >> 1 == Int128(-3, 9223372036854775805));
    assert(-c >>> 1 == Int128(9223372036854775805, 9223372036854775805));

    assert((c += 1) == Int128(5, 7));
    assert((c -= 1) == Int128(5, 6));
    assert((c += Int128(0, 1)) == Int128(5, 7));
    assert((c -= Int128(0, 1)) == Int128(5, 6));
    assert((c *= 2) == Int128(10, 12));
    assert((c /= 2) == Int128(5, 6));
    assert((c %= 2) == Int128());
    c += Int128(5, 6);
    assert((c *= Int128(10, 20)) == Int128(160, 120));
    assert((c /= Int128(10, 20)) == Int128(0, 15));
    c += Int128(72, 0);
    assert((c %= Int128(10, 20)) == Int128(1, -125));
    assert((c &= Int128(3, 20)) == Int128(1, 0));
    assert((c |= Int128(8, 2)) == Int128(9, 2));
    assert((c ^= Int128(8, 2)) == Int128(1, 0));
    c |= Int128(10, 5);
    assert((c <<= 1) == Int128(11 * 2, 5 * 2));
    assert((c >>>= 1) == Int128(11, 5));
    c = Int128(long.min, long.min);
    assert((c >>= 1) == Int128(long.min >> 1, cast(ulong) long.min >> 1));

    assert(-Int128.min == Int128.min);
    assert(Int128.max + 1 == Int128.min);

    c = Int128(5, 6);
    assert(c < Int128(6, 5));
    assert(c > 10);

    c = Int128(-1UL);
    assert(c == -1UL);
    c = Int128(-1L);
    assert(c == -1L);
}

