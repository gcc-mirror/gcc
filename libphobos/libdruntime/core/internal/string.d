/**
 * String manipulation and comparison utilities.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly, Walter Bright
 * Source: $(DRUNTIMESRC src/rt/util/_string.d)
 */

module core.internal.string;

pure:
nothrow:
@nogc:

alias UnsignedStringBuf = char[20];

char[] unsignedToTempString(ulong value, return char[] buf, uint radix = 10) @safe
{
    size_t i = buf.length;
    do
    {
        ubyte x = cast(ubyte)(value % radix);
        value = value / radix;
        buf[--i] = cast(char)((x < 10) ? x + '0' : x - 10 + 'a');
    } while (value);
    return buf[i .. $];
}

private struct TempStringNoAlloc
{
    // need to handle 65 bytes for radix of 2 with negative sign.
    private char[65] _buf;
    private ubyte _len;
    auto get() return
    {
        return _buf[$-_len..$];
    }
    alias get this;
}

auto unsignedToTempString(ulong value, uint radix) @safe
{
    TempStringNoAlloc result = void;
    result._len = unsignedToTempString(value, result._buf, radix).length & 0xff;
    return result;
}

unittest
{
    UnsignedStringBuf buf;
    assert(0.unsignedToTempString(buf, 10) == "0");
    assert(1.unsignedToTempString(buf, 10) == "1");
    assert(12.unsignedToTempString(buf, 10) == "12");
    assert(0x12ABCF .unsignedToTempString(buf, 16) == "12abcf");
    assert(long.sizeof.unsignedToTempString(buf, 10) == "8");
    assert(uint.max.unsignedToTempString(buf, 10) == "4294967295");
    assert(ulong.max.unsignedToTempString(buf, 10) == "18446744073709551615");

    // use stack allocated struct version
    assert(0.unsignedToTempString(10) == "0");
    assert(1.unsignedToTempString(10) == "1");
    assert(12.unsignedToTempString(10) == "12");
    assert(0x12ABCF .unsignedToTempString(16) == "12abcf");
    assert(long.sizeof.unsignedToTempString(10) == "8");
    assert(uint.max.unsignedToTempString(10) == "4294967295");
    assert(ulong.max.unsignedToTempString(10) == "18446744073709551615");
}

alias SignedStringBuf = char[20];

char[] signedToTempString(long value, return char[] buf, uint radix) @safe
{
    bool neg = value < 0;
    if (neg)
        value = cast(ulong)-value;
    auto r = unsignedToTempString(value, buf, radix);
    if (neg)
    {
        // about to do a slice without a bounds check
        auto trustedSlice(return char[] r) @trusted { assert(r.ptr > buf.ptr); return (r.ptr-1)[0..r.length+1]; }
        r = trustedSlice(r);
        r[0] = '-';
    }
    return r;
}

auto signedToTempString(long value, uint radix) @safe
{
    bool neg = value < 0;
    if (neg)
        value = cast(ulong)-value;
    auto r = unsignedToTempString(value, radix);
    if (neg)
    {
        r._len++;
        r.get()[0] = '-';
    }
    return r;
}

unittest
{
    SignedStringBuf buf;
    assert(0.signedToTempString(buf, 10) == "0");
    assert(1.signedToTempString(buf, 10) == "1");
    assert((-1).signedToTempString(buf, 10) == "-1");
    assert(12.signedToTempString(buf, 10) == "12");
    assert((-12).signedToTempString(buf, 10) == "-12");
    assert(0x12ABCF .signedToTempString(buf, 16) == "12abcf");
    assert((-0x12ABCF) .signedToTempString(buf, 16) == "-12abcf");
    assert(long.sizeof.signedToTempString(buf, 10) == "8");
    assert(int.max.signedToTempString(buf, 10) == "2147483647");
    assert(int.min.signedToTempString(buf, 10) == "-2147483648");
    assert(long.max.signedToTempString(buf, 10) == "9223372036854775807");
    assert(long.min.signedToTempString(buf, 10) == "-9223372036854775808");

    // use stack allocated struct version
    assert(0.signedToTempString(10) == "0");
    assert(1.signedToTempString(10) == "1");
    assert((-1).signedToTempString(10) == "-1");
    assert(12.signedToTempString(10) == "12");
    assert((-12).signedToTempString(10) == "-12");
    assert(0x12ABCF .signedToTempString(16) == "12abcf");
    assert((-0x12ABCF) .signedToTempString(16) == "-12abcf");
    assert(long.sizeof.signedToTempString(10) == "8");
    assert(int.max.signedToTempString(10) == "2147483647");
    assert(int.min.signedToTempString(10) == "-2147483648");
    assert(long.max.signedToTempString(10) == "9223372036854775807");
    assert(long.min.signedToTempString(10) == "-9223372036854775808");
    assert(long.max.signedToTempString(2) == "111111111111111111111111111111111111111111111111111111111111111");
    assert(long.min.signedToTempString(2) == "-1000000000000000000000000000000000000000000000000000000000000000");
}


/********************************
 * Determine number of digits that will result from a
 * conversion of value to a string.
 * Params:
 *      value = number to convert
 *      radix = radix
 * Returns:
 *      number of digits
 */
int numDigits(uint radix = 10)(ulong value) @safe
{
     int n = 1;
     while (1)
     {
        if (value <= uint.max)
        {
            uint v = cast(uint)value;
            while (1)
            {
                if (v < radix)
                    return n;
                if (v < radix * radix)
                    return n + 1;
                if (v < radix * radix * radix)
                    return n + 2;
                if (v < radix * radix * radix * radix)
                    return n + 3;
                n += 4;
                v /= radix * radix * radix * radix;
            }
        }
        n += 4;
        value /= radix * radix * radix * radix;
     }
}

unittest
{
    assert(0.numDigits == 1);
    assert(9.numDigits == 1);
    assert(10.numDigits == 2);
    assert(99.numDigits == 2);
    assert(100.numDigits == 3);
    assert(999.numDigits == 3);
    assert(1000.numDigits == 4);
    assert(9999.numDigits == 4);
    assert(10000.numDigits == 5);
    assert(99999.numDigits == 5);
    assert(uint.max.numDigits == 10);
    assert(ulong.max.numDigits == 20);

    assert(0.numDigits!2 == 1);
    assert(1.numDigits!2 == 1);
    assert(2.numDigits!2 == 2);
    assert(3.numDigits!2 == 2);
}

int dstrcmp( scope const char[] s1, scope const char[] s2 ) @trusted
{
    immutable len = s1.length <= s2.length ? s1.length : s2.length;
    if (__ctfe)
    {
        foreach (const u; 0 .. len)
        {
            if (s1[u] != s2[u])
                return s1[u] > s2[u] ? 1 : -1;
        }
    }
    else
    {
        import core.stdc.string : memcmp;

        const ret = memcmp( s1.ptr, s2.ptr, len );
        if ( ret )
            return ret;
    }
    return s1.length < s2.length ? -1 : (s1.length > s2.length);
}
