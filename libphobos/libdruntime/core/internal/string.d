/**
 * String manipulation and comparison utilities.
 *
 * Copyright: Copyright Sean Kelly 2005 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Sean Kelly, Walter Bright
 * Source: $(DRUNTIMESRC rt/util/_string.d)
 */

module core.internal.string;

pure:
nothrow:
@nogc:

alias UnsignedStringBuf = char[20];

/**
Converts an unsigned integer value to a string of characters.

This implementation is a template so it can be used when compiling with -betterC.

Params:
    value = the unsigned integer value to convert
    buf   = the pre-allocated buffer used to store the result
    radix = the numeric base to use in the conversion (defaults to 10)

Returns:
    The unsigned integer value as a string of characters
*/
char[] unsignedToTempString(uint radix = 10)(ulong value, return scope char[] buf) @safe
if (radix >= 2 && radix <= 16)
{
    size_t i = buf.length;
    do
    {
        uint x = void;
        if (value < radix)
        {
            x = cast(uint)value;
            value = 0;
        }
        else
        {
            x = cast(uint)(value % radix);
            value /= radix;
        }
        buf[--i] = cast(char)((radix <= 10 || x < 10) ? x + '0' : x - 10 + 'a');
    } while (value);
    return buf[i .. $];
}

private struct TempStringNoAlloc(ubyte N)
{
    private char[N] _buf = void;
    private ubyte _len;
    inout(char)[] get() inout return
    {
        return _buf[$-_len..$];
    }
    alias get this;
}

/**
Converts an unsigned integer value to a string of characters.

This implementation is a template so it can be used when compiling with -betterC.

Params:
    value = the unsigned integer value to convert
    radix = the numeric base to use in the conversion (defaults to 10)

Returns:
    The unsigned integer value as a string of characters
*/
auto unsignedToTempString(uint radix = 10)(ulong value) @safe
{
    // Need a buffer of 65 bytes for radix of 2 with room for
    // signedToTempString to possibly add a negative sign.
    enum bufferSize = radix >= 10 ? 20 : 65;
    TempStringNoAlloc!bufferSize result = void;
    result._len = unsignedToTempString!radix(value, result._buf).length & 0xff;
    return result;
}

unittest
{
    UnsignedStringBuf buf;
    assert(0.unsignedToTempString(buf) == "0");
    assert(1.unsignedToTempString(buf) == "1");
    assert(12.unsignedToTempString(buf) == "12");
    assert(0x12ABCF .unsignedToTempString!16(buf) == "12abcf");
    assert(long.sizeof.unsignedToTempString(buf) == "8");
    assert(uint.max.unsignedToTempString(buf) == "4294967295");
    assert(ulong.max.unsignedToTempString(buf) == "18446744073709551615");

    // use stack allocated struct version
    assert(0.unsignedToTempString == "0");
    assert(1.unsignedToTempString == "1");
    assert(12.unsignedToTempString == "12");
    assert(0x12ABCF .unsignedToTempString!16 == "12abcf");
    assert(long.sizeof.unsignedToTempString == "8");
    assert(uint.max.unsignedToTempString == "4294967295");
    assert(ulong.max.unsignedToTempString == "18446744073709551615");

    // test bad radices
    assert(!is(typeof(100.unsignedToTempString!1(buf))));
    assert(!is(typeof(100.unsignedToTempString!0(buf) == "")));
}

alias SignedStringBuf = char[20];

char[] signedToTempString(uint radix = 10)(long value, return scope char[] buf) @safe
{
    bool neg = value < 0;
    if (neg)
        value = cast(ulong)-value;
    auto r = unsignedToTempString!radix(value, buf);
    if (neg)
    {
        // about to do a slice without a bounds check
        auto trustedSlice(return scope char[] r) @trusted { assert(r.ptr > buf.ptr); return (r.ptr-1)[0..r.length+1]; }
        r = trustedSlice(r);
        r[0] = '-';
    }
    return r;
}

auto signedToTempString(uint radix = 10)(long value) @safe
{
    bool neg = value < 0;
    if (neg)
        value = cast(ulong)-value;
    auto r = unsignedToTempString!radix(value);
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
    assert(0.signedToTempString(buf) == "0");
    assert(1.signedToTempString(buf) == "1");
    assert((-1).signedToTempString(buf) == "-1");
    assert(12.signedToTempString(buf) == "12");
    assert((-12).signedToTempString(buf) == "-12");
    assert(0x12ABCF .signedToTempString!16(buf) == "12abcf");
    assert((-0x12ABCF) .signedToTempString!16(buf) == "-12abcf");
    assert(long.sizeof.signedToTempString(buf) == "8");
    assert(int.max.signedToTempString(buf) == "2147483647");
    assert(int.min.signedToTempString(buf) == "-2147483648");
    assert(long.max.signedToTempString(buf) == "9223372036854775807");
    assert(long.min.signedToTempString(buf) == "-9223372036854775808");

    // use stack allocated struct version
    assert(0.signedToTempString() == "0");
    assert(1.signedToTempString == "1");
    assert((-1).signedToTempString == "-1");
    assert(12.signedToTempString == "12");
    assert((-12).signedToTempString == "-12");
    assert(0x12ABCF .signedToTempString!16 == "12abcf");
    assert((-0x12ABCF) .signedToTempString!16 == "-12abcf");
    assert(long.sizeof.signedToTempString == "8");
    assert(int.max.signedToTempString == "2147483647");
    assert(int.min.signedToTempString == "-2147483648");
    assert(long.max.signedToTempString == "9223372036854775807");
    assert(long.min.signedToTempString == "-9223372036854775808");
    assert(long.max.signedToTempString!2 == "111111111111111111111111111111111111111111111111111111111111111");
    assert(long.min.signedToTempString!2 == "-1000000000000000000000000000000000000000000000000000000000000000");
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
int numDigits(uint radix = 10)(ulong value) @safe if (radix >= 2 && radix <= 36)
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

    // test bad radices
    static assert(!__traits(compiles, 100.numDigits!1()));
    static assert(!__traits(compiles, 100.numDigits!0()));
    static assert(!__traits(compiles, 100.numDigits!37()));
}

int dstrcmp()( scope const char[] s1, scope const char[] s2 ) @trusted
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
    return (s1.length > s2.length) - (s1.length < s2.length);
}
