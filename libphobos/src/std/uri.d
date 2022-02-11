// Written in the D programming language.

/**
 * Encode and decode Uniform Resource Identifiers (URIs).
 * URIs are used in internet transfer protocols.
 * Valid URI characters consist of letters, digits,
 * and the characters $(B ;/?:@&amp;=+$,-_.!~*'())
 * Reserved URI characters are $(B ;/?:@&amp;=+$,)
 * Escape sequences consist of $(B %) followed by two hex digits.
 *
 * See_Also:
 *  $(LINK2 https://www.ietf.org/rfc/rfc3986.txt, RFC 3986)<br>
 *  $(LINK2 http://en.wikipedia.org/wiki/Uniform_resource_identifier, Wikipedia)
 * Copyright: Copyright The D Language Foundation 2000 - 2009.
 * License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   $(HTTP digitalmars.com, Walter Bright)
 * Source:    $(PHOBOSSRC std/uri.d)
 */
/*          Copyright The D Language Foundation 2000 - 2009.
 * Distributed under the Boost Software License, Version 1.0.
 *    (See accompanying file LICENSE_1_0.txt or copy at
 *          http://www.boost.org/LICENSE_1_0.txt)
 */
module std.uri;

//debug=uri;        // uncomment to turn on debugging writefln's
debug(uri) import std.stdio;
import std.traits : isSomeChar;

/** This Exception is thrown if something goes wrong when encoding or
decoding a URI.
*/
class URIException : Exception
{
    import std.exception : basicExceptionCtors;
    mixin basicExceptionCtors;
}

///
@safe unittest
{
    import std.exception : assertThrown;
    assertThrown!URIException("%ab".decode);
}

private enum
{
    URI_Alpha = 1,
    URI_Reserved = 2,
    URI_Mark = 4,
    URI_Digit = 8,
    URI_Hash = 0x10,        // '#'
}

private immutable char[16] hex2ascii = "0123456789ABCDEF";

private immutable ubyte[128] uri_flags =      // indexed by character
    ({
        ubyte[128] uflags;

        // Compile time initialize
        uflags['#'] |= URI_Hash;

        foreach (c; 'A' .. 'Z' + 1)
        {
            uflags[c] |= URI_Alpha;
            uflags[c + 0x20] |= URI_Alpha;   // lowercase letters
        }
        foreach (c; '0' .. '9' + 1) uflags[c] |= URI_Digit;
        foreach (c; ";/?:@&=+$,")   uflags[c] |= URI_Reserved;
        foreach (c; "-_.!~*'()")    uflags[c] |= URI_Mark;
        return uflags;
    })();

private string URI_Encode(dstring str, uint unescapedSet) @safe pure
{
    uint j;
    uint k;
    dchar V;
    dchar C;

    // result buffer
    char[50] buffer = void;
    char[] R;
    uint Rlen;
    uint Rsize; // alloc'd size

    immutable len = str.length;

    R = buffer[];
    Rsize = buffer.length;
    Rlen = 0;

    for (k = 0; k != len; k++)
    {
        C = str[k];
        // if (C in unescapedSet)
        if (C < uri_flags.length && uri_flags[C] & unescapedSet)
        {
            if (Rlen == Rsize)
            {
                char[] R2;

                Rsize *= 2;
                R2 = new char[Rsize];
                R2[0 .. Rlen] = R[0 .. Rlen];
                R = R2;
            }
            R[Rlen] = cast(char) C;
            Rlen++;
        }
        else
        {
            char[6] Octet;
            uint L;

            V = C;

            // Transform V into octets
            if (V <= 0x7F)
            {
                Octet[0] = cast(char) V;
                L = 1;
            }
            else if (V <= 0x7FF)
            {
                Octet[0] = cast(char)(0xC0 | (V >> 6));
                Octet[1] = cast(char)(0x80 | (V & 0x3F));
                L = 2;
            }
            else if (V <= 0xFFFF)
            {
                Octet[0] = cast(char)(0xE0 | (V >> 12));
                Octet[1] = cast(char)(0x80 | ((V >> 6) & 0x3F));
                Octet[2] = cast(char)(0x80 | (V & 0x3F));
                L = 3;
            }
            else if (V <= 0x1FFFFF)
            {
                Octet[0] = cast(char)(0xF0 | (V >> 18));
                Octet[1] = cast(char)(0x80 | ((V >> 12) & 0x3F));
                Octet[2] = cast(char)(0x80 | ((V >> 6) & 0x3F));
                Octet[3] = cast(char)(0x80 | (V & 0x3F));
                L = 4;
            }
            else
            {
                throw new URIException("Undefined UTF-32 code point");
            }

            if (Rlen + L * 3 > Rsize)
            {
                char[] R2;

                Rsize = 2 * (Rlen + L * 3);
                R2 = new char[Rsize];
                R2[0 .. Rlen] = R[0 .. Rlen];
                R = R2;
            }

            for (j = 0; j < L; j++)
            {
                R[Rlen] = '%';
                R[Rlen + 1] = hex2ascii[Octet[j] >> 4];
                R[Rlen + 2] = hex2ascii[Octet[j] & 15];

                Rlen += 3;
            }
        }
    }

    return R[0 .. Rlen].idup;
}

@safe pure unittest
{
    import std.exception : assertThrown;

    assert(URI_Encode("", 0) == "");
    assert(URI_Encode(URI_Decode("%F0%BF%BF%BF", 0), 0) == "%F0%BF%BF%BF");
    dstring a;
    a ~= cast(dchar) 0xFFFFFFFF;
    assertThrown(URI_Encode(a, 0));
    assert(URI_Encode("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 0).length == 3 * 60);
}

private uint ascii2hex(dchar c) @nogc @safe pure nothrow
{
    return (c <= '9') ? c - '0' :
        (c <= 'F') ? c - 'A' + 10 :
        c - 'a' + 10;
}

private dstring URI_Decode(Char)(scope const(Char)[] uri, uint reservedSet)
if (isSomeChar!Char)
{
    import std.ascii : isHexDigit;

    uint j;
    uint k;
    uint V;
    dchar C;

    uint Rlen;
    immutable len = uri.length;
    auto s = uri;

    auto Rsize = len;
    dchar[] R = new dchar[Rsize];
    Rlen = 0;

    for (k = 0; k != len; k++)
    {
        char B;
        uint start;

        C = s[k];
        if (C != '%')
        {
            R[Rlen] = C;
            Rlen++;
            continue;
        }
        start = k;
        if (k + 2 >= len)
            throw new URIException("Unexpected end of URI");
        if (!isHexDigit(s[k + 1]) || !isHexDigit(s[k + 2]))
            throw new URIException("Expected two hexadecimal digits after '%'");
        B = cast(char)((ascii2hex(s[k + 1]) << 4) + ascii2hex(s[k + 2]));
        k += 2;
        if ((B & 0x80) == 0)
        {
            C = B;
        }
        else
        {
            uint n;

            for (n = 1; ; n++)
            {
                if (n > 4)
                    throw new URIException("UTF-32 code point size too large");
                if (((B << n) & 0x80) == 0)
                {
                    if (n == 1)
                        throw new URIException("UTF-32 code point size too small");
                    break;
                }
            }

            // Pick off (7 - n) significant bits of B from first byte of octet
            V = B & ((1 << (7 - n)) - 1);   // (!!!)

            if (k + (3 * (n - 1)) >= len)
                throw new URIException("UTF-32 unaligned String");
            for (j = 1; j != n; j++)
            {
                k++;
                if (s[k] != '%')
                    throw new URIException("Expected: '%'");
                if (!isHexDigit(s[k + 1]) || !isHexDigit(s[k + 2]))
                    throw new URIException("Expected two hexadecimal digits after '%'");
                B = cast(char)((ascii2hex(s[k + 1]) << 4) + ascii2hex(s[k + 2]));
                if ((B & 0xC0) != 0x80)
                    throw new URIException("Incorrect UTF-32 multi-byte sequence");
                k += 2;
                V = (V << 6) | (B & 0x3F);
            }
            if (V > 0x10FFFF)
                throw new URIException("Unknown UTF-32 code point");
            C = V;
        }
        if (C < uri_flags.length && uri_flags[C] & reservedSet)
        {
            // R ~= s[start .. k + 1];
            immutable width = (k + 1) - start;
            for (int ii = 0; ii < width; ii++)
                R[Rlen + ii] = s[start + ii];
            Rlen += width;
        }
        else
        {
            R[Rlen] = C;
            Rlen++;
        }
    }
    assert(Rlen <= Rsize);  // enforce our preallocation size guarantee

    // Copy array on stack to array in memory
    return R[0 .. Rlen].idup;
}

@safe pure unittest
{
    import std.exception : assertThrown;

    assert(URI_Decode("", 0) == "");
    assertThrown!URIException(URI_Decode("%", 0));
    assertThrown!URIException(URI_Decode("%xx", 0));
    assertThrown!URIException(URI_Decode("%FF", 0));
    assertThrown!URIException(URI_Decode("%C0", 0));
    assertThrown!URIException(URI_Decode("%C0000000", 0));
    assertThrown!URIException(URI_Decode("%C0%xx0000", 0));
    assertThrown!URIException(URI_Decode("%C0%C00000", 0));
    assertThrown!URIException(URI_Decode("%F7%BF%BF%BF", 0));
    assert(URI_Decode("%23", URI_Hash) == "%23");
}

/*************************************
 * Decodes the URI string encodedURI into a UTF-8 string and returns it.
 * Escape sequences that resolve to reserved URI characters are not replaced.
 * Escape sequences that resolve to the '#' character are not replaced.
 */
string decode(Char)(scope const(Char)[] encodedURI)
if (isSomeChar!Char)
{
    import std.algorithm.iteration : each;
    import std.utf : encode;
    auto s = URI_Decode(encodedURI, URI_Reserved | URI_Hash);
    char[] r;
    s.each!(c => encode(r, c));
    return r;
}

///
@safe unittest
{
    assert("foo%20bar".decode == "foo bar");
    assert("%3C%3E.@.%E2%84%A2".decode == "<>.@.™");
    assert("foo&/".decode == "foo&/");
    assert("!@#$&*(".decode == "!@#$&*(");
}

/*******************************
 * Decodes the URI string encodedURI into a UTF-8 string and returns it. All
 * escape sequences are decoded.
 */
string decodeComponent(Char)(scope const(Char)[] encodedURIComponent)
if (isSomeChar!Char)
{
    import std.algorithm.iteration : each;
    import std.utf : encode;
    auto s = URI_Decode(encodedURIComponent, 0);
    char[] r;
    s.each!(c => encode(r, c));
    return r;
}

///
@safe unittest
{
    assert("foo%2F%26".decodeComponent == "foo/&");
    assert("dl%C3%A4ng%20r%C3%B6cks".decodeComponent == "dläng röcks");
    assert("!%40%23%24%25%5E%26*(".decodeComponent == "!@#$%^&*(");
}

/*****************************
 * Encodes the UTF-8 string uri into a URI and returns that URI. Any character
 * not a valid URI character is escaped. The '#' character is not escaped.
 */
string encode(Char)(scope const(Char)[] uri)
if (isSomeChar!Char)
{
    import std.utf : toUTF32;
    auto s = toUTF32(uri);
    return URI_Encode(s, URI_Reserved | URI_Hash | URI_Alpha | URI_Digit | URI_Mark);
}

///
@safe unittest
{
    assert("foo bar".encode == "foo%20bar");
    assert("<>.@.™".encode == "%3C%3E.@.%E2%84%A2");
    assert("foo/#?a=1&b=2".encode == "foo/#?a=1&b=2");
    assert("dlang+rocks!".encode == "dlang+rocks!");
    assert("!@#$%^&*(".encode == "!@#$%25%5E&*(");
}

/********************************
 * Encodes the UTF-8 string uriComponent into a URI and returns that URI.
 * Any character not a letter, digit, or one of -_.!~*'() is escaped.
 */
string encodeComponent(Char)(scope const(Char)[] uriComponent)
if (isSomeChar!Char)
{
    import std.utf : toUTF32;
    auto s = toUTF32(uriComponent);
    return URI_Encode(s, URI_Alpha | URI_Digit | URI_Mark);
}

///
@safe unittest
{
    assert("!@#$%^&*(".encodeComponent == "!%40%23%24%25%5E%26*(");
    assert("<>.@.™".encodeComponent == "%3C%3E.%40.%E2%84%A2");
    assert("foo/&".encodeComponent == "foo%2F%26");
    assert("dläng röcks".encodeComponent == "dl%C3%A4ng%20r%C3%B6cks");
    assert("dlang+rocks!".encodeComponent == "dlang%2Brocks!");
}

/* Encode associative array using www-form-urlencoding
 *
 * Params:
 *      values = an associative array containing the values to be encoded.
 *
 * Returns:
 *      A string encoded using www-form-urlencoding.
 */
package string urlEncode(scope string[string] values) @safe pure
{
    if (values.length == 0)
        return "";

    import std.array : Appender;
    import std.format.write : formattedWrite;

    Appender!string enc;
    enc.reserve(values.length * 128);

    bool first = true;
    foreach (k, v; values)
    {
        if (!first)
            enc.put('&');
        formattedWrite(enc, "%s=%s", encodeComponent(k), encodeComponent(v));
        first = false;
    }
    return enc.data;
}

@safe pure unittest
{
    // @system because urlEncode -> encodeComponent -> URI_Encode
    // URI_Encode uses alloca and pointer slicing
    string[string] a;
    assert(urlEncode(a) == "");
    assert(urlEncode(["name1" : "value1"]) == "name1=value1");
    auto enc = urlEncode(["name1" : "value1", "name2" : "value2"]);
    assert(enc == "name1=value1&name2=value2" || enc == "name2=value2&name1=value1");
}

/***************************
 * Does string s[] start with a URL?
 * Returns:
 *  -1   it does not
 *  len  it does, and s[0 .. len] is the slice of s[] that is that URL
 */

ptrdiff_t uriLength(Char)(scope const(Char)[] s)
if (isSomeChar!Char)
{
    /* Must start with one of:
     *  http://
     *  https://
     *  www.
     */
    import std.ascii : isAlphaNum;
    import std.uni : icmp;

    ptrdiff_t i;

    if (s.length <= 4)
        return -1;

    if (s.length > 7 && icmp(s[0 .. 7], "http://") == 0)
    {
        i = 7;
    }
    else
    {
        if (s.length > 8 && icmp(s[0 .. 8], "https://") == 0)
            i = 8;
        else
            return -1;
    }

    ptrdiff_t lastdot;
    for (; i < s.length; i++)
    {
        auto c = s[i];
        if (isAlphaNum(c))
            continue;
        if (c == '-' || c == '_' || c == '?' ||
                c == '=' || c == '%' || c == '&' ||
                c == '/' || c == '+' || c == '#' ||
                c == '~' || c == '$')
            continue;
        if (c == '.')
        {
            lastdot = i;
            continue;
        }
        break;
    }
    if (!lastdot)
        return -1;

    return i;
}

///
@safe pure unittest
{
    string s1 = "http://www.digitalmars.com/~fred/fredsRX.html#foo end!";
    assert(uriLength(s1) == 49);
    string s2 = "no uri here";
    assert(uriLength(s2) == -1);
    assert(uriLength("issue 14924") < 0);
}

@safe pure nothrow @nogc unittest
{
    assert(uriLength("") == -1);
    assert(uriLength("https://www") == -1);
}

/***************************
 * Does string s[] start with an email address?
 * Returns:
 *  -1    it does not
 *  len   it does, and s[0 .. i] is the slice of s[] that is that email address
 * References:
 *  RFC2822
 */
ptrdiff_t emailLength(Char)(scope const(Char)[] s)
if (isSomeChar!Char)
{
    import std.ascii : isAlpha, isAlphaNum;

    ptrdiff_t i;

    if (s.length == 0)
        return -1;

    if (!isAlpha(s[0]))
        return -1;

    for (i = 1; 1; i++)
    {
        if (i == s.length)
            return -1;
        auto c = s[i];
        if (isAlphaNum(c))
            continue;
        if (c == '-' || c == '_' || c == '.')
            continue;
        if (c != '@')
            return -1;
        i++;
        break;
    }

    /* Now do the part past the '@'
     */
    ptrdiff_t lastdot;
    for (; i < s.length; i++)
    {
        auto c = s[i];
        if (isAlphaNum(c))
            continue;
        if (c == '-' || c == '_')
            continue;
        if (c == '.')
        {
            lastdot = i;
            continue;
        }
        break;
    }
    if (!lastdot || (i - lastdot != 3 && i - lastdot != 4))
        return -1;

    return i;
}

///
@safe pure unittest
{
    string s1 = "my.e-mail@www.example-domain.com with garbage added";
    assert(emailLength(s1) == 32);
    string s2 = "no email address here";
    assert(emailLength(s2) == -1);
    assert(emailLength("issue 14924") < 0);
}

@safe pure unittest
{
    //@system because of encode -> URI_Encode
    debug(uri) writeln("uri.encodeURI.unittest");

    string source = "http://www.digitalmars.com/~fred/fred's RX.html#foo";
    string target = "http://www.digitalmars.com/~fred/fred's%20RX.html#foo";

    auto result = encode(source);
    debug(uri) writefln("result = '%s'", result);
    assert(result == target);
    result = decode(target);
    debug(uri) writefln("result = '%s'", result);
    assert(result == source);

    result = encode(decode("%E3%81%82%E3%81%82"));
    assert(result == "%E3%81%82%E3%81%82");

    result = encodeComponent("c++");
    assert(result == "c%2B%2B");

    auto str = new char[10_000_000];
    str[] = 'A';
    result = encodeComponent(str);
    foreach (char c; result)
        assert(c == 'A');

    result = decode("%41%42%43");
    debug(uri) writeln(result);

    import std.meta : AliasSeq;
    static foreach (StringType; AliasSeq!(char[], wchar[], dchar[], string, wstring, dstring))
    {{
        import std.conv : to;
        StringType decoded1 = source.to!StringType;
        string encoded1 = encode(decoded1);
        assert(decoded1 == source.to!StringType); // check that `decoded1` wasn't changed
        assert(encoded1 == target);
        assert(decoded1 == decode(encoded1).to!StringType);

        StringType encoded2 = target.to!StringType;
        string decoded2 = decode(encoded2);
        assert(encoded2 == target.to!StringType); // check that `encoded2` wasn't changed
        assert(decoded2 == source);
        assert(encoded2 == encode(decoded2).to!StringType);
    }}
}

@safe pure nothrow @nogc unittest
{
    assert(emailLength("") == -1);
    assert(emailLength("@") == -1);
    assert(emailLength("abcd") == -1);
    assert(emailLength("blah@blub") == -1);
    assert(emailLength("blah@blub.") == -1);
    assert(emailLength("blah@blub.domain") == -1);
}
