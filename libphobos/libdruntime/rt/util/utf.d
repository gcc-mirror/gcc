/********************************************
 * Encode and decode UTF-8, UTF-16 and UTF-32 strings.
 *
 * For Win32 systems, the C wchar_t type is UTF-16 and corresponds to the D
 * wchar type.
 * For Posix systems, the C wchar_t type is UTF-32 and corresponds to
 * the D utf.dchar type.
 *
 * UTF character support is restricted to (\u0000 &lt;= character &lt;= \U0010FFFF).
 *
 * See_Also:
 *      $(LINK2 http://en.wikipedia.org/wiki/Unicode, Wikipedia)<br>
 *      $(LINK http://www.cl.cam.ac.uk/~mgk25/unicode.html#utf-8)<br>
 *      $(LINK http://anubis.dkuug.dk/JTC1/SC2/WG2/docs/n1335)
 *
 * Copyright: Copyright Digital Mars 2003 - 2016.
 * License:   $(WEB www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
 * Authors:   Walter Bright, Sean Kelly
 * Source:    $(DRUNTIMESRC src/rt/util/_utf.d)
 */

module rt.util.utf;

extern (C) void onUnicodeError( string msg, size_t idx, string file = __FILE__, size_t line = __LINE__ ) @safe pure;

/*******************************
 * Test if c is a valid UTF-32 character.
 *
 * \uFFFE and \uFFFF are considered valid by this function,
 * as they are permitted for internal use by an application,
 * but they are not allowed for interchange by the Unicode standard.
 *
 * Returns: true if it is, false if not.
 */

@safe @nogc pure nothrow
bool isValidDchar(dchar c)
{
    /* Note: FFFE and FFFF are specifically permitted by the
     * Unicode standard for application internal use, but are not
     * allowed for interchange.
     * (thanks to Arcane Jill)
     */

    return c < 0xD800 ||
        (c > 0xDFFF && c <= 0x10FFFF /*&& c != 0xFFFE && c != 0xFFFF*/);
}

unittest
{
    debug(utf) printf("utf.isValidDchar.unittest\n");
    assert(isValidDchar(cast(dchar)'a') == true);
    assert(isValidDchar(cast(dchar)0x1FFFFF) == false);
}



static immutable UTF8stride =
[
    cast(ubyte)
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
    3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
    4,4,4,4,4,4,4,4,5,5,5,5,6,6,0xFF,0xFF,
];

/**
 * stride() returns the length of a UTF-8 sequence starting at index i
 * in string s.
 * Returns:
 *      The number of bytes in the UTF-8 sequence or
 *      0xFF meaning s[i] is not the start of of UTF-8 sequence.
 */
@safe @nogc pure nothrow
uint stride(in char[] s, size_t i)
{
    return UTF8stride[s[i]];
}

/**
 * stride() returns the length of a UTF-16 sequence starting at index i
 * in string s.
 */
@safe @nogc pure nothrow
uint stride(in wchar[] s, size_t i)
{   uint u = s[i];
    return 1 + (u >= 0xD800 && u <= 0xDBFF);
}

/**
 * stride() returns the length of a UTF-32 sequence starting at index i
 * in string s.
 * Returns: The return value will always be 1.
 */
@safe @nogc pure nothrow
uint stride(in dchar[] s, size_t i)
{
    return 1;
}

/*******************************************
 * Given an index i into an array of characters s[],
 * and assuming that index i is at the start of a UTF character,
 * determine the number of UCS characters up to that index i.
 */
@safe pure
size_t toUCSindex(in char[] s, size_t i)
{
    size_t n;
    size_t j;

    for (j = 0; j < i; )
    {
        j += stride(s, j);
        n++;
    }
    if (j > i)
    {
        onUnicodeError("invalid UTF-8 sequence", j);
    }
    return n;
}

/** ditto */
@safe pure
size_t toUCSindex(in wchar[] s, size_t i)
{
    size_t n;
    size_t j;

    for (j = 0; j < i; )
    {
        j += stride(s, j);
        n++;
    }
    if (j > i)
    {
        onUnicodeError("invalid UTF-16 sequence", j);
    }
    return n;
}

/** ditto */
@safe @nogc pure nothrow
size_t toUCSindex(in dchar[] s, size_t i)
{
    return i;
}

/******************************************
 * Given a UCS index n into an array of characters s[], return the UTF index.
 */
@safe pure
size_t toUTFindex(in char[] s, size_t n)
{
    size_t i;

    while (n--)
    {
        uint j = UTF8stride[s[i]];
        if (j == 0xFF)
            onUnicodeError("invalid UTF-8 sequence", i);
        i += j;
    }
    return i;
}

/** ditto */
@safe @nogc pure nothrow
size_t toUTFindex(in wchar[] s, size_t n)
{
    size_t i;

    while (n--)
    {   wchar u = s[i];

        i += 1 + (u >= 0xD800 && u <= 0xDBFF);
    }
    return i;
}

/** ditto */
@safe @nogc pure nothrow
size_t toUTFindex(in dchar[] s, size_t n)
{
    return n;
}

/* =================== Decode ======================= */

/***************
 * Decodes and returns character starting at s[idx]. idx is advanced past the
 * decoded character. If the character is not well formed, a UtfException is
 * thrown and idx remains unchanged.
 */
@safe pure
dchar decode(in char[] s, ref size_t idx)
    in
    {
        assert(idx >= 0 && idx < s.length);
    }
    out (result)
    {
        assert(isValidDchar(result));
    }
    body
    {
        size_t len = s.length;
        dchar V;
        size_t i = idx;
        char u = s[i];

        if (u & 0x80)
        {   uint n;
            char u2;

            /* The following encodings are valid, except for the 5 and 6 byte
             * combinations:
             *  0xxxxxxx
             *  110xxxxx 10xxxxxx
             *  1110xxxx 10xxxxxx 10xxxxxx
             *  11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
             *  111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
             *  1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
             */
            for (n = 1; ; n++)
            {
                if (n > 4)
                    goto Lerr;          // only do the first 4 of 6 encodings
                if (((u << n) & 0x80) == 0)
                {
                    if (n == 1)
                        goto Lerr;
                    break;
                }
            }

            // Pick off (7 - n) significant bits of B from first byte of octet
            V = cast(dchar)(u & ((1 << (7 - n)) - 1));

            if (i + (n - 1) >= len)
                goto Lerr;                      // off end of string

            /* The following combinations are overlong, and illegal:
             *  1100000x (10xxxxxx)
             *  11100000 100xxxxx (10xxxxxx)
             *  11110000 1000xxxx (10xxxxxx 10xxxxxx)
             *  11111000 10000xxx (10xxxxxx 10xxxxxx 10xxxxxx)
             *  11111100 100000xx (10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx)
             */
            u2 = s[i + 1];
            if ((u & 0xFE) == 0xC0 ||
                (u == 0xE0 && (u2 & 0xE0) == 0x80) ||
                (u == 0xF0 && (u2 & 0xF0) == 0x80) ||
                (u == 0xF8 && (u2 & 0xF8) == 0x80) ||
                (u == 0xFC && (u2 & 0xFC) == 0x80))
                goto Lerr;                      // overlong combination

            for (uint j = 1; j != n; j++)
            {
                u = s[i + j];
                if ((u & 0xC0) != 0x80)
                    goto Lerr;                  // trailing bytes are 10xxxxxx
                V = (V << 6) | (u & 0x3F);
            }
            if (!isValidDchar(V))
                goto Lerr;
            i += n;
        }
        else
        {
            V = cast(dchar) u;
            i++;
        }

        idx = i;
        return V;

      Lerr:
      onUnicodeError("invalid UTF-8 sequence", i);
    return V; // dummy return
    }

unittest
{   size_t i;
    dchar c;

    debug(utf) printf("utf.decode.unittest\n");

    static s1 = "abcd"c;
    i = 0;
    c = decode(s1, i);
    assert(c == cast(dchar)'a');
    assert(i == 1);
    c = decode(s1, i);
    assert(c == cast(dchar)'b');
    assert(i == 2);

    static s2 = "\xC2\xA9"c;
    i = 0;
    c = decode(s2, i);
    assert(c == cast(dchar)'\u00A9');
    assert(i == 2);

    static s3 = "\xE2\x89\xA0"c;
    i = 0;
    c = decode(s3, i);
    assert(c == cast(dchar)'\u2260');
    assert(i == 3);

    static s4 =
    [   "\xE2\x89"c[],          // too short
        "\xC0\x8A",
        "\xE0\x80\x8A",
        "\xF0\x80\x80\x8A",
        "\xF8\x80\x80\x80\x8A",
        "\xFC\x80\x80\x80\x80\x8A",
    ];

    for (int j = 0; j < s4.length; j++)
    {
        try
        {
            i = 0;
            c = decode(s4[j], i);
            assert(0);
        }
        catch (Throwable o)
        {
            i = 23;
        }
        assert(i == 23);
    }
}

/** ditto */
@safe pure
dchar decode(in wchar[] s, ref size_t idx)
    in
    {
        assert(idx >= 0 && idx < s.length);
    }
    out (result)
    {
        assert(isValidDchar(result));
    }
    body
    {
        string msg;
        dchar V;
        size_t i = idx;
        uint u = s[i];

        if (u & ~0x7F)
        {   if (u >= 0xD800 && u <= 0xDBFF)
            {   uint u2;

                if (i + 1 == s.length)
                {   msg = "surrogate UTF-16 high value past end of string";
                    goto Lerr;
                }
                u2 = s[i + 1];
                if (u2 < 0xDC00 || u2 > 0xDFFF)
                {   msg = "surrogate UTF-16 low value out of range";
                    goto Lerr;
                }
                u = ((u - 0xD7C0) << 10) + (u2 - 0xDC00);
                i += 2;
            }
            else if (u >= 0xDC00 && u <= 0xDFFF)
            {   msg = "unpaired surrogate UTF-16 value";
                goto Lerr;
            }
            else if (u == 0xFFFE || u == 0xFFFF)
            {   msg = "illegal UTF-16 value";
                goto Lerr;
            }
            else
                i++;
        }
        else
        {
            i++;
        }

        idx = i;
        return cast(dchar)u;

      Lerr:
          onUnicodeError(msg, i);
        return cast(dchar)u; // dummy return
    }

/** ditto */
@safe pure
dchar decode(in dchar[] s, ref size_t idx)
    in
    {
        assert(idx >= 0 && idx < s.length);
    }
    body
    {
        size_t i = idx;
        dchar c = s[i];

        if (!isValidDchar(c))
            goto Lerr;
        idx = i + 1;
        return c;

      Lerr:
          onUnicodeError("invalid UTF-32 value", i);
        return c; // dummy return
    }


/* =================== Encode ======================= */

/*******************************
 * Encodes character c and appends it to array s[].
 */
@safe pure nothrow
void encode(ref char[] s, dchar c)
    in
    {
        assert(isValidDchar(c));
    }
    body
    {
        char[] r = s;

        if (c <= 0x7F)
        {
            r ~= cast(char) c;
        }
        else
        {
            char[4] buf;
            uint L;

            if (c <= 0x7FF)
            {
                buf[0] = cast(char)(0xC0 | (c >> 6));
                buf[1] = cast(char)(0x80 | (c & 0x3F));
                L = 2;
            }
            else if (c <= 0xFFFF)
            {
                buf[0] = cast(char)(0xE0 | (c >> 12));
                buf[1] = cast(char)(0x80 | ((c >> 6) & 0x3F));
                buf[2] = cast(char)(0x80 | (c & 0x3F));
                L = 3;
            }
            else if (c <= 0x10FFFF)
            {
                buf[0] = cast(char)(0xF0 | (c >> 18));
                buf[1] = cast(char)(0x80 | ((c >> 12) & 0x3F));
                buf[2] = cast(char)(0x80 | ((c >> 6) & 0x3F));
                buf[3] = cast(char)(0x80 | (c & 0x3F));
                L = 4;
            }
            else
            {
                assert(0);
            }
            r ~= buf[0 .. L];
        }
        s = r;
    }

unittest
{
    debug(utf) printf("utf.encode.unittest\n");

    char[] s = "abcd".dup;
    encode(s, cast(dchar)'a');
    assert(s.length == 5);
    assert(s == "abcda");

    encode(s, cast(dchar)'\u00A9');
    assert(s.length == 7);
    assert(s == "abcda\xC2\xA9");
    //assert(s == "abcda\u00A9");       // BUG: fix compiler

    encode(s, cast(dchar)'\u2260');
    assert(s.length == 10);
    assert(s == "abcda\xC2\xA9\xE2\x89\xA0");
}

/** ditto */
@safe pure nothrow
void encode(ref wchar[] s, dchar c)
    in
    {
        assert(isValidDchar(c));
    }
    body
    {
        wchar[] r = s;

        if (c <= 0xFFFF)
        {
            r ~= cast(wchar) c;
        }
        else
        {
            wchar[2] buf;

            buf[0] = cast(wchar) ((((c - 0x10000) >> 10) & 0x3FF) + 0xD800);
            buf[1] = cast(wchar) (((c - 0x10000) & 0x3FF) + 0xDC00);
            r ~= buf;
        }
        s = r;
    }

/** ditto */
@safe pure nothrow
void encode(ref dchar[] s, dchar c)
    in
    {
        assert(isValidDchar(c));
    }
    body
    {
        s ~= c;
    }

/**
Returns the code length of $(D c) in the encoding using $(D C) as a
code point. The code is returned in character count, not in bytes.
 */
@safe pure nothrow @nogc
ubyte codeLength(C)(dchar c)
{
    static if (C.sizeof == 1)
    {
        if (c <= 0x7F) return 1;
        if (c <= 0x7FF) return 2;
        if (c <= 0xFFFF) return 3;
        if (c <= 0x10FFFF) return 4;
        assert(false);
    }
    else static if (C.sizeof == 2)
    {
        return c <= 0xFFFF ? 1 : 2;
    }
    else
    {
        static assert(C.sizeof == 4);
        return 1;
    }
}

/* =================== Validation ======================= */

/***********************************
Checks to see if string is well formed or not. $(D S) can be an array
 of $(D char), $(D wchar), or $(D dchar). Throws a $(D UtfException)
 if it is not. Use to check all untrusted input for correctness.
 */
@safe pure
void validate(S)(in S s)
{
    auto len = s.length;
    for (size_t i = 0; i < len; )
    {
        decode(s, i);
    }
}

/* =================== Conversion to UTF8 ======================= */

@safe pure nothrow @nogc
char[] toUTF8(char[] buf, dchar c)
    in
    {
        assert(isValidDchar(c));
    }
    body
    {
        if (c <= 0x7F)
        {
            buf[0] = cast(char) c;
            return buf[0 .. 1];
        }
        else if (c <= 0x7FF)
        {
            buf[0] = cast(char)(0xC0 | (c >> 6));
            buf[1] = cast(char)(0x80 | (c & 0x3F));
            return buf[0 .. 2];
        }
        else if (c <= 0xFFFF)
        {
            buf[0] = cast(char)(0xE0 | (c >> 12));
            buf[1] = cast(char)(0x80 | ((c >> 6) & 0x3F));
            buf[2] = cast(char)(0x80 | (c & 0x3F));
            return buf[0 .. 3];
        }
        else if (c <= 0x10FFFF)
        {
            buf[0] = cast(char)(0xF0 | (c >> 18));
            buf[1] = cast(char)(0x80 | ((c >> 12) & 0x3F));
            buf[2] = cast(char)(0x80 | ((c >> 6) & 0x3F));
            buf[3] = cast(char)(0x80 | (c & 0x3F));
            return buf[0 .. 4];
        }
        assert(0);
    }

/*******************
 * Encodes string s into UTF-8 and returns the encoded string.
 */
@safe pure nothrow
string toUTF8(string s)
    in
    {
        validate(s);
    }
    body
    {
        return s;
    }

/** ditto */
@trusted pure
string toUTF8(in wchar[] s)
{
    char[] r;
    size_t i;
    size_t slen = s.length;

    r.length = slen;

    for (i = 0; i < slen; i++)
    {   wchar c = s[i];

        if (c <= 0x7F)
            r[i] = cast(char)c;         // fast path for ascii
        else
        {
            r.length = i;
            foreach (dchar c; s[i .. slen])
            {
                encode(r, c);
            }
            break;
        }
    }
    return cast(string)r;
}

/** ditto */
@trusted pure
string toUTF8(in dchar[] s)
{
    char[] r;
    size_t i;
    size_t slen = s.length;

    r.length = slen;

    for (i = 0; i < slen; i++)
    {   dchar c = s[i];

        if (c <= 0x7F)
            r[i] = cast(char)c;         // fast path for ascii
        else
        {
            r.length = i;
            foreach (dchar d; s[i .. slen])
            {
                encode(r, d);
            }
            break;
        }
    }
    return cast(string)r;
}

/* =================== Conversion to UTF16 ======================= */

@safe pure nothrow @nogc
wchar[] toUTF16(wchar[] buf, dchar c)
    in
    {
        assert(isValidDchar(c));
    }
    body
    {
        if (c <= 0xFFFF)
        {
            buf[0] = cast(wchar) c;
            return buf[0 .. 1];
        }
        else
        {
            buf[0] = cast(wchar) ((((c - 0x10000) >> 10) & 0x3FF) + 0xD800);
            buf[1] = cast(wchar) (((c - 0x10000) & 0x3FF) + 0xDC00);
            return buf[0 .. 2];
        }
    }

/****************
 * Encodes string s into UTF-16 and returns the encoded string.
 * toUTF16z() is suitable for calling the 'W' functions in the Win32 API that take
 * an LPWSTR or LPCWSTR argument.
 */
@trusted pure
wstring toUTF16(in char[] s)
{
    wchar[] r;
    size_t slen = s.length;

    r.length = slen;
    r.length = 0;
    for (size_t i = 0; i < slen; )
    {
        dchar c = s[i];
        if (c <= 0x7F)
        {
            i++;
            r ~= cast(wchar)c;
        }
        else
        {
            c = decode(s, i);
            encode(r, c);
        }
    }
    return cast(wstring)r;
}

alias const(wchar)* wptr;
/** ditto */
@safe pure
wptr toUTF16z(in char[] s)
{
    wchar[] r;
    size_t slen = s.length;

    r.length = slen + 1;
    r.length = 0;
    for (size_t i = 0; i < slen; )
    {
        dchar c = s[i];
        if (c <= 0x7F)
        {
            i++;
            r ~= cast(wchar)c;
        }
        else
        {
            c = decode(s, i);
            encode(r, c);
        }
    }
    r ~= '\000';
    return &r[0];
}

/** ditto */
@safe pure nothrow
wstring toUTF16(wstring s)
    in
    {
        validate(s);
    }
    body
    {
        return s;
    }

/** ditto */
@trusted pure nothrow
wstring toUTF16(in dchar[] s)
{
    wchar[] r;
    size_t slen = s.length;

    r.length = slen;
    r.length = 0;
    for (size_t i = 0; i < slen; i++)
    {
        encode(r, s[i]);
    }
    return cast(wstring)r;
}

/* =================== Conversion to UTF32 ======================= */

/*****
 * Encodes string s into UTF-32 and returns the encoded string.
 */
@trusted pure
dstring toUTF32(in char[] s)
{
    dchar[] r;
    size_t slen = s.length;
    size_t j = 0;

    r.length = slen;            // r[] will never be longer than s[]
    for (size_t i = 0; i < slen; )
    {
        dchar c = s[i];
        if (c >= 0x80)
            c = decode(s, i);
        else
            i++;                // c is ascii, no need for decode
        r[j++] = c;
    }
    return cast(dstring)r[0 .. j];
}

/** ditto */
@trusted pure
dstring toUTF32(in wchar[] s)
{
    dchar[] r;
    size_t slen = s.length;
    size_t j = 0;

    r.length = slen;            // r[] will never be longer than s[]
    for (size_t i = 0; i < slen; )
    {
        dchar c = s[i];
        if (c >= 0x80)
            c = decode(s, i);
        else
            i++;                // c is ascii, no need for decode
        r[j++] = c;
    }
    return cast(dstring)r[0 .. j];
}

/** ditto */
@safe pure nothrow
dstring toUTF32(dstring s)
    in
    {
        validate(s);
    }
    body
    {
        return s;
    }

/* ================================ tests ================================== */

unittest
{
    debug(utf) printf("utf.toUTF.unittest\n");

    auto c = "hello"c[];
    auto w = toUTF16(c);
    assert(w == "hello");
    auto d = toUTF32(c);
    assert(d == "hello");

    c = toUTF8(w);
    assert(c == "hello");
    d = toUTF32(w);
    assert(d == "hello");

    c = toUTF8(d);
    assert(c == "hello");
    w = toUTF16(d);
    assert(w == "hello");


    c = "hel\u1234o";
    w = toUTF16(c);
    assert(w == "hel\u1234o");
    d = toUTF32(c);
    assert(d == "hel\u1234o");

    c = toUTF8(w);
    assert(c == "hel\u1234o");
    d = toUTF32(w);
    assert(d == "hel\u1234o");

    c = toUTF8(d);
    assert(c == "hel\u1234o");
    w = toUTF16(d);
    assert(w == "hel\u1234o");


    c = "he\U000BAAAAllo";
    w = toUTF16(c);
    //foreach (wchar c; w) printf("c = x%x\n", c);
    //foreach (wchar c; cast(wstring)"he\U000BAAAAllo") printf("c = x%x\n", c);
    assert(w == "he\U000BAAAAllo");
    d = toUTF32(c);
    assert(d == "he\U000BAAAAllo");

    c = toUTF8(w);
    assert(c == "he\U000BAAAAllo");
    d = toUTF32(w);
    assert(d == "he\U000BAAAAllo");

    c = toUTF8(d);
    assert(c == "he\U000BAAAAllo");
    w = toUTF16(d);
    assert(w == "he\U000BAAAAllo");

    wchar[2] buf;
    auto ret = toUTF16(buf, '\U000BAAAA');
    assert(ret == "\U000BAAAA");
}
