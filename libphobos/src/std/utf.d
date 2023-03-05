// Written in the D programming language.

/++
    Encode and decode UTF-8, UTF-16 and UTF-32 strings.

    UTF character support is restricted to
    $(D '\u0000' &lt;= character &lt;= '\U0010FFFF').

$(SCRIPT inhibitQuickIndex = 1;)
$(DIVC quickindex,
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD Decode) $(TD
    $(LREF decode)
    $(LREF decodeFront)
))
$(TR $(TD Lazy decode) $(TD
    $(LREF byCodeUnit)
    $(LREF byChar)
    $(LREF byWchar)
    $(LREF byDchar)
    $(LREF byUTF)
))
$(TR $(TD Encode) $(TD
    $(LREF encode)
    $(LREF toUTF8)
    $(LREF toUTF16)
    $(LREF toUTF32)
    $(LREF toUTFz)
    $(LREF toUTF16z)
))
$(TR $(TD Length) $(TD
    $(LREF codeLength)
    $(LREF count)
    $(LREF stride)
    $(LREF strideBack)
))
$(TR $(TD Index) $(TD
    $(LREF toUCSindex)
    $(LREF toUTFindex)
))
$(TR $(TD Validation) $(TD
    $(LREF isValidDchar)
    $(LREF isValidCodepoint)
    $(LREF validate)
))
$(TR $(TD Miscellaneous) $(TD
    $(LREF replacementDchar)
    $(LREF UseReplacementDchar)
    $(LREF UTFException)
))
))
    See_Also:
        $(LINK2 http://en.wikipedia.org/wiki/Unicode, Wikipedia)<br>
        $(LINK http://www.cl.cam.ac.uk/~mgk25/unicode.html#utf-8)<br>
        $(LINK http://anubis.dkuug.dk/JTC1/SC2/WG2/docs/n1335)
    Copyright: Copyright The D Language Foundation 2000 - 2012.
    License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
    Authors:   $(HTTP digitalmars.com, Walter Bright) and
               $(HTTP jmdavisprog.com, Jonathan M Davis)
    Source:    $(PHOBOSSRC std/utf.d)
   +/
module std.utf;

import std.exception : basicExceptionCtors;
import core.exception : UnicodeException;
import std.meta : AliasSeq;
import std.range;
import std.traits : isAutodecodableString, isConvertibleToString,
    isSomeChar, isSomeString, isStaticArray, Unqual;
import std.typecons : Flag, Yes, No;


/++
    Exception thrown on errors in std.utf functions.
  +/
class UTFException : UnicodeException
{
    import core.internal.string : unsignedToTempString, UnsignedStringBuf;

    uint[4] sequence;
    size_t  len;

    @safe pure nothrow @nogc
    UTFException setSequence(scope uint[] data...) return
    {
        assert(data.length <= 4);

        len = data.length < 4 ? data.length : 4;
        sequence[0 .. len] = data[0 .. len];

        return this;
    }

    // FIXME: Use std.exception.basicExceptionCtors here once
    // https://issues.dlang.org/show_bug.cgi?id=11500 is fixed

    /**
    Standard exception constructors.
     */
    this(string msg, string file = __FILE__, size_t line = __LINE__,
         Throwable next = null) @nogc @safe pure nothrow
    {
        super(msg, 0, file, line, next);
    }
    /// ditto
    this(string msg, size_t index, string file = __FILE__,
         size_t line = __LINE__, Throwable next = null) @safe pure nothrow
    {
        UnsignedStringBuf buf = void;
        msg ~= " (at index " ~ unsignedToTempString(index, buf) ~ ")";
        super(msg, index, file, line, next);
    }

    /**
    Returns:
        A `string` detailing the invalid UTF sequence.
     */
    override string toString() const
    {
        if (len == 0)
        {
            /* Exception.toString() is not marked as const, although
             * it is const-compatible.
             */
            //return super.toString();
            auto e = () @trusted { return cast(Exception) super; } ();
            return e.toString();
        }

        string result = "Invalid UTF sequence:";

        foreach (i; sequence[0 .. len])
        {
            UnsignedStringBuf buf = void;
            result ~= ' ';
            auto h = unsignedToTempString!16(i, buf);
            if (h.length == 1)
                result ~= '0';
            result ~= h;
            result ~= 'x';
        }

        if (super.msg.length > 0)
        {
            result ~= " - ";
            result ~= super.msg;
        }

        return result;
    }
}

///
@safe unittest
{
    import std.exception : assertThrown;

    char[4] buf;
    assertThrown!UTFException(encode(buf, cast(dchar) 0xD800));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDBFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDC00));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDFFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0x110000));
}

/*
   Provide array of invalidly encoded UTF strings. Useful for testing.

   Params:
        Char = char, wchar, or dchar

   Returns:
        an array of invalidly encoded UTF strings
 */

package auto invalidUTFstrings(Char)() @safe pure @nogc nothrow
if (isSomeChar!Char)
{
    static if (is(Char == char))
    {
        enum x = 0xDC00;         // invalid surrogate value
        enum y = 0x110000;       // out of range

        static immutable string[8] result =
        [
            "\x80",             // not a start byte
            "\xC0",             // truncated
            "\xC0\xC0",         // invalid continuation
            "\xF0\x82\x82\xAC", // overlong
            [
              0xE0 | (x >> 12),
              0x80 | ((x >> 6) & 0x3F),
              0x80 | (x & 0x3F)
            ],
            [
              cast(char)(0xF0 | (y >> 18)),
              cast(char)(0x80 | ((y >> 12) & 0x3F)),
              cast(char)(0x80 | ((y >> 6) & 0x3F)),
              cast(char)(0x80 | (y & 0x3F))
            ],
            [
              cast(char)(0xF8 | 3),     // 5 byte encoding
              cast(char)(0x80 | 3),
              cast(char)(0x80 | 3),
              cast(char)(0x80 | 3),
              cast(char)(0x80 | 3),
            ],
            [
              cast(char)(0xFC | 3),     // 6 byte encoding
              cast(char)(0x80 | 3),
              cast(char)(0x80 | 3),
              cast(char)(0x80 | 3),
              cast(char)(0x80 | 3),
              cast(char)(0x80 | 3),
            ],
        ];

        return result[];
    }
    else static if (is(Char == wchar))
    {
        static immutable wstring[5] result =
        [
            [
              cast(wchar) 0xDC00,
            ],
            [
              cast(wchar) 0xDFFF,
            ],
            [
              cast(wchar) 0xDBFF,
              cast(wchar) 0xDBFF,
            ],
            [
              cast(wchar) 0xDBFF,
              cast(wchar) 0xE000,
            ],
            [
              cast(wchar) 0xD800,
            ],
        ];

        return result[];
    }
    else static if (is(Char == dchar))
    {
        static immutable dstring[3] result =
        [
            [ cast(dchar) 0x110000 ],
            [ cast(dchar) 0x00D800 ],
            [ cast(dchar) 0x00DFFF ],
        ];

        return result;
    }
    else
        static assert(0);
}

/++
    Check whether the given Unicode code point is valid.

    Params:
        c = code point to check

    Returns:
        `true` if and only if `c` is a valid Unicode code point

    Note:
    `'\uFFFE'` and `'\uFFFF'` are considered valid by `isValidDchar`,
    as they are permitted for internal use by an application, but they are
    not allowed for interchange by the Unicode standard.
  +/
bool isValidDchar(dchar c) pure nothrow @safe @nogc
{
    return c < 0xD800 || (c > 0xDFFF && c <= 0x10FFFF);
}

///
@safe @nogc pure nothrow unittest
{
    assert( isValidDchar(cast(dchar) 0x41));
    assert( isValidDchar(cast(dchar) 0x00));
    assert(!isValidDchar(cast(dchar) 0xD800));
    assert(!isValidDchar(cast(dchar) 0x11FFFF));
}

pure nothrow @safe @nogc unittest
{
    import std.exception;

    assertCTFEable!(
    {
    assert( isValidDchar(cast(dchar)'a') == true);
    assert( isValidDchar(cast(dchar) 0x1FFFFF) == false);

    assert(!isValidDchar(cast(dchar) 0x00D800));
    assert(!isValidDchar(cast(dchar) 0x00DBFF));
    assert(!isValidDchar(cast(dchar) 0x00DC00));
    assert(!isValidDchar(cast(dchar) 0x00DFFF));
    assert( isValidDchar(cast(dchar) 0x00FFFE));
    assert( isValidDchar(cast(dchar) 0x00FFFF));
    assert( isValidDchar(cast(dchar) 0x01FFFF));
    assert( isValidDchar(cast(dchar) 0x10FFFF));
    assert(!isValidDchar(cast(dchar) 0x110000));
    });
}

/**
Checks if a single character forms a valid code point.

When standing alone, some characters are invalid code points. For
example the `wchar` `0xD800` is a so called high surrogate, which can
only be interpreted together with a low surrogate following it. As a
standalone character it is considered invalid.

See $(LINK2 http://www.unicode.org/versions/Unicode13.0.0/,
Unicode Standard, D90, D91 and D92) for more details.

Params:
    c = character to test
    Char = character type of `c`

Returns:
    `true`, if `c` forms a valid code point.
 */
bool isValidCodepoint(Char)(Char c)
if (isSomeChar!Char)
{
    alias UChar = Unqual!Char;
    static if (is(UChar == char))
    {
        return c <= 0x7F;
    }
    else static if (is(UChar == wchar))
    {
        return c <= 0xD7FF || c >= 0xE000;
    }
    else static if (is(UChar == dchar))
    {
        return isValidDchar(c);
    }
    else
        static assert(false, "unknown character type: `" ~ Char.stringof ~ "`");
}

///
@safe pure nothrow unittest
{
    assert( isValidCodepoint(cast(char) 0x40));
    assert(!isValidCodepoint(cast(char) 0x80));
    assert( isValidCodepoint(cast(wchar) 0x1234));
    assert(!isValidCodepoint(cast(wchar) 0xD800));
    assert( isValidCodepoint(cast(dchar) 0x0010FFFF));
    assert(!isValidCodepoint(cast(dchar) 0x12345678));
}

/++
    Calculate the length of the UTF sequence starting at `index`
    in `str`.

    Params:
        str = $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
        of UTF code units. Must be random access if `index` is passed
        index = starting index of UTF sequence (default: `0`)

    Returns:
        The number of code units in the UTF sequence. For UTF-8, this is a
        value between 1 and 4 (as per $(HTTP tools.ietf.org/html/rfc3629#section-3, RFC 3629$(COMMA) section 3)).
        For UTF-16, it is either 1 or 2. For UTF-32, it is always 1.

    Throws:
        May throw a `UTFException` if `str[index]` is not the start of a
        valid UTF sequence.

    Note:
        `stride` will only analyze the first `str[index]` element. It
        will not fully verify the validity of the UTF sequence, nor even verify
        the presence of the sequence: it will not actually guarantee that
        $(D index + stride(str, index) <= str.length).
  +/
uint stride(S)(auto ref S str, size_t index)
if (is(S : const char[]) ||
    (isRandomAccessRange!S && is(immutable ElementType!S == immutable char)))
{
    static if (is(typeof(str.length) : ulong))
        assert(index < str.length, "Past the end of the UTF-8 sequence");
    immutable c = str[index];

    if (c < 0x80)
        return 1;
    else
        return strideImpl(c, index);
}

/// Ditto
uint stride(S)(auto ref S str)
if (is(S : const char[]) ||
    (isInputRange!S && is(immutable ElementType!S == immutable char)))
{
    static if (is(S : const char[]))
        immutable c = str[0];
    else
        immutable c = str.front;

    if (c < 0x80)
        return 1;
    else
        return strideImpl(c, 0);
}

@system unittest
{
    import core.exception : AssertError;
    import std.conv : to;
    import std.exception;
    import std.string : format;
    import std.traits : FunctionAttribute, functionAttributes, isSafe;
    static void test(string s, dchar c, size_t i = 0, size_t line = __LINE__)
    {
        enforce(stride(s, i) == codeLength!char(c),
                new AssertError(format("Unit test failure string: %s", s), __FILE__, line));

        enforce(stride(RandomCU!char(s), i) == codeLength!char(c),
                new AssertError(format("Unit test failure range: %s", s), __FILE__, line));

        auto refRandom = new RefRandomCU!char(s);
        immutable randLen = refRandom.length;
        enforce(stride(refRandom, i) == codeLength!char(c),
                new AssertError(format("Unit test failure rand ref range: %s", s), __FILE__, line));
        enforce(refRandom.length == randLen,
                new AssertError(format("Unit test failure rand ref range length: %s", s), __FILE__, line));

        if (i == 0)
        {
            enforce(stride(s) == codeLength!char(c),
                    new AssertError(format("Unit test failure string 0: %s", s), __FILE__, line));

            enforce(stride(InputCU!char(s)) == codeLength!char(c),
                    new AssertError(format("Unit test failure range 0: %s", s), __FILE__, line));

            auto refBidir = new RefBidirCU!char(s);
            immutable bidirLen = refBidir.length;
            enforce(stride(refBidir) == codeLength!char(c),
                    new AssertError(format("Unit test failure bidir ref range code length: %s", s), __FILE__, line));
            enforce(refBidir.length == bidirLen,
                    new AssertError(format("Unit test failure bidir ref range length: %s", s), __FILE__, line));
        }
    }

    assertCTFEable!(
    {
    test("a", 'a');
    test(" ", ' ');
    test("\u2029", '\u2029'); //paraSep
    test("\u0100", '\u0100');
    test("\u0430", '\u0430');
    test("\U00010143", '\U00010143');
    test("abcdefcdef", 'a');
    test("hello\U00010143\u0100\U00010143", 'h', 0);
    test("hello\U00010143\u0100\U00010143", 'e', 1);
    test("hello\U00010143\u0100\U00010143", 'l', 2);
    test("hello\U00010143\u0100\U00010143", 'l', 3);
    test("hello\U00010143\u0100\U00010143", 'o', 4);
    test("hello\U00010143\u0100\U00010143", '\U00010143', 5);
    test("hello\U00010143\u0100\U00010143", '\u0100', 9);
    test("hello\U00010143\u0100\U00010143", '\U00010143', 11);

    foreach (S; AliasSeq!(char[], const char[], string))
    {
        enum str = to!S("hello world");
        static assert(isSafe!({ stride(str, 0); }));
        static assert(isSafe!({ stride(str);    }));
        static assert((functionAttributes!({ stride(str, 0); }) & FunctionAttribute.pure_) != 0);
        static assert((functionAttributes!({ stride(str);    }) & FunctionAttribute.pure_) != 0);
    }
    });
}

@safe unittest // invalid start bytes
{
    import std.exception : assertThrown;
    immutable char[] invalidStartBytes = [
        0b1111_1000, // indicating a sequence length of 5
        0b1111_1100, // 6
        0b1111_1110, // 7
        0b1111_1111, // 8
        0b1000_0000, // continuation byte
    ];
    foreach (c; invalidStartBytes)
        assertThrown!UTFException(stride([c]));
}

/// Ditto
uint stride(S)(auto ref S str, size_t index)
if (is(S : const wchar[]) ||
    (isRandomAccessRange!S && is(immutable ElementType!S == immutable wchar)))
{
    static if (is(typeof(str.length) : ulong))
        assert(index < str.length, "Past the end of the UTF-16 sequence");
    immutable uint u = str[index];
    return 1 + (u >= 0xD800 && u <= 0xDBFF);
}

/// Ditto
uint stride(S)(auto ref S str) @safe pure
if (is(S : const wchar[]))
{
    return stride(str, 0);
}

/// Ditto
uint stride(S)(auto ref S str)
if (isInputRange!S && is(immutable ElementType!S == immutable wchar) &&
    !is(S : const wchar[]))
{
    assert(!str.empty, "UTF-16 sequence is empty");
    immutable uint u = str.front;
    return 1 + (u >= 0xD800 && u <= 0xDBFF);
}

@system unittest
{
    import core.exception : AssertError;
    import std.conv : to;
    import std.exception;
    import std.string : format;
    import std.traits : FunctionAttribute, functionAttributes, isSafe;
    static void test(wstring s, dchar c, size_t i = 0, size_t line = __LINE__)
    {
        enforce(stride(s, i) == codeLength!wchar(c),
                new AssertError(format("Unit test failure string: %s", s), __FILE__, line));

        enforce(stride(RandomCU!wchar(s), i) == codeLength!wchar(c),
                new AssertError(format("Unit test failure range: %s", s), __FILE__, line));

        auto refRandom = new RefRandomCU!wchar(s);
        immutable randLen = refRandom.length;
        enforce(stride(refRandom, i) == codeLength!wchar(c),
                new AssertError(format("Unit test failure rand ref range: %s", s), __FILE__, line));
        enforce(refRandom.length == randLen,
                new AssertError(format("Unit test failure rand ref range length: %s", s), __FILE__, line));

        if (i == 0)
        {
            enforce(stride(s) == codeLength!wchar(c),
                    new AssertError(format("Unit test failure string 0: %s", s), __FILE__, line));

            enforce(stride(InputCU!wchar(s)) == codeLength!wchar(c),
                    new AssertError(format("Unit test failure range 0: %s", s), __FILE__, line));

            auto refBidir = new RefBidirCU!wchar(s);
            immutable bidirLen = refBidir.length;
            enforce(stride(refBidir) == codeLength!wchar(c),
                    new AssertError(format("Unit test failure bidir ref range code length: %s", s), __FILE__, line));
            enforce(refBidir.length == bidirLen,
                    new AssertError(format("Unit test failure bidir ref range length: %s", s), __FILE__, line));
        }
    }

    assertCTFEable!(
    {
    test("a", 'a');
    test(" ", ' ');
    test("\u2029", '\u2029'); //paraSep
    test("\u0100", '\u0100');
    test("\u0430", '\u0430');
    test("\U00010143", '\U00010143');
    test("abcdefcdef", 'a');
    test("hello\U00010143\u0100\U00010143", 'h', 0);
    test("hello\U00010143\u0100\U00010143", 'e', 1);
    test("hello\U00010143\u0100\U00010143", 'l', 2);
    test("hello\U00010143\u0100\U00010143", 'l', 3);
    test("hello\U00010143\u0100\U00010143", 'o', 4);
    test("hello\U00010143\u0100\U00010143", '\U00010143', 5);
    test("hello\U00010143\u0100\U00010143", '\u0100', 7);
    test("hello\U00010143\u0100\U00010143", '\U00010143', 8);

    foreach (S; AliasSeq!(wchar[], const wchar[], wstring))
    {
        enum str = to!S("hello world");
        static assert(isSafe!(() => stride(str, 0)));
        static assert(isSafe!(() => stride(str)   ));
        static assert((functionAttributes!(() => stride(str, 0)) & FunctionAttribute.pure_) != 0);
        static assert((functionAttributes!(() => stride(str)   ) & FunctionAttribute.pure_) != 0);
    }
    });
}

/// Ditto
uint stride(S)(auto ref S str, size_t index = 0)
if (is(S : const dchar[]) ||
    (isInputRange!S && is(immutable ElementEncodingType!S == immutable dchar)))
{
    static if (is(typeof(str.length) : ulong))
        assert(index < str.length, "Past the end of the UTF-32 sequence");
    else
        assert(!str.empty, "UTF-32 sequence is empty.");
    return 1;
}

///
@safe unittest
{
    assert("a".stride == 1);
    assert("λ".stride == 2);
    assert("aλ".stride == 1);
    assert("aλ".stride(1) == 2);
    assert("𐐷".stride == 4);
}

@system unittest
{
    import core.exception : AssertError;
    import std.conv : to;
    import std.exception;
    import std.string : format;
    import std.traits : FunctionAttribute, functionAttributes, isSafe;
    static void test(dstring s, dchar c, size_t i = 0, size_t line = __LINE__)
    {
        enforce(stride(s, i) == codeLength!dchar(c),
                new AssertError(format("Unit test failure string: %s", s), __FILE__, line));

        enforce(stride(RandomCU!dchar(s), i) == codeLength!dchar(c),
                new AssertError(format("Unit test failure range: %s", s), __FILE__, line));

        auto refRandom = new RefRandomCU!dchar(s);
        immutable randLen = refRandom.length;
        enforce(stride(refRandom, i) == codeLength!dchar(c),
                new AssertError(format("Unit test failure rand ref range: %s", s), __FILE__, line));
        enforce(refRandom.length == randLen,
                new AssertError(format("Unit test failure rand ref range length: %s", s), __FILE__, line));

        if (i == 0)
        {
            enforce(stride(s) == codeLength!dchar(c),
                    new AssertError(format("Unit test failure string 0: %s", s), __FILE__, line));

            enforce(stride(InputCU!dchar(s)) == codeLength!dchar(c),
                    new AssertError(format("Unit test failure range 0: %s", s), __FILE__, line));

            auto refBidir = new RefBidirCU!dchar(s);
            immutable bidirLen = refBidir.length;
            enforce(stride(refBidir) == codeLength!dchar(c),
                    new AssertError(format("Unit test failure bidir ref range code length: %s", s), __FILE__, line));
            enforce(refBidir.length == bidirLen,
                    new AssertError(format("Unit test failure bidir ref range length: %s", s), __FILE__, line));
        }
    }

    assertCTFEable!(
    {
    test("a", 'a');
    test(" ", ' ');
    test("\u2029", '\u2029'); //paraSep
    test("\u0100", '\u0100');
    test("\u0430", '\u0430');
    test("\U00010143", '\U00010143');
    test("abcdefcdef", 'a');
    test("hello\U00010143\u0100\U00010143", 'h', 0);
    test("hello\U00010143\u0100\U00010143", 'e', 1);
    test("hello\U00010143\u0100\U00010143", 'l', 2);
    test("hello\U00010143\u0100\U00010143", 'l', 3);
    test("hello\U00010143\u0100\U00010143", 'o', 4);
    test("hello\U00010143\u0100\U00010143", '\U00010143', 5);
    test("hello\U00010143\u0100\U00010143", '\u0100', 6);
    test("hello\U00010143\u0100\U00010143", '\U00010143', 7);

    foreach (S; AliasSeq!(dchar[], const dchar[], dstring))
    {
        enum str = to!S("hello world");
        static assert(isSafe!(() => stride(str, 0)));
        static assert(isSafe!(() => stride(str)   ));
        static assert((functionAttributes!(() => stride(str, 0)) & FunctionAttribute.pure_) != 0);
        static assert((functionAttributes!(() => stride(str)   ) & FunctionAttribute.pure_) != 0);
    }
    });
}

private uint strideImpl(char c, size_t index) @trusted pure
in { assert(c & 0x80); }
do
{
    import core.bitop : bsr;
    immutable msbs = 7 - bsr((~uint(c)) & 0xFF);
    if (c == 0xFF || msbs < 2 || msbs > 4)
        throw new UTFException("Invalid UTF-8 sequence", index);
    return msbs;
}

/++
    Calculate the length of the UTF sequence ending one code unit before
    `index` in `str`.

    Params:
        str = bidirectional range of UTF code units. Must be random access if
        `index` is passed
        index = index one past end of UTF sequence (default: `str.length`)

    Returns:
        The number of code units in the UTF sequence. For UTF-8, this is a
        value between 1 and 4 (as per $(HTTP tools.ietf.org/html/rfc3629#section-3, RFC 3629$(COMMA) section 3)).
        For UTF-16, it is either 1 or 2. For UTF-32, it is always 1.

    Throws:
        May throw a `UTFException` if `str[index]` is not one past the
        end of a valid UTF sequence.

    Note:
        `strideBack` will only analyze the element at $(D str[index - 1])
        element. It will not fully verify the validity of the UTF sequence, nor
        even verify the presence of the sequence: it will not actually
        guarantee that $(D strideBack(str, index) <= index).
  +/
uint strideBack(S)(auto ref S str, size_t index)
if (is(S : const char[]) ||
    (isRandomAccessRange!S && is(immutable ElementType!S == immutable char)))
{
    static if (is(typeof(str.length) : ulong))
        assert(index <= str.length, "Past the end of the UTF-8 sequence");
    assert(index > 0, "Not the end of the UTF-8 sequence");

    if ((str[index-1] & 0b1100_0000) != 0b1000_0000)
        return 1;

    if (index >= 4) //single verification for most common case
    {
        static foreach (i; 2 .. 5)
        {
            if ((str[index-i] & 0b1100_0000) != 0b1000_0000)
                return i;
        }
    }
    else
    {
        static foreach (i; 2 .. 4)
        {
            if (index >= i && (str[index-i] & 0b1100_0000) != 0b1000_0000)
                return i;
        }
    }
    throw new UTFException("Not the end of the UTF sequence", index);
}

/// Ditto
uint strideBack(S)(auto ref S str)
if (is(S : const char[]) ||
    (isRandomAccessRange!S && hasLength!S && is(immutable ElementType!S == immutable char)))
{
    return strideBack(str, str.length);
}

/// Ditto
uint strideBack(S)(auto ref S str)
if (isBidirectionalRange!S && is(immutable ElementType!S == immutable char) && !isRandomAccessRange!S)
{
    assert(!str.empty, "Past the end of the UTF-8 sequence");
    auto temp = str.save;
    foreach (i; AliasSeq!(1, 2, 3, 4))
    {
        if ((temp.back & 0b1100_0000) != 0b1000_0000)
            return i;
        temp.popBack();
        if (temp.empty)
            break;
    }
    throw new UTFException("The last code unit is not the end of the UTF-8 sequence");
}

@system unittest
{
    import core.exception : AssertError;
    import std.conv : to;
    import std.exception;
    import std.string : format;
    import std.traits : FunctionAttribute, functionAttributes, isSafe;
    static void test(string s, dchar c, size_t i = size_t.max, size_t line = __LINE__)
    {
        enforce(strideBack(s, i == size_t.max ? s.length : i) == codeLength!char(c),
                new AssertError(format("Unit test failure string: %s", s), __FILE__, line));

        enforce(strideBack(RandomCU!char(s), i == size_t.max ? s.length : i) == codeLength!char(c),
                new AssertError(format("Unit test failure range: %s", s), __FILE__, line));

        auto refRandom = new RefRandomCU!char(s);
        immutable randLen = refRandom.length;
        enforce(strideBack(refRandom, i == size_t.max ? s.length : i) == codeLength!char(c),
                new AssertError(format("Unit test failure rand ref range: %s", s), __FILE__, line));
        enforce(refRandom.length == randLen,
                new AssertError(format("Unit test failure rand ref range length: %s", s), __FILE__, line));

        if (i == size_t.max)
        {
            enforce(strideBack(s) == codeLength!char(c),
                    new AssertError(format("Unit test failure string code length: %s", s), __FILE__, line));

            enforce(strideBack(BidirCU!char(s)) == codeLength!char(c),
                    new AssertError(format("Unit test failure range code length: %s", s), __FILE__, line));

            auto refBidir = new RefBidirCU!char(s);
            immutable bidirLen = refBidir.length;
            enforce(strideBack(refBidir) == codeLength!char(c),
                    new AssertError(format("Unit test failure bidir ref range code length: %s", s), __FILE__, line));
            enforce(refBidir.length == bidirLen,
                    new AssertError(format("Unit test failure bidir ref range length: %s", s), __FILE__, line));
        }
    }

    assertCTFEable!(
    {
    test("a", 'a');
    test(" ", ' ');
    test("\u2029", '\u2029'); //paraSep
    test("\u0100", '\u0100');
    test("\u0430", '\u0430');
    test("\U00010143", '\U00010143');
    test("abcdefcdef", 'f');
    test("\U00010143\u0100\U00010143hello", 'o', 15);
    test("\U00010143\u0100\U00010143hello", 'l', 14);
    test("\U00010143\u0100\U00010143hello", 'l', 13);
    test("\U00010143\u0100\U00010143hello", 'e', 12);
    test("\U00010143\u0100\U00010143hello", 'h', 11);
    test("\U00010143\u0100\U00010143hello", '\U00010143', 10);
    test("\U00010143\u0100\U00010143hello", '\u0100', 6);
    test("\U00010143\u0100\U00010143hello", '\U00010143', 4);

    foreach (S; AliasSeq!(char[], const char[], string))
    {
        enum str = to!S("hello world");
        static assert(isSafe!({ strideBack(str, 0); }));
        static assert(isSafe!({ strideBack(str);    }));
        static assert((functionAttributes!({ strideBack(str, 0); }) & FunctionAttribute.pure_) != 0);
        static assert((functionAttributes!({ strideBack(str);    }) & FunctionAttribute.pure_) != 0);
    }
    });
}

//UTF-16 is self synchronizing: The length of strideBack can be found from
//the value of a single wchar
/// Ditto
uint strideBack(S)(auto ref S str, size_t index)
if (is(S : const wchar[]) ||
    (isRandomAccessRange!S && is(immutable ElementType!S == immutable wchar)))
{
    static if (is(typeof(str.length) : ulong))
        assert(index <= str.length, "Past the end of the UTF-16 sequence");
    assert(index > 0, "Not the end of a UTF-16 sequence");

    immutable c2 = str[index-1];
    return 1 + (0xDC00 <= c2 && c2 < 0xE000);
}

/// Ditto
uint strideBack(S)(auto ref S str)
if (is(S : const wchar[]) ||
    (isBidirectionalRange!S && is(immutable ElementType!S == immutable wchar)))
{
    assert(!str.empty, "UTF-16 sequence is empty");

    static if (is(S : const(wchar)[]))
        immutable c2 = str[$ - 1];
    else
        immutable c2 = str.back;

    return 1 + (0xDC00 <= c2 && c2 <= 0xE000);
}

@system unittest
{
    import core.exception : AssertError;
    import std.conv : to;
    import std.exception;
    import std.string : format;
    import std.traits : FunctionAttribute, functionAttributes, isSafe;
    static void test(wstring s, dchar c, size_t i = size_t.max, size_t line = __LINE__)
    {
        enforce(strideBack(s, i == size_t.max ? s.length : i) == codeLength!wchar(c),
                new AssertError(format("Unit test failure string: %s", s), __FILE__, line));

        enforce(strideBack(RandomCU!wchar(s), i == size_t.max ? s.length : i) == codeLength!wchar(c),
                new AssertError(format("Unit test failure range: %s", s), __FILE__, line));

        auto refRandom = new RefRandomCU!wchar(s);
        immutable randLen = refRandom.length;
        enforce(strideBack(refRandom, i == size_t.max ? s.length : i) == codeLength!wchar(c),
                new AssertError(format("Unit test failure rand ref range: %s", s), __FILE__, line));
        enforce(refRandom.length == randLen,
                new AssertError(format("Unit test failure rand ref range length: %s", s), __FILE__, line));

        if (i == size_t.max)
        {
            enforce(strideBack(s) == codeLength!wchar(c),
                    new AssertError(format("Unit test failure string code length: %s", s), __FILE__, line));

            enforce(strideBack(BidirCU!wchar(s)) == codeLength!wchar(c),
                    new AssertError(format("Unit test failure range code length: %s", s), __FILE__, line));

            auto refBidir = new RefBidirCU!wchar(s);
            immutable bidirLen = refBidir.length;
            enforce(strideBack(refBidir) == codeLength!wchar(c),
                    new AssertError(format("Unit test failure bidir ref range code length: %s", s), __FILE__, line));
            enforce(refBidir.length == bidirLen,
                    new AssertError(format("Unit test failure bidir ref range length: %s", s), __FILE__, line));
        }
    }

    assertCTFEable!(
    {
    test("a", 'a');
    test(" ", ' ');
    test("\u2029", '\u2029'); //paraSep
    test("\u0100", '\u0100');
    test("\u0430", '\u0430');
    test("\U00010143", '\U00010143');
    test("abcdefcdef", 'f');
    test("\U00010143\u0100\U00010143hello", 'o', 10);
    test("\U00010143\u0100\U00010143hello", 'l', 9);
    test("\U00010143\u0100\U00010143hello", 'l', 8);
    test("\U00010143\u0100\U00010143hello", 'e', 7);
    test("\U00010143\u0100\U00010143hello", 'h', 6);
    test("\U00010143\u0100\U00010143hello", '\U00010143', 5);
    test("\U00010143\u0100\U00010143hello", '\u0100', 3);
    test("\U00010143\u0100\U00010143hello", '\U00010143', 2);

    foreach (S; AliasSeq!(wchar[], const wchar[], wstring))
    {
        enum str = to!S("hello world");
        static assert(isSafe!(() => strideBack(str, 0)));
        static assert(isSafe!(() => strideBack(str)   ));
        static assert((functionAttributes!(() => strideBack(str, 0)) & FunctionAttribute.pure_) != 0);
        static assert((functionAttributes!(() => strideBack(str)   ) & FunctionAttribute.pure_) != 0);
    }
    });
}

/// Ditto
uint strideBack(S)(auto ref S str, size_t index)
if (isRandomAccessRange!S && is(immutable ElementEncodingType!S == immutable dchar))
{
    static if (is(typeof(str.length) : ulong))
        assert(index <= str.length, "Past the end of the UTF-32 sequence");
    assert(index > 0, "Not the end of the UTF-32 sequence");
    return 1;
}

/// Ditto
uint strideBack(S)(auto ref S str)
if (isBidirectionalRange!S && is(immutable ElementEncodingType!S == immutable dchar))
{
    assert(!str.empty, "Empty UTF-32 sequence");
    return 1;
}

///
@safe unittest
{
    assert("a".strideBack == 1);
    assert("λ".strideBack == 2);
    assert("aλ".strideBack == 2);
    assert("aλ".strideBack(1) == 1);
    assert("𐐷".strideBack == 4);
}

@system unittest
{
    import core.exception : AssertError;
    import std.conv : to;
    import std.exception;
    import std.string : format;
    import std.traits : FunctionAttribute, functionAttributes, isSafe;
    static void test(dstring s, dchar c, size_t i = size_t.max, size_t line = __LINE__)
    {
        enforce(strideBack(s, i == size_t.max ? s.length : i) == codeLength!dchar(c),
                new AssertError(format("Unit test failure string: %s", s), __FILE__, line));

        enforce(strideBack(RandomCU!dchar(s), i == size_t.max ? s.length : i) == codeLength!dchar(c),
                new AssertError(format("Unit test failure range: %s", s), __FILE__, line));

        auto refRandom = new RefRandomCU!dchar(s);
        immutable randLen = refRandom.length;
        enforce(strideBack(refRandom, i == size_t.max ? s.length : i) == codeLength!dchar(c),
                new AssertError(format("Unit test failure rand ref range: %s", s), __FILE__, line));
        enforce(refRandom.length == randLen,
                new AssertError(format("Unit test failure rand ref range length: %s", s), __FILE__, line));

        if (i == size_t.max)
        {
            enforce(strideBack(s) == codeLength!dchar(c),
                    new AssertError(format("Unit test failure string code length: %s", s), __FILE__, line));

            enforce(strideBack(BidirCU!dchar(s)) == codeLength!dchar(c),
                    new AssertError(format("Unit test failure range code length: %s", s), __FILE__, line));

            auto refBidir = new RefBidirCU!dchar(s);
            immutable bidirLen = refBidir.length;
            enforce(strideBack(refBidir) == codeLength!dchar(c),
                    new AssertError(format("Unit test failure bidir ref range code length: %s", s), __FILE__, line));
            enforce(refBidir.length == bidirLen,
                    new AssertError(format("Unit test failure bidir ref range length: %s", s), __FILE__, line));
        }
    }

    assertCTFEable!(
    {
    test("a", 'a');
    test(" ", ' ');
    test("\u2029", '\u2029'); //paraSep
    test("\u0100", '\u0100');
    test("\u0430", '\u0430');
    test("\U00010143", '\U00010143');
    test("abcdefcdef", 'f');
    test("\U00010143\u0100\U00010143hello", 'o', 8);
    test("\U00010143\u0100\U00010143hello", 'l', 7);
    test("\U00010143\u0100\U00010143hello", 'l', 6);
    test("\U00010143\u0100\U00010143hello", 'e', 5);
    test("\U00010143\u0100\U00010143hello", 'h', 4);
    test("\U00010143\u0100\U00010143hello", '\U00010143', 3);
    test("\U00010143\u0100\U00010143hello", '\u0100', 2);
    test("\U00010143\u0100\U00010143hello", '\U00010143', 1);

    foreach (S; AliasSeq!(dchar[], const dchar[], dstring))
    {
        enum str = to!S("hello world");
        static assert(isSafe!(() => strideBack(str, 0)));
        static assert(isSafe!(() => strideBack(str)   ));
        static assert((functionAttributes!(() => strideBack(str, 0)) & FunctionAttribute.pure_) != 0);
        static assert((functionAttributes!(() => strideBack(str)   ) & FunctionAttribute.pure_) != 0);
    }
    });
}


/++
    Given `index` into `str` and assuming that `index` is at the start
    of a UTF sequence, `toUCSindex` determines the number of UCS characters
    up to `index`. So, `index` is the index of a code unit at the
    beginning of a code point, and the return value is how many code points into
    the string that that code point is.
  +/
size_t toUCSindex(C)(const(C)[] str, size_t index) @safe pure
if (isSomeChar!C)
{
    static if (is(immutable C == immutable dchar))
        return index;
    else
    {
        size_t n = 0;
        size_t j = 0;

        for (; j < index; ++n)
            j += stride(str, j);

        if (j > index)
        {
            static if (is(immutable C == immutable char))
                throw new UTFException("Invalid UTF-8 sequence", index);
            else
                throw new UTFException("Invalid UTF-16 sequence", index);
        }

        return n;
    }
}

///
@safe unittest
{
    assert(toUCSindex(`hello world`, 7) == 7);
    assert(toUCSindex(`hello world`w, 7) == 7);
    assert(toUCSindex(`hello world`d, 7) == 7);

    assert(toUCSindex(`Ma Chérie`, 7) == 6);
    assert(toUCSindex(`Ma Chérie`w, 7) == 7);
    assert(toUCSindex(`Ma Chérie`d, 7) == 7);

    assert(toUCSindex(`さいごの果実 / ミツバチと科学者`, 9) == 3);
    assert(toUCSindex(`さいごの果実 / ミツバチと科学者`w, 9) == 9);
    assert(toUCSindex(`さいごの果実 / ミツバチと科学者`d, 9) == 9);
}


/++
    Given a UCS index `n` into `str`, returns the UTF index.
    So, `n` is how many code points into the string the code point is, and
    the array index of the code unit is returned.
  +/
size_t toUTFindex(C)(const(C)[] str, size_t n) @safe pure
if (isSomeChar!C)
{
    static if (is(immutable C == immutable dchar))
    {
        return n;
    }
    else
    {
        size_t i;
        while (n--)
        {
            i += stride(str, i);
        }
        return i;
    }
}

///
@safe unittest
{
    assert(toUTFindex(`hello world`, 7) == 7);
    assert(toUTFindex(`hello world`w, 7) == 7);
    assert(toUTFindex(`hello world`d, 7) == 7);

    assert(toUTFindex(`Ma Chérie`, 6) == 7);
    assert(toUTFindex(`Ma Chérie`w, 7) == 7);
    assert(toUTFindex(`Ma Chérie`d, 7) == 7);

    assert(toUTFindex(`さいごの果実 / ミツバチと科学者`, 3) == 9);
    assert(toUTFindex(`さいごの果実 / ミツバチと科学者`w, 9) == 9);
    assert(toUTFindex(`さいごの果実 / ミツバチと科学者`d, 9) == 9);
}


/* =================== Decode ======================= */

/// Whether or not to replace invalid UTF with $(LREF replacementDchar)
alias UseReplacementDchar = Flag!"useReplacementDchar";

/++
    Decodes and returns the code point starting at `str[index]`. `index`
    is advanced to one past the decoded code point. If the code point is not
    well-formed, then a `UTFException` is thrown and `index` remains
    unchanged.

    decode will only work with strings and random access ranges of code units
    with length and slicing, whereas $(LREF decodeFront) will work with any
    input range of code units.

    Params:
        useReplacementDchar = if invalid UTF, return replacementDchar rather than throwing
        str = input string or indexable Range
        index = starting index into s[]; incremented by number of code units processed

    Returns:
        decoded character

    Throws:
        $(LREF UTFException) if `str[index]` is not the start of a valid UTF
        sequence and useReplacementDchar is `No.useReplacementDchar`
  +/
dchar decode(UseReplacementDchar useReplacementDchar = No.useReplacementDchar, S)(auto ref S str, ref size_t index)
if (!isSomeString!S &&
    isRandomAccessRange!S && hasSlicing!S && hasLength!S && isSomeChar!(ElementType!S))
in
{
    assert(index < str.length, "Attempted to decode past the end of a string");
}
out (result)
{
    assert(isValidDchar(result));
}
do
{
    if (str[index] < codeUnitLimit!S)
        return str[index++];
    else
        return decodeImpl!(true, useReplacementDchar)(str, index);
}

/// ditto
dchar decode(UseReplacementDchar useReplacementDchar = No.useReplacementDchar, S)(
auto ref scope S str, ref size_t index) @trusted pure
if (isSomeString!S)
in
{
    assert(index < str.length, "Attempted to decode past the end of a string");
}
out (result)
{
    assert(isValidDchar(result));
}
do
{
    if (str[index] < codeUnitLimit!S)
        return str[index++];
    else static if (is(immutable S == immutable C[], C))
        return decodeImpl!(true, useReplacementDchar)(cast(const(C)[]) str, index);
}

///
@safe pure unittest
{
    size_t i;

    assert("a".decode(i) == 'a' && i == 1);
    i = 0;
    assert("å".decode(i) == 'å' && i == 2);
    i = 1;
    assert("aå".decode(i) == 'å' && i == 3);
    i = 0;
    assert("å"w.decode(i) == 'å' && i == 1);

    // ë as a multi-code point grapheme
    i = 0;
    assert("e\u0308".decode(i) == 'e' && i == 1);
    // ë as a single code point grapheme
    i = 0;
    assert("ë".decode(i) == 'ë' && i == 2);
    i = 0;
    assert("ë"w.decode(i) == 'ë' && i == 1);
}

@safe pure unittest // https://issues.dlang.org/show_bug.cgi?id=22867
{
    import std.conv : hexString;
    string data = hexString!"f787a598";
    size_t offset = 0;
    try data.decode(offset);
    catch (UTFException ex) assert(offset == 0);
}

/++
    `decodeFront` is a variant of $(LREF decode) which specifically decodes
    the first code point. Unlike $(LREF decode), `decodeFront` accepts any
    $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
    of code units (rather than just a string or random access
    range). It also takes the range by `ref` and pops off the elements as it
    decodes them. If `numCodeUnits` is passed in, it gets set to the number
    of code units which were in the code point which was decoded.

    Params:
        useReplacementDchar = if invalid UTF, return replacementDchar rather than throwing
        str = input string or indexable Range
        numCodeUnits = set to number of code units processed

    Returns:
        decoded character

    Throws:
        $(LREF UTFException) if `str.front` is not the start of a valid UTF
        sequence. If an exception is thrown, then there is no guarantee as to
        the number of code units which were popped off, as it depends on the
        type of range being used and how many code units had to be popped off
        before the code point was determined to be invalid.
  +/
dchar decodeFront(UseReplacementDchar useReplacementDchar = No.useReplacementDchar, S)(
ref S str, out size_t numCodeUnits)
if (!isSomeString!S && isInputRange!S && isSomeChar!(ElementType!S))
in
{
    assert(!str.empty);
}
out (result)
{
    assert(isValidDchar(result));
}
do
{
    immutable fst = str.front;

    if (fst < codeUnitLimit!S)
    {
        str.popFront();
        numCodeUnits = 1;
        return fst;
    }
    else
    {
        // https://issues.dlang.org/show_bug.cgi?id=14447 forces canIndex to be
        // done outside of decodeImpl, which is undesirable, since not all
        // overloads of decodeImpl need it. So, it should be moved back into
        // decodeImpl once https://issues.dlang.org/show_bug.cgi?id=8521
        // has been fixed.
        enum canIndex = is(S : const char[]) || isRandomAccessRange!S && hasSlicing!S && hasLength!S;
        immutable retval = decodeImpl!(canIndex, useReplacementDchar)(str, numCodeUnits);

        // The other range types were already popped by decodeImpl.
        static if (isRandomAccessRange!S && hasSlicing!S && hasLength!S)
            str = str[numCodeUnits .. str.length];

        return retval;
    }
}

/// ditto
dchar decodeFront(UseReplacementDchar useReplacementDchar = No.useReplacementDchar, S)(
ref scope S str, out size_t numCodeUnits) @trusted pure
if (isSomeString!S)
in
{
    assert(!str.empty);
}
out (result)
{
    assert(isValidDchar(result));
}
do
{
    if (str[0] < codeUnitLimit!S)
    {
        numCodeUnits = 1;
        immutable retval = str[0];
        str = str[1 .. $];
        return retval;
    }
    else static if (is(immutable S == immutable C[], C))
    {
        immutable retval = decodeImpl!(true, useReplacementDchar)(cast(const(C)[]) str, numCodeUnits);
        str = str[numCodeUnits .. $];
        return retval;
    }
}

/++ Ditto +/
dchar decodeFront(UseReplacementDchar useReplacementDchar = No.useReplacementDchar, S)(ref S str)
if (isInputRange!S && isSomeChar!(ElementType!S))
{
    size_t numCodeUnits;
    return decodeFront!useReplacementDchar(str, numCodeUnits);
}

///
@safe pure unittest
{
    import std.range.primitives;
    string str = "Hello, World!";

    assert(str.decodeFront == 'H' && str == "ello, World!");
    str = "å";
    assert(str.decodeFront == 'å' && str.empty);
    str = "å";
    size_t i;
    assert(str.decodeFront(i) == 'å' && i == 2 && str.empty);
}

/++
    `decodeBack` is a variant of $(LREF decode) which specifically decodes
    the last code point. Unlike $(LREF decode), `decodeBack` accepts any
    bidirectional range of code units (rather than just a string or random access
    range). It also takes the range by `ref` and pops off the elements as it
    decodes them. If `numCodeUnits` is passed in, it gets set to the number
    of code units which were in the code point which was decoded.

    Params:
        useReplacementDchar = if invalid UTF, return `replacementDchar` rather than throwing
        str = input string or bidirectional Range
        numCodeUnits = gives the number of code units processed

    Returns:
        A decoded UTF character.

    Throws:
        $(LREF UTFException) if `str.back` is not the end of a valid UTF
        sequence. If an exception is thrown, the `str` itself remains unchanged,
        but there is no guarantee as to the value of `numCodeUnits` (when passed).
  +/
dchar decodeBack(UseReplacementDchar useReplacementDchar = No.useReplacementDchar, S)(
    ref S str, out size_t numCodeUnits)
if (isSomeString!S)
in
{
    assert(!str.empty);
}
out (result)
{
    assert(isValidDchar(result));
}
do
{
    if (str[$ - 1] < codeUnitLimit!S)
    {
        numCodeUnits = 1;
        immutable retval = str[$ - 1];
        str = str[0 .. $ - 1];
        return retval;
    }
    else static if (is(immutable S == immutable C[], C))
    {
        numCodeUnits = strideBack(str);
        immutable newLength = str.length - numCodeUnits;
        size_t index = newLength;
        immutable retval = decodeImpl!(true, useReplacementDchar)(cast(const(C)[]) str, index);
        str = str[0 .. newLength];
        return retval;
    }
}

/++ Ditto +/
dchar decodeBack(UseReplacementDchar useReplacementDchar = No.useReplacementDchar, S)(
    ref S str, out size_t numCodeUnits)
if (!isSomeString!S && isSomeChar!(ElementType!S) && isBidirectionalRange!S
    && ((isRandomAccessRange!S && hasLength!S) || !isRandomAccessRange!S))
in
{
    assert(!str.empty);
}
out (result)
{
    assert(isValidDchar(result));
}
do
{
    if (str.back < codeUnitLimit!S)
    {
        numCodeUnits = 1;
        immutable retval = str.back;
        str.popBack();
        return retval;
    }
    else
    {
        numCodeUnits = strideBack(str);
        static if (isRandomAccessRange!S)
        {
            size_t index = str.length - numCodeUnits;
            immutable retval = decodeImpl!(true, useReplacementDchar)(str, index);
            str.popBackExactly(numCodeUnits);
            return retval;
        }
        else
        {
            alias Char = Unqual!(ElementType!S);
            Char[4] codeUnits;
            S tmp = str.save;
            for (size_t i = numCodeUnits; i > 0; )
            {
                codeUnits[--i] = tmp.back;
                tmp.popBack();
            }
            const Char[] codePoint = codeUnits[0 .. numCodeUnits];
            size_t index = 0;
            immutable retval = decodeImpl!(true, useReplacementDchar)(codePoint, index);
            str = tmp;
            return retval;
        }
    }
}

/++ Ditto +/
dchar decodeBack(UseReplacementDchar useReplacementDchar = No.useReplacementDchar, S)(ref S str)
if (isSomeString!S
    || (isRandomAccessRange!S && hasLength!S && isSomeChar!(ElementType!S))
    || (!isRandomAccessRange!S && isBidirectionalRange!S && isSomeChar!(ElementType!S)))
in
{
    assert(!str.empty);
}
out (result)
{
    assert(isValidDchar(result));
}
do
{
    size_t numCodeUnits;
    return decodeBack!useReplacementDchar(str, numCodeUnits);
}

///
@system pure unittest
{
    import std.range.primitives;
    string str = "Hello, World!";

    assert(str.decodeBack == '!' && str == "Hello, World");
    str = "å";
    assert(str.decodeBack == 'å' && str.empty);
    str = "å";
    size_t i;
    assert(str.decodeBack(i) == 'å' && i == 2 && str.empty);
}

// For the given range, code unit values less than this
// are guaranteed to be valid single-codepoint encodings.
package template codeUnitLimit(S)
if (isSomeChar!(ElementEncodingType!S))
{
    static if (is(immutable ElementEncodingType!S == immutable char))
        enum char codeUnitLimit = 0x80;
    else static if (is(immutable ElementEncodingType!S == immutable wchar))
        enum wchar codeUnitLimit = 0xD800;
    else
        enum dchar codeUnitLimit = 0xD800;
}

/*
 * For strings, this function does its own bounds checking to give a
 * more useful error message when attempting to decode past the end of a string.
 * Subsequently it uses a pointer instead of an array to avoid
 * redundant bounds checking.
 *
 * The three overloads of this operate on chars, wchars, and dchars.
 *
 * Params:
 *      canIndex = if S is indexable
 *      useReplacementDchar = if invalid UTF, return replacementDchar rather than throwing
 *      str = input string or Range
 *      index = starting index into s[]; incremented by number of code units processed
 *
 * Returns:
 *      decoded character
 */
private dchar decodeImpl(bool canIndex, UseReplacementDchar useReplacementDchar = No.useReplacementDchar, S)(
    auto ref S str, ref size_t index)
if (
    is(S : const char[]) || (isInputRange!S && is(immutable ElementEncodingType!S == immutable char)))
{
    /* The following encodings are valid, except for the 5 and 6 byte
     * combinations:
     *  0xxxxxxx
     *  110xxxxx 10xxxxxx
     *  1110xxxx 10xxxxxx 10xxxxxx
     *  11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
     *  111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
     *  1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
     */

    /* Dchar bitmask for different numbers of UTF-8 code units.
     */
    alias bitMask = AliasSeq!((1 << 7) - 1, (1 << 11) - 1, (1 << 16) - 1, (1 << 21) - 1);

    static if (is(S : const char[]))
        auto pstr = str.ptr + index;    // this is what makes decodeImpl() @system code
    else static if (isRandomAccessRange!S && hasSlicing!S && hasLength!S)
        auto pstr = str[index .. str.length];
    else
        alias pstr = str;

    // https://issues.dlang.org/show_bug.cgi?id=14447 forces this to be done
    // outside of decodeImpl
    //enum canIndex = is(S : const char[]) || (isRandomAccessRange!S && hasSlicing!S && hasLength!S);

    static if (canIndex)
    {
        immutable length = str.length - index;
        ubyte fst = pstr[0];
    }
    else
    {
        ubyte fst = pstr.front;
        pstr.popFront();
    }

    static if (!useReplacementDchar)
    {
        static if (canIndex)
        {
            static UTFException exception(S)(S str, string msg)
            {
                uint[4] sequence = void;
                size_t i;

                do
                {
                    sequence[i] = str[i];
                } while (++i < str.length && i < 4 && (str[i] & 0xC0) == 0x80);

                return new UTFException(msg, i).setSequence(sequence[0 .. i]);
            }
        }

        UTFException invalidUTF()
        {
            static if (canIndex)
               return exception(pstr[0 .. length], "Invalid UTF-8 sequence");
            else
            {
                //We can't include the invalid sequence with input strings without
                //saving each of the code units along the way, and we can't do it with
                //forward ranges without saving the entire range. Both would incur a
                //cost for the decoding of every character just to provide a better
                //error message for the (hopefully) rare case when an invalid UTF-8
                //sequence is encountered, so we don't bother trying to include the
                //invalid sequence here, unlike with strings and sliceable ranges.
               return new UTFException("Invalid UTF-8 sequence");
            }
        }

        UTFException outOfBounds()
        {
            static if (canIndex)
               return exception(pstr[0 .. length], "Attempted to decode past the end of a string");
            else
               return new UTFException("Attempted to decode past the end of a string");
        }
    }

    if ((fst & 0b1100_0000) != 0b1100_0000)
    {
        static if (useReplacementDchar)
        {
            ++index;            // always consume bad input to avoid infinite loops
            return replacementDchar;
        }
        else
            throw invalidUTF(); // starter must have at least 2 first bits set
    }
    ubyte tmp = void;
    dchar d = fst; // upper control bits are masked out later
    fst <<= 1;

    foreach (i; AliasSeq!(1, 2, 3))
    {

        static if (canIndex)
        {
            if (i == length)
            {
                static if (useReplacementDchar)
                {
                    index += i;
                    return replacementDchar;
                }
                else
                    throw outOfBounds();
            }
        }
        else
        {
            if (pstr.empty)
            {
                static if (useReplacementDchar)
                {
                    index += i;
                    return replacementDchar;
                }
                else
                    throw outOfBounds();
            }
        }

        static if (canIndex)
            tmp = pstr[i];
        else
        {
            tmp = pstr.front;
            pstr.popFront();
        }

        if ((tmp & 0xC0) != 0x80)
        {
            static if (useReplacementDchar)
            {
                index += i + 1;
                return replacementDchar;
            }
            else
                throw invalidUTF();
        }

        d = (d << 6) | (tmp & 0x3F);
        fst <<= 1;

        if (!(fst & 0x80)) // no more bytes
        {
            d &= bitMask[i]; // mask out control bits

            // overlong, could have been encoded with i bytes
            if ((d & ~bitMask[i - 1]) == 0)
            {
                static if (useReplacementDchar)
                {
                    index += i + 1;
                    return replacementDchar;
                }
                else
                    throw invalidUTF();
            }

            // check for surrogates only needed for 3 bytes
            static if (i == 2)
            {
                if (!isValidDchar(d))
                {
                    static if (useReplacementDchar)
                    {
                        index += i + 1;
                        return replacementDchar;
                    }
                    else
                        throw invalidUTF();
                }
            }

            static if (i == 3)
            {
                if (d > dchar.max)
                {
                    static if (useReplacementDchar)
                        d = replacementDchar;
                    else
                        throw invalidUTF();
                }
            }

            index += i + 1;
            return d;
        }
    }

    static if (useReplacementDchar)
    {
        index += 4;             // read 4 chars by now
        return replacementDchar;
    }
    else
        throw invalidUTF();
}

@safe pure @nogc nothrow
unittest
{
    // Add tests for useReplacemendDchar == yes path

    static struct R
    {
      @safe pure @nogc nothrow:
        this(string s) { this.s = s; }
        @property bool empty() { return idx == s.length; }
        @property char front() { return s[idx]; }
        void popFront() { ++idx; }
        size_t idx;
        string s;
    }

    foreach (s; invalidUTFstrings!char())
    {
        auto r = R(s);
        size_t index;
        dchar dc = decodeImpl!(false, Yes.useReplacementDchar)(r, index);
        assert(dc == replacementDchar);
        assert(1 <= index && index <= s.length);
    }
}

private dchar decodeImpl(bool canIndex, UseReplacementDchar useReplacementDchar = No.useReplacementDchar, S)
(auto ref S str, ref size_t index)
if (is(S : const wchar[]) || (isInputRange!S && is(immutable ElementEncodingType!S == immutable wchar)))
{
    static if (is(S : const wchar[]))
        auto pstr = str.ptr + index;
    else static if (isRandomAccessRange!S && hasSlicing!S && hasLength!S)
        auto pstr = str[index .. str.length];
    else
        alias pstr = str;

    // https://issues.dlang.org/show_bug.cgi?id=14447 forces this to be done
    // outside of decodeImpl
    //enum canIndex = is(S : const wchar[]) || (isRandomAccessRange!S && hasSlicing!S && hasLength!S);

    static if (canIndex)
    {
        immutable length = str.length - index;
        uint u = pstr[0];
    }
    else
    {
        uint u = pstr.front;
        pstr.popFront();
    }

    static if (!useReplacementDchar)
    {
        UTFException exception(string msg)
        {
            static if (canIndex)
                return new UTFException(msg).setSequence(pstr[0]);
            else
                return new UTFException(msg);
        }
    }

    // The < case must be taken care of before decodeImpl is called.
    assert(u >= 0xD800);

    if (u <= 0xDBFF)
    {
        static if (canIndex)
            immutable onlyOneCodeUnit = length == 1;
        else
            immutable onlyOneCodeUnit = pstr.empty;

        if (onlyOneCodeUnit)
        {
            static if (useReplacementDchar)
            {
                ++index;
                return replacementDchar;
            }
            else
                throw exception("surrogate UTF-16 high value past end of string");
        }

        static if (canIndex)
            immutable uint u2 = pstr[1];
        else
        {
            immutable uint u2 = pstr.front;
            pstr.popFront();
        }

        if (u2 < 0xDC00 || u2 > 0xDFFF)
        {
            static if (useReplacementDchar)
                u = replacementDchar;
            else
                throw exception("surrogate UTF-16 low value out of range");
        }
        else
            u = ((u - 0xD7C0) << 10) + (u2 - 0xDC00);
        ++index;
    }
    else if (u >= 0xDC00 && u <= 0xDFFF)
    {
        static if (useReplacementDchar)
            u = replacementDchar;
        else
            throw exception("unpaired surrogate UTF-16 value");
    }
    ++index;

    // Note: u+FFFE and u+FFFF are specifically permitted by the
    // Unicode standard for application internal use (see isValidDchar)

    return cast(dchar) u;
}

@safe pure @nogc nothrow
unittest
{
    // Add tests for useReplacemendDchar == true path

    static struct R
    {
      @safe pure @nogc nothrow:
        this(wstring s) { this.s = s; }
        @property bool empty() { return idx == s.length; }
        @property wchar front() { return s[idx]; }
        void popFront() { ++idx; }
        size_t idx;
        wstring s;
    }

    foreach (s; invalidUTFstrings!wchar())
    {
        auto r = R(s);
        size_t index;
        dchar dc = decodeImpl!(false, Yes.useReplacementDchar)(r, index);
        assert(dc == replacementDchar);
        assert(1 <= index && index <= s.length);
    }
}

private dchar decodeImpl(bool canIndex, UseReplacementDchar useReplacementDchar = No.useReplacementDchar, S)(
    auto ref S str, ref size_t index)
if (is(S : const dchar[]) || (isInputRange!S && is(immutable ElementEncodingType!S == immutable dchar)))
{
    static if (is(S : const dchar[]))
        auto pstr = str.ptr;
    else
        alias pstr = str;

    static if (is(S : const dchar[]) || isRandomAccessRange!S)
    {
        dchar dc = pstr[index];
        if (!isValidDchar(dc))
        {
            static if (useReplacementDchar)
                dc = replacementDchar;
            else
                throw new UTFException("Invalid UTF-32 value").setSequence(dc);
        }
        ++index;
        return dc;
    }
    else
    {
        dchar dc = pstr.front;
        if (!isValidDchar(dc))
        {
            static if (useReplacementDchar)
                dc = replacementDchar;
            else
                throw new UTFException("Invalid UTF-32 value").setSequence(dc);
        }
        ++index;
        pstr.popFront();
        return dc;
    }
}

@safe pure @nogc nothrow
unittest
{
    // Add tests for useReplacemendDchar == true path

    static struct R
    {
      @safe pure @nogc nothrow:
        this(dstring s) { this.s = s; }
        @property bool empty() { return idx == s.length; }
        @property dchar front() { return s[idx]; }
        void popFront() { ++idx; }
        size_t idx;
        dstring s;
    }

    foreach (s; invalidUTFstrings!dchar())
    {
        auto r = R(s);
        size_t index;
        dchar dc = decodeImpl!(false, Yes.useReplacementDchar)(r, index);
        assert(dc == replacementDchar);
        assert(1 <= index && index <= s.length);
    }
}


version (StdUnittest) private void testDecode(R)(R range,
                                             size_t index,
                                             dchar expectedChar,
                                             size_t expectedIndex,
                                             size_t line = __LINE__)
{
    import core.exception : AssertError;
    import std.exception : enforce;
    import std.string : format;
    import std.traits : isNarrowString;

    static if (hasLength!R)
        immutable lenBefore = range.length;

    static if (isRandomAccessRange!R && !isNarrowString!R)
    {
        {
            immutable result = decode(range, index);
            enforce(result == expectedChar,
                    new AssertError(format("decode: Wrong character: %s", result), __FILE__, line));
            enforce(index == expectedIndex,
                    new AssertError(format("decode: Wrong index: %s", index), __FILE__, line));
            static if (hasLength!R)
            {
                enforce(range.length == lenBefore,
                        new AssertError(format("decode: length changed: %s", range.length), __FILE__, line));
            }
        }
    }
}

version (StdUnittest) private void testDecodeFront(R)(ref R range,
                                                  dchar expectedChar,
                                                  size_t expectedNumCodeUnits,
                                                  size_t line = __LINE__)
{
    import core.exception : AssertError;
    import std.exception : enforce;
    import std.string : format;

    static if (hasLength!R)
        immutable lenBefore = range.length;

    size_t numCodeUnits;
    immutable result = decodeFront(range, numCodeUnits);
    enforce(result == expectedChar,
            new AssertError(format("decodeFront: Wrong character: %s", result), __FILE__, line));
    enforce(numCodeUnits == expectedNumCodeUnits,
            new AssertError(format("decodeFront: Wrong numCodeUnits: %s", numCodeUnits), __FILE__, line));

    static if (hasLength!R)
    {
        enforce(range.length == lenBefore - numCodeUnits,
                new AssertError(format("decodeFront: wrong length: %s", range.length), __FILE__, line));
    }
}

version (StdUnittest) private void testDecodeBack(R)(ref R range,
                                                 dchar expectedChar,
                                                 size_t expectedNumCodeUnits,
                                                 size_t line = __LINE__)
{
    // This condition is to allow unit testing all `decode` functions together
    static if (!isBidirectionalRange!R)
        return;
    else
    {
        import core.exception : AssertError;
        import std.exception : enforce;
        import std.string : format;

        static if (hasLength!R)
            immutable lenBefore = range.length;

        size_t numCodeUnits;
        immutable result = decodeBack(range, numCodeUnits);
        enforce(result == expectedChar,
                new AssertError(format("decodeBack: Wrong character: %s", result), __FILE__, line));
        enforce(numCodeUnits == expectedNumCodeUnits,
                new AssertError(format("decodeBack: Wrong numCodeUnits: %s", numCodeUnits), __FILE__, line));

        static if (hasLength!R)
        {
            enforce(range.length == lenBefore - numCodeUnits,
                    new AssertError(format("decodeBack: wrong length: %s", range.length), __FILE__, line));
        }
    }
}

version (StdUnittest) private void testAllDecode(R)(R range,
                                                dchar expectedChar,
                                                size_t expectedIndex,
                                                size_t line = __LINE__)
{
    testDecode(range, 0, expectedChar, expectedIndex, line);
    static if (isBidirectionalRange!R)
    {
        auto rangeCopy = range.save;
        testDecodeBack(rangeCopy, expectedChar, expectedIndex, line);
    }
    testDecodeFront(range, expectedChar, expectedIndex, line);
}

version (StdUnittest) private void testBadDecode(R)(R range, size_t index, size_t line = __LINE__)
{
    import core.exception : AssertError;
    import std.exception : assertThrown, enforce;
    import std.string : format;

    immutable initialIndex = index;

    static if (hasLength!R)
        immutable lenBefore = range.length;

    static if (isRandomAccessRange!R)
    {
        assertThrown!UTFException(decode(range, index), null, __FILE__, line);
        enforce(index == initialIndex,
                new AssertError(format("decode: Wrong index: %s", index), __FILE__, line));
        static if (hasLength!R)
        {
            enforce(range.length == lenBefore,
                    new AssertError(format("decode: length changed:", range.length), __FILE__, line));
        }
    }

    if (initialIndex == 0)
        assertThrown!UTFException(decodeFront(range, index), null, __FILE__, line);
}

version (StdUnittest) private void testBadDecodeBack(R)(R range, size_t line = __LINE__)
{
    // This condition is to allow unit testing all `decode` functions together
    static if (!isBidirectionalRange!R)
        return;
    else
    {
        import core.exception : AssertError;
        import std.exception : assertThrown, enforce;
        import std.string : format;

        static if (hasLength!R)
            immutable lenBefore = range.length;

        static if (isRandomAccessRange!R)
        {
            assertThrown!UTFException(decodeBack(range), null, __FILE__, line);
            static if (hasLength!R)
            {
                enforce(range.length == lenBefore,
                        new AssertError(format("decodeBack: length changed:", range.length), __FILE__, line));
            }
        }
    }
}

@system unittest
{
    import std.conv : to;
    import std.exception;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(to!string, InputCU!char, RandomCU!char,
                          (string s) => new RefBidirCU!char(s),
                          (string s) => new RefRandomCU!char(s)))
    {
        enum sHasLength = hasLength!(typeof(S("abcd")));

        {
            auto range = S("abcd");
            testDecode(range, 0, 'a', 1);
            testDecode(range, 1, 'b', 2);
            testDecodeFront(range, 'a', 1);
            testDecodeFront(range, 'b', 1);
            assert(decodeFront(range) == 'c');
            assert(decodeFront(range) == 'd');
        }

        {
            auto range = S("ウェブサイト");
            testDecode(range, 0, 'ウ', 3);
            testDecode(range, 3, 'ェ', 6);
            testDecodeFront(range, 'ウ', 3);
            testDecodeFront(range, 'ェ', 3);
            assert(decodeFront(range) == 'ブ');
            assert(decodeFront(range) == 'サ');
        }

        {
            auto range = S("abcd");
            testDecodeBack(range, 'd', 1);
            testDecodeBack(range, 'c', 1);
            testDecodeBack(range, 'b', 1);
            testDecodeBack(range, 'a', 1);
        }

        {
            auto range = S("ウェブサイト");
            testDecodeBack(range, 'ト', 3);
            testDecodeBack(range, 'イ', 3);
            testDecodeBack(range, 'サ', 3);
            testDecodeBack(range, 'ブ', 3);
        }

        testAllDecode(S("\xC2\xA9"), '\u00A9', 2);
        testAllDecode(S("\xE2\x89\xA0"), '\u2260', 3);

        foreach (str; ["\xE2\x89", // too short
                       "\xC0\x8A",
                       "\xE0\x80\x8A",
                       "\xF0\x80\x80\x8A",
                       "\xF8\x80\x80\x80\x8A",
                       "\xFC\x80\x80\x80\x80\x8A"])
        {
            testBadDecode(S(str), 0);
            testBadDecode(S(str), 1);
            testBadDecodeBack(S(str));
        }

        //Invalid UTF-8 sequence where the first code unit is valid.
        testAllDecode(S("\xEF\xBF\xBE"), cast(dchar) 0xFFFE, 3);
        testAllDecode(S("\xEF\xBF\xBF"), cast(dchar) 0xFFFF, 3);

        //Invalid UTF-8 sequence where the first code unit isn't valid.
        foreach (str; ["\xED\xA0\x80",
                       "\xED\xAD\xBF",
                       "\xED\xAE\x80",
                       "\xED\xAF\xBF",
                       "\xED\xB0\x80",
                       "\xED\xBE\x80",
                       "\xED\xBF\xBF"])
        {
            testBadDecode(S(str), 0);
            testBadDecodeBack(S(str));
        }
    }
    });
}

@system unittest
{
    import std.exception;
    assertCTFEable!(
    {
    foreach (S; AliasSeq!((wstring s) => s, InputCU!wchar, RandomCU!wchar,
                          (wstring s) => new RefBidirCU!wchar(s),
                          (wstring s) => new RefRandomCU!wchar(s)))
    {
        testAllDecode(S([cast(wchar) 0x1111]), cast(dchar) 0x1111, 1);
        testAllDecode(S([cast(wchar) 0xD800, cast(wchar) 0xDC00]), cast(dchar) 0x10000, 2);
        testAllDecode(S([cast(wchar) 0xDBFF, cast(wchar) 0xDFFF]), cast(dchar) 0x10FFFF, 2);
        testAllDecode(S([cast(wchar) 0xFFFE]), cast(dchar) 0xFFFE, 1);
        testAllDecode(S([cast(wchar) 0xFFFF]), cast(dchar) 0xFFFF, 1);

        testBadDecode(S([ cast(wchar) 0xD801 ]), 0);
        testBadDecode(S([ cast(wchar) 0xD800, cast(wchar) 0x1200 ]), 0);

        testBadDecodeBack(S([ cast(wchar) 0xD801 ]));
        testBadDecodeBack(S([ cast(wchar) 0x0010, cast(wchar) 0xD800 ]));

        {
            auto range = S("ウェブサイト");
            testDecode(range, 0, 'ウ', 1);
            testDecode(range, 1, 'ェ', 2);
            testDecodeFront(range, 'ウ', 1);
            testDecodeFront(range, 'ェ', 1);
            assert(decodeFront(range) == 'ブ');
            assert(decodeFront(range) == 'サ');
        }

        {
            auto range = S("ウェブサイト");
            testDecodeBack(range, 'ト', 1);
            testDecodeBack(range, 'イ', 1);
            testDecodeBack(range, 'サ', 1);
            testDecodeBack(range, 'ブ', 1);
        }
    }

    foreach (S; AliasSeq!((wchar[] s) => s.idup, RandomCU!wchar, (wstring s) => new RefRandomCU!wchar(s)))
    {
        auto str = S([cast(wchar) 0xD800, cast(wchar) 0xDC00,
                      cast(wchar) 0x1400,
                      cast(wchar) 0xDAA7, cast(wchar) 0xDDDE]);
        testDecode(str, 0, cast(dchar) 0x10000, 2);
        testDecode(str, 2, cast(dchar) 0x1400, 3);
        testDecode(str, 3, cast(dchar) 0xB9DDE, 5);
        testDecodeBack(str, cast(dchar) 0xB9DDE, 2);
        testDecodeBack(str, cast(dchar) 0x1400, 1);
        testDecodeBack(str, cast(dchar) 0x10000, 2);
    }
    });
}

@system unittest
{
    import std.exception;
    assertCTFEable!(
    {
    foreach (S; AliasSeq!((dstring s) => s, RandomCU!dchar, InputCU!dchar,
                          (dstring s) => new RefBidirCU!dchar(s),
                          (dstring s) => new RefRandomCU!dchar(s)))
    {
        testAllDecode(S([cast(dchar) 0x1111]), cast(dchar) 0x1111, 1);
        testAllDecode(S([cast(dchar) 0x10000]), cast(dchar) 0x10000, 1);
        testAllDecode(S([cast(dchar) 0x10FFFF]), cast(dchar) 0x10FFFF, 1);
        testAllDecode(S([cast(dchar) 0xFFFE]), cast(dchar) 0xFFFE, 1);
        testAllDecode(S([cast(dchar) 0xFFFF]), cast(dchar) 0xFFFF, 1);

        testBadDecode(S([cast(dchar) 0xD800]), 0);
        testBadDecode(S([cast(dchar) 0xDFFE]), 0);
        testBadDecode(S([cast(dchar) 0x110000]), 0);

        testBadDecodeBack(S([cast(dchar) 0xD800]));
        testBadDecodeBack(S([cast(dchar) 0xDFFE]));
        testBadDecodeBack(S([cast(dchar) 0x110000]));

        {
            auto range = S("ウェブサイト");
            testDecode(range, 0, 'ウ', 1);
            testDecode(range, 1, 'ェ', 2);
            testDecodeFront(range, 'ウ', 1);
            testDecodeFront(range, 'ェ', 1);
            assert(decodeFront(range) == 'ブ');
            assert(decodeFront(range) == 'サ');
        }

        {
            auto range = S("ウェブサイト");
            testDecodeBack(range, 'ト', 1);
            testDecodeBack(range, 'イ', 1);
            testDecodeBack(range, 'サ', 1);
            testDecodeBack(range, 'ブ', 1);
        }
    }

    foreach (S; AliasSeq!((dchar[] s) => s.idup, RandomCU!dchar, (dstring s) => new RefRandomCU!dchar(s)))
    {
        auto str = S([cast(dchar) 0x10000, cast(dchar) 0x1400, cast(dchar) 0xB9DDE]);
        testDecode(str, 0, 0x10000, 1);
        testDecode(str, 1, 0x1400, 2);
        testDecode(str, 2, 0xB9DDE, 3);
        testDecodeBack(str, cast(dchar) 0xB9DDE, 1);
        testDecodeBack(str, cast(dchar) 0x1400, 1);
        testDecodeBack(str, cast(dchar) 0x10000, 1);
    }
    });
}

@safe unittest
{
    import std.exception;
    import std.traits : FunctionAttribute, functionAttributes, isSafe;
    assertCTFEable!(
    {
    foreach (S; AliasSeq!( char[], const( char)[],  string,
                          wchar[], const(wchar)[], wstring,
                          dchar[], const(dchar)[], dstring))
    {
        static assert(isSafe!({ S str; size_t i = 0; decode(str, i);      }));
        static assert(isSafe!({ S str; size_t i = 0; decodeFront(str, i); }));
        static assert(isSafe!({ S str; decodeFront(str); }));
        static assert((functionAttributes!({ S str; size_t i = 0; decode(str, i); }) & FunctionAttribute.pure_) != 0);
        static assert((functionAttributes!({
            S str; size_t i = 0; decodeFront(str, i);
        }) & FunctionAttribute.pure_) != 0);
        static assert((functionAttributes!({ S str; decodeFront(str); }) & FunctionAttribute.pure_) != 0);
        static assert((functionAttributes!({
            S str; size_t i = 0; decodeBack(str, i);
        }) & FunctionAttribute.pure_) != 0);
        static assert((functionAttributes!({ S str; decodeBack(str); }) & FunctionAttribute.pure_) != 0);
    }
    });
}

@safe unittest
{
    import std.exception;
    char[4] val;
    val[0] = 0b1111_0111;
    val[1] = 0b1011_1111;
    val[2] = 0b1011_1111;
    val[3] = 0b1011_1111;
    size_t i = 0;
    assertThrown!UTFException((){ dchar ch = decode(val[], i); }());
}
/* =================== Encode ======================= */

private dchar _utfException(UseReplacementDchar useReplacementDchar)(string msg, dchar c)
{
    static if (useReplacementDchar)
        return replacementDchar;
    else
        throw new UTFException(msg).setSequence(c);
}

/++
    Encodes `c` into the static array, `buf`, and returns the actual
    length of the encoded character (a number between `1` and `4` for
    `char[4]` buffers and a number between `1` and `2` for
    `wchar[2]` buffers).

    Throws:
        `UTFException` if `c` is not a valid UTF code point.
  +/
size_t encode(UseReplacementDchar useReplacementDchar = No.useReplacementDchar)(
    out char[4] buf, dchar c) @safe pure
{
    if (c <= 0x7F)
    {
        assert(isValidDchar(c));
        buf[0] = cast(char) c;
        return 1;
    }
    if (c <= 0x7FF)
    {
        assert(isValidDchar(c));
        buf[0] = cast(char)(0xC0 | (c >> 6));
        buf[1] = cast(char)(0x80 | (c & 0x3F));
        return 2;
    }
    if (c <= 0xFFFF)
    {
        if (0xD800 <= c && c <= 0xDFFF)
            c = _utfException!useReplacementDchar("Encoding a surrogate code point in UTF-8", c);

        assert(isValidDchar(c));
    L3:
        buf[0] = cast(char)(0xE0 | (c >> 12));
        buf[1] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        buf[2] = cast(char)(0x80 | (c & 0x3F));
        return 3;
    }
    if (c <= 0x10FFFF)
    {
        assert(isValidDchar(c));
        buf[0] = cast(char)(0xF0 | (c >> 18));
        buf[1] = cast(char)(0x80 | ((c >> 12) & 0x3F));
        buf[2] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        buf[3] = cast(char)(0x80 | (c & 0x3F));
        return 4;
    }

    assert(!isValidDchar(c));
    c = _utfException!useReplacementDchar("Encoding an invalid code point in UTF-8", c);
    goto L3;
}

///
@safe unittest
{
    import std.exception : assertThrown;
    import std.typecons : Yes;

    char[4] buf;

    assert(encode(buf, '\u0000') == 1 && buf[0 .. 1] == "\u0000");
    assert(encode(buf, '\u007F') == 1 && buf[0 .. 1] == "\u007F");
    assert(encode(buf, '\u0080') == 2 && buf[0 .. 2] == "\u0080");
    assert(encode(buf, '\uE000') == 3 && buf[0 .. 3] == "\uE000");
    assert(encode(buf, 0xFFFE) == 3 && buf[0 .. 3] == "\xEF\xBF\xBE");
    assertThrown!UTFException(encode(buf, cast(dchar) 0x110000));

    encode!(Yes.useReplacementDchar)(buf, cast(dchar) 0x110000);
    auto slice = buf[];
    assert(slice.decodeFront == replacementDchar);
}

///
@safe unittest
{
    import std.exception : assertThrown;
    import std.typecons : Yes;

    wchar[2] buf;

    assert(encode(buf, '\u0000') == 1 && buf[0 .. 1] == "\u0000");
    assert(encode(buf, '\uD7FF') == 1 && buf[0 .. 1] == "\uD7FF");
    assert(encode(buf, '\uE000') == 1 && buf[0 .. 1] == "\uE000");
    assert(encode(buf, '\U00010000') == 2 && buf[0 .. 2] == "\U00010000");
    assert(encode(buf, '\U0010FFFF') == 2 && buf[0 .. 2] == "\U0010FFFF");
    assertThrown!UTFException(encode(buf, cast(dchar) 0xD800));

    encode!(Yes.useReplacementDchar)(buf, cast(dchar) 0x110000);
    auto slice = buf[];
    assert(slice.decodeFront == replacementDchar);
}

///
@safe unittest
{
    import std.exception : assertThrown;
    import std.typecons : Yes;

    dchar[1] buf;

    assert(encode(buf, '\u0000') == 1 && buf[0] == '\u0000');
    assert(encode(buf, '\uD7FF') == 1 && buf[0] == '\uD7FF');
    assert(encode(buf, '\uE000') == 1 && buf[0] == '\uE000');
    assert(encode(buf, '\U0010FFFF') == 1 && buf[0] == '\U0010FFFF');
    assertThrown!UTFException(encode(buf, cast(dchar) 0xD800));

    encode!(Yes.useReplacementDchar)(buf, cast(dchar) 0x110000);
    assert(buf[0] == replacementDchar);
}

@safe unittest
{
    import std.exception;
    assertCTFEable!(
    {
    char[4] buf;

    assert(encode(buf, '\u0000') == 1 && buf[0 .. 1] == "\u0000");
    assert(encode(buf, '\u007F') == 1 && buf[0 .. 1] == "\u007F");
    assert(encode(buf, '\u0080') == 2 && buf[0 .. 2] == "\u0080");
    assert(encode(buf, '\u07FF') == 2 && buf[0 .. 2] == "\u07FF");
    assert(encode(buf, '\u0800') == 3 && buf[0 .. 3] == "\u0800");
    assert(encode(buf, '\uD7FF') == 3 && buf[0 .. 3] == "\uD7FF");
    assert(encode(buf, '\uE000') == 3 && buf[0 .. 3] == "\uE000");
    assert(encode(buf, 0xFFFE) == 3 && buf[0 .. 3] == "\xEF\xBF\xBE");
    assert(encode(buf, 0xFFFF) == 3 && buf[0 .. 3] == "\xEF\xBF\xBF");
    assert(encode(buf, '\U00010000') == 4 && buf[0 .. 4] == "\U00010000");
    assert(encode(buf, '\U0010FFFF') == 4 && buf[0 .. 4] == "\U0010FFFF");

    assertThrown!UTFException(encode(buf, cast(dchar) 0xD800));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDBFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDC00));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDFFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0x110000));

    assert(encode!(Yes.useReplacementDchar)(buf, cast(dchar) 0x110000) == buf.stride);
    enum replacementDcharString = "\uFFFD";
    assert(buf[0 .. replacementDcharString.length] == replacementDcharString);
    });
}


/// Ditto
size_t encode(UseReplacementDchar useReplacementDchar = No.useReplacementDchar)(
    out wchar[2] buf, dchar c) @safe pure
{
    if (c <= 0xFFFF)
    {
        if (0xD800 <= c && c <= 0xDFFF)
            c = _utfException!useReplacementDchar("Encoding an isolated surrogate code point in UTF-16", c);

        assert(isValidDchar(c));
    L1:
        buf[0] = cast(wchar) c;
        return 1;
    }
    if (c <= 0x10FFFF)
    {
        assert(isValidDchar(c));
        buf[0] = cast(wchar)((((c - 0x10000) >> 10) & 0x3FF) + 0xD800);
        buf[1] = cast(wchar)(((c - 0x10000) & 0x3FF) + 0xDC00);
        return 2;
    }

    c = _utfException!useReplacementDchar("Encoding an invalid code point in UTF-16", c);
    goto L1;
}

@safe unittest
{
    import std.exception;
    assertCTFEable!(
    {
    wchar[2] buf;

    assert(encode(buf, '\u0000') == 1 && buf[0 .. 1] == "\u0000");
    assert(encode(buf, '\uD7FF') == 1 && buf[0 .. 1] == "\uD7FF");
    assert(encode(buf, '\uE000') == 1 && buf[0 .. 1] == "\uE000");
    assert(encode(buf, 0xFFFE) == 1 && buf[0] == 0xFFFE);
    assert(encode(buf, 0xFFFF) == 1 && buf[0] == 0xFFFF);
    assert(encode(buf, '\U00010000') == 2 && buf[0 .. 2] == "\U00010000");
    assert(encode(buf, '\U0010FFFF') == 2 && buf[0 .. 2] == "\U0010FFFF");

    assertThrown!UTFException(encode(buf, cast(dchar) 0xD800));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDBFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDC00));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDFFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0x110000));

    assert(encode!(Yes.useReplacementDchar)(buf, cast(dchar) 0x110000) == buf.stride);
    assert(buf.front == replacementDchar);
    });
}


/// Ditto
size_t encode(UseReplacementDchar useReplacementDchar = No.useReplacementDchar)(
    out dchar[1] buf, dchar c) @safe pure
{
    if ((0xD800 <= c && c <= 0xDFFF) || 0x10FFFF < c)
        c = _utfException!useReplacementDchar("Encoding an invalid code point in UTF-32", c);
    else
        assert(isValidDchar(c));
    buf[0] = c;
    return 1;
}

@safe unittest
{
    import std.exception;
    assertCTFEable!(
    {
    dchar[1] buf;

    encode(buf, '\u0000'); assert(buf[0] == '\u0000');
    encode(buf, '\uD7FF'); assert(buf[0] == '\uD7FF');
    encode(buf, '\uE000'); assert(buf[0] == '\uE000');
    encode(buf, 0xFFFE ); assert(buf[0] == 0xFFFE);
    encode(buf, 0xFFFF ); assert(buf[0] == 0xFFFF);
    encode(buf, '\U0010FFFF'); assert(buf[0] == '\U0010FFFF');

    assertThrown!UTFException(encode(buf, cast(dchar) 0xD800));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDBFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDC00));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDFFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0x110000));

    assert(encode!(Yes.useReplacementDchar)(buf, cast(dchar) 0x110000) == buf.stride);
    assert(buf.front == replacementDchar);
    });
}


/++
    Encodes `c` in `str`'s encoding and appends it to `str`.

    Throws:
        `UTFException` if `c` is not a valid UTF code point.
  +/
void encode(UseReplacementDchar useReplacementDchar = No.useReplacementDchar)(
    ref scope char[] str, dchar c) @safe pure
{
    if (c <= 0x7F)
    {
        assert(isValidDchar(c));
        str ~= cast(char) c;
    }
    else
    {
        char[4] buf;
        uint L;

        if (c <= 0x7FF)
        {
            assert(isValidDchar(c));
            buf[0] = cast(char)(0xC0 | (c >> 6));
            buf[1] = cast(char)(0x80 | (c & 0x3F));
            L = 2;
        }
        else if (c <= 0xFFFF)
        {
            if (0xD800 <= c && c <= 0xDFFF)
                c = _utfException!useReplacementDchar("Encoding a surrogate code point in UTF-8", c);

            assert(isValidDchar(c));
        L3:
            buf[0] = cast(char)(0xE0 | (c >> 12));
            buf[1] = cast(char)(0x80 | ((c >> 6) & 0x3F));
            buf[2] = cast(char)(0x80 | (c & 0x3F));
            L = 3;
        }
        else if (c <= 0x10FFFF)
        {
            assert(isValidDchar(c));
            buf[0] = cast(char)(0xF0 | (c >> 18));
            buf[1] = cast(char)(0x80 | ((c >> 12) & 0x3F));
            buf[2] = cast(char)(0x80 | ((c >> 6) & 0x3F));
            buf[3] = cast(char)(0x80 | (c & 0x3F));
            L = 4;
        }
        else
        {
            assert(!isValidDchar(c));
            c = _utfException!useReplacementDchar("Encoding an invalid code point in UTF-8", c);
            goto L3;
        }
        str ~= buf[0 .. L];
    }
}

///
@safe unittest
{
    char[] s = "abcd".dup;
    dchar d1 = 'a';
    dchar d2 = 'ø';

    encode(s, d1);
    assert(s.length == 5);
    assert(s == "abcda");
    encode(s, d2);
    assert(s.length == 7);
    assert(s == "abcdaø");
}

@safe unittest
{
    import std.exception;

    assertCTFEable!(
    {
    char[] s = "abcd".dup;
    encode(s, cast(dchar)'a');
    assert(s.length == 5);
    assert(s == "abcda");

    encode(s, cast(dchar)'\u00A9');
    assert(s.length == 7);
    assert(s == "abcda\xC2\xA9");
    //assert(s == "abcda\u00A9");   // BUG: fix compiler

    encode(s, cast(dchar)'\u2260');
    assert(s.length == 10);
    assert(s == "abcda\xC2\xA9\xE2\x89\xA0");
    });
}

@safe unittest
{
    import std.exception;
    assertCTFEable!(
    {
    char[] buf;

    encode(buf, '\u0000'); assert(buf[0 .. $] == "\u0000");
    encode(buf, '\u007F'); assert(buf[1 .. $] == "\u007F");
    encode(buf, '\u0080'); assert(buf[2 .. $] == "\u0080");
    encode(buf, '\u07FF'); assert(buf[4 .. $] == "\u07FF");
    encode(buf, '\u0800'); assert(buf[6 .. $] == "\u0800");
    encode(buf, '\uD7FF'); assert(buf[9 .. $] == "\uD7FF");
    encode(buf, '\uE000'); assert(buf[12 .. $] == "\uE000");
    encode(buf, 0xFFFE); assert(buf[15 .. $] == "\xEF\xBF\xBE");
    encode(buf, 0xFFFF); assert(buf[18 .. $] == "\xEF\xBF\xBF");
    encode(buf, '\U00010000'); assert(buf[21 .. $] == "\U00010000");
    encode(buf, '\U0010FFFF'); assert(buf[25 .. $] == "\U0010FFFF");

    assertThrown!UTFException(encode(buf, cast(dchar) 0xD800));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDBFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDC00));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDFFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0x110000));

    enum replacementDcharString = "\uFFFD";
    enum rdcslen = replacementDcharString.length;
    assert(buf[$ - rdcslen .. $] != replacementDcharString);
    encode!(Yes.useReplacementDchar)(buf, cast(dchar) 0x110000);
    assert(buf[$ - rdcslen .. $] == replacementDcharString);
    });
}

/// ditto
void encode(UseReplacementDchar useReplacementDchar = No.useReplacementDchar)(
    ref scope wchar[] str, dchar c) @safe pure
{
    if (c <= 0xFFFF)
    {
        if (0xD800 <= c && c <= 0xDFFF)
            c = _utfException!useReplacementDchar("Encoding an isolated surrogate code point in UTF-16", c);

        assert(isValidDchar(c));
    L1:
        str ~= cast(wchar) c;
    }
    else if (c <= 0x10FFFF)
    {
        wchar[2] buf;

        assert(isValidDchar(c));
        buf[0] = cast(wchar)((((c - 0x10000) >> 10) & 0x3FF) + 0xD800);
        buf[1] = cast(wchar)(((c - 0x10000) & 0x3FF) + 0xDC00);
        str ~= buf;
    }
    else
    {
        assert(!isValidDchar(c));
        c = _utfException!useReplacementDchar("Encoding an invalid code point in UTF-16", c);
        goto L1;
    }
}

@safe unittest
{
    import std.exception;
    assertCTFEable!(
    {
    wchar[] buf;

    encode(buf, '\u0000'); assert(buf[0] == '\u0000');
    encode(buf, '\uD7FF'); assert(buf[1] == '\uD7FF');
    encode(buf, '\uE000'); assert(buf[2] == '\uE000');
    encode(buf, 0xFFFE); assert(buf[3] == 0xFFFE);
    encode(buf, 0xFFFF); assert(buf[4] == 0xFFFF);
    encode(buf, '\U00010000'); assert(buf[5 .. $] == "\U00010000");
    encode(buf, '\U0010FFFF'); assert(buf[7 .. $] == "\U0010FFFF");

    assertThrown!UTFException(encode(buf, cast(dchar) 0xD800));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDBFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDC00));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDFFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0x110000));

    assert(buf.back != replacementDchar);
    encode!(Yes.useReplacementDchar)(buf, cast(dchar) 0x110000);
    assert(buf.back == replacementDchar);
    });
}

/// ditto
void encode(UseReplacementDchar useReplacementDchar = No.useReplacementDchar)(
    ref scope dchar[] str, dchar c) @safe pure
{
    if ((0xD800 <= c && c <= 0xDFFF) || 0x10FFFF < c)
        c = _utfException!useReplacementDchar("Encoding an invalid code point in UTF-32", c);
    else
        assert(isValidDchar(c));
    str ~= c;
}

@safe unittest
{
    import std.exception;
    assertCTFEable!(
    {
    dchar[] buf;

    encode(buf, '\u0000'); assert(buf[0] == '\u0000');
    encode(buf, '\uD7FF'); assert(buf[1] == '\uD7FF');
    encode(buf, '\uE000'); assert(buf[2] == '\uE000');
    encode(buf, 0xFFFE ); assert(buf[3] == 0xFFFE);
    encode(buf, 0xFFFF ); assert(buf[4] == 0xFFFF);
    encode(buf, '\U0010FFFF'); assert(buf[5] == '\U0010FFFF');

    assertThrown!UTFException(encode(buf, cast(dchar) 0xD800));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDBFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDC00));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDFFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0x110000));

    assert(buf.back != replacementDchar);
    encode!(Yes.useReplacementDchar)(buf, cast(dchar) 0x110000);
    assert(buf.back == replacementDchar);
    });
}


/++
    Returns the number of code units that are required to encode the code point
    `c` when `C` is the character type used to encode it.
  +/
ubyte codeLength(C)(dchar c) @safe pure nothrow @nogc
if (isSomeChar!C)
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

///
@safe pure nothrow @nogc unittest
{
    assert(codeLength!char('a') == 1);
    assert(codeLength!wchar('a') == 1);
    assert(codeLength!dchar('a') == 1);

    assert(codeLength!char('\U0010FFFF') == 4);
    assert(codeLength!wchar('\U0010FFFF') == 2);
    assert(codeLength!dchar('\U0010FFFF') == 1);
}


/++
    Returns the number of code units that are required to encode `str`
    in a string whose character type is `C`. This is particularly useful
    when slicing one string with the length of another and the two string
    types use different character types.

    Params:
        C = the character type to get the encoding length for
        input = the $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
        to calculate the encoding length from
    Returns:
        The number of code units in `input` when encoded to `C`
  +/
size_t codeLength(C, InputRange)(InputRange input)
if (isSomeFiniteCharInputRange!InputRange)
{
    alias EncType = Unqual!(ElementEncodingType!InputRange);
    static if (isSomeString!InputRange && is(EncType == C) && is(typeof(input.length)))
        return input.length;
    else
    {
        size_t total = 0;

        foreach (c; input.byDchar)
            total += codeLength!C(c);

        return total;
    }
}

///
@safe unittest
{
    assert(codeLength!char("hello world") ==
           "hello world".length);
    assert(codeLength!wchar("hello world") ==
           "hello world"w.length);
    assert(codeLength!dchar("hello world") ==
           "hello world"d.length);

    assert(codeLength!char(`プログラミング`) ==
           `プログラミング`.length);
    assert(codeLength!wchar(`プログラミング`) ==
           `プログラミング`w.length);
    assert(codeLength!dchar(`プログラミング`) ==
           `プログラミング`d.length);

    string haystack = `Être sans la verité, ça, ce ne serait pas bien.`;
    wstring needle = `Être sans la verité`;
    assert(haystack[codeLength!char(needle) .. $] ==
           `, ça, ce ne serait pas bien.`);
}

@safe unittest
{
    import std.algorithm.iteration : filter;
    import std.conv : to;
    import std.exception;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!( char[], const  char[],  string,
                          wchar[], const wchar[], wstring,
                          dchar[], const dchar[], dstring))
    {
        foreach (C; AliasSeq!(char, wchar, dchar))
        {
            assert(codeLength!C(to!S("Walter Bright")) == to!(C[])("Walter Bright").length);
            assert(codeLength!C(to!S(`言語`)) == to!(C[])(`言語`).length);
            assert(codeLength!C(to!S(`ウェブサイト@La_Verité.com`)) ==
                   to!(C[])(`ウェブサイト@La_Verité.com`).length);
            assert(codeLength!C(to!S(`ウェブサイト@La_Verité.com`).filter!(x => true)()) ==
                   to!(C[])(`ウェブサイト@La_Verité.com`).length);
        }
    }
    });
}

/+
Internal helper function:

Returns true if it is safe to search for the Codepoint `c` inside
code units, without decoding.

This is a runtime check that is used an optimization in various functions,
particularly, in `std.string`.
  +/
package bool canSearchInCodeUnits(C)(dchar c)
if (isSomeChar!C)
{
    static if (C.sizeof == 1)
         return c <= 0x7F;
    else static if (C.sizeof == 2)
        return c <= 0xD7FF || (0xE000 <= c && c <= 0xFFFF);
    else static if (C.sizeof == 4)
        return true;
    else
        static assert(0);
}
@safe unittest
{
    assert( canSearchInCodeUnits! char('a'));
    assert( canSearchInCodeUnits!wchar('a'));
    assert( canSearchInCodeUnits!dchar('a'));
    assert(!canSearchInCodeUnits! char('ö')); //Important test: ö <= 0xFF
    assert(!canSearchInCodeUnits! char(cast(char)'ö')); //Important test: ö <= 0xFF
    assert( canSearchInCodeUnits!wchar('ö'));
    assert( canSearchInCodeUnits!dchar('ö'));
    assert(!canSearchInCodeUnits! char('日'));
    assert( canSearchInCodeUnits!wchar('日'));
    assert( canSearchInCodeUnits!dchar('日'));
    assert(!canSearchInCodeUnits!wchar(cast(wchar) 0xDA00));
    assert( canSearchInCodeUnits!dchar(cast(dchar) 0xDA00));
    assert(!canSearchInCodeUnits! char('\U00010001'));
    assert(!canSearchInCodeUnits!wchar('\U00010001'));
    assert( canSearchInCodeUnits!dchar('\U00010001'));
}

/* =================== Validation ======================= */

/++
    Checks to see if `str` is well-formed unicode or not.

    Throws:
        `UTFException` if `str` is not well-formed.
  +/
void validate(S)(in S str) @safe pure
if (isSomeString!S)
{
    immutable len = str.length;
    for (size_t i = 0; i < len; )
    {
        decode(str, i);
    }
}

///
@safe unittest
{
    import std.exception : assertThrown;
    char[] a = [167, 133, 175];
    assertThrown!UTFException(validate(a));
}

// https://issues.dlang.org/show_bug.cgi?id=12923
@safe unittest
{
    import std.exception;
    assertThrown((){
        char[3]a=[167, 133, 175];
        validate(a[]);
    }());
}

/**
 * Encodes the elements of `s` to UTF-8 and returns a newly allocated
 * string of the elements.
 *
 * Params:
 *     s = the string to encode
 * Returns:
 *     A UTF-8 string
 * See_Also:
 *     For a lazy, non-allocating version of these functions, see $(LREF byUTF).
 */
string toUTF8(S)(S s)
if (isSomeFiniteCharInputRange!S)
{
    return toUTFImpl!string(s);
}

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    // The ö is represented by two UTF-8 code units
    assert("Hellø"w.toUTF8.equal(['H', 'e', 'l', 'l', 0xC3, 0xB8]));

    // 𐐷 is four code units in UTF-8
    assert("𐐷"d.toUTF8.equal([0xF0, 0x90, 0x90, 0xB7]));
}

@system pure unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : ReferenceInputRange;

    alias RT = ReferenceInputRange!(ElementType!(string));
    auto r1 = new RT("Hellø");
    auto r2 = new RT("𐐷");

    assert(r1.toUTF8.equal(['H', 'e', 'l', 'l', 0xC3, 0xB8]));
    assert(r2.toUTF8.equal([0xF0, 0x90, 0x90, 0xB7]));
}

/**
 * Encodes the elements of `s` to UTF-16 and returns a newly GC allocated
 * `wstring` of the elements.
 *
 * Params:
 *     s = the range to encode
 * Returns:
 *     A UTF-16 string
 * See_Also:
 *     For a lazy, non-allocating version of these functions, see $(LREF byUTF).
 */
wstring toUTF16(S)(S s)
if (isSomeFiniteCharInputRange!S)
{
    return toUTFImpl!wstring(s);
}

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    // these graphemes are two code units in UTF-16 and one in UTF-32
    assert("𤭢"d.length == 1);
    assert("𐐷"d.length == 1);

    assert("𤭢"d.toUTF16.equal([0xD852, 0xDF62]));
    assert("𐐷"d.toUTF16.equal([0xD801, 0xDC37]));
}

@system pure unittest
{
    import std.algorithm.comparison : equal;
    import std.internal.test.dummyrange : ReferenceInputRange;

    alias RT = ReferenceInputRange!(ElementType!(string));
    auto r1 = new RT("𤭢");
    auto r2 = new RT("𐐷");

    assert(r1.toUTF16.equal([0xD852, 0xDF62]));
    assert(r2.toUTF16.equal([0xD801, 0xDC37]));
}


/**
 * Encodes the elements of `s` to UTF-32 and returns a newly GC allocated
 * `dstring` of the elements.
 *
 * Params:
 *     s = the range to encode
 * Returns:
 *     A UTF-32 string
 * See_Also:
 *     For a lazy, non-allocating version of these functions, see $(LREF byUTF).
 */
dstring toUTF32(S)(scope S s)
if (isSomeFiniteCharInputRange!S)
{
    return toUTFImpl!dstring(s);
}

///
@safe pure unittest
{
    import std.algorithm.comparison : equal;

    // these graphemes are two code units in UTF-16 and one in UTF-32
    assert("𤭢"w.length == 2);
    assert("𐐷"w.length == 2);

    assert("𤭢"w.toUTF32.equal([0x00024B62]));
    assert("𐐷"w.toUTF32.equal([0x00010437]));
}

private T toUTFImpl(T, S)(scope S s)
{
    static if (is(S : T))
    {
        return s.idup;
    }
    else
    {
        import std.array : appender;
        auto app = appender!T();

        static if (is(S == C[], C) || hasLength!S)
            app.reserve(s.length);

        foreach (c; s.byUTF!(Unqual!(ElementEncodingType!T)))
            app.put(c);

        return app.data;
    }
}

/* =================== toUTFz ======================= */

/++
    Returns a C-style zero-terminated string equivalent to `str`. `str`
    must not contain embedded `'\0'`'s as any C function will treat the first
    `'\0'` that it sees as the end of the string. If `str.empty` is
    `true`, then a string containing only `'\0'` is returned.

    `toUTFz` accepts any type of string and is templated on the type of
    character pointer that you wish to convert to. It will avoid allocating a
    new string if it can, but there's a decent chance that it will end up having
    to allocate a new string - particularly when dealing with character types
    other than `char`.

    $(RED Warning 1:) If the result of `toUTFz` equals `str.ptr`, then if
    anything alters the character one past the end of `str` (which is the
    `'\0'` character terminating the string), then the string won't be
    zero-terminated anymore. The most likely scenarios for that are if you
    append to `str` and no reallocation takes place or when `str` is a
    slice of a larger array, and you alter the character in the larger array
    which is one character past the end of `str`. Another case where it could
    occur would be if you had a mutable character array immediately after
    `str` in memory (for example, if they're member variables in a
    user-defined type with one declared right after the other) and that
    character array happened to start with `'\0'`. Such scenarios will never
    occur if you immediately use the zero-terminated string after calling
    `toUTFz` and the C function using it doesn't keep a reference to it.
    Also, they are unlikely to occur even if you save the zero-terminated string
    (the cases above would be among the few examples of where it could happen).
    However, if you save the zero-terminate string and want to be absolutely
    certain that the string stays zero-terminated, then simply append a
    `'\0'` to the string and use its `ptr` property rather than calling
    `toUTFz`.

    $(RED Warning 2:) When passing a character pointer to a C function, and the
    C function keeps it around for any reason, make sure that you keep a
    reference to it in your D code. Otherwise, it may go away during a garbage
    collection cycle and cause a nasty bug when the C code tries to use it.
  +/
template toUTFz(P)
if (is(P == C*, C) && isSomeChar!C)
{
    P toUTFz(S)(S str) @safe pure
    if (isSomeString!S)
    {
        return toUTFzImpl!(P, S)(str);
    }
}

///
@safe pure unittest
{
    auto p1 = toUTFz!(char*)("hello world");
    auto p2 = toUTFz!(const(char)*)("hello world");
    auto p3 = toUTFz!(immutable(char)*)("hello world");
    auto p4 = toUTFz!(char*)("hello world"d);
    auto p5 = toUTFz!(const(wchar)*)("hello world");
    auto p6 = toUTFz!(immutable(dchar)*)("hello world"w);
}

private P toUTFzImpl(P, S)(return scope S str) @safe pure
if (is(immutable typeof(*P.init) == typeof(str[0])))
//immutable(C)[] -> C*, const(C)*, or immutable(C)*
{
    if (str.empty)
    {
        typeof(*P.init)[] retval = ['\0'];

        auto trustedPtr() @trusted { return retval.ptr; }
        return trustedPtr();
    }

    alias C = Unqual!(ElementEncodingType!S);

    //If the P is mutable, then we have to make a copy.
    static if (is(Unqual!(typeof(*P.init)) == typeof(*P.init)))
    {
        return toUTFzImpl!(P, const(C)[])(cast(const(C)[])str);
    }
    else
    {
        if (!__ctfe)
        {
            auto trustedPtrAdd(S s) @trusted { return s.ptr + s.length; }
            immutable p = trustedPtrAdd(str);

            // Peek past end of str, if it's 0, no conversion necessary.
            // Note that the compiler will put a 0 past the end of static
            // strings, and the storage allocator will put a 0 past the end
            // of newly allocated char[]'s.
            // Is p dereferenceable? A simple test: if the p points to an
            // address multiple of 4, then conservatively assume the pointer
            // might be pointing to a new block of memory, which might be
            // unreadable. Otherwise, it's definitely pointing to valid
            // memory.
            if ((cast(size_t) p & 3) && *p == '\0')
                return &str[0];
        }

        return toUTFzImpl!(P, const(C)[])(cast(const(C)[])str);
    }
}

private P toUTFzImpl(P, S)(return scope S str) @safe pure
if (is(typeof(str[0]) C) && is(immutable typeof(*P.init) == immutable C) && !is(C == immutable))
//C[] or const(C)[] -> C*, const(C)*, or immutable(C)*
{
    alias InChar  = typeof(str[0]);
    alias OutChar = typeof(*P.init);

    //const(C)[] -> const(C)* or
    //C[] -> C* or const(C)*
    static if (( is(const(Unqual!InChar) == InChar) &&  is(const(Unqual!OutChar) == OutChar)) ||
               (!is(const(Unqual!InChar) == InChar) && !is(immutable(Unqual!OutChar) == OutChar)))
    {
        if (!__ctfe)
        {
            auto trustedPtrAdd(S s) @trusted { return s.ptr + s.length; }
            auto p = trustedPtrAdd(str);

            if ((cast(size_t) p & 3) && *p == '\0')
                return &str[0];
        }

        str ~= '\0';
        return &str[0];
    }
    //const(C)[] -> C* or immutable(C)* or
    //C[] -> immutable(C)*
    else
    {
        import std.array : uninitializedArray;
        auto copy = uninitializedArray!(Unqual!OutChar[])(str.length + 1);
        copy[0 .. $ - 1] = str[];
        copy[$ - 1] = '\0';

        auto trustedCast(typeof(copy) c) @trusted { return cast(P) c.ptr; }
        return trustedCast(copy);
    }
}

private P toUTFzImpl(P, S)(S str) @safe pure
if (!is(immutable typeof(*P.init) == immutable typeof(str[0])))
//C1[], const(C1)[], or immutable(C1)[] -> C2*, const(C2)*, or immutable(C2)*
{
    import std.array : appender;
    auto retval = appender!(typeof(*P.init)[])();

    foreach (dchar c; str)
        retval.put(c);
    retval.put('\0');

    return () @trusted { return cast(P) retval.data.ptr; } ();
}

@safe pure unittest
{
    import core.exception : AssertError;
    import std.algorithm;
    import std.conv : to;
    import std.exception;
    import std.string : format;

    assertCTFEable!(
    {
    foreach (S; AliasSeq!(string, wstring, dstring))
    {
        alias C = Unqual!(ElementEncodingType!S);

        auto s1 = to!S("hello\U00010143\u0100\U00010143");
        auto temp = new C[](s1.length + 1);
        temp[0 .. $ - 1] = s1[0 .. $];
        temp[$ - 1] = '\n';
        --temp.length;
        auto trustedAssumeUnique(T)(T t) @trusted { return assumeUnique(t); }
        auto s2 = trustedAssumeUnique(temp);
        assert(s1 == s2);

        void trustedCStringAssert(P, S)(S s) @trusted
        {
            auto p = toUTFz!P(s);
            assert(p[0 .. s.length] == s);
            assert(p[s.length] == '\0');
        }

        foreach (P; AliasSeq!(C*, const(C)*, immutable(C)*))
        {
            trustedCStringAssert!P(s1);
            trustedCStringAssert!P(s2);
        }
    }
    });

    static void test(P, S)(S s, size_t line = __LINE__) @trusted
    {
        static size_t zeroLen(C)(const(C)* ptr) @trusted
        {
            size_t len = 0;
            while (*ptr != '\0') { ++ptr; ++len; }
            return len;
        }

        auto p = toUTFz!P(s);
        immutable len = zeroLen(p);
        enforce(cmp(s, p[0 .. len]) == 0,
                new AssertError(format("Unit test failed: %s %s", P.stringof, S.stringof),
                                __FILE__, line));
    }

    assertCTFEable!(
    {
    foreach (P; AliasSeq!(wchar*, const(wchar)*, immutable(wchar)*,
                          dchar*, const(dchar)*, immutable(dchar)*))
    {
        test!P("hello\U00010143\u0100\U00010143");
    }
    foreach (P; AliasSeq!( char*, const( char)*, immutable( char)*,
                          dchar*, const(dchar)*, immutable(dchar)*))
    {
        test!P("hello\U00010143\u0100\U00010143"w);
    }
    foreach (P; AliasSeq!( char*, const( char)*, immutable( char)*,
                          wchar*, const(wchar)*, immutable(wchar)*))
    {
        test!P("hello\U00010143\u0100\U00010143"d);
    }
    foreach (S; AliasSeq!( char[], const( char)[],
                          wchar[], const(wchar)[],
                          dchar[], const(dchar)[]))
    {
        auto s = to!S("hello\U00010143\u0100\U00010143");

        foreach (P; AliasSeq!( char*, const( char)*, immutable( char)*,
                              wchar*, const(wchar)*, immutable(wchar)*,
                              dchar*, const(dchar)*, immutable(dchar)*))
        {
            test!P(s);
        }
    }
    });
}


/++
    `toUTF16z` is a convenience function for `toUTFz!(const(wchar)*)`.

    Encodes string `s` into UTF-16 and returns the encoded string.
    `toUTF16z` is suitable for calling the 'W' functions in the Win32 API
    that take an `LPCWSTR` argument.
  +/
const(wchar)* toUTF16z(C)(const(C)[] str) @safe pure
if (isSomeChar!C)
{
    return toUTFz!(const(wchar)*)(str);
}

///
@system unittest
{
    string str = "Hello, World!";
    const(wchar)* p = str.toUTF16z;
    assert(p[str.length] == '\0');
}

@safe pure unittest
{
    import std.conv : to;
    //toUTFz is already thoroughly tested, so this will just verify that
    //toUTF16z compiles properly for the various string types.
    foreach (S; AliasSeq!(string, wstring, dstring))
        assert(toUTF16z(to!S("hello world")) !is null);
}


/* ================================ tests ================================== */

@safe pure unittest
{
    import std.exception;

    assertCTFEable!(
    {
    assert(toUTF16("hello"c) == "hello");
    assert(toUTF32("hello"c) == "hello");
    assert(toUTF8 ("hello"w) == "hello");
    assert(toUTF32("hello"w) == "hello");
    assert(toUTF8 ("hello"d) == "hello");
    assert(toUTF16("hello"d) == "hello");

    assert(toUTF16("hel\u1234o"c) == "hel\u1234o");
    assert(toUTF32("hel\u1234o"c) == "hel\u1234o");
    assert(toUTF8 ("hel\u1234o"w) == "hel\u1234o");
    assert(toUTF32("hel\u1234o"w) == "hel\u1234o");
    assert(toUTF8 ("hel\u1234o"d) == "hel\u1234o");
    assert(toUTF16("hel\u1234o"d) == "hel\u1234o");

    assert(toUTF16("he\U0010AAAAllo"c) == "he\U0010AAAAllo");
    assert(toUTF32("he\U0010AAAAllo"c) == "he\U0010AAAAllo");
    assert(toUTF8 ("he\U0010AAAAllo"w) == "he\U0010AAAAllo");
    assert(toUTF32("he\U0010AAAAllo"w) == "he\U0010AAAAllo");
    assert(toUTF8 ("he\U0010AAAAllo"d) == "he\U0010AAAAllo");
    assert(toUTF16("he\U0010AAAAllo"d) == "he\U0010AAAAllo");
    });
}


/++
    Returns the total number of code points encoded in `str`.

    Supercedes: This function supercedes $(LREF toUCSindex).

    Standards: Unicode 5.0, ASCII, ISO-8859-1, WINDOWS-1252

    Throws:
        `UTFException` if `str` is not well-formed.
  +/
size_t count(C)(const(C)[] str) @safe pure nothrow @nogc
if (isSomeChar!C)
{
    return walkLength(str.byDchar);
}

///
@safe pure nothrow @nogc unittest
{
    assert(count("") == 0);
    assert(count("a") == 1);
    assert(count("abc") == 3);
    assert(count("\u20AC100") == 4);
}

@safe pure nothrow @nogc unittest
{
    import std.exception;
    assertCTFEable!(
    {
    assert(count("") == 0);
    assert(count("a") == 1);
    assert(count("abc") == 3);
    assert(count("\u20AC100") == 4);
    });
}


// Ranges of code units for testing.
version (StdUnittest)
{
private:
    struct InputCU(C)
    {
        import std.conv : to;
        @property bool empty() { return _str.empty; }
        @property C front() { return _str[0]; }
        void popFront() { _str = _str[1 .. $]; }

        this(inout(C)[] str)
        {
            _str = to!(C[])(str);
        }

        C[] _str;
    }

    struct BidirCU(C)
    {
        import std.conv : to;
        @property bool empty() { return _str.empty; }
        @property C front() { return _str[0]; }
        void popFront() { _str = _str[1 .. $]; }
        @property C back() { return _str[$ - 1]; }
        void popBack() { _str = _str[0 .. $ - 1]; }
        @property auto save() { return BidirCU(_str); }
        @property size_t length() { return _str.length; }

        this(inout(C)[] str)
        {
            _str = to!(C[])(str);
        }

        C[] _str;
    }

    struct RandomCU(C)
    {
        import std.conv : to;
        @property bool empty() { return _str.empty; }
        @property C front() { return _str[0]; }
        void popFront() { _str = _str[1 .. $]; }
        @property C back() { return _str[$ - 1]; }
        void popBack() { _str = _str[0 .. $ - 1]; }
        @property auto save() { return RandomCU(_str); }
        @property size_t length() { return _str.length; }
        C opIndex(size_t i) { return _str[i]; }
        auto opSlice(size_t i, size_t j) { return RandomCU(_str[i .. j]); }

        this(inout(C)[] str)
        {
            _str = to!(C[])(str);
        }

        C[] _str;
    }

    class RefBidirCU(C)
    {
        import std.conv : to;
        @property bool empty() { return _str.empty; }
        @property C front() { return _str[0]; }
        void popFront() { _str = _str[1 .. $]; }
        @property C back() { return _str[$ - 1]; }
        void popBack() { _str = _str[0 .. $ - 1]; }
        @property auto save() { return new RefBidirCU(_str); }
        @property size_t length() { return _str.length; }

        this(inout(C)[] str)
        {
            _str = to!(C[])(str);
        }

        C[] _str;
    }

    class RefRandomCU(C)
    {
        import std.conv : to;
        @property bool empty() { return _str.empty; }
        @property C front() { return _str[0]; }
        void popFront() { _str = _str[1 .. $]; }
        @property C back() { return _str[$ - 1]; }
        void popBack() { _str = _str[0 .. $ - 1]; }
        @property auto save() { return new RefRandomCU(_str); }
        @property size_t length() { return _str.length; }
        C opIndex(size_t i) { return _str[i]; }
        auto opSlice(size_t i, size_t j) { return new RefRandomCU(_str[i .. j]); }

        this(inout(C)[] str)
        {
            _str = to!(C[])(str);
        }

        C[] _str;
    }
}


/**
 * Inserted in place of invalid UTF sequences.
 *
 * References:
 *      $(LINK http://en.wikipedia.org/wiki/Replacement_character#Replacement_character)
 */
enum dchar replacementDchar = '\uFFFD';

/********************************************
 * Iterate a range of char, wchar, or dchars by code unit.
 *
 * The purpose is to bypass the special case decoding that
 * $(REF front, std,range,primitives) does to character arrays. As a result,
 * using ranges with `byCodeUnit` can be `nothrow` while
 * $(REF front, std,range,primitives) throws when it encounters invalid Unicode
 * sequences.
 *
 * A code unit is a building block of the UTF encodings. Generally, an
 * individual code unit does not represent what's perceived as a full
 * character (a.k.a. a grapheme cluster in Unicode terminology). Many characters
 * are encoded with multiple code units. For example, the UTF-8 code units for
 * `ø` are `0xC3 0xB8`. That means, an individual element of `byCodeUnit`
 * often does not form a character on its own. Attempting to treat it as
 * one while iterating over the resulting range will give nonsensical results.
 *
 * Params:
 *      r = an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
 *      of characters (including strings) or a type that implicitly converts to a string type.
 * Returns:
 *      If `r` is not an auto-decodable string (i.e. a narrow string or a
 *      user-defined type that implicitly converts to a string type), then `r`
 *      is returned.
 *
 *      Otherwise, `r` is converted to its corresponding string type (if it's
 *      not already a string) and wrapped in a random-access range where the
 *      element encoding type of the string (its code unit) is the element type
 *      of the range, and that range returned. The range has slicing.
 *
 *      If `r` is quirky enough to be a struct or class which is an input range
 *      of characters on its own (i.e. it has the input range API as member
 *      functions), $(I and) it's implicitly convertible to a string type, then
 *      `r` is returned, and no implicit conversion takes place.
 *
 *      If `r` is wrapped in a new range, then that range has a `source`
 *      property for returning the string that's currently contained within that
 *      range.
 *
 * See_Also:
 *      Refer to the $(MREF std, uni) docs for a reference on Unicode
 *      terminology.
 *
 *      For a range that iterates by grapheme cluster (written character) see
 *      $(REF byGrapheme, std,uni).
 */
auto byCodeUnit(R)(R r)
if ((isConvertibleToString!R && !isStaticArray!R) ||
    (isInputRange!R && isSomeChar!(ElementEncodingType!R)))
{
    import std.traits : StringTypeOf;
    static if (// This would be cleaner if we had a way to check whether a type
               // was a range without any implicit conversions.
               (isAutodecodableString!R && !__traits(hasMember, R, "empty") &&
                !__traits(hasMember, R, "front") && !__traits(hasMember, R, "popFront")))
    {
        static struct ByCodeUnitImpl
        {
        @safe pure nothrow @nogc:

            @property bool empty() const     { return source.length == 0; }
            @property auto ref front() inout { return source[0]; }
            void popFront()                  { source = source[1 .. $]; }

            @property auto save() { return ByCodeUnitImpl(source.save); }

            @property auto ref back() inout { return source[$ - 1]; }
            void popBack()                  { source = source[0 .. $-1]; }

            auto ref opIndex(size_t index) inout     { return source[index]; }
            auto opSlice(size_t lower, size_t upper) { return ByCodeUnitImpl(source[lower .. upper]); }

            @property size_t length() const { return source.length; }
            alias opDollar = length;

            StringTypeOf!R source;
        }

        static assert(isRandomAccessRange!ByCodeUnitImpl);

        return ByCodeUnitImpl(r);
    }
    else static if (!isInputRange!R ||
                    (is(R : const dchar[]) && !__traits(hasMember, R, "empty") &&
                    !__traits(hasMember, R, "front") && !__traits(hasMember, R, "popFront")))
    {
        return cast(StringTypeOf!R) r;
    }
    else
    {
        // byCodeUnit for ranges and dchar[] is a no-op
        return r;
    }
}

///
@safe unittest
{
    import std.range.primitives;
    import std.traits : isAutodecodableString;

    auto r = "Hello, World!".byCodeUnit();
    static assert(hasLength!(typeof(r)));
    static assert(hasSlicing!(typeof(r)));
    static assert(isRandomAccessRange!(typeof(r)));
    static assert(is(ElementType!(typeof(r)) == immutable char));

    // contrast with the range capabilities of standard strings (with or
    // without autodecoding enabled).
    auto s = "Hello, World!";
    static assert(isBidirectionalRange!(typeof(r)));
    static if (isAutodecodableString!(typeof(s)))
    {
        // with autodecoding enabled, strings are non-random-access ranges of
        // dchar.
        static assert(is(ElementType!(typeof(s)) == dchar));
        static assert(!isRandomAccessRange!(typeof(s)));
        static assert(!hasSlicing!(typeof(s)));
        static assert(!hasLength!(typeof(s)));
    }
    else
    {
        // without autodecoding, strings are normal arrays.
        static assert(is(ElementType!(typeof(s)) == immutable char));
        static assert(isRandomAccessRange!(typeof(s)));
        static assert(hasSlicing!(typeof(s)));
        static assert(hasLength!(typeof(s)));
    }
}

/// `byCodeUnit` does no Unicode decoding
@safe unittest
{
    string noel1 = "noe\u0308l"; // noël using e + combining diaeresis
    assert(noel1.byCodeUnit[2] != 'ë');
    assert(noel1.byCodeUnit[2] == 'e');

    string noel2 = "no\u00EBl"; // noël using a precomposed ë character
    // Because string is UTF-8, the code unit at index 2 is just
    // the first of a sequence that encodes 'ë'
    assert(noel2.byCodeUnit[2] != 'ë');
}

/// `byCodeUnit` exposes a `source` property when wrapping narrow strings.
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.range : popFrontN;
    import std.traits : isAutodecodableString;
    {
        auto range = byCodeUnit("hello world");
        range.popFrontN(3);
        assert(equal(range.save, "lo world"));
        static if (isAutodecodableString!string) // only enabled with autodecoding
        {
            string str = range.source;
            assert(str == "lo world");
        }
    }
    // source only exists if the range was wrapped
    {
        auto range = byCodeUnit("hello world"d);
        static assert(!__traits(compiles, range.source));
    }
}

@safe pure nothrow @nogc unittest
{
    import std.range;
    {
        enum testStr = "𐁄𐂌𐃯 hello ディラン";
        char[testStr.length] s;
        int i;
        foreach (c; testStr.byCodeUnit().byCodeUnit())
        {
            s[i++] = c;
        }
        assert(s == testStr);
    }
    {
        enum testStr = "𐁄𐂌𐃯 hello ディラン"w;
        wchar[testStr.length] s;
        int i;
        foreach (c; testStr.byCodeUnit().byCodeUnit())
        {
            s[i++] = c;
        }
        assert(s == testStr);
    }
    {
        enum testStr = "𐁄𐂌𐃯 hello ディラン"d;
        dchar[testStr.length] s;
        int i;
        foreach (c; testStr.byCodeUnit().byCodeUnit())
        {
            s[i++] = c;
        }
        assert(s == testStr);
    }
    {
        auto bcu = "hello".byCodeUnit();
        assert(bcu.length == 5);
        assert(bcu[3] == 'l');
        assert(bcu[2 .. 4][1] == 'l');
    }
    {
        char[5] orig = "hello";
        auto bcu = orig[].byCodeUnit();
        bcu.front = 'H';
        assert(bcu.front == 'H');
        bcu[1] = 'E';
        assert(bcu[1] == 'E');
    }
    {
        auto bcu = "hello".byCodeUnit().byCodeUnit();
        static assert(isForwardRange!(typeof(bcu)));
        static assert(is(typeof(bcu) == struct) == isAutodecodableString!string);
        auto s = bcu.save;
        bcu.popFront();
        assert(s.front == 'h');
    }
    {
        auto bcu = "hello".byCodeUnit();
        static assert(hasSlicing!(typeof(bcu)));
        static assert(isBidirectionalRange!(typeof(bcu)));
        static assert(is(typeof(bcu) == struct) == isAutodecodableString!string);
        static assert(is(typeof(bcu) == typeof(bcu.byCodeUnit())));
        auto ret = bcu.retro;
        assert(ret.front == 'o');
        ret.popFront();
        assert(ret.front == 'l');
    }
    {
        auto bcu = "κόσμε"w.byCodeUnit();
        static assert(hasSlicing!(typeof(bcu)));
        static assert(isBidirectionalRange!(typeof(bcu)));
        static assert(is(typeof(bcu) == struct) == isAutodecodableString!wstring);
        static assert(is(typeof(bcu) == typeof(bcu.byCodeUnit())));
        auto ret = bcu.retro;
        assert(ret.front == 'ε');
        ret.popFront();
        assert(ret.front == 'μ');
    }
    {
        static struct Stringish
        {
            string s;
            alias s this;
        }

        auto orig = Stringish("\U0010fff8 𐁊 foo 𐂓");
        auto bcu = orig.byCodeUnit();
        static assert(is(typeof(bcu) == struct));
        static assert(!is(typeof(bcu) == Stringish) == isAutodecodableString!Stringish);
        static assert(is(typeof(bcu) == typeof(bcu.byCodeUnit())));
        static assert(is(ElementType!(typeof(bcu)) == immutable char));
        assert(bcu.front == cast(char) 244);
    }
    {
        static struct WStringish
        {
            wstring s;
            alias s this;
        }

        auto orig = WStringish("\U0010fff8 𐁊 foo 𐂓"w);
        auto bcu = orig.byCodeUnit();
        static assert(is(typeof(bcu) == struct));
        static assert(!is(typeof(bcu) == WStringish) == isAutodecodableString!WStringish);
        static assert(is(typeof(bcu) == typeof(bcu.byCodeUnit())));
        static assert(is(ElementType!(typeof(bcu)) == immutable wchar));
        assert(bcu.front == cast(wchar) 56319);
    }
    {
        static struct DStringish
        {
            dstring s;
            alias s this;
        }

        auto orig = DStringish("\U0010fff8 𐁊 foo 𐂓"d);
        auto bcu = orig.byCodeUnit();
        static assert(is(typeof(bcu) == dstring));
        static assert(is(typeof(bcu) == typeof(bcu.byCodeUnit())));
        static assert(is(ElementType!(typeof(bcu)) == immutable dchar));
        assert(bcu.front == cast(dchar) 1114104);
    }
    {
        static struct FuncStringish
        {
            string str;
            string s() pure nothrow @nogc { return str; }
            alias s this;
        }

        auto orig = FuncStringish("\U0010fff8 𐁊 foo 𐂓");
        auto bcu = orig.byCodeUnit();
        static if (isAutodecodableString!FuncStringish)
            static assert(is(typeof(bcu) == struct));
        else
            static assert(is(typeof(bcu) == string));
        static assert(!is(typeof(bcu) == FuncStringish));
        static assert(is(typeof(bcu) == typeof(bcu.byCodeUnit())));
        static assert(is(ElementType!(typeof(bcu)) == immutable char));
        assert(bcu.front == cast(char) 244);
    }
    {
        static struct Range
        {
            string data;
            bool empty() pure nothrow @nogc { return data.empty; }
            char front() pure nothrow @nogc { return data[0]; }
            void popFront() pure nothrow @nogc { data = data[1 .. $]; }
        }

        auto orig = Range("\U0010fff8 𐁊 foo 𐂓");
        auto bcu = orig.byCodeUnit();
        static assert(is(typeof(bcu) == Range));
        static assert(is(typeof(bcu) == typeof(bcu.byCodeUnit())));
        static assert(is(ElementType!(typeof(bcu)) == char));
        assert(bcu.front == cast(char) 244);
    }
    {
        static struct WRange
        {
            wstring data;
            bool empty() pure nothrow @nogc { return data.empty; }
            wchar front() pure nothrow @nogc { return data[0]; }
            void popFront() pure nothrow @nogc { data = data[1 .. $]; }
        }

        auto orig = WRange("\U0010fff8 𐁊 foo 𐂓"w);
        auto bcu = orig.byCodeUnit();
        static assert(is(typeof(bcu) == WRange));
        static assert(is(typeof(bcu) == typeof(bcu.byCodeUnit())));
        static assert(is(ElementType!(typeof(bcu)) == wchar));
        assert(bcu.front == 56319);
    }
    {
        static struct DRange
        {
            dstring data;
            bool empty() pure nothrow @nogc { return data.empty; }
            dchar front() pure nothrow @nogc { return data[0]; }
            void popFront() pure nothrow @nogc { data = data[1 .. $]; }
        }

        auto orig = DRange("\U0010fff8 𐁊 foo 𐂓"d);
        auto bcu = orig.byCodeUnit();
        static assert(is(typeof(bcu) == DRange));
        static assert(is(typeof(bcu) == typeof(bcu.byCodeUnit())));
        static assert(is(ElementType!(typeof(bcu)) == dchar));
        assert(bcu.front == 1114104);
    }
    {
        static struct RangeAndStringish
        {
            bool empty() pure nothrow @nogc { return data.empty; }
            char front() pure nothrow @nogc { return data[0]; }
            void popFront() pure nothrow @nogc { data = data[1 .. $]; }

            string data;
            string s;
            alias s this;
        }

        auto orig = RangeAndStringish("test.d", "other");
        auto bcu = orig.byCodeUnit();
        static assert(is(typeof(bcu) == RangeAndStringish));
        static assert(is(typeof(bcu) == typeof(bcu.byCodeUnit())));
        static assert(is(ElementType!(typeof(bcu)) == char));
        assert(bcu.front == 't');
    }
    {
        static struct WRangeAndStringish
        {
            bool empty() pure nothrow @nogc { return data.empty; }
            wchar front() pure nothrow @nogc { return data[0]; }
            void popFront() pure nothrow @nogc { data = data[1 .. $]; }

            wstring data;
            wstring s;
            alias s this;
        }

        auto orig = WRangeAndStringish("test.d"w, "other"w);
        auto bcu = orig.byCodeUnit();
        static assert(is(typeof(bcu) == WRangeAndStringish));
        static assert(is(typeof(bcu) == typeof(bcu.byCodeUnit())));
        static assert(is(ElementType!(typeof(bcu)) == wchar));
        assert(bcu.front == 't');
    }
    {
        static struct DRangeAndStringish
        {
            bool empty() pure nothrow @nogc { return data.empty; }
            dchar front() pure nothrow @nogc { return data[0]; }
            void popFront() pure nothrow @nogc { data = data[1 .. $]; }

            dstring data;
            dstring s;
            alias s this;
        }

        auto orig = DRangeAndStringish("test.d"d, "other"d);
        auto bcu = orig.byCodeUnit();
        static assert(is(typeof(bcu) == DRangeAndStringish));
        static assert(is(typeof(bcu) == typeof(bcu.byCodeUnit())));
        static assert(is(ElementType!(typeof(bcu)) == dchar));
        assert(bcu.front == 't');
    }
    {
        enum Enum : string { a = "test.d" }

        auto orig = Enum.a;
        auto bcu = orig.byCodeUnit();
        static assert(!is(typeof(bcu) == Enum));
        static if (isAutodecodableString!Enum)
            static assert(is(typeof(bcu) == struct));
        else
            static assert(is(typeof(bcu) == string));
        static assert(is(ElementType!(typeof(bcu)) == immutable char));
        assert(bcu.front == 't');
    }
    {
        enum WEnum : wstring { a = "test.d"w }

        auto orig = WEnum.a;
        auto bcu = orig.byCodeUnit();
        static assert(!is(typeof(bcu) == WEnum));
        static if (isAutodecodableString!WEnum)
            static assert(is(typeof(bcu) == struct));
        else
            static assert(is(typeof(bcu) == wstring));
        static assert(is(ElementType!(typeof(bcu)) == immutable wchar));
        assert(bcu.front == 't');
    }
    {
        enum DEnum : dstring { a = "test.d"d }

        auto orig = DEnum.a;
        auto bcu = orig.byCodeUnit();
        static assert(is(typeof(bcu) == dstring));
        static assert(is(ElementType!(typeof(bcu)) == immutable dchar));
        assert(bcu.front == 't');
    }

    static if (autodecodeStrings)
    {
        static assert(!is(typeof(byCodeUnit("hello")) == string));
        static assert(!is(typeof(byCodeUnit("hello"w)) == wstring));
    }
    else
    {
        static assert(is(typeof(byCodeUnit("hello")) == string));
        static assert(is(typeof(byCodeUnit("hello"w)) == wstring));
    }
    static assert(is(typeof(byCodeUnit("hello"d)) == dstring));

    static assert(!__traits(compiles, byCodeUnit((char[5]).init)));
    static assert(!__traits(compiles, byCodeUnit((wchar[5]).init)));
    static assert(!__traits(compiles, byCodeUnit((dchar[5]).init)));

    enum SEnum : char[5] { a = "hello" }
    enum WSEnum : wchar[5] { a = "hello"w }
    enum DSEnum : dchar[5] { a = "hello"d }

    static assert(!__traits(compiles, byCodeUnit(SEnum.a)));
    static assert(!__traits(compiles, byCodeUnit(WSEnum.a)));
    static assert(!__traits(compiles, byCodeUnit(DSEnum.a)));
}

/****************************
 * Iterate an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
 * of characters by char, wchar, or dchar.
 * These aliases simply forward to $(LREF byUTF) with the
 * corresponding C argument.
 *
 * Params:
 *      r = input range of characters, or array of characters
 */
alias byChar = byUTF!char;

/// Ditto
alias byWchar = byUTF!wchar;

/// Ditto
alias byDchar = byUTF!dchar;

@safe pure nothrow @nogc unittest
{
  {
    char[5] s;
    int i;
    foreach (c; "hello".byChar.byChar())
    {
        //writefln("[%d] '%c'", i, c);
        s[i++] = c;
    }
    assert(s == "hello");
  }
  {
    char[5+2+3+4+3+3] s;
    int i;
    dchar[10] a;
    a[0 .. 8] = "hello\u07FF\uD7FF\U0010FFFF"d;
    a[8] = 0xD800;   // invalid
    a[9] = cast(dchar) 0x110000; // invalid
    foreach (c; a[].byChar())
    {
        //writefln("[%d] '%c'", i, c);
        s[i++] = c;
    }
    assert(s == "hello\u07FF\uD7FF\U0010FFFF\uFFFD\uFFFD");
  }
  {
    auto r = "hello"w.byChar();
    r.popFront();
    r.popFront();
    assert(r.front == 'l');
  }
  {
    auto r = "hello"d.byChar();
    r.popFront();
    r.popFront();
    assert(r.front == 'l');
  }
  {
    auto r = "hello"d.byChar();
    assert(isForwardRange!(typeof(r)));
    auto s = r.save;
    r.popFront();
    assert(s.front == 'h');
  }
}

@safe pure nothrow @nogc unittest
{
  {
    wchar[11] s;
    int i;
    dchar[10] a;
    a[0 .. 8] = "hello\u07FF\uD7FF\U0010FFFF"d;
    a[8] = 0xD800;   // invalid
    a[9] = cast(dchar) 0x110000; // invalid
    foreach (c; a[].byWchar())
    {
        //writefln("[%d] '%c' x%x", i, c, c);
        s[i++] = c;
    }
    foreach (j, wchar c; "hello\u07FF\uD7FF\U0010FFFF\uFFFD\uFFFD"w)
    {
        //writefln("[%d] '%c' x%x", j, c, c);
    }
    assert(s == "hello\u07FF\uD7FF\U0010FFFF\uFFFD\uFFFD"w);
  }

  {
    auto r = "hello".byWchar();
    r.popFront();
    r.popFront();
    assert(r.front == 'l');
  }
  {
    auto r = "hello"d.byWchar();
    r.popFront();
    r.popFront();
    assert(r.front == 'l');
  }
  {
    auto r = "hello"d.byWchar();
    assert(isForwardRange!(typeof(r)));
    auto s = r.save;
    r.popFront();
    assert(s.front == 'h');
  }
}

@safe pure nothrow @nogc unittest
{
  {
    dchar[9] s;
    int i;
    string a = "hello\u07FF\uD7FF\U00010000\U0010FFFF"; // 1,2,3,4 byte sequences
    foreach (c; a.byDchar())
    {
        s[i++] = c;
    }
    assert(s == "hello\u07FF\uD7FF\U00010000\U0010FFFF"d);
  }
  {
    foreach (s; invalidUTFstrings!char())
    {
        auto r = s.byDchar();
        assert(!r.empty);
        assert(r.front == r.front);
        dchar c = r.front;
        assert(c == replacementDchar);
    }
  }
  {
    auto r = "hello".byDchar();
    r.popFront();
    r.popFront();
    assert(r.front == 'l');
  }

  {
    dchar[8] s;
    int i;
    wstring a = "hello\u07FF\uD7FF\U0010FFFF"w;
    foreach (c; a.byDchar())
    {
        //writefln("[%d] '%c' x%x", i, c, c);
        s[i++] = c;
    }
    assert(s == "hello\u07FF\uD7FF\U0010FFFF"d);
  }
  {
    foreach (s; invalidUTFstrings!wchar())
    {
        auto r = s.byDchar();
        assert(!r.empty);
        assert(r.front == r.front);
        dchar c = r.front;
        assert(c == replacementDchar);
    }
  }
  {
    wchar[2] ws;
    ws[0] = 0xD800;
    ws[1] = 0xDD00;             // correct surrogate pair
    auto r = ws[].byDchar();
    assert(!r.empty);
    assert(r.front == r.front);
    dchar c = r.front;
    assert(c == '\U00010100');
  }
  {
    auto r = "hello"w.byDchar();
    r.popFront();
    r.popFront();
    assert(r.front == 'l');
  }

  {
    dchar[5] s;
    int i;
    dstring a = "hello"d;
    foreach (c; a.byDchar.byDchar())
    {
        //writefln("[%d] '%c' x%x", i, c, c);
        s[i++] = c;
    }
    assert(s == "hello"d);
  }
  {
    auto r = "hello".byDchar();
    assert(isForwardRange!(typeof(r)));
    auto s = r.save;
    r.popFront();
    assert(s.front == 'h');
  }
  {
    auto r = "hello"w.byDchar();
    assert(isForwardRange!(typeof(r)));
    auto s = r.save;
    r.popFront();
    assert(s.front == 'h');
  }
}

// test pure, @safe, nothrow, @nogc correctness of byChar/byWchar/byDchar,
// which needs to support ranges with and without those attributes

pure @safe nothrow @nogc unittest
{
    dchar[5] s = "hello"d;
    foreach (c; s[].byChar())  { }
    foreach (c; s[].byWchar()) { }
    foreach (c; s[].byDchar()) { }
}

version (StdUnittest)
private int impureVariable;

@system unittest
{
    static struct ImpureThrowingSystemRange(Char)
    {
        @property bool empty() const { return true; }
        @property Char front() const { return Char.init; }
        void popFront()
        {
            impureVariable++;
            throw new Exception("only for testing nothrow");
        }
    }

    foreach (Char; AliasSeq!(char, wchar, dchar))
    {
        ImpureThrowingSystemRange!Char range;
        foreach (c; range.byChar())  { }
        foreach (c; range.byWchar()) { }
        foreach (c; range.byDchar()) { }
    }
}

/****************************
 * Iterate an $(REF_ALTTEXT input range, isInputRange, std,range,primitives)
 * of characters by char type `C` by encoding the elements of the range.
 *
 * UTF sequences that cannot be converted to the specified encoding are either
 * replaced by U+FFFD per "5.22 Best Practice for U+FFFD Substitution"
 * of the Unicode Standard 6.2 or result in a thrown UTFException.
 *  Hence byUTF is not symmetric.
 * This algorithm is lazy, and does not allocate memory.
 * `@nogc`, `pure`-ity, `nothrow`, and `@safe`-ty are inferred from the
 * `r` parameter.
 *
 * Params:
 *      C = `char`, `wchar`, or `dchar`
 *      useReplacementDchar = UseReplacementDchar.yes means replace invalid UTF with `replacementDchar`,
 *                            UseReplacementDchar.no means throw `UTFException` for invalid UTF
 *
 * Throws:
 *      `UTFException` if invalid UTF sequence and `useReplacementDchar` is set to `UseReplacementDchar.no`
 *
 * GC:
 *      Does not use GC if `useReplacementDchar` is set to `UseReplacementDchar.yes`
 *
 * Returns:
 *      A bidirectional range if `R` is a bidirectional range and not auto-decodable,
 *      as defined by $(REF isAutodecodableString, std, traits).
 *
 *      A forward range if `R` is a forward range and not auto-decodable.
 *
 *      Or, if `R` is a range and it is auto-decodable and
 *      `is(ElementEncodingType!typeof(r) == C)`, then the range is passed
 *      to $(LREF byCodeUnit).
 *
 *      Otherwise, an input range of characters.
 */
template byUTF(C, UseReplacementDchar useReplacementDchar = Yes.useReplacementDchar)
if (isSomeChar!C)
{
    static if (is(immutable C == immutable UC, UC) && !is(C == UC))
        alias byUTF = byUTF!UC;
    else:

    auto ref byUTF(R)(R r)
        if (isAutodecodableString!R && isInputRange!R && isSomeChar!(ElementEncodingType!R))
    {
        return byUTF(r.byCodeUnit());
    }

    auto ref byUTF(R)(R r)
        if (!isAutodecodableString!R && isInputRange!R && isSomeChar!(ElementEncodingType!R))
    {
        static if (is(immutable ElementEncodingType!R == immutable RC, RC) && is(RC == C))
        {
            return r.byCodeUnit();
        }
        else static if (is(C == dchar))
        {
            static struct Result
            {
                enum Empty = uint.max;  // range is empty or just constructed

                this(return scope R r)
                {
                    this.r = r;
                }

                this(return scope R r, uint buff)
                {
                    this.r = r;
                    this.buff = buff;
                }

                static if (isBidirectionalRange!R)
                {
                    this(return scope R r, uint frontBuff, uint backBuff)
                    {
                        this.r = r;
                        this.buff = frontBuff;
                        this.backBuff = backBuff;
                    }
                }

                @property bool empty()
                {
                    static if (isBidirectionalRange!R)
                        return buff == Empty && backBuff == Empty && r.empty;
                    else
                        return buff == Empty && r.empty;
                }

                @property dchar front() scope // 'scope' required by call to decodeFront() below
                {
                    if (buff == Empty)
                    {
                        auto c = r.front;

                        static if (is(RC == wchar))
                            enum firstMulti = 0xD800; // First high surrogate.
                        else
                            enum firstMulti = 0x80; // First non-ASCII.
                        if (c < firstMulti)
                        {
                            r.popFront;
                            buff = cast(dchar) c;
                        }
                        else
                        {
                            buff = () @trusted { return decodeFront!(useReplacementDchar)(r); }();
                        }
                    }
                    return cast(dchar) buff;
                }

                void popFront()
                {
                    if (buff == Empty)
                        front();
                    buff = Empty;
                }

                static if (isForwardRange!R)
                {
                    @property auto save()
                    {
                        static if (isBidirectionalRange!R)
                        {
                            return Result(r.save, buff, backBuff);
                        }
                        else
                        {
                            return Result(r.save, buff);
                        }
                    }
                }

                static if (isBidirectionalRange!R)
                {
                    @property dchar back() scope // 'scope' required by call to decodeBack() below
                    {
                        if (backBuff != Empty)
                            return cast(dchar) backBuff;

                        auto c = r.back;
                        static if (is(RC == wchar))
                            enum firstMulti = 0xD800; // First high surrogate.
                        else
                            enum firstMulti = 0x80; // First non-ASCII.
                        if (c < firstMulti)
                        {
                            r.popBack;
                            backBuff = cast(dchar) c;
                        }
                        else
                        {
                            backBuff = () @trusted { return decodeBack!useReplacementDchar(r); }();
                        }
                        return cast(dchar) backBuff;

                    }

                    void popBack()
                    {
                        if (backBuff == Empty)
                            back();
                        backBuff = Empty;
                    }
                }

            private:

                R r;
                uint buff = Empty;      // one character lookahead buffer
                static if (isBidirectionalRange!R)
                    uint backBuff = Empty;
            }

            return Result(r);
        }
        else
        {
            static struct Result
            {
                this(return scope R r)
                {
                    this.r = r;
                }

                this(return scope R r, ushort pos, ushort fill, C[4 / C.sizeof] buf)
                {
                    this.r = r;
                    this.pos = pos;
                    this.fill = fill;
                    this.buf = buf;
                }

                static if (isBidirectionalRange!R)
                {
                    this(return scope R r, ushort frontPos, ushort frontFill,
                         ushort backPos, ushort backFill, C[4 / C.sizeof] buf)
                    {
                        this.r = r;
                        this.pos = frontPos;
                        this.fill = frontFill;
                        this.backPos = backPos;
                        this.backFill = backFill;
                        this.buf = buf;
                    }
                }

                @property bool empty()
                {
                    static if (isBidirectionalRange!R)
                        return pos == fill && backPos == backFill && r.empty;
                    else
                        return pos == fill && r.empty;
                }

                @property auto front() scope // 'scope' required by call to decodeFront() below
                {
                    if (pos == fill)
                    {
                        pos = 0;
                        auto c = r.front;

                        static if (C.sizeof >= 2 && RC.sizeof >= 2)
                            enum firstMulti = 0xD800; // First high surrogate.
                        else
                            enum firstMulti = 0x80; // First non-ASCII.
                        if (c < firstMulti)
                        {
                            fill = 1;
                            r.popFront;
                            buf[pos] = cast(C) c;
                        }
                        else
                        {
                            static if (is(RC == dchar))
                            {
                                r.popFront;
                                dchar dc = c;
                            }
                            else
                                dchar dc = () @trusted { return decodeFront!(useReplacementDchar)(r); }();
                            fill = cast(ushort) encode!(useReplacementDchar)(buf, dc);
                        }
                    }
                    return buf[pos];
                }

                void popFront()
                {
                    if (pos == fill)
                        front;
                    ++pos;
                }

                static if (isForwardRange!R)
                {
                    @property auto save()
                    {
                        static if (isBidirectionalRange!R)
                        {
                            return Result(r.save, pos, fill, backPos, backFill, buf);
                        }
                        else
                        {
                            return Result(r.save, pos, fill, buf);
                        }
                    }
                }

                static if (isBidirectionalRange!R)
                {
                    @property auto back() scope // 'scope' required by call to decodeBack() below
                    {
                        if (backPos != backFill)
                            return buf[cast(ushort) (backFill - backPos - 1)];

                        backPos = 0;
                        auto c = r.back;
                        static if (C.sizeof >= 2 && RC.sizeof >= 2)
                            enum firstMulti = 0xD800; // First high surrogate.
                        else
                            enum firstMulti = 0x80; // First non-ASCII.
                        if (c < firstMulti)
                        {
                            backFill = 1;
                            r.popBack;
                            buf[cast(ushort) (backFill - backPos - 1)] = cast(C) c;
                        }
                        else
                        {
                            static if (is(RC == dchar))
                            {
                                r.popBack;
                                dchar dc = c;
                            }
                            else
                                dchar dc = () @trusted { return decodeBack!(useReplacementDchar)(r); }();
                            backFill = cast(ushort) encode!(useReplacementDchar)(buf, dc);
                        }
                        return buf[cast(ushort) (backFill - backPos - 1)];
                    }

                    void popBack()
                    {
                        if (backPos == backFill)
                            back;
                        ++backPos;
                    }
                }

            private:

                R r;
                ushort pos, fill;
                static if (isBidirectionalRange!R)
                    ushort backPos, backFill;
                C[4 / C.sizeof] buf = void;
            }

            return Result(r);
        }
    }
}

///
@safe pure nothrow unittest
{
    import std.algorithm.comparison : equal;

    // hellö as a range of `char`s, which are UTF-8
    assert("hell\u00F6".byUTF!char().equal(['h', 'e', 'l', 'l', 0xC3, 0xB6]));

    // `wchar`s are able to hold the ö in a single element (UTF-16 code unit)
    assert("hell\u00F6".byUTF!wchar().equal(['h', 'e', 'l', 'l', 'ö']));

    // 𐐷 is four code units in UTF-8, two in UTF-16, and one in UTF-32
    assert("𐐷".byUTF!char().equal([0xF0, 0x90, 0x90, 0xB7]));
    assert("𐐷".byUTF!wchar().equal([0xD801, 0xDC37]));
    assert("𐐷".byUTF!dchar().equal([0x00010437]));
}

///
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.exception : assertThrown;

    assert("hello\xF0betty".byChar.byUTF!(dchar, UseReplacementDchar.yes).equal("hello\uFFFDetty"));
    assertThrown!UTFException("hello\xF0betty".byChar.byUTF!(dchar, UseReplacementDchar.no).equal("hello betty"));
}

@safe unittest
{
    {
        wchar[] s = ['a', 'b', 0x219];
        auto r = s.byUTF!char;
        assert(isBidirectionalRange!(typeof(r)));
        assert(r.back == 0x99);
        r.popBack;
        assert(r.back == 0xc8);
        r.popBack;
        assert(r.back == 'b');

    }

    {
        wchar[] s = ['a', 'b', 0x219];
        auto r = s.byUTF!wchar;
        uint i;
        assert(isBidirectionalRange!(typeof(r)));
        assert(r.back == 0x219);
        r.popBack;
        assert(r.back == 'b');
    }

    {
        wchar[] s = ['a', 'b', 0x219];
        auto r = s.byUTF!dchar;
        assert(isBidirectionalRange!(typeof(r)));
        assert(r.back == 0x219);
        r.popBack;
        assert(r.back == 'b');
    }

    {
        dchar[] s = ['𐐷', '😁'];
        auto r = s.byUTF!wchar;
        assert(r.back == 0xde01);
        r.popBack;
        assert(r.back == 0xd83d);
        r.popBack;
        assert(r.back == 0xdc37);
        r.popBack;
        assert(r.back == 0xd801);
    }

    {
        dchar[] s = ['𐐷', '😁'];
        auto r = s.byUTF!char;
        char[] res;
        while (!r.empty)
        {
            res ~= r.back;
            r.popBack;
        }
        import std.algorithm.comparison : equal;
        assert(res.equal([0x81, 0x98, 0x9f, 0xf0, 0xb7, 0x90, 0x90, 0xf0]));
    }

    {
        dchar[] res;
        auto r = ['a', 'b', 'c', 'd', 'e'].byUTF!dchar;
        while (!r.empty)
        {
            res ~= r.back;
            r.popBack;
        }
        import std.algorithm.comparison : equal;
        assert(res.equal(['e', 'd', 'c', 'b', 'a']));
    }

    {
        //testing the save() function
        wchar[] s = ['Ă','ț'];

        auto rc = s.byUTF!char;
        rc.popBack;
        auto rcCopy = rc.save;
        assert(rc.back == rcCopy.back);
        assert(rcCopy.back == 0xc8);

        auto rd = s.byUTF!dchar;
        rd.popBack;
        auto rdCopy = rd.save;
        assert(rd.back == rdCopy.back);
        assert(rdCopy.back == 'Ă');
    }
}

///
@safe pure nothrow unittest
{
    import std.range.primitives;
    wchar[] s = ['ă', 'î'];

    auto rc = s.byUTF!char;
    static assert(isBidirectionalRange!(typeof(rc)));
    assert(rc.back == 0xae);
    rc.popBack;
    assert(rc.back == 0xc3);
    rc.popBack;
    assert(rc.back == 0x83);
    rc.popBack;
    assert(rc.back == 0xc4);

    auto rw = s.byUTF!wchar;
    static assert(isBidirectionalRange!(typeof(rw)));
    assert(rw.back == 'î');
    rw.popBack;
    assert(rw.back == 'ă');

    auto rd = s.byUTF!dchar;
    static assert(isBidirectionalRange!(typeof(rd)));
    assert(rd.back == 'î');
    rd.popBack;
    assert(rd.back == 'ă');
}
