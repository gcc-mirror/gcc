// Written in the D programming language.

/**
Classes and functions for handling and transcoding between various encodings.

For cases where the _encoding is known at compile-time, functions are provided
for arbitrary _encoding and decoding of characters, arbitrary transcoding
between strings of different type, as well as validation and sanitization.

Encodings currently supported are UTF-8, UTF-16, UTF-32, ASCII, ISO-8859-1
(also known as LATIN-1), ISO-8859-2 (LATIN-2), WINDOWS-1250 and WINDOWS-1252.

$(SCRIPT inhibitQuickIndex = 1;)
$(BOOKTABLE,
$(TR $(TH Category) $(TH Functions))
$(TR $(TD Decode) $(TD
    $(LREF codePoints)
    $(LREF decode)
    $(LREF decodeReverse)
    $(LREF safeDecode)
))
$(TR $(TD Conversion) $(TD
    $(LREF codeUnits)
    $(LREF sanitize)
    $(LREF transcode)
))
$(TR $(TD Classification) $(TD
    $(LREF canEncode)
    $(LREF isValid)
    $(LREF isValidCodePoint)
    $(LREF isValidCodeUnit)
))
$(TR $(TD BOM) $(TD
    $(LREF BOM)
    $(LREF BOMSeq)
    $(LREF getBOM)
    $(LREF utfBOM)
))
$(TR $(TD Length &amp; Index) $(TD
    $(LREF firstSequence)
    $(LREF encodedLength)
    $(LREF index)
    $(LREF lastSequence)
    $(LREF validLength)
))
$(TR $(TD Encoding schemes) $(TD
    $(LREF encodingName)
    $(LREF EncodingScheme)
    $(LREF EncodingSchemeASCII)
    $(LREF EncodingSchemeLatin1)
    $(LREF EncodingSchemeLatin2)
    $(LREF EncodingSchemeUtf16Native)
    $(LREF EncodingSchemeUtf32Native)
    $(LREF EncodingSchemeUtf8)
    $(LREF EncodingSchemeWindows1250)
    $(LREF EncodingSchemeWindows1252)
))
$(TR $(TD Representation) $(TD
    $(LREF AsciiChar)
    $(LREF AsciiString)
    $(LREF Latin1Char)
    $(LREF Latin1String)
    $(LREF Latin2Char)
    $(LREF Latin2String)
    $(LREF Windows1250Char)
    $(LREF Windows1250String)
    $(LREF Windows1252Char)
    $(LREF Windows1252String)
))
$(TR $(TD Exceptions) $(TD
    $(LREF INVALID_SEQUENCE)
    $(LREF EncodingException)
))
)

For cases where the _encoding is not known at compile-time, but is
known at run-time, the abstract class $(LREF EncodingScheme)
and its subclasses is provided.  To construct a run-time encoder/decoder,
one does e.g.

----------------------------------------------------
auto e = EncodingScheme.create("utf-8");
----------------------------------------------------

This library supplies $(LREF EncodingScheme) subclasses for ASCII,
ISO-8859-1 (also known as LATIN-1), ISO-8859-2 (LATIN-2), WINDOWS-1250,
WINDOWS-1252, UTF-8, and (on little-endian architectures) UTF-16LE and
UTF-32LE; or (on big-endian architectures) UTF-16BE and UTF-32BE.

This library provides a mechanism whereby other modules may add $(LREF
EncodingScheme) subclasses for any other _encoding.

Copyright: Copyright Janice Caron 2008 - 2009.
License:   $(HTTP www.boost.org/LICENSE_1_0.txt, Boost License 1.0).
Authors:   Janice Caron
Source:    $(PHOBOSSRC std/_encoding.d)
*/
/*
         Copyright Janice Caron 2008 - 2009.
Distributed under the Boost Software License, Version 1.0.
   (See accompanying file LICENSE_1_0.txt or copy at
         http://www.boost.org/LICENSE_1_0.txt)
*/
module std.encoding;

import std.range.primitives;
import std.traits;
import std.typecons;

@system unittest
{
    static ubyte[][] validStrings =
    [
        // Plain ASCII
        cast(ubyte[])"hello",

        // First possible sequence of a certain length
        [ 0x00 ],                       // U+00000000   one byte
        [ 0xC2, 0x80 ],                 // U+00000080   two bytes
        [ 0xE0, 0xA0, 0x80 ],           // U+00000800   three bytes
        [ 0xF0, 0x90, 0x80, 0x80 ],     // U+00010000   three bytes

        // Last possible sequence of a certain length
        [ 0x7F ],                       // U+0000007F   one byte
        [ 0xDF, 0xBF ],                 // U+000007FF   two bytes
        [ 0xEF, 0xBF, 0xBF ],           // U+0000FFFF   three bytes

        // Other boundary conditions
        [ 0xED, 0x9F, 0xBF ],
        // U+0000D7FF   Last character before surrogates
        [ 0xEE, 0x80, 0x80 ],
        // U+0000E000   First character after surrogates
        [ 0xEF, 0xBF, 0xBD ],
        // U+0000FFFD   Unicode replacement character
        [ 0xF4, 0x8F, 0xBF, 0xBF ],
        // U+0010FFFF   Very last character

        // Non-character code points
        /*  NOTE: These are legal in UTF, and may be converted from
            one UTF to another, however they do not represent Unicode
            characters. These code points have been reserved by
            Unicode as non-character code points. They are permissible
            for data exchange within an application, but they are are
            not permitted to be used as characters. Since this module
            deals with UTF, and not with Unicode per se, we choose to
            accept them here. */
        [ 0xDF, 0xBE ],                 // U+0000FFFE
        [ 0xDF, 0xBF ],                 // U+0000FFFF
    ];

    static ubyte[][] invalidStrings =
    [
        // First possible sequence of a certain length, but greater
        // than U+10FFFF
        [ 0xF8, 0x88, 0x80, 0x80, 0x80 ],           // U+00200000   five bytes
        [ 0xFC, 0x84, 0x80, 0x80, 0x80, 0x80 ],     // U+04000000   six bytes

        // Last possible sequence of a certain length, but greater than U+10FFFF
        [ 0xF7, 0xBF, 0xBF, 0xBF ],                 // U+001FFFFF   four bytes
        [ 0xFB, 0xBF, 0xBF, 0xBF, 0xBF ],           // U+03FFFFFF   five bytes
        [ 0xFD, 0xBF, 0xBF, 0xBF, 0xBF, 0xBF ],     // U+7FFFFFFF   six bytes

        // Other boundary conditions
        [ 0xF4, 0x90, 0x80, 0x80 ],                 // U+00110000
                                                    // First code
                                                    // point after
                                                    // last character

        // Unexpected continuation bytes
        [ 0x80 ],
        [ 0xBF ],
        [ 0x20, 0x80, 0x20 ],
        [ 0x20, 0xBF, 0x20 ],
        [ 0x80, 0x9F, 0xA0 ],

        // Lonely start bytes
        [ 0xC0 ],
        [ 0xCF ],
        [ 0x20, 0xC0, 0x20 ],
        [ 0x20, 0xCF, 0x20 ],
        [ 0xD0 ],
        [ 0xDF ],
        [ 0x20, 0xD0, 0x20 ],
        [ 0x20, 0xDF, 0x20 ],
        [ 0xE0 ],
        [ 0xEF ],
        [ 0x20, 0xE0, 0x20 ],
        [ 0x20, 0xEF, 0x20 ],
        [ 0xF0 ],
        [ 0xF1 ],
        [ 0xF2 ],
        [ 0xF3 ],
        [ 0xF4 ],
        [ 0xF5 ],   // If this were legal it would start a character > U+10FFFF
        [ 0xF6 ],   // If this were legal it would start a character > U+10FFFF
        [ 0xF7 ],   // If this were legal it would start a character > U+10FFFF

        [ 0xEF, 0xBF ],             // Three byte sequence with third byte missing
        [ 0xF7, 0xBF, 0xBF ],       // Four byte sequence with fourth byte missing
        [ 0xEF, 0xBF, 0xF7, 0xBF, 0xBF ],   // Concatenation of the above

        // Impossible bytes
        [ 0xF8 ],
        [ 0xF9 ],
        [ 0xFA ],
        [ 0xFB ],
        [ 0xFC ],
        [ 0xFD ],
        [ 0xFE ],
        [ 0xFF ],
        [ 0x20, 0xF8, 0x20 ],
        [ 0x20, 0xF9, 0x20 ],
        [ 0x20, 0xFA, 0x20 ],
        [ 0x20, 0xFB, 0x20 ],
        [ 0x20, 0xFC, 0x20 ],
        [ 0x20, 0xFD, 0x20 ],
        [ 0x20, 0xFE, 0x20 ],
        [ 0x20, 0xFF, 0x20 ],

        // Overlong sequences, all representing U+002F
        /*  With a safe UTF-8 decoder, all of the following five overlong
            representations of the ASCII character slash ("/") should be
            rejected like a malformed UTF-8 sequence */
        [ 0xC0, 0xAF ],
        [ 0xE0, 0x80, 0xAF ],
        [ 0xF0, 0x80, 0x80, 0xAF ],
        [ 0xF8, 0x80, 0x80, 0x80, 0xAF ],
        [ 0xFC, 0x80, 0x80, 0x80, 0x80, 0xAF ],

        // Maximum overlong sequences
        /*  Below you see the highest Unicode value that is still resulting in
            an overlong sequence if represented with the given number of bytes.
            This is a boundary test for safe UTF-8 decoders. All five
            characters should be rejected like malformed UTF-8 sequences. */
        [ 0xC1, 0xBF ],                             // U+0000007F
        [ 0xE0, 0x9F, 0xBF ],                       // U+000007FF
        [ 0xF0, 0x8F, 0xBF, 0xBF ],                 // U+0000FFFF
        [ 0xF8, 0x87, 0xBF, 0xBF, 0xBF ],           // U+001FFFFF
        [ 0xFC, 0x83, 0xBF, 0xBF, 0xBF, 0xBF ],     // U+03FFFFFF

        // Overlong representation of the NUL character
        /*  The following five sequences should also be rejected like malformed
            UTF-8 sequences and should not be treated like the ASCII NUL
            character. */
        [ 0xC0, 0x80 ],
        [ 0xE0, 0x80, 0x80 ],
        [ 0xF0, 0x80, 0x80, 0x80 ],
        [ 0xF8, 0x80, 0x80, 0x80, 0x80 ],
        [ 0xFC, 0x80, 0x80, 0x80, 0x80, 0x80 ],

        // Illegal code positions
        /*  The following UTF-8 sequences should be rejected like malformed
            sequences, because they never represent valid ISO 10646 characters
            and a UTF-8 decoder that accepts them might introduce security
            problems comparable to overlong UTF-8 sequences. */
        [ 0xED, 0xA0, 0x80 ],       // U+D800
        [ 0xED, 0xAD, 0xBF ],       // U+DB7F
        [ 0xED, 0xAE, 0x80 ],       // U+DB80
        [ 0xED, 0xAF, 0xBF ],       // U+DBFF
        [ 0xED, 0xB0, 0x80 ],       // U+DC00
        [ 0xED, 0xBE, 0x80 ],       // U+DF80
        [ 0xED, 0xBF, 0xBF ],       // U+DFFF
    ];

    static string[] sanitizedStrings =
    [
        "\uFFFD","\uFFFD",
        "\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD"," \uFFFD ",
        " \uFFFD ","\uFFFD\uFFFD\uFFFD","\uFFFD","\uFFFD"," \uFFFD "," \uFFFD ",
        "\uFFFD","\uFFFD"," \uFFFD "," \uFFFD ","\uFFFD","\uFFFD"," \uFFFD ",
        " \uFFFD ","\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD",
        "\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD\uFFFD","\uFFFD","\uFFFD",
        "\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD"," \uFFFD ",
        " \uFFFD "," \uFFFD "," \uFFFD "," \uFFFD "," \uFFFD "," \uFFFD ",
        " \uFFFD ","\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD",
        "\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD",
        "\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD","\uFFFD",
    ];

    // Make sure everything that should be valid, is
    foreach (a;validStrings)
    {
        string s = cast(string) a;
        assert(isValid(s),"Failed to validate: "~makeReadable(s));
    }

    // Make sure everything that shouldn't be valid, isn't
    foreach (a;invalidStrings)
    {
        string s = cast(string) a;
        assert(!isValid(s),"Incorrectly validated: "~makeReadable(s));
    }

    // Make sure we can sanitize everything bad
    assert(invalidStrings.length == sanitizedStrings.length);
    for (int i=0; i<invalidStrings.length; ++i)
    {
        string s = cast(string) invalidStrings[i];
        string t = sanitize(s);
        assert(isValid(t));
        assert(t == sanitizedStrings[i]);
        ubyte[] u = cast(ubyte[]) t;
        validStrings ~= u;
    }

    // Make sure all transcodings work in both directions, using both forward
    // and reverse iteration
    foreach (a; validStrings)
    {
        string s = cast(string) a;
        string s2;
        wstring ws, ws2;
        dstring ds, ds2;

        transcode(s,ws);
        assert(isValid(ws));
        transcode(ws,s2);
        assert(s == s2);

        transcode(s,ds);
        assert(isValid(ds));
        transcode(ds,s2);
        assert(s == s2);

        transcode(ws,s);
        assert(isValid(s));
        transcode(s,ws2);
        assert(ws == ws2);

        transcode(ws,ds);
        assert(isValid(ds));
        transcode(ds,ws2);
        assert(ws == ws2);

        transcode(ds,s);
        assert(isValid(s));
        transcode(s,ds2);
        assert(ds == ds2);

        transcode(ds,ws);
        assert(isValid(ws));
        transcode(ws,ds2);
        assert(ds == ds2);

        transcodeReverse(s,ws);
        assert(isValid(ws));
        transcodeReverse(ws,s2);
        assert(s == s2);

        transcodeReverse(s,ds);
        assert(isValid(ds));
        transcodeReverse(ds,s2);
        assert(s == s2);

        transcodeReverse(ws,s);
        assert(isValid(s));
        transcodeReverse(s,ws2);
        assert(ws == ws2);

        transcodeReverse(ws,ds);
        assert(isValid(ds));
        transcodeReverse(ds,ws2);
        assert(ws == ws2);

        transcodeReverse(ds,s);
        assert(isValid(s));
        transcodeReverse(s,ds2);
        assert(ds == ds2);

        transcodeReverse(ds,ws);
        assert(isValid(ws));
        transcodeReverse(ws,ds2);
        assert(ds == ds2);
    }

    // Make sure the non-UTF encodings work too
    {
        auto s = "\u20AC100";
        Windows1252String t;
        transcode(s,t);
        assert(t == cast(Windows1252Char[])[0x80, '1', '0', '0']);
        string u;
        transcode(s,u);
        assert(s == u);
        Latin1String v;
        transcode(s,v);
        assert(cast(string) v == "?100");
        AsciiString w;
        transcode(v,w);
        assert(cast(string) w == "?100");
        s = "\u017Dlu\u0165ou\u010Dk\u00FD k\u016F\u0148";
        Latin2String x;
        transcode(s,x);
        assert(x == cast(Latin2Char[])[0xae, 'l', 'u', 0xbb, 'o', 'u', 0xe8, 'k', 0xfd, ' ', 'k', 0xf9, 0xf2]);
        Windows1250String y;
        transcode(s,y);
        assert(y == cast(Windows1250Char[])[0x8e, 'l', 'u', 0x9d, 'o', 'u', 0xe8, 'k', 0xfd, ' ', 'k', 0xf9, 0xf2]);
    }

    // Make sure we can count properly
    {
        assert(encodedLength!(char)('A') == 1);
        assert(encodedLength!(char)('\u00E3') == 2);
        assert(encodedLength!(char)('\u2028') == 3);
        assert(encodedLength!(char)('\U0010FFF0') == 4);
        assert(encodedLength!(wchar)('A') == 1);
        assert(encodedLength!(wchar)('\U0010FFF0') == 2);
    }

    // Make sure we can write into mutable arrays
    {
        char[4] buffer;
        auto n = encode(cast(dchar)'\u00E3',buffer);
        assert(n == 2);
        assert(buffer[0] == 0xC3);
        assert(buffer[1] == 0xA3);
    }
}

//=============================================================================

/** Special value returned by $(D safeDecode) */
enum dchar INVALID_SEQUENCE = cast(dchar) 0xFFFFFFFF;

template EncoderFunctions()
{
    // Various forms of read

    template ReadFromString()
    {
        @property bool canRead() { return s.length != 0; }
        E peek() @safe pure @nogc nothrow { return s[0]; }
        E read() @safe pure @nogc nothrow { E t = s[0]; s = s[1..$]; return t; }
    }

    template ReverseReadFromString()
    {
        @property bool canRead() { return s.length != 0; }
        E peek() @safe pure @nogc nothrow { return s[$-1]; }
        E read() @safe pure @nogc nothrow { E t = s[$-1]; s = s[0..$-1]; return t; }
    }

    // Various forms of Write

    template WriteToString()
    {
        E[] s;
        void write(E c) @safe pure nothrow { s ~= c; }
    }

    template WriteToArray()
    {
        void write(E c) @safe pure @nogc nothrow { array[0] = c; array = array[1..$]; }
    }

    template WriteToDelegate()
    {
        void write(E c) { dg(c); }
    }

    // Functions we will export

    template EncodeViaWrite()
    {
        mixin encodeViaWrite;
        void encode(dchar c) { encodeViaWrite(c); }
    }

    template SkipViaRead()
    {
        mixin skipViaRead;
        void skip() @safe pure @nogc nothrow { skipViaRead(); }
    }

    template DecodeViaRead()
    {
        mixin decodeViaRead;
        dchar decode() @safe pure @nogc nothrow { return decodeViaRead(); }
    }

    template SafeDecodeViaRead()
    {
        mixin safeDecodeViaRead;
        dchar safeDecode() @safe pure @nogc nothrow { return safeDecodeViaRead(); }
    }

    template DecodeReverseViaRead()
    {
        mixin decodeReverseViaRead;
        dchar decodeReverse() @safe pure @nogc nothrow { return decodeReverseViaRead(); }
    }

    // Encoding to different destinations

    template EncodeToString()
    {
        mixin WriteToString;
        mixin EncodeViaWrite;
    }

    template EncodeToArray()
    {
        mixin WriteToArray;
        mixin EncodeViaWrite;
    }

    template EncodeToDelegate()
    {
        mixin WriteToDelegate;
        mixin EncodeViaWrite;
    }

    // Decoding functions

    template SkipFromString()
    {
        mixin ReadFromString;
        mixin SkipViaRead;
    }

    template DecodeFromString()
    {
        mixin ReadFromString;
        mixin DecodeViaRead;
    }

    template SafeDecodeFromString()
    {
        mixin ReadFromString;
        mixin SafeDecodeViaRead;
    }

    template DecodeReverseFromString()
    {
        mixin ReverseReadFromString;
        mixin DecodeReverseViaRead;
    }

    //=========================================================================

    // Below are the functions we will ultimately expose to the user

    E[] encode(dchar c) @safe pure nothrow
    {
        mixin EncodeToString e;
        e.encode(c);
        return e.s;
    }

    void encode(dchar c, ref E[] array) @safe pure nothrow
    {
        mixin EncodeToArray e;
        e.encode(c);
    }

    void encode(dchar c, void delegate(E) dg)
    {
        mixin EncodeToDelegate e;
        e.encode(c);
    }

    void skip(ref const(E)[] s) @safe pure nothrow
    {
        mixin SkipFromString e;
        e.skip();
    }

    dchar decode(S)(ref S s)
    {
        mixin DecodeFromString e;
        return e.decode();
    }

    dchar safeDecode(S)(ref S s)
    {
        mixin SafeDecodeFromString e;
        return e.safeDecode();
    }

    dchar decodeReverse(ref const(E)[] s) @safe pure nothrow
    {
        mixin DecodeReverseFromString e;
        return e.decodeReverse();
    }
}

//=========================================================================

struct CodePoints(E)
{
    const(E)[] s;

    this(const(E)[] s)
    in
    {
        assert(isValid(s));
    }
    body
    {
        this.s = s;
    }

    int opApply(scope int delegate(ref dchar) dg)
    {
        int result = 0;
        while (s.length != 0)
        {
            dchar c = decode(s);
            result = dg(c);
            if (result != 0) break;
        }
        return result;
    }

    int opApply(scope int delegate(ref size_t, ref dchar) dg)
    {
        size_t i = 0;
        int result = 0;
        while (s.length != 0)
        {
            immutable len = s.length;
            dchar c = decode(s);
            size_t j = i; // We don't want the delegate corrupting i
            result = dg(j,c);
            if (result != 0) break;
            i += len - s.length;
        }
        return result;
    }

    int opApplyReverse(scope int delegate(ref dchar) dg)
    {
        int result = 0;
        while (s.length != 0)
        {
            dchar c = decodeReverse(s);
            result = dg(c);
            if (result != 0) break;
        }
        return result;
    }

    int opApplyReverse(scope int delegate(ref size_t, ref dchar) dg)
    {
        int result = 0;
        while (s.length != 0)
        {
            dchar c = decodeReverse(s);
            size_t i = s.length;
            result = dg(i,c);
            if (result != 0) break;
        }
        return result;
    }
}

struct CodeUnits(E)
{
    E[] s;

    this(dchar d)
    in
    {
        assert(isValidCodePoint(d));
    }
    body
    {
        s = encode!(E)(d);
    }

    int opApply(scope int delegate(ref E) dg)
    {
        int result = 0;
        foreach (E c;s)
        {
            result = dg(c);
            if (result != 0) break;
        }
        return result;
    }

    int opApplyReverse(scope int delegate(ref E) dg)
    {
        int result = 0;
        foreach_reverse (E c;s)
        {
            result = dg(c);
            if (result != 0) break;
        }
        return result;
    }
}

//=============================================================================

template EncoderInstance(E)
{
    static assert(false,"Cannot instantiate EncoderInstance for type "
        ~ E.stringof);
}

private template GenericEncoder()
{
    bool canEncode(dchar c) @safe pure @nogc nothrow
    {
        if (c < m_charMapStart || (c > m_charMapEnd && c < 0x100)) return true;
        if (c >= 0xFFFD) return false;

        auto idx = 0;
        while (idx < bstMap.length)
        {
            if (bstMap[idx][0] == c) return true;
            idx = bstMap[idx][0] > c ? 2 * idx + 1 : 2 * idx + 2; // next BST index
        }

        return false;
    }

    bool isValidCodeUnit(E c) @safe pure @nogc nothrow
    {
        if (c < m_charMapStart || c > m_charMapEnd) return true;
        return charMap[c-m_charMapStart] != 0xFFFD;
    }

    size_t encodedLength(dchar c) @safe pure @nogc nothrow
    in
    {
        assert(canEncode(c));
    }
    body
    {
        return 1;
    }

    void encodeViaWrite()(dchar c)
    {
        if (c < m_charMapStart || (c > m_charMapEnd && c < 0x100)) {}
        else if (c >= 0xFFFD) { c = '?'; }
        else
        {
            auto idx = 0;
            while (idx < bstMap.length)
            {
                if (bstMap[idx][0] == c)
                {
                    write(cast(E) bstMap[idx][1]);
                    return;
                }
                idx = bstMap[idx][0] > c ? 2 * idx + 1 : 2 * idx + 2; // next BST index
            }
            c = '?';
        }
        write(cast(E) c);
    }

    void skipViaRead()()
    {
        read();
    }

    dchar decodeViaRead()()
    {
        E c = read();
        return (c >= m_charMapStart && c <= m_charMapEnd) ? charMap[c-m_charMapStart] : c;
    }

    dchar safeDecodeViaRead()()
    {
        immutable E c = read();
        immutable d = (c >= m_charMapStart && c <= m_charMapEnd) ? charMap[c-m_charMapStart] : c;
        return d == 0xFFFD ? INVALID_SEQUENCE : d;
    }

    dchar decodeReverseViaRead()()
    {
        E c = read();
        return (c >= m_charMapStart && c <= m_charMapEnd) ? charMap[c-m_charMapStart] : c;
    }

    @property EString replacementSequence() @safe pure @nogc nothrow
    {
        return cast(EString)("?");
    }

    mixin EncoderFunctions;
}

//=============================================================================
//          ASCII
//=============================================================================

/** Defines various character sets. */
enum AsciiChar : ubyte { init }
/// Ditto
alias AsciiString = immutable(AsciiChar)[];

template EncoderInstance(CharType : AsciiChar)
{
    alias E = AsciiChar;
    alias EString = AsciiString;

    @property string encodingName() @safe pure nothrow @nogc
    {
        return "ASCII";
    }

    bool canEncode(dchar c) @safe pure nothrow @nogc
    {
        return c < 0x80;
    }

    bool isValidCodeUnit(AsciiChar c) @safe pure nothrow @nogc
    {
        return c < 0x80;
    }

    size_t encodedLength(dchar c) @safe pure nothrow @nogc
    in
    {
        assert(canEncode(c));
    }
    body
    {
        return 1;
    }

    void encodeX(Range)(dchar c, Range r)
    {
        if (!canEncode(c)) c = '?';
        r.write(cast(AsciiChar) c);
    }

    void encodeViaWrite()(dchar c)
    {
        if (!canEncode(c)) c = '?';
        write(cast(AsciiChar) c);
    }

    void skipViaRead()()
    {
        read();
    }

    dchar decodeViaRead()()
    {
        return read();
    }

    dchar safeDecodeViaRead()()
    {
        immutable c = read();
        return canEncode(c) ? c : INVALID_SEQUENCE;
    }

    dchar decodeReverseViaRead()()
    {
        return read();
    }

    @property EString replacementSequence() @safe pure nothrow @nogc
    {
        return cast(EString)("?");
    }

    mixin EncoderFunctions;
}

//=============================================================================
//          ISO-8859-1
//=============================================================================

/** Defines an Latin1-encoded character. */
enum Latin1Char : ubyte { init }
/**
Defines an Latin1-encoded string (as an array of $(D
immutable(Latin1Char))).
 */
alias Latin1String = immutable(Latin1Char)[];

template EncoderInstance(CharType : Latin1Char)
{
    alias E = Latin1Char;
    alias EString = Latin1String;

    @property string encodingName() @safe pure nothrow @nogc
    {
        return "ISO-8859-1";
    }

    bool canEncode(dchar c) @safe pure nothrow @nogc
    {
        return c < 0x100;
    }

    bool isValidCodeUnit(Latin1Char c) @safe pure nothrow @nogc
    {
        return true;
    }

    size_t encodedLength(dchar c) @safe pure nothrow @nogc
    in
    {
        assert(canEncode(c));
    }
    body
    {
        return 1;
    }

    void encodeViaWrite()(dchar c)
    {
        if (!canEncode(c)) c = '?';
        write(cast(Latin1Char) c);
    }

    void skipViaRead()()
    {
        read();
    }

    dchar decodeViaRead()()
    {
        return read();
    }

    dchar safeDecodeViaRead()()
    {
        return read();
    }

    dchar decodeReverseViaRead()()
    {
        return read();
    }

    @property EString replacementSequence() @safe pure nothrow @nogc
    {
        return cast(EString)("?");
    }

    mixin EncoderFunctions;
}

//=============================================================================
//          ISO-8859-2
//=============================================================================

/// Defines a Latin2-encoded character.
enum Latin2Char : ubyte { init }

/**
 * Defines an Latin2-encoded string (as an array of $(D
 * immutable(Latin2Char))).
 */
alias Latin2String = immutable(Latin2Char)[];

private template EncoderInstance(CharType : Latin2Char)
{
    import std.typecons : Tuple, tuple;

    alias E = Latin2Char;
    alias EString = Latin2String;

    @property string encodingName() @safe pure nothrow @nogc
    {
        return "ISO-8859-2";
    }

    private static immutable dchar m_charMapStart = 0xa1;
    private static immutable dchar m_charMapEnd = 0xff;

    private immutable wstring charMap =
        "\u0104\u02D8\u0141\u00A4\u013D\u015A\u00A7\u00A8"~
        "\u0160\u015E\u0164\u0179\u00AD\u017D\u017B\u00B0"~
        "\u0105\u02DB\u0142\u00B4\u013E\u015B\u02C7\u00B8"~
        "\u0161\u015F\u0165\u017A\u02DD\u017E\u017C\u0154"~
        "\u00C1\u00C2\u0102\u00C4\u0139\u0106\u00C7\u010C"~
        "\u00C9\u0118\u00CB\u011A\u00CD\u00CE\u010E\u0110"~
        "\u0143\u0147\u00D3\u00D4\u0150\u00D6\u00D7\u0158"~
        "\u016E\u00DA\u0170\u00DC\u00DD\u0162\u00DF\u0155"~
        "\u00E1\u00E2\u0103\u00E4\u013A\u0107\u00E7\u010D"~
        "\u00E9\u0119\u00EB\u011B\u00ED\u00EE\u010F\u0111"~
        "\u0144\u0148\u00F3\u00F4\u0151\u00F6\u00F7\u0159"~
        "\u016F\u00FA\u0171\u00FC\u00FD\u0163\u02D9";

    private immutable Tuple!(wchar, char)[] bstMap = [
        tuple('\u0148','\xF2'), tuple('\u00F3','\xF3'), tuple('\u0165','\xBB'),
        tuple('\u00D3','\xD3'), tuple('\u010F','\xEF'), tuple('\u015B','\xB6'),
        tuple('\u017C','\xBF'), tuple('\u00C1','\xC1'), tuple('\u00E1','\xE1'),
        tuple('\u0103','\xE3'), tuple('\u013A','\xE5'), tuple('\u0155','\xE0'),
        tuple('\u0161','\xB9'), tuple('\u0171','\xFB'), tuple('\u02D8','\xA2'),
        tuple('\u00AD','\xAD'), tuple('\u00C9','\xC9'), tuple('\u00DA','\xDA'),
        tuple('\u00E9','\xE9'), tuple('\u00FA','\xFA'), tuple('\u0107','\xE6'),
        tuple('\u0119','\xEA'), tuple('\u0142','\xB3'), tuple('\u0151','\xF5'),
        tuple('\u0159','\xF8'), tuple('\u015F','\xBA'), tuple('\u0163','\xFE'),
        tuple('\u016F','\xF9'), tuple('\u017A','\xBC'), tuple('\u017E','\xBE'),
        tuple('\u02DB','\xB2'), tuple('\u00A7','\xA7'), tuple('\u00B4','\xB4'),
        tuple('\u00C4','\xC4'), tuple('\u00CD','\xCD'), tuple('\u00D6','\xD6'),
        tuple('\u00DD','\xDD'), tuple('\u00E4','\xE4'), tuple('\u00ED','\xED'),
        tuple('\u00F6','\xF6'), tuple('\u00FD','\xFD'), tuple('\u0105','\xB1'),
        tuple('\u010D','\xE8'), tuple('\u0111','\xF0'), tuple('\u011B','\xEC'),
        tuple('\u013E','\xB5'), tuple('\u0144','\xF1'), tuple('\u0150','\xD5'),
        tuple('\u0154','\xC0'), tuple('\u0158','\xD8'), tuple('\u015A','\xA6'),
        tuple('\u015E','\xAA'), tuple('\u0160','\xA9'), tuple('\u0162','\xDE'),
        tuple('\u0164','\xAB'), tuple('\u016E','\xD9'), tuple('\u0170','\xDB'),
        tuple('\u0179','\xAC'), tuple('\u017B','\xAF'), tuple('\u017D','\xAE'),
        tuple('\u02C7','\xB7'), tuple('\u02D9','\xFF'), tuple('\u02DD','\xBD'),
        tuple('\u00A4','\xA4'), tuple('\u00A8','\xA8'), tuple('\u00B0','\xB0'),
        tuple('\u00B8','\xB8'), tuple('\u00C2','\xC2'), tuple('\u00C7','\xC7'),
        tuple('\u00CB','\xCB'), tuple('\u00CE','\xCE'), tuple('\u00D4','\xD4'),
        tuple('\u00D7','\xD7'), tuple('\u00DC','\xDC'), tuple('\u00DF','\xDF'),
        tuple('\u00E2','\xE2'), tuple('\u00E7','\xE7'), tuple('\u00EB','\xEB'),
        tuple('\u00EE','\xEE'), tuple('\u00F4','\xF4'), tuple('\u00F7','\xF7'),
        tuple('\u00FC','\xFC'), tuple('\u0102','\xC3'), tuple('\u0104','\xA1'),
        tuple('\u0106','\xC6'), tuple('\u010C','\xC8'), tuple('\u010E','\xCF'),
        tuple('\u0110','\xD0'), tuple('\u0118','\xCA'), tuple('\u011A','\xCC'),
        tuple('\u0139','\xC5'), tuple('\u013D','\xA5'), tuple('\u0141','\xA3'),
        tuple('\u0143','\xD1'), tuple('\u0147','\xD2')
    ];

    mixin GenericEncoder!();
}

//=============================================================================
//          WINDOWS-1250
//=============================================================================

/// Defines a Windows1250-encoded character.
enum Windows1250Char : ubyte { init }

/**
 * Defines an Windows1250-encoded string (as an array of $(D
 * immutable(Windows1250Char))).
 */
alias Windows1250String = immutable(Windows1250Char)[];

private template EncoderInstance(CharType : Windows1250Char)
{
    import std.typecons : Tuple, tuple;

    alias E = Windows1250Char;
    alias EString = Windows1250String;

    @property string encodingName() @safe pure nothrow @nogc
    {
        return "windows-1250";
    }

    private static immutable dchar m_charMapStart = 0x80;
    private static immutable dchar m_charMapEnd = 0xff;

    private immutable wstring charMap =
        "\u20AC\uFFFD\u201A\uFFFD\u201E\u2026\u2020\u2021"~
        "\uFFFD\u2030\u0160\u2039\u015A\u0164\u017D\u0179"~
        "\uFFFD\u2018\u2019\u201C\u201D\u2022\u2013\u2014"~
        "\uFFFD\u2122\u0161\u203A\u015B\u0165\u017E\u017A"~
        "\u00A0\u02C7\u02D8\u0141\u00A4\u0104\u00A6\u00A7"~
        "\u00A8\u00A9\u015E\u00AB\u00AC\u00AD\u00AE\u017B"~
        "\u00B0\u00B1\u02DB\u0142\u00B4\u00B5\u00B6\u00B7"~
        "\u00B8\u0105\u015F\u00BB\u013D\u02DD\u013E\u017C"~
        "\u0154\u00C1\u00C2\u0102\u00C4\u0139\u0106\u00C7"~
        "\u010C\u00C9\u0118\u00CB\u011A\u00CD\u00CE\u010E"~
        "\u0110\u0143\u0147\u00D3\u00D4\u0150\u00D6\u00D7"~
        "\u0158\u016E\u00DA\u0170\u00DC\u00DD\u0162\u00DF"~
        "\u0155\u00E1\u00E2\u0103\u00E4\u013A\u0107\u00E7"~
        "\u010D\u00E9\u0119\u00EB\u011B\u00ED\u00EE\u010F"~
        "\u0111\u0144\u0148\u00F3\u00F4\u0151\u00F6\u00F7"~
        "\u0159\u016F\u00FA\u0171\u00FC\u00FD\u0163\u02D9";

    private immutable Tuple!(wchar, char)[] bstMap = [
        tuple('\u011A','\xCC'), tuple('\u00DC','\xDC'), tuple('\u0179','\x8F'),
        tuple('\u00B7','\xB7'), tuple('\u00FC','\xFC'), tuple('\u0158','\xD8'),
        tuple('\u201C','\x93'), tuple('\u00AC','\xAC'), tuple('\u00CB','\xCB'),
        tuple('\u00EB','\xEB'), tuple('\u010C','\xC8'), tuple('\u0143','\xD1'),
        tuple('\u0162','\xDE'), tuple('\u02D9','\xFF'), tuple('\u2039','\x8B'),
        tuple('\u00A7','\xA7'), tuple('\u00B1','\xB1'), tuple('\u00C2','\xC2'),
        tuple('\u00D4','\xD4'), tuple('\u00E2','\xE2'), tuple('\u00F4','\xF4'),
        tuple('\u0104','\xA5'), tuple('\u0110','\xD0'), tuple('\u013D','\xBC'),
        tuple('\u0150','\xD5'), tuple('\u015E','\xAA'), tuple('\u016E','\xD9'),
        tuple('\u017D','\x8E'), tuple('\u2014','\x97'), tuple('\u2021','\x87'),
        tuple('\u20AC','\x80'), tuple('\u00A4','\xA4'), tuple('\u00A9','\xA9'),
        tuple('\u00AE','\xAE'), tuple('\u00B5','\xB5'), tuple('\u00BB','\xBB'),
        tuple('\u00C7','\xC7'), tuple('\u00CE','\xCE'), tuple('\u00D7','\xD7'),
        tuple('\u00DF','\xDF'), tuple('\u00E7','\xE7'), tuple('\u00EE','\xEE'),
        tuple('\u00F7','\xF7'), tuple('\u0102','\xC3'), tuple('\u0106','\xC6'),
        tuple('\u010E','\xCF'), tuple('\u0118','\xCA'), tuple('\u0139','\xC5'),
        tuple('\u0141','\xA3'), tuple('\u0147','\xD2'), tuple('\u0154','\xC0'),
        tuple('\u015A','\x8C'), tuple('\u0160','\x8A'), tuple('\u0164','\x8D'),
        tuple('\u0170','\xDB'), tuple('\u017B','\xAF'), tuple('\u02C7','\xA1'),
        tuple('\u02DD','\xBD'), tuple('\u2019','\x92'), tuple('\u201E','\x84'),
        tuple('\u2026','\x85'), tuple('\u203A','\x9B'), tuple('\u2122','\x99'),
        tuple('\u00A0','\xA0'), tuple('\u00A6','\xA6'), tuple('\u00A8','\xA8'),
        tuple('\u00AB','\xAB'), tuple('\u00AD','\xAD'), tuple('\u00B0','\xB0'),
        tuple('\u00B4','\xB4'), tuple('\u00B6','\xB6'), tuple('\u00B8','\xB8'),
        tuple('\u00C1','\xC1'), tuple('\u00C4','\xC4'), tuple('\u00C9','\xC9'),
        tuple('\u00CD','\xCD'), tuple('\u00D3','\xD3'), tuple('\u00D6','\xD6'),
        tuple('\u00DA','\xDA'), tuple('\u00DD','\xDD'), tuple('\u00E1','\xE1'),
        tuple('\u00E4','\xE4'), tuple('\u00E9','\xE9'), tuple('\u00ED','\xED'),
        tuple('\u00F3','\xF3'), tuple('\u00F6','\xF6'), tuple('\u00FA','\xFA'),
        tuple('\u00FD','\xFD'), tuple('\u0103','\xE3'), tuple('\u0105','\xB9'),
        tuple('\u0107','\xE6'), tuple('\u010D','\xE8'), tuple('\u010F','\xEF'),
        tuple('\u0111','\xF0'), tuple('\u0119','\xEA'), tuple('\u011B','\xEC'),
        tuple('\u013A','\xE5'), tuple('\u013E','\xBE'), tuple('\u0142','\xB3'),
        tuple('\u0144','\xF1'), tuple('\u0148','\xF2'), tuple('\u0151','\xF5'),
        tuple('\u0155','\xE0'), tuple('\u0159','\xF8'), tuple('\u015B','\x9C'),
        tuple('\u015F','\xBA'), tuple('\u0161','\x9A'), tuple('\u0163','\xFE'),
        tuple('\u0165','\x9D'), tuple('\u016F','\xF9'), tuple('\u0171','\xFB'),
        tuple('\u017A','\x9F'), tuple('\u017C','\xBF'), tuple('\u017E','\x9E'),
        tuple('\u02D8','\xA2'), tuple('\u02DB','\xB2'), tuple('\u2013','\x96'),
        tuple('\u2018','\x91'), tuple('\u201A','\x82'), tuple('\u201D','\x94'),
        tuple('\u2020','\x86'), tuple('\u2022','\x95'), tuple('\u2030','\x89')
    ];

    mixin GenericEncoder!();
}

//=============================================================================
//          WINDOWS-1252
//=============================================================================

/// Defines a Windows1252-encoded character.
enum Windows1252Char : ubyte { init }

/**
 * Defines an Windows1252-encoded string (as an array of $(D
 * immutable(Windows1252Char))).
 */
alias Windows1252String = immutable(Windows1252Char)[];

template EncoderInstance(CharType : Windows1252Char)
{
    import std.typecons : Tuple, tuple;

    alias E = Windows1252Char;
    alias EString = Windows1252String;

    @property string encodingName() @safe pure nothrow @nogc
    {
        return "windows-1252";
    }

    private static immutable dchar m_charMapStart = 0x80;
    private static immutable dchar m_charMapEnd = 0x9f;

    private immutable wstring charMap =
        "\u20AC\uFFFD\u201A\u0192\u201E\u2026\u2020\u2021"~
        "\u02C6\u2030\u0160\u2039\u0152\uFFFD\u017D\uFFFD"~
        "\uFFFD\u2018\u2019\u201C\u201D\u2022\u2013\u2014"~
        "\u02DC\u2122\u0161\u203A\u0153\uFFFD\u017E\u0178";

    private immutable Tuple!(wchar, char)[] bstMap = [
        tuple('\u201C','\x93'), tuple('\u0192','\x83'), tuple('\u2039','\x8B'),
        tuple('\u0161','\x9A'), tuple('\u2014','\x97'), tuple('\u2021','\x87'),
        tuple('\u20AC','\x80'), tuple('\u0153','\x9C'), tuple('\u017D','\x8E'),
        tuple('\u02DC','\x98'), tuple('\u2019','\x92'), tuple('\u201E','\x84'),
        tuple('\u2026','\x85'), tuple('\u203A','\x9B'), tuple('\u2122','\x99'),
        tuple('\u0152','\x8C'), tuple('\u0160','\x8A'), tuple('\u0178','\x9F'),
        tuple('\u017E','\x9E'), tuple('\u02C6','\x88'), tuple('\u2013','\x96'),
        tuple('\u2018','\x91'), tuple('\u201A','\x82'), tuple('\u201D','\x94'),
        tuple('\u2020','\x86'), tuple('\u2022','\x95'), tuple('\u2030','\x89')
    ];

    mixin GenericEncoder!();
}

//=============================================================================
//          UTF-8
//=============================================================================

template EncoderInstance(CharType : char)
{
    alias E = char;
    alias EString = immutable(char)[];

    @property string encodingName() @safe pure nothrow @nogc
    {
        return "UTF-8";
    }

    bool canEncode(dchar c) @safe pure nothrow @nogc
    {
        return isValidCodePoint(c);
    }

    bool isValidCodeUnit(char c) @safe pure nothrow @nogc
    {
        return (c < 0xC0 || (c >= 0xC2 && c < 0xF5));
    }

    immutable ubyte[128] tailTable =
    [
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        3,3,3,3,3,3,3,3,4,4,4,4,5,5,6,0,
    ];

    private int tails(char c) @safe pure nothrow @nogc
    in
    {
        assert(c >= 0x80);
    }
    body
    {
        return tailTable[c-0x80];
    }

    size_t encodedLength(dchar c) @safe pure nothrow @nogc
    in
    {
        assert(canEncode(c));
    }
    body
    {
        if (c < 0x80) return 1;
        if (c < 0x800) return 2;
        if (c < 0x10000) return 3;
        return 4;
    }

    void encodeViaWrite()(dchar c)
    {
        if (c < 0x80)
        {
            write(cast(char) c);
        }
        else if (c < 0x800)
        {
            write(cast(char)((c >> 6) + 0xC0));
            write(cast(char)((c & 0x3F) + 0x80));
        }
        else if (c < 0x10000)
        {
            write(cast(char)((c >> 12) + 0xE0));
            write(cast(char)(((c >> 6) & 0x3F) + 0x80));
            write(cast(char)((c & 0x3F) + 0x80));
        }
        else
        {
            write(cast(char)((c >> 18) + 0xF0));
            write(cast(char)(((c >> 12) & 0x3F) + 0x80));
            write(cast(char)(((c >> 6) & 0x3F) + 0x80));
            write(cast(char)((c & 0x3F) + 0x80));
        }
    }

    void skipViaRead()()
    {
        auto c = read();
        if (c < 0xC0) return;
        int n = tails(cast(char) c);
        for (size_t i=0; i<n; ++i)
        {
            read();
        }
    }

    dchar decodeViaRead()()
    {
        dchar c = read();
        if (c < 0xC0) return c;
        int n = tails(cast(char) c);
        c &= (1 << (6 - n)) - 1;
        for (size_t i=0; i<n; ++i)
        {
            c = (c << 6) + (read() & 0x3F);
        }
        return c;
    }

    dchar safeDecodeViaRead()()
    {
        dchar c = read();
        if (c < 0x80) return c;
        int n = tails(cast(char) c);
        if (n == 0) return INVALID_SEQUENCE;

        if (!canRead) return INVALID_SEQUENCE;
        size_t d = peek();
        immutable err =
        (
            (c < 0xC2)                              // fail overlong 2-byte sequences
        ||  (c > 0xF4)                              // fail overlong 4-6-byte sequences
        ||  (c == 0xE0 && ((d & 0xE0) == 0x80))     // fail overlong 3-byte sequences
        ||  (c == 0xED && ((d & 0xE0) == 0xA0))     // fail surrogates
        ||  (c == 0xF0 && ((d & 0xF0) == 0x80))     // fail overlong 4-byte sequences
        ||  (c == 0xF4 && ((d & 0xF0) >= 0x90))     // fail code points > 0x10FFFF
        );

        c &= (1 << (6 - n)) - 1;
        for (size_t i=0; i<n; ++i)
        {
            if (!canRead) return INVALID_SEQUENCE;
            d = peek();
            if ((d & 0xC0) != 0x80) return INVALID_SEQUENCE;
            c = (c << 6) + (read() & 0x3F);
        }

        return err ? INVALID_SEQUENCE : c;
    }

    dchar decodeReverseViaRead()()
    {
        dchar c = read();
        if (c < 0x80) return c;
        size_t shift = 0;
        c &= 0x3F;
        for (size_t i=0; i<4; ++i)
        {
            shift += 6;
            auto d = read();
            size_t n = tails(cast(char) d);
            immutable mask = n == 0 ? 0x3F : (1 << (6 - n)) - 1;
            c += ((d & mask) << shift);
            if (n != 0) break;
        }
        return c;
    }

    @property EString replacementSequence() @safe pure nothrow @nogc
    {
        return "\uFFFD";
    }

    mixin EncoderFunctions;
}

//=============================================================================
//          UTF-16
//=============================================================================

template EncoderInstance(CharType : wchar)
{
    alias E = wchar;
    alias EString = immutable(wchar)[];

    @property string encodingName() @safe pure nothrow @nogc
    {
        return "UTF-16";
    }

    bool canEncode(dchar c) @safe pure nothrow @nogc
    {
        return isValidCodePoint(c);
    }

    bool isValidCodeUnit(wchar c) @safe pure nothrow @nogc
    {
        return true;
    }

    size_t encodedLength(dchar c) @safe pure nothrow @nogc
    in
    {
        assert(canEncode(c));
    }
    body
    {
        return (c < 0x10000) ? 1 : 2;
    }

    void encodeViaWrite()(dchar c)
    {
        if (c < 0x10000)
        {
            write(cast(wchar) c);
        }
        else
        {
            size_t n = c - 0x10000;
            write(cast(wchar)(0xD800 + (n >> 10)));
            write(cast(wchar)(0xDC00 + (n & 0x3FF)));
        }
    }

    void skipViaRead()()
    {
        immutable c = read();
        if (c < 0xD800 || c >= 0xE000) return;
        read();
    }

    dchar decodeViaRead()()
    {
        wchar c = read();
        if (c < 0xD800 || c >= 0xE000) return cast(dchar) c;
        wchar d = read();
        c &= 0x3FF;
        d &= 0x3FF;
        return 0x10000 + (c << 10) + d;
    }

    dchar safeDecodeViaRead()()
    {
        wchar c = read();
        if (c < 0xD800 || c >= 0xE000) return cast(dchar) c;
        if (c >= 0xDC00) return INVALID_SEQUENCE;
        if (!canRead) return INVALID_SEQUENCE;
        wchar d = peek();
        if (d < 0xDC00 || d >= 0xE000) return INVALID_SEQUENCE;
        d = read();
        c &= 0x3FF;
        d &= 0x3FF;
        return 0x10000 + (c << 10) + d;
    }

    dchar decodeReverseViaRead()()
    {
        wchar c = read();
        if (c < 0xD800 || c >= 0xE000) return cast(dchar) c;
        wchar d = read();
        c &= 0x3FF;
        d &= 0x3FF;
        return 0x10000 + (d << 10) + c;
    }

    @property EString replacementSequence() @safe pure nothrow @nogc
    {
        return "\uFFFD"w;
    }

    mixin EncoderFunctions;
}

//=============================================================================
//          UTF-32
//=============================================================================

template EncoderInstance(CharType : dchar)
{
    alias E = dchar;
    alias EString = immutable(dchar)[];

    @property string encodingName() @safe pure nothrow @nogc
    {
        return "UTF-32";
    }

    bool canEncode(dchar c) @safe pure @nogc nothrow
    {
        return isValidCodePoint(c);
    }

    bool isValidCodeUnit(dchar c) @safe pure @nogc nothrow
    {
        return isValidCodePoint(c);
    }

    size_t encodedLength(dchar c) @safe pure @nogc nothrow
    in
    {
        assert(canEncode(c));
    }
    body
    {
        return 1;
    }

    void encodeViaWrite()(dchar c)
    {
        write(c);
    }

    void skipViaRead()()
    {
        read();
    }

    dchar decodeViaRead()()
    {
        return cast(dchar) read();
    }

    dchar safeDecodeViaRead()()
    {
        immutable c = read();
        return isValidCodePoint(c) ? c : INVALID_SEQUENCE;
    }

    dchar decodeReverseViaRead()()
    {
        return cast(dchar) read();
    }

    @property EString replacementSequence() @safe pure nothrow @nogc
    {
        return "\uFFFD"d;
    }

    mixin EncoderFunctions;
}

//=============================================================================
// Below are forwarding functions which expose the function to the user

/**
Returns true if c is a valid code point

 Note that this includes the non-character code points U+FFFE and U+FFFF,
 since these are valid code points (even though they are not valid
 characters).

 Supersedes:
 This function supersedes $(D std.utf.startsValidDchar()).

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    c = the code point to be tested
 */
bool isValidCodePoint(dchar c) @safe pure nothrow @nogc
{
    return c < 0xD800 || (c >= 0xE000 && c < 0x110000);
}

/**
 Returns the name of an encoding.

 The type of encoding cannot be deduced. Therefore, it is necessary to
 explicitly specify the encoding type.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252
 */
@property string encodingName(T)()
{
    return EncoderInstance!(T).encodingName;
}

///
@safe unittest
{
    assert(encodingName!(char) == "UTF-8");
    assert(encodingName!(wchar) == "UTF-16");
    assert(encodingName!(dchar) == "UTF-32");
    assert(encodingName!(AsciiChar) == "ASCII");
    assert(encodingName!(Latin1Char) == "ISO-8859-1");
    assert(encodingName!(Latin2Char) == "ISO-8859-2");
    assert(encodingName!(Windows1250Char) == "windows-1250");
    assert(encodingName!(Windows1252Char) == "windows-1252");
}

/**
 Returns true iff it is possible to represent the specified codepoint
 in the encoding.

 The type of encoding cannot be deduced. Therefore, it is necessary to
 explicitly specify the encoding type.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252
 */
bool canEncode(E)(dchar c)
{
    return EncoderInstance!(E).canEncode(c);
}

///
@safe pure unittest
{
    assert( canEncode!(Latin1Char)('A'));
    assert( canEncode!(Latin2Char)('A'));
    assert(!canEncode!(AsciiChar)('\u00A0'));
    assert( canEncode!(Latin1Char)('\u00A0'));
    assert( canEncode!(Latin2Char)('\u00A0'));
    assert( canEncode!(Windows1250Char)('\u20AC'));
    assert(!canEncode!(Windows1250Char)('\u20AD'));
    assert(!canEncode!(Windows1250Char)('\uFFFD'));
    assert( canEncode!(Windows1252Char)('\u20AC'));
    assert(!canEncode!(Windows1252Char)('\u20AD'));
    assert(!canEncode!(Windows1252Char)('\uFFFD'));
    assert(!canEncode!(char)(cast(dchar) 0x110000));
}

/// How to check an entire string
@safe pure unittest
{
    import std.algorithm.searching : find;
    import std.utf : byDchar;

    assert("The quick brown fox"
        .byDchar
        .find!(x => !canEncode!AsciiChar(x))
        .empty);
}

/**
 Returns true if the code unit is legal. For example, the byte 0x80 would
 not be legal in ASCII, because ASCII code units must always be in the range
 0x00 to 0x7F.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    c = the code unit to be tested
 */
bool isValidCodeUnit(E)(E c)
{
    return EncoderInstance!(E).isValidCodeUnit(c);
}

///
@system pure unittest
{
    assert(!isValidCodeUnit(cast(char) 0xC0));
    assert(!isValidCodeUnit(cast(char) 0xFF));
    assert( isValidCodeUnit(cast(wchar) 0xD800));
    assert(!isValidCodeUnit(cast(dchar) 0xD800));
    assert(!isValidCodeUnit(cast(AsciiChar) 0xA0));
    assert( isValidCodeUnit(cast(Windows1250Char) 0x80));
    assert(!isValidCodeUnit(cast(Windows1250Char) 0x81));
    assert( isValidCodeUnit(cast(Windows1252Char) 0x80));
    assert(!isValidCodeUnit(cast(Windows1252Char) 0x81));
}

/**
 Returns true if the string is encoded correctly

 Supersedes:
 This function supersedes std.utf.validate(), however note that this
 function returns a bool indicating whether the input was valid or not,
 whereas the older function would throw an exception.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    s = the string to be tested
 */
bool isValid(E)(const(E)[] s)
{
    return s.length == validLength(s);
}

///
@system pure unittest
{
    assert( isValid("\u20AC100"));
    assert(!isValid(cast(char[3])[167, 133, 175]));
}

/**
 Returns the length of the longest possible substring, starting from
 the first code unit, which is validly encoded.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    s = the string to be tested
 */
size_t validLength(E)(const(E)[] s)
{
    size_t result, before = void;
    while ((before = s.length) > 0)
    {
        if (EncoderInstance!(E).safeDecode(s) == INVALID_SEQUENCE)
            break;
        result += before - s.length;
    }
    return result;
}

/**
 Sanitizes a string by replacing malformed code unit sequences with valid
 code unit sequences. The result is guaranteed to be valid for this encoding.

 If the input string is already valid, this function returns the original,
 otherwise it constructs a new string by replacing all illegal code unit
 sequences with the encoding's replacement character, Invalid sequences will
 be replaced with the Unicode replacement character (U+FFFD) if the
 character repertoire contains it, otherwise invalid sequences will be
 replaced with '?'.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    s = the string to be sanitized
 */
immutable(E)[] sanitize(E)(immutable(E)[] s)
{
    size_t n = validLength(s);
    if (n == s.length) return s;

    auto repSeq = EncoderInstance!(E).replacementSequence;

    // Count how long the string needs to be.
    // Overestimating is not a problem
    size_t len = s.length;
    const(E)[] t = s[n..$];
    while (t.length != 0)
    {
        immutable c = EncoderInstance!(E).safeDecode(t);
        assert(c == INVALID_SEQUENCE);
        len += repSeq.length;
        t = t[validLength(t)..$];
    }

    // Now do the write
    E[] array = new E[len];
    array[0 .. n] = s[0 .. n];
    size_t offset = n;

    t = s[n..$];
    while (t.length != 0)
    {
        immutable c = EncoderInstance!(E).safeDecode(t);
        assert(c == INVALID_SEQUENCE);
        array[offset .. offset+repSeq.length] = repSeq[];
        offset += repSeq.length;
        n = validLength(t);
        array[offset .. offset+n] = t[0 .. n];
        offset += n;
        t = t[n..$];
    }
    return cast(immutable(E)[])array[0 .. offset];
}

///
@system pure unittest
{
    assert(sanitize("hello \xF0\x80world") == "hello \xEF\xBF\xBDworld");
}

/**
 Returns the length of the first encoded sequence.

 The input to this function MUST be validly encoded.
 This is enforced by the function's in-contract.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
 s = the string to be sliced
 */
size_t firstSequence(E)(const(E)[] s)
in
{
    assert(s.length != 0);
    const(E)[] u = s;
    assert(safeDecode(u) != INVALID_SEQUENCE);
}
body
{
    auto before = s.length;
    EncoderInstance!(E).skip(s);
    return before - s.length;
}

///
@system pure unittest
{
    assert(firstSequence("\u20AC1000") == "\u20AC".length);
    assert(firstSequence("hel") == "h".length);
}

/**
 Returns the length of the last encoded sequence.

 The input to this function MUST be validly encoded.
 This is enforced by the function's in-contract.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    s = the string to be sliced
 */
size_t lastSequence(E)(const(E)[] s)
in
{
    assert(s.length != 0);
    assert(isValid(s));
}
body
{
    const(E)[] t = s;
    EncoderInstance!(E).decodeReverse(s);
    return t.length - s.length;
}

///
@system pure unittest
{
    assert(lastSequence("1000\u20AC") == "\u20AC".length);
    assert(lastSequence("hellö") == "ö".length);
}

/**
 Returns the array index at which the (n+1)th code point begins.

 The input to this function MUST be validly encoded.
 This is enforced by the function's in-contract.

 Supersedes:
 This function supersedes std.utf.toUTFindex().

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    s = the string to be counted
    n = the current code point index
 */
ptrdiff_t index(E)(const(E)[] s,int n)
in
{
    assert(isValid(s));
    assert(n >= 0);
}
body
{
    const(E)[] t = s;
    for (size_t i=0; i<n; ++i) EncoderInstance!(E).skip(s);
    return t.length - s.length;
}

///
@system pure unittest
{
    assert(index("\u20AC100",1) == 3);
    assert(index("hällo",2) == 3);
}

/**
 Decodes a single code point.

 This function removes one or more code units from the start of a string,
 and returns the decoded code point which those code units represent.

 The input to this function MUST be validly encoded.
 This is enforced by the function's in-contract.

 Supersedes:
 This function supersedes std.utf.decode(), however, note that the
 function codePoints() supersedes it more conveniently.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    s = the string whose first code point is to be decoded
 */
dchar decode(S)(ref S s)
in
{
    assert(s.length != 0);
    auto u = s;
    assert(safeDecode(u) != INVALID_SEQUENCE);
}
body
{
    return EncoderInstance!(typeof(s[0])).decode(s);
}

/**
 Decodes a single code point from the end of a string.

 This function removes one or more code units from the end of a string,
 and returns the decoded code point which those code units represent.

 The input to this function MUST be validly encoded.
 This is enforced by the function's in-contract.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    s = the string whose first code point is to be decoded
 */
dchar decodeReverse(E)(ref const(E)[] s)
in
{
    assert(s.length != 0);
    assert(isValid(s));
}
body
{
    return EncoderInstance!(E).decodeReverse(s);
}

/**
 Decodes a single code point. The input does not have to be valid.

 This function removes one or more code units from the start of a string,
 and returns the decoded code point which those code units represent.

 This function will accept an invalidly encoded string as input.
 If an invalid sequence is found at the start of the string, this
 function will remove it, and return the value INVALID_SEQUENCE.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    s = the string whose first code point is to be decoded
 */
dchar safeDecode(S)(ref S s)
in
{
    assert(s.length != 0);
}
body
{
    return EncoderInstance!(typeof(s[0])).safeDecode(s);
}

/**
 Returns the number of code units required to encode a single code point.

 The input to this function MUST be a valid code point.
 This is enforced by the function's in-contract.

 The type of the output cannot be deduced. Therefore, it is necessary to
 explicitly specify the encoding as a template parameter.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    c = the code point to be encoded
 */
size_t encodedLength(E)(dchar c)
in
{
    assert(isValidCodePoint(c));
}
body
{
    return EncoderInstance!(E).encodedLength(c);
}

/**
 Encodes a single code point.

 This function encodes a single code point into one or more code units.
 It returns a string containing those code units.

 The input to this function MUST be a valid code point.
 This is enforced by the function's in-contract.

 The type of the output cannot be deduced. Therefore, it is necessary to
 explicitly specify the encoding as a template parameter.

 Supersedes:
 This function supersedes std.utf.encode(), however, note that the
 function codeUnits() supersedes it more conveniently.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    c = the code point to be encoded
 */
E[] encode(E)(dchar c)
in
{
    assert(isValidCodePoint(c));
}
body
{
    return EncoderInstance!(E).encode(c);
}

/**
 Encodes a single code point into an array.

 This function encodes a single code point into one or more code units
 The code units are stored in a user-supplied fixed-size array,
 which must be passed by reference.

 The input to this function MUST be a valid code point.
 This is enforced by the function's in-contract.

 The type of the output cannot be deduced. Therefore, it is necessary to
 explicitly specify the encoding as a template parameter.

 Supersedes:
 This function supersedes std.utf.encode(), however, note that the
 function codeUnits() supersedes it more conveniently.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    c     = the code point to be encoded
    array = the destination array

 Returns:
          the number of code units written to the array
 */
size_t encode(E)(dchar c, E[] array)
in
{
    assert(isValidCodePoint(c));
}
body
{
    E[] t = array;
    EncoderInstance!(E).encode(c,t);
    return array.length - t.length;
}

/*
Encodes $(D c) in units of type $(D E) and writes the result to the
output range $(D R). Returns the number of $(D E)s written.
 */
size_t encode(E, R)(dchar c, auto ref R range)
if (isNativeOutputRange!(R, E))
{
    static if (is(Unqual!E == char))
    {
        if (c <= 0x7F)
        {
            put(range, cast(char) c);
            return 1;
        }
        if (c <= 0x7FF)
        {
            put(range, cast(char)(0xC0 | (c >> 6)));
            put(range, cast(char)(0x80 | (c & 0x3F)));
            return 2;
        }
        if (c <= 0xFFFF)
        {
            put(range, cast(char)(0xE0 | (c >> 12)));
            put(range, cast(char)(0x80 | ((c >> 6) & 0x3F)));
            put(range, cast(char)(0x80 | (c & 0x3F)));
            return 3;
        }
        if (c <= 0x10FFFF)
        {
            put(range, cast(char)(0xF0 | (c >> 18)));
            put(range, cast(char)(0x80 | ((c >> 12) & 0x3F)));
            put(range, cast(char)(0x80 | ((c >> 6) & 0x3F)));
            put(range, cast(char)(0x80 | (c & 0x3F)));
            return 4;
        }
        else
        {
            assert(0);
        }
    }
    else static if (is(Unqual!E == wchar))
    {
        if (c <= 0xFFFF)
        {
            range.put(cast(wchar) c);
            return 1;
        }
        range.put(cast(wchar) ((((c - 0x10000) >> 10) & 0x3FF) + 0xD800));
        range.put(cast(wchar) (((c - 0x10000) & 0x3FF) + 0xDC00));
        return 2;
    }
    else static if (is(Unqual!E == dchar))
    {
        range.put(c);
        return 1;
    }
    else
    {
        static assert(0);
    }
}

@safe pure unittest
{
    import std.array;
    Appender!(char[]) r;
    assert(encode!(char)('T', r) == 1);
    assert(encode!(wchar)('T', r) == 1);
    assert(encode!(dchar)('T', r) == 1);
}

/**
 Encodes a single code point to a delegate.

 This function encodes a single code point into one or more code units.
 The code units are passed one at a time to the supplied delegate.

 The input to this function MUST be a valid code point.
 This is enforced by the function's in-contract.

 The type of the output cannot be deduced. Therefore, it is necessary to
 explicitly specify the encoding as a template parameter.

 Supersedes:
 This function supersedes std.utf.encode(), however, note that the
 function codeUnits() supersedes it more conveniently.

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    c  = the code point to be encoded
    dg = the delegate to invoke for each code unit
 */
void encode(E)(dchar c, void delegate(E) dg)
in
{
    assert(isValidCodePoint(c));
}
body
{
    EncoderInstance!(E).encode(c,dg);
}

/**
Encodes the contents of $(D s) in units of type $(D Tgt), writing the result to an
output range.

Returns: The number of $(D Tgt) elements written.
Params:
Tgt = Element type of $(D range).
s = Input array.
range = Output range.
 */
size_t encode(Tgt, Src, R)(in Src[] s, R range)
{
    size_t result;
    foreach (c; s)
    {
        result += encode!(Tgt)(c, range);
    }
    return result;
}

/**
 Returns a foreachable struct which can bidirectionally iterate over all
 code points in a string.

 The input to this function MUST be validly encoded.
 This is enforced by the function's in-contract.

 You can foreach either
 with or without an index. If an index is specified, it will be initialized
 at each iteration with the offset into the string at which the code point
 begins.

 Supersedes:
 This function supersedes std.utf.decode().

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    s = the string to be decoded

 Example:
 --------------------------------------------------------
 string s = "hello world";
 foreach (c;codePoints(s))
 {
     // do something with c (which will always be a dchar)
 }
 --------------------------------------------------------

 Note that, currently, foreach (c:codePoints(s)) is superior to foreach (c;s)
 in that the latter will fall over on encountering U+FFFF.
 */
CodePoints!(E) codePoints(E)(immutable(E)[] s)
in
{
    assert(isValid(s));
}
body
{
    return CodePoints!(E)(s);
}

///
@system unittest
{
    string s = "hello";
    string t;
    foreach (c;codePoints(s))
    {
        t ~= cast(char) c;
    }
    assert(s == t);
}

/**
 Returns a foreachable struct which can bidirectionally iterate over all
 code units in a code point.

 The input to this function MUST be a valid code point.
 This is enforced by the function's in-contract.

 The type of the output cannot be deduced. Therefore, it is necessary to
 explicitly specify the encoding type in the template parameter.

 Supersedes:
 This function supersedes std.utf.encode().

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    c = the code point to be encoded
 */
CodeUnits!(E) codeUnits(E)(dchar c)
in
{
    assert(isValidCodePoint(c));
}
body
{
    return CodeUnits!(E)(c);
}

///
@system unittest
{
    char[] a;
    foreach (c;codeUnits!(char)(cast(dchar)'\u20AC'))
    {
        a ~= c;
    }
    assert(a.length == 3);
    assert(a[0] == 0xE2);
    assert(a[1] == 0x82);
    assert(a[2] == 0xAC);
}

/**
 Convert a string from one encoding to another.

 Supersedes:
 This function supersedes std.utf.toUTF8(), std.utf.toUTF16() and
 std.utf.toUTF32()
 (but note that to!() supersedes it more conveniently).

 Standards: Unicode 5.0, ASCII, ISO-8859-1, ISO-8859-2, WINDOWS-1250,
 WINDOWS-1252

 Params:
    s = Source string. $(B Must) be validly encoded.
        This is enforced by the function's in-contract.
    r = Destination string

 See_Also:
    $(REF to, std,conv)
 */
void transcode(Src, Dst)(Src[] s, out Dst[] r)
in
{
    assert(isValid(s));
}
body
{
    static if (is(Src == Dst) && is(Src == immutable))
    {
        r = s;
    }
    else static if (is(Unqual!Src == AsciiChar))
    {
        transcode(cast(const(char)[])s, r);
    }
    else
    {
        static if (is(Unqual!Dst == wchar))
        {
            immutable minReservePlace = 2;
        }
        else static if (is(Unqual!Dst == dchar))
        {
            immutable minReservePlace = 1;
        }
        else
        {
            immutable minReservePlace = 6;
        }

        auto buffer = new Unqual!Dst[s.length];
        auto tmpBuffer = buffer;

        while (s.length != 0)
        {
            if (tmpBuffer.length < minReservePlace)
            {
                size_t prevLength = buffer.length;
                buffer.length += s.length + minReservePlace;
                tmpBuffer = buffer[prevLength - tmpBuffer.length .. $];
            }
            EncoderInstance!(Unqual!Dst).encode(decode(s), tmpBuffer);
        }

        r = cast(Dst[]) buffer[0 .. buffer.length - tmpBuffer.length];
    }
}

///
@system pure unittest
{
    wstring ws;
    // transcode from UTF-8 to UTF-16
    transcode("hello world",ws);
    assert(ws == "hello world"w);

    Latin1String ls;
    // transcode from UTF-16 to ISO-8859-1
    transcode(ws, ls);
    assert(ws == "hello world");
}

@system pure unittest
{
    import std.meta;
    import std.range;
    {
        import std.conv : to;

        string asciiCharString = to!string(iota(0, 128, 1));

        alias Types = AliasSeq!(string, Latin1String, Latin2String, AsciiString,
            Windows1250String, Windows1252String, dstring, wstring);
        foreach (S; Types)
            foreach (D; Types)
            {
                string str;
                S sStr;
                D dStr;
                transcode(asciiCharString, sStr);
                transcode(sStr, dStr);
                transcode(dStr, str);
                assert(asciiCharString == str);
            }
    }
    {
        string czechChars = "Příliš žluťoučký kůň úpěl ďábelské ódy.";
        alias Types = AliasSeq!(string, dstring, wstring);
        foreach (S; Types)
            foreach (D; Types)
            {
                string str;
                S sStr;
                D dStr;
                transcode(czechChars, sStr);
                transcode(sStr, dStr);
                transcode(dStr, str);
                assert(czechChars == str);
            }
    }
}

@system unittest // mutable/const input/output
{
    import std.meta : AliasSeq;

    foreach (O; AliasSeq!(Latin1Char, const Latin1Char, immutable Latin1Char))
    {
        O[] output;

        char[] mutableInput = "äbc".dup;
        transcode(mutableInput, output);
        assert(output == [0xE4, 'b', 'c']);

        const char[] constInput = "öbc";
        transcode(constInput, output);
        assert(output == [0xF6, 'b', 'c']);

        immutable char[] immutInput = "übc";
        transcode(immutInput, output);
        assert(output == [0xFC, 'b', 'c']);
    }

    // Make sure that const/mutable input is copied.
    foreach (C; AliasSeq!(char, const char))
    {
        C[] input = "foo".dup;
        C[] output;
        transcode(input, output);
        assert(input == output);
        assert(input !is output);
    }

    // But immutable input should not be copied.
    string input = "foo";
    string output;
    transcode(input, output);
    assert(input is output);
}

//=============================================================================

/** The base class for exceptions thrown by this module */
class EncodingException : Exception { this(string msg) @safe pure { super(msg); } }

class UnrecognizedEncodingException : EncodingException
{
    private this(string msg) @safe pure { super(msg); }
}

/** Abstract base class of all encoding schemes */
abstract class EncodingScheme
{
    import std.uni : toLower;

    /**
     * Registers a subclass of EncodingScheme.
     *
     * This function allows user-defined subclasses of EncodingScheme to
     * be declared in other modules.
     *
     * Params:
     *     Klass = The subclass of EncodingScheme to register.
     *
     * Example:
     * ----------------------------------------------
     * class Amiga1251 : EncodingScheme
     * {
     *     shared static this()
     *     {
     *         EncodingScheme.register!Amiga1251;
     *     }
     * }
     * ----------------------------------------------
     */
    static void register(Klass:EncodingScheme)()
    {
        scope scheme = new Klass();
        foreach (encodingName;scheme.names())
        {
            supported[toLower(encodingName)] = () => new Klass();
        }
    }

    deprecated("Please pass the EncodingScheme subclass as template argument instead.")
    static void register(string className)
    {
        auto scheme = cast(EncodingScheme) ClassInfo.find(className).create();
        if (scheme is null)
            throw new EncodingException("Unable to create class "~className);
        foreach (encodingName;scheme.names())
        {
            supportedFactories[toLower(encodingName)] = className;
        }
    }

    /**
     * Obtains a subclass of EncodingScheme which is capable of encoding
     * and decoding the named encoding scheme.
     *
     * This function is only aware of EncodingSchemes which have been
     * registered with the register() function.
     *
     * Example:
     * ---------------------------------------------------
     * auto scheme = EncodingScheme.create("Amiga-1251");
     * ---------------------------------------------------
     */
    static EncodingScheme create(string encodingName)
    {
        static bool registerDefaultEncodings()
        {
            EncodingScheme.register!EncodingSchemeASCII;
            EncodingScheme.register!EncodingSchemeLatin1;
            EncodingScheme.register!EncodingSchemeLatin2;
            EncodingScheme.register!EncodingSchemeWindows1250;
            EncodingScheme.register!EncodingSchemeWindows1252;
            EncodingScheme.register!EncodingSchemeUtf8;
            EncodingScheme.register!EncodingSchemeUtf16Native;
            EncodingScheme.register!EncodingSchemeUtf32Native;
            return true;
        }

        static shared bool initialized;
        import std.concurrency : initOnce;
        initOnce!initialized(registerDefaultEncodings());
        encodingName = toLower(encodingName);

        if (auto p = encodingName in supported)
            return (*p)();

        auto p = encodingName in supportedFactories;
        if (p is null)
            throw new EncodingException("Unrecognized Encoding: "~encodingName);
        string className = *p;
        auto scheme = cast(EncodingScheme) ClassInfo.find(className).create();
        if (scheme is null) throw new EncodingException("Unable to create class "~className);
        return scheme;
    }

    const
    {
        /**
         * Returns the standard name of the encoding scheme
         */
        abstract override string toString();

        /**
         * Returns an array of all known names for this encoding scheme
         */
        abstract string[] names();

        /**
         * Returns true if the character c can be represented
         * in this encoding scheme.
         */
        abstract bool canEncode(dchar c);

        /**
         * Returns the number of ubytes required to encode this code point.
         *
         * The input to this function MUST be a valid code point.
         *
         * Params:
         *    c = the code point to be encoded
         *
         * Returns:
         *    the number of ubytes required.
         */
        abstract size_t encodedLength(dchar c);

        /**
         * Encodes a single code point into a user-supplied, fixed-size buffer.
         *
         * This function encodes a single code point into one or more ubytes.
         * The supplied buffer must be code unit aligned.
         * (For example, UTF-16LE or UTF-16BE must be wchar-aligned,
         * UTF-32LE or UTF-32BE must be dchar-aligned, etc.)
         *
         * The input to this function MUST be a valid code point.
         *
         * Params:
         *    c      = the code point to be encoded
         *    buffer = the destination array
         *
         * Returns:
         *    the number of ubytes written.
         */
        abstract size_t encode(dchar c, ubyte[] buffer);

        /**
         * Decodes a single code point.
         *
         * This function removes one or more ubytes from the start of an array,
         * and returns the decoded code point which those ubytes represent.
         *
         * The input to this function MUST be validly encoded.
         *
         * Params:
         *    s = the array whose first code point is to be decoded
         */
        abstract dchar decode(ref const(ubyte)[] s);

        /**
         * Decodes a single code point. The input does not have to be valid.
         *
         * This function removes one or more ubytes from the start of an array,
         * and returns the decoded code point which those ubytes represent.
         *
         * This function will accept an invalidly encoded array as input.
         * If an invalid sequence is found at the start of the string, this
         * function will remove it, and return the value INVALID_SEQUENCE.
         *
         * Params:
         *    s = the array whose first code point is to be decoded
         */
        abstract dchar safeDecode(ref const(ubyte)[] s);

        /**
         * Returns the sequence of ubytes to be used to represent
         * any character which cannot be represented in the encoding scheme.
         *
         * Normally this will be a representation of some substitution
         * character, such as U+FFFD or '?'.
         */
        abstract @property immutable(ubyte)[] replacementSequence();
    }

    /**
     * Returns true if the array is encoded correctly
     *
     * Params:
     *    s = the array to be tested
     */
    bool isValid(const(ubyte)[] s)
    {
        while (s.length != 0)
        {
            if (safeDecode(s) == INVALID_SEQUENCE)
                return false;
        }
        return true;
    }

    /**
     * Returns the length of the longest possible substring, starting from
     * the first element, which is validly encoded.
     *
     * Params:
     *    s = the array to be tested
     */
    size_t validLength()(const(ubyte)[] s)
    {
        const(ubyte)[] r = s;
        const(ubyte)[] t = s;
        while (s.length != 0)
        {
            if (safeDecode(s) == INVALID_SEQUENCE) break;
            t = s;
        }
        return r.length - t.length;
    }

    /**
     * Sanitizes an array by replacing malformed ubyte sequences with valid
     * ubyte sequences. The result is guaranteed to be valid for this
     * encoding scheme.
     *
     * If the input array is already valid, this function returns the
     * original, otherwise it constructs a new array by replacing all illegal
     * sequences with the encoding scheme's replacement sequence.
     *
     * Params:
     *    s = the string to be sanitized
     */
    immutable(ubyte)[] sanitize()(immutable(ubyte)[] s)
    {
        auto n = validLength(s);
        if (n == s.length) return s;

        auto repSeq = replacementSequence;

        // Count how long the string needs to be.
        // Overestimating is not a problem
        auto len = s.length;
        const(ubyte)[] t = s[n..$];
        while (t.length != 0)
        {
            immutable c = safeDecode(t);
            assert(c == INVALID_SEQUENCE);
            len += repSeq.length;
            t = t[validLength(t)..$];
        }

        // Now do the write
        ubyte[] array = new ubyte[len];
        array[0 .. n] = s[0 .. n];
        auto offset = n;

        t = s[n..$];
        while (t.length != 0)
        {
            immutable c = safeDecode(t);
            assert(c == INVALID_SEQUENCE);
            array[offset .. offset+repSeq.length] = repSeq[];
            offset += repSeq.length;
            n = validLength(t);
            array[offset .. offset+n] = t[0 .. n];
            offset += n;
            t = t[n..$];
        }
        return cast(immutable(ubyte)[])array[0 .. offset];
    }

    /**
     * Returns the length of the first encoded sequence.
     *
     * The input to this function MUST be validly encoded.
     * This is enforced by the function's in-contract.
     *
     * Params:
     *    s = the array to be sliced
     */
    size_t firstSequence()(const(ubyte)[] s)
    in
    {
        assert(s.length != 0);
        const(ubyte)[] u = s;
        assert(safeDecode(u) != INVALID_SEQUENCE);
    }
    body
    {
        const(ubyte)[] t = s;
        decode(s);
        return t.length - s.length;
    }

    /**
     * Returns the total number of code points encoded in a ubyte array.
     *
     * The input to this function MUST be validly encoded.
     * This is enforced by the function's in-contract.
     *
     * Params:
     *    s = the string to be counted
     */
    size_t count()(const(ubyte)[] s)
    in
    {
        assert(isValid(s));
    }
    body
    {
        size_t n = 0;
        while (s.length != 0)
        {
            decode(s);
            ++n;
        }
        return n;
    }

    /**
     * Returns the array index at which the (n+1)th code point begins.
     *
     * The input to this function MUST be validly encoded.
     * This is enforced by the function's in-contract.
     *
     * Params:
     *    s = the string to be counted
     *    n = the current code point index
     */
    ptrdiff_t index()(const(ubyte)[] s, size_t n)
    in
    {
        assert(isValid(s));
        assert(n >= 0);
    }
    body
    {
        const(ubyte)[] t = s;
        for (size_t i=0; i<n; ++i) decode(s);
        return t.length - s.length;
    }

    __gshared EncodingScheme function()[string] supported;
    __gshared string[string] supportedFactories;
}

/**
 EncodingScheme to handle ASCII

 This scheme recognises the following names:
                 "ANSI_X3.4-1968",
                 "ANSI_X3.4-1986",
                 "ASCII",
                 "IBM367",
                 "ISO646-US",
                 "ISO_646.irv:1991",
                 "US-ASCII",
                 "cp367",
                 "csASCII"
                 "iso-ir-6",
                 "us"
 */
class EncodingSchemeASCII : EncodingScheme
{
    /* // moved to std.internal.phobosinit
    shared static this()
    {
        EncodingScheme.register("std.encoding.EncodingSchemeASCII");
    }*/

    const
    {
        override string[] names() @safe pure nothrow
        {
            return
            [
                "ANSI_X3.4-1968",
                "ANSI_X3.4-1986",
                "ASCII",
                "IBM367",
                "ISO646-US",
                "ISO_646.irv:1991",
                "US-ASCII",
                "cp367",
                "csASCII",
                "iso-ir-6",
                "us"
            ];
        }

        override string toString() @safe pure nothrow @nogc
        {
            return "ASCII";
        }

        override bool canEncode(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.canEncode!(AsciiChar)(c);
        }

        override size_t encodedLength(dchar c)  @safe pure nothrow @nogc
        {
            return std.encoding.encodedLength!(AsciiChar)(c);
        }

        override size_t encode(dchar c, ubyte[] buffer) @safe pure nothrow @nogc
        {
            auto r = cast(AsciiChar[]) buffer;
            return std.encoding.encode(c,r);
        }

        override dchar decode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        {
            auto t = cast(const(AsciiChar)[]) s;
            dchar c = std.encoding.decode(t);
            s = s[$-t.length..$];
            return c;
        }

        override dchar safeDecode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        {
            auto t = cast(const(AsciiChar)[]) s;
            dchar c = std.encoding.safeDecode(t);
            s = s[$-t.length..$];
            return c;
        }

        override @property immutable(ubyte)[] replacementSequence() @safe pure nothrow @nogc
        {
            return cast(immutable(ubyte)[])"?";
        }
    }
}

/**
 EncodingScheme to handle Latin-1

 This scheme recognises the following names:
                 "CP819",
                 "IBM819",
                 "ISO-8859-1",
                 "ISO_8859-1",
                 "ISO_8859-1:1987",
                 "csISOLatin1",
                 "iso-ir-100",
                 "l1",
                 "latin1"
 */
class EncodingSchemeLatin1 : EncodingScheme
{
    /* // moved to std.internal.phobosinit
    shared static this()
    {
        EncodingScheme.register("std.encoding.EncodingSchemeLatin1");
    }*/

    const
    {
        override string[] names() @safe pure nothrow
        {
            return
            [
                "CP819",
                "IBM819",
                "ISO-8859-1",
                "ISO_8859-1",
                "ISO_8859-1:1987",
                "csISOLatin1",
                "iso-ir-100",
                "l1",
                "latin1"
            ];
        }

        override string toString() @safe pure nothrow @nogc
        {
            return "ISO-8859-1";
        }

        override bool canEncode(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.canEncode!(Latin1Char)(c);
        }

        override size_t encodedLength(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.encodedLength!(Latin1Char)(c);
        }

        override size_t encode(dchar c, ubyte[] buffer) @safe pure nothrow @nogc
        {
            auto r = cast(Latin1Char[]) buffer;
            return std.encoding.encode(c,r);
        }

        override dchar decode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        {
            auto t = cast(const(Latin1Char)[]) s;
            dchar c = std.encoding.decode(t);
            s = s[$-t.length..$];
            return c;
        }

        override dchar safeDecode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        {
            auto t = cast(const(Latin1Char)[]) s;
            dchar c = std.encoding.safeDecode(t);
            s = s[$-t.length..$];
            return c;
        }

        override @property immutable(ubyte)[] replacementSequence() @safe pure nothrow @nogc
        {
            return cast(immutable(ubyte)[])"?";
        }
    }
}

/**
 EncodingScheme to handle Latin-2

 This scheme recognises the following names:
                 "Latin 2",
                 "ISO-8859-2",
                 "ISO_8859-2",
                 "ISO_8859-2:1999",
                 "Windows-28592"
 */
class EncodingSchemeLatin2 : EncodingScheme
{
    /* // moved to std.internal.phobosinit
    shared static this()
    {
        EncodingScheme.register("std.encoding.EncodingSchemeLatin2");
    }*/

    const
    {
        override string[] names() @safe pure nothrow
        {
            return
            [
                "Latin 2",
                "ISO-8859-2",
                "ISO_8859-2",
                "ISO_8859-2:1999",
                "windows-28592"
            ];
        }

        override string toString() @safe pure nothrow @nogc
        {
            return "ISO-8859-2";
        }

        override bool canEncode(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.canEncode!(Latin2Char)(c);
        }

        override size_t encodedLength(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.encodedLength!(Latin2Char)(c);
        }

        override size_t encode(dchar c, ubyte[] buffer) @safe pure nothrow @nogc
        {
            auto r = cast(Latin2Char[]) buffer;
            return std.encoding.encode(c,r);
        }

        override dchar decode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        {
            auto t = cast(const(Latin2Char)[]) s;
            dchar c = std.encoding.decode(t);
            s = s[$-t.length..$];
            return c;
        }

        override dchar safeDecode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        {
            auto t = cast(const(Latin2Char)[]) s;
            dchar c = std.encoding.safeDecode(t);
            s = s[$-t.length..$];
            return c;
        }

        override @property immutable(ubyte)[] replacementSequence() @safe pure nothrow @nogc
        {
            return cast(immutable(ubyte)[])"?";
        }
    }
}

/**
 EncodingScheme to handle Windows-1250

 This scheme recognises the following names:
                 "windows-1250"
 */
class EncodingSchemeWindows1250 : EncodingScheme
{
    /* // moved to std.internal.phobosinit
    shared static this()
    {
        EncodingScheme.register("std.encoding.EncodingSchemeWindows1250");
    }*/

    const
    {
        override string[] names() @safe pure nothrow
        {
            return
            [
                "windows-1250"
            ];
        }

        override string toString() @safe pure nothrow @nogc
        {
            return "windows-1250";
        }

        override bool canEncode(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.canEncode!(Windows1250Char)(c);
        }

        override size_t encodedLength(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.encodedLength!(Windows1250Char)(c);
        }

        override size_t encode(dchar c, ubyte[] buffer) @safe pure nothrow @nogc
        {
            auto r = cast(Windows1250Char[]) buffer;
            return std.encoding.encode(c,r);
        }

        override dchar decode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        {
            auto t = cast(const(Windows1250Char)[]) s;
            dchar c = std.encoding.decode(t);
            s = s[$-t.length..$];
            return c;
        }

        override dchar safeDecode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        {
            auto t = cast(const(Windows1250Char)[]) s;
            dchar c = std.encoding.safeDecode(t);
            s = s[$-t.length..$];
            return c;
        }

        override @property immutable(ubyte)[] replacementSequence() @safe pure nothrow @nogc
        {
            return cast(immutable(ubyte)[])"?";
        }
    }
}

/**
 EncodingScheme to handle Windows-1252

 This scheme recognises the following names:
                 "windows-1252"
 */
class EncodingSchemeWindows1252 : EncodingScheme
{
    /* // moved to std.internal.phobosinit
    shared static this()
    {
        EncodingScheme.register("std.encoding.EncodingSchemeWindows1252");
    }*/

    const
    {
        override string[] names() @safe pure nothrow
        {
            return
            [
                "windows-1252"
            ];
        }

        override string toString() @safe pure nothrow @nogc
        {
            return "windows-1252";
        }

        override bool canEncode(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.canEncode!(Windows1252Char)(c);
        }

        override size_t encodedLength(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.encodedLength!(Windows1252Char)(c);
        }

        override size_t encode(dchar c, ubyte[] buffer) @safe pure nothrow @nogc
        {
            auto r = cast(Windows1252Char[]) buffer;
            return std.encoding.encode(c,r);
        }

        override dchar decode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        {
            auto t = cast(const(Windows1252Char)[]) s;
            dchar c = std.encoding.decode(t);
            s = s[$-t.length..$];
            return c;
        }

        override dchar safeDecode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        {
            auto t = cast(const(Windows1252Char)[]) s;
            dchar c = std.encoding.safeDecode(t);
            s = s[$-t.length..$];
            return c;
        }

        override @property immutable(ubyte)[] replacementSequence() @safe pure nothrow @nogc
        {
            return cast(immutable(ubyte)[])"?";
        }
    }
}

/**
 EncodingScheme to handle UTF-8

 This scheme recognises the following names:
                 "UTF-8"
 */
class EncodingSchemeUtf8 : EncodingScheme
{
    /* // moved to std.internal.phobosinit
    shared static this()
    {
        EncodingScheme.register("std.encoding.EncodingSchemeUtf8");
    }*/

    const
    {
        override string[] names() @safe pure nothrow
        {
            return
            [
                "UTF-8"
            ];
        }

        override string toString() @safe pure nothrow @nogc
        {
            return "UTF-8";
        }

        override bool canEncode(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.canEncode!(char)(c);
        }

        override size_t encodedLength(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.encodedLength!(char)(c);
        }

        override size_t encode(dchar c, ubyte[] buffer) @safe pure nothrow @nogc
        {
            auto r = cast(char[]) buffer;
            return std.encoding.encode(c,r);
        }

        override dchar decode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        {
            auto t = cast(const(char)[]) s;
            dchar c = std.encoding.decode(t);
            s = s[$-t.length..$];
            return c;
        }

        override dchar safeDecode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        {
            auto t = cast(const(char)[]) s;
            dchar c = std.encoding.safeDecode(t);
            s = s[$-t.length..$];
            return c;
        }

        override @property immutable(ubyte)[] replacementSequence() @safe pure nothrow @nogc
        {
            return cast(immutable(ubyte)[])"\uFFFD";
        }
    }
}

/**
 EncodingScheme to handle UTF-16 in native byte order

 This scheme recognises the following names:
                 "UTF-16LE" (little-endian architecture only)
                 "UTF-16BE" (big-endian architecture only)
 */
class EncodingSchemeUtf16Native : EncodingScheme
{
    /* // moved to std.internal.phobosinit
    shared static this()
    {
        EncodingScheme.register("std.encoding.EncodingSchemeUtf16Native");
    }*/

    const
    {
        version (LittleEndian) { enum string NAME = "UTF-16LE"; }
        version (BigEndian)    { enum string NAME = "UTF-16BE"; }

        override string[] names() @safe pure nothrow
        {
            return [ NAME ];
        }

        override string toString() @safe pure nothrow @nogc
        {
            return NAME;
        }

        override bool canEncode(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.canEncode!(wchar)(c);
        }

        override size_t encodedLength(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.encodedLength!(wchar)(c);
        }

        override size_t encode(dchar c, ubyte[] buffer) @safe pure nothrow @nogc
        {
            auto r = cast(wchar[]) buffer;
            return wchar.sizeof * std.encoding.encode(c,r);
        }

        override dchar decode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        in
        {
            assert((s.length & 1) == 0);
        }
        body
        {
            auto t = cast(const(wchar)[]) s;
            dchar c = std.encoding.decode(t);
            s = s[$-t.length * wchar.sizeof..$];
            return c;
        }

        override dchar safeDecode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        in
        {
            assert((s.length & 1) == 0);
        }
        body
        {
            auto t = cast(const(wchar)[]) s;
            dchar c = std.encoding.safeDecode(t);
            s = s[$-t.length * wchar.sizeof..$];
            return c;
        }

        override @property immutable(ubyte)[] replacementSequence() @safe pure nothrow @nogc
        {
            return cast(immutable(ubyte)[])"\uFFFD"w;
        }
    }
}
@system unittest
{
    version (LittleEndian)
    {
        auto efrom = EncodingScheme.create("utf-16le");
        ubyte[6] sample = [154,1, 155,1, 156,1];
    }
    version (BigEndian)
    {
        auto efrom = EncodingScheme.create("utf-16be");
        ubyte[6] sample = [1,154, 1,155, 1,156];
    }
    const(ubyte)[] ub = cast(const(ubyte)[])sample;
    dchar dc = efrom.safeDecode(ub);
    assert(dc == 410);
    assert(ub.length == 4);
}

/**
 EncodingScheme to handle UTF-32 in native byte order

 This scheme recognises the following names:
                 "UTF-32LE" (little-endian architecture only)
                 "UTF-32BE" (big-endian architecture only)
 */
class EncodingSchemeUtf32Native : EncodingScheme
{
    /* // moved to std.internal.phobosinit
    shared static this()
    {
        EncodingScheme.register("std.encoding.EncodingSchemeUtf32Native");
    }*/

    const
    {
        version (LittleEndian) { enum string NAME = "UTF-32LE"; }
        version (BigEndian)    { enum string NAME = "UTF-32BE"; }

        override string[] names() @safe pure nothrow
        {
            return [ NAME ];
        }

        override string toString() @safe pure nothrow @nogc
        {
            return NAME;
        }

        override bool canEncode(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.canEncode!(dchar)(c);
        }

        override size_t encodedLength(dchar c) @safe pure nothrow @nogc
        {
            return std.encoding.encodedLength!(dchar)(c);
        }

        override size_t encode(dchar c, ubyte[] buffer) @safe pure nothrow @nogc
        {
            auto r = cast(dchar[]) buffer;
            return dchar.sizeof * std.encoding.encode(c,r);
        }

        override dchar decode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        in
        {
            assert((s.length & 3) == 0);
        }
        body
        {
            auto t = cast(const(dchar)[]) s;
            dchar c = std.encoding.decode(t);
            s = s[$-t.length * dchar.sizeof..$];
            return c;
        }

        override dchar safeDecode(ref const(ubyte)[] s) @safe pure nothrow @nogc
        in
        {
            assert((s.length & 3) == 0);
        }
        body
        {
            auto t = cast(const(dchar)[]) s;
            dchar c = std.encoding.safeDecode(t);
            s = s[$-t.length * dchar.sizeof..$];
            return c;
        }

        override @property immutable(ubyte)[] replacementSequence() @safe pure nothrow @nogc
        {
            return cast(immutable(ubyte)[])"\uFFFD"d;
        }
    }
}
@system unittest
{
    version (LittleEndian)
    {
        auto efrom = EncodingScheme.create("utf-32le");
        ubyte[12] sample = [154,1,0,0, 155,1,0,0, 156,1,0,0];
    }
    version (BigEndian)
    {
        auto efrom = EncodingScheme.create("utf-32be");
        ubyte[12] sample = [0,0,1,154, 0,0,1,155, 0,0,1,156];
    }
    const(ubyte)[] ub = cast(const(ubyte)[])sample;
    dchar dc = efrom.safeDecode(ub);
    assert(dc == 410);
    assert(ub.length == 8);
}

//=============================================================================


// Helper functions
version (unittest)
{
    void transcodeReverse(Src,Dst)(immutable(Src)[] s, out immutable(Dst)[] r)
    {
        static if (is(Src == Dst))
        {
            return s;
        }
        else static if (is(Src == AsciiChar))
        {
            transcodeReverse!(char,Dst)(cast(string) s,r);
        }
        else
        {
            foreach_reverse (d;codePoints(s))
            {
                foreach_reverse (c;codeUnits!(Dst)(d))
                {
                    r = c ~ r;
                }
            }
        }
    }

    string makeReadable(string s)
    {
        string r = "\"";
        foreach (char c;s)
        {
            if (c >= 0x20 && c < 0x80)
            {
                r ~= c;
            }
            else
            {
                r ~= "\\x";
                r ~= toHexDigit(c >> 4);
                r ~= toHexDigit(c);
            }
        }
        r ~= "\"";
        return r;
    }

    string makeReadable(wstring s)
    {
        string r = "\"";
        foreach (wchar c;s)
        {
            if (c >= 0x20 && c < 0x80)
            {
                r ~= cast(char) c;
            }
            else
            {
                r ~= "\\u";
                r ~= toHexDigit(c >> 12);
                r ~= toHexDigit(c >> 8);
                r ~= toHexDigit(c >> 4);
                r ~= toHexDigit(c);
            }
        }
        r ~= "\"w";
        return r;
    }

    string makeReadable(dstring s)
    {
        string r = "\"";
        foreach (dchar c; s)
        {
            if (c >= 0x20 && c < 0x80)
            {
                r ~= cast(char) c;
            }
            else if (c < 0x10000)
            {
                r ~= "\\u";
                r ~= toHexDigit(c >> 12);
                r ~= toHexDigit(c >> 8);
                r ~= toHexDigit(c >> 4);
                r ~= toHexDigit(c);
            }
            else
            {
                r ~= "\\U00";
                r ~= toHexDigit(c >> 20);
                r ~= toHexDigit(c >> 16);
                r ~= toHexDigit(c >> 12);
                r ~= toHexDigit(c >> 8);
                r ~= toHexDigit(c >> 4);
                r ~= toHexDigit(c);
            }
        }
        r ~= "\"d";
        return r;
    }

    char toHexDigit(int n)
    {
        return "0123456789ABCDEF"[n & 0xF];
    }
}

/** Definitions of common Byte Order Marks.
The elements of the $(D enum) can used as indices into $(D bomTable) to get
matching $(D BOMSeq).
*/
enum BOM
{
    none      = 0,  /// no BOM was found
    utf32be   = 1,  /// [0x00, 0x00, 0xFE, 0xFF]
    utf32le   = 2,  /// [0xFF, 0xFE, 0x00, 0x00]
    utf7      = 3,  /*  [0x2B, 0x2F, 0x76, 0x38]
                        [0x2B, 0x2F, 0x76, 0x39],
                        [0x2B, 0x2F, 0x76, 0x2B],
                        [0x2B, 0x2F, 0x76, 0x2F],
                        [0x2B, 0x2F, 0x76, 0x38, 0x2D]
                    */
    utf1      = 8,  /// [0xF7, 0x64, 0x4C]
    utfebcdic = 9,  /// [0xDD, 0x73, 0x66, 0x73]
    scsu      = 10, /// [0x0E, 0xFE, 0xFF]
    bocu1     = 11, /// [0xFB, 0xEE, 0x28]
    gb18030   = 12, /// [0x84, 0x31, 0x95, 0x33]
    utf8      = 13, /// [0xEF, 0xBB, 0xBF]
    utf16be   = 14, /// [0xFE, 0xFF]
    utf16le   = 15  /// [0xFF, 0xFE]
}

/// The type stored inside $(D bomTable).
alias BOMSeq = Tuple!(BOM, "schema", ubyte[], "sequence");

/** Mapping of a byte sequence to $(B Byte Order Mark (BOM))
*/
immutable bomTable = [
    BOMSeq(BOM.none, null),
    BOMSeq(BOM.utf32be, cast(ubyte[])([0x00, 0x00, 0xFE, 0xFF])),
    BOMSeq(BOM.utf32le, cast(ubyte[])([0xFF, 0xFE, 0x00, 0x00])),
    BOMSeq(BOM.utf7, cast(ubyte[])([0x2B, 0x2F, 0x76, 0x39])),
    BOMSeq(BOM.utf7, cast(ubyte[])([0x2B, 0x2F, 0x76, 0x2B])),
    BOMSeq(BOM.utf7, cast(ubyte[])([0x2B, 0x2F, 0x76, 0x2F])),
    BOMSeq(BOM.utf7, cast(ubyte[])([0x2B, 0x2F, 0x76, 0x38, 0x2D])),
    BOMSeq(BOM.utf7, cast(ubyte[])([0x2B, 0x2F, 0x76, 0x38])),
    BOMSeq(BOM.utf1, cast(ubyte[])([0xF7, 0x64, 0x4C])),
    BOMSeq(BOM.utfebcdic, cast(ubyte[])([0xDD, 0x73, 0x66, 0x73])),
    BOMSeq(BOM.scsu, cast(ubyte[])([0x0E, 0xFE, 0xFF])),
    BOMSeq(BOM.bocu1, cast(ubyte[])([0xFB, 0xEE, 0x28])),
    BOMSeq(BOM.gb18030, cast(ubyte[])([0x84, 0x31, 0x95, 0x33])),
    BOMSeq(BOM.utf8, cast(ubyte[])([0xEF, 0xBB, 0xBF])),
    BOMSeq(BOM.utf16be, cast(ubyte[])([0xFE, 0xFF])),
    BOMSeq(BOM.utf16le, cast(ubyte[])([0xFF, 0xFE]))
];

/** Returns a $(D BOMSeq) for a given $(D input).
If no $(D BOM) is present the $(D BOMSeq) for $(D BOM.none) is
returned. The $(D BOM) sequence at the beginning of the range will
not be comsumed from the passed range. If you pass a reference type
range make sure that $(D save) creates a deep copy.

Params:
    input = The sequence to check for the $(D BOM)

Returns:
    the found $(D BOMSeq) corresponding to the passed $(D input).
*/
immutable(BOMSeq) getBOM(Range)(Range input)
if (isForwardRange!Range && is(Unqual!(ElementType!Range) == ubyte))
{
    import std.algorithm.searching : startsWith;
    foreach (it; bomTable[1 .. $])
    {
        if (startsWith(input.save, it.sequence))
        {
            return it;
        }
    }

    return bomTable[0];
}

///
@system unittest
{
    import std.format : format;

    auto ts = dchar(0x0000FEFF) ~ "Hello World"d;

    auto entry = getBOM(cast(ubyte[]) ts);
    version (BigEndian)
    {
        assert(entry.schema == BOM.utf32be, format("%s", entry.schema));
    }
    else
    {
        assert(entry.schema == BOM.utf32le, format("%s", entry.schema));
    }
}

@system unittest
{
    import std.format : format;

    foreach (idx, it; bomTable)
    {
        auto s = it[1] ~ cast(ubyte[])"hello world";
        auto i = getBOM(s);
        assert(i[0] == bomTable[idx][0]);

        if (idx < 4 || idx > 7) // get around the multiple utf7 bom's
        {
            assert(i[0] == BOM.init + idx);
            assert(i[1] == it[1]);
        }
    }
}

@safe pure unittest
{
    struct BOMInputRange
    {
        ubyte[] arr;

        @property ubyte front()
        {
            return this.arr.front;
        }

        @property bool empty()
        {
            return this.arr.empty;
        }

        void popFront()
        {
            this.arr = this.arr[1 .. $];
        }

        @property typeof(this) save()
        {
            return this;
        }
    }

    static assert( isInputRange!BOMInputRange);
    static assert(!isArray!BOMInputRange);

    ubyte[] dummyEnd = [0,0,0,0];

    foreach (idx, it; bomTable[1 .. $])
    {
        {
            auto ir = BOMInputRange(it.sequence.dup);

            auto b = getBOM(ir);
            assert(b.schema == it.schema);
            assert(ir.arr == it.sequence);
        }

        {
            auto noBom = it.sequence[0 .. 1].dup ~ dummyEnd;
            size_t oldLen = noBom.length;
            assert(oldLen - 4 < it.sequence.length);

            auto ir = BOMInputRange(noBom.dup);
            auto b = getBOM(ir);
            assert(b.schema == BOM.none);
            assert(noBom.length == oldLen);
        }
    }
}

/** Constant defining a fully decoded BOM */
enum dchar utfBOM = 0xfeff;
