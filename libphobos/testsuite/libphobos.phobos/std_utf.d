@safe unittest
{
    import std.utf;

    import std.exception : assertThrown;

    char[4] buf;
    assertThrown!UTFException(encode(buf, cast(dchar) 0xD800));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDBFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDC00));
    assertThrown!UTFException(encode(buf, cast(dchar) 0xDFFF));
    assertThrown!UTFException(encode(buf, cast(dchar) 0x110000));
}

@safe @nogc pure nothrow unittest
{
    import std.utf;

    assert( isValidDchar(cast(dchar) 0x41));
    assert( isValidDchar(cast(dchar) 0x00));
    assert(!isValidDchar(cast(dchar) 0xD800));
    assert(!isValidDchar(cast(dchar) 0x11FFFF));
}

@safe pure nothrow unittest
{
    import std.utf;

    assert( isValidCodepoint(cast(char) 0x40));
    assert(!isValidCodepoint(cast(char) 0x80));
    assert( isValidCodepoint(cast(wchar) 0x1234));
    assert(!isValidCodepoint(cast(wchar) 0xD800));
    assert( isValidCodepoint(cast(dchar) 0x0010FFFF));
    assert(!isValidCodepoint(cast(dchar) 0x12345678));
}

@safe unittest
{
    import std.utf;

    assert("a".stride == 1);
    assert("λ".stride == 2);
    assert("aλ".stride == 1);
    assert("aλ".stride(1) == 2);
    assert("𐐷".stride == 4);
}

@safe unittest
{
    import std.utf;

    assert("a".strideBack == 1);
    assert("λ".strideBack == 2);
    assert("aλ".strideBack == 2);
    assert("aλ".strideBack(1) == 1);
    assert("𐐷".strideBack == 4);
}

@safe unittest
{
    import std.utf;

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

@safe unittest
{
    import std.utf;

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

@safe pure unittest
{
    import std.utf;

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

@safe pure unittest
{
    import std.utf;

    import std.range.primitives;
    string str = "Hello, World!";

    assert(str.decodeFront == 'H' && str == "ello, World!");
    str = "å";
    assert(str.decodeFront == 'å' && str.empty);
    str = "å";
    size_t i;
    assert(str.decodeFront(i) == 'å' && i == 2 && str.empty);
}

@system pure unittest
{
    import std.utf;

    import std.range.primitives;
    string str = "Hello, World!";

    assert(str.decodeBack == '!' && str == "Hello, World");
    str = "å";
    assert(str.decodeBack == 'å' && str.empty);
    str = "å";
    size_t i;
    assert(str.decodeBack(i) == 'å' && i == 2 && str.empty);
}

@safe unittest
{
    import std.utf;

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

@safe unittest
{
    import std.utf;

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

@safe unittest
{
    import std.utf;

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
    import std.utf;

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

@safe pure nothrow @nogc unittest
{
    import std.utf;

    assert(codeLength!char('a') == 1);
    assert(codeLength!wchar('a') == 1);
    assert(codeLength!dchar('a') == 1);

    assert(codeLength!char('\U0010FFFF') == 4);
    assert(codeLength!wchar('\U0010FFFF') == 2);
    assert(codeLength!dchar('\U0010FFFF') == 1);
}

@safe unittest
{
    import std.utf;

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
    import std.utf;

    import std.exception : assertThrown;
    char[] a = [167, 133, 175];
    assertThrown!UTFException(validate(a));
}

@safe pure unittest
{
    import std.utf;

    import std.algorithm.comparison : equal;

    // The ö is represented by two UTF-8 code units
    assert("Hellø"w.toUTF8.equal(['H', 'e', 'l', 'l', 0xC3, 0xB8]));

    // 𐐷 is four code units in UTF-8
    assert("𐐷"d.toUTF8.equal([0xF0, 0x90, 0x90, 0xB7]));
}

@safe pure unittest
{
    import std.utf;

    import std.algorithm.comparison : equal;

    // these graphemes are two code units in UTF-16 and one in UTF-32
    assert("𤭢"d.length == 1);
    assert("𐐷"d.length == 1);

    assert("𤭢"d.toUTF16.equal([0xD852, 0xDF62]));
    assert("𐐷"d.toUTF16.equal([0xD801, 0xDC37]));
}

@safe pure unittest
{
    import std.utf;

    import std.algorithm.comparison : equal;

    // these graphemes are two code units in UTF-16 and one in UTF-32
    assert("𤭢"w.length == 2);
    assert("𐐷"w.length == 2);

    assert("𤭢"w.toUTF32.equal([0x00024B62]));
    assert("𐐷"w.toUTF32.equal([0x00010437]));
}

@safe pure unittest
{
    import std.utf;

    auto p1 = toUTFz!(char*)("hello world");
    auto p2 = toUTFz!(const(char)*)("hello world");
    auto p3 = toUTFz!(immutable(char)*)("hello world");
    auto p4 = toUTFz!(char*)("hello world"d);
    auto p5 = toUTFz!(const(wchar)*)("hello world");
    auto p6 = toUTFz!(immutable(dchar)*)("hello world"w);
}

@system unittest
{
    import std.utf;

    string str = "Hello, World!";
    const(wchar)* p = str.toUTF16z;
    assert(p[str.length] == '\0');
}

@safe pure nothrow @nogc unittest
{
    import std.utf;

    assert(count("") == 0);
    assert(count("a") == 1);
    assert(count("abc") == 3);
    assert(count("\u20AC100") == 4);
}

@safe unittest
{
    import std.utf;

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

@safe unittest
{
    import std.utf;

    string noel1 = "noe\u0308l"; // noël using e + combining diaeresis
    assert(noel1.byCodeUnit[2] != 'ë');
    assert(noel1.byCodeUnit[2] == 'e');

    string noel2 = "no\u00EBl"; // noël using a precomposed ë character
    // Because string is UTF-8, the code unit at index 2 is just
    // the first of a sequence that encodes 'ë'
    assert(noel2.byCodeUnit[2] != 'ë');
}

@safe unittest
{
    import std.utf;

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

@safe pure nothrow unittest
{
    import std.utf;

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

@safe unittest
{
    import std.utf;

    import std.algorithm.comparison : equal;
    import std.exception : assertThrown;

    assert("hello\xF0betty".byChar.byUTF!(dchar, UseReplacementDchar.yes).equal("hello\uFFFDetty"));
    assertThrown!UTFException("hello\xF0betty".byChar.byUTF!(dchar, UseReplacementDchar.no).equal("hello betty"));
}

@safe pure nothrow unittest
{
    import std.utf;

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

