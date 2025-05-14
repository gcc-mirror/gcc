@safe unittest
{
    import std.encoding;

    assert(encodingName!(char) == "UTF-8");
    assert(encodingName!(wchar) == "UTF-16");
    assert(encodingName!(dchar) == "UTF-32");
    assert(encodingName!(AsciiChar) == "ASCII");
    assert(encodingName!(Latin1Char) == "ISO-8859-1");
    assert(encodingName!(Latin2Char) == "ISO-8859-2");
    assert(encodingName!(Windows1250Char) == "windows-1250");
    assert(encodingName!(Windows1251Char) == "windows-1251");
    assert(encodingName!(Windows1252Char) == "windows-1252");
}

@safe pure unittest
{
    import std.encoding;

    assert( canEncode!(Latin1Char)('A'));
    assert( canEncode!(Latin2Char)('A'));
    assert(!canEncode!(AsciiChar)('\u00A0'));
    assert( canEncode!(Latin1Char)('\u00A0'));
    assert( canEncode!(Latin2Char)('\u00A0'));
    assert( canEncode!(Windows1250Char)('\u20AC'));
    assert(!canEncode!(Windows1250Char)('\u20AD'));
    assert(!canEncode!(Windows1250Char)('\uFFFD'));
    assert( canEncode!(Windows1251Char)('\u0402'));
    assert(!canEncode!(Windows1251Char)('\u20AD'));
    assert(!canEncode!(Windows1251Char)('\uFFFD'));
    assert( canEncode!(Windows1252Char)('\u20AC'));
    assert(!canEncode!(Windows1252Char)('\u20AD'));
    assert(!canEncode!(Windows1252Char)('\uFFFD'));
    assert(!canEncode!(char)(cast(dchar) 0x110000));
}

@safe pure unittest
{
    import std.encoding;

    import std.algorithm.searching : find;
    import std.utf : byDchar;

    assert("The quick brown fox"
        .byDchar
        .find!(x => !canEncode!AsciiChar(x))
        .empty);
}

@system pure unittest
{
    import std.encoding;

    assert(!isValidCodeUnit(cast(char) 0xC0));
    assert(!isValidCodeUnit(cast(char) 0xFF));
    assert( isValidCodeUnit(cast(wchar) 0xD800));
    assert(!isValidCodeUnit(cast(dchar) 0xD800));
    assert(!isValidCodeUnit(cast(AsciiChar) 0xA0));
    assert( isValidCodeUnit(cast(Windows1250Char) 0x80));
    assert(!isValidCodeUnit(cast(Windows1250Char) 0x81));
    assert( isValidCodeUnit(cast(Windows1251Char) 0x80));
    assert(!isValidCodeUnit(cast(Windows1251Char) 0x98));
    assert( isValidCodeUnit(cast(Windows1252Char) 0x80));
    assert(!isValidCodeUnit(cast(Windows1252Char) 0x81));
}

@system pure unittest
{
    import std.encoding;

    assert( isValid("\u20AC100"));
    assert(!isValid(cast(char[3])[167, 133, 175]));
}

@system pure unittest
{
    import std.encoding;

    assert(sanitize("hello \xF0\x80world") == "hello \xEF\xBF\xBDworld");
}

@system pure unittest
{
    import std.encoding;

    assert(firstSequence("\u20AC1000") == "\u20AC".length);
    assert(firstSequence("hel") == "h".length);
}

@system pure unittest
{
    import std.encoding;

    assert(lastSequence("1000\u20AC") == "\u20AC".length);
    assert(lastSequence("hellö") == "ö".length);
}

@system pure unittest
{
    import std.encoding;

    assert(index("\u20AC100",1) == 3);
    assert(index("hällo",2) == 3);
}

@system unittest
{
    import std.encoding;

    string s = "hello";
    string t;
    foreach (c;codePoints(s))
    {
        t ~= cast(char) c;
    }
    assert(s == t);
}

@system unittest
{
    import std.encoding;

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

@system pure unittest
{
    import std.encoding;

    wstring ws;
    // transcode from UTF-8 to UTF-16
    transcode("hello world",ws);
    assert(ws == "hello world"w);

    Latin1String ls;
    // transcode from UTF-16 to ISO-8859-1
    transcode(ws, ls);
    assert(ls == "hello world");
}

@system unittest
{
    import std.encoding;

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

