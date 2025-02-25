@system unittest
{
    import std.regex;

    void test(S)()
    {
        // multi-pattern regex example
        S[] arr = [`([a-z]+):(\d+)`, `(\d+),\d+`];
        auto multi = regex(arr); // multi regex
        S str = "abc:43 12,34";
        auto m = str.matchAll(multi);
        assert(m.front.whichPattern == 1);
        assert(m.front[1] == "abc");
        assert(m.front[2] == "43");
        m.popFront();
        assert(m.front.whichPattern == 2);
        assert(m.front[1] == "12");
    }

    import std.meta : AliasSeq;
    static foreach (C; AliasSeq!(string, wstring, dstring))
        // Test with const array of patterns - see https://issues.dlang.org/show_bug.cgi?id=20301
        static foreach (S; AliasSeq!(C, const C, immutable C))
            test!S();
}

@system unittest
{
    import std.regex;

        import std.regex;
        assert(matchFirst("abc", "[0-9]+", "[a-z]+").whichPattern == 2);
    
}

@system unittest
{
    import std.regex;

    import std.range.primitives : popFrontN;

    auto c = matchFirst("@abc#", regex(`(\w)(\w)(\w)`));
    assert(c.pre == "@"); // Part of input preceding match
    assert(c.post == "#"); // Immediately after match
    assert(c.hit == c[0] && c.hit == "abc"); // The whole match
    assert(c[2] == "b");
    assert(c.front == "abc");
    c.popFront();
    assert(c.front == "a");
    assert(c.back == "c");
    c.popBack();
    assert(c.back == "b");
    popFrontN(c, 2);
    assert(c.empty);

    assert(!matchFirst("nothing", "something"));

    // Captures that are not matched will be null.
    c = matchFirst("ac", regex(`a(b)?c`));
    assert(c);
    assert(!c[1]);
}

@system unittest
{
    import std.regex;

    assert(replaceFirst("noon", regex("n"), "[$&]") == "[n]oon");
}

@system unittest
{
    import std.regex;

    import std.conv : to;
    string list = "#21 out of 46";
    string newList = replaceFirst!(cap => to!string(to!int(cap.hit)+1))
        (list, regex(`[0-9]+`));
    assert(newList == "#22 out of 46");
}

@system unittest
{
    import std.regex;

    import std.array;
    string m1 = "first message\n";
    string m2 = "second message\n";
    auto result = appender!string();
    replaceFirstInto(result, m1, regex(`([a-z]+) message`), "$1");
    //equivalent of the above with user-defined callback
    replaceFirstInto!(cap=>cap[1])(result, m2, regex(`([a-z]+) message`));
    assert(result.data == "first\nsecond\n");
}

@system unittest
{
    import std.regex;

    // insert comma as thousands delimiter
    auto re = regex(r"(?<=\d)(?=(\d\d\d)+\b)","g");
    assert(replaceAll("12000 + 42100 = 54100", re, ",") == "12,000 + 42,100 = 54,100");
}

@system unittest
{
    import std.regex;

    string baz(Captures!(string) m)
    {
        import std.string : toUpper;
        return toUpper(m.hit);
    }
    // Capitalize the letters 'a' and 'r':
    auto s = replaceAll!(baz)("Strap a rocket engine on a chicken.",
            regex("[ar]"));
    assert(s == "StRAp A Rocket engine on A chicken.");
}

@system unittest
{
    import std.regex;

    // insert comma as thousands delimiter in fifty randomly produced big numbers
    import std.array, std.conv, std.random, std.range;
    static re = regex(`(?<=\d)(?=(\d\d\d)+\b)`, "g");
    auto sink = appender!(char [])();
    enum ulong min = 10UL ^^ 10, max = 10UL ^^ 19;
    foreach (i; 0 .. 50)
    {
        sink.clear();
        replaceAllInto(sink, text(uniform(min, max)), re, ",");
        foreach (pos; iota(sink.data.length - 4, 0, -4))
            assert(sink.data[pos] == ',');
    }
}

@system unittest
{
    import std.regex;

    import std.algorithm.comparison : equal;
    auto s1 = ", abc, de,  fg, hi, ";
    assert(equal(splitter(s1, regex(", *")),
        ["", "abc", "de", "fg", "hi", ""]));
}

@system unittest
{
    import std.regex;

    import std.algorithm.comparison : equal;
    import std.typecons : Yes;

    auto pattern = regex(`([\.,])`);

    assert("2003.04.05"
        .splitter!(Yes.keepSeparators)(pattern)
        .equal(["2003", ".", "04", ".", "05"]));

    assert(",1,2,3"
        .splitter!(Yes.keepSeparators)(pattern)
        .equal([",", "1", ",", "2", ",", "3"]));
}

@system unittest
{
    import std.regex;

    import std.algorithm.comparison;
    import std.regex;
    string s = `This is {unfriendly} to *regex*`;
    assert(s.escaper.equal(`This is \{unfriendly\} to \*regex\*`));
}

