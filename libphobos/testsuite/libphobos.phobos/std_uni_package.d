pure @safe unittest
{
    import std.uni;

        import std.algorithm.comparison : equal;

        auto set = CodepointSet('a', 'z'+1, 'а', 'я'+1);
        foreach (v; 'a'..'z'+1)
            assert(set[v]);
        // Cyrillic lowercase interval
        foreach (v; 'а'..'я'+1)
            assert(set[v]);
        //specific order is not required, intervals may interesect
        auto set2 = CodepointSet('а', 'я'+1, 'a', 'd', 'b', 'z'+1);
        //the same end result
        assert(set2.byInterval.equal(set.byInterval));
        // test constructor this(Range)(Range intervals)
        auto chessPiecesWhite = CodepointInterval(9812, 9818);
        auto chessPiecesBlack = CodepointInterval(9818, 9824);
        auto set3 = CodepointSet([chessPiecesWhite, chessPiecesBlack]);
        foreach (v; '♔'..'♟'+1)
            assert(set3[v]);
    
}

pure @safe unittest
{
    import std.uni;

        auto gothic = unicode.Gothic;
        // Gothic letter ahsa
        assert(gothic['\U00010330']);
        // no ascii in Gothic obviously
        assert(!gothic['$']);
    
}

pure @safe unittest
{
    import std.uni;

        import std.algorithm.comparison : equal;
        import std.range : iota;

        auto lower = unicode.LowerCase;
        auto upper = unicode.UpperCase;
        auto ascii = unicode.ASCII;

        assert((lower & upper).empty); // no intersection
        auto lowerASCII = lower & ascii;
        assert(lowerASCII.byCodepoint.equal(iota('a', 'z'+1)));
        // throw away all of the lowercase ASCII
        assert((ascii - lower).length == 128 - 26);

        auto onlyOneOf = lower ~ ascii;
        assert(!onlyOneOf['Δ']); // not ASCII and not lowercase
        assert(onlyOneOf['$']); // ASCII and not lowercase
        assert(!onlyOneOf['a']); // ASCII and lowercase
        assert(onlyOneOf['я']); // not ASCII but lowercase

        // throw away all cased letters from ASCII
        auto noLetters = ascii - (lower | upper);
        assert(noLetters.length == 128 - 26*2);
    
}

pure @safe unittest
{
    import std.uni;

        assert('я' in unicode.Cyrillic);
        assert(!('z' in unicode.Cyrillic));
    
}

pure @safe unittest
{
    import std.uni;

        import std.algorithm.comparison : equal;
        import std.range : iota;

        auto set = unicode.ASCII;
        set.byCodepoint.equal(iota(0, 0x80));
    
}

pure @safe unittest
{
    import std.uni;

        import std.conv : to;
        import std.format : format;
        import std.uni : unicode;

        // This was originally using Cyrillic script.
        // Unfortunately this is a pretty active range for changes,
        // and hence broke in an update.
        // Therefore the range Basic latin was used instead as it
        // unlikely to ever change.

        assert(unicode.InBasic_latin.to!string == "[0..128)");

        // The specs '%s' and '%d' are equivalent to the to!string call above.
        assert(format("%d", unicode.InBasic_latin) == unicode.InBasic_latin.to!string);

        assert(format("%#x", unicode.InBasic_latin) == "[0..0x80)");
        assert(format("%#X", unicode.InBasic_latin) == "[0..0X80)");
    
}

pure @safe unittest
{
    import std.uni;

        CodepointSet someSet;
        someSet.add('0', '5').add('A','Z'+1);
        someSet.add('5', '9'+1);
        assert(someSet['0']);
        assert(someSet['5']);
        assert(someSet['9']);
        assert(someSet['Z']);
    
}

pure @safe unittest
{
    import std.uni;

        auto set = unicode.ASCII;
        // union with the inverse gets all of the code points in the Unicode
        assert((set | set.inverted).length == 0x110000);
        // no intersection with the inverse
        assert((set & set.inverted).empty);
    
}

pure @safe unittest
{
    import std.uni;

        CodepointSet emptySet;
        assert(emptySet.length == 0);
        assert(emptySet.empty);
    
}

pure @safe unittest
{
    import std.uni;

        string truth = "2² = 4";
        auto m = utfMatcher!char(unicode.Number);
        assert(m.match(truth)); // '2' is a number all right
        assert(truth == "² = 4"); // skips on match
        assert(m.match(truth)); // so is the superscript '2'
        assert(!m.match(truth)); // space is not a number
        assert(truth == " = 4"); // unaffected on no match
        assert(!m.skip(truth)); // same test ...
        assert(truth == "= 4"); // but skips a codepoint regardless
        assert(!m.test(truth)); // '=' is not a number
        assert(truth == "= 4"); // test never affects argument
    
}

@safe unittest
{
    import std.uni;

        import std.exception : collectException;
        auto ascii = unicode.ASCII;
        assert(ascii['A']);
        assert(ascii['~']);
        assert(!ascii['\u00e0']);
        // matching is case-insensitive
        assert(ascii == unicode.ascII);
        assert(!ascii['à']);
        // underscores, '-' and whitespace in names are ignored too
        auto latin = unicode.in_latin1_Supplement;
        assert(latin['à']);
        assert(!latin['$']);
        // BTW Latin 1 Supplement is a block, hence "In" prefix
        assert(latin == unicode("In Latin 1 Supplement"));
        // run-time look up throws if no such set is found
        assert(collectException(unicode("InCyrilliac")));
    
}

@safe unittest
{
    import std.uni;

        // use .block for explicitness
        assert(unicode.block.Greek_and_Coptic == unicode.InGreek_and_Coptic);
    
}

@safe unittest
{
    import std.uni;

        auto arabicScript = unicode.script.arabic;
        auto arabicBlock = unicode.block.arabic;
        // there is an intersection between script and block
        assert(arabicBlock['؁']);
        assert(arabicScript['؁']);
        // but they are different
        assert(arabicBlock != arabicScript);
        assert(arabicBlock == unicode.inArabic);
        assert(arabicScript == unicode.arabic);
    
}

@safe unittest
{
    import std.uni;

        // L here is syllable type not Letter as in unicode.L short-cut
        auto leadingVowel = unicode.hangulSyllableType("L");
        // check that some leading vowels are present
        foreach (vowel; '\u1110'..'\u115F')
            assert(leadingVowel[vowel]);
        assert(leadingVowel == unicode.hangulSyllableType.L);
    
}

@safe unittest
{
    import std.uni;

        import std.uni : unicode;
        string pat = "[a-zA-Z0-9]hello";
        auto set = unicode.parseSet(pat);
        // check some of the codepoints
        assert(set['a'] && set['A'] && set['9']);
        assert(pat == "hello");
    
}

@safe unittest
{
    import std.uni;

    assert(graphemeStride("  ", 1) == 1);
    // A + combing ring above
    string city = "A\u030Arhus";
    size_t first = graphemeStride(city, 0);
    assert(first == 3); //\u030A has 2 UTF-8 code units
    assert(city[0 .. first] == "A\u030A");
    assert(city[first..$] == "rhus");
}

@safe pure unittest
{
    import std.uni;

    // Two Union Jacks of the Great Britain in each
    string s = "\U0001F1EC\U0001F1E7\U0001F1EC\U0001F1E7";
    wstring ws = "\U0001F1EC\U0001F1E7\U0001F1EC\U0001F1E7";
    dstring ds = "\U0001F1EC\U0001F1E7\U0001F1EC\U0001F1E7";

    // String pop length in code units, not points.
    assert(s.popGrapheme() == 8);
    assert(ws.popGrapheme() == 4);
    assert(ds.popGrapheme() == 2);

    assert(s == "\U0001F1EC\U0001F1E7");
    assert(ws == "\U0001F1EC\U0001F1E7");
    assert(ds == "\U0001F1EC\U0001F1E7");

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter;

    // Also works for non-random access ranges as long as the
    // character type is 32-bit.
    auto testPiece = "\r\nhello!"d.filter!(x => !x.isAlpha);
    // Windows-style line ending is two code points in a single grapheme.
    assert(testPiece.popGrapheme() == 2);
    assert(testPiece.equal("!"d));
}

@safe unittest
{
    import std.uni;

    import std.algorithm.comparison : equal;
    import std.range.primitives : walkLength;
    import std.range : take, drop;
    auto text = "noe\u0308l"; // noël using e + combining diaeresis
    assert(text.walkLength == 5); // 5 code points

    auto gText = text.byGrapheme;
    assert(gText.walkLength == 4); // 4 graphemes

    assert(gText.take(3).equal("noe\u0308".byGrapheme));
    assert(gText.drop(3).equal("l".byGrapheme));
}

@safe unittest
{
    import std.uni;

    import std.array : array;
    import std.conv : text;
    import std.range : retro;

    string s = "noe\u0308l"; // noël

    // reverse it and convert the result to a string
    string reverse = s.byGrapheme
        .array
        .retro
        .byCodePoint
        .text;

    assert(reverse == "le\u0308on"); // lëon
}

@safe unittest
{
    import std.uni;

        auto g = Grapheme("A\u0302");
        assert(g[0] == 'A');
        assert(g.valid);
        g[1] = '~'; // ASCII tilda is not a combining mark
        assert(g[1] == '~');
        assert(!g.valid);
    
}

@safe unittest
{
    import std.uni;

        import std.algorithm.comparison : equal;
        auto g = Grapheme("A");
        assert(g.valid);
        g ~= '\u0301';
        assert(g[].equal("A\u0301"));
        assert(g.valid);
        g ~= "B";
        // not a valid grapheme cluster anymore
        assert(!g.valid);
        // still could be useful though
        assert(g[].equal("A\u0301B"));
    
}

@safe unittest
{
    import std.uni;

    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : filter;
    import std.range : isRandomAccessRange;

    string bold = "ku\u0308hn";

    // note that decodeGrapheme takes parameter by ref
    auto first = decodeGrapheme(bold);

    assert(first.length == 1);
    assert(first[0] == 'k');

    // the next grapheme is 2 characters long
    auto wideOne = decodeGrapheme(bold);
    // slicing a grapheme yields a random-access range of dchar
    assert(wideOne[].equal("u\u0308"));
    assert(wideOne.length == 2);
    static assert(isRandomAccessRange!(typeof(wideOne[])));

    // all of the usual range manipulation is possible
    assert(wideOne[].filter!isMark().equal("\u0308"));

    auto g = Grapheme("A");
    assert(g.valid);
    g ~= '\u0301';
    assert(g[].equal("A\u0301"));
    assert(g.valid);
    g ~= "B";
    // not a valid grapheme cluster anymore
    assert(!g.valid);
    // still could be useful though
    assert(g[].equal("A\u0301B"));
}

@safe @nogc pure nothrow unittest
{
    import std.uni;

    assert(sicmp("Август", "авгусТ") == 0);
    // Greek also works as long as there is no 1:M mapping in sight
    assert(sicmp("ΌΎ", "όύ") == 0);
    // things like the following won't get matched as equal
    // Greek small letter iota with dialytika and tonos
    assert(sicmp("ΐ", "\u03B9\u0308\u0301") != 0);

    // while icmp has no problem with that
    assert(icmp("ΐ", "\u03B9\u0308\u0301") == 0);
    assert(icmp("ΌΎ", "όύ") == 0);
}

@safe @nogc pure nothrow unittest
{
    import std.uni;

    assert(icmp("Rußland", "Russland") == 0);
    assert(icmp("ᾩ -> \u1F70\u03B9", "\u1F61\u03B9 -> ᾲ") == 0);
}

@safe @nogc nothrow pure unittest
{
    import std.uni;

    import std.utf : byDchar;

    assert(icmp("Rußland".byDchar, "Russland".byDchar) == 0);
    assert(icmp("ᾩ -> \u1F70\u03B9".byDchar, "\u1F61\u03B9 -> ᾲ".byDchar) == 0);
}

@safe unittest
{
    import std.uni;

    // shorten the code
    alias CC = combiningClass;

    // combining tilda
    assert(CC('\u0303') == 230);
    // combining ring below
    assert(CC('\u0325') == 220);
    // the simple consequence is that  "tilda" should be
    // placed after a "ring below" in a sequence
}

@safe unittest
{
    import std.uni;

    assert(compose('A','\u0308') == '\u00C4');
    assert(compose('A', 'B') == dchar.init);
    assert(compose('C', '\u0301') == '\u0106');
    // note that the starter is the first one
    // thus the following doesn't compose
    assert(compose('\u0308', 'A') == dchar.init);
}

@safe unittest
{
    import std.uni;

    import std.algorithm.comparison : equal;

    assert(compose('A','\u0308') == '\u00C4');
    assert(compose('A', 'B') == dchar.init);
    assert(compose('C', '\u0301') == '\u0106');
    // note that the starter is the first one
    // thus the following doesn't compose
    assert(compose('\u0308', 'A') == dchar.init);

    assert(decompose('Ĉ')[].equal("C\u0302"));
    assert(decompose('D')[].equal("D"));
    assert(decompose('\uD4DC')[].equal("\u1111\u1171\u11B7"));
    assert(decompose!Compatibility('¹')[].equal("1"));
}

@safe unittest
{
    import std.uni;

    import std.algorithm.comparison : equal;
    assert(decomposeHangul('\uD4DB')[].equal("\u1111\u1171\u11B6"));
}

@safe unittest
{
    import std.uni;

    assert(composeJamo('\u1111', '\u1171', '\u11B6') == '\uD4DB');
    // leaving out T-vowel, or passing any codepoint
    // that is not trailing consonant composes an LV-syllable
    assert(composeJamo('\u1111', '\u1171') == '\uD4CC');
    assert(composeJamo('\u1111', '\u1171', ' ') == '\uD4CC');
    assert(composeJamo('\u1111', 'A') == dchar.init);
    assert(composeJamo('A', '\u1171') == dchar.init);
}

@safe pure unittest
{
    import std.uni;

    // any encoding works
    wstring greet = "Hello world";
    assert(normalize(greet) is greet); // the same exact slice

    // An example of a character with all 4 forms being different:
    // Greek upsilon with acute and hook symbol (code point 0x03D3)
    assert(normalize!NFC("ϓ") == "\u03D3");
    assert(normalize!NFD("ϓ") == "\u03D2\u0301");
    assert(normalize!NFKC("ϓ") == "\u038E");
    assert(normalize!NFKD("ϓ") == "\u03A5\u0301");
}

@safe unittest
{
    import std.uni;

    // e.g. Cyrillic is always allowed, so is ASCII
    assert(allowedIn!NFC('я'));
    assert(allowedIn!NFD('я'));
    assert(allowedIn!NFKC('я'));
    assert(allowedIn!NFKD('я'));
    assert(allowedIn!NFC('Z'));
}

@safe pure unittest
{
    import std.uni;

    import std.algorithm.comparison : equal;

    assert("hEllo".asUpperCase.equal("HELLO"));
}

@safe pure unittest
{
    import std.uni;

    import std.algorithm.comparison : equal;

    assert("hEllo".asCapitalized.equal("Hello"));
}

@safe unittest
{
    import std.uni;

    import std.algorithm.iteration : map;
    import std.algorithm.mutation : copy;
    import std.array : appender;

    auto abuf = appender!(char[])();
    "hello".map!toUpper.copy(abuf);
    assert(abuf.data == "HELLO");
}

