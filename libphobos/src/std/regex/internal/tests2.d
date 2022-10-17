// Split-up due to DMD's enormous memory consumption

module std.regex.internal.tests2;

package(std.regex):

import std.conv, std.exception, std.meta, std.range,
    std.typecons, std.regex;

import std.uni : Escapables; // characters that need escaping

@safe unittest
{
    auto cr = ctRegex!("abc");
    assert(bmatch("abc",cr).hit == "abc");
    auto cr2 = ctRegex!("ab*c");
    assert(bmatch("abbbbc",cr2).hit == "abbbbc");
}
@safe unittest
{
    auto cr3 = ctRegex!("^abc$");
    assert(bmatch("abc",cr3).hit == "abc");
    auto cr4 = ctRegex!(`\b(a\B[a-z]b)\b`);
    assert(array(match("azb",cr4).captures) == ["azb", "azb"]);
}

@safe unittest
{
    auto cr5 = ctRegex!("(?:a{2,4}b{1,3}){1,2}");
    assert(bmatch("aaabaaaabbb", cr5).hit == "aaabaaaabbb");
    auto cr6 = ctRegex!("(?:a{2,4}b{1,3}){1,2}?"w);
    assert(bmatch("aaabaaaabbb"w,  cr6).hit == "aaab"w);
}

@safe unittest
{
    auto cr7 = ctRegex!(`\r.*?$`,"sm");
    assert(bmatch("abc\r\nxy",  cr7).hit == "\r\nxy");
    auto greed =  ctRegex!("<packet.*?/packet>");
    assert(bmatch("<packet>text</packet><packet>text</packet>", greed).hit
            == "<packet>text</packet>");
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    auto cr8 = ctRegex!("^(a)(b)?(c*)");
    auto m8 = bmatch("abcc",cr8);
    assert(m8);
    assert(m8.captures[1] == "a");
    assert(m8.captures[2] == "b");
    assert(m8.captures[3] == "cc");
    auto cr9 = ctRegex!("q(a|b)*q");
    auto m9 = match("xxqababqyy",cr9);
    assert(m9);
    assert(equal(bmatch("xxqababqyy",cr9).captures, ["qababq", "b"]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    auto rtr = regex("a|b|c");
    static ctr = regex("a|b|c");
    assert(equal(rtr.ir,ctr.ir));
    //CTFE parser BUG is triggered by group
    //in the middle of alternation (at least not first and not last)
    static testCT = regex(`abc|(edf)|xyz`);
    auto testRT = regex(`abc|(edf)|xyz`);
    assert(equal(testCT.ir,testRT.ir));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    enum cx = ctRegex!"(A|B|C)";
    auto mx = match("B",cx);
    assert(mx);
    assert(equal(mx.captures, [ "B", "B"]));
    enum cx2 = ctRegex!"(A|B)*";
    assert(match("BAAA",cx2));

    enum cx3 = ctRegex!("a{3,4}","i");
    auto mx3 = match("AaA",cx3);
    assert(mx3);
    assert(mx3.captures[0] == "AaA");
    enum cx4 = ctRegex!(`^a{3,4}?[a-zA-Z0-9~]{1,2}`,"i");
    auto mx4 = match("aaaabc", cx4);
    assert(mx4);
    assert(mx4.captures[0] == "aaaab");
    auto cr8 = ctRegex!("(a)(b)?(c*)");
    auto m8 = bmatch("abcc",cr8);
    assert(m8);
    assert(m8.captures[1] == "a");
    assert(m8.captures[2] == "b");
    assert(m8.captures[3] == "cc");
    auto cr9 = ctRegex!(".*$", "gm");
    auto m9 = match("First\rSecond", cr9);
    assert(m9);
    assert(equal(map!"a.hit"(m9), ["First", "", "Second"]));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
//global matching
    void test_body(alias matchFn)()
    {
        string s = "a quick brown fox jumps over a lazy dog";
        auto r1 = regex("\\b[a-z]+\\b","g");
        string[] test;
        foreach (m; matchFn(s, r1))
            test ~= m.hit;
        assert(equal(test, [ "a", "quick", "brown", "fox", "jumps", "over", "a", "lazy", "dog"]));
        auto free_reg = regex(`

            abc
            \s+
            "
            (
                    [^"]+
                |   \\ "
            )+
            "
            z
        `, "x");
        auto m = match(`abc  "quoted string with \" inside"z`,free_reg);
        assert(m);
        string mails = " hey@you.com no@spam.net ";
        auto rm = regex(`@(?<=\S+@)\S+`,"g");
        assert(equal(map!"a[0]"(matchFn(mails, rm)), ["@you.com", "@spam.net"]));
        auto m2 = matchFn("First line\nSecond line",regex(".*$","gm"));
        assert(equal(map!"a[0]"(m2), ["First line", "", "Second line"]));
        auto m2a = matchFn("First line\nSecond line",regex(".+$","gm"));
        assert(equal(map!"a[0]"(m2a), ["First line", "Second line"]));
        auto m2b = matchFn("First line\nSecond line",regex(".+?$","gm"));
        assert(equal(map!"a[0]"(m2b), ["First line", "Second line"]));
        debug(std_regex_test) writeln("!!! FReD FLAGS test done "~matchFn.stringof~" !!!");
    }
    test_body!bmatch();
    test_body!match();
}

//tests for accumulated std.regex issues and other regressions
@safe unittest
{
    import std.algorithm.comparison : equal;
    import std.algorithm.iteration : map;
    void test_body(alias matchFn)()
    {
        // https://issues.dlang.org/show_bug.cgi?id=5857
        //matching goes out of control if ... in (...){x} has .*/.+
        auto c = matchFn("axxxzayyyyyzd",regex("(a.*z){2}d")).captures;
        assert(c[0] == "axxxzayyyyyzd");
        assert(c[1] == "ayyyyyz");
        auto c2 = matchFn("axxxayyyyyd",regex("(a.*){2}d")).captures;
        assert(c2[0] == "axxxayyyyyd");
        assert(c2[1] == "ayyyyy");
        // https://issues.dlang.org/show_bug.cgi?id=2108
        //greedy vs non-greedy
        auto nogreed = regex("<packet.*?/packet>");
        assert(matchFn("<packet>text</packet><packet>text</packet>", nogreed).hit
               == "<packet>text</packet>");
        auto greed =  regex("<packet.*/packet>");
        assert(matchFn("<packet>text</packet><packet>text</packet>", greed).hit
               == "<packet>text</packet><packet>text</packet>");
        // https://issues.dlang.org/show_bug.cgi?id=4574
        //empty successful match still advances the input
        string[] pres, posts, hits;
        foreach (m; matchFn("abcabc", regex("","g")))
        {
            pres ~= m.pre;
            posts ~= m.post;
            assert(m.hit.empty);

        }
        auto heads = [
            "abcabc",
            "abcab",
            "abca",
            "abc",
            "ab",
            "a",
            ""
        ];
        auto tails = [
            "abcabc",
             "bcabc",
              "cabc",
               "abc",
                "bc",
                 "c",
                  ""
        ];
        assert(pres == array(retro(heads)));
        assert(posts == tails);
        // https://issues.dlang.org/show_bug.cgi?id=6076
        //regression on .*
        auto re = regex("c.*|d");
        auto m = matchFn("mm", re);
        assert(!m);
        debug(std_regex_test) writeln("!!! FReD REGRESSION test done "~matchFn.stringof~" !!!");
        auto rprealloc = regex(`((.){5}.{1,10}){5}`);
        auto arr = array(repeat('0',100));
        auto m2 = matchFn(arr, rprealloc);
        assert(m2);
        assert(collectException(
                regex(r"^(import|file|binary|config)\s+([^\(]+)\(?([^\)]*)\)?\s*$")
                ) is null);
        foreach (ch; [Escapables])
        {
            assert(match(to!string(ch),regex(`[\`~ch~`]`)));
            assert(!match(to!string(ch),regex(`[^\`~ch~`]`)));
            assert(match(to!string(ch),regex(`[\`~ch~`-\`~ch~`]`)));
        }
        // https://issues.dlang.org/show_bug.cgi?id=7718
        string strcmd = "./myApp.rb -os OSX -path \"/GIT/Ruby Apps/sec\" -conf 'notimer'";
        auto reStrCmd = regex (`(".*")|('.*')`, "g");
        assert(equal(map!"a[0]"(matchFn(strcmd, reStrCmd)),
                     [`"/GIT/Ruby Apps/sec"`, `'notimer'`]));
    }
    test_body!bmatch();
    test_body!match();
}

// tests for replace
@safe unittest
{
    void test(alias matchFn)()
    {
        import std.uni : toUpper;

        static foreach (i, v; AliasSeq!(string, wstring, dstring))
        {{
            auto baz(Cap)(Cap m)
            if (is(Cap == Captures!(Cap.String)))
            {
                return toUpper(m.hit);
            }
            alias String = v;
            assert(std.regex.replace!(matchFn)(to!String("ark rapacity"), regex(to!String("r")), to!String("c"))
                   == to!String("ack rapacity"));
            assert(std.regex.replace!(matchFn)(to!String("ark rapacity"), regex(to!String("r"), "g"), to!String("c"))
                   == to!String("ack capacity"));
            assert(std.regex.replace!(matchFn)(to!String("noon"), regex(to!String("^n")), to!String("[$&]"))
                   == to!String("[n]oon"));
            assert(std.regex.replace!(matchFn)(
                to!String("test1 test2"), regex(to!String(`\w+`),"g"), to!String("$`:$'")
            ) == to!String(": test2 test1 :"));
            auto s = std.regex.replace!(baz!(Captures!(String)))(to!String("Strap a rocket engine on a chicken."),
                    regex(to!String("[ar]"), "g"));
            assert(s == "StRAp A Rocket engine on A chicken.");
        }}
        debug(std_regex_test) writeln("!!! Replace test done "~matchFn.stringof~"  !!!");
    }
    test!(bmatch)();
    test!(match)();
}

// tests for splitter
@safe unittest
{
    import std.algorithm.comparison : equal;
    auto s1 = ", abc, de,     fg, hi, ";
    auto sp1 = splitter(s1, regex(", *"));
    auto w1 = ["", "abc", "de", "fg", "hi", ""];
    assert(equal(sp1, w1));

    auto s2 = ", abc, de,  fg, hi";
    auto sp2 = splitter(s2, regex(", *"));
    auto w2 = ["", "abc", "de", "fg", "hi"];

    uint cnt;
    foreach (e; sp2)
    {
        assert(w2[cnt++] == e);
    }
    assert(equal(sp2, w2));
}

@safe unittest
{
    char[] s1 = ", abc, de,  fg, hi, ".dup;
    auto sp2 = splitter(s1, regex(", *"));
}

@safe unittest
{
    import std.algorithm.comparison : equal;
    auto s1 = ", abc, de,  fg, hi, ";
    auto w1 = ["", "abc", "de", "fg", "hi", ""];
    assert(equal(split(s1, regex(", *")), w1[]));
}

// https://issues.dlang.org/show_bug.cgi?id=7141
@safe unittest
{
    string pattern = `[a\--b]`;
    assert(match("-", pattern));
    assert(match("b", pattern));
    string pattern2 = `[&-z]`;
    assert(match("b", pattern2));
}

// https://issues.dlang.org/show_bug.cgi?id=7111
@safe unittest
{
    assert(match("", regex("^")));
}

// https://issues.dlang.org/show_bug.cgi?id=7300
@safe unittest
{
    assert(!match("a"d, "aa"d));
}

// https://issues.dlang.org/show_bug.cgi?id=7551
@safe unittest
{
    auto r = regex("[]abc]*");
    assert("]ab".matchFirst(r).hit == "]ab");
    assertThrown(regex("[]"));
    auto r2 = regex("[]abc--ab]*");
    assert("]ac".matchFirst(r2).hit == "]");
}

// https://issues.dlang.org/show_bug.cgi?id=7674
@safe unittest
{
    assert("1234".replace(regex("^"), "$$") == "$1234");
    assert("hello?".replace(regex(r"\?", "g"), r"\?") == r"hello\?");
    assert("hello?".replace(regex(r"\?", "g"), r"\\?") != r"hello\?");
}

// https://issues.dlang.org/show_bug.cgi?id=7679
@safe unittest
{
    import std.algorithm.comparison : equal;
    static foreach (S; AliasSeq!(string, wstring, dstring))
    {{
        enum re = ctRegex!(to!S(r"\."));
        auto str = to!S("a.b");
        assert(equal(std.regex.splitter(str, re), [to!S("a"), to!S("b")]));
        assert(split(str, re) == [to!S("a"), to!S("b")]);
    }}
}

// https://issues.dlang.org/show_bug.cgi?id=8203
@safe unittest
{
    string data = "
    NAME   = XPAW01_STA:STATION
    NAME   = XPAW01_STA
    ";
    auto uniFileOld = data;
    auto r = regex(
       r"^NAME   = (?P<comp>[a-zA-Z0-9_]+):*(?P<blk>[a-zA-Z0-9_]*)","gm");
    auto uniCapturesNew = match(uniFileOld, r);
    for (int i = 0; i < 20; i++)
        foreach (matchNew; uniCapturesNew) {}
    //a second issue with same symptoms
    auto r2 = regex(`([а-яА-Я\-_]+\s*)+(?<=[\s\.,\^])`);
    match("аллея Театральная", r2);
}

// https://issues.dlang.org/show_bug.cgi?id=8637 purity of enforce
@safe unittest
{
    auto m = match("hello world", regex("world"));
    enforce(m);
}

// https://issues.dlang.org/show_bug.cgi?id=8725
@safe unittest
{
  static italic = regex( r"\*
                (?!\s+)
                (.*?)
                (?!\s+)
                \*", "gx" );
  string input = "this * is* interesting, *very* interesting";
  assert(replace(input, italic, "<i>$1</i>") ==
      "this * is* interesting, <i>very</i> interesting");
}

// https://issues.dlang.org/show_bug.cgi?id=8349
@safe unittest
{
    enum peakRegexStr = r"\>(wgEncode.*Tfbs.*\.(?:narrow)|(?:broad)Peak.gz)</a>";
    enum peakRegex = ctRegex!(peakRegexStr);
    //note that the regex pattern itself is probably bogus
    assert(match(r"\>wgEncode-blah-Tfbs.narrow</a>", peakRegex));
}

// https://issues.dlang.org/show_bug.cgi?id=9211
@safe unittest
{
    import std.algorithm.comparison : equal;
    auto rx_1 =  regex(r"^(\w)*(\d)");
    auto m = match("1234", rx_1);
    assert(equal(m.front, ["1234", "3", "4"]));
    auto rx_2 = regex(r"^([0-9])*(\d)");
    auto m2 = match("1234", rx_2);
    assert(equal(m2.front, ["1234", "3", "4"]));
}

// https://issues.dlang.org/show_bug.cgi?id=9280
@safe unittest
{
    string tomatch = "a!b@c";
    static r = regex(r"^(?P<nick>.*?)!(?P<ident>.*?)@(?P<host>.*?)$");
    auto nm = match(tomatch, r);
    assert(nm);
    auto c = nm.captures;
    assert(c[1] == "a");
    assert(c["nick"] == "a");
}


// https://issues.dlang.org/show_bug.cgi?id=9579
@safe unittest
{
    char[] input = ['a', 'b', 'c'];
    string format = "($1)";
    // used to give a compile error:
    auto re = regex(`(a)`, "g");
    auto r = replace(input, re, format);
    assert(r == "(a)bc");
}

// https://issues.dlang.org/show_bug.cgi?id=9634
@safe unittest
{
    auto re = ctRegex!"(?:a+)";
    assert(match("aaaa", re).hit == "aaaa");
}

// https://issues.dlang.org/show_bug.cgi?id=10798
@safe unittest
{
    auto cr = ctRegex!("[abcd--c]*");
    auto m  = "abc".match(cr);
    assert(m);
    assert(m.hit == "ab");
}

// https://issues.dlang.org/show_bug.cgi?id=10913
@system unittest
{
    @system static string foo(const(char)[] s)
    {
        return s.dup;
    }
    @safe static string bar(const(char)[] s)
    {
        return s.dup;
    }
    () @system {
        replace!((a) => foo(a.hit))("blah", regex(`a`));
    }();
    () @safe {
        replace!((a) => bar(a.hit))("blah", regex(`a`));
    }();
}

// https://issues.dlang.org/show_bug.cgi?id=11262
@safe unittest
{
    enum reg = ctRegex!(r",", "g");
    auto str = "This,List";
    str = str.replace(reg, "-");
    assert(str == "This-List");
}

// https://issues.dlang.org/show_bug.cgi?id=11775
@safe unittest
{
    assert(collectException(regex("a{1,0}")));
}

// https://issues.dlang.org/show_bug.cgi?id=11839
@safe unittest
{
    import std.algorithm.comparison : equal;
    assert(regex(`(?P<var1>\w+)`).namedCaptures.equal(["var1"]));
    assert(collectException(regex(`(?P<1>\w+)`)));
    assert(regex(`(?P<v1>\w+)`).namedCaptures.equal(["v1"]));
    assert(regex(`(?P<__>\w+)`).namedCaptures.equal(["__"]));
    assert(regex(`(?P<я>\w+)`).namedCaptures.equal(["я"]));
}

// https://issues.dlang.org/show_bug.cgi?id=12076
@safe unittest
{
    auto RE = ctRegex!(r"(?<!x[a-z]+)\s([a-z]+)");
    string s = "one two";
    auto m = match(s, RE);
}

// https://issues.dlang.org/show_bug.cgi?id=12105
@safe unittest
{
    auto r = ctRegex!`.*?(?!a)`;
    assert("aaab".matchFirst(r).hit == "aaa");
    auto r2 = ctRegex!`.*(?!a)`;
    assert("aaab".matchFirst(r2).hit == "aaab");
}

// https://issues.dlang.org/show_bug.cgi?id=11784
@safe unittest
{
    assert("abcdefghijklmnopqrstuvwxyz"
        .matchFirst("[a-z&&[^aeiuo]]").hit == "b");
}

// https://issues.dlang.org/show_bug.cgi?id=12366
@safe unittest
{
     auto re = ctRegex!(`^((?=(xx+?)\2+$)((?=\2+$)(?=(x+)(\4+$))\5){2})*x?$`);
     assert("xxxxxxxx".match(re).empty);
     assert(!"xxxx".match(re).empty);
}

// https://issues.dlang.org/show_bug.cgi?id=12582
@safe unittest
{
    auto r = regex(`(?P<a>abc)`);
    assert(collectException("abc".matchFirst(r)["b"]));
}

// https://issues.dlang.org/show_bug.cgi?id=12691
@safe unittest
{
    assert(bmatch("e@", "^([a-z]|)*$").empty);
    assert(bmatch("e@", ctRegex!`^([a-z]|)*$`).empty);
}

// https://issues.dlang.org/show_bug.cgi?id=12713
@safe unittest
{
    assertThrown(regex("[[a-z]([a-z]|(([[a-z])))"));
}

// https://issues.dlang.org/show_bug.cgi?id=12747
@safe unittest
{
    assertThrown(regex(`^x(\1)`));
    assertThrown(regex(`^(x(\1))`));
    assertThrown(regex(`^((x)(?=\1))`));
}

// https://issues.dlang.org/show_bug.cgi?id=13532
version (none) // TODO: revist once we have proper benchmark framework
@safe unittest
{
    import std.datetime.stopwatch : StopWatch, AutoStart;
    import std.math.algebraic : abs;
    import std.conv : to;
    enum re1 = ctRegex!`[0-9][0-9]`;
    immutable static re2 = ctRegex!`[0-9][0-9]`;
    immutable iterations = 1_000_000;
    size_t result1 = 0, result2 = 0;
    auto sw = StopWatch(AutoStart.yes);
    foreach (_; 0 .. iterations)
    {
        result1 += matchFirst("12345678", re1).length;
    }
    const staticTime = sw.peek();
    sw.reset();
    foreach (_; 0 .. iterations)
    {
        result2 += matchFirst("12345678", re2).length;
    }
    const enumTime = sw.peek();
    assert(result1 == result2);
    auto ratio = 1.0 * enumTime.total!"usecs" / staticTime.total!"usecs";
    // enum is faster or the diff is less < 30%
    assert(ratio < 1.0 || abs(ratio - 1.0) < 0.75,
        "enum regex to static regex ratio "~to!string(ratio));
}

// https://issues.dlang.org/show_bug.cgi?id=14504
@safe unittest
{
    auto p = ctRegex!("a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?" ~
            "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
}

// https://issues.dlang.org/show_bug.cgi?id=14529
@safe unittest
{
    auto ctPat2 = regex(r"^[CDF]$", "i");
    foreach (v; ["C", "c", "D", "d", "F", "f"])
        assert(matchAll(v, ctPat2).front.hit == v);
}

// https://issues.dlang.org/show_bug.cgi?id=14615
@safe unittest
{
    import std.array : appender;
    import std.regex : replaceFirst, replaceFirstInto, regex;
    import std.stdio : writeln;

    auto example = "Hello, world!";
    auto pattern = regex("^Hello, (bug)");  // won't find this one
    auto result = replaceFirst(example, pattern, "$1 Sponge Bob");
    assert(result == "Hello, world!");  // Ok.

    auto sink = appender!string;
    replaceFirstInto(sink, example, pattern, "$1 Sponge Bob");
    assert(sink.data == "Hello, world!");
    replaceAllInto(sink, example, pattern, "$1 Sponge Bob");
    assert(sink.data == "Hello, world!Hello, world!");
}

// https://issues.dlang.org/show_bug.cgi?id=15573
@safe unittest
{
    auto rx = regex("[c d]", "x");
    assert("a b".matchFirst(rx));
}

// https://issues.dlang.org/show_bug.cgi?id=15864
@safe unittest
{
    regex(`(<a (?:(?:\w+=\"[^"]*\")?\s*)*href="\.\.?)"`);
}

@safe unittest
{
    auto r = regex("(?# comment)abc(?# comment2)");
    assert("abc".matchFirst(r));
    assertThrown(regex("(?#..."));
}

// https://issues.dlang.org/show_bug.cgi?id=17075
@safe unittest
{
    enum titlePattern = `<title>(.+)</title>`;
    static titleRegex = ctRegex!titlePattern;
    string input = "<title>" ~ "<".repeat(100_000).join;
    assert(input.matchFirst(titleRegex).empty);
}

// https://issues.dlang.org/show_bug.cgi?id=17212
@safe unittest
{
    auto r = regex(" [a] ", "x");
    assert("a".matchFirst(r));
}

// https://issues.dlang.org/show_bug.cgi?id=17157
@safe unittest
{
    import std.algorithm.comparison : equal;
    auto ctr = ctRegex!"(a)|(b)|(c)|(d)";
    auto r = regex("(a)|(b)|(c)|(d)", "g");
    auto s = "--a--b--c--d--";
    auto outcomes = [
        ["a", "a", "", "", ""],
        ["b", "", "b", "", ""],
        ["c", "", "", "c", ""],
        ["d", "", "", "", "d"]
    ];
    assert(equal!equal(s.matchAll(ctr), outcomes));
    assert(equal!equal(s.bmatch(r), outcomes));
}

// https://issues.dlang.org/show_bug.cgi?id=17667
@safe unittest
{
    import std.algorithm.searching : canFind;
    void willThrow(T, size_t line = __LINE__)(T arg, string msg)
    {
        auto e = collectException(regex(arg));
        assert(e.msg.canFind(msg), to!string(line) ~ ": " ~ e.msg);
    }
    willThrow([r".", r"[\(\{[\]\}\)]"], "no matching ']' found while parsing character class");
    willThrow([r"[\", r"123"], "no matching ']' found while parsing character class");
    willThrow([r"[a-", r"123"], "no matching ']' found while parsing character class");
    willThrow([r"[a-\", r"123"], "no matching ']' found while parsing character class");
    willThrow([r"\", r"123"], "invalid escape sequence");
}

// https://issues.dlang.org/show_bug.cgi?id=17668
@safe unittest
{
    import std.algorithm.searching;
    auto e = collectException!RegexException(regex(q"<[^]>"));
    assert(e.msg.canFind("no operand for '^'"), e.msg);
}

// https://issues.dlang.org/show_bug.cgi?id=17673
@safe unittest
{
    string str = `<">`;
    string[] regexps = ["abc", "\"|x"];
    auto regexp = regex(regexps);
    auto c = matchFirst(str, regexp);
    assert(c);
    assert(c.whichPattern == 2);
}

// https://issues.dlang.org/show_bug.cgi?id=18692
@safe unittest
{
    auto rx = regex("()()()");
    auto ma = "".matchFirst(rx);
    auto ma2 = ma;
    ma = ma2;
    assert(ma[1] == "");
}
