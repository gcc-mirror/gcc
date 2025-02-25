@safe pure unittest
{
    import std.string;

    import std.exception : assertThrown;
    auto bad = "      a\n\tb\n   c";
    assertThrown!StringException(bad.outdent);
}

@system pure unittest
{
    import std.string;

    assert(fromStringz("foo\0"c.ptr) == "foo"c);
    assert(fromStringz("foo\0"w.ptr) == "foo"w);
    assert(fromStringz("foo\0"d.ptr) == "foo"d);

    assert(fromStringz("福\0"c.ptr) == "福"c);
    assert(fromStringz("福\0"w.ptr) == "福"w);
    assert(fromStringz("福\0"d.ptr) == "福"d);
}

@nogc @safe pure nothrow unittest
{
    import std.string;

    struct C
    {
        char[32] name;
    }
    assert(C("foo\0"c).name.fromStringz() == "foo"c);

    struct W
    {
        wchar[32] name;
    }
    assert(W("foo\0"w).name.fromStringz() == "foo"w);

    struct D
    {
        dchar[32] name;
    }
    assert(D("foo\0"d).name.fromStringz() == "foo"d);
}

pure nothrow @system unittest
{
    import std.string;

    import core.stdc.string : strlen;
    import std.conv : to;

    auto p = toStringz("foo");
    assert(strlen(p) == 3);
    const(char)[] foo = "abbzxyzzy";
    p = toStringz(foo[3 .. 5]);
    assert(strlen(p) == 2);

    string test = "";
    p = toStringz(test);
    assert(*p == 0);

    test = "\0";
    p = toStringz(test);
    assert(*p == 0);

    test = "foo\0";
    p = toStringz(test);
    assert(p[0] == 'f' && p[1] == 'o' && p[2] == 'o' && p[3] == 0);

    const string test2 = "";
    p = toStringz(test2);
    assert(*p == 0);

    assert(toStringz([]) is toStringz(""));
}

@safe pure unittest
{
    import std.string;

    import std.typecons : No;

    string s = "Hello World";
    assert(indexOf(s, 'W') == 6);
    assert(indexOf(s, 'Z') == -1);
    assert(indexOf(s, 'w', No.caseSensitive) == 6);
}

@safe pure unittest
{
    import std.string;

    import std.typecons : No;

    string s = "Hello World";
    assert(indexOf(s, 'W', 4) == 6);
    assert(indexOf(s, 'Z', 100) == -1);
    assert(indexOf(s, 'w', 3, No.caseSensitive) == 6);
}

@safe pure unittest
{
    import std.string;

    import std.typecons : No;

    string s = "Hello World";
    assert(indexOf(s, "Wo", 4) == 6);
    assert(indexOf(s, "Zo", 100) == -1);
    assert(indexOf(s, "wo", 3, No.caseSensitive) == 6);
}

@safe pure unittest
{
    import std.string;

    import std.typecons : No;

    string s = "Hello World";
    assert(indexOf(s, "Wo") == 6);
    assert(indexOf(s, "Zo") == -1);
    assert(indexOf(s, "wO", No.caseSensitive) == 6);
}

@safe pure unittest
{
    import std.string;

    import std.typecons : No;

    string s = "Hello World";
    assert(lastIndexOf(s, 'l') == 9);
    assert(lastIndexOf(s, 'Z') == -1);
    assert(lastIndexOf(s, 'L', No.caseSensitive) == 9);
}

@safe pure unittest
{
    import std.string;

    import std.typecons : No;

    string s = "Hello World";
    assert(lastIndexOf(s, 'l', 4) == 3);
    assert(lastIndexOf(s, 'Z', 1337) == -1);
    assert(lastIndexOf(s, 'L', 7, No.caseSensitive) == 3);
}

@safe pure unittest
{
    import std.string;

    import std.typecons : No;

    string s = "Hello World";
    assert(lastIndexOf(s, "ll") == 2);
    assert(lastIndexOf(s, "Zo") == -1);
    assert(lastIndexOf(s, "lL", No.caseSensitive) == 2);
}

@safe pure unittest
{
    import std.string;

    import std.typecons : No;

    string s = "Hello World";
    assert(lastIndexOf(s, "ll", 4) == 2);
    assert(lastIndexOf(s, "Zo", 128) == -1);
    assert(lastIndexOf(s, "lL", 3, No.caseSensitive) == -1);
}

@safe pure unittest
{
    import std.string;

    import std.conv : to;

    ptrdiff_t i = "helloWorld".indexOfAny("Wr");
    assert(i == 5);
    i = "öällo world".indexOfAny("lo ");
    assert(i == 4, to!string(i));
}

@safe pure unittest
{
    import std.string;

    import std.conv : to;

    ptrdiff_t i = "helloWorld".indexOfAny("Wr", 4);
    assert(i == 5);

    i = "Foo öällo world".indexOfAny("lh", 3);
    assert(i == 8, to!string(i));
}

@safe pure unittest
{
    import std.string;

    ptrdiff_t i = "helloWorld".lastIndexOfAny("Wlo");
    assert(i == 8);

    i = "Foo öäöllo world".lastIndexOfAny("öF");
    assert(i == 8);
}

@safe pure unittest
{
    import std.string;

    import std.conv : to;

    ptrdiff_t i = "helloWorld".lastIndexOfAny("Wlo", 4);
    assert(i == 3);

    i = "Foo öäöllo world".lastIndexOfAny("öF", 3);
    assert(i == 0);
}

@safe pure unittest
{
    import std.string;

    assert(indexOfNeither("abba", "a", 2) == 2);
    assert(indexOfNeither("def", "de", 1) == 2);
    assert(indexOfNeither("dfefffg", "dfe", 4) == 6);
}

@safe pure unittest
{
    import std.string;

    assert(indexOfNeither("def", "a") == 0);
    assert(indexOfNeither("def", "de") == 2);
    assert(indexOfNeither("dfefffg", "dfe") == 6);
}

@safe pure unittest
{
    import std.string;

    assert(lastIndexOfNeither("abba", "a") == 2);
    assert(lastIndexOfNeither("def", "f") == 1);
}

@safe pure unittest
{
    import std.string;

    assert(lastIndexOfNeither("def", "rsa", 3) == -1);
    assert(lastIndexOfNeither("abba", "a", 2) == 1);
}

@safe pure unittest
{
    import std.string;

    string s = "hello";
    static assert(is(typeof(representation(s)) == immutable(ubyte)[]));
    assert(representation(s) is cast(immutable(ubyte)[]) s);
    assert(representation(s) == [0x68, 0x65, 0x6c, 0x6c, 0x6f]);
}

pure @safe unittest
{
    import std.string;

    assert(capitalize("hello") == "Hello");
    assert(capitalize("World") == "World");
}

@safe pure nothrow unittest
{
    import std.string;

    string s = "Hello\nmy\rname\nis";
    assert(splitLines(s) == ["Hello", "my", "name", "is"]);
}

@safe pure unittest
{
    import std.string;

    import std.array : array;

    string s = "Hello\nmy\rname\nis";

    /* notice the call to 'array' to turn the lazy range created by
    lineSplitter comparable to the string[] created by splitLines.
    */
    assert(lineSplitter(s).array == splitLines(s));
}

@nogc @safe pure unittest
{
    import std.string;

    auto s = "\rpeter\n\rpaul\r\njerry\u2028ice\u2029cream\n\nsunday\nmon\u2030day\n";
    auto lines = s.lineSplitter();
    static immutable witness = ["", "peter", "", "paul", "jerry", "ice", "cream", "", "sunday", "mon\u2030day"];
    uint i;
    foreach (line; lines)
    {
        assert(line == witness[i++]);
    }
    assert(i == witness.length);
}

nothrow @safe pure unittest
{
    import std.string;

    import std.uni : lineSep, paraSep;
    assert(stripLeft("     hello world     ") ==
           "hello world     ");
    assert(stripLeft("\n\t\v\rhello world\n\t\v\r") ==
           "hello world\n\t\v\r");
    assert(stripLeft(" \u2028hello world") ==
           "hello world");
    assert(stripLeft("hello world") ==
           "hello world");
    assert(stripLeft([lineSep] ~ "hello world" ~ lineSep) ==
           "hello world" ~ [lineSep]);
    assert(stripLeft([paraSep] ~ "hello world" ~ paraSep) ==
           "hello world" ~ [paraSep]);

    import std.array : array;
    import std.utf : byChar;
    assert(stripLeft("     hello world     "w.byChar).array ==
           "hello world     ");
    assert(stripLeft("     \u2022hello world     ".byChar).array ==
           "\u2022hello world     ");
}

@safe pure unittest
{
    import std.string;

    assert(stripLeft("     hello world     ", " ") ==
           "hello world     ");
    assert(stripLeft("xxxxxhello world     ", "x") ==
           "hello world     ");
    assert(stripLeft("xxxyy    hello world     ", "xy ") ==
           "hello world     ");
}

@safe pure unittest
{
    import std.string;

    import std.array : array;
    import std.utf : byChar, byWchar, byDchar;

    assert(stripLeft("  xxxyy hello world     "w.byChar, "xy ").array ==
           "hello world     ");

    assert(stripLeft("\u2028\u2020hello world\u2028"w.byWchar,
                     "\u2028").array == "\u2020hello world\u2028");
    assert(stripLeft("\U00010001hello world"w.byWchar, " ").array ==
           "\U00010001hello world"w);
    assert(stripLeft("\U00010001 xyhello world"d.byDchar,
                     "\U00010001 xy").array == "hello world"d);

    assert(stripLeft("\u2020hello"w, "\u2020"w) == "hello"w);
    assert(stripLeft("\U00010001hello"d, "\U00010001"d) == "hello"d);
    assert(stripLeft(" hello ", "") == " hello ");
}

nothrow @safe pure unittest
{
    import std.string;

    import std.uni : lineSep, paraSep;
    assert(stripRight("     hello world     ") ==
           "     hello world");
    assert(stripRight("\n\t\v\rhello world\n\t\v\r") ==
           "\n\t\v\rhello world");
    assert(stripRight("hello world") ==
           "hello world");
    assert(stripRight([lineSep] ~ "hello world" ~ lineSep) ==
           [lineSep] ~ "hello world");
    assert(stripRight([paraSep] ~ "hello world" ~ paraSep) ==
           [paraSep] ~ "hello world");
}

@safe pure unittest
{
    import std.string;

    assert(stripRight("     hello world     ", "x") ==
           "     hello world     ");
    assert(stripRight("     hello world     ", " ") ==
           "     hello world");
    assert(stripRight("     hello worldxy     ", "xy ") ==
           "     hello world");
}

@safe pure unittest
{
    import std.string;

    import std.uni : lineSep, paraSep;
    assert(strip("     hello world     ") ==
           "hello world");
    assert(strip("\n\t\v\rhello world\n\t\v\r") ==
           "hello world");
    assert(strip("hello world") ==
           "hello world");
    assert(strip([lineSep] ~ "hello world" ~ [lineSep]) ==
           "hello world");
    assert(strip([paraSep] ~ "hello world" ~ [paraSep]) ==
           "hello world");
}

@safe pure unittest
{
    import std.string;

    assert(strip("     hello world     ", "x") ==
           "     hello world     ");
    assert(strip("     hello world     ", " ") ==
           "hello world");
    assert(strip("   xyxyhello worldxyxy     ", "xy ") ==
           "hello world");
    assert(strip("\u2020hello\u2020"w, "\u2020"w) == "hello"w);
    assert(strip("\U00010001hello\U00010001"d, "\U00010001"d) == "hello"d);
    assert(strip(" hello ", "") == " hello ");
}

@safe pure unittest
{
    import std.string;

    assert(strip("xxhelloyy", "x", "y") == "hello");
    assert(strip("   xyxyhello worldxyxyzz    ", "xy ", "xyz ") ==
           "hello world");
    assert(strip("\u2020hello\u2028"w, "\u2020"w, "\u2028"w) == "hello"w);
    assert(strip("\U00010001hello\U00010002"d, "\U00010001"d, "\U00010002"d) ==
           "hello"d);
    assert(strip(" hello ", "", "") == " hello ");
}

@safe pure unittest
{
    import std.string;

    import std.uni : lineSep, paraSep, nelSep;
    import std.utf : decode;
    assert(chomp(" hello world  \n\r") == " hello world  \n");
    assert(chomp(" hello world  \r\n") == " hello world  ");
    assert(chomp(" hello world  \f") == " hello world  ");
    assert(chomp(" hello world  \v") == " hello world  ");
    assert(chomp(" hello world  \n\n") == " hello world  \n");
    assert(chomp(" hello world  \n\n ") == " hello world  \n\n ");
    assert(chomp(" hello world  \n\n" ~ [lineSep]) == " hello world  \n\n");
    assert(chomp(" hello world  \n\n" ~ [paraSep]) == " hello world  \n\n");
    assert(chomp(" hello world  \n\n" ~ [ nelSep]) == " hello world  \n\n");
    assert(chomp(" hello world ") == " hello world ");
    assert(chomp(" hello world") == " hello world");
    assert(chomp("") == "");

    assert(chomp(" hello world", "orld") == " hello w");
    assert(chomp(" hello world", " he") == " hello world");
    assert(chomp("", "hello") == "");

    // Don't decode pointlessly
    assert(chomp("hello\xFE", "\r") == "hello\xFE");
}

@safe pure unittest
{
    import std.string;

    assert(chompPrefix("hello world", "he") == "llo world");
    assert(chompPrefix("hello world", "hello w") == "orld");
    assert(chompPrefix("hello world", " world") == "hello world");
    assert(chompPrefix("", "hello") == "");
}

@safe pure unittest
{
    import std.string;

    assert(chop("hello world") == "hello worl");
    assert(chop("hello world\n") == "hello world");
    assert(chop("hello world\r") == "hello world");
    assert(chop("hello world\n\r") == "hello world\n");
    assert(chop("hello world\r\n") == "hello world");
    assert(chop("Walter Bright") == "Walter Brigh");
    assert(chop("") == "");
}

@safe pure unittest
{
    import std.string;

    assert(leftJustify("hello", 7, 'X') == "helloXX");
    assert(leftJustify("hello", 2, 'X') == "hello");
    assert(leftJustify("hello", 9, 'X') == "helloXXXX");
}

@safe pure @nogc nothrow unittest
{
    import std.string;

    import std.algorithm.comparison : equal;
    import std.utf : byChar;
    assert(leftJustifier("hello", 2).equal("hello".byChar));
    assert(leftJustifier("hello", 7).equal("hello  ".byChar));
    assert(leftJustifier("hello", 7, 'x').equal("helloxx".byChar));
}

@safe pure unittest
{
    import std.string;

    assert(rightJustify("hello", 7, 'X') == "XXhello");
    assert(rightJustify("hello", 2, 'X') == "hello");
    assert(rightJustify("hello", 9, 'X') == "XXXXhello");
}

@safe pure @nogc nothrow unittest
{
    import std.string;

    import std.algorithm.comparison : equal;
    import std.utf : byChar;
    assert(rightJustifier("hello", 2).equal("hello".byChar));
    assert(rightJustifier("hello", 7).equal("  hello".byChar));
    assert(rightJustifier("hello", 7, 'x').equal("xxhello".byChar));
}

@safe pure unittest
{
    import std.string;

    assert(center("hello", 7, 'X') == "XhelloX");
    assert(center("hello", 2, 'X') == "hello");
    assert(center("hello", 9, 'X') == "XXhelloXX");
}

@safe pure @nogc nothrow unittest
{
    import std.string;

    import std.algorithm.comparison : equal;
    import std.utf : byChar;
    assert(centerJustifier("hello", 2).equal("hello".byChar));
    assert(centerJustifier("hello", 8).equal(" hello  ".byChar));
    assert(centerJustifier("hello", 7, 'x').equal("xhellox".byChar));
}

@safe pure unittest
{
    import std.string;

    assert(detab(" \n\tx", 9) == " \n         x");
}

@safe pure unittest
{
    import std.string;

    import std.array : array;

    assert(detabber(" \n\tx", 9).array == " \n         x");
}

@safe pure unittest
{
    import std.string;

    import std.array : array;
    import std.utf : byChar, byWchar;

    assert(detabber(" \u2029\t".byChar, 9).array == " \u2029         ");
    auto r = "hel\tx".byWchar.detabber();
    assert(r.front == 'h');
    auto s = r.save;
    r.popFront();
    r.popFront();
    assert(r.front == 'l');
    assert(s.front == 'h');
}

@safe pure unittest
{
    import std.string;

    assert(entab("        x \n") == "\tx\n");
}

@safe pure unittest
{
    import std.string;

    import std.array : array;
    assert(entabber("        x \n").array == "\tx\n");
}

@safe pure unittest
{
    import std.string;

    dchar[dchar] transTable1 = ['e' : '5', 'o' : '7', '5': 'q'];
    assert(translate("hello world", transTable1) == "h5ll7 w7rld");

    assert(translate("hello world", transTable1, "low") == "h5 rd");

    string[dchar] transTable2 = ['e' : "5", 'o' : "orange"];
    assert(translate("hello world", transTable2) == "h5llorange worangerld");
}

@safe pure unittest
{
    import std.string;

    import std.array : appender;
    dchar[dchar] transTable1 = ['e' : '5', 'o' : '7', '5': 'q'];
    auto buffer = appender!(dchar[])();
    translate("hello world", transTable1, null, buffer);
    assert(buffer.data == "h5ll7 w7rld");

    buffer.clear();
    translate("hello world", transTable1, "low", buffer);
    assert(buffer.data == "h5 rd");

    buffer.clear();
    string[dchar] transTable2 = ['e' : "5", 'o' : "orange"];
    translate("hello world", transTable2, null, buffer);
    assert(buffer.data == "h5llorange worangerld");
}

@safe pure nothrow unittest
{
    import std.string;

    auto transTable1 = makeTrans("eo5", "57q");
    assert(translate("hello world", transTable1) == "h5ll7 w7rld");

    assert(translate("hello world", transTable1, "low") == "h5 rd");
}

@safe pure nothrow unittest
{
    import std.string;

    auto transTable1 = makeTrans("eo5", "57q");
    assert(translate("hello world", transTable1) == "h5ll7 w7rld");

    assert(translate("hello world", transTable1, "low") == "h5 rd");
}

@safe pure unittest
{
    import std.string;

    assert(translate("hello world", makeTransTable("hl", "q5")) == "qe55o wor5d");
    assert(translate("hello world", makeTransTable("12345", "67890")) == "hello world");
}

@safe pure unittest
{
    import std.string;

    import std.array : appender;
    auto buffer = appender!(char[])();
    auto transTable1 = makeTransTable("eo5", "57q");
    translate("hello world", transTable1, null, buffer);
    assert(buffer.data == "h5ll7 w7rld");

    buffer.clear();
    translate("hello world", transTable1, "low", buffer);
    assert(buffer.data == "h5 rd");
}

@safe pure unittest
{
    import std.string;

    assert(succ("1") == "2");
    assert(succ("9") == "10");
    assert(succ("999") == "1000");
    assert(succ("zz99") == "aaa00");
}

@safe pure unittest
{
    import std.string;

    assert(tr("abcdef", "cd", "CD") == "abCDef");
    assert(tr("1st March, 2018", "March", "MAR", "s") == "1st MAR, 2018");
    assert(tr("abcdef", "ef", "", "d") == "abcd");
    assert(tr("14-Jul-87", "a-zA-Z", " ", "cs") == " Jul ");
}

@safe @nogc pure nothrow unittest
{
    import std.string;

    assert(isNumeric("123"));
    assert(isNumeric("123UL"));
    assert(isNumeric("123L"));
    assert(isNumeric("+123U"));
    assert(isNumeric("-123L"));
}

@safe @nogc pure nothrow unittest
{
    import std.string;

    assert(isNumeric("+123"));
    assert(isNumeric("-123.01"));
    assert(isNumeric("123.3e-10f"));
    assert(isNumeric("123.3e-10fi"));
    assert(isNumeric("123.3e-10L"));

    assert(isNumeric("nan"));
    assert(isNumeric("nani"));
    assert(isNumeric("-inf"));
}

@safe @nogc pure nothrow unittest
{
    import std.string;

    assert(isNumeric("-123e-1+456.9e-10Li"));
    assert(isNumeric("+123e+10+456i"));
    assert(isNumeric("123+456"));
}

@safe pure unittest
{
    import std.string;

    enum a = isNumeric("123.00E-5+1234.45E-12Li");
    enum b = isNumeric("12345xxxx890");

    static assert( a);
    static assert(!b);
}

@safe unittest
{
    import std.string;

    assert(soundexer("Gauss") == "G200");
    assert(soundexer("Ghosh") == "G200");

    assert(soundexer("Robert") == "R163");
    assert(soundexer("Rupert") == "R163");

    assert(soundexer("0123^&^^**&^") == ['\0', '\0', '\0', '\0']);
}

@safe unittest
{
    import std.string;

    assert(soundex("Gauss") == "G200");
    assert(soundex("Ghosh") == "G200");

    assert(soundex("Robert") == "R163");
    assert(soundex("Rupert") == "R163");

    assert(soundex("0123^&^^**&^") == null);
}

@safe unittest
{
    import std.string;

    import std.string;

    static string[] list = [ "food", "foxy" ];
    auto abbrevs = abbrev(list);
    assert(abbrevs == ["fox": "foxy", "food": "food",
                       "foxy": "foxy", "foo": "food"]);
}

@safe pure unittest
{
    import std.string;

    import std.utf : byChar, byWchar, byDchar;

    assert(column("1234 ") == 5);
    assert(column("1234 "w) == 5);
    assert(column("1234 "d) == 5);

    assert(column("1234 ".byChar()) == 5);
    assert(column("1234 "w.byWchar()) == 5);
    assert(column("1234 "d.byDchar()) == 5);

    // Tab stops are set at 8 spaces by default; tab characters insert enough
    // spaces to bring the column position to the next multiple of 8.
    assert(column("\t") == 8);
    assert(column("1\t") == 8);
    assert(column("\t1") == 9);
    assert(column("123\t") == 8);

    // Other tab widths are possible by specifying it explicitly:
    assert(column("\t", 4) == 4);
    assert(column("1\t", 4) == 4);
    assert(column("\t1", 4) == 5);
    assert(column("123\t", 4) == 4);

    // New lines reset the column number.
    assert(column("abc\n") == 0);
    assert(column("abc\n1") == 1);
    assert(column("abcdefg\r1234") == 4);
    assert(column("abc\u20281") == 1);
    assert(column("abc\u20291") == 1);
    assert(column("abc\u00851") == 1);
    assert(column("abc\u00861") == 5);
}

@safe pure unittest
{
    import std.string;

    assert(wrap("a short string", 7) == "a short\nstring\n");

    // wrap will not break inside of a word, but at the next space
    assert(wrap("a short string", 4) == "a\nshort\nstring\n");

    assert(wrap("a short string", 7, "\t") == "\ta\nshort\nstring\n");
    assert(wrap("a short string", 7, "\t", "    ") == "\ta\n    short\n    string\n");
}

@safe pure unittest
{
    import std.string;

    enum pretty = q{
       import std.stdio;
       void main() {
           writeln("Hello");
       }
    }.outdent();

    enum ugly = q{
import std.stdio;
void main() {
    writeln("Hello");
}
};

    assert(pretty == ugly);
}

@safe pure unittest
{
    import std.string;

    auto str1 = [
        "    void main()\n",
        "    {\n",
        "        test();\n",
        "    }\n"
    ];
    auto str1Expected = [
        "void main()\n",
        "{\n",
        "    test();\n",
        "}\n"
    ];
    assert(str1.outdent == str1Expected);

    auto str2 = [
        "void main()\n",
        "    {\n",
        "            test();\n",
        "    }\n"
    ];
    assert(str2.outdent == str2);
}

@safe pure unittest
{
    import std.string;

    string a = "Hölo World";
    immutable(ubyte)[] b = a.representation;
    string c = b.assumeUTF;

    assert(c == "Hölo World");
}

