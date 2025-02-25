@safe unittest
{
    import std.conv;

    import std.exception : assertThrown;
    assertThrown!ConvException(to!int("abc"));
}

@safe unittest
{
    import std.conv;

    import std.exception : assertThrown;
    assertThrown!ConvOverflowException(to!ubyte(1_000_000));
}

@safe pure unittest
{
    import std.conv;

    int a = 42;
    int b = to!int(a);
    double c = to!double(3.14); // c is double with value 3.14
}

@safe pure unittest
{
    import std.conv;

    import std.exception : assertThrown;

    int a = 420;
    assert(to!long(a) == a);
    assertThrown!ConvOverflowException(to!byte(a));

    assert(to!int(4.2e6) == 4200000);
    assertThrown!ConvOverflowException(to!uint(-3.14));
    assert(to!uint(3.14) == 3);
    assert(to!uint(3.99) == 3);
    assert(to!int(-3.99) == -3);
}

@safe pure unittest
{
    import std.conv;

    auto str = to!string(42, 16);
    assert(str == "2A");
    auto i = to!int(str, 16);
    assert(i == 42);
}

@safe pure unittest
{
    import std.conv;

    // 2^24 - 1, largest proper integer representable as float
    int a = 16_777_215;
    assert(to!int(to!float(a)) == a);
    assert(to!int(to!float(-a)) == -a);
}

@safe pure unittest
{
    import std.conv;

    import std.exception : assertThrown;

    assert(to!char("a") == 'a');
    assertThrown(to!char("ñ")); // 'ñ' does not fit into a char
    assert(to!wchar("ñ") == 'ñ');
    assertThrown(to!wchar("😃")); // '😃' does not fit into a wchar
    assert(to!dchar("😃") == '😃');

    // Using wstring or dstring as source type does not affect the result
    assert(to!char("a"w) == 'a');
    assert(to!char("a"d) == 'a');

    // Two code points cannot be converted to a single one
    assertThrown(to!char("ab"));
}

@safe pure unittest
{
    import std.conv;

    import std.string : split;

    int[] a = [1, 2, 3];
    auto b = to!(float[])(a);
    assert(b == [1.0f, 2, 3]);
    string str = "1 2 3 4 5 6";
    auto numbers = to!(double[])(split(str));
    assert(numbers == [1.0, 2, 3, 4, 5, 6]);
    int[string] c;
    c["a"] = 1;
    c["b"] = 2;
    auto d = to!(double[wstring])(c);
    assert(d["a"w] == 1 && d["b"w] == 2);
}

@safe unittest
{
    import std.conv;

    int[string][double[int[]]] a;
    auto b = to!(short[wstring][string[double[]]])(a);
}

@safe pure unittest
{
    import std.conv;

    import std.exception : assertThrown;
    // Testing object conversions
    class A {}
    class B : A {}
    class C : A {}
    A a1 = new A, a2 = new B, a3 = new C;
    assert(to!B(a2) is a2);
    assert(to!C(a3) is a3);
    assertThrown!ConvException(to!B(a3));
}

@system pure unittest
{
    import std.conv;

    // Conversion representing dynamic/static array with string
    long[] a = [ 1, 3, 5 ];
    assert(to!string(a) == "[1, 3, 5]");

    // Conversion representing associative array with string
    int[string] associativeArray = ["0":1, "1":2];
    assert(to!string(associativeArray) == `["0":1, "1":2]` ||
           to!string(associativeArray) == `["1":2, "0":1]`);

    // char* to string conversion
    assert(to!string(cast(char*) null) == "");
    assert(to!string("foo\0".ptr) == "foo");

    // Conversion reinterpreting void array to string
    auto w = "abcx"w;
    const(void)[] b = w;
    assert(b.length == 8);

    auto c = to!(wchar[])(b);
    assert(c == "abcx");
}

@safe pure unittest
{
    import std.conv;

    import std.exception : assertThrown;

    enum E { a, b, c }
    assert(to!E("a") == E.a);
    assert(to!E("b") == E.b);
    assertThrown!ConvException(to!E("A"));
}

@safe unittest
{
    import std.conv;

    assert(roundTo!int(3.14) == 3);
    assert(roundTo!int(3.49) == 3);
    assert(roundTo!int(3.5) == 4);
    assert(roundTo!int(3.999) == 4);
    assert(roundTo!int(-3.14) == -3);
    assert(roundTo!int(-3.49) == -3);
    assert(roundTo!int(-3.5) == -4);
    assert(roundTo!int(-3.999) == -4);
    assert(roundTo!(const int)(to!(const double)(-3.999)) == -4);
}

@safe unittest
{
    import std.conv;

    import std.typecons : Flag, Yes, No;
    auto s = "true";
    bool b = parse!bool(s);
    assert(b);
    auto s2 = "true";
    bool b2 = parse!(bool, string, No.doCount)(s2);
    assert(b2);
    auto s3 = "true";
    auto b3 = parse!(bool, string, Yes.doCount)(s3);
    assert(b3.data && b3.count == 4);
    auto s4 = "falSE";
    auto b4 = parse!(bool, string, Yes.doCount)(s4);
    assert(!b4.data && b4.count == 5);
}

@safe pure unittest
{
    import std.conv;

    import std.typecons : Flag, Yes, No;
    string s = "123";
    auto a = parse!int(s);
    assert(a == 123);

    string s1 = "123";
    auto a1 = parse!(int, string, Yes.doCount)(s1);
    assert(a1.data == 123 && a1.count == 3);
}

@safe pure unittest
{
    import std.conv;

    import std.string : tr;
    import std.typecons : Flag, Yes, No;
    string test = "123 \t  76.14";
    auto a = parse!uint(test);
    assert(a == 123);
    assert(test == " \t  76.14"); // parse bumps string
    test = tr(test, " \t\n\r", "", "d"); // skip ws
    assert(test == "76.14");
    auto b = parse!double(test);
    assert(b == 76.14);
    assert(test == "");

    string test2 = "123 \t  76.14";
    auto a2 = parse!(uint, string, Yes.doCount)(test2);
    assert(a2.data == 123 && a2.count == 3);
    assert(test2 == " \t  76.14");// parse bumps string
    test2 = tr(test2, " \t\n\r", "", "d"); // skip ws
    assert(test2 == "76.14");
    auto b2 = parse!(double, string, Yes.doCount)(test2);
    assert(b2.data == 76.14 && b2.count == 5);
    assert(test2 == "");

}

@safe unittest
{
    import std.conv;

    import std.typecons : Flag, Yes, No, tuple;
    enum EnumType : bool { a = true, b = false, c = a }

    auto str = "a";
    assert(parse!EnumType(str) == EnumType.a);
    auto str2 = "a";
    assert(parse!(EnumType, string, No.doCount)(str2) == EnumType.a);
    auto str3 = "a";
    assert(parse!(EnumType, string, Yes.doCount)(str3) == tuple(EnumType.a, 1));

}

@safe unittest
{
    import std.conv;

    import std.math.operations : isClose;
    import std.math.traits : isNaN, isInfinity;
    import std.typecons : Flag, Yes, No;
    auto str = "123.456";
    assert(parse!double(str).isClose(123.456));
    auto str2 = "123.456";
    assert(parse!(double, string, No.doCount)(str2).isClose(123.456));
    auto str3 = "123.456";
    auto r = parse!(double, string, Yes.doCount)(str3);
    assert(r.data.isClose(123.456));
    assert(r.count == 7);
    auto str4 = "-123.456";
    r = parse!(double, string, Yes.doCount)(str4);
    assert(r.data.isClose(-123.456));
    assert(r.count == 8);
    auto str5 = "+123.456";
    r = parse!(double, string, Yes.doCount)(str5);
    assert(r.data.isClose(123.456));
    assert(r.count == 8);
    auto str6 = "inf0";
    r = parse!(double, string, Yes.doCount)(str6);
    assert(isInfinity(r.data) && r.count == 3 && str6 == "0");
    auto str7 = "-0";
    auto r2 = parse!(float, string, Yes.doCount)(str7);
    assert(r2.data.isClose(0.0) && r2.count == 2);
    auto str8 = "nan";
    auto r3 = parse!(real, string, Yes.doCount)(str8);
    assert(isNaN(r3.data) && r3.count == 3);
}

@safe pure unittest
{
    import std.conv;

    import std.typecons : Flag, Yes, No;
    auto s = "Hello, World!";
    char first = parse!char(s);
    assert(first == 'H');
    assert(s == "ello, World!");
    char second = parse!(char, string, No.doCount)(s);
    assert(second == 'e');
    assert(s == "llo, World!");
    auto third = parse!(char, string, Yes.doCount)(s);
    assert(third.data == 'l' && third.count == 1);
    assert(s == "lo, World!");
}

@safe pure unittest
{
    import std.conv;

    import std.exception : assertThrown;
    import std.typecons : Flag, Yes, No;

    alias NullType = typeof(null);
    auto s1 = "null";
    assert(parse!NullType(s1) is null);
    assert(s1 == "");

    auto s2 = "NUll"d;
    assert(parse!NullType(s2) is null);
    assert(s2 == "");

    auto s3 = "nuLlNULl";
    assert(parse!(NullType, string, No.doCount)(s3) is null);
    auto r = parse!(NullType, string, Yes.doCount)(s3);
    assert(r.data is null && r.count == 4);

    auto m = "maybe";
    assertThrown!ConvException(parse!NullType(m));
    assertThrown!ConvException(parse!(NullType, string, Yes.doCount)(m));
    assert(m == "maybe");  // m shouldn't change on failure

    auto s = "NULL";
    assert(parse!(const NullType)(s) is null);
}

@safe pure unittest
{
    import std.conv;

    import std.typecons : Flag, Yes, No;
    auto s1 = `[['h', 'e', 'l', 'l', 'o'], "world"]`;
    auto a1 = parse!(string[])(s1);
    assert(a1 == ["hello", "world"]);

    auto s2 = `["aaa", "bbb", "ccc"]`;
    auto a2 = parse!(string[])(s2);
    assert(a2 == ["aaa", "bbb", "ccc"]);

    auto s3 = `[['h', 'e', 'l', 'l', 'o'], "world"]`;
    auto len3 = s3.length;
    auto a3 = parse!(string[], string, Yes.doCount)(s3);
    assert(a3.data == ["hello", "world"]);
    assert(a3.count == len3);
}

@safe pure unittest
{
    import std.conv;

    import std.typecons : Flag, Yes, No, tuple;
    import std.range.primitives : save;
    import std.array : assocArray;
    auto s1 = "[1:10, 2:20, 3:30]";
    auto copyS1 = s1.save;
    auto aa1 = parse!(int[int])(s1);
    assert(aa1 == [1:10, 2:20, 3:30]);
    assert(tuple([1:10, 2:20, 3:30], copyS1.length) == parse!(int[int], string, Yes.doCount)(copyS1));

    auto s2 = `["aaa":10, "bbb":20, "ccc":30]`;
    auto copyS2 = s2.save;
    auto aa2 = parse!(int[string])(s2);
    assert(aa2 == ["aaa":10, "bbb":20, "ccc":30]);
    assert(tuple(["aaa":10, "bbb":20, "ccc":30], copyS2.length) ==
        parse!(int[string], string, Yes.doCount)(copyS2));

    auto s3 = `["aaa":[1], "bbb":[2,3], "ccc":[4,5,6]]`;
    auto copyS3 = s3.save;
    auto aa3 = parse!(int[][string])(s3);
    assert(aa3 == ["aaa":[1], "bbb":[2,3], "ccc":[4,5,6]]);
    assert(tuple(["aaa":[1], "bbb":[2,3], "ccc":[4,5,6]], copyS3.length) ==
        parse!(int[][string], string, Yes.doCount)(copyS3));

    auto s4 = `[]`;
    int[int] emptyAA;
    assert(tuple(emptyAA, s4.length) == parse!(int[int], string, Yes.doCount)(s4));
}

@safe unittest
{
    import std.conv;

    assert( text(42, ' ', 1.5, ": xyz") == "42 1.5: xyz"c);
    assert(wtext(42, ' ', 1.5, ": xyz") == "42 1.5: xyz"w);
    assert(dtext(42, ' ', 1.5, ": xyz") == "42 1.5: xyz"d);
}

@safe unittest
{
    import std.conv;

    // Same as 0177
    auto a = octal!177;
    // octal is a compile-time device
    enum b = octal!160;
    // Create an unsigned octal
    auto c = octal!"1_000_000u";
    // Leading zeros are allowed when converting from a string
    auto d = octal!"0001_200_000";
}

@safe unittest
{
    import std.conv;

    import std.traits : Unsigned;
    immutable int s = 42;
    auto u1 = unsigned(s); //not qualified
    static assert(is(typeof(u1) == uint));
    Unsigned!(typeof(s)) u2 = unsigned(s); //same qualification
    static assert(is(typeof(u2) == immutable uint));
    immutable u3 = unsigned(s); //explicitly qualified
}

@safe unittest
{
    import std.conv;

    import std.traits : Signed;

    immutable uint u = 42;
    auto s1 = signed(u); //not qualified
    static assert(is(typeof(s1) == int));
    Signed!(typeof(u)) s2 = signed(u); //same qualification
    static assert(is(typeof(s2) == immutable int));
    immutable s3 = signed(u); //explicitly qualified
}

@safe unittest
{
    import std.conv;

    enum A { a = 42 }
    static assert(is(typeof(A.a.asOriginalType) == int));
    assert(A.a.asOriginalType == 42);
    enum B : double { a = 43 }
    static assert(is(typeof(B.a.asOriginalType) == double));
    assert(B.a.asOriginalType == 43);
}

@system unittest
{
    import std.conv;

    // Regular cast, which has been verified to be legal by the programmer:
    {
        long x;
        auto y = cast(int) x;
    }

    // However this will still compile if 'x' is changed to be a pointer:
    {
        long* x;
        auto y = cast(int) x;
    }

    // castFrom provides a more reliable alternative to casting:
    {
        long x;
        auto y = castFrom!long.to!int(x);
    }

    // Changing the type of 'x' will now issue a compiler error,
    // allowing bad casts to be caught before it's too late:
    {
        long* x;
        static assert(
            !__traits(compiles, castFrom!long.to!int(x))
        );

        // if cast is still needed, must be changed to:
        auto y = castFrom!(long*).to!int(x);
    }
}

@safe unittest
{
    import std.conv;

    // conversion at compile time
    auto string1 = hexString!"304A314B";
    assert(string1 == "0J1K");
    auto string2 = hexString!"304A314B"w;
    assert(string2 == "0J1K"w);
    auto string3 = hexString!"304A314B"d;
    assert(string3 == "0J1K"d);
}

@safe unittest
{
    import std.conv;

    import std.algorithm.comparison : equal;

    assert(toChars(1).equal("1"));
    assert(toChars(1_000_000).equal("1000000"));

    assert(toChars!(2)(2U).equal("10"));
    assert(toChars!(16)(255U).equal("ff"));
    assert(toChars!(16, char, LetterCase.upper)(255U).equal("FF"));
}

