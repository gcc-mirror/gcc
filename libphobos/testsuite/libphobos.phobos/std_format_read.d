@safe pure unittest
{
    import std.format.read;

    import std.format.spec : singleSpec;

    auto str = "false";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!bool(spec) == false);

    str = "1";
    spec = singleSpec("%d");
    assert(str.unformatValue!bool(spec) == true);
}

@safe pure unittest
{
    import std.format.read;

    import std.format.spec : singleSpec;

    auto str = "null";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!(typeof(null))(spec) == null);
}

@safe pure unittest
{
    import std.format.read;

    import std.format.spec : singleSpec;

    // signed decimal values
    auto str = "123";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!int(spec) == 123);

    // hexadecimal values
    str = "ABC";
    spec = singleSpec("%X");
    assert(str.unformatValue!int(spec) == 2748);

    // octal values
    str = "11610";
    spec = singleSpec("%o");
    assert(str.unformatValue!int(spec) == 5000);

    // raw read, depends on endianess
    str = "\x75\x01";
    spec = singleSpec("%r");
    auto result = str.unformatValue!short(spec);
    assert(result == 373 /* little endian */ || result == 29953 /* big endian */ );
}

@safe pure unittest
{
    import std.format.read;

    import std.format.spec : singleSpec;
    import std.math.operations : isClose;

    // natural notation
    auto str = "123.456";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!double(spec).isClose(123.456));

    // scientific notation
    str = "1e17";
    spec = singleSpec("%e");
    assert(str.unformatValue!double(spec).isClose(1e17));

    // raw read, depends on endianess
    str = "\x40\x00\x00\xBF";
    spec = singleSpec("%r");
    auto result = str.unformatValue!float(spec);
    assert(isClose(result, -0.5) /* little endian */ || isClose(result, 2.0) /* big endian */ );
}

@safe pure unittest
{
    import std.format.read;

    import std.format.spec : singleSpec;

    // only the first character is read
    auto str = "abc";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!char(spec) == 'a');

    // using a numerical format character treats the read number as unicode code point
    str = "65";
    spec = singleSpec("%d");
    assert(str.unformatValue!char(spec) == 'A');

    str = "41";
    spec = singleSpec("%x");
    assert(str.unformatValue!char(spec) == 'A');

    str = "10003";
    spec = singleSpec("%d");
    assert(str.unformatValue!dchar(spec) == '✓');
}

@safe pure unittest
{
    import std.format.read;

    import std.format.spec : singleSpec;

    // string value
    string str = "aaa";
    auto spec = singleSpec("%s");
    assert(str.unformatValue!(dchar[])(spec) == "aaa"d);

    // fixed size array with characters
    str = "aaa";
    spec = singleSpec("%s");
    dchar[3] ret = ['a', 'a', 'a'];
    assert(str.unformatValue!(dchar[3])(spec) == ret);

    // dynamic array
    str = "[1, 2, 3, 4]";
    spec = singleSpec("%s");
    assert(str.unformatValue!(int[])(spec) == [1, 2, 3, 4]);

    // fixed size array with integers
    str = "[1, 2, 3, 4]";
    spec = singleSpec("%s");
    int[4] ret2 = [1, 2, 3, 4];
    assert(str.unformatValue!(int[4])(spec) == ret2);

    // compound specifiers can be used for more control
    str = "1,2,3";
    spec = singleSpec("%(%s,%)");
    assert(str.unformatValue!(int[])(spec) == [1, 2, 3]);

    str = "cool";
    spec = singleSpec("%(%c%)");
    assert(str.unformatValue!(char[])(spec) == ['c', 'o', 'o', 'l']);
}

@safe pure unittest
{
    import std.format.read;

    import std.format.spec : singleSpec;

    // as single value
    auto str = `["one": 1, "two": 2]`;
    auto spec = singleSpec("%s");
    assert(str.unformatValue!(int[string])(spec) == ["one": 1, "two": 2]);

    // with compound specifier for more control
    str = "1/1, 2/4, 3/9";
    spec = singleSpec("%(%d/%d%|, %)");
    assert(str.unformatValue!(int[int])(spec) == [1: 1, 2: 4, 3: 9]);
}

@safe pure unittest
{
    import std.format.read;

    string object;
    char cmp;
    int value;

    assert(formattedRead("angle < 36", "%s %c %d", object, cmp, value) == 3);
    assert(object == "angle");
    assert(cmp == '<');
    assert(value == 36);

    // reading may end early:
    assert(formattedRead("length >", "%s %c %d", object, cmp, value) == 2);
    assert(object == "length");
    assert(cmp == '>');
    // value is not changed:
    assert(value == 36);
}

@safe pure unittest
{
    import std.format.read;

    string a;
    int b;
    double c;

    assert("hello!124:34.5".formattedRead!"%s!%s:%s"(a, b, c) == 3);
    assert(a == "hello");
    assert(b == 124);
    assert(c == 34.5);
}

@safe pure unittest
{
    import std.format.read;

    string item;
    double amount;

    assert("orange: (12%) 15.25".formattedRead("%s: (%*d%%) %f", item, amount) == 2);
    assert(item == "orange");
    assert(amount == 15.25);

    // can also be used with tuples
    import std.typecons : Tuple;

    Tuple!(int, float) t;
    char[] line = "1 7643 2.125".dup;
    formattedRead(line, "%s %*u %s", t);
    assert(t[0] == 1 && t[1] == 2.125);
}

@safe pure unittest
{
    import std.format.read;

    import std.exception : assertThrown;
    import std.format : FormatException;
    import std.typecons : tuple;

    auto complete = "hello!34.5:124".formattedRead!(string, double, int)("%s!%s:%s");
    assert(complete == tuple("hello", 34.5, 124));

    // reading ends early
    assertThrown!FormatException("hello!34.5:".formattedRead!(string, double, int)("%s!%s:%s"));
}

@safe pure unittest
{
    import std.format.read;

    import std.format : FormatException;
    import std.typecons : tuple;

    auto result = "orange: (12%) 15.25".formattedRead!(string, double)("%s: (%*d%%) %f");
    assert(result == tuple("orange", 15.25));
}

@safe pure unittest
{
    import std.format.read;

    import std.exception : assertThrown;
    import std.format : FormatException;
    import std.typecons : tuple;

    auto expected = tuple("hello", 124, 34.5);
    auto result = "hello!124:34.5".formattedRead!("%s!%s:%s", string, int, double);
    assert(result == expected);

    assertThrown!FormatException("hello!34.5:".formattedRead!("%s!%s:%s", string, double, int));
}

@safe pure unittest
{
    import std.format.read;

    import std.format : FormatException;
    import std.typecons : tuple;

    static assert(!__traits(compiles, "orange: (12%) 15.25".formattedRead!("%s: (%*d%%) %f", string, double)));
}

@safe pure unittest
{
    import std.format.read;

    import std.format.spec : singleSpec;

    string s = "42";
    auto spec = singleSpec("%s");
    assert(unformatValue!int(s, spec) == 42);
}

