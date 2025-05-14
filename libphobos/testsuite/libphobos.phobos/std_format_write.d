@safe pure unittest
{
    import std.format.write;

    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    formatValue(w1, true, spec1);

    assert(w1.data == "true");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%#x");
    formatValue(w2, true, spec2);

    assert(w2.data == "0x1");
}

@safe pure unittest
{
    import std.format.write;

    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w = appender!string();
    auto spec = singleSpec("%s");
    formatValue(w, null, spec);

    assert(w.data == "null");
}

@safe pure unittest
{
    import std.format.write;

    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%d");
    formatValue(w1, -1337, spec1);

    assert(w1.data == "-1337");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%x");
    formatValue(w2, -1337, spec2);

    assert(w2.data == "fffffac7");
}

@safe unittest
{
    import std.format.write;

    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%.3f");
    formatValue(w1, 1337.7779, spec1);

    assert(w1.data == "1337.778");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%.3e");
    formatValue(w2, 1337.7779, spec2);

    assert(w2.data == "1.338e+03");

    auto w3 = appender!string();
    auto spec3 = singleSpec("%.3g");
    formatValue(w3, 1337.7779, spec3);

    assert(w3.data == "1.34e+03");

    auto w4 = appender!string();
    auto spec4 = singleSpec("%.3a");
    formatValue(w4, 1337.7779, spec4);

    assert(w4.data == "0x1.4e7p+10");
}

@safe pure unittest
{
    import std.format.write;

    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%c");
    formatValue(w1, 'ì', spec1);

    assert(w1.data == "ì");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%#x");
    formatValue(w2, 'ì', spec2);

    assert(w2.data == "0xec");
}

@safe pure unittest
{
    import std.format.write;

    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    formatValue(w1, "hello", spec1);

    assert(w1.data == "hello");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%(%#x%|/%)");
    formatValue(w2, "hello", spec2);

    assert(w2.data == "0x68/0x65/0x6c/0x6c/0x6f");
}

@safe pure unittest
{
    import std.format.write;

    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w = appender!string();
    auto spec = singleSpec("%s");
    int[2] two = [1, 2];
    formatValue(w, two, spec);

    assert(w.data == "[1, 2]");
}

@safe pure unittest
{
    import std.format.write;

    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    auto two = [1, 2];
    formatValue(w1, two, spec1);

    assert(w1.data == "[1, 2]");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%(%g%|, %)");
    auto consts = [3.1415926, 299792458, 6.67430e-11];
    formatValue(w2, consts, spec2);

    assert(w2.data == "3.14159, 2.99792e+08, 6.6743e-11");

    // void[] is treated like ubyte[]
    auto w3 = appender!string();
    auto spec3 = singleSpec("%s");
    void[] val = cast(void[]) cast(ubyte[])[1, 2, 3];
    formatValue(w3, val, spec3);

    assert(w3.data == "[1, 2, 3]");
}

@safe pure unittest
{
    import std.format.write;

    import std.array : appender;
    import std.format.spec : singleSpec;

    auto aa = [10:17.5, 20:9.99];

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    formatValue(w1, aa, spec1);

    assert(w1.data == "[10:17.5, 20:9.99]" || w1.data == "[20:9.99, 10:17.5]");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%(%x = %.0e%| # %)");
    formatValue(w2, aa, spec2);

    assert(w2.data == "a = 2e+01 # 14 = 1e+01" || w2.data == "14 = 1e+01 # a = 2e+01");
}

@safe pure unittest
{
    import std.format.write;

    import std.array : appender;
    import std.format.spec : singleSpec;

    enum A { first, second, third }

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    formatValue(w1, A.second, spec1);

    assert(w1.data == "second");

    auto w2 = appender!string();
    auto spec2 = singleSpec("%d");
    formatValue(w2, A.second, spec2);

    assert(w2.data == "1");

    // values of an enum that have no name are formatted with %s using a cast
    A a = A.third;
    a++;

    auto w3 = appender!string();
    auto spec3 = singleSpec("%s");
    formatValue(w3, a, spec3);

    assert(w3.data == "cast(A)3");
}

@safe unittest
{
    import std.format.write;

    import std.array : appender;
    import std.format.spec : FormatSpec, singleSpec;

    // Using a `toString` with a writer
    static struct Point1
    {
        import std.range.primitives : isOutputRange, put;

        int x, y;

        void toString(W)(ref W writer, scope const ref FormatSpec!char f)
        if (isOutputRange!(W, char))
        {
            put(writer, "(");
            formatValue(writer, x, f);
            put(writer, ",");
            formatValue(writer, y, f);
            put(writer, ")");
        }
    }

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    auto p1 = Point1(16, 11);

    formatValue(w1, p1, spec1);
    assert(w1.data == "(16,11)");

    // Using a `toString` with a sink
    static struct Point2
    {
        int x, y;

        void toString(scope void delegate(scope const(char)[]) @safe sink,
                      scope const FormatSpec!char fmt) const
        {
            sink("(");
            sink.formatValue(x, fmt);
            sink(",");
            sink.formatValue(y, fmt);
            sink(")");
        }
    }

    auto w2 = appender!string();
    auto spec2 = singleSpec("%03d");
    auto p2 = Point2(16,11);

    formatValue(w2, p2, spec2);
    assert(w2.data == "(016,011)");

    // Using `string toString()`
    static struct Point3
    {
        int x, y;

        string toString()
        {
            import std.conv : to;

            return "(" ~ to!string(x) ~ "," ~ to!string(y) ~ ")";
        }
    }

    auto w3 = appender!string();
    auto spec3 = singleSpec("%s"); // has to be %s
    auto p3 = Point3(16,11);

    formatValue(w3, p3, spec3);
    assert(w3.data == "(16,11)");

    // without `toString`
    static struct Point4
    {
        int x, y;
    }

    auto w4 = appender!string();
    auto spec4 = singleSpec("%s"); // has to be %s
    auto p4 = Point4(16,11);

    formatValue(w4, p4, spec3);
    assert(w4.data == "Point4(16, 11)");
}

@safe pure unittest
{
    import std.format.write;

    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w1 = appender!string();
    auto spec1 = singleSpec("%s");
    auto p1 = () @trusted { return cast(void*) 0xFFEECCAA; } ();
    formatValue(w1, p1, spec1);

    assert(w1.data == "FFEECCAA");

    // null pointers are printed as `"null"` when used with `%s` and as hexadecimal integer else
    auto w2 = appender!string();
    auto spec2 = singleSpec("%s");
    auto p2 = () @trusted { return cast(void*) 0x00000000; } ();
    formatValue(w2, p2, spec2);

    assert(w2.data == "null");

    auto w3 = appender!string();
    auto spec3 = singleSpec("%x");
    formatValue(w3, p2, spec3);

    assert(w3.data == "0");
}

@safe unittest
{
    import std.format.write;

    import core.simd; // cannot be selective, because float4 might not be defined
    import std.array : appender;
    import std.format.spec : singleSpec;

    auto w = appender!string();
    auto spec = singleSpec("%s");

    static if (is(float4))
    {
        version (X86) {}
        else
        {
            float4 f4;
            f4.array[0] = 1;
            f4.array[1] = 2;
            f4.array[2] = 3;
            f4.array[3] = 4;

            formatValue(w, f4, spec);
            assert(w.data == "[1, 2, 3, 4]");
        }
    }
}

@safe pure unittest
{
    import std.format.write;

    import std.array : appender;

    auto writer1 = appender!string();
    formattedWrite(writer1, "%s is the ultimate %s.", 42, "answer");
    assert(writer1[] == "42 is the ultimate answer.");

    auto writer2 = appender!string();
    formattedWrite(writer2, "Increase: %7.2f %%", 17.4285);
    assert(writer2[] == "Increase:   17.43 %");
}

@safe pure unittest
{
    import std.format.write;

    import std.array : appender;

    auto writer = appender!string();
    writer.formattedWrite!"%d is the ultimate %s."(42, "answer");
    assert(writer[] == "42 is the ultimate answer.");

    // This line doesn't compile, because 3.14 cannot be formatted with %d:
    // writer.formattedWrite!"%d is the ultimate %s."(3.14, "answer");
}

@safe pure unittest
{
    import std.format.write;

    import std.array : appender;
    import std.format.spec : singleSpec;

    auto writer = appender!string();
    auto spec = singleSpec("%08b");
    writer.formatValue(42, spec);
    assert(writer.data == "00101010");

    spec = singleSpec("%2s");
    writer.formatValue('=', spec);
    assert(writer.data == "00101010 =");

    spec = singleSpec("%+14.6e");
    writer.formatValue(42.0, spec);
    assert(writer.data == "00101010 = +4.200000e+01");
}

